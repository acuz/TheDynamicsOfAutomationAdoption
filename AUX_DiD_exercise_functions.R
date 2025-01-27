###################################################################
####### Script defining functions to prepare the dataset    #######
####### and to run the CS estimator with pretrend testing   #######
###################################################################


############################
###### Import libraries ####
############################

library(data.table)
library(here)
library(fixest)
library(did)
library(MatchIt)
library(Matching)

library(foreach)
library(doParallel)

################################################################
################ Preliminary data operations ###################
################################################################

####### Define the functions for dataset transformation  #######

data_import <-  function()
{
  ###### First operations with the dataset: retrieving and integrating variables and datasets #####
  
  
  ### Import firm level dataset
  i_am('Task_3.2_revisions.Rproj')
  dt <- fread(here::here('elaborated_datasets','firm_level_clean_complete_rv24.csv'))
  dt$fl_idbase <- as.factor(dt$fl_idbase)
  
  ############### REMEMBER TO UNZIP THE FOLDERS CONTAINING FRAME DATA ###########
  
  # ### Merge with the additional variables from frame
  # i_am('desktop.ini')
  # library(haven)
  # 
  # file_list <- list.files(here::here('dataset_univ'),pattern = 'frame*')
  # zip_list <- list.files(here::here('dataset_univ'), pattern = 'zip')
  # 
  # file_list <- setdiff(file_list,zip_list)
  # 
  # dt_frame_list <- list()
  # for(fold in file_list)
  # {
  #   dt_frame_year <- fread(here::here('dataset_univ',fold,paste0(fold,'.csv')))
  #   
  #   dt_frame_year <- dt_frame_year[, c(list(anno = anno,codice_unita = codice_unita,codice_reg = codice_reg,
  #                                           ateco = ateco,flag_liquidazione = flag_liquidazione,att6mesi = att6mesi),
  #                                      lapply(.SD, function(x) as.numeric(x))), .SDcols = c('addetti','dipendenti','sal_stip','personale',
  #                                                                                           'ricavi_vend_prest','acquisti','servizi','vagg','vp','mol')]
  #   
  #   dt_frame_list[[fold]] <- dt_frame_year
  # }
  # 
  # dt_frame <- do.call(rbind,dt_frame_list)
  # rm(dt_frame_year)
  # 
  # # Check distribution for given variables
  # #q_is_na <- function(x,qq, na.rm = T) c(sum(is.na(x))/length(x), as.numeric(quantile(x,qq, na.rm = na.rm)))
  # # chack_vars <- c('vagg')
  # #dt_frame[,lapply(.SD, function(x) q_is_na(x, 0:10/10, na.rm =T)), .SDcols = check_vars]
  # 
  # dt_frame <- dt_frame[,c('anno','codice_unita',setdiff(colnames(dt_frame),intersect(colnames(dt),colnames(dt_frame)))),with = F]
  # 
  # dt <- merge(dt,dt_frame, by = c('anno','codice_unita'), all.x = T)
  # rm(dt_frame)
  # 
  # i_am('Task_3.2_revisions.Rproj')
  
  return(dt)
}

data_reshaping <- function(dt,spike_type,tech_type)
{
 
  description_spike <- function(spike_type)
  {
    if(spike_type =='spike'){output <- 'Spike identified using largest import event'}  
    if(spike_type =='spike2'){output <- 'Spike identified using largest import and relative minimal threshold using average within-firm imports'}  
    if(spike_type =='spike3'){output <- 'Spike identified using largest import and relative minimal threshold using average within-sector-class imports'}  
    if(spike_type =='spike4'){output <- 'Spike identified using largest import and relative minimal threshold using average within-firm and within-sector-class imports'}  
    return(output)
  }
  
  setDT(dt)
  ### filter intermediaries
  intermediary_check <- function(x)
  {
    ateco <- as.character(x) 
    u1 <- startsWith(as.character(ateco),'4614')
    u2 <- startsWith(as.character(ateco),'4619')
    u3 <- startsWith(as.character(ateco),'465')
    u4 <- startsWith(as.character(ateco),'46699')
    u5 <- startsWith(as.character(ateco),'469')
    u6 <- startsWith(as.character(ateco),'4662')
    u7 <- startsWith(as.character(ateco),'4664')
    return(ifelse((u1|u2|u3|u4|u5|u6|u7),1,0))
  }  
  
  #dt[,fl_idbase := ifelse(is.na(fl_idbase),0,fl_idbase)]
  dt[,is_intermediary := intermediary_check(ateco)]
  
  dt[,type_ateco:= nchar(ateco)]
  dt[type_ateco == 4, list(ateco)]
  dt[,ateco := ifelse(type_ateco==4,paste0('0',ateco),ateco)]
  dt[,NC2 := substr(ateco,1,2)]
  
  
  summary_stats <- dt[anno == 2019, list(.N, n_dip = sum(dipendenti, na.rm = T)) , keyby = list(NC2)][order(-N)]
  summary_stats[,agg_n_dip := sum(n_dip)]
  summary_stats[,share_dip := round(100*(n_dip/agg_n_dip),4)]
  summary_stats[,agg_N := sum(N)]
  summary_stats[,share_N := round(100*(N/agg_N),4)]
  summary_stats[, agg_n_dip:= NULL]
  summary_stats[,agg_N := NULL]
  
  dt <- dt[!is.na(codice_unita)]
  
  
  identification_spike <- paste0(spike_type,'_',tech_type)
  
  df <- dt ## remove any intermediary if any
  df[,ident_spike := get(paste0(spike_type,'_',tech_type))]
  df[,ident_imp := get(paste0('imp_',tech_type))]
  df[,ident_exp := get(paste0('exp_',tech_type))]
  df[,ident_rank := get(paste0('rank_',tech_type))]
  
  
  cat('Running the estimation procedure using SPIKE TYPE: ',description_spike(spike_type),'\n')
  cat('Running the estimation procedure using TECH TYPE: ',tech_type,'\n')
  
  ### Spike and is adopter definition
  df[,ident_spike := ifelse(is.na(ident_spike),0,ident_spike)]
  df[,is_adopter := sum(ident_spike,na.rm = T), by = list(codice_unita)]
  df[,is_man := as.factor(ifelse(NC2>= 10 & NC2 <= 33,1,0))]
  df[,is_adopter_bf2011 := sum(ident_spike[anno<2011], na.rm = T), by = list(codice_unita)]
  df <- df[is_adopter <= 1]
  
  cat('Adopters/Non-adopters in the full COE span (2005-2019) \n')
  counter <- unique(df[,list(is_adopter,codice_unita)])
  counter <- counter[,.N, keyby = list(is_adopter)]
  cat('N. firms:',length(unique(df$codice_unita)),'\n')
  cat(paste0(c(' Sh. non adopters: ', 'Sh. adopters: '),
             round(100*counter[,N/sum(N)],2),
             c('%','%'),'\n'),'\n')
  
  df[,year_treated := ifelse(ident_spike == 1,anno,NA)]
  df[,year_treated := ifelse(is_adopter == 0, NA,min(year_treated, na.rm = T)) , by = .(codice_unita)]
  
  
  cat('Summary of adoption years\n')
  adoption_checker_dt <- unique(df[,list(year_treated,codice_unita,is_adopter)])
  adoption_checker_dt <- adoption_checker_dt[,.N, keyby = year_treated]
  adoption_checker_dt[,year_treated := ifelse(is.na(year_treated), 'Non-adopter',year_treated)]
  print(adoption_checker_dt)
  
  #### Exclude the firms which had a spike before 2011 (so that they do not appear neither as controls or as adopters)
  df <- df[is_intermediary == 0,] ## exclude intermediaries if any
  df <- df[is_adopter_bf2011 == 0,] ## exclude adopter before 2011
  df <- df[is_adopter <= 1,] ## exclude anomalies such as double spikes
  df <- df[anno>2010,]  ## exclude years before 2011
  
  
  counter <- unique(df[,list(is_adopter,codice_unita)])
  counter <- counter[,.N, keyby = list(is_adopter)]
  cat('Adopters/Non-adopters after exclusion of adopters before 2011\n')
  cat('N. firms:',length(unique(df$codice_unita)),'\n')
  cat(paste0(c(' Sh. non adopters: ', 'Sh. adopters: '),
             round(100*counter[,N/sum(N)],2),
             c('%','%'),'\n'),'\n')
  
  # add size class coarser categorization
  
  df[,sizecl_coarse := ifelse(sizecl %in% c('00-01','04-05','02-03','06-09'),'Micro',
                              ifelse(sizecl %in% c('10-19','20-49'),'Small',
                                     ifelse(sizecl %in% c('100-250','50-99','100+'),'Medium',
                                            ifelse(sizecl %in% c('250+'),'Large',NA))))]
  
  df$sizecl_coarse <- factor(df$sizecl_coarse, levels = c('Micro','Small','Medium','Large'))
  
  
  
  ##### Define treatment variables ######
  df[,year_treated := ifelse(ident_spike == 1,anno,NA)]
  df[,year_treated := ifelse(is_adopter == 0, NA,min(year_treated, na.rm = T)) , by = .(codice_unita)]
  df[,years_from_treatment := ifelse(is_adopter == 1,anno - year_treated,NA)]
  
  df[,absorbing_adopter := cumsum(ident_spike), by = list(codice_unita)] ### define dummy of absorption into treatment
  df$absorbing_adopter <- factor(df$absorbing_adopter)
  
  df[,pre_treat := ifelse(years_from_treatment < -2,1,0)] ### define pretreatment excluding relatime period -1,-2
  df$pre_treat <- factor(df$pre_treat)
  
  df[,post_treat := ifelse(years_from_treatment>=0,1,0)] ## define posttreatment including year of treatment
  df$post_treat <- factor(df$post_treat)
  
  
  
  ### Check on adoption statistics
  cat('Summary of adoption years\n')
  adoption_checker_dt <- unique(df[,list(year_treated,codice_unita,is_adopter)])
  adoption_checker_dt <- adoption_checker_dt[,.N, keyby = year_treated]
  adoption_checker_dt[,year_treated := ifelse(is.na(year_treated), 'Non-adopter',year_treated)]
  print(adoption_checker_dt)
  
  
  cat('Check presence of duplicates\n')
  df_dup <- df[,'n':= .N, keyby = list(codice_unita,anno)][n>1]
  cat('Number of dupplicates: ', nrow(df_dup), '\n')
  
  year_cap <- function(x,yy)pmax(pmin(yy,x),-yy)
  summary(as.factor(year_cap(df$years_from_treatment,7)))
  summary(as.factor(year_cap(df$years_from_treatment,6)))
  summary(as.factor(year_cap(df$years_from_treatment,5)))
  summary(as.factor(year_cap(df$years_from_treatment,4)))
  
  
  
  
  #################################################################
  #################### Start the event study ######################
  #################################################################
  
  
  #### Import additional variables ####
  
  add_vars_dt <- fread(here::here('add_var_dt.csv'))
  add_vars_dt <- add_vars_dt[,c('codice_unita','anno', setdiff(colnames(add_vars_dt),colnames(df))), with = F] ## select only the variables which are missing from the orgiinal dataset
  
  df <- merge(df,add_vars_dt,by = c('codice_unita','anno') , all.x = T)
  
  #### Derived variable definitions (logs and grs) ######
  
  df[,prod := as.numeric(vagg)/dipendenti]
  df[,ln_prod := log((1+as.numeric(vagg))/(addetti+1))][,ln_prod := ifelse(is.infinite(ln_prod),NA,ln_prod)]
  #df[,ln_prod_1 := dplyr::lag(ln_prod ,1), keyby = list(codice_unita)]
  #df[,ln_prod_2 := dplyr::lag(ln_prod ,2), keyby = list(codice_unita)]
  #df[,ln_prod_3 := dplyr::lag(ln_prod,3), keyby = list(codice_unita)]
  
  logG = function(x) c(NA, diff(log(1+x)))
  
  df[,gr_dip := logG(dipendenti), keyby = list(codice_unita)]
  #df[,gr_dip_1 := dplyr::lag(gr_dip,1), keyby = list(codice_unita)]
  #df[,vp_1 := dplyr::lag(vp,1), keyby = list(codice_unita)]
  
  df[,is_exporter := ifelse(sum(tot_export_val,na.rm = T)>0, 1, 0), by = .(codice_unita)]
  
  df[,ln_dip := log(1+dipendenti)]
  #df[,ln_dip_2:= log(1+dip)]
  #df[,ln_hirings := log(1+hirings)]
  #df[,ln_separations := log(1+separations)]
  
  df[,ln_add := log(1+addetti)]
  df[,lab_share := as.numeric(personale)/as.numeric(vagg)]
  df[,top10_med := (1+top10_retr_sett)/(1+med_retr_sett)]
  df[,top10_bottom10 := (1+top10_retr_sett)/(1+bottom10_retr_sett)]
  df[,ln_avg_retr_sett := log(1+avg_retr_sett)]
  df[,ln_IQR_retr_sett := log(1+IQR_retr_sett)]
  df[,ln_sd_retr_sett := log(1+sd_retr_sett)]
  df[,ln_top10_bottom10 := log((1+top10_retr_sett)/(1+bottom10_retr_sett))]
  df[,ln_top10_med := log((1+top10_retr_sett)/(1+med_retr_sett))]
  df[,ln_top10 := log((1+top10_retr_sett))]
  df[,ln_bottom10 := log(1+bottom10_retr_sett)]
  
  df_balance <- df
  #df_balance <- setDT(df, key = c("codice_unita", "anno"))[CJ(codice_unita, anno, unique=TRUE)]
  #summary(df_balance[,list(ln_avg_retr_sett,avg_retr_sett,dipendenti)])
  
  #source(here::here('custom_compute.att_gt.R'))
  #environment(custom_compute.att_gt) <- asNamespace('did')
  #assignInNamespace("compute.att_gt", custom_compute.att_gt, ns = "did")
  
  df_balance <- df_balance[, is_adopter := ifelse(is.na(is_adopter),0,is_adopter)]
  df_balance[is_adopter == 0,year_treated := 0, by = .(codice_unita)]
  df_balance[,ln_sales :=  log(1 + as.numeric(vp))][,ln_sales := ifelse(is.infinite(ln_sales),NA,ln_sales)]
  df_balance[,ln_sales_new :=  log(1 + as.numeric(ricavi_vend_prest))][,ln_sales_new := ifelse(is.infinite(ln_sales_new),NA,ln_sales_new)]
  df_balance[,ln_vadd := log(1 + as.numeric(vagg))][,ln_vadd := ifelse(is.infinite(ln_vadd),NA,ln_vadd)]
  df_balance[,ln_lab_share := log(1 + as.numeric(lab_share))][,ln_lab_share := ifelse(is.infinite(ln_lab_share),NA,ln_lab_share)]
  df_balance[,ln_exp_val := log(1+ifelse(is.na(tot_export_val),0,tot_export_val/1000))]
  df_balance[,ln_exp_ident_val := log(1+ifelse(is.na(ident_exp),0,ident_exp/1000))]
  df_balance[,ln_imp_val := log(1+ifelse(is.na(tot_import_val),0,tot_import_val/1000))]
  df_balance[,ln_sal_stip := log(1+sal_stip)]
  df_balance[,ln_personale := log(1+personale)]
  df_balance[,ln_beniserv := log(1+beniserv)][,ln_beniserv := ifelse(is.na(ln_beniserv),0,ln_beniserv)]
  df_balance[,ln_acquisti := log(1+acquisti)][,ln_acquisti := ifelse(is.na(ln_acquisti),0,ln_beniserv)]
  df_balance[,ln_vert_int := log(1+((1+as.numeric(vagg))/(1+as.numeric(ricavi_vend_prest))))][,ln_vert_int := ifelse(is.infinite(ln_vert_int),NA,ln_vert_int)]
  df_balance[,vert_int := as.numeric(vagg)/as.numeric(1+ricavi_vend_prest)][,vert_int := ifelse(vert_int<= 0,NA,vert_int)]
  
  
  ###### Cleaning dataset from rexporter of automation/ai goods #####
  
  df_balance[,ident_exp := ifelse(is.na(ident_exp),0,ident_exp)]
  df_balance[,ident_imp := ifelse(is.na(ident_imp),0,ident_imp)]
  df_balance[,ident_spike := ifelse(is.na(ident_spike),0,ident_spike)]
  
  
  
  reexp_account <- function(is_adopter, spike,imp,exp,bench = 4)
  {
    if(sum(is_adopter,na.rm=T)>0)
    {  
      spike[is.na(spike)] <- 0  
      pre_post <- cumsum(spike)
      years_of_interest <-  cumsum(pre_post)
      years_of_interest[years_of_interest <= bench & years_of_interest > 0] <- 1
      years_of_interest[!(years_of_interest <= bench & years_of_interest > 0)] <- 0
      
      crit_val <- abs(sum(exp*years_of_interest, na.rm = T)-sum(imp*spike, na.rm = T))/sum(imp*spike)
      
      output <- rep(ifelse((crit_val >= 0)&(crit_val <= 0.7),1,0),length(is_adopter))
      
    }else{output <- rep(0,length(is_adopter))}
    
    return(output)
  }
  
  
  ##### Defining reexport minimal ample
  
  df_balance[, is_reexporter_count := reexp_account(is_adopter,ident_spike,ident_imp,ident_exp),
             by = list(codice_unita)]
  df_balance[, is_reexporter_simple := ifelse(sum(is_adopter,na.rm = T)>0,
                                              ifelse(sum(cumsum(ident_spike)*ident_exp)>0,1,0),
                                              0), by = list(codice_unita)]
  
  ##### Check those that are importing every year
  df_balance[,import_years := sum(ident_imp>0)/length(ident_imp), by =.(codice_unita)]
  df_balance[, is_persistent_importer := ifelse(import_years ==1,1,0)]
  
  #### Check those that import too much
  
  df_balance[, ident_imp_per_addetto := ident_imp/(1000*addetti)]
  df_balance[, avg_ident_imp_per_addetto := mean(ident_imp_per_addetto, na.rm = T), by = .(codice_unita)]
  
  q_vals <- quantile(df_balance$avg_ident_imp_per_addetto,(800:1000)/1000, na.rm = T)
  
  q_vals[2:length(q_vals)]/q_vals[1:length(q_vals)-1]
  
  over_size_importers <- unique(df_balance[avg_ident_imp_per_addetto>q_vals['99.0%']]$codice_unita)
  
  df_balance[,is_over_size_imp := ifelse(codice_unita %in% over_size_importers,1,0)]
  
  summary_stats <- df_balance[, list(.N, n_dip = sum(addetti, na.rm = T)) , keyby = list(NC2)][order(-N)]
  summary_stats[,agg_n_dip := sum(n_dip, na.rm = T)]
  summary_stats[,share_dip := round(100*(n_dip/agg_n_dip),4)]
  summary_stats[,agg_N := sum(N, na.rm = T)]
  summary_stats[,share_N := round(100*(N/agg_N),4)]
  
  
  summary_stats[order(share_dip,share_N)][,list(NC2,N,n_dip,share_dip,share_N)]
  
  pv_tax_sb <- c(20,21,26,61,62,72)
  pv_tax_ss <- c(27,28,30,33,68,69,70,71,73,74,77,82)
  pv_tax_sii <- c(17,1,19,22,23,24,29,58,59,60,63,64,65,66)
  pv_tax_sd <- c(10,11,12,13,14,15,16,25,31,32,45,46,47,49,50,51,52,53,55,56,75,78,79,80,81)
  
  
  df_balance[,pavitt := ifelse(NC2 %in% pv_tax_sb,'pv_tax_sb',
                               ifelse(NC2 %in% pv_tax_ss,'pv_tax_ss',
                                      ifelse(NC2 %in% pv_tax_ss,'pv_tax_sii',
                                             ifelse(NC2 %in% pv_tax_sd,'pv_tax_sb','Others')))) ]
  
  
  ## NACE 2 to NACE1 sections 
  
  nace_mapping <- list(
    "ABCD" = c("01", "02", "03","05", "06","07", "08","09", "10"),
    "E" = c("11", "12", "13", "14", "15", "16"),
    "FGH" = c("17", "18", "19", "20", "21", "22","23","24"),
    "I" = c("25", "26", "27", "28", "29", "30", "31", "32", "33"),
    "J" = c("35", "36", "37", "38", "39", "41"),
    "KLM" = c("42", "43","45", "46","47", "49"),
    "N" = c("50", "51", "52", "53"),
    "O" = c("55", "56", "58"),
    "P" = c("59", "60", "61", "62", "63"),
    "Q" = c("64", "65", "66", "68"),
    "RS" = c("69", "70","71", "72", "73"),
    "TU" = c("74", "75","77", "78"),
    "V" = c("79", "80", "81", "82", "84"),
    "W" = c("85", "86", "87", "88", "90"),
    "X" = c("91", "92", "93"),
    "YZ" = c("94","95", "96", "97", "99")
  )
  
  # Function to map NACE2 code to NACE1 section
  nace_map <- function(nace2_code) {
    nace1_section <- NA
    for (section in names(nace_mapping)) {
      if (nace2_code %in% nace_mapping[[section]]) {
        nace1_section <- section
        break
      }
    }
    return(nace1_section)
  }
  
  df_balance[, NC1 := sapply(NC2, function(x) nace_map(x))]
  
  
  
  ############# Cycling over vars and computing the estimates ###############
  df_balance$NC2 <- factor(df_balance$NC2)
  
  return(df_balance)
}


subsampling <- function(df_balance,rec,tp)
{
  
  
  if(rec == 'Re-exporters exclusion - Minimal'){
    df_filtered <- df_balance[is_reexporter_count == 0]
  }else if(rec == 'Re-exporters exclusion - Ample'){
    df_filtered <- df_balance[is_reexporter_simple == 0]
  }else if(rec == 'Re-exporters exclusion - None'){
    df_filtered <- df_balance
  }else if(rec == 'Excluding persistent importers'){
    df_filtered <- df_balance[is_persistent_importer == 0]
  }else if(rec == 'Excluding over-sized importers'){
    df_filtered <- df_balance[is_over_size_imp == 0]
  }
  
  
  if(tp == 'Manufacturing - Adopters only'){
    sub_sample <- df_filtered[is_adopter == 1 & is_man == 1]
  }else if(tp == 'Manufacturing'){
    sub_sample <- df_filtered[is_man == 1]
  }else if(tp == 'Non-Manufacturing - Adopters only'){
    sub_sample <- df_filtered[is_adopter == 1 & is_man == 0]
  }else if(tp == 'Non-Manufacturing'){
    sub_sample <- df_filtered[is_man == 0]
  }else if(tp == 'Total economy - Adopters only'){
    sub_sample <- df_filtered[is_adopter == 1]
  }else if(tp == 'Total economy'){
    sub_sample <- df_filtered
  }else if(tp == 'Science based'){
    sub_sample <- df_filtered[NC2 %in% pv_tax_sb]
  }else if(tp == 'Specialized suppliers'){
    sub_sample <- df_filtered[NC2 %in% pv_tax_ss]
  }else if(tp == 'Scale and information intensive'){
    sub_sample <- df_filtered[NC2 %in% pv_tax_sii]
  }else if(tp == 'Suppliers dominated'){
    sub_sample <- df_filtered[NC2 %in% pv_tax_sd]
  }else if(tp == 'Micro/Small firms (0-19)'){
    firms_selection <- unique(df_filtered[,first_year := min(anno, na.rm =T),
                                          by = list(codice_unita)][anno == first_year &
                                                                     dipendenti <= 15]$codice_unita) 
    sub_sample <- df_filtered[codice_unita %in% firms_selection]
  }else if(tp == 'Small firms (20-49)'){
    firms_selection <- unique(df_filtered[,first_year := min(anno, na.rm =T),
                                          by = list(codice_unita)][anno == first_year &
                                                                     dipendenti>15 & dipendenti<=50]$codice_unita) 
    sub_sample <- df_filtered[codice_unita %in% firms_selection]
  }else if(tp == 'Medium and large firms (+50)'){
    firms_selection <- unique(df_filtered[,first_year := min(anno, na.rm =T),
                                          by = list(codice_unita)][anno == first_year &
                                                                     dipendenti>50]$codice_unita) 
    sub_sample <- df_filtered[codice_unita %in% firms_selection]
  }else if(tp == 'Micro/Small firms (0-19) - Adopters only'){
    firms_selection <- unique(df_filtered[,first_year := min(anno, na.rm =T),
                                          by = list(codice_unita)][anno == first_year &
                                                                     dipendenti <= 15]$codice_unita) 
    sub_sample <- df_filtered[is_adopter == 1 & codice_unita %in% firms_selection]
  }else if(tp == 'Small firms (20-49) - Adopters only'){
    firms_selection <- unique(df_filtered[,first_year := min(anno, na.rm =T),
                                          by = list(codice_unita)][anno == first_year &
                                                                     dipendenti>15 & dipendenti<=50]$codice_unita) 
    sub_sample <- df_filtered[is_adopter == 1 & codice_unita %in% firms_selection]
  }else if(tp == 'Medium and large firms (+50) - Adopters only'){
    firms_selection <- unique(df_filtered[,first_year := min(anno, na.rm =T),
                                          by = list(codice_unita)][anno == first_year &
                                                                     dipendenti>50]$codice_unita) 
    sub_sample <- df_filtered[is_adopter == 1 & codice_unita %in% firms_selection]
  }else if(tp =='Total economy (above 15 emp.) - Adopters only'){
    firms_exclusion <- unique(df_filtered[,first_year := min(anno, na.rm =T),
                                          by = list(codice_unita)][anno == first_year & dipendenti<15]$codice_unita) 
    sub_sample <- df_filtered[!(codice_unita %in% firms_exclusion)][is_adopter == 1]
  }else if(tp =='Total economy (above 15 emp.)'){
    firms_exclusion <- unique(df_filtered[,first_year := min(anno, na.rm =T),
                                          by = list(codice_unita)][anno == first_year & dipendenti<15]$codice_unita) 
    sub_sample <- df_filtered[!(codice_unita %in% firms_exclusion)]
  }
  
  
  return(sub_sample)
}



pre_matching <- function(sub_sample,vr)
{
  sub_sample[,sizecl_coarse := cut(dipendenti,c(0,10,50, Inf), c('Micro','Small','Medium-Large'), right = F)]
  
  sub_sample$sizecl_coarse <- factor(sub_sample$sizecl_coarse, levels = c('Micro','Small','Medium-Large'))
  
  sub_sample[,first_year := min(anno, na.rm = T), by = .(codice_unita)]
  
  sub_sample[,min_year := min(anno[!is.na(sizecl_coarse)], na.rm = T), 
             by = .(codice_unita)][,first_size_class := unique(sizecl_coarse[anno==min_year]), by = .(codice_unita)]
  
  sub_sample[,min_year := min(anno[!is.na(ln_sales)], na.rm = T), 
             by = .(codice_unita)][,first_ln_sales := unique(ln_sales[anno==min_year]), by = .(codice_unita)]
  
  sub_sample[,min_year := min(anno[!is.na(ln_dip)], na.rm = T), 
             by = .(codice_unita)][,first_ln_dip := unique(ln_dip[anno==min_year]), by = .(codice_unita)]
  
  sub_sample[,min_year := min(anno[!is.na(ln_prod)], na.rm = T), 
             by = .(codice_unita)][,first_ln_prod := unique(ln_prod[anno==min_year]), by = .(codice_unita)]
  
  sub_sample[,min_year := min(anno[!is.na(vp)], na.rm = T), 
             by = .(codice_unita)][,first_sales := unique(vp[anno==min_year]), by = .(codice_unita)]
  
  sub_sample[,min_year := min(anno[!is.na(dipendenti)], na.rm = T), 
             by = .(codice_unita)][,first_dip := unique(dipendenti[anno==min_year]), by = .(codice_unita)]
  
  sub_sample[,min_year := min(anno[!is.na(prod)], na.rm = T), 
             by = .(codice_unita)][,first_prod := unique(prod[anno==min_year]), by = .(codice_unita)]
  
  sub_sample[,avg_gr_dip := mean(gr_dip, na.rm = T), by = .(codice_unita)]
  
  sub_sample[,variable := get(vr)]
  
  sub_sample[,avg_gr_var := mean(c(NA,diff(log(1+variable)))*(1-as.numeric(as.character(absorbing_adopter))), na.rm = T), by = .(codice_unita)]
  
  sub_sample[,min_year := min(anno[!is.na(variable)], na.rm = T), 
             by = .(codice_unita)][,first_variable := unique(variable[anno==min_year]), by = .(codice_unita)]
  
  sub_sample$is_exporter <- as.factor(sub_sample$is_exporter)
  
  sub_sample$is_man <- as.factor(sub_sample$is_man)
  
  sub_sample$first_size <- as.factor(sub_sample$first_size)
  
  return(sub_sample)
}
  

matching <- function(sub_sample,tp,vr,  n_match = 2)
{

      sub_sample[,sizecl_coarse := cut(dipendenti,c(0,10,50, Inf), c('Micro','Small','Medium-Large'), right = F)]
      
      sub_sample$sizecl_coarse <- factor(sub_sample$sizecl_coarse, levels = c('Micro','Small','Medium-Large'))
      
      sub_sample[,first_year := min(anno, na.rm = T), by = .(codice_unita)]
      
      sub_sample[,min_year := min(anno[!is.na(sizecl_coarse)], na.rm = T), 
                 by = .(codice_unita)][,first_size_class := unique(sizecl_coarse[anno==min_year]), by = .(codice_unita)]
      
      sub_sample[,min_year := min(anno[!is.na(ln_sales)], na.rm = T), 
                 by = .(codice_unita)][,first_ln_sales := unique(ln_sales[anno==min_year]), by = .(codice_unita)]
      
      sub_sample[,min_year := min(anno[!is.na(ln_dip)], na.rm = T), 
                 by = .(codice_unita)][,first_ln_dip := unique(ln_dip[anno==min_year]), by = .(codice_unita)]
      
      sub_sample[,min_year := min(anno[!is.na(ln_prod)], na.rm = T), 
                 by = .(codice_unita)][,first_ln_prod := unique(ln_prod[anno==min_year]), by = .(codice_unita)]
      
      sub_sample[,min_year := min(anno[!is.na(vp)], na.rm = T), 
                 by = .(codice_unita)][,first_sales := unique(vp[anno==min_year]), by = .(codice_unita)]
      
      sub_sample[,min_year := min(anno[!is.na(dipendenti)], na.rm = T), 
                 by = .(codice_unita)][,first_dip := unique(dipendenti[anno==min_year]), by = .(codice_unita)]
      
      sub_sample[,min_year := min(anno[!is.na(prod)], na.rm = T), 
                 by = .(codice_unita)][,first_prod := unique(prod[anno==min_year]), by = .(codice_unita)]
      
      sub_sample[,avg_gr_dip := mean(gr_dip, na.rm = T), by = .(codice_unita)]
      
      sub_sample[,variable := get(vr)]
      
      sub_sample[,avg_gr_var := mean(c(NA,diff(log(1+variable)))*(1-as.numeric(as.character(absorbing_adopter))), na.rm = T), by = .(codice_unita)]
      
      sub_sample[,min_year := min(anno[!is.na(variable)], na.rm = T), 
                 by = .(codice_unita)][,first_variable := unique(variable[anno==min_year]), by = .(codice_unita)]
      
      sub_sample$is_exporter <- as.factor(sub_sample$is_exporter)
      
      sub_sample$is_man <- as.factor(sub_sample$is_man)
      
      sub_sample$first_size <- as.factor(sub_sample$first_size)
      
      
      
      ### Go with the matching if the population has never treated units
      if(grepl('Adopters only',tp))
      {
        redux_sample <- sub_sample
        return(list(redux = redux_sample, redux_minimal = redux_sample))
        
      }else{
      
        ###### Reducing the control units by means of matching (using first values in the dataset)
        
        main_ateco <-  unique(sub_sample[, list(codice_unita,ateco,NC2,vp)])[, list(.N, sal_val = sum(vp, na.rm = T)), by = list(codice_unita,ateco,NC2)]
        main_ateco <- main_ateco[order(codice_unita,-sal_val,-N)]
        main_ateco[,main_ateco := ateco[1], by = list(codice_unita)]
        main_ateco[,main_NC2 := NC2[1], by = list(codice_unita)]
        main_ateco <- unique(main_ateco[,list(codice_unita,main_ateco, main_NC2)])
        
        sample_for_matching <- unique(sub_sample[,list(codice_unita,is_adopter, is_exporter,avg_gr_dip,
                                                       first_dip,first_ln_dip,first_ln_sales,
                                                       first_size_class,first_ln_prod,
                                                       first_variable,avg_gr_var)])
        
        sample_for_matching <- merge(sample_for_matching,main_ateco, by = 'codice_unita', all.x = T)
        sample_for_matching <- unique(sample_for_matching)
        
        
        sample_for_matching <- sample_for_matching[complete.cases(sample_for_matching)]
        
        sample_for_matching[,NC2 := main_NC2]
        sample_for_matching[,ateco := main_ateco]
        sample_for_matching[,is_adopter := factor(is_adopter)]
        
        
        ###### Select the ateco sectors where there are adopters and count adopters/non-adopters per sector
        ateco_census <- sample_for_matching[ateco %in% sample_for_matching[is_adopter == 1]$ateco,.N, by = list(ateco,is_adopter)][order(ateco)]
        NC2_census <- sample_for_matching[NC2 %in% sample_for_matching[is_adopter == 1]$NC2,.N, by = list(NC2,is_adopter)][order(NC2)]
        
        NC2_without_non_adopters <- NC2_census[,.N,by = NC2][N<2]$NC2
        NC2_with_few_non_adopters <- NC2_census[is_adopter == 0 & N<3]$NC2
        
        NC2_selected_in_full <- union(NC2_with_few_non_adopters,NC2_without_non_adopters)
        
        NC2_for_matching <- setdiff(unique(NC2_census$NC2),NC2_selected_in_full)
        
        
        cat('\nReducing the sample with preliminary matching...\n')
        
        ### Perform the matching NC2-by-NC2
        selected_firms <- NULL
        for(nc2 in NC2_for_matching)
        {
          # Try matching with all variables
          m.out <- tryCatch({
            matchit(is_adopter ~ first_variable + avg_gr_var + first_dip + avg_gr_dip,
                    data = sample_for_matching[(NC2 == nc2) & (ateco %in% ateco_census$ateco)],
                    method = "nearest", replace = FALSE, ratio = n_match, m.order = "random", distance = 'scaled_euclidean')
          }, error = function(e) {NULL}
          )
          
          # If the first matching fails, try matching with the reduced set of variables
          if (is.null(m.out))
          {
            m.out <- tryCatch({
              matchit(is_adopter ~ first_dip + avg_gr_dip,
                      data = sample_for_matching[(NC2 == nc2) & (ateco %in% ateco_census$ateco)],
                      method = "nearest", replace = FALSE, ratio = n_match, m.order = "random", distance = 'scaled_euclidean')
            }, error = function(e) {NULL}
            )
          }
          
          # If both matchings fail, select random non-adopters
          if (is.null(m.out))
          {
            non_adopters <- sample_for_matching[(NC2 == nc2) & (ateco %in% ateco_census$ateco) & (is_adopter == 0), list(codice_unita)]$codice_unita
            selected_firms <- union(sample(non_adopters, size = min(2, length(non_adopters)), replace = FALSE), selected_firms)
          }else{
            selected_firms <- union(match.data(m.out)$codice_unita, selected_firms)
          }
          
        }
        
        for(nc2 in NC2_selected_in_full){selected_firms <- union(selected_firms,sample_for_matching[NC2 == nc2]$codice_unita)}
        
        
        
        ### Perform the matching NC2-by-NC2
        selected_firms_minimal <- NULL
        for(nc2 in NC2_for_matching)
        {
          # Try matching with all variables
          m.out <- tryCatch({
            matchit(is_adopter ~ first_variable + avg_gr_var + first_dip + avg_gr_dip,
                    data = sample_for_matching[(NC2 == nc2) & (ateco %in% ateco_census$ateco)],
                    method = "nearest", replace = FALSE, ratio = 1, m.order = "random", distance = 'scaled_euclidean')
          }, error = function(e) {NULL}
          )
          
          # If the first matching fails, try matching with the reduced set of variables
          if (is.null(m.out))
          {
            m.out <- tryCatch({
              matchit(is_adopter ~ first_dip + avg_gr_dip,
                      data = sample_for_matching[(NC2 == nc2) & (ateco %in% ateco_census$ateco)],
                      method = "nearest", replace = FALSE, ratio = 1, m.order = "random", distance = 'scaled_euclidean')
            }, error = function(e) {NULL}
            )
          }
          
          # If both matchings fail, select random non-adopters
          if (is.null(m.out))
          {

            non_adopters <- sample_for_matching[(NC2 == nc2) & (ateco %in% ateco_census$ateco) & (is_adopter == 0), list(codice_unita)]$codice_unita
            selected_firms_minimal <- union(sample(non_adopters, size = min(1, length(non_adopters)), replace = FALSE), selected_firms)
          }else{
            selected_firms_minimal <- union(match.data(m.out)$codice_unita, selected_firms_minimal)
          }
          
        }
        
        for(nc2 in NC2_selected_in_full){selected_firms_minimal <- union(selected_firms_minimal,sample_for_matching[NC2 == nc2]$codice_unita)}
        
        
        
        cat('\nSample caractheristics before the matching:\n')
        m.out1 <- matchit(is_adopter ~ first_variable  + avg_gr_var  + first_dip + avg_gr_dip,
                          data = sample_for_matching,
                          method = NULL, replace = F)
        print(summary(m.out1))
        
        cat('\nSample caractheristics after the matching (ratio 1):\n')
        m.out2 <- matchit(is_adopter ~ first_variable  + avg_gr_var  + first_dip + avg_gr_dip,
                          data = sample_for_matching[codice_unita %in% selected_firms],
                          method = NULL, replace = F)
        print(summary(m.out2))
        
        cat('\nSample caractheristics before the matching (ratio 2):\n')
        m.out3 <- matchit(is_adopter ~ first_variable  + avg_gr_var + first_dip + avg_gr_dip,
                          data = sample_for_matching[codice_unita %in% selected_firms_minimal],
                          method = NULL, replace = F)
        print(summary(m.out3))
        
        
        
        redux_sample <- sub_sample[codice_unita %in% selected_firms,]
        redux_sample_minimal <- sub_sample[codice_unita %in% selected_firms_minimal,]    
        
        
        
        return(list(redux = redux_sample, redux_minimal = redux_sample_minimal))
      }
}  



estimation <- function(redux_sample,identification_spike, vr, tp, rec, control_type, anticipation_num, estimation_label, 
                       formula_string,
                       wgt = NULL,
                       folder_exercise)
{  
  
  
  ###### Start pretrend testing
  

  ant_label <- paste0('Ant. years = ',anticipation_num)
  
 
      
      tryCatch({ 
        mw.attgt <- att_gt(yname = vr,
                           gname = "year_treated",
                           idname = "codice_unita",
                           tname = "anno",
                           xformla = as.formula(formula_string),
                           data = redux_sample,
                           clustervars = 'codice_unita',
                           anticipation = anticipation_num,
                           weightsname = wgt,
                           control_group = control_type,
                           allow_unbalanced_panel = TRUE)
        agg.dyn <- aggte(mw.attgt, type = "dynamic",na.rm = T)
        
        
        
        estimates_dt <- data.table(formula = gsub('~ ', '',gsub(' + ',', ',formula_string)),
                                   pretrend_pval = as.numeric(mw.attgt$Wpval),
                                   att = agg.dyn$overall.att, 
                                   se = agg.dyn$overall.se,
                                   cv = agg.dyn$crit.val.egt,
                                   signif = ifelse(agg.dyn$overall.att - agg.dyn$crit.val.egt*agg.dyn$overall.se > 0 |
                                                     agg.dyn$overall.att + agg.dyn$crit.val.egt*agg.dyn$overall.se < 0,
                                                   1,0),
                                   signif_light = ifelse(agg.dyn$overall.att - 1.96*agg.dyn$overall.se > 0 |
                                                           agg.dyn$overall.att + 1.96*agg.dyn$overall.se < 0,
                                                         1,0),
                                   spike = identification_spike,
                                   variable = vr,
                                   sample_type = tp,
                                   exp_excl = rec,
                                   balanced_panel = 'unbalanced',
                                   weighting = ifelse(is.null(wgt),'Non-weighted',paste0(wgt,' - Weighted')),
                                   control = control_type,
                                   time = agg.dyn$egt,
                                   anticipation = ant_label,
                                   ATT = agg.dyn$att.egt,
                                   ATT_se = agg.dyn$se.egt,
                                   crit_val = agg.dyn$crit.val.egt,
                                   pre_test_pval = as.numeric(mw.attgt$Wpval),
                                   ATT_overall = agg.dyn$overall.att,
                                   ATT_overall_se = agg.dyn$overall.se)
        
      }, error = function(e){
        
        estimates_dt <- data.table(formula = gsub('~ ', '',gsub(' + ',', ',formula_string)),
                                 pretrend_pval = NA,
                                 att = NA, 
                                 se = NA,
                                 cv = NA,
                                 signif = NA,
                                 signif_light = NA,
                                 spike = identification_spike,
                                 variable = vr,
                                 sample_type = tp,
                                 exp_excl = rec,
                                 balanced_panel = 'unbalanced',
                                 weighting = ifelse(is.null(wgt),'Non-weighted',paste0(wgt,' - Weighted')),
                                 control = control_type,
                                 time = NA,
                                 anticipation = ant_label,
                                 ATT = NA,
                                 ATT_se = NA,
                                 crit_val = NA,
                                 pre_test_pval = NA,
                                 ATT_overall = NA,
                                 ATT_overall_se = NA,
                                 error = e)
        
      })
      
   

  #setDT(estimates_dt)
  
  pretrend_trials <- unique(estimates_dt[,1:10])
  estimates_dt <- estimates_dt[,list(spike,variable,sample_type,exp_excl,
                                      balanced_panel,weighting,control, formula,time,
                                      anticipation, ATT, ATT_se,crit_val,
                                      pre_test_pval,ATT_overall,ATT_overall_se)]
  
  
  
  #dir.create(here::here('DiD_results',folder_exercise))
 # dir.create(here::here('DiD_results',folder_exercise,'pretest_results'))
 # fwrite(pretrend_trials, file = here::here('DiD_results',
 #                                     folder_exercise,'pretest_results',
 #                                     paste0(estimation_label,'_pretrend_trials.csv')))
  
  
  
  return(estimates_dt)
  
}



estimation_calendar <- function(redux_sample,identification_spike, vr, tp, rec, control_type, anticipation_num, estimation_label, 
                               formula_string,
                               wgt = NULL,
                               folder_exercise)
{  
  
  
  ###### Start pretrend testing
  
  
  ant_label <- paste0('Ant. years = ',anticipation_num)
  
  
  
  tryCatch({ 
    mw.attgt <- att_gt(yname = vr,
                       gname = "year_treated",
                       idname = "codice_unita",
                       tname = "anno",
                       xformla = as.formula(formula_string),
                       data = redux_sample,
                       clustervars = 'codice_unita',
                       anticipation = anticipation_num,
                       weightsname = wgt,
                       control_group = control_type,
                       allow_unbalanced_panel = TRUE)
    
    agg.dyn <- aggte(mw.attgt, type = "group",na.rm = T)
    
    
    
    estimates_dt <- data.table(formula = gsub('~ ', '',gsub(' + ',', ',formula_string)),
                               pretrend_pval = as.numeric(mw.attgt$Wpval),
                               att = agg.dyn$overall.att, 
                               se = agg.dyn$overall.se,
                               cv = agg.dyn$crit.val.egt,
                               signif = ifelse(agg.dyn$overall.att - agg.dyn$crit.val.egt*agg.dyn$overall.se > 0 |
                                                 agg.dyn$overall.att + agg.dyn$crit.val.egt*agg.dyn$overall.se < 0,
                                               1,0),
                               signif_light = ifelse(agg.dyn$overall.att - 1.96*agg.dyn$overall.se > 0 |
                                                       agg.dyn$overall.att + 1.96*agg.dyn$overall.se < 0,
                                                     1,0),
                               spike = identification_spike,
                               variable = vr,
                               sample_type = tp,
                               exp_excl = rec,
                               balanced_panel = 'unbalanced',
                               weighting = ifelse(is.null(wgt),'Non-weighted',paste0(wgt,' - Weighted')),
                               control = control_type,
                               group = agg.dyn$egt,
                               anticipation = ant_label,
                               ATT = agg.dyn$att.egt,
                               ATT_se = agg.dyn$se.egt,
                               crit_val = agg.dyn$crit.val.egt,
                               pre_test_pval = as.numeric(mw.attgt$Wpval),
                               ATT_overall = agg.dyn$overall.att,
                               ATT_overall_se = agg.dyn$overall.se)
    
  }, error = function(e){
    
    estimates_dt <- data.table(formula = gsub('~ ', '',gsub(' + ',', ',formula_string)),
                               pretrend_pval = NA,
                               att = NA, 
                               se = NA,
                               cv = NA,
                               signif = NA,
                               signif_light = NA,
                               spike = identification_spike,
                               variable = vr,
                               sample_type = tp,
                               exp_excl = rec,
                               balanced_panel = 'unbalanced',
                               weighting = ifelse(is.null(wgt),'Non-weighted',paste0(wgt,' - Weighted')),
                               control = control_type,
                               time = NA,
                               anticipation = ant_label,
                               ATT = NA,
                               ATT_se = NA,
                               crit_val = NA,
                               pre_test_pval = NA,
                               ATT_overall = NA,
                               ATT_overall_se = NA,
                               error = e)
    
  })
  
  
  
  #setDT(estimates_dt)

  estimates_dt <- estimates_dt[,list(spike,variable,sample_type,exp_excl,
                                     balanced_panel,weighting,control, formula,group,
                                     anticipation, ATT, ATT_se,crit_val,
                                     pre_test_pval,ATT_overall,ATT_overall_se)]
  
  
  
  #dir.create(here::here('DiD_results',folder_exercise))
  # dir.create(here::here('DiD_results',folder_exercise,'pretest_results'))
  # fwrite(pretrend_trials, file = here::here('DiD_results',
  #                                     folder_exercise,'pretest_results',
  #                                     paste0(estimation_label,'_pretrend_trials.csv')))
  
  
  
  return(estimates_dt)
  
}