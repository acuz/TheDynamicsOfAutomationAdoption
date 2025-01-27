#######################################
######### Dataset preparation #########
#######################################

## This script transofrms the yearly linked employer-employee datasets       
## It provides a dataset containing the main statistics on the distribution of salaries and 
## shares of the workforce along different dimensions, e.g. occupational categories, age, tenure, contract types

## The script works with the following steps:
## 1. Takes as inputs the yearly datasets containing the information on the universe of Italian workforce from the folder path source_datasets/leed_datasets
## 2. Cycling over the years (2011-2019) operates i) performing minimal cleaning operations on each year dataset;
##    ii) defining a set of workforce-related statistics at the firm-level.
## 3. Writes to disk the dataset for each year and the unique dataset containing the firm-level information on the workforce in the span 2011-2019

## Main output: firm_dataset_leed_variables.csv
## Stored in the folder: source_datasets

#### Import packages ####
library(here)
library(data.table)


#### Extract the list of files storing LEED datasets from the folder path source_datasets/leed_datasets ####

dataset_names_list <- list.files(here('source_datasets','leed_datasets'),pattern = 'asiaocc.*csv')
firm_datasets_byyear <- list()

#### Cycle over the list of files, elaborating data yer-by-year and then stacking the firm-level datasets into a single final dataset ####
for(dataset_name in dataset_names_list) 
{
  
  #### Import yearly file 
  cat('\nImporting csv file: ',dataset_name,'\n')
  dt <- fread(here('source_datasets','leed_datasets',dataset_name))
  setDT(dt)
  
  print(unique(dt$anno)) ## Print the year to keep track of the cycle advancements
  
  # Clean 1: Remove pos_lav_media <= 0, and force pos_lav_media below 1 (this also excludes NA from the dataset)
  dt <- dt[pos_lav_media>0]
  dt[pos_lav_media > 1, pos_lav_media := 1]
  
  # Clean 2: not imputing missing gender, just use NA
  dt[gender == 9, gender:= NA]
  
  # Clean 3: not imputing missing age, just use NA
  dt[cl_eta == 9, cl_eta:= NA]
  
  # Computing annual income using weeks of actually worked
  dt[,retr_ann := retr/pos_lav_media]
  
  # Calculate firm average salary per week
  dt[, retr_sett := retr_ann/52]
  
  ## Firm level statistics of the salaries distribution
  dt[, avg_retr_sett := mean(retr_sett, na.rm = T), by = .(codice_unita, anno,lavoratore)]
  dt[, sd_retr_sett := sd(retr_sett, na.rm = T), by = .(codice_unita, anno, lavoratore)]
  dt[, IQR_retr_sett := IQR(retr_sett, na.rm = T), by = .(codice_unita, anno, lavoratore)]
  dt[, med_retr_sett := median(retr_sett, na.rm = T), by = .(codice_unita, anno, lavoratore)]
  dt[, bottom10_retr_sett := quantile(retr_sett, probs = .1, na.rm = T), by = .(codice_unita, anno,lavoratore)]
  dt[, top10_retr_sett := quantile(retr_sett, probs = .9, na.rm = T), by = .(codice_unita, anno,lavoratore)]
  dt[, top1_retr_sett := quantile(retr_sett, probs = .99, na.rm = T), by = .(codice_unita, anno, lavoratore)]
  dt[, span_retr_sett := max(retr_sett, na.rm = T) - min(retr_sett, na.rm = T), by = .(codice_unita, anno, lavoratore)]
  dt[span_retr_sett == -Inf]$span_retr_sett <- NA
  
  ## Firm level statistics of the log salaries distribution
  dt[, avg_ln_retr_sett := mean(log(1+retr_sett), na.rm = T), by = .(codice_unita, anno, lavoratore)]
  dt[, sd_ln_retr_sett := sd(log(1+retr_sett), na.rm = T), by = .(codice_unita, anno, lavoratore)]
  dt[, IQR_ln_retr_sett := IQR(log(1+retr_sett), na.rm = T), by = .(codice_unita, anno, lavoratore)]
  dt[, med_ln_retr_sett := median(log(1+retr_sett), na.rm = T), by = .(codice_unita, anno, lavoratore)]
  dt[, bottom10_ln_retr_sett := quantile(log(1+retr_sett), probs = .1, na.rm = T), by = .(codice_unita, anno, lavoratore)]
  dt[, top10_ln_retr_sett := quantile(log(1+retr_sett), probs = .9, na.rm = T), by = .(codice_unita, anno, lavoratore)]
  dt[, top1_ln_retr_sett := quantile(log(1+retr_sett), probs = .99, na.rm = T), by = .(codice_unita, anno, lavoratore)]
  dt[, span_ln_retr_sett := max(log(1+retr_sett), na.rm = T) - min(retr_sett, na.rm = T), by = .(codice_unita, anno, lavoratore)]
  dt[span_ln_retr_sett == -Inf]$span_ln_retr_sett <- NA
  
  ## Group and rename by tenures
  dt[,tenure_class := ifelse(tenure<4,'1-3',
                             ifelse(tenure<11,'3-10',
                                    ifelse(tenure>10,'>10',
                                           ifelse(tenure>30,'>30','Other'))))]
  
  ## Group and rename occupation categories variables (according to metada info)
  dt[,qualif1 := ifelse(qualif1 %in% c(5),'Manager',
                        ifelse(qualif1 %in% c(3),'Middle_management',
                               ifelse(qualif1 %in% c(1),'Blue_collar',
                                      ifelse(qualif1 %in% c(4),'Trainee',
                                             ifelse(qualif1 %in% c(2),'White_collar','Others')))))]
  
  ## Group and rename part_time/full_time variable (according to metadata info)
  dt[,qualif2 := ifelse(qualif2 %in% c(1),'full_time',
                        ifelse(qualif2 %in% c(2),'part_time','full_part_NA'))]
  
  ## Group and rename permanent vs temporary contract (according to metadata info)
  dt[,qualif3 := ifelse(qualif3 %in% c(1),'permanent',
                        ifelse(qualif3 %in% c(2),'temporary','perm_temp_NA'))]
  
  
  ## Group and rename education variable (according to metadata info)
  dt[,titolo_dbocc_1 := ifelse(titolo_dbocc_1 %in% c(10,20),'low',
                               ifelse(titolo_dbocc_1 %in% c(30,40),'medium',
                                      ifelse(titolo_dbocc_1 %in% c(50,60,70),'high','titolo_NA')))]
  
  
  ## Grouping and calculations of aggregates and means per class
  
  ## Employees shares per class
  dt[,agg_q1:= sum(pos_lav_media, na.rm = T), by = .(codice_unita,anno,qualif1,lavoratore)][,sh_q1:= agg_q1/sum(pos_lav_media), by =.(codice_unita,anno, lavoratore)]
  dt[,agg_q2:= sum(pos_lav_media, na.rm = T), by = .(codice_unita,anno,qualif2,lavoratore)][,sh_q2:= agg_q2/sum(pos_lav_media), by =.(codice_unita,anno, lavoratore)]
  dt[,agg_q3:= sum(pos_lav_media, na.rm = T), by = .(codice_unita,anno,qualif3,lavoratore)][,sh_q3:= agg_q3/sum(pos_lav_media), by =.(codice_unita,anno, lavoratore)]
  dt[,agg_titolo:= sum(pos_lav_media, na.rm = T), by = .(codice_unita,anno,titolo_dbocc_1,lavoratore)][,sh_titolo:= agg_titolo/sum(pos_lav_media), by =.(codice_unita,anno, lavoratore)]
  dt[,agg_cl_eta:= sum(pos_lav_media, na.rm = T), by = .(codice_unita,anno,cl_eta, lavoratore)][,sh_cl_eta:= agg_cl_eta/sum(pos_lav_media), by =.(codice_unita,anno, lavoratore)]
  dt[,agg_tenure_class:= sum(pos_lav_media, na.rm = T), by = .(codice_unita,anno,tenure_class, lavoratore)][,sh_tenure_class:= agg_tenure_class/sum(pos_lav_media), by =.(codice_unita,anno, lavoratore)]
  dt[,agg_gender:= sum(pos_lav_media, na.rm = T), by = .(codice_unita,anno,gender, lavoratore)][,sh_gender:= agg_gender/sum(pos_lav_media), by =.(codice_unita,anno, lavoratore)]
  
  
  ## Salary average per class
  dt[,ln_retr_q1:= log(1+mean(retr_sett, na.rm = T)), by = .(codice_unita,anno,qualif1, lavoratore)]
  dt[,ln_retr_q2:= log(1+mean(retr_sett, na.rm = T)), by = .(codice_unita,anno,qualif2, lavoratore)]
  dt[,ln_retr_q3:= log(1+mean(retr_sett, na.rm = T)), by = .(codice_unita,anno,qualif3, lavoratore)]
  dt[,ln_retr_titolo:= log(1+mean(retr_sett, na.rm = T)), by = .(codice_unita,anno,titolo_dbocc_1, lavoratore)]
  dt[,ln_retr_eta:= log(1+mean(retr_sett, na.rm = T)), by = .(codice_unita,anno,cl_eta, lavoratore)]
  dt[,ln_retr_tenure:= log(1+mean(retr_sett, na.rm = T)), by = .(codice_unita,anno,tenure_class, lavoratore)]
  dt[,ln_retr_gender:= log(1+mean(retr_sett, na.rm = T)), by = .(codice_unita,anno,gender, lavoratore)]
  
  
  ## Store a vector of firm-level vars which will be stored in the single year firm-level dataset
  
  firm_level_vars <- c('anno','codice_unita','codice_reg','fl_idbase','dipendenti','addetti',
                       'ateco','sizecl','beniserv','personale','prcf','vp','ci','fg',
                       'sal_stip','vagg','mol','ore_lav',
                       "tot_import_val","tot_import_xue_val","tot_import_intra_val",
                       "tot_export_val","tot_export_xue_val", 
                       "imp_aut_ai","imp_autom","imp_ai","imp_robot",
                       "exp_aut_ai","exp_autom","exp_ai", "exp_robot", 
                       "spike_auto","spike_ai", "spike_aut_ai","spike_rob", 
                       "lavoratore",                                             ## DIP/INDIP
                       "avg_retr_sett","sd_retr_sett","IQR_retr_sett",
                       "med_retr_sett","bottom10_retr_sett",'top10_retr_sett','top1_retr_sett',
                       "span_retr_sett","avg_ln_retr_sett","sd_ln_retr_sett","IQR_ln_retr_sett",   
                       "med_ln_retr_sett",'top10_ln_retr_sett','bottom10_ln_retr_sett','top1_ln_retr_sett','span_ln_retr_sett',
                       'qualif1','sh_q1','qualif2','sh_q2','qualif3','sh_q3','gender','sh_gender',
                       'titolo_dbocc_1','sh_titolo','cl_eta','sh_cl_eta','tenure_class','sh_tenure_class','ln_retr_tenure',
                       'ln_retr_q1',"ln_retr_q2",'ln_retr_q3','ln_retr_titolo','ln_retr_eta','ln_retr_gender')
  
  
  ## Define the firm-level dataset selecting the variables stored and the eliminate mulitplicity taking distince values (by selection) 
  dataset_firms <- unique(dt[,firm_level_vars, with = F])
  
  
  ### Creating salaries variables (in order to split and to have ready columns names)
  dataset_firms[ , retr_qualif1 := paste0('ln_retr_',qualif1)]
  dataset_firms[ , retr_qualif2 := paste0('ln_retr_',qualif2)]
  dataset_firms[ , retr_qualif3 := paste0('ln_retr_',qualif3)]
  dataset_firms[ , retr_titolo_dbocc_1 := paste0('ln_retr_titolo_',titolo_dbocc_1)] ## education classes
  dataset_firms[ , retr_cl_eta := paste0('ln_retr_cl_eta_',cl_eta)] ## age classes
  dataset_firms[ , retr_tenure_class := paste0('ln_retr_tenure_class_',tenure_class)] ## tenure classes
  dataset_firms[ , retr_gender := paste0('ln_retr_gender_',gender)] ## gender classes
  
  ### Creating shares variables (in order to split and to have ready columns names)
  dataset_firms[ , qualif1 := paste0('sh_',qualif1)]
  dataset_firms[ , qualif2 := paste0('sh_',qualif2)]
  dataset_firms[ , qualif3 := paste0('sh_',qualif3)]
  dataset_firms[ , titolo_dbocc_1 := paste0('sh_titolo_',titolo_dbocc_1)]
  dataset_firms[ , cl_eta := paste0('sh_cl_eta_',cl_eta)]
  dataset_firms[ , tenure_class := paste0('sh_tenure_class_',tenure_class)]
  dataset_firms[ , gender := paste0('sh_gender_',gender)]
  
  
  ## Isolate the columns belonging to the core of the dataset (namely those for which an expansion
  ## to wide form is not necessary)
  ## (NOTE: the following command is order dependent, selecting the columns manually is preferable.
  ##        Just exclude all the variables that are selected in the "data_cut" datasets below)
  
  data_bulk <- unique(dataset_firms[,1:(which(colnames(dataset_firms)=='qualif1')-1)])
  setkey(data_bulk,codice_unita,anno, lavoratore)
  
  
  
  ## Use different chunk of code to expand the datasets from long to wide form

  ## Operate on qualif1 shares (occupational categories, mangaers, white collars, blue collars...)
  data_cut <- dataset_firms[,list(codice_unita,anno, lavoratore,qualif1,sh_q1)]
  data_cut <- dcast(unique(data_cut), ... ~ qualif1, value.var = 'sh_q1')
  data_cut[is.na(data_cut)] <- 0
  setkey(data_cut,codice_unita, anno, lavoratore)
  data_bulk <- merge(data_bulk,data_cut)
  
  ## Operate on qualif2 shares (part vs full time)
  data_cut <- dataset_firms[,list(codice_unita,anno,lavoratore,qualif2,sh_q2)]
  data_cut <- dcast(unique(data_cut), ... ~ qualif2, value.var = 'sh_q2')
  data_cut[is.na(data_cut)] <- 0
  setkey(data_cut,codice_unita, anno, lavoratore)
  data_bulk <- merge(data_bulk,data_cut)
  
  ## Operate on qualif3 shares (permanent vs temporary contracts)
  data_cut <- dataset_firms[,list(codice_unita,anno, lavoratore,qualif3,sh_q3)]
  data_cut <- dcast(unique(data_cut), ... ~ qualif3, value.var = 'sh_q3')
  data_cut[is.na(data_cut)] <- 0
  setkey(data_cut,codice_unita, anno, lavoratore)
  data_bulk <- merge(data_bulk,data_cut)
  
  ## Operate on titolo share (education)
  data_cut <- dataset_firms[,list(codice_unita, anno, lavoratore, titolo_dbocc_1, sh_titolo)]
  data_cut <- dcast(unique(data_cut), ... ~ titolo_dbocc_1, value.var = 'sh_titolo')
  data_cut[is.na(data_cut)] <- 0
  setkey(data_cut,codice_unita, anno, lavoratore)
  data_bulk <- merge(data_bulk,data_cut)
  
  ## Operate on age class shares
  data_cut <- dataset_firms[,list(codice_unita,anno, lavoratore, cl_eta,sh_cl_eta)]
  data_cut <- dcast(unique(data_cut), ... ~ cl_eta, value.var = 'sh_cl_eta')
  data_cut[is.na(data_cut)] <- 0
  setkey(data_cut,codice_unita, anno, lavoratore)
  data_bulk <- merge(data_bulk,data_cut)
  
  ## Operate on tenure class shares
  data_cut <- dataset_firms[,list(codice_unita,anno, lavoratore,tenure_class,sh_tenure_class)]
  data_cut <- dcast(unique(data_cut), ... ~ tenure_class, value.var = 'sh_tenure_class')
  data_cut[is.na(data_cut)] <- 0
  setkey(data_cut,codice_unita, anno, lavoratore)
  data_bulk <- merge(data_bulk,data_cut)
  
  ## Operate on qualif1 salaries (occupational categories, mangaers, white collars, blue collars...)
  data_cut <- dataset_firms[,list(codice_unita,anno, lavoratore,retr_qualif1,ln_retr_q1)]
  data_cut <- dcast(unique(data_cut), ... ~ retr_qualif1, value.var = 'ln_retr_q1')
  setkey(data_cut,codice_unita, anno, lavoratore)
  data_bulk <- merge(data_bulk,data_cut)
  
  ## Operate on qualif2 salaries (part vs full time)
  data_cut <- dataset_firms[,list(codice_unita,anno, lavoratore,retr_qualif2,ln_retr_q2)]
  data_cut <- dcast(unique(data_cut), ... ~ retr_qualif2, value.var = 'ln_retr_q2')
  setkey(data_cut,codice_unita, anno, lavoratore)
  data_bulk <- merge(data_bulk,data_cut)
  
  ## Operate on qualif3 salaries (permanent vs temporary contracts)
  data_cut <- dataset_firms[,list(codice_unita,anno,lavoratore,retr_qualif3,ln_retr_q3)]
  data_cut <- dcast(unique(data_cut), ... ~ retr_qualif3, value.var = 'ln_retr_q3')
  setkey(data_cut,codice_unita, anno, lavoratore)
  data_bulk <- merge(data_bulk,data_cut)
  
  ## Operate on titolo salaries (education)
  data_cut <- dataset_firms[,list(codice_unita,anno, lavoratore,retr_titolo_dbocc_1,ln_retr_titolo)]
  data_cut <- dcast(unique(data_cut), ... ~ retr_titolo_dbocc_1, value.var = 'ln_retr_titolo')
  setkey(data_cut,codice_unita, anno, lavoratore)
  data_bulk <- merge(data_bulk,data_cut)
  
  ## Operate on age class salaries
    data_cut <- dataset_firms[,list(codice_unita,anno, lavoratore,retr_cl_eta,ln_retr_eta)]
  data_cut <- dcast(unique(data_cut), ... ~ retr_cl_eta, value.var = 'ln_retr_eta')
  setkey(data_cut,codice_unita, anno, lavoratore)
  data_bulk <- merge(data_bulk,data_cut)
  
  ## Operate on tenure class salaries
  data_cut <- dataset_firms[,list(codice_unita,anno, lavoratore,retr_tenure_class,ln_retr_tenure)]
  data_cut <- dcast(unique(data_cut), ... ~ retr_tenure_class, value.var = 'ln_retr_tenure')
  setkey(data_cut,codice_unita, anno, lavoratore)
  data_bulk <- merge(data_bulk,data_cut)
  
  ## Operate on gender salaries
  data_cut <- dataset_firms[,list(codice_unita,anno, lavoratore,retr_gender,ln_retr_gender)]
  data_cut <- dcast(unique(data_cut), ... ~ retr_gender, value.var = 'ln_retr_gender')
  setkey(data_cut,codice_unita, anno, lavoratore)
  data_bulk <- merge(data_bulk,data_cut)
  
  year <- unique(data_bulk$anno)
  firm_datasets_byyear[[as.character(year)]] <- data_bulk ## store the firm-level dataset in the list containing all the lists
  rm(data_bulk)
  
  ## Write to disk the year-specific dataset
  name <- paste0('transformed_leed_',substr(dataset_name,1,nchar(dataset_name)-4),'.csv')
  fwrite(dt, file = here('source_datasets','transformed_leed_datasets',name))
  rm(dt)
  gc()
}


## Store the names of the columns of the dataset
allNms <- unique(unlist(lapply(firm_datasets_byyear, names)))

## Merge the datasets selecting only the variables which are valid for all the years
firm_dataset_complete <- do.call(rbind,
                                 c(lapply(firm_datasets_byyear,
                                          function(x) data.frame(c(x, sapply(setdiff(allNms, names(x)),
                                                                             function(y) NA)))),
                                   make.row.names=FALSE))

#### Write to disk the unique dataset containing the firm-level information on the workforce from 2011-2019 ####
fwrite(firm_dataset_complete, file = here('source_datasets','firm_dataset_leed_variables.csv'))




