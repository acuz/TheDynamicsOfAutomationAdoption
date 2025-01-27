######################################################################
#########  Firm-level Difference-in-Differences Analysis    ##########
#########  Assuming different anticipation windows          ##########
######################################################################


# This R script conducts the benchmark Difference-in-Differences (DiD) exercise of the paper accessible at 
# the DOI: https://doi.org/10.1016/j.euroecorev.2024.104943. 
# Firm-level outcomes: employment
#
# INPUT FILES:
# - AUX_DiD_exercise_functions.R: Contains functions for data handling and estimation
# - Raw firm-level data (imported through data_import() function from the folder
#   'elaborated_datasets' containing the dataset 'firm_level_clean_complete_rv24.csv')
#
# OUTPUT FILES 
# - DiD_results/anticipation/spike2_autn_rob_ai_ln_dip_CS_att_results.RData
#   containing estimation results for:
#   * Benchmark employment estimates
#   * Employment weighted estimates 
#   * Size-split estimates for employment
#
#   and different anticipation windows (1,2,3,4 years)
#  
#   
# The analysis is structured in multiple sections:
# 1. Data preparation and reshaping
# 2. Benchmark employment estimates
# 3. Employment-weighted estimates
# 4. Split size estimates for employment
#
###############################################################################


### Clean the environment ###
rm(list = ls())

###### Import libraries ####
library(here)


##### Source the functions for data handling and estimation ####
source(here::here('AUX_DiD_exercise_functions.R'))


##### Import data ####
base_dt <- data_import()

folder_exercise <- 'anticipation'
dir.create(here::here('DiD_results',folder_exercise))

results_par <- list()
results_dts <- list()  

export_dt <- NULL
for(anticipation_num in c(1,2,3,4))
{
  

  spike_type <- 'spike2'
  tech_type <- 'autn_rob_ai'
  
  identification_spike <- paste0(spike_type,'_',tech_type)
  
  #### Reshape data according to spike definitions ####
  df <- data_reshaping(base_dt,
                       spike_type,
                       tech_type)
  
  
  
  
  #########################################################
  ############ Benchmark employment estimates ############# 
  #########################################################
  
  rec <- 'Re-exporters exclusion - Minimal'
  tp <- 'Total economy'
  
  
  cat(rec,' - ',as.character(Sys.time()),'\n')  
  cat('..',tp,' - ',as.character(Sys.time()),'\n')  
  
  ### Subsample data according to reexporting restriction and sampling definitions
  sub_sample <- subsampling(df, 
                            rec = rec,
                            tp = tp)
  
  
  ### Fix variable and do prematching
  vr <- 'ln_dip'
  
  full_sample <- pre_matching(sub_sample,vr)
  
  redux_dts_list <- matching(sub_sample,
                             tp = tp, 
                             vr = vr,
                             n_match = 2) 
  
  
  redux_sample <- redux_dts_list$redux_minimal ### select matching with ratio equal n_match
  
  

  
  if(grepl('Adopters only',tp))
  {
    control_type <- 'notyettreated'
  }else{
    control_type <- 'nevertreated'
  }  
  
  ant_label <- paste0('Ant. years = ',anticipation_num)
  cat('......',ant_label,'\n')  
  
  estimation_label <- paste0(identification_spike,'-',vr,'-',tp,'-',rec,'-',ant_label)
  
  
  
  ### Total economy
  formula_string <- '~ is_exporter + first_size_class + initial_gr_dip + ln_prod + ln_sales + sh_titolo_low + sh_White_collar + sh_full_time' 
  
  estimates_dt <- estimation(redux_sample,
                             identification_spike,
                             vr, tp, rec,
                             control_type = 'nevertreated', 
                             anticipation_num,
                             estimation_label,
                             formula_string,
                             wgt= NULL,
                             folder_exercise) 
  
  estimates_dt[,signif := 0 < ATT_overall - crit_val*ATT_overall_se | 0 > ATT_overall + crit_val*ATT_overall_se]
  
  (estimates_dt)
  
  # ### Total economy - Only adopters
  # colnames(redux_sample)
  # formula_string <- '~ is_exporter + first_size_class + initial_gr_dip + ln_prod + ln_sales + sh_titolo_low + sh_White_collar + sh_full_time' 
  # estimates_dt_oa <- estimation(redux_sample[is_adopter == 1],
  #                               identification_spike,
  #                               vr, tp = 'Total economy - Adopters only', rec,
  #                               control_type = 'notyettreated', 
  #                               anticipation_num,
  #                               estimation_label,
  #                               formula_string,
  #                               wgt= NULL,
  #                               folder_exercise) 
  # 
  # estimates_dt_oa[,signif := 0 < ATT_overall - crit_val*ATT_overall_se | 0 > ATT_overall + crit_val*ATT_overall_se]
  # estimates_dt_oa[,signif_light := 0 < ATT_overall - 1.96*ATT_overall_se | 0 > ATT_overall + 1.96*ATT_overall_se]
  # 
  # (estimates_dt_oa)
  
  bench_dt <- data.table(rbind(estimates_dt))
  
  
  
  #################################################################################
  ####################### Employment weighted estimates ###########################
  #################################################################################
  

  
  ### Total economy ln_personale
  formula_string <- '~ is_exporter + first_size_class + initial_gr_dip + ln_prod + ln_sales + sh_titolo_low + sh_White_collar + sh_full_time' 
  
  wgt_estimates_dt <- estimation(redux_sample,
                                 identification_spike,
                                 vr, tp, rec,
                                 control_type = 'nevertreated', 
                                 anticipation_num,
                                 estimation_label,
                                 formula_string,
                                 wgt= 'ln_dip',
                                 folder_exercise) 
  
  wgt_estimates_dt[,signif := 0 < ATT_overall - crit_val*ATT_overall_se | 0 > ATT_overall + crit_val*ATT_overall_se]
  wgt_
  
  (wgt_estimates_dt)
  
  ### Total economy - Only adopters
  
  # formula_string <- '~  first_size_class + ln_prod +  sh_titolo_low + sh_White_collar'
  # 
  # 
  # wgt_estimates_dt_oa <- estimation(redux_sample[is_adopter == 1],
  #                                   identification_spike,
  #                                   vr, tp = 'Total economy - Adopters only', rec,
  #                                   control_type = 'notyettreated', 
  #                                   anticipation_num,
  #                                   estimation_label,
  #                                   formula_string,
  #                                   wgt= 'ln_dip',
  #                                   folder_exercise) 
  # 
  # 
  # wgt_estimates_dt_oa[,signif := 0 < ATT_overall - crit_val*ATT_overall_se | 0 > ATT_overall + crit_val*ATT_overall_se]
  # wgt_estimates_dt_oa[,signif_light := 0 < ATT_overall - 1.96*ATT_overall_se | 0 > ATT_overall + 1.96*ATT_overall_se]
  # 
  # (wgt_estimates_dt_oa)
  
  
  wgt_dt <- rbind(wgt_estimates_dt) ## store the estimates
  
  
  
  
  ########################################################################
  ####################### Split size estimates ###########################
  ########################################################################   
  
  
  ######### Micro firms sample estimation #########
  tp <-  'Micro/Small firms (0-19)'
  
  sub_sample <- subsampling(df, 
                            rec = rec,
                            tp = tp)
  
  
  ### Fix variable and do prematching
  vr <- 'ln_dip'
  
  
  redux_dts_list <- matching(sub_sample,
                             tp = tp, 
                             vr = vr,
                             n_match = 2) 
  
  redux_sample <- redux_dts_list$redux_minimal ### select matching with ratio equal n_match
  
  
  
  
  ### Never treated controls and  non adopters (not yet treated controls)
  
  formula_string <- '~ is_exporter  + initial_gr_dip + ln_prod + ln_sales + sh_titolo_low + sh_White_collar + sh_full_time' 
  estimates_dt <- estimation(redux_sample,
                             identification_spike,
                             vr, tp, rec,
                             control_type = 'nevertreated', 
                             anticipation_num,
                             estimation_label,
                             formula_string,
                             wgt= NULL,
                             folder_exercise) 
  
  estimates_dt[,signif := 0 < ATT_overall - crit_val*ATT_overall_se | 0 > ATT_overall + crit_val*ATT_overall_se]
  
  
  
  
  # formula_string <- '~ is_exporter  + initial_gr_dip + ln_prod + ln_sales + sh_titolo_low + sh_White_collar + sh_full_time' 
  # estimates_dt_oa <- estimation(redux_sample[is_adopter == 1],
  #                               identification_spike,
  #                               vr, tp = paste0(tp,' - Adopters only'), rec,
  #                               control_type = 'notyettreated', 
  #                               anticipation_num,
  #                               estimation_label,
  #                               formula_string,
  #                               wgt= NULL,
  #                               folder_exercise) 
  # 
  # estimates_dt_oa[,signif := 0 < ATT_overall - crit_val*ATT_overall_se | 0 > ATT_overall + crit_val*ATT_overall_se]
  # estimates_dt_oa[,signif_light := 0 < ATT_overall - 1.96*ATT_overall_se | 0 > ATT_overall + 1.96*ATT_overall_se]
  # 
  
  micro_firm_export <- data.table(rbind(estimates_dt))
  
  
  ######### Small firms sample estimation #########
  tp <-  'Small firms (20-49)'
  
  sub_sample <- subsampling(df, 
                            rec = rec,
                            tp = tp)
  
  
  ### Fix variable and do prematching
  vr <- 'ln_dip'
  
  
  redux_dts_list <- matching(sub_sample,
                             tp = tp, 
                             vr = vr,
                             n_match = 2) 
  
  redux_sample <- redux_dts_list$redux_minimal ### select matching with ratio equal n_match
  
  
  
  
  ### Never treated controls and  non adopters (not yet treated controls)
  
  formula_string <- '~ is_exporter + initial_gr_dip + ln_prod + ln_sales + sh_titolo_low + sh_White_collar + sh_full_time' 
  estimates_dt <- estimation(redux_sample,
                             identification_spike,
                             vr, tp, rec,
                             control_type = 'nevertreated', 
                             anticipation_num,
                             estimation_label,
                             formula_string,
                             wgt= NULL,
                             folder_exercise) 
  
  estimates_dt[,signif := 0 < ATT_overall - crit_val*ATT_overall_se | 0 > ATT_overall + crit_val*ATT_overall_se]
  
  
  
  
  # formula_string <- '~ is_exporter + initial_gr_dip + ln_prod + ln_sales + sh_titolo_low + sh_White_collar + sh_full_time' 
  # estimates_dt_oa <- estimation(redux_sample[is_adopter == 1],
  #                               identification_spike,
  #                               vr, tp = paste0(tp,' - Adopters only'), rec,
  #                               control_type = 'notyettreated', 
  #                               anticipation_num,
  #                               estimation_label,
  #                               formula_string,
  #                               wgt= NULL,
  #                               folder_exercise) 
  # 
  # estimates_dt_oa[,signif := 0 < ATT_overall - crit_val*ATT_overall_se | 0 > ATT_overall + crit_val*ATT_overall_se]
  # estimates_dt_oa[,signif_light := 0 < ATT_overall - 1.96*ATT_overall_se | 0 > ATT_overall + 1.96*ATT_overall_se]
  # 
  
  small_firm_export <- data.table(rbind(estimates_dt))
  
  
  
  
  ######### Med/Large firms sample estimation #########
  tp <-  'Medium and large firms (+50)'
  
  sub_sample <- subsampling(df, 
                            rec = rec,
                            tp = tp)
  
  
  ### Fix variable and do prematching
  vr <- 'ln_dip'
  
  
  redux_dts_list <- matching(sub_sample,
                             tp = tp, 
                             vr = vr,
                             n_match = 2) 
  
  redux_sample <- redux_dts_list$redux_minimal ### select matching with ratio equal n_match
  redux_sample <- redux_dts_list$redux ### select matching with ratio equal n_match
  
  
  
  
  ### Never treated controls and  non adopters (not yet treated controls)
  
  formula_string <- '~ is_exporter + initial_gr_dip + ln_prod + ln_sales + sh_titolo_low + sh_White_collar + sh_full_time' 
  estimates_dt <- estimation(redux_sample,
                             identification_spike,
                             vr, tp, rec,
                             control_type = 'nevertreated', 
                             anticipation_num,
                             estimation_label,
                             formula_string,
                             wgt= NULL,
                             folder_exercise) 
  
  estimates_dt[,signif := 0 < ATT_overall - crit_val*ATT_overall_se | 0 > ATT_overall + crit_val*ATT_overall_se]
  
  
  
  
  # formula_string <- '~ is_exporter  + initial_gr_dip + ln_prod + ln_sales + sh_titolo_low + sh_White_collar + sh_full_time' 
  # estimates_dt_oa <- estimation(redux_sample[is_adopter == 1],
  #                               identification_spike,
  #                               vr, tp = paste0(tp,' - Adopters only'), rec,
  #                               control_type = 'notyettreated', 
  #                               anticipation_num,
  #                               estimation_label,
  #                               formula_string,
  #                               wgt= NULL,
  #                               folder_exercise) 
  # 
  # estimates_dt_oa[,signif := 0 < ATT_overall - crit_val*ATT_overall_se | 0 > ATT_overall + crit_val*ATT_overall_se]
  # estimates_dt_oa[,signif_light := 0 < ATT_overall - 1.96*ATT_overall_se | 0 > ATT_overall + 1.96*ATT_overall_se]
  # 
   
  
  large_firm_export <- data.table(rbind(estimates_dt))
  
  
  
  ############## Size split datasets merge #################
  
  split_dt <- data.table(rbind(micro_firm_export,small_firm_export,large_firm_export))
  
  
  
  
  ################################################################################
  ################ Merging the datasets of the exercise and save #################
  ################################################################################
  bench_dt[, spike_type := spike_type]
  wgt_dt[, spike_type := spike_type]
  split_dt[,spike_type := spike_type]
  
  bench_dt[, tech_type := tech_type]
  wgt_dt[, tech_type := tech_type]
  split_dt[,tech_type := tech_type]
  
  export_dt <- data.table(rbind(export_dt,
                                bench_dt,
                                wgt_dt,
                                split_dt))
  
  
}



save(export_dt, file = here::here('DiD_results',
                                  folder_exercise,
                                  paste0(identification_spike,'_',vr,'_CS_att_results.RData')))





################################################################################
################ Merging the datasets of the exercise and save #################
################################################################################

export_dt <- data.table(rbind(prod_split_dt))


save(export_dt, file = here::here('DiD_results',
                                  folder_exercise,
                                  paste0(identification_spike,'_',vr,'_CS_att_results.RData')))





