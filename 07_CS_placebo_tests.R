######################################################################
#########  Firm-level Difference-in-Differences Analysis    ##########
#########  Assigning random treatments and placebo testing  ##########
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
# - DiD_results/placebo_test/spike2_autn_rob_ai_ln_dip_CS_att_results.RData
#   containing estimation results for:
#   * Benchmark employment estimates
#   * Employment weighted estimates 
#   * Size-split estimates for employment
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


#spike_type <- 'spike2'
#tech_type <- 'automn'
#identification_spike <- paste0(spike_type,'_',tech_type)

folder_exercise <- 'placebo_test'
dir.create(here::here('DiD_results',folder_exercise))

results_par <- list()
results_dts <- list()  


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


#############################################################
########### Setting placebo treatment variables #############
#############################################################


treatment_summary <- unique(redux_sample[,list(codice_unita,year_treated)])[,.N, by = year_treated]


treatment_randomizing <- function(l, rand_type = 'timing')
{
  
if(rand_type == 'timing')
{  
treatment_years <- treatment_summary[year_treated != 0]$year_treated
treatment_probs <- treatment_summary[year_treated != 0]$N/sum(treatment_summary[year_treated != 0]$N)
}else if(rand_type == 'treatment'){
treatment_years <- treatment_summary$year_treated
treatment_probs <- treatment_summary$N/sum(treatment_summary$N)  
}

random_treatment <- sample(treatment_years,1,prob = treatment_probs)

return(rep(random_treatment,l))
}

time_placebo_sample <- copy(redux_sample)
time_placebo_sample[is_adopter == 1,year_treated := treatment_randomizing(length(anno), rand_type = 'timing'), by = codice_unita]


treat_placebo_sample <- copy(redux_sample)
treat_placebo_sample[,year_treated := treatment_randomizing(length(anno), rand_type = 'treatment'), by = codice_unita]


redux_sample[,.N, by = year_treated][order(year_treated)]
time_placebo_sample[,.N, by = year_treated][order(year_treated)]
treat_placebo_sample[,.N, by = year_treated][order(year_treated)]



### Set anticipation and other parameters
anticipation_num <- 1

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

timing_estimates_dt <- estimation(time_placebo_sample,
                           identification_spike,
                           vr, tp, rec,
                           control_type = 'nevertreated', 
                           anticipation_num,
                           estimation_label,
                           formula_string,
                           wgt= NULL,
                           folder_exercise) 

timing_estimates_dt[,signif := 0 < ATT_overall - crit_val*ATT_overall_se | 0 > ATT_overall + crit_val*ATT_overall_se]

timing_estimates_dt[,placebo_type := 'Random timing']

(timing_estimates_dt)

treat_estimates_dt <- estimation(treat_placebo_sample,
                                  identification_spike,
                                  vr, tp, rec,
                                  control_type = 'nevertreated', 
                                  anticipation_num,
                                  estimation_label,
                                  formula_string,
                                  wgt= NULL,
                                  folder_exercise) 

treat_estimates_dt[,signif := 0 < ATT_overall - crit_val*ATT_overall_se | 0 > ATT_overall + crit_val*ATT_overall_se]

treat_estimates_dt[,placebo_type := 'Random treatment']

(treat_estimates_dt)




### Total economy - Only adopters
colnames(redux_sample)
formula_string <- '~ is_exporter + first_size_class + initial_gr_dip + ln_prod + ln_sales + sh_titolo_low + sh_White_collar + sh_full_time' 
timing_estimates_dt_oa <- estimation(time_placebo_sample[is_adopter == 1],
                                     identification_spike,
                                     vr, tp, rec,
                                     control_type = 'notyettreated', 
                                     anticipation_num,
                                     estimation_label,
                                     formula_string,
                                     wgt= NULL,
                                     folder_exercise) 

timing_estimates_dt_oa[,signif := 0 < ATT_overall - crit_val*ATT_overall_se | 0 > ATT_overall + crit_val*ATT_overall_se]
timing_estimates_dt_oa[,signif_light := 0 < ATT_overall - 1.96*ATT_overall_se | 0 > ATT_overall + 1.96*ATT_overall_se]
timing_estimates_dt_oa[,placebo_type := 'Random timing']

(timing_estimates_dt_oa)

formula_string <- '~ is_exporter + first_size_class + initial_gr_dip + ln_prod + ln_sales + sh_titolo_low + sh_White_collar + sh_full_time' 
treat_estimates_dt_oa <- estimation(treat_placebo_sample[is_adopter == 1],
                                     identification_spike,
                                     vr, tp, rec,
                                     control_type = 'notyettreated', 
                                     anticipation_num,
                                     estimation_label,
                                     formula_string,
                                     wgt= NULL,
                                     folder_exercise) 

treat_estimates_dt_oa[,signif := 0 < ATT_overall - crit_val*ATT_overall_se | 0 > ATT_overall + crit_val*ATT_overall_se]
treat_estimates_dt_oa[,signif_light := 0 < ATT_overall - 1.96*ATT_overall_se | 0 > ATT_overall + 1.96*ATT_overall_se]
treat_estimates_dt_oa[,placebo_type := 'Random treatment']

(treat_estimates_dt_oa)




#################################################################################
####################### Employment weighted estimates ###########################
#################################################################################

### Total economy ln_personale
formula_string <- '~ is_exporter + first_size_class + initial_gr_dip + ln_prod + ln_sales + sh_titolo_low + sh_White_collar + sh_full_time' 

wgt_timing_estimates_dt <- estimation(time_placebo_sample,
                               identification_spike,
                               vr, tp, rec,
                               control_type = 'nevertreated', 
                               anticipation_num,
                               estimation_label,
                               formula_string,
                               wgt= 'ln_dip',
                               folder_exercise) 

wgt_timing_estimates_dt[,signif := 0 < ATT_overall - crit_val*ATT_overall_se | 0 > ATT_overall + crit_val*ATT_overall_se]

wgt_timing_estimates_dt[,placebo_type := 'Random timing']

(wgt_timing_estimates_dt)


### Total economy ln_personale
formula_string <- '~ is_exporter + first_size_class + initial_gr_dip + ln_prod + ln_sales + sh_titolo_low + sh_White_collar + sh_full_time' 

wgt_treat_estimates_dt <- estimation(treat_placebo_sample,
                                     identification_spike,
                                     vr, tp, rec,
                                     control_type = 'nevertreated', 
                                     anticipation_num,
                                     estimation_label,
                                     formula_string,
                                     wgt= 'ln_dip',
                                     folder_exercise) 

wgt_treat_estimates_dt[,signif := 0 < ATT_overall - crit_val*ATT_overall_se | 0 > ATT_overall + crit_val*ATT_overall_se]

wgt_treat_estimates_dt[,placebo_type := 'Random treatment']

(wgt_treat_estimates_dt)


### Total economy - Only adopters

formula_string <- '~ is_exporter + first_size_class + initial_gr_dip + ln_prod + ln_sales + sh_titolo_low + sh_White_collar + sh_full_time' 


wgt_timing_estimates_dt_oa <- estimation(time_placebo_sample[is_adopter == 1,],
                                        identification_spike,
                                        vr, tp, rec,
                                        control_type = 'notyettreated', 
                                        anticipation_num,
                                        estimation_label,
                                        formula_string,
                                        wgt= 'ln_dip',
                                        folder_exercise) 

wgt_timing_estimates_dt_oa[,signif := 0 < ATT_overall - crit_val*ATT_overall_se | 0 > ATT_overall + crit_val*ATT_overall_se]
wgt_timing_estimates_dt_oa[,signif_light := 0 < ATT_overall - 1.96*ATT_overall_se | 0 > ATT_overall + 1.96*ATT_overall_se]
wgt_timing_estimates_dt_oa[,placebo_type := 'Random timing']

(wgt_timing_estimates_dt_oa)




wgt_treat_estimates_dt_oa <- estimation(treat_placebo_sample[is_adopter == 1,],
                               identification_spike,
                               vr, tp, rec,
                               control_type = 'notyettreated', 
                               anticipation_num,
                               estimation_label,
                               formula_string,
                               wgt= 'ln_dip',
                               folder_exercise) 

wgt_treat_estimates_dt_oa[,signif := 0 < ATT_overall - crit_val*ATT_overall_se | 0 > ATT_overall + crit_val*ATT_overall_se]
wgt_treat_estimates_dt_oa[,signif_light := 0 < ATT_overall - 1.96*ATT_overall_se | 0 > ATT_overall + 1.96*ATT_overall_se]
wgt_treat_estimates_dt_oa[,placebo_type := 'Random treatment']

(wgt_treat_estimates_dt_oa)



############# Merge all the datasets into a single dataframe ###########



placebo_dt <- data.table(rbind(timing_estimates_dt,treat_estimates_dt,
                               timing_estimates_dt_oa, treat_estimates_dt_oa,
                               wgt_timing_estimates_dt, wgt_treat_estimates_dt,
                               wgt_timing_estimates_dt_oa, wgt_treat_estimates_dt_oa))




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



####### Generating placebo treatment variables ########


treatment_summary <- unique(redux_sample[,list(codice_unita,year_treated)])[,.N, by = year_treated]


treatment_randomizing <- function(l, rand_type = 'timing')
{
  
  if(rand_type == 'timing')
  {  
    treatment_years <- treatment_summary[year_treated != 0]$year_treated
    treatment_probs <- treatment_summary[year_treated != 0]$N/sum(treatment_summary[year_treated != 0]$N)
  }else if(rand_type == 'treatment'){
    treatment_years <- treatment_summary$year_treated
    treatment_probs <- treatment_summary$N/sum(treatment_summary$N)  
  }
  
  random_treatment <- sample(treatment_years,1,prob = treatment_probs)
  
  return(rep(random_treatment,l))
}

time_placebo_sample <- copy(redux_sample)
time_placebo_sample[is_adopter == 1,year_treated := treatment_randomizing(length(anno), rand_type = 'timing'), by = codice_unita]


treat_placebo_sample <- copy(redux_sample)
treat_placebo_sample[,year_treated := treatment_randomizing(length(anno), rand_type = 'treatment'), by = codice_unita]


redux_sample[,.N, by = year_treated][order(year_treated)]
time_placebo_sample[,.N, by = year_treated][order(year_treated)]
treat_placebo_sample[,.N, by = year_treated][order(year_treated)]



### Never treated controls and  non adopters (not yet treated controls)

formula_string <- '~ is_exporter  + initial_gr_dip + ln_prod + ln_sales + sh_titolo_low + sh_White_collar + sh_full_time' 


micro_timing_estimates_dt <- estimation(time_placebo_sample,
                                        identification_spike,
                                        vr, tp, rec,
                                        control_type = 'nevertreated', 
                                        anticipation_num,
                                        estimation_label,
                                        formula_string,
                                        wgt= NULL,
                                        folder_exercise) 

micro_timing_estimates_dt[,signif := 0 < ATT_overall - crit_val*ATT_overall_se | 0 > ATT_overall + crit_val*ATT_overall_se]
micro_timing_estimates_dt[,signif_light := 0 < ATT_overall - 1.96*ATT_overall_se | 0 > ATT_overall + 1.96*ATT_overall_se]
micro_timing_estimates_dt[,placebo_type := 'Random timing']

(micro_timing_estimates_dt)

micro_treat_estimates_dt <- estimation(treat_placebo_sample,
                                       identification_spike,
                                       vr, tp, rec,
                                       control_type = 'nevertreated', 
                                       anticipation_num,
                                       estimation_label,
                                       formula_string,
                                       wgt= NULL,
                                       folder_exercise) 

micro_treat_estimates_dt[,signif := 0 < ATT_overall - crit_val*ATT_overall_se | 0 > ATT_overall + crit_val*ATT_overall_se]
micro_treat_estimates_dt[,signif_light := 0 < ATT_overall - 1.96*ATT_overall_se | 0 > ATT_overall + 1.96*ATT_overall_se]
micro_treat_estimates_dt[,placebo_type := 'Random treatment']

(micro_treat_estimates_dt)



formula_string <- '~ is_exporter  + initial_gr_dip + ln_prod + ln_sales + sh_titolo_low + sh_White_collar + sh_full_time' 

micro_timing_estimates_dt_oa <- estimation(time_placebo_sample[is_adopter == 1],
                                        identification_spike,
                                        vr, tp, rec,
                                        control_type = 'notyettreated', 
                                        anticipation_num,
                                        estimation_label,
                                        formula_string,
                                        wgt= NULL,
                                        folder_exercise) 

micro_timing_estimates_dt_oa[,signif := 0 < ATT_overall - crit_val*ATT_overall_se | 0 > ATT_overall + crit_val*ATT_overall_se]
micro_timing_estimates_dt_oa[,signif_light := 0 < ATT_overall - 1.96*ATT_overall_se | 0 > ATT_overall + 1.96*ATT_overall_se]
micro_timing_estimates_dt_oa[,placebo_type := 'Random timing']

(micro_timing_estimates_dt_oa)


micro_treat_estimates_dt_oa <- estimation(treat_placebo_sample[is_adopter == 1],
                                           identification_spike,
                                           vr, tp, rec,
                                           control_type = 'notyettreated', 
                                           anticipation_num,
                                           estimation_label,
                                           formula_string,
                                           wgt= NULL,
                                           folder_exercise) 

micro_treat_estimates_dt_oa[,signif := 0 < ATT_overall - crit_val*ATT_overall_se | 0 > ATT_overall + crit_val*ATT_overall_se]
micro_treat_estimates_dt_oa[,signif_light := 0 < ATT_overall - 1.96*ATT_overall_se | 0 > ATT_overall + 1.96*ATT_overall_se]
micro_treat_estimates_dt_oa[,placebo_type := 'Random treatment']

(micro_treat_estimates_dt_oa)



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



####### Generating placebo treatment variables ########


treatment_summary <- unique(redux_sample[,list(codice_unita,year_treated)])[,.N, by = year_treated]


treatment_randomizing <- function(l, rand_type = 'timing')
{
  
  if(rand_type == 'timing')
  {  
    treatment_years <- treatment_summary[year_treated != 0]$year_treated
    treatment_probs <- treatment_summary[year_treated != 0]$N/sum(treatment_summary[year_treated != 0]$N)
  }else if(rand_type == 'treatment'){
    treatment_years <- treatment_summary$year_treated
    treatment_probs <- treatment_summary$N/sum(treatment_summary$N)  
  }
  
  random_treatment <- sample(treatment_years,1,prob = treatment_probs)
  
  return(rep(random_treatment,l))
}

time_placebo_sample <- copy(redux_sample)
time_placebo_sample[is_adopter == 1,year_treated := treatment_randomizing(length(anno), rand_type = 'timing'), by = codice_unita]


treat_placebo_sample <- copy(redux_sample)
treat_placebo_sample[,year_treated := treatment_randomizing(length(anno), rand_type = 'treatment'), by = codice_unita]


redux_sample[,.N, by = year_treated][order(year_treated)]
time_placebo_sample[,.N, by = year_treated][order(year_treated)]
treat_placebo_sample[,.N, by = year_treated][order(year_treated)]






### Never treated controls and  non adopters (not yet treated controls)

formula_string <- '~ is_exporter  + initial_gr_dip + ln_prod + ln_sales + sh_titolo_low + sh_White_collar + sh_full_time' 

small_timing_estimates_dt <- estimation(time_placebo_sample,
                                        identification_spike,
                                        vr, tp, rec,
                                        control_type = 'nevertreated', 
                                        anticipation_num,
                                        estimation_label,
                                        formula_string,
                                        wgt= NULL,
                                        folder_exercise) 

small_timing_estimates_dt[,signif := 0 < ATT_overall - crit_val*ATT_overall_se | 0 > ATT_overall + crit_val*ATT_overall_se]
small_timing_estimates_dt[,signif_light := 0 < ATT_overall - 1.96*ATT_overall_se | 0 > ATT_overall + 1.96*ATT_overall_se]
small_timing_estimates_dt[,placebo_type := 'Random timing']

(small_timing_estimates_dt)

small_treat_estimates_dt <- estimation(treat_placebo_sample,
                                       identification_spike,
                                       vr, tp, rec,
                                       control_type = 'nevertreated', 
                                       anticipation_num,
                                       estimation_label,
                                       formula_string,
                                       wgt= NULL,
                                       folder_exercise) 

small_treat_estimates_dt[,signif := 0 < ATT_overall - crit_val*ATT_overall_se | 0 > ATT_overall + crit_val*ATT_overall_se]
small_treat_estimates_dt[,signif_light := 0 < ATT_overall - 1.96*ATT_overall_se | 0 > ATT_overall + 1.96*ATT_overall_se]
small_treat_estimates_dt[,placebo_type := 'Random treatment']

(small_treat_estimates_dt)




formula_string <- '~ is_exporter  + initial_gr_dip + ln_prod + ln_sales + sh_titolo_low + sh_White_collar + sh_full_time' 

small_timing_estimates_dt_oa <- estimation(time_placebo_sample[is_adopter == 1],
                                        identification_spike,
                                        vr, tp, rec,
                                        control_type = 'notyettreated', 
                                        anticipation_num,
                                        estimation_label,
                                        formula_string,
                                        wgt= NULL,
                                        folder_exercise) 

small_timing_estimates_dt_oa[,signif := 0 < ATT_overall - crit_val*ATT_overall_se | 0 > ATT_overall + crit_val*ATT_overall_se]
small_timing_estimates_dt_oa[,signif_light := 0 < ATT_overall - 1.96*ATT_overall_se | 0 > ATT_overall + 1.96*ATT_overall_se]
small_timing_estimates_dt_oa[,placebo_type := 'Random timing']

(small_timing_estimates_dt_oa)



small_treat_estimates_dt_oa <- estimation(treat_placebo_sample[is_adopter == 1],
                                         identification_spike,
                                         vr, tp, rec,
                                         control_type = 'notyettreated', 
                                         anticipation_num,
                                         estimation_label,
                                         formula_string,
                                         wgt= NULL,
                                         folder_exercise) 

small_treat_estimates_dt_oa[,signif := 0 < ATT_overall - crit_val*ATT_overall_se | 0 > ATT_overall + crit_val*ATT_overall_se]
small_treat_estimates_dt_oa[,signif_light := 0 < ATT_overall - 1.96*ATT_overall_se | 0 > ATT_overall + 1.96*ATT_overall_se]
small_treat_estimates_dt_oa[,placebo_type := 'Random treatment']

(small_treat_estimates_dt_oa)




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




##### Generating placebo treatment variables #######


treatment_summary <- unique(redux_sample[,list(codice_unita,year_treated)])[,.N, by = year_treated]


treatment_randomizing <- function(l, rand_type = 'timing')
{
  
  if(rand_type == 'timing')
  {  
    treatment_years <- treatment_summary[year_treated != 0]$year_treated
    treatment_probs <- treatment_summary[year_treated != 0]$N/sum(treatment_summary[year_treated != 0]$N)
  }else if(rand_type == 'treatment'){
    treatment_years <- treatment_summary$year_treated
    treatment_probs <- treatment_summary$N/sum(treatment_summary$N)  
  }
  
  random_treatment <- sample(treatment_years,1,prob = treatment_probs)
  
  return(rep(random_treatment,l))
}

time_placebo_sample <- copy(redux_sample)
time_placebo_sample[is_adopter == 1,year_treated := treatment_randomizing(length(anno), rand_type = 'timing'), by = codice_unita]


treat_placebo_sample <- copy(redux_sample)
treat_placebo_sample[,year_treated := treatment_randomizing(length(anno), rand_type = 'treatment'), by = codice_unita]


redux_sample[,.N, by = year_treated][order(year_treated)]
time_placebo_sample[,.N, by = year_treated][order(year_treated)]
treat_placebo_sample[,.N, by = year_treated][order(year_treated)]





### Never treated controls and  non adopters (not yet treated controls)

formula_string <- '~ is_exporter + first_size_class + initial_gr_dip + ln_prod + ln_sales + sh_titolo_low + sh_White_collar + sh_full_time' 

large_timing_estimates_dt <- estimation(time_placebo_sample,
                                        identification_spike,
                                        vr, tp, rec,
                                        control_type = 'nevertreated', 
                                        anticipation_num,
                                        estimation_label,
                                        formula_string,
                                        wgt= NULL,
                                        folder_exercise) 

large_timing_estimates_dt[,signif := 0 < ATT_overall - crit_val*ATT_overall_se | 0 > ATT_overall + crit_val*ATT_overall_se]
large_timing_estimates_dt[,signif_light := 0 < ATT_overall - 1.96*ATT_overall_se | 0 > ATT_overall + 1.96*ATT_overall_se]
large_timing_estimates_dt[,placebo_type := 'Random timing']

(large_timing_estimates_dt)


large_treat_estimates_dt <- estimation(treat_placebo_sample,
                                       identification_spike,
                                       vr, tp, rec,
                                       control_type = 'nevertreated', 
                                       anticipation_num,
                                       estimation_label,
                                       formula_string,
                                       wgt= NULL,
                                       folder_exercise) 

large_treat_estimates_dt[,signif := 0 < ATT_overall - crit_val*ATT_overall_se | 0 > ATT_overall + crit_val*ATT_overall_se]
large_treat_estimates_dt[,signif_light := 0 < ATT_overall - 1.96*ATT_overall_se | 0 > ATT_overall + 1.96*ATT_overall_se]
large_treat_estimates_dt[,placebo_type := 'Random treatment']

(large_treat_estimates_dt)



formula_string <- '~ is_exporter + first_size_class + initial_gr_dip + ln_prod + ln_sales + sh_titolo_low + sh_White_collar + sh_full_time' 

large_timing_estimates_dt_oa <- estimation(time_placebo_sample[is_adopter == 1],
                                           identification_spike,
                                           vr, tp, rec,
                                           control_type = 'notyettreated', 
                                           anticipation_num,
                                           estimation_label,
                                           formula_string,
                                           wgt= NULL,
                                           folder_exercise) 

large_timing_estimates_dt_oa[,signif := 0 < ATT_overall - crit_val*ATT_overall_se | 0 > ATT_overall + crit_val*ATT_overall_se]
large_timing_estimates_dt_oa[,signif_light := 0 < ATT_overall - 1.96*ATT_overall_se | 0 > ATT_overall + 1.96*ATT_overall_se]
large_timing_estimates_dt_oa[,placebo_type := 'Random timing']

(large_timing_estimates_dt_oa)



large_treat_estimates_dt_oa <- estimation(treat_placebo_sample[is_adopter == 1],
                                          identification_spike,
                                          vr, tp, rec,
                                          control_type = 'notyettreated', 
                                          anticipation_num,
                                          estimation_label,
                                          formula_string,
                                          wgt= NULL,
                                          folder_exercise) 

large_treat_estimates_dt_oa[,signif := 0 < ATT_overall - crit_val*ATT_overall_se | 0 > ATT_overall + crit_val*ATT_overall_se]
large_treat_estimates_dt_oa[,signif_light := 0 < ATT_overall - 1.96*ATT_overall_se | 0 > ATT_overall + 1.96*ATT_overall_se]
large_treat_estimates_dt_oa[,placebo_type := 'Random treatment']

(large_treat_estimates_dt_oa)




############## Size split datasets merge #################

placebo_split_dt <- data.table(rbind(micro_timing_estimates_dt, micro_treat_estimates_dt,
                                     micro_timing_estimates_dt_oa, micro_treat_estimates_dt_oa,
                                     small_timing_estimates_dt, small_treat_estimates_dt,
                                     small_timing_estimates_dt_oa, small_treat_estimates_dt_oa,
                                     large_timing_estimates_dt, large_treat_estimates_dt,
                                     large_timing_estimates_dt_oa, large_treat_estimates_dt_oa))




################################################################################
################ Merging the datasets of the exercise and save #################
################################################################################

export_dt <- data.table(rbind(placebo_dt,placebo_split_dt))


export_dt[,.N, keyby = .(signif_light,signif,placebo_type,weighting,sample_type,control)]

save(export_dt, file = here::here('DiD_results',folder_exercise,paste0(identification_spike,'_',vr,'_CS_att_results.RData')))




