##################################################################
##################################################################
############ Market dynamics and automation adoption  ############
##################################################################
##################################################################

#
# This code analyzes the relationship between automation adoption and market dynamics
# across Italian firms and sectors.
# Benchmark exercise and relative robustness checks of section 3.2 
# ("Firm competition and the sector-level effects of automation") of the paper accessible at 
# the DOI: https://doi.org/10.1016/j.euroecorev.2024.104943
#
# INPUT FILES:
# - AUX_DiD_exercise_functions.R: Contains functions for data handling and estimation
# - Raw firm-level data (imported through data_import() function from the folder
#   'elaborated_datasets' containing the dataset 'firm_level_clean_complete_rv24.csv')
#
# KEY VARIABLES:
# - ident_spike: Automation adoption indicator
# - ident_imp: Import automation indicator 
# - vp: Sales
# - vagg: Value added
# - dipendenti: Number of employees
# - ateco: Industry classification code
#
# MAIN STEPS:
# 1. Data preprocessing and cleaning
# 2. Computing automation penetration metrics by sector and year
# 3. Calculating growth rates for employment, sales, and productivity
# 4. Constructing cumulative measures over different time windows
# 5. Conducting regression analysis:
#    - Employment effects
#    - Sales effects
#    - Productivity effects
#    - Business stealing effects
#
# OUTPUTS:
#
# 1. Regression Tables (.tex files):
#    - tab_emp_ateco_baseline_11_15/19: Baseline employment effects by sector
#    - tab_emp_ateco_windows_11_15/19: Employment effects across time windows
#    - tab_emp_ateco_variables_11_15/19: Employment effects with different variables
#    - tab_emp_on_sales_ateco_*: Employment/sales ratio analysis
#    - competition_horizon_tables: Business stealing effects analysis
#
# 2. Statistical Results:
#    - Aggregates_NC2: Sector-level statistics including:
#      * Growth rates (employment, sales, value added)
#      * Exit rates
#      * Cumulative growth measures (2011-2019)
#    - Aggregates_ateco: Firm-level statistics including:
#      * Growth rates
#      * Productivity measures
#      * Employment dynamics
#
# 3. Visualization Plots:
#    - NC2 sectors and employment: Boxplots comparing adopters vs non-adopters
#    - NC2 sectors and sales: Distribution of sales growth by sector
#    - NC2 sectors and value added: Sectoral productivity patterns
#    - Net employment effects at sectoral level: Bar plots
#
# 4. Analysis Windows:
#    - Short-term effects (2015-2019)
#    - Medium-term effects (2013-2019)
#    - Long-term effects (2011-2019)
#
# 5. Model Specifications:
#    - Without sector fixed effects
#    - With NC2 (2-digit) fixed effects
#    - With full sectoral controls
#    - With various robustness checks
#
# All results are saved in both .tex format (for LaTeX compilation) 
#
###############################################################################


### Clean the environment ###
rm(list = ls())

###### Import libraries ####
library(data.table)
library(here)

##### Source the functions for data handling and estimation ####
source(here::here('AUX_DiD_exercise_functions.R'))


##### Import data ####
base_dt <- data_import()


folder_exercise <- 'market_dynamics'
results_par <- list()
results_dts <- list()  


spike_type <- 'spike2'
tech_type <- 'autn_rob_ai'

identification_spike <- paste0(spike_type,'_',tech_type)

#### Reshape data according to spike definitions ####
dt <- data_reshaping(base_dt,
                     spike_type,
                     tech_type)


#######  Computing automation penetration per year  #######

### Computing exposition to adoption per sector per year
length_NA <- function(x){length(x[!is.na(x)])}

sh_spike_auto <- dt[ ,lapply(.SD,function(x) sum(x, na.rm = T)/length_NA(x)),
                     .SDcols = c('ident_spike'),
                     keyby = list(anno,ateco)]
colnames(sh_spike_auto)[3] <- 'sh_spike_auto'

dt[,import_spike_auto := ident_spike*as.numeric(ident_imp)]


sh_spike_import_auto <- dt[ ,sum(as.numeric(import_spike_auto), 
                                 na.rm = T)/sum(as.numeric(tot_import_val), 
                                                na.rm = T),
                            keyby = list(anno,ateco)]

sh_import_auto <- dt[ ,sum(as.numeric(ident_imp),
                           na.rm = T)/sum(as.numeric(tot_import_val),
                                          na.rm = T),
                      keyby = list(anno,ateco)]

sh_wgt_auto_import <- dt[ ,weighted.mean(as.numeric(ident_imp)/as.numeric(tot_import_val),
                                         vp/sum(vp, na.rm = T), na.rm = T),
                           keyby = list(anno,ateco)]


import_auto_sec <- dt[ ,sum(as.numeric(ident_imp),
                            na.rm = T),
                       keyby = list(anno,ateco)]

import_auto_sec[,ateco := as.character(ateco)]
import_auto_sec[,anno := as.integer(anno)]

wgt_dip_import_auto_sec <- dt[ ,sum(as.numeric(ident_imp)*dipendenti, na.rm = T),
                                keyby = list(anno,ateco)]

wgt_dip_import_auto_sec[,ateco := as.character(ateco)]
wgt_dip_import_auto_sec[,anno := as.integer(anno)]


wgt_sales_import_auto_sec <- dt[ ,sum(as.numeric(ident_imp)*vp, na.rm = T),
                                   keyby = list(anno,ateco)]

wgt_sales_import_auto_sec[,ateco := as.character(ateco)]
wgt_sales_import_auto_sec[,anno := as.integer(anno)]




##### Computing the exposures for the firms

colnames(sh_spike_import_auto)[3] <- 'sh_spike_import_auto'
colnames(sh_import_auto)[3] <- 'sh_import_auto'
colnames(sh_wgt_auto_import)[3] <- 'sh_wgt_auto_import'
colnames(import_auto_sec)[3] <- 'import_auto_sec'
colnames(wgt_dip_import_auto_sec)[3] <- 'wgt_dip_import_auto_sec'
colnames(wgt_sales_import_auto_sec)[3] <- 'wgt_sales_import_auto_sec'


#######  Computing lagged automation penetration   #######

library(dplyr)

sh_spike_auto[,sh_spike_auto_1 := dplyr::lag(sh_spike_auto,1), keyby = list(ateco)]
sh_spike_auto[,sh_spike_auto_2 := dplyr::lag(sh_spike_auto,2), keyby = list(ateco)]
sh_spike_auto[,sh_spike_auto_3 := dplyr::lag(sh_spike_auto,3), keyby = list(ateco)]
sh_spike_auto[,sh_spike_auto_4 := dplyr::lag(sh_spike_auto,4), keyby = list(ateco)]


sh_spike_import_auto[,sh_spike_import_auto_1 := dplyr::lag(sh_spike_import_auto,1), keyby = list(ateco)]
sh_spike_import_auto[,sh_spike_import_auto_2 := dplyr::lag(sh_spike_import_auto,2), keyby = list(ateco)]
sh_spike_import_auto[,sh_spike_import_auto_3 := dplyr::lag(sh_spike_import_auto,3), keyby = list(ateco)]
sh_spike_import_auto[,sh_spike_import_auto_4 := dplyr::lag(sh_spike_import_auto,4), keyby = list(ateco)]


sh_import_auto[,sh_import_auto_1 := dplyr::lag(sh_import_auto,1), keyby = list(ateco)]
sh_import_auto[,sh_import_auto_2 := dplyr::lag(sh_import_auto,2), keyby = list(ateco)]
sh_import_auto[,sh_import_auto_3 := dplyr::lag(sh_import_auto,3), keyby = list(ateco)]
sh_import_auto[,sh_import_auto_4 := dplyr::lag(sh_import_auto,4), keyby = list(ateco)]


sh_wgt_auto_import[,sh_wgt_auto_import_1 := dplyr::lag(sh_wgt_auto_import,1), keyby = list(ateco)]
sh_wgt_auto_import[,sh_wgt_auto_import_2 := dplyr::lag(sh_wgt_auto_import,2), keyby = list(ateco)]
sh_wgt_auto_import[,sh_wgt_auto_import_3 := dplyr::lag(sh_wgt_auto_import,3), keyby = list(ateco)]
sh_wgt_auto_import[,sh_wgt_auto_import_4 := dplyr::lag(sh_wgt_auto_import,4), keyby = list(ateco)]



########## Construct cumulated automation penetration ###########

sh_sum_window <- function(num,denom,anni, name)
{
  output <- lapply(2013:2019, function(aa) sum(as.numeric(num)*(anni>=aa),
                                               na.rm = T)/sum(as.numeric(denom)*(anni>=aa),
                                                              na.rm = T))
  names(output) <- paste0(name,13:19)
  return(output)
}

cumulated_sh_spike_import_auto <- dt[ ,sh_sum_window(ident_spike*imp_autom,
                                                     tot_import_val,
                                                     anno,
                                                     name ='sh_spike_import_auto_11_'), keyby = list(ateco)]

cumulated_sh_import_auto <- dt[ ,sh_sum_window(ident_imp,
                                               tot_import_val,
                                               anno,
                                               name = 'sh_import_auto_11_'), keyby = list(ateco)]



sh_wgt_mean_window <- function(num,denom, weights_var,anni, name)
{
  output <- lapply(2013:2019, function(aa) weighted.mean((as.numeric(num)/(1+as.numeric(denom))*(anni>=aa)), w = weights_var*(anni>=aa))
                   
                   )
  names(output) <- paste0(name,13:19)
  return(output)
}

cumulated_sh_wgt_auto_import <- dt[ ,sh_wgt_mean_window(ident_imp,
                                                   tot_import_val,
                                                   vp/sum(vp, na.rm = T),
                                                   anno,
                                                   name = 'sh_wgt_auto_import_11_'), keyby = list(ateco)]


sh_length_window <- function(x,anni)
{
  output <- lapply(2013:2019, function(y) sum(x*(as.integer(as.character(anni))>=y), na.rm = T)/length(x*(anni>=y)))
  names(output) <- paste0('sh_spike_cum_11_',13:19)
  return(output)
}
cumulated_sh_spike_auto <- dt[ ,Reduce(c,lapply(.SD,function(x) sh_length_window(x,anno))),
                               .SDcols = c('ident_spike'),
                               keyby = list(ateco)]


##### Merge the datasets with sectoral variables and firm level data ######
dt[,ateco := as.character(ateco)]
dt[,anno := as.integer(anno)]
setkey(dt, ateco, anno)

## Datasets with variable by year and sector
setkey(sh_spike_auto, ateco,anno)
setkey(sh_spike_import_auto, ateco,anno)
setkey(sh_import_auto, ateco,anno)
setkey(sh_wgt_auto_import, ateco, anno)
setkey(wgt_dip_import_auto_sec, ateco, anno)
setkey(wgt_sales_import_auto_sec, ateco, anno)
setkey(import_auto_sec, ateco, anno)

dt_exposure <- sh_spike_auto[dt]
dt_exposure <- sh_spike_import_auto[dt_exposure]
dt_exposure <- sh_import_auto[dt_exposure]
dt_exposure <- sh_wgt_auto_import[dt_exposure]
dt_exposure <- unique(wgt_sales_import_auto_sec)[dt_exposure]
dt_exposure <- wgt_dip_import_auto_sec[dt_exposure]
dt_exposure <- import_auto_sec[dt_exposure]


## Datasets with variables by sector
setkey(cumulated_sh_spike_auto, ateco)
setkey(cumulated_sh_spike_import_auto, ateco)
setkey(cumulated_sh_import_auto, ateco)
setkey(cumulated_sh_wgt_auto_import)

dt_exposure <- cumulated_sh_spike_auto[dt_exposure]
dt_exposure <- cumulated_sh_import_auto[dt_exposure]
dt_exposure <- cumulated_sh_spike_import_auto[dt_exposure]
dt_exposure <- cumulated_sh_wgt_auto_import[dt_exposure]


##### Calculate employment growth rates #######

logG = function(x) c(NA, diff(log(1+x)))

##### Employment growth rate per year 
dt_exposure[,gr_dip := logG(addetti), keyby = list(codice_unita)]

dt_exposure[,gr_dip_on_sales := logG(addetti/as.numeric(vp)), keyby = list(codice_unita)]

##### Sales growth rate per year 
dt_exposure[,gr_vp := logG(as.numeric(vp)), keyby = list(codice_unita)]

##### Vagg growth rate per year 
dt_exposure[,gr_vagg := logG(as.numeric(vagg)), keyby = list(codice_unita)]

### Productivit and derivatives
dt_exposure[,ln_prod := log(1+(as.numeric(vagg)/dipendenti))]
dt_exposure[,first_year := min(anno, na.rm = T), 
            by = .(codice_unita)][,first_prod := unique(ln_prod[anno==first_year]), by = .(codice_unita)]
dt_exposure[,first_size := unique(sizecl[anno==first_year]), by = .(codice_unita)]

dt_exposure[,gr_prod := logG(1+(as.numeric(vagg)/dipendenti))]


#### Calculate dataset exit year #####
dt_exposure$anno <- as.integer(as.character(dt_exposure$anno))
dt_exposure[,exit := ifelse(max(anno) == anno,1,0), keyby = list(codice_unita)]
dt_exposure[,exit_in_11_18 := ifelse(max(anno) < 2011,NA,ifelse(max(anno)<=2018,1,0)), keyby = list(codice_unita)]
dt_exposure[,exit_in_12_18 := ifelse(max(anno) < 2012,NA,ifelse(max(anno)<=2018,1,0)), keyby = list(codice_unita)]
dt_exposure[,exit_in_13_18 := ifelse(max(anno) < 2013,NA,ifelse(max(anno)<=2018,1,0)), keyby = list(codice_unita)]
dt_exposure[,exit_in_14_18 := ifelse(max(anno) < 2014,NA,ifelse(max(anno)<=2018,1,0)), keyby = list(codice_unita)]
dt_exposure[,exit_in_15_18 := ifelse(max(anno) < 2015,NA,ifelse(max(anno)<=2018,1,0)), keyby = list(codice_unita)]
dt_exposure[,exit_in_16_18 := ifelse(max(anno) < 2016,NA,ifelse(max(anno)<=2018,1,0)), keyby = list(codice_unita)]
dt_exposure[,exit_in_17_18 := ifelse(max(anno) < 2017,NA,ifelse(max(anno)<=2018,1,0)), keyby = list(codice_unita)]
dt_exposure[,exit_in_18 := ifelse(max(anno) <= 2018,1,ifelse(max(anno)<=2018,1,0)), keyby = list(codice_unita)]


### Calculate liquidation per year #####
dt_exposure$anno <- as.integer(as.character(dt_exposure$anno))
dt_exposure[,is_ever_liq := (sum(flag_liquidazione)>=1), keyby = list(codice_unita)]
dt_exposure[,liq_in_11_19 := sum(flag_liquidazione*(anno>=2012), na.rm = T)*ifelse(sum(flag_liquidazione*(anno==2011))>0,0,1), by = codice_unita]
dt_exposure[,liq_in_12_19 := sum(flag_liquidazione*(anno>=2013), na.rm = T)*ifelse(sum(flag_liquidazione*(anno==2012))>0,0,1), by = codice_unita]
dt_exposure[,liq_in_13_19 := sum(flag_liquidazione*(anno>=2014), na.rm = T)*ifelse(sum(flag_liquidazione*(anno==2013))>0,0,1), by = codice_unita]
dt_exposure[,liq_in_14_19 := sum(flag_liquidazione*(anno>=2015), na.rm = T)*ifelse(sum(flag_liquidazione*(anno==2014))>0,0,1), by = codice_unita]
dt_exposure[,liq_in_15_19 := sum(flag_liquidazione*(anno>=2016), na.rm = T)*ifelse(sum(flag_liquidazione*(anno==2015))>0,0,1), by = codice_unita]
dt_exposure[,liq_in_16_19 := sum(flag_liquidazione*(anno>=2017), na.rm = T)*ifelse(sum(flag_liquidazione*(anno==2016))>0,0,1), by = codice_unita]
dt_exposure[,liq_in_17_19 := sum(flag_liquidazione*(anno>=2018), na.rm = T)*ifelse(sum(flag_liquidazione*(anno==2017))>0,0,1), by = codice_unita]
dt_exposure[,liq_in_18_19 := sum(flag_liquidazione*(anno>=2019), na.rm = T)*ifelse(sum(flag_liquidazione*(anno==2018))>0,0,1), by = codice_unita]

dt_exposure[,paste0('liq_in_',11:18,'_19') := lapply(.SD, function(x) ifelse(x>0,1,0)), .SDcols = paste0('liq_in_',11:18,'_19') ]


#### Calculate the cumulated employment growth rate for various span ######
dt_exposure$anno <- as.integer(as.character(dt_exposure$anno))
dt_exposure[,gr_dip_11_19 := sum(gr_dip, na.rm = T)*ifelse(exit_in_11_18,NA,1), keyby = list(codice_unita)]
dt_exposure[,gr_dip_12_19 := sum(gr_dip*ifelse(anno>=2012,1,0), na.rm = T)*ifelse(exit_in_12_18,NA,1), keyby = list(codice_unita)]
dt_exposure[,gr_dip_13_19 := sum(gr_dip*ifelse(anno>=2013,1,0), na.rm = T)*ifelse(exit_in_13_18,NA,1), keyby = list(codice_unita)]
dt_exposure[,gr_dip_14_19 := sum(gr_dip*ifelse(anno>=2014,1,0), na.rm = T)*ifelse(exit_in_14_18,NA,1), keyby = list(codice_unita)]
dt_exposure[,gr_dip_15_19 := sum(gr_dip*ifelse(anno>=2015,1,0), na.rm = T)*ifelse(exit_in_15_18,NA,1), keyby = list(codice_unita)]
dt_exposure[,gr_dip_16_19 := sum(gr_dip*ifelse(anno>=2016,1,0), na.rm = T)*ifelse(exit_in_16_18,NA,1), keyby = list(codice_unita)]
dt_exposure[,gr_dip_17_19 := sum(gr_dip*ifelse(anno>=2017,1,0), na.rm = T)*ifelse(exit_in_17_18,NA,1), keyby = list(codice_unita)]
dt_exposure[,gr_dip_18_19 := sum(gr_dip*ifelse(anno>=2018,1,0), na.rm = T)*ifelse(exit_in_18,NA,1), keyby = list(codice_unita)]


#### Calculate the cumulated employment growth rate for various span ######
dt_exposure$anno <- as.integer(as.character(dt_exposure$anno))
dt_exposure[,gr_dip_on_sales_11_19 := sum(gr_dip_on_sales, na.rm = T)*ifelse(exit_in_11_18,NA,1), keyby = list(codice_unita)]
dt_exposure[,gr_dip_on_sales_12_19 := sum(gr_dip_on_sales*ifelse(anno>=2012,1,0), na.rm = T)*ifelse(exit_in_12_18,NA,1), keyby = list(codice_unita)]
dt_exposure[,gr_dip_on_sales_13_19 := sum(gr_dip_on_sales*ifelse(anno>=2013,1,0), na.rm = T)*ifelse(exit_in_13_18,NA,1), keyby = list(codice_unita)]
dt_exposure[,gr_dip_on_sales_14_19 := sum(gr_dip_on_sales*ifelse(anno>=2014,1,0), na.rm = T)*ifelse(exit_in_14_18,NA,1), keyby = list(codice_unita)]
dt_exposure[,gr_dip_on_sales_15_19 := sum(gr_dip_on_sales*ifelse(anno>=2015,1,0), na.rm = T)*ifelse(exit_in_15_18,NA,1), keyby = list(codice_unita)]
dt_exposure[,gr_dip_on_sales_16_19 := sum(gr_dip_on_sales*ifelse(anno>=2016,1,0), na.rm = T)*ifelse(exit_in_16_18,NA,1), keyby = list(codice_unita)]
dt_exposure[,gr_dip_on_sales_17_19 := sum(gr_dip_on_sales*ifelse(anno>=2017,1,0), na.rm = T)*ifelse(exit_in_17_18,NA,1), keyby = list(codice_unita)]
dt_exposure[,gr_dip_on_sales_18_19 := sum(gr_dip_on_sales*ifelse(anno>=2018,1,0), na.rm = T)*ifelse(exit_in_18,NA,1), keyby = list(codice_unita)]



#### Calculate the cumulated productivity growth rate for various span ######
dt_exposure$anno <- as.integer(as.character(dt_exposure$anno))
dt_exposure[,gr_prod_11_19 := sum(gr_prod, na.rm = T)*ifelse(exit_in_11_18,NA,1), keyby = list(codice_unita)]
dt_exposure[,gr_prod_12_19 := sum(gr_prod*ifelse(anno>=2012,1,0), na.rm = T)*ifelse(exit_in_12_18,NA,1), keyby = list(codice_unita)]
dt_exposure[,gr_prod_13_19 := sum(gr_prod*ifelse(anno>=2013,1,0), na.rm = T)*ifelse(exit_in_13_18,NA,1), keyby = list(codice_unita)]
dt_exposure[,gr_prod_14_19 := sum(gr_prod*ifelse(anno>=2014,1,0), na.rm = T)*ifelse(exit_in_14_18,NA,1), keyby = list(codice_unita)]
dt_exposure[,gr_prod_15_19 := sum(gr_prod*ifelse(anno>=2015,1,0), na.rm = T)*ifelse(exit_in_15_18,NA,1), keyby = list(codice_unita)]
dt_exposure[,gr_prod_16_19 := sum(gr_prod*ifelse(anno>=2016,1,0), na.rm = T)*ifelse(exit_in_16_18,NA,1), keyby = list(codice_unita)]
dt_exposure[,gr_prod_17_19 := sum(gr_prod*ifelse(anno>=2017,1,0), na.rm = T)*ifelse(exit_in_17_18,NA,1), keyby = list(codice_unita)]
dt_exposure[,gr_prod_18_19 := sum(gr_prod*ifelse(anno>=2018,1,0), na.rm = T)*ifelse(exit_in_18,NA,1), keyby = list(codice_unita)]


#### Calculate the cumulated vagg growth rate for various span #####
dt_exposure$anno <- as.integer(as.character(dt_exposure$anno))
dt_exposure[,gr_vagg_11_19 := sum(gr_vagg, na.rm = T)*ifelse(exit_in_11_18,NA,1), keyby = list(codice_unita)]
dt_exposure[,gr_vagg_12_19 := sum(gr_vagg*ifelse(anno>=2012,1,0), na.rm = T)*ifelse(exit_in_12_18,NA,1), keyby = list(codice_unita)]
dt_exposure[,gr_vagg_13_19 := sum(gr_vagg*ifelse(anno>=2013,1,0), na.rm = T)*ifelse(exit_in_13_18,NA,1), keyby = list(codice_unita)]
dt_exposure[,gr_vagg_14_19 := sum(gr_vagg*ifelse(anno>=2014,1,0), na.rm = T)*ifelse(exit_in_14_18,NA,1), keyby = list(codice_unita)]
dt_exposure[,gr_vagg_15_19 := sum(gr_vagg*ifelse(anno>=2015,1,0), na.rm = T)*ifelse(exit_in_15_18,NA,1), keyby = list(codice_unita)]
dt_exposure[,gr_vagg_16_19 := sum(gr_vagg*ifelse(anno>=2016,1,0), na.rm = T)*ifelse(exit_in_16_18,NA,1), keyby = list(codice_unita)]
dt_exposure[,gr_vagg_17_19 := sum(gr_vagg*ifelse(anno>=2017,1,0), na.rm = T)*ifelse(exit_in_17_18,NA,1), keyby = list(codice_unita)]
dt_exposure[,gr_vagg_18_19 := sum(gr_vagg*ifelse(anno>=2018,1,0), na.rm = T)*ifelse(exit_in_18,NA,1), keyby = list(codice_unita)]


#### Calculate the cumulated sales growth rate for various span #####
dt_exposure$anno <- as.integer(as.character(dt_exposure$anno))
dt_exposure[,gr_vp_11_19 := sum(gr_vp, na.rm = T)*ifelse(exit_in_11_18,NA,1), keyby = list(codice_unita)]
dt_exposure[,gr_vp_12_19 := sum(gr_vp*ifelse(anno>=2012,1,0), na.rm = T)*ifelse(exit_in_12_18,NA,1), keyby = list(codice_unita)]
dt_exposure[,gr_vp_13_19 := sum(gr_vp*ifelse(anno>=2013,1,0), na.rm = T)*ifelse(exit_in_13_18,NA,1), keyby = list(codice_unita)]
dt_exposure[,gr_vp_14_19 := sum(gr_vp*ifelse(anno>=2014,1,0), na.rm = T)*ifelse(exit_in_14_18,NA,1), keyby = list(codice_unita)]
dt_exposure[,gr_vp_15_19 := sum(gr_vp*ifelse(anno>=2015,1,0), na.rm = T)*ifelse(exit_in_15_18,NA,1), keyby = list(codice_unita)]
dt_exposure[,gr_vp_16_19 := sum(gr_vp*ifelse(anno>=2016,1,0), na.rm = T)*ifelse(exit_in_16_18,NA,1), keyby = list(codice_unita)]
dt_exposure[,gr_vp_17_19 := sum(gr_vp*ifelse(anno>=2017,1,0), na.rm = T)*ifelse(exit_in_17_18,NA,1), keyby = list(codice_unita)]
dt_exposure[,gr_vp_18_19 := sum(gr_vp*ifelse(anno>=2018,1,0), na.rm = T)*ifelse(exit_in_18,NA,1), keyby = list(codice_unita)]



##################################################################################
####### Measuring the effect of automation adoption at the sectoral level ########
##################################################################################


##### Construct an ordered list of 5-digit and 2 digit sectors ##### 
ord_ateco <- unique(dt_exposure[,list(ateco,sh_spike_import_auto_11_19)])[order(sh_spike_import_auto_11_19)]
ord_NC2 <-  unique(dt_exposure[,
                            list(ateco,
                                 NC2, agg_dip_NC5 = sum(dipendenti, na.rm = T),
                                 sh_spike_import_auto),
                            by = ateco][,sh_spike_import_auto := ifelse(is.na(sh_spike_import_auto),0,sh_spike_import_auto)])

ord_NC2[,NC5_NC2_emp_sh := agg_dip_NC5/sum(agg_dip_NC5, na.rm = T), by = NC2]



ord_NC2 <- unique(ord_NC2[,sh_spike_import_auto_11_19_avg := weighted.mean(sh_spike_import_auto,NC5_NC2_emp_sh, na.rm = T),
                           keyby = NC2][, list(NC2,sh_spike_import_auto_11_19_avg)])[order(sh_spike_import_auto_11_19_avg)]

library(ggplot2)

###### NC2 sectors and employment ####### 

aux_data <- dt_exposure[,list(codice_unita,NC2,gr_dip_11_19,is_adopter)]
aux_data[, paste0('q',c(1,10,25,50,75,90,99)) := as.list(quantile(gr_dip_11_19,
                                                                  c(1,10,25,50,75,90,99)/100, na.rm = T)),
         keyby = list(NC2,is_adopter)]

boxplot_data <- unique(aux_data[,-c(1,3)])
boxplot_data <- boxplot_data[complete.cases(boxplot_data)]
boxplot_data[,is_adopter := as.factor(is_adopter)]

ggplot(boxplot_data) +
#  geom_ribbon(aes(x = factor(NC2, levels = ord_NC2$NC2),  ymin = q1, ymax = q99, group = is_adopter, fill = is_adopter),
#              alpha = 0.3) +
  geom_boxplot(aes(x = factor(NC2, levels = ord_NC2$NC2), 
                   ymin = q10, lower = q25, middle = q50, upper = q75, ymax = q90, fill = is_adopter),
                   stat = 'identity') +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 18),
        axis.text.y = element_text(size = 18),
        panel.grid.major.y = element_line(color = 'azure2'),
        panel.grid.major.x = element_blank(),
        panel.background = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.title = element_text(size = 22),
        legend.key= element_blank(),
        legend.position = 'top',
        legend.text = element_text(size = 18),
        strip.text.x = element_text(size = 20, color = "black", face = "bold"),
        strip.text.y = element_text(size = 20, color = "black", face = "bold")) +
  scale_fill_grey(start = 0.4,
                  end = 0.8, name = '', labels = c('Non-adopters','Adopters'))



###### NC2 sectors and sales ####### 
aux_data <- dt_exposure[,list(NC2,gr_vp_11_19)]
aux_data[, paste0('q',c(1,10,25,50,75,90,99)) := as.list(quantile(gr_vp_11_19,c(1,10,25,50,75,90,99)/100, na.rm = T)), keyby = list(NC2)]

boxplot_data <- unique(aux_data[,-c(2)])
boxplot_data <- boxplot_data[complete.cases(boxplot_data)][,is_man:= as.factor(ifelse(NC2>= 10 & NC2 <= 33,1,0))]

ggplot(boxplot_data) +
  geom_ribbon(aes(x = factor(NC2, levels = ord_NC2$NC2),  ymin = q1, ymax = q99, group = 1), alpha = 0.3) +
  geom_boxplot(aes(x = factor(NC2, levels = ord_NC2$NC2), 
                   ymin = q10, lower = q25, middle = q50, upper = q75, ymax = q90, fill = is_man),
               stat = 'identity') +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 18),
        axis.text.y = element_text(size = 18),
        panel.grid.major.y = element_line(color = 'azure2'),
        panel.grid.major.x = element_blank(),
        panel.background = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.title = element_text(size = 22),
        legend.key= element_blank(),
        legend.position = 'top',
        legend.text = element_text(size = 18),
        strip.text.x = element_text(size = 20, color = "black", face = "bold"),
        strip.text.y = element_text(size = 20, color = "black", face = "bold")) +
  scale_fill_grey(start = 0.5,
                  end = 1, name = '', labels = c('Other sectors','Manufacturing'))



###### NC2 sectors and vagg  ####### 
aux_data <- dt_exposure[,list(NC2,gr_vagg_15_19)]
aux_data[, paste0('q',c(1,10,25,50,75,90,99)) := as.list(quantile(gr_vagg_15_19,c(1,10,25,50,75,90,99)/100, na.rm = T)), keyby = list(NC2)]

boxplot_data <- unique(aux_data[,-c(2)])
boxplot_data <- boxplot_data[complete.cases(boxplot_data)][,is_man:= as.factor(ifelse(NC2>= 10 & NC2 <= 33,1,0))]

ggplot(boxplot_data) +
  geom_ribbon(aes(x = factor(NC2, levels = ord_NC2$NC2),  ymin = q1, ymax = q99, group = 1), alpha = 0.3) +
  geom_boxplot(aes(x = factor(NC2, levels = ord_NC2$NC2), 
                   ymin = q10, lower = q25, middle = q50, upper = q75, ymax = q90, fill = is_man),
               stat = 'identity') +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 18),
        axis.text.y = element_text(size = 18),
        panel.grid.major.y = element_line(color = 'azure2'),
        panel.grid.major.x = element_blank(),
        panel.background = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.title = element_text(size = 22),
        legend.key= element_blank(),
        legend.position = 'top',
        legend.text = element_text(size = 18),
        strip.text.x = element_text(size = 20, color = "black", face = "bold"),
        strip.text.y = element_text(size = 20, color = "black", face = "bold")) +
  scale_fill_grey(start = 0.5,
                  end = 1, name = '', labels = c('Other sectors','Manufacturing'))




######### Construct the dataset of NC2 statistics  #############

aggregates_NC2 <- dt_exposure[,lapply(.SD, function(x) sum(x,  na.rm = T)), keyby = list(anno,NC2),
                             .SDcols = c('vp','vagg','addetti','dipendenti','exit')]


## Calculate growth rates

aggregates_NC2[,gr_vp := logG(as.numeric(vp)), keyby = list(NC2)]
aggregates_NC2[,gr_vagg := logG(as.numeric(vagg)), keyby = list(NC2)]
aggregates_NC2[,gr_addetti := logG(addetti), keyby = list(NC2)]
aggregates_NC2[,gr_dipendenti := logG(dipendenti), keyby = list(NC2)]


aggregates_NC2$anno <- as.integer(as.character(aggregates_NC2$anno))
aggregates_NC2[,exit_in_11_18 := sum(exit*ifelse(anno>=2011 & anno <= 2018,1,0))/length(exit*ifelse(anno>=2011 & anno <= 2018,1,0)), keyby = list(NC2)]
aggregates_NC2[,exit_in_12_18 := sum(exit*ifelse(anno>=2012 & anno <= 2018,1,0))/length(exit*ifelse(anno>=2012 & anno <= 2018,1,0)), keyby = list(NC2)]
aggregates_NC2[,exit_in_13_18 := sum(exit*ifelse(anno>=2013 & anno <= 2018,1,0))/length(exit*ifelse(anno>=2013 & anno <= 2018,1,0)), keyby = list(NC2)]
aggregates_NC2[,exit_in_14_18 := sum(exit*ifelse(anno>=2014 & anno <= 2018,1,0))/length(exit*ifelse(anno>=2014 & anno <= 2018,1,0)), keyby = list(NC2)]
aggregates_NC2[,exit_in_15_18 := sum(exit*ifelse(anno>=2015 & anno <= 2018,1,0))/length(exit*ifelse(anno>=2015 & anno <= 2018,1,0)), keyby = list(NC2)]
aggregates_NC2[,exit_in_16_18 := sum(exit*ifelse(anno>=2016 & anno <= 2018,1,0))/length(exit*ifelse(anno>=2016 & anno <= 2018,1,0)), keyby = list(NC2)]
aggregates_NC2[,exit_in_17_18 := sum(exit*ifelse(anno>=2017 & anno <= 2018,1,0))/length(exit*ifelse(anno>=2017 & anno <= 2018,1,0)), keyby = list(NC2)]
aggregates_NC2[,exit_in_18 := sum(exit*ifelse(anno>=2018 & anno <= 2018,1,0))/length(exit*ifelse(anno>=2018 & anno <= 2018,1,0)), keyby = list(NC2)]

#### Calculate the cumulated employment growth rate for various span
aggregates_NC2$anno <- as.integer(as.character(aggregates_NC2$anno))
aggregates_NC2[,gr_dip_11_19 := sum(gr_dipendenti, na.rm = T), keyby = list(NC2)]
aggregates_NC2[,gr_dip_12_19 := sum(gr_dipendenti*ifelse(anno>=2012,1,0), na.rm = T), keyby = list(NC2)]
aggregates_NC2[,gr_dip_13_19 := sum(gr_dipendenti*ifelse(anno>=2013,1,0), na.rm = T), keyby = list(NC2)]
aggregates_NC2[,gr_dip_14_19 := sum(gr_dipendenti*ifelse(anno>=2014,1,0), na.rm = T), keyby = list(NC2)]
aggregates_NC2[,gr_dip_15_19 := sum(gr_dipendenti*ifelse(anno>=2015,1,0), na.rm = T), keyby = list(NC2)]
aggregates_NC2[,gr_dip_16_19 := sum(gr_dipendenti*ifelse(anno>=2016,1,0), na.rm = T), keyby = list(NC2)]
aggregates_NC2[,gr_dip_17_19 := sum(gr_dipendenti*ifelse(anno>=2017,1,0), na.rm = T), keyby = list(NC2)]
aggregates_NC2[,gr_dip_18_19 := sum(gr_dipendenti*ifelse(anno>=2018,1,0), na.rm = T), keyby = list(NC2)]


#### Calculate the cumulated vagg growth rate for various span
aggregates_NC2[,gr_vagg_11_19 := sum(gr_vagg, na.rm = T), keyby = list(NC2)]
aggregates_NC2[,gr_vagg_12_19 := sum(gr_vagg*ifelse(anno>=2012,1,0), na.rm = T), keyby = list(NC2)]
aggregates_NC2[,gr_vagg_13_19 := sum(gr_vagg*ifelse(anno>=2013,1,0), na.rm = T), keyby = list(NC2)]
aggregates_NC2[,gr_vagg_14_19 := sum(gr_vagg*ifelse(anno>=2014,1,0), na.rm = T), keyby = list(NC2)]
aggregates_NC2[,gr_vagg_15_19 := sum(gr_vagg*ifelse(anno>=2015,1,0), na.rm = T), keyby = list(NC2)]
aggregates_NC2[,gr_vagg_16_19 := sum(gr_vagg*ifelse(anno>=2016,1,0), na.rm = T), keyby = list(NC2)]
aggregates_NC2[,gr_vagg_17_19 := sum(gr_vagg*ifelse(anno>=2017,1,0), na.rm = T), keyby = list(NC2)]
aggregates_NC2[,gr_vagg_18_19 := sum(gr_vagg*ifelse(anno>=2018,1,0), na.rm = T), keyby = list(NC2)]


#### Calculate the cumulated vagg growth rate for various span
aggregates_NC2$anno <- as.integer(as.character(aggregates_NC2$anno))
aggregates_NC2[,gr_vp_11_19 := sum(gr_vp, na.rm = T), keyby = list(NC2)]
aggregates_NC2[,gr_vp_12_19 := sum(gr_vp*ifelse(anno>=2012,1,0), na.rm = T), keyby = list(NC2)]
aggregates_NC2[,gr_vp_13_19 := sum(gr_vp*ifelse(anno>=2013,1,0), na.rm = T), keyby = list(NC2)]
aggregates_NC2[,gr_vp_14_19 := sum(gr_vp*ifelse(anno>=2014,1,0), na.rm = T), keyby = list(NC2)]
aggregates_NC2[,gr_vp_15_19 := sum(gr_vp*ifelse(anno>=2015,1,0), na.rm = T), keyby = list(NC2)]
aggregates_NC2[,gr_vp_16_19 := sum(gr_vp*ifelse(anno>=2016,1,0), na.rm = T), keyby = list(NC2)]
aggregates_NC2[,gr_vp_17_19 := sum(gr_vp*ifelse(anno>=2017,1,0), na.rm = T), keyby = list(NC2)]
aggregates_NC2[,gr_vp_18_19 := sum(gr_vp*ifelse(anno>=2018,1,0), na.rm = T), keyby = list(NC2)]



aggregates_NC2$NC2 <- as.factor(aggregates_NC2$NC2)
aggregates_NC2$anno <- as.factor(aggregates_NC2$anno)


######## Plot the net employment effect at the sectoral level #############

dt_NC2 <- unique(aggregates_NC2[,-c('vp','vagg','addetti','dipendenti','anno','gr_vp','gr_addetti','gr_vagg','gr_dipendenti','exit')])

dt_NC2 <- dt_NC2[,is_man := as.factor(ifelse(as.numeric(NC2)>=10 & as.numeric(NC2)<=33,1,0))]

ggplot(dt_NC2) +
geom_bar(aes(x = factor(NC2, levels = ord_NC2$NC2) , y = gr_dip_11_19, fill = as.factor(is_man)),
         color = 'black', stat = 'identity', size = 0.5, alpha = 0.8)  +
  theme_minimal() +
  theme(axis.text.x =  element_text(angle = 45, hjust = 1, size = 18),
        axis.text.y = element_text(size = 18),
        panel.grid.major.y = element_line(color = 'azure2'),
        panel.grid.major.x = element_blank(),
        panel.background = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = 'top',
        legend.title = element_text(size = 22),
        legend.key= element_blank(),
        legend.text = element_text(size = 18),
        strip.text.x = element_text(size = 20, color = "black", face = "bold"),
        strip.text.y = element_text(size = 20, color = "black", face = "bold")) +
  scale_fill_grey(start = 0.5,
                  end = 1, name = '', labels = c('Other sectors','Manufacturing'))



######### Construct the dataset of ateco statistics  #############

aggregates_ateco <- dt_exposure[,lapply(.SD, function(x) sum(x,  na.rm = T)), keyby = list(anno,NC2,ateco),
                           .SDcols = c('vp','vagg','addetti','dipendenti','exit')]


## Calculate growth rates

aggregates_ateco[,gr_vp := logG(vp), keyby = list(ateco)]
aggregates_ateco[,gr_vagg := logG(vagg), keyby = list(ateco)]
aggregates_ateco[,gr_addetti := logG(dipendenti), keyby = list(ateco)]
aggregates_ateco[,gr_dipendenti := logG(dipendenti), keyby = list(ateco)]
aggregates_ateco[,gr_dipendenti_on_sales := logG(dipendenti/log(1+vp)), keyby = list(ateco)]


aggregates_ateco$anno <- as.integer(as.character(aggregates_ateco$anno))
aggregates_ateco[,exit_in_11_18 := sum(exit*ifelse(anno>=2011 & anno <= 2018,1,0))/length(exit*ifelse(anno>=2011 & anno <= 2018,1,0)), keyby = list(ateco)]
aggregates_ateco[,exit_in_12_18 := sum(exit*ifelse(anno>=2012 & anno <= 2018,1,0))/length(exit*ifelse(anno>=2012 & anno <= 2018,1,0)), keyby = list(ateco)]
aggregates_ateco[,exit_in_13_18 := sum(exit*ifelse(anno>=2013 & anno <= 2018,1,0))/length(exit*ifelse(anno>=2013 & anno <= 2018,1,0)), keyby = list(ateco)]
aggregates_ateco[,exit_in_14_18 := sum(exit*ifelse(anno>=2014 & anno <= 2018,1,0))/length(exit*ifelse(anno>=2014 & anno <= 2018,1,0)), keyby = list(ateco)]
aggregates_ateco[,exit_in_15_18 := sum(exit*ifelse(anno>=2015 & anno <= 2018,1,0))/length(exit*ifelse(anno>=2015 & anno <= 2018,1,0)), keyby = list(ateco)]
aggregates_ateco[,exit_in_16_18 := sum(exit*ifelse(anno>=2016 & anno <= 2018,1,0))/length(exit*ifelse(anno>=2016 & anno <= 2018,1,0)), keyby = list(ateco)]
aggregates_ateco[,exit_in_17_18 := sum(exit*ifelse(anno>=2017 & anno <= 2018,1,0))/length(exit*ifelse(anno>=2017 & anno <= 2018,1,0)), keyby = list(ateco)]
aggregates_ateco[,exit_in_18 := sum(exit*ifelse(anno>=2018 & anno <= 2018,1,0))/length(exit*ifelse(anno>=2018 & anno <= 2018,1,0)), keyby = list(ateco)]

#### Calculate the cumulated employment growth rate for various span
aggregates_ateco$anno <- as.integer(as.character(aggregates_ateco$anno))
aggregates_ateco[,gr_dip_on_sales_11_19 := sum(gr_dipendenti_on_sales, na.rm = T), keyby = list(ateco)]
aggregates_ateco[,gr_dip_on_sales_12_19 := sum(gr_dipendenti_on_sales*ifelse(anno>=2012,1,0), na.rm = T), keyby = list(ateco)]
aggregates_ateco[,gr_dip_on_sales_13_19 := sum(gr_dipendenti_on_sales*ifelse(anno>=2013,1,0), na.rm = T), keyby = list(ateco)]
aggregates_ateco[,gr_dip_on_sales_14_19 := sum(gr_dipendenti_on_sales*ifelse(anno>=2014,1,0), na.rm = T), keyby = list(ateco)]
aggregates_ateco[,gr_dip_on_sales_15_19 := sum(gr_dipendenti_on_sales*ifelse(anno>=2015,1,0), na.rm = T), keyby = list(ateco)]
aggregates_ateco[,gr_dip_on_sales_16_19 := sum(gr_dipendenti_on_sales*ifelse(anno>=2016,1,0), na.rm = T), keyby = list(ateco)]
aggregates_ateco[,gr_dip_on_sales_17_19 := sum(gr_dipendenti_on_sales*ifelse(anno>=2017,1,0), na.rm = T), keyby = list(ateco)]
aggregates_ateco[,gr_dip_on_sales_18_19 := sum(gr_dipendenti_on_sales*ifelse(anno>=2018,1,0), na.rm = T), keyby = list(ateco)]


#### Calculate the cumulated employment growth rate for various span
aggregates_ateco$anno <- as.integer(as.character(aggregates_ateco$anno))
aggregates_ateco[,gr_dip_11_19 := sum(gr_dipendenti, na.rm = T), keyby = list(ateco)]
aggregates_ateco[,gr_dip_12_19 := sum(gr_dipendenti*ifelse(anno>=2012,1,0), na.rm = T), keyby = list(ateco)]
aggregates_ateco[,gr_dip_13_19 := sum(gr_dipendenti*ifelse(anno>=2013,1,0), na.rm = T), keyby = list(ateco)]
aggregates_ateco[,gr_dip_14_19 := sum(gr_dipendenti*ifelse(anno>=2014,1,0), na.rm = T), keyby = list(ateco)]
aggregates_ateco[,gr_dip_15_19 := sum(gr_dipendenti*ifelse(anno>=2015,1,0), na.rm = T), keyby = list(ateco)]
aggregates_ateco[,gr_dip_16_19 := sum(gr_dipendenti*ifelse(anno>=2016,1,0), na.rm = T), keyby = list(ateco)]
aggregates_ateco[,gr_dip_17_19 := sum(gr_dipendenti*ifelse(anno>=2017,1,0), na.rm = T), keyby = list(ateco)]
aggregates_ateco[,gr_dip_18_19 := sum(gr_dipendenti*ifelse(anno>=2018,1,0), na.rm = T), keyby = list(ateco)]


#### Calculate the cumulated vagg growth rate for various span
aggregates_ateco[,gr_vagg_11_19 := sum(gr_vagg, na.rm = T), keyby = list(ateco)]
aggregates_ateco[,gr_vagg_12_19 := sum(gr_vagg*ifelse(anno>=2012,1,0), na.rm = T), keyby = list(ateco)]
aggregates_ateco[,gr_vagg_13_19 := sum(gr_vagg*ifelse(anno>=2013,1,0), na.rm = T), keyby = list(ateco)]
aggregates_ateco[,gr_vagg_14_19 := sum(gr_vagg*ifelse(anno>=2014,1,0), na.rm = T), keyby = list(ateco)]
aggregates_ateco[,gr_vagg_15_19 := sum(gr_vagg*ifelse(anno>=2015,1,0), na.rm = T), keyby = list(ateco)]
aggregates_ateco[,gr_vagg_16_19 := sum(gr_vagg*ifelse(anno>=2016,1,0), na.rm = T), keyby = list(ateco)]
aggregates_ateco[,gr_vagg_17_19 := sum(gr_vagg*ifelse(anno>=2017,1,0), na.rm = T), keyby = list(ateco)]
aggregates_ateco[,gr_vagg_18_19 := sum(gr_vagg*ifelse(anno>=2018,1,0), na.rm = T), keyby = list(ateco)]


#### Calculate the cumulated vagg growth rate for various span
aggregates_ateco$anno <- as.integer(as.character(aggregates_ateco$anno))
aggregates_ateco[,gr_vp_11_19 := sum(gr_vp, na.rm = T), keyby = list(ateco)]
aggregates_ateco[,gr_vp_12_19 := sum(gr_vp*ifelse(anno>=2012,1,0), na.rm = T), keyby = list(ateco)]
aggregates_ateco[,gr_vp_13_19 := sum(gr_vp*ifelse(anno>=2013,1,0), na.rm = T), keyby = list(ateco)]
aggregates_ateco[,gr_vp_14_19 := sum(gr_vp*ifelse(anno>=2014,1,0), na.rm = T), keyby = list(ateco)]
aggregates_ateco[,gr_vp_15_19 := sum(gr_vp*ifelse(anno>=2015,1,0), na.rm = T), keyby = list(ateco)]
aggregates_ateco[,gr_vp_16_19 := sum(gr_vp*ifelse(anno>=2016,1,0), na.rm = T), keyby = list(ateco)]
aggregates_ateco[,gr_vp_17_19 := sum(gr_vp*ifelse(anno>=2017,1,0), na.rm = T), keyby = list(ateco)]
aggregates_ateco[,gr_vp_18_19 := sum(gr_vp*ifelse(anno>=2018,1,0), na.rm = T), keyby = list(ateco)]



aggregates_ateco$ateco <- as.factor(aggregates_ateco$ateco)
aggregates_ateco$anno <- as.factor(aggregates_ateco$anno)


########### Regressions at the 5-digit level    ################


## Aux packages ##
library(fixest)
library(sjPlot)
library(modelsummary)
library(kableExtra)

dt_ateco <- unique(aggregates_ateco[,
                                    -c('anno','vp','vagg','addetti','dipendenti','exit','gr_vp','gr_addetti','gr_dipendenti',
                                       'gr_dipendenti_on_sales')])

setkey(cumulated_sh_import_auto,ateco)
setkey(cumulated_sh_spike_auto,ateco)
setkey(cumulated_sh_spike_import_auto,ateco)
setkey(cumulated_sh_wgt_auto_import,ateco)
setkey(dt_ateco,ateco)
dt_ateco <- cumulated_sh_wgt_auto_import[cumulated_sh_spike_import_auto[cumulated_sh_spike_auto[cumulated_sh_import_auto[dt_ateco]]]]
dt_ateco[,is_man := as.factor(ifelse(as.numeric(NC2)>=10 & as.numeric(NC2)<=33,1,0))]
colnames(dt_ateco)


##### Define penetration variables over two windows (11-19, 11-15)#####

dataset <- unique(dt_ateco[,-c('gr_vagg','gr_dipendenti_on_sales')])
dataset[,sh_spike_import_auto_11_19_std := sh_spike_import_auto_11_19/mean(sh_import_auto_11_19, na.rm = T)]#(sh_spike_import_auto_11_19-mean(sh_import_auto_11_19, na.rm = T))/sd(sh_import_auto_11_19, na.rm = T)]
dataset[,sh_import_auto_11_19_std := sh_import_auto_11_19/mean(sh_import_auto_11_19, na.rm = T) ]#(sh_import_auto_11_19-mean(sh_import_auto_11_19, na.rm = T))/sd(sh_import_auto_11_19, na.rm = T)]
dataset[,sh_spike_cum_11_19_std := sh_spike_cum_11_19/mean(sh_spike_cum_11_19, na.rm = T)]#(sh_spike_cum_11_19-mean(sh_spike_cum_11_19, na.rm = T))/sd(sh_import_auto_11_19, na.rm = T)]
dataset[,sh_wgt_auto_import_11_19_std := sh_wgt_auto_import_11_19/mean(sh_wgt_auto_import_11_19 , na.rm = T) ]#(sh_wgt_auto_import_11_19 -mean(sh_wgt_auto_import_11_19 , na.rm = T))/sd(sh_wgt_auto_import_11_19 , na.rm = T)]

dataset[,sh_spike_import_auto_11_15_std := sh_spike_import_auto_11_15/mean(sh_import_auto_11_15, na.rm = T)]#(sh_spike_import_auto_11_19-mean(sh_import_auto_11_19, na.rm = T))/sd(sh_import_auto_11_19, na.rm = T)]
dataset[,sh_import_auto_11_15_std := sh_import_auto_11_15/mean(sh_import_auto_11_15, na.rm = T) ]#(sh_import_auto_11_19-mean(sh_import_auto_11_19, na.rm = T))/sd(sh_import_auto_11_19, na.rm = T)]
dataset[,sh_spike_cum_11_15_std := sh_spike_cum_11_15/mean(sh_spike_cum_11_15, na.rm = T)]#(sh_spike_cum_11_19-mean(sh_spike_cum_11_19, na.rm = T))/sd(sh_import_auto_11_19, na.rm = T)]
dataset[,sh_wgt_auto_import_11_15_std := sh_wgt_auto_import_11_15/mean(sh_wgt_auto_import_11_15 , na.rm = T) ]#(sh_wgt_auto_import_11_19 -mean(sh_wgt_auto_import_11_19 , na.rm = T))/sd(sh_wgt_auto_import_11_19 , na.rm = T)]



##### Employment/Sales model Robustness check - With penetration window 11-15 #####

### Three different specification
# - Without NC2 fixed effects 
# - Con NC2 fixed effects
# - NC2 + delta sales

lhs_rhs_members <- list(list(lhs = 'gr_dip_on_sales_17_19',rhs = ''),
                        list(lhs = 'gr_dip_on_sales_15_19',rhs = ''),
                        list(lhs = 'gr_dip_on_sales_13_19',rhs = ''),
                        list(lhs = 'gr_dip_on_sales_11_19',rhs = ''))



all_models <- NULL

for(var in c('sh_spike_import_auto_11_15_std',
             'sh_import_auto_11_15_std',
             'sh_spike_cum_11_15_std'))
  
{  
  for(lhs_rhs in lhs_rhs_members)
  {
    
    formula1 <- as.formula(paste0(lhs_rhs[['lhs']], '~', var))
    model1 <- feols(formula1,
                    cluster = ~  NC2 ,
                    data = unique(dataset))
    
    formula2 <- as.formula(paste0(lhs_rhs[['lhs']], ' ~', var,' | NC2'))
    model2 <- feols(formula2,
                    cluster = ~  NC2 ,
                    data = unique(dataset))
    
    formula3 <- as.formula(paste0(lhs_rhs[['lhs']],' ~ ', lhs_rhs[['rhs']] ,'+ ',var, '| NC2'))
    model3 <- feols(formula3,
                    cluster = ~  NC2 ,
                    data = unique(dataset))
    
    
    models_list <- list(model1, model2, model3)
    
    names(models_list) <- paste0(lhs_rhs[['lhs']],c(' - Spec1',' - Spec2',' - Spec3'))
    
    all_models <- c(all_models, models_list)
  }
}

modelsummary(all_models,
             statistic = 'std.error',
             fmt = fmt_significant(3),
             se.type = 'robust',
             stars = c('*' = .1, '**' = .05,'***' = 0.01))#,



cm <- c( 'gr_vp_17_19' = '$\\Delta\\text{Sales}_{17-19}$',
         'gr_vp_15_19' = '$\\Delta\\text{Sales}_{15-19}$',
         'gr_vp_13_19' = '$\\Delta\\text{Sales}_{13-19}$',
         'gr_vp_11_19' = '$\\Delta\\text{Sales}_{11-19}$',
         'sh_spike_cum_11_19_std' = '$\\text{ShSpike}_{11-19}(\\text{std.})$',
         'sh_spike_cum_11_15_std' = '$\\text{ShSpike}_{11-15}(\\text{std.})$',
         'sh_wgt_auto_import_11_19_std' = '$\\text{ShSpike}_{11-19}(\\text{std.})$',
         'sh_wgt_auto_import_11_15_std' = '$\\text{ShSpike}_{11-15}(\\text{std.})$',
         'sh_spike_import_auto_11_19_std' = '$\\text{SpikeVal}_{11-19}(\\text{std.})$',
         'sh_spike_import_auto_11_15_std' = '$\\text{SpikeVal}_{11-15}(\\text{std.})$',
         'sh_import_auto_11_19_std' = '$\\text{ShAutoImp}_{11-19}$(\\text{std.})',
         'sh_import_auto_11_15_std' = '$\\text{ShAutoImp}_{11-15}$(\\text{std.})')

names(all_models) <- rep(c( '$\\Delta\\text{Emp/Sales}_{17-19}$',
                            '$\\Delta\\text{Emp/Sales}_{17-19}$',
                            '$\\Delta\\text{Emp/Sales}_{17-19}$',
                            '$\\Delta\\text{Emp/Sales}_{15-19}$',
                            '$\\Delta\\text{Emp/Sales}_{15-19}$',
                            '$\\Delta\\text{Emp/Sales}_{15-19}$',
                            '$\\Delta\\text{Emp/Sales}_{13-19}$',
                            '$\\Delta\\text{Emp/Sales}_{13-19}$',
                            '$\\Delta\\text{Emp/Sales}_{13-19}$',
                            '$\\Delta\\text{Emp/Sales}_{11-19}$',
                            '$\\Delta\\text{Emp/Sales}_{11-19}$',
                            '$\\Delta\\text{Emp/Sales}_{11-19}$'),3)

tab_emp_ateco_baseline <- modelsummary(all_models[4:6],
                                       statistic = 'std.error',
                                       fmt = fmt_significant(1),
                                       coef_map = cm,
                                       se.type = 'robust',
                                       stars = c('*' = .1, '**' = .05,'***' = 0.01),
                                       output = 'latex',
                                       escape = F)#,


# Export table to latex
tab_emp_ateco_baseline <- add_header_above(tab_emp_ateco_baseline,c(" " = 1, 
                                                                    "$\\Delta\\text{Emp/Sales}_{15-19}$" = 3))

writeLines(tab_emp_ateco_baseline, paste0('tab_emp_on_sales_ateco_baseline_11_15_',identification_spike,'.tex'))


########## Select models with different windows

tab_emp_ateco_windows <- modelsummary(all_models[c(1:3,7:12)],
                                      statistic = 'std.error',
                                      fmt = fmt_significant(1),
                                      coef_map = cm,
                                      se.type = 'robust',
                                      stars = c('*' = .1, '**' = .05,'***' = 0.01),
                                      output = 'latex',
                                      escape = F)#,

names(all_models[c(1:3,7:12)])

# Export table to latex
tab_emp_ateco_windows <- add_header_above(tab_emp_ateco_windows,c(" " = 1, 
                                                                  "$\\Delta\\text{Emp/Sales}_{17-19}$" = 3,
                                                                  "$\\Delta\\text{Emp/Sales}_{13-19}$" = 3,
                                                                  "$\\Delta\\text{Emp/Sales}_{11-19}$" = 3))

writeLines(tab_emp_ateco_windows, paste0('tab_emp_on_sales_ateco_windows_11_15_',identification_spike,'.tex'))



############### Select models with different variables

mod_selection <- all_models[13:48]
tab_emp_ateco_variables <- modelsummary(mod_selection[grepl('11',names(mod_selection))|grepl('15',names(mod_selection))],
                                        statistic = 'std.error',
                                        fmt = fmt_significant(1),
                                        coef_map = cm,
                                        se.type = 'robust',
                                        stars = c('*' = .1, '**' = .05,'***' = 0.01),
                                        output = 'latex',
                                        escape = F)#,

names(mod_selection[grepl('11',names(mod_selection))|grepl('15',names(mod_selection))])

# Export table to latex
tab_emp_ateco_variables <- add_header_above(tab_emp_ateco_variables,c(" " = 1, 
                                                                      "$\\Delta\\text{Emp/Sales}_{15-19}$" = 3,
                                                                      "$\\Delta\\text{Emp/Sales}_{11-19}$" = 3,
                                                                      "$\\Delta\\text{Emp/Sales}_{15-19}$" = 3,
                                                                      "$\\Delta\\text{Emp/Sales}_{11-19}$" = 3))

writeLines(tab_emp_ateco_variables, paste0('tab_emp_on_sales_ateco_variables_11_15',identification_spike,'.tex'))




##### Employment/Sales model Robustness check - With penetration window 11-19 #####

### Three different specification
# - Without NC2 fixed effects 
# - Con NC2 fixed effects
# - NC2 + delta sales

lhs_rhs_members <- list(list(lhs = 'gr_dip_on_sales_17_19',rhs = ''),
                        list(lhs = 'gr_dip_on_sales_15_19',rhs = ''),
                        list(lhs = 'gr_dip_on_sales_13_19',rhs = ''),
                        list(lhs = 'gr_dip_on_sales_11_19',rhs = ''))
 

all_models <- NULL
#'sh_spike_import_auto_11_15_std','sh_import_auto_11_15_std','sh_spike_cum_11_15_std','sh_wgt_auto_import_11_15_std')
for(var in c('sh_spike_import_auto_11_19_std',
             'sh_import_auto_11_19_std',
             'sh_spike_cum_11_19_std'))

{  
for(lhs_rhs in lhs_rhs_members)
{
 
 formula1 <- as.formula(paste0(lhs_rhs[['lhs']], '~', var))
 model1 <- feols(formula1,
                cluster = ~  NC2 ,
                data = unique(dataset))
 
 formula2 <- as.formula(paste0(lhs_rhs[['lhs']], ' ~', var,' | NC2'))
 model2 <- feols(formula2,
                 cluster = ~  NC2 ,
                 data = unique(dataset))
 
 formula3 <- as.formula(paste0(lhs_rhs[['lhs']],' ~ ', lhs_rhs[['rhs']] ,'+ ',var, '| NC2'))
 model3 <- feols(formula3,
                 cluster = ~  NC2 ,
                 data = unique(dataset))
 
 
 models_list <- list(model1, model2, model3)
 
 names(models_list) <- paste0(lhs_rhs[['lhs']],c(' - Spec1',' - Spec2',' - Spec3'))
 
 all_models <- c(all_models, models_list)
}
}

modelsummary(all_models,
             statistic = 'std.error',
             fmt = fmt_significant(3),
             se.type = 'robust',
             stars = c('*' = .1, '**' = .05,'***' = 0.01))#,




cm <- c( 'gr_vp_17_19' = '$\\Delta\\text{Sales}_{17-19}$',
         'gr_vp_15_19' = '$\\Delta\\text{Sales}_{15-19}$',
         'gr_vp_13_19' = '$\\Delta\\text{Sales}_{13-19}$',
         'gr_vp_11_19' = '$\\Delta\\text{Sales}_{11-19}$',
         'sh_spike_cum_11_19_std' = '$\\text{ShSpike}_{11-19}(\\text{std.})$',
         'sh_spike_cum_11_15_std' = '$\\text{ShSpike}_{11-15}(\\text{std.})$',
         'sh_wgt_auto_import_11_19_std' = '$\\text{ShSpike}_{11-19}(\\text{std.})$',
         'sh_wgt_auto_import_11_15_std' = '$\\text{ShSpike}_{11-15}(\\text{std.})$',
         'sh_spike_import_auto_11_19_std' = '$\\text{SpikeVal}_{11-19}(\\text{std.})$',
         'sh_spike_import_auto_11_15_std' = '$\\text{SpikeVal}_{11-15}(\\text{std.})$',
         'sh_import_auto_11_19_std' = '$\\text{ShAutoImp}_{11-19}$(\\text{std.})',
         'sh_import_auto_11_15_std' = '$\\text{ShAutoImp}_{11-15}$(\\text{std.})')

names(all_models) <- rep(c( '$\\Delta\\text{Emp/Sales}_{17-19}$',
                            '$\\Delta\\text{Emp/Sales}_{17-19}$',
                            '$\\Delta\\text{Emp/Sales}_{17-19}$',
                            '$\\Delta\\text{Emp/Sales}_{15-19}$',
                            '$\\Delta\\text{Emp/Sales}_{15-19}$',
                            '$\\Delta\\text{Emp/Sales}_{15-19}$',
                            '$\\Delta\\text{Emp/Sales}_{13-19}$',
                            '$\\Delta\\text{Emp/Sales}_{13-19}$',
                            '$\\Delta\\text{Emp/Sales}_{13-19}$',
                            '$\\Delta\\text{Emp/Sales}_{11-19}$',
                            '$\\Delta\\text{Emp/Sales}_{11-19}$',
                            '$\\Delta\\text{Emp/Sales}_{11-19}$'),3)

tab_emp_ateco_baseline <- modelsummary(all_models[4:6],
                                       statistic = 'std.error',
                                       fmt = fmt_significant(1),
                                       coef_map = cm,
                                       se.type = 'robust',
                                       stars = c('*' = .1, '**' = .05,'***' = 0.01),
                                       output = 'latex',
                                       escape = F)#,


# Export table to latex
tab_emp_ateco_baseline <- add_header_above(tab_emp_ateco_baseline,c(" " = 1, 
                                                                    "$\\Delta\\text{Emp/Sales}_{15-19}$" = 3))

writeLines(tab_emp_ateco_baseline, paste0('tab_emp_on_sales_ateco_baseline_11_19_',identification_spike,'.tex'))


########## Select models with different windows

tab_emp_ateco_windows <- modelsummary(all_models[c(1:3,7:12)],
                                      statistic = 'std.error',
                                      fmt = fmt_significant(1),
                                      coef_map = cm,
                                      se.type = 'robust',
                                      stars = c('*' = .1, '**' = .05,'***' = 0.01),
                                      output = 'latex',
                                      escape = F)#,

names(all_models[c(1:3,7:12)])

# Export table to latex
tab_emp_ateco_windows <- add_header_above(tab_emp_ateco_windows,c(" " = 1, 
                                                                  "$\\Delta\\text{Emp/Sales}_{17-19}$" = 3,
                                                                  "$\\Delta\\text{Emp/Sales}_{13-19}$" = 3,
                                                                  "$\\Delta\\text{Emp/Sales}_{11-19}$" = 3))

writeLines(tab_emp_ateco_windows, paste0('tab_emp_on_sales_ateco_windows_11_19_',identification_spike,'.tex'))



############### Select models with different variables

mod_selection <- all_models[13:48]
tab_emp_ateco_variables <- modelsummary(mod_selection[grepl('11',names(mod_selection))|grepl('15',names(mod_selection))],
                                        statistic = 'std.error',
                                        fmt = fmt_significant(1),
                                        coef_map = cm,
                                        se.type = 'robust',
                                        stars = c('*' = .1, '**' = .05,'***' = 0.01),
                                        output = 'latex',
                                        escape = F)#,

names(mod_selection[grepl('11',names(mod_selection))|grepl('15',names(mod_selection))])

# Export table to latex
tab_emp_ateco_variables <- add_header_above(tab_emp_ateco_variables,c(" " = 1, 
                                                                      "$\\Delta\\text{Emp/Sales}_{15-19}$" = 3,
                                                                      "$\\Delta\\text{Emp/Sales}_{11-19}$" = 3,
                                                                      "$\\Delta\\text{Emp/Sales}_{15-19}$" = 3,
                                                                      "$\\Delta\\text{Emp/Sales}_{11-19}$" = 3))

writeLines(tab_emp_ateco_variables, paste0('tab_emp_on_sales_ateco_variables_11_19_',identification_spike,'.tex'))




##### Employment model - With penetration window 11-15 #####


lhs_rhs_members <- list(list(lhs = 'gr_dip_17_19',rhs = 'gr_vp_17_19'),
                        list(lhs = 'gr_dip_15_19',rhs = 'gr_vp_15_19'),
                        list(lhs = 'gr_dip_13_19',rhs = 'gr_vp_13_19'),
                        list(lhs = 'gr_dip_11_19',rhs = 'gr_vp_11_19'))


all_models <- NULL
#'sh_spike_import_auto_11_15_std','sh_import_auto_11_15_std','sh_spike_cum_11_15_std','sh_wgt_auto_import_11_15_std')
for(var in c('sh_spike_import_auto_11_15_std','sh_import_auto_11_15_std',
             'sh_spike_cum_11_15_std'))
{  
  for(lhs_rhs in lhs_rhs_members)
  {
    
    formula1 <- as.formula(paste0(lhs_rhs[['lhs']], '~', var))
    model1 <- feols(formula1,
                    cluster = ~  NC2 ,
                    data = unique(dataset))
    
    formula2 <- as.formula(paste0(lhs_rhs[['lhs']], ' ~', var,' | NC2'))
    model2 <- feols(formula2,
                    cluster = ~  NC2 ,
                    data = unique(dataset))
    
    formula3 <- as.formula(paste0(lhs_rhs[['lhs']],' ~ ', lhs_rhs[['rhs']] ,'+ ',var, '| NC2'))
    model3 <- feols(formula3,
                    cluster = ~  NC2 ,
                    data = unique(dataset))
    
    
    models_list <- list(model1, model2, model3)
    
    names(models_list) <- paste0(lhs_rhs[['lhs']],c(' - Spec1',' - Spec2',' - Spec3'))
    
    all_models <- c(all_models, models_list)
  }
}

modelsummary(all_models,
             statistic = 'std.error',
             fmt = fmt_significant(3),
             se.type = 'robust',
             stars = c('*' = .1, '**' = .05,'***' = 0.01))#,



cm <- c( 'gr_vp_17_19' = '$\\Delta\\text{Sales}_{17-19}$',
         'gr_vp_15_19' = '$\\Delta\\text{Sales}_{15-19}$',
         'gr_vp_13_19' = '$\\Delta\\text{Sales}_{13-19}$',
         'gr_vp_11_19' = '$\\Delta\\text{Sales}_{11-19}$',
         'sh_spike_cum_11_19_std' = '$\\text{ShSpike}_{11-19}(\\text{std.})$',
         'sh_spike_cum_11_15_std' = '$\\text{ShSpike}_{11-15}(\\text{std.})$',
         'sh_wgt_auto_import_11_19_std' = '$\\text{ShSpike}_{11-19}(\\text{std.})$',
         'sh_wgt_auto_import_11_15_std' = '$\\text{ShSpike}_{11-15}(\\text{std.})$',
         'sh_spike_import_auto_11_19_std' = '$\\text{SpikeVal}_{11-19}(\\text{std.})$',
         'sh_spike_import_auto_11_15_std' = '$\\text{SpikeVal}_{11-15}(\\text{std.})$',
         'sh_import_auto_11_19_std' = '$\\text{ShAutoImp}_{11-19}$(\\text{std.})',
         'sh_import_auto_11_15_std' = '$\\text{ShAutoImp}_{11-15}$(\\text{std.})')

names(all_models) <- rep(c( '$\\Delta\\text{Emp}_{17-19}$',
                            '$\\Delta\\text{Emp}_{17-19}$',
                            '$\\Delta\\text{Emp}_{17-19}$',
                            '$\\Delta\\text{Emp}_{15-19}$',
                            '$\\Delta\\text{Emp}_{15-19}$',
                            '$\\Delta\\text{Emp}_{15-19}$',
                            '$\\Delta\\text{Emp}_{13-19}$',
                            '$\\Delta\\text{Emp}_{13-19}$',
                            '$\\Delta\\text{Emp}_{13-19}$',
                            '$\\Delta\\text{Emp}_{11-19}$',
                            '$\\Delta\\text{Emp}_{11-19}$',
                            '$\\Delta\\text{Emp}_{11-19}$'),3)

tab_emp_ateco_baseline <- modelsummary(all_models[4:6],
                                       statistic = 'std.error',
                                       fmt = fmt_significant(1),
                                       coef_map = cm,
                                       se.type = 'robust',
                                       stars = c('*' = .1, '**' = .05,'***' = 0.01),
                                       output = 'latex',
                                       escape = F)#,


# Export table to latex
tab_emp_ateco_baseline <- add_header_above(tab_emp_ateco_baseline,c(" " = 1, 
                                                                    "$\\Delta\\text{Emp}_{15-19}$" = 3))

writeLines(tab_emp_ateco_baseline, paste0('tab_emp_ateco_baseline_11_15_',identification_spike,'.tex'))





########## Select models with different windows

tab_emp_ateco_windows <- modelsummary(all_models[c(1:3,7:12)],
                                      statistic = 'std.error',
                                      fmt = fmt_significant(1),
                                      coef_map = cm,
                                      se.type = 'robust',
                                      stars = c('*' = .1, '**' = .05,'***' = 0.01),
                                      output = 'latex',
                                      escape = F)#,


# Export table to latex
tab_emp_ateco_windows <- add_header_above(tab_emp_ateco_windows,c(" " = 1, 
                                                                  "$\\Delta\\text{Emp}_{17-19}$" = 3,
                                                                  "$\\Delta\\text{Emp}_{13-19}$" = 3,
                                                                  "$\\Delta\\text{Emp}_{11-19}$" = 3))

writeLines(tab_emp_ateco_windows, paste0('tab_emp_ateco_windows_11_15_',identification_spike,'.tex'))



############### Select models with different variables

mod_selection <- all_models[13:48]
tab_emp_ateco_variables <- modelsummary(mod_selection[grepl('11',names(mod_selection))|grepl('15',names(mod_selection))],
                                        statistic = 'std.error',
                                        fmt = fmt_significant(1),
                                        coef_map = cm,
                                        se.type = 'robust',
                                        stars = c('*' = .1, '**' = .05,'***' = 0.01),
                                        output = 'latex',
                                        escape = F)#,

names(mod_selection[grepl('11',names(mod_selection))|grepl('15',names(mod_selection))])

# Export table to latex
tab_emp_ateco_variables <- add_header_above(tab_emp_ateco_variables,c(" " = 1, 
                                                                      "$\\Delta\\text{Emp}_{15-19}$" = 3,
                                                                      "$\\Delta\\text{Emp}_{11-19}$" = 3,
                                                                      "$\\Delta\\text{Emp}_{15-19}$" = 3,
                                                                      "$\\Delta\\text{Emp}_{11-19}$" = 3))

writeLines(tab_emp_ateco_variables, paste0('tab_emp_ateco_variables_11_15_',identification_spike,'.tex'))





##### Employment model - With penetration window 11-19 #####


lhs_rhs_members <- list(list(lhs = 'gr_dip_17_19',rhs = 'gr_vp_17_19'),
                        list(lhs = 'gr_dip_15_19',rhs = 'gr_vp_15_19'),
                        list(lhs = 'gr_dip_13_19',rhs = 'gr_vp_13_19'),
                        list(lhs = 'gr_dip_11_19',rhs = 'gr_vp_11_19'))


all_models <- NULL
#'sh_spike_import_auto_11_15_std','sh_import_auto_11_15_std','sh_spike_cum_11_15_std','sh_wgt_auto_import_11_15_std')
for(var in c('sh_spike_import_auto_11_19_std','sh_import_auto_11_19_std',
             'sh_spike_cum_11_19_std'))
{  
  for(lhs_rhs in lhs_rhs_members)
  {
    
    formula1 <- as.formula(paste0(lhs_rhs[['lhs']], '~', var))
    model1 <- feols(formula1,
                    cluster = ~  NC2 ,
                    data = unique(dataset))
    
    formula2 <- as.formula(paste0(lhs_rhs[['lhs']], ' ~', var,' | NC2'))
    model2 <- feols(formula2,
                    cluster = ~  NC2 ,
                    data = unique(dataset))
    
    formula3 <- as.formula(paste0(lhs_rhs[['lhs']],' ~ ', lhs_rhs[['rhs']] ,'+ ',var, '| NC2'))
    model3 <- feols(formula3,
                    cluster = ~  NC2 ,
                    data = unique(dataset))
    
    
    models_list <- list(model1, model2, model3)
    
    names(models_list) <- paste0(lhs_rhs[['lhs']],c(' - Spec1',' - Spec2',' - Spec3'))
    
    all_models <- c(all_models, models_list)
  }
}

modelsummary(all_models,
             statistic = 'std.error',
             fmt = fmt_significant(3),
             se.type = 'robust',
             stars = c('*' = .1, '**' = .05,'***' = 0.01))#,



cm <- c( 'gr_vp_17_19' = '$\\Delta\\text{Sales}_{17-19}$',
         'gr_vp_15_19' = '$\\Delta\\text{Sales}_{15-19}$',
         'gr_vp_13_19' = '$\\Delta\\text{Sales}_{13-19}$',
         'gr_vp_11_19' = '$\\Delta\\text{Sales}_{11-19}$',
         'sh_spike_cum_11_19_std' = '$\\text{ShSpike}_{11-19}(\\text{std.})$', 
         'sh_spike_import_auto_11_19_std' = '$\\text{SpikeVal}_{11-19}(\\text{std.})$',
         'sh_import_auto_11_19_std' = '$\\text{ShAutoImp}_{11-19}$(\\text{std.})')

names(all_models) <- rep(c( '$\\Delta\\text{Emp}_{17-19}$',
                         '$\\Delta\\text{Emp}_{17-19}$',
                          '$\\Delta\\text{Emp}_{17-19}$',
                        '$\\Delta\\text{Emp}_{15-19}$',
                        '$\\Delta\\text{Emp}_{15-19}$',
                        '$\\Delta\\text{Emp}_{15-19}$',
                        '$\\Delta\\text{Emp}_{13-19}$',
                        '$\\Delta\\text{Emp}_{13-19}$',
                        '$\\Delta\\text{Emp}_{13-19}$',
                        '$\\Delta\\text{Emp}_{11-19}$',
                        '$\\Delta\\text{Emp}_{11-19}$',
                        '$\\Delta\\text{Emp}_{11-19}$'),3)

tab_emp_ateco_baseline <- modelsummary(all_models[4:6],
                                       statistic = 'std.error',
                                       fmt = fmt_significant(1),
                                       coef_map = cm,
                                       se.type = 'robust',
                                       stars = c('*' = .1, '**' = .05,'***' = 0.01),
                                       output = 'latex',
                                       escape = F)#,


# Export table to latex
tab_emp_ateco_baseline <- add_header_above(tab_emp_ateco_baseline,c(" " = 1, 
                                                    "$\\Delta\\text{Emp}_{15-19}$" = 3))

writeLines(tab_emp_ateco_baseline, paste0('tab_emp_ateco_baseline_11_19_',identification_spike,'.tex'))



########## Select models with different windows

tab_emp_ateco_windows <- modelsummary(all_models[c(1:3,7:12)],
                                       statistic = 'std.error',
                                       fmt = fmt_significant(1),
                                       coef_map = cm,
                                       se.type = 'robust',
                                       stars = c('*' = .1, '**' = .05,'***' = 0.01),
                                       output = 'latex',
                                       escape = F)#,


# Export table to latex
tab_emp_ateco_windows <- add_header_above(tab_emp_ateco_windows,c(" " = 1, 
                                                           "$\\Delta\\text{Emp}_{17-19}$" = 3,
                                                           "$\\Delta\\text{Emp}_{13-19}$" = 3,
                                                           "$\\Delta\\text{Emp}_{11-19}$" = 3))

writeLines(tab_emp_ateco_windows, paste0('tab_emp_ateco_windows_11_19_',identification_spike,'.tex'))



############### Select models with different variables

mod_selection <- all_models[13:48]
tab_emp_ateco_variables <- modelsummary(mod_selection[grepl('11',names(mod_selection))|grepl('15',names(mod_selection))],
                                       statistic = 'std.error',
                                       fmt = fmt_significant(1),
                                       coef_map = cm,
                                       se.type = 'robust',
                                       stars = c('*' = .1, '**' = .05,'***' = 0.01),
                                       output = 'latex',
                                       escape = F)#,

names(mod_selection[grepl('11',names(mod_selection))|grepl('15',names(mod_selection))])

# Export table to latex
tab_emp_ateco_variables <- add_header_above(tab_emp_ateco_variables,c(" " = 1, 
                                                                    "$\\Delta\\text{Emp}_{15-19}$" = 3,
                                                                    "$\\Delta\\text{Emp}_{11-19}$" = 3,
                                                                    "$\\Delta\\text{Emp}_{15-19}$" = 3,
                                                                    "$\\Delta\\text{Emp}_{11-19}$" = 3))

writeLines(tab_emp_ateco_variables, paste0('tab_emp_ateco_variables_11_19_',identification_spike,'.tex'))




#####################################################################
######## Regression on business stealing. Benchmark exercise ########
######## Using the automation+ai spikes. ############################
######## Studying sales, employment, exit rates #####################
#####################################################################


##### Basic cleaning of the firm-level dataset #####

dt_exposure[,first_sh_titolo := unique(sh_titolo_high[anno==first_year]), by = .(codice_unita)]
dt_exposure[,first_sh_occupation := unique(sh_Blue_collar[anno==first_year]), by = .(codice_unita)]
dt_exposure[,first_sh_eta := unique(sh_cl_eta_1[anno==first_year]), by = .(codice_unita)]
dt_exposure[,first_salary := unique(avg_retr_sett[anno==first_year]), by = .(codice_unita)]
dt_exposure[,first_dip := unique(dipendenti[anno==first_year]), by = .(codice_unita)]
dt_exposure[,avg_sh_he := mean(sh_titolo_high, na.rm = T), by = ateco]
dt_exposure[,emp_wgt_avg_sh_he := weighted.mean(sh_titolo_high, dipendenti, na.rm = T), by = ateco]
dt_exposure[,sizecl_coarse := cut(addetti,c(0,19,49,Inf),c('Micro/Small','Small','Medium/Large'))]
dt_exposure[,first_size_cl := unique(sizecl_coarse[anno==first_year]), by = codice_unita]





dt_exposure[, firm_auto_exposure := (import_auto_sec - as.numeric(ident_imp))/(1+import_auto_sec),
            by = list(codice_unita,anno)]


dt_exposure[, firm_wgt_dip_auto_exposure := (wgt_dip_import_auto_sec - as.numeric(ident_imp)*dipendenti)/(1+wgt_dip_import_auto_sec),
            by = list(codice_unita,anno)]


dt_exposure[, firm_wgt_sales_auto_exposure := (wgt_sales_import_auto_sec - as.numeric(ident_imp)*vp)/(1+wgt_sales_import_auto_sec),
            by = list(codice_unita,anno)]


dt_exposure[,firm_auto_norm_exp := (sum(as.numeric(ident_imp), na.rm = T)-as.numeric(ident_imp))/(sum(as.numeric(tot_import_val), na.rm = T)-as.numeric(tot_import_val)),
            keyby = list(anno,ateco)]

dt_exposure[,firm_wgt_dip_auto_norm_exp := (sum(as.numeric(ident_imp)*dipendenti, na.rm = T)-as.numeric(ident_imp)*dipendenti)/(sum(as.numeric(tot_import_val)*dipendenti, na.rm = T)-as.numeric(tot_import_val)*dipendenti),
            keyby = list(anno,ateco)]

dt_exposure[,firm_wgt_sales_auto_norm_exp := (sum(as.numeric(ident_imp)*vp, na.rm = T)-as.numeric(ident_imp)*vp)/(sum(as.numeric(tot_import_val)*vp, na.rm = T)-as.numeric(tot_import_val)*vp),
            keyby = list(anno,ateco)]




#dt[, firm_auto_exposure := import_auto_sec[ateco_ref == ateco],
#   by = list(codice_unita,anno)]

nrow(dt_exposure)
nrow(dt_restr)

unique(dt_exposure$lavoratore)
summary(dt_exposure[, list(lavoratore,
                           fl_idbase,
                           is_intermediary,
                           firm_auto_exposure,
                           firm_wgt_dip_auto_exposure,
                           firm_wgt_sales_auto_exposure)])


length(unique(dt_exposure$codice_unita))
length(unique(dt_restr$codice_unita))



dt_restr <- dt_exposure ##copy wihtout filtering


dt_restr[,is_man := as.factor(ifelse(as.numeric(NC2)>= 10 & as.numeric(NC2) <= 33,1,0))]

## convert years in factor vars
dt_restr$anno <- factor(dt_restr$anno)

setkey(dt_restr, ateco)
setkey(dataset, ateco)




dt_restr <- unique(dataset[,list(ateco,
                          sh_spike_import_auto_11_19_std,
                          sh_import_auto_11_19_std,
                          sh_spike_cum_11_19_std,
                          sh_spike_import_auto_11_15_std,
                          sh_import_auto_11_15_std,
                          sh_spike_cum_11_15_std)])[dt_restr]



dt_restr[,firm_imp_auto := ident_imp/import_auto_sec]
dt_restr[,firm_wgt_dip_imp_auto := ident_imp*dipendenti/wgt_dip_import_auto_sec]
dt_restr[,firm_wgt_sales_imp_auto := ident_imp*vp/wgt_sales_import_auto_sec]



dataset_firm <- unique(dt_restr[,c('anno','codice_unita','ateco','first_size','codice_reg',
                                   'dipendenti','addetti','vp','prod','sizecl_coarse',
                                    'first_prod','is_adopter','NC2','avg_sh_he',"emp_wgt_avg_sh_he",
                                   'firm_imp_auto','firm_wgt_dip_imp_auto','firm_wgt_sales_imp_auto',
                                    'sh_spike_import_auto_11_19_std','first_size_cl',
                                    'sh_import_auto_11_19_std',
                                    'sh_spike_cum_11_19_std',
                                    'sh_spike_import_auto_11_15_std',
                                    'sh_import_auto_11_15_std',
                                    'sh_spike_cum_11_15_std',
                                    'firm_auto_exposure',
                                    'firm_wgt_dip_auto_exposure',
                                    'firm_wgt_sales_auto_exposure',
                                    'firm_auto_norm_exp',
                                    'firm_wgt_dip_auto_norm_exp',
                                    'firm_wgt_sales_auto_norm_exp',
                                    paste0('liq_in_',11:17,'_19'),
                                    paste0('exit_in_',11:17,'_18'),
                                    paste0('gr_dip_',11:18,'_19'),
                                    paste0('gr_prod_',11:18,'_19'),
                                    paste0('gr_vp_',11:18,'_19')),
                                    with = F ][, n_years := .N,
                                    by = .(codice_unita, ateco)])

ateco_vars <- c('sh_spike_import_auto_11_19_std',
                'sh_import_auto_11_19_std',
                'sh_spike_cum_11_19_std',
                'sh_spike_import_auto_11_15_std',
                'sh_import_auto_11_15_std',
                'sh_spike_cum_11_15_std',
                'avg_sh_he',"emp_wgt_avg_sh_he")

dataset_firm[, (ateco_vars) := lapply(.SD, function(x) weighted.mean(x,n_years, na.rm =T)),
        .SDcols = ateco_vars, by = codice_unita]

dataset_firm <- dataset_firm[,ateco_max := ateco[which(n_years == max(n_years))][1],
                              by = codice_unita]
dataset_firm <- dataset_firm[,codice_reg_max := codice_reg[which(n_years == max(n_years))][1], by = codice_unita ]
dataset_firm[, ateco := ateco_max]
dataset_firm[,codice_reg := codice_reg_max]


dataset_firm <- unique(dataset_firm[,c('anno','codice_unita','ateco','first_size','codice_reg',
                                       'dipendenti','addetti','vp','prod','sizecl_coarse',
                                        'firm_imp_auto','firm_wgt_dip_imp_auto','firm_wgt_sales_imp_auto',
                                       'first_prod','is_adopter','NC2','avg_sh_he',"emp_wgt_avg_sh_he",
                                       'sh_spike_import_auto_11_19_std','first_size_cl',
                                       'sh_import_auto_11_19_std',
                                       'sh_spike_cum_11_19_std',
                                       'sh_spike_import_auto_11_15_std',
                                       'sh_import_auto_11_15_std',
                                       'sh_spike_cum_11_15_std',
                                       'firm_auto_exposure',
                                       'firm_wgt_dip_auto_exposure',
                                       'firm_wgt_sales_auto_exposure',
                                       'firm_auto_norm_exp',
                                       'firm_wgt_dip_auto_norm_exp',
                                       'firm_wgt_sales_auto_norm_exp',
                             'firm_wgt_sales_auto_norm_exp','n_years',
                             paste0('liq_in_',11:17,'_19'),
                             paste0('exit_in_',11:17,'_18'),
                             paste0('gr_dip_',11:18,'_19'),
                             paste0('gr_prod_',11:18,'_19'),
                             paste0('gr_vp_',11:18,'_19')), with = F ])


dataset_firm <- unique(dataset_firm)
dataset_firm[,.N, by = list(codice_unita,anno)][order(N)][N>2] ## check multiplicity


## ad hoc function to flag in one line
flag <- function(x) ifelse(x>0,1,0)

######## Run the time window regressions with cumulated effects on employment #############

###### Differential effect on employment growth between adopters and non-adopters ######


dataset_firm$first_size_cl <- factor(dataset_firm$first_size_cl, levels = c('Micro/Small','Small/Medium','Large') )

dataset_firm[,ln_dip := log(1+dipendenti)]
dataset_firm[,ln_dip_1 :=  shift(ln_dip,-1), by = codice_unita]
dataset_firm[,ln_dip_2 :=  shift(ln_dip,-2), by = codice_unita]
dataset_firm[,ln_dip_3 :=  shift(ln_dip,-3), by = codice_unita]
dataset_firm[,ln_dip_4 :=  shift(ln_dip,-4), by = codice_unita]
dataset_firm[,ln_dip_5 :=  shift(ln_dip,-5), by = codice_unita]


dataset_firm[,ln_dip_on_sales := log(1+dipendenti)/log(1+as.numeric(vp))]
dataset_firm[,ln_dip_on_sales_1 :=  shift(ln_dip_on_sales,-1), by = codice_unita]
dataset_firm[,ln_dip_on_sales_2 :=  shift(ln_dip_on_sales,-2), by = codice_unita]
dataset_firm[,ln_dip_on_sales_3 :=  shift(ln_dip_on_sales,-3), by = codice_unita]
dataset_firm[,ln_dip_on_sales_4 :=  shift(ln_dip_on_sales,-4), by = codice_unita]
dataset_firm[,ln_dip_on_sales_5 :=  shift(ln_dip_on_sales,-5), by = codice_unita]


dataset_firm[,ln_sales := log(1+vp)]
dataset_firm[,ln_sales_1 :=  shift(ln_sales,-1), by = codice_unita]
dataset_firm[,ln_sales_2 :=  shift(ln_sales,-2), by = codice_unita]
dataset_firm[,ln_sales_3 :=  shift(ln_sales,-3), by = codice_unita]
dataset_firm[,ln_sales_4 :=  shift(ln_sales,-4), by = codice_unita]
dataset_firm[,ln_sales_5 :=  shift(ln_sales,-5), by = codice_unita]

dataset_firm[,ln_prod := log(1+prod)]
dataset_firm[,ln_prod_1 :=  shift(ln_prod,-1), by = codice_unita]
dataset_firm[,ln_prod_2 :=  shift(ln_prod,-2), by = codice_unita]
dataset_firm[,ln_prod_3 :=  shift(ln_prod,-3), by = codice_unita]
dataset_firm[,ln_prod_4 :=  shift(ln_prod,-4), by = codice_unita]
dataset_firm[,ln_prod_5 :=  shift(ln_prod,-5), by = codice_unita]




dataset_firm[,is_non_adopter := (1-is_adopter)]

all_models <- NULL

## Running linear models with fixest

for(dep_var in c('ln_dip','ln_sales','ln_dip_on_sales','ln_prod'))
{
for(explan_var in c('firm_imp_auto + firm_auto_norm_exp + firm_auto_norm_exp',
                    'firm_imp_auto + firm_wgt_dip_auto_norm_exp + firm_wgt_dip_auto_norm_exp',
                    'firm_imp_auto + firm_wgt_sales_auto_norm_exp + firm_wgt_sales_auto_norm_exp'))
{
  
  lhs_members <- paste0(dep_var,'_',1:5)
  
  rhs_controls <- c('first_size')

  regr_list <- apply(rbind(lhs_members,rhs_controls),2,as.list)
  
  estimation_by_char <- function(lhs_and_controls)
  {
    print(lhs_and_controls)
    
    formula <- as.formula(paste0(lhs_and_controls[[1]] , '- ', dep_var,' ~ ',lhs_and_controls[[2]],' + ',
                                 explan_var,':is_non_adopter',
                                 #' + ',
                                 #paste(c('first_sh_titolo','first_sh_occupation','first_sh_eta',
                                 #       'first_salary'), collapse = ' + '),
                                 '|  factor(anno) + factor(ateco)'))
    model <- feols(formula,
                   cluster = ~  NC2,
                   data = dataset_firm)
    return(model)
  }
  
  models_list <- lapply(regr_list,estimation_by_char)
  
  names(models_list) <- paste0(lhs_members)
  
  
  all_models <- c(all_models, models_list)
}
}




library(stargazer)
library(dplyr)

extract_coef_t_sig <- function(model, var_name) {
  coef <- model$coefficients[var_name]
  t_val <- coef / model$se[var_name]
  p_val <- summary(model)$coeftable[,4][var_name]
  stars <- case_when(
    p_val < 0.001 ~ "***",
    p_val < 0.01 ~ "**",
    p_val < 0.05 ~ "*",
    p_val < 0.1 ~ ".",
    TRUE ~ ""
  )
  return(c(sprintf("%.3f%s", coef, stars), sprintf("[%.2f]", t_val)))
}

# Function to create a table for a set of regressions
create_table <- function(models, var_names, titles) {
  
  results <- matrix(nrow = 2*(length(models)/5), ncol = 16)
  
  for (i in 1:length(models)) {

    firm_coef_t <- extract_coef_t_sig(models[[i]], var_names[1])
    comp_coef_t <- extract_coef_t_sig(models[[i]], var_names[2])
    comp_non_adopt_coef_t <- extract_coef_t_sig(models[[i]], var_names[3])
    

    row_indicator <- ifelse(i%%5 != 0, i %% 5, 5)

    results[2*floor((i-1)/5) + 1,c(row_indicator, row_indicator + 5, row_indicator + 10)] <- c(firm_coef_t[1], comp_coef_t[1], comp_non_adopt_coef_t[1])
    results[2*floor((i-1)/5) + 2,c(row_indicator, row_indicator + 5, row_indicator + 10)] <- c(firm_coef_t[2], comp_coef_t[2], comp_non_adopt_coef_t[2])
    results[2*floor((i-1)/5) + 1,16] <- titles[floor((i-1)/5) + 1]
    results[2*floor((i-1)/5) + 2,16] <- titles[floor((i-1)/5) + 1] 
  }
  
   
  aux_names_vector <- rle(results[,16])[[1]]
  names(aux_names_vector) <- rle(results[,16])[[2]]
  
  
  table <- kable(results[,-16], format = "latex", booktabs = TRUE, align = "c", caption = paste0(titles, collapse = ' '), label = paste0("tab:")) %>%
    kable_styling(latex_options = c("scale_down", "hold_position")) %>%
    add_header_above(c("1" = 1, "2" = 1, "3" = 1, "4" = 1, "5" = 1,
                       "1" = 1, "2" = 1, "3" = 1, "4" = 1, "5" = 1,
                       "1" = 1, "2" = 1, "3" = 1, "4" = 1, "5" = 1)) %>%
    add_header_above(c("Firm (Horizon)" = 5, "Competitors (Horizon)" = 5, "Competitors Non-Adopters (Horizon)" = 5)) %>%
    pack_rows(index = aux_names_vector) 
  
  
  return(table)
}

# Create tables for each set of regressions
table1 <- create_table(models = all_models[c(1:5,16:20,31:35,46:50)], 
                       var_names = c("firm_imp_auto", "firm_auto_norm_exp", "firm_auto_norm_exp:is_non_adopter"), 
                       titles = c("Employment","Sales","Employment on Sales",'Productivity'))


table2 <-  create_table(all_models[c(6:10,21:25,36:40,51:55)], 
                          c("firm_imp_auto", "firm_wgt_dip_auto_norm_exp", "firm_wgt_dip_auto_norm_exp:is_non_adopter"),
                          titles = c("Employment","Sales","Employment on Sales",'Productivity'))

table3 <-  create_table(all_models[c(11:15,26:30,41:45,56:60)],
                          c("firm_imp_auto", "firm_wgt_sales_auto_norm_exp", "firm_wgt_sales_auto_norm_exp:is_non_adopter"),
                          titles = c("Employment","Sales","Employment on Sales",'Productivity'))


# Combine all tables into one LaTeX file
cat(
  "\\documentclass{article}",
  "\\usepackage{booktabs}",
  "\\usepackage{caption}",
  "\\begin{document}", 
  table1, table2, table3,
  "\\end{document}",
  file = "competition_horizon_tables.tex",
  sep = "\n\n"
)

### Run the same battery of regressions without the 2-digits fixed effects ####

all_models <- NULL

## Running linear models with fixest

for(dep_var in c('ln_dip','ln_sales','ln_dip_on_sales','ln_prod'))
{
  for(explan_var in c('firm_imp_auto + firm_auto_norm_exp + firm_auto_norm_exp',
                      'firm_imp_auto + firm_wgt_dip_auto_norm_exp + firm_wgt_dip_auto_norm_exp',
                      'firm_imp_auto + firm_wgt_sales_auto_norm_exp + firm_wgt_sales_auto_norm_exp'))
  {
    
    lhs_members <- paste0(dep_var,'_',1:5)
    
    rhs_controls <- c('first_size')
    
    regr_list <- apply(rbind(lhs_members,rhs_controls),2,as.list)
    
    estimation_by_char <- function(lhs_and_controls)
    {
      print(lhs_and_controls)
      
      formula <- as.formula(paste0(lhs_and_controls[[1]] , '- ', dep_var,' ~ ',lhs_and_controls[[2]],' + ',
                                   explan_var,':is_non_adopter',
                                   #' + ',
                                   #paste(c('first_sh_titolo','first_sh_occupation','first_sh_eta',
                                   #       'first_salary'), collapse = ' + '),
                                   '|  factor(anno) + factor(NC2)'))
      model <- feols(formula,
                     cluster = ~  NC2,
                     data = dataset_firm)
      return(model)
    }
    
    models_list <- lapply(regr_list,estimation_by_char)
    
    names(models_list) <- paste0(lhs_members)
    
    
    all_models <- c(all_models, models_list)
  }
}




library(stargazer)
library(dplyr)

extract_coef_t_sig <- function(model, var_name) {
  coef <- model$coefficients[var_name]
  t_val <- coef / model$se[var_name]
  p_val <- summary(model)$coeftable[,4][var_name]
  stars <- case_when(
    p_val < 0.001 ~ "***",
    p_val < 0.01 ~ "**",
    p_val < 0.05 ~ "*",
    p_val < 0.1 ~ ".",
    TRUE ~ ""
  )
  return(c(sprintf("%.3f%s", coef, stars), sprintf("[%.2f]", t_val)))
}

# Function to create a table for a set of regressions
create_table <- function(models, var_names, titles) {
  
  results <- matrix(nrow = 2*(length(models)/5), ncol = 16)
  
  for (i in 1:length(models)) {
    
    firm_coef_t <- extract_coef_t_sig(models[[i]], var_names[1])
    comp_coef_t <- extract_coef_t_sig(models[[i]], var_names[2])
    comp_non_adopt_coef_t <- extract_coef_t_sig(models[[i]], var_names[3])
    
    
    row_indicator <- ifelse(i%%5 != 0, i %% 5, 5)
    
    results[2*floor((i-1)/5) + 1,c(row_indicator, row_indicator + 5, row_indicator + 10)] <- c(firm_coef_t[1], comp_coef_t[1], comp_non_adopt_coef_t[1])
    results[2*floor((i-1)/5) + 2,c(row_indicator, row_indicator + 5, row_indicator + 10)] <- c(firm_coef_t[2], comp_coef_t[2], comp_non_adopt_coef_t[2])
    results[2*floor((i-1)/5) + 1,16] <- titles[floor((i-1)/5) + 1]
    results[2*floor((i-1)/5) + 2,16] <- titles[floor((i-1)/5) + 1] 
  }
  
  
  aux_names_vector <- rle(results[,16])[[1]]
  names(aux_names_vector) <- rle(results[,16])[[2]]
  
  
  table <- kable(results[,-16], format = "latex", booktabs = TRUE, align = "c", caption = paste0(titles, collapse = ' '), label = paste0("tab:")) %>%
    kable_styling(latex_options = c("scale_down", "hold_position")) %>%
    add_header_above(c("1" = 1, "2" = 1, "3" = 1, "4" = 1, "5" = 1,
                       "1" = 1, "2" = 1, "3" = 1, "4" = 1, "5" = 1,
                       "1" = 1, "2" = 1, "3" = 1, "4" = 1, "5" = 1)) %>%
    add_header_above(c("Firm (Horizon)" = 5, "Competitors (Horizon)" = 5, "Competitors Non-Adopters (Horizon)" = 5)) %>%
    pack_rows(index = aux_names_vector) 
  
  
  return(table)
}

# Create tables for each set of regressions
table1 <- create_table(models = all_models[c(1:5,16:20,31:35,46:50)], 
                       var_names = c("firm_imp_auto", "firm_auto_norm_exp", "firm_auto_norm_exp:is_non_adopter"), 
                       titles = c("Employment","Sales","Employment on Sales",'Productivity'))


table2 <-  create_table(all_models[c(6:10,21:25,36:40,51:55)], 
                        c("firm_imp_auto", "firm_wgt_dip_auto_norm_exp", "firm_wgt_dip_auto_norm_exp:is_non_adopter"),
                        titles = c("Employment","Sales","Employment on Sales",'Productivity'))

table3 <-  create_table(all_models[c(11:15,26:30,41:45,56:60)],
                        c("firm_imp_auto", "firm_wgt_sales_auto_norm_exp", "firm_wgt_sales_auto_norm_exp:is_non_adopter"),
                        titles = c("Employment","Sales","Employment on Sales",'Productivity'))


# Combine all tables into one LaTeX file
cat(
  "\\documentclass{article}",
  "\\usepackage{booktabs}",
  "\\usepackage{caption}",
  "\\begin{document}", 
  table1, table2, table3,
  "\\end{document}",
  file = "competition_horizon_tables_NC2_FE.tex",
  sep = "\n\n"
)



### Run the same battery of regressions without the 2-digits fixed effects ####

all_models <- NULL

## Running linear models with fixest

for(dep_var in c('ln_dip','ln_sales','ln_dip_on_sales','ln_prod'))
{
  for(explan_var in c('firm_imp_auto + firm_auto_norm_exp + firm_auto_norm_exp',
                      'firm_imp_auto + firm_wgt_dip_auto_norm_exp + firm_wgt_dip_auto_norm_exp',
                      'firm_imp_auto + firm_wgt_sales_auto_norm_exp + firm_wgt_sales_auto_norm_exp'))
  {
    
    lhs_members <- paste0(dep_var,'_',1:5)
    
    rhs_controls <- c('first_size')
    
    regr_list <- apply(rbind(lhs_members,rhs_controls),2,as.list)
    
    estimation_by_char <- function(lhs_and_controls)
    {
      print(lhs_and_controls)
      
      formula <- as.formula(paste0(lhs_and_controls[[1]] , '- ', dep_var,' ~ ',lhs_and_controls[[2]],' + ',
                                   explan_var,':is_non_adopter',
                                   #' + ',
                                   #paste(c('first_sh_titolo','first_sh_occupation','first_sh_eta',
                                   #       'first_salary'), collapse = ' + '),
                                   '|  factor(anno) + factor(NC2)'))
      model <- feols(formula,
                     cluster = ~  NC2,
                     data = dataset_firm)
      return(model)
    }
    
    models_list <- lapply(regr_list,estimation_by_char)
    
    names(models_list) <- paste0(lhs_members)
    
    
    all_models <- c(all_models, models_list)
  }
}




library(stargazer)
library(dplyr)

extract_coef_t_sig <- function(model, var_name) {
  coef <- model$coefficients[var_name]
  t_val <- coef / model$se[var_name]
  p_val <- summary(model)$coeftable[,4][var_name]
  stars <- case_when(
    p_val < 0.001 ~ "***",
    p_val < 0.01 ~ "**",
    p_val < 0.05 ~ "*",
    p_val < 0.1 ~ ".",
    TRUE ~ ""
  )
  return(c(sprintf("%.3f%s", coef, stars), sprintf("[%.2f]", t_val)))
}

# Function to create a table for a set of regressions
create_table <- function(models, var_names, titles) {
  
  results <- matrix(nrow = 2*(length(models)/5), ncol = 16)
  
  for (i in 1:length(models)) {
    
    firm_coef_t <- extract_coef_t_sig(models[[i]], var_names[1])
    comp_coef_t <- extract_coef_t_sig(models[[i]], var_names[2])
    comp_non_adopt_coef_t <- extract_coef_t_sig(models[[i]], var_names[3])
    
    
    row_indicator <- ifelse(i%%5 != 0, i %% 5, 5)
    
    results[2*floor((i-1)/5) + 1,c(row_indicator, row_indicator + 5, row_indicator + 10)] <- c(firm_coef_t[1], comp_coef_t[1], comp_non_adopt_coef_t[1])
    results[2*floor((i-1)/5) + 2,c(row_indicator, row_indicator + 5, row_indicator + 10)] <- c(firm_coef_t[2], comp_coef_t[2], comp_non_adopt_coef_t[2])
    results[2*floor((i-1)/5) + 1,16] <- titles[floor((i-1)/5) + 1]
    results[2*floor((i-1)/5) + 2,16] <- titles[floor((i-1)/5) + 1] 
  }
  
  
  aux_names_vector <- rle(results[,16])[[1]]
  names(aux_names_vector) <- rle(results[,16])[[2]]
  
  
  table <- kable(results[,-16], format = "latex", booktabs = TRUE, align = "c", caption = paste0(titles, collapse = ' '), label = paste0("tab:")) %>%
    kable_styling(latex_options = c("scale_down", "hold_position")) %>%
    add_header_above(c("1" = 1, "2" = 1, "3" = 1, "4" = 1, "5" = 1,
                       "1" = 1, "2" = 1, "3" = 1, "4" = 1, "5" = 1,
                       "1" = 1, "2" = 1, "3" = 1, "4" = 1, "5" = 1)) %>%
    add_header_above(c("Firm (Horizon)" = 5, "Competitors (Horizon)" = 5, "Competitors Non-Adopters (Horizon)" = 5)) %>%
    pack_rows(index = aux_names_vector) 
  
  
  return(table)
}

# Create tables for each set of regressions
table1 <- create_table(models = all_models[c(1:5,16:20,31:35,46:50)], 
                       var_names = c("firm_imp_auto", "firm_auto_norm_exp", "firm_auto_norm_exp:is_non_adopter"), 
                       titles = c("Employment","Sales","Employment on Sales",'Productivity'))


table2 <-  create_table(all_models[c(6:10,21:25,36:40,51:55)], 
                        c("firm_imp_auto", "firm_wgt_dip_auto_norm_exp", "firm_wgt_dip_auto_norm_exp:is_non_adopter"),
                        titles = c("Employment","Sales","Employment on Sales",'Productivity'))

table3 <-  create_table(all_models[c(11:15,26:30,41:45,56:60)],
                        c("firm_imp_auto", "firm_wgt_sales_auto_norm_exp", "firm_wgt_sales_auto_norm_exp:is_non_adopter"),
                        titles = c("Employment","Sales","Employment on Sales",'Productivity'))


# Combine all tables into one LaTeX file
cat(
  "\\documentclass{article}",
  "\\usepackage{booktabs}",
  "\\usepackage{caption}",
  "\\begin{document}", 
  table1, table2, table3,
  "\\end{document}",
  file = "competition_horizon_tables_NC2_FE.tex",
  sep = "\n\n"
)



### Run the same battery of regressions without the 2-digits fixed effects ####

all_models <- NULL

## Running linear models with fixest

for(dep_var in c('ln_dip','ln_sales','ln_dip_on_sales','ln_prod'))
{
  for(explan_var in c('firm_imp_auto + firm_auto_norm_exp + firm_auto_norm_exp',
                      'firm_imp_auto + firm_wgt_dip_auto_norm_exp + firm_wgt_dip_auto_norm_exp',
                      'firm_imp_auto + firm_wgt_sales_auto_norm_exp + firm_wgt_sales_auto_norm_exp'))
  {
    
    lhs_members <- paste0(dep_var,'_',1:5)
    
    rhs_controls <- c('first_size')
    
    regr_list <- apply(rbind(lhs_members,rhs_controls),2,as.list)
    
    estimation_by_char <- function(lhs_and_controls)
    {
      print(lhs_and_controls)
      
      formula <- as.formula(paste0(lhs_and_controls[[1]] , '- ', dep_var,' ~ ',lhs_and_controls[[2]],' + ',
                                   explan_var,':is_non_adopter',
                                   #' + ',
                                   #paste(c('first_sh_titolo','first_sh_occupation','first_sh_eta',
                                   #       'first_salary'), collapse = ' + '),
                                   '|  factor(anno) + factor(NC2)'))
      model <- feols(formula,
                     cluster = ~  NC2,
                     data = dataset_firm)
      return(model)
    }
    
    models_list <- lapply(regr_list,estimation_by_char)
    
    names(models_list) <- paste0(lhs_members)
    
    
    all_models <- c(all_models, models_list)
  }
}




library(stargazer)
library(dplyr)

extract_coef_t_sig <- function(model, var_name) {
  coef <- model$coefficients[var_name]
  t_val <- coef / model$se[var_name]
  p_val <- summary(model)$coeftable[,4][var_name]
  stars <- case_when(
    p_val < 0.001 ~ "***",
    p_val < 0.01 ~ "**",
    p_val < 0.05 ~ "*",
    p_val < 0.1 ~ ".",
    TRUE ~ ""
  )
  return(c(sprintf("%.3f%s", coef, stars), sprintf("[%.2f]", t_val)))
}

# Function to create a table for a set of regressions
create_table <- function(models, var_names, titles) {
  
  results <- matrix(nrow = 2*(length(models)/5), ncol = 16)
  
  for (i in 1:length(models)) {
    
    firm_coef_t <- extract_coef_t_sig(models[[i]], var_names[1])
    comp_coef_t <- extract_coef_t_sig(models[[i]], var_names[2])
    comp_non_adopt_coef_t <- extract_coef_t_sig(models[[i]], var_names[3])
    
    
    row_indicator <- ifelse(i%%5 != 0, i %% 5, 5)
    
    results[2*floor((i-1)/5) + 1,c(row_indicator, row_indicator + 5, row_indicator + 10)] <- c(firm_coef_t[1], comp_coef_t[1], comp_non_adopt_coef_t[1])
    results[2*floor((i-1)/5) + 2,c(row_indicator, row_indicator + 5, row_indicator + 10)] <- c(firm_coef_t[2], comp_coef_t[2], comp_non_adopt_coef_t[2])
    results[2*floor((i-1)/5) + 1,16] <- titles[floor((i-1)/5) + 1]
    results[2*floor((i-1)/5) + 2,16] <- titles[floor((i-1)/5) + 1] 
  }
  
  
  aux_names_vector <- rle(results[,16])[[1]]
  names(aux_names_vector) <- rle(results[,16])[[2]]
  
  
  table <- kable(results[,-16], format = "latex", booktabs = TRUE, align = "c", caption = paste0(titles, collapse = ' '), label = paste0("tab:")) %>%
    kable_styling(latex_options = c("scale_down", "hold_position")) %>%
    add_header_above(c("1" = 1, "2" = 1, "3" = 1, "4" = 1, "5" = 1,
                       "1" = 1, "2" = 1, "3" = 1, "4" = 1, "5" = 1,
                       "1" = 1, "2" = 1, "3" = 1, "4" = 1, "5" = 1)) %>%
    add_header_above(c("Firm (Horizon)" = 5, "Competitors (Horizon)" = 5, "Competitors Non-Adopters (Horizon)" = 5)) %>%
    pack_rows(index = aux_names_vector) 
  
  
  return(table)
}

# Create tables for each set of regressions
table1 <- create_table(models = all_models[c(1:5,16:20,31:35,46:50)], 
                       var_names = c("firm_imp_auto", "firm_auto_norm_exp", "firm_auto_norm_exp:is_non_adopter"), 
                       titles = c("Employment","Sales","Employment on Sales",'Productivity'))


table2 <-  create_table(all_models[c(6:10,21:25,36:40,51:55)], 
                        c("firm_imp_auto", "firm_wgt_dip_auto_norm_exp", "firm_wgt_dip_auto_norm_exp:is_non_adopter"),
                        titles = c("Employment","Sales","Employment on Sales",'Productivity'))

table3 <-  create_table(all_models[c(11:15,26:30,41:45,56:60)],
                        c("firm_imp_auto", "firm_wgt_sales_auto_norm_exp", "firm_wgt_sales_auto_norm_exp:is_non_adopter"),
                        titles = c("Employment","Sales","Employment on Sales",'Productivity'))


# Combine all tables into one LaTeX file
cat(
  "\\documentclass{article}",
  "\\usepackage{booktabs}",
  "\\usepackage{caption}",
  "\\begin{document}", 
  table1, table2, table3,
  "\\end{document}",
  file = "competition_horizon_tables_NC2_FE.tex",
  sep = "\n\n"
)

### Run the same battery of regressions with the 2-digits fixed effects ####

all_models <- NULL

## Running linear models with fixest

for(dep_var in c('ln_dip','ln_sales','ln_dip_on_sales','ln_prod'))
{
  for(explan_var in c('firm_imp_auto + firm_auto_norm_exp + firm_auto_norm_exp',
                      'firm_imp_auto + firm_wgt_dip_auto_norm_exp + firm_wgt_dip_auto_norm_exp',
                      'firm_imp_auto + firm_wgt_sales_auto_norm_exp + firm_wgt_sales_auto_norm_exp'))
  {
    
    lhs_members <- paste0(dep_var,'_',1:5)
    
    rhs_controls <- c('first_size')
    
    regr_list <- apply(rbind(lhs_members,rhs_controls),2,as.list)
    
    estimation_by_char <- function(lhs_and_controls)
    {
      print(lhs_and_controls)
      
      formula <- as.formula(paste0(lhs_and_controls[[1]] , '- ', dep_var,' ~ ',lhs_and_controls[[2]],' + ',
                                   explan_var,':is_non_adopter',
                                   #' + ',
                                   #paste(c('first_sh_titolo','first_sh_occupation','first_sh_eta',
                                   #       'first_salary'), collapse = ' + '),
                                   '|  factor(anno) + factor(NC2)'))
      model <- feols(formula,
                     cluster = ~  NC2,
                     data = dataset_firm)
      return(model)
    }
    
    models_list <- lapply(regr_list,estimation_by_char)
    
    names(models_list) <- paste0(lhs_members)
    
    
    all_models <- c(all_models, models_list)
  }
}




library(stargazer)
library(dplyr)

extract_coef_t_sig <- function(model, var_name) {
  coef <- model$coefficients[var_name]
  t_val <- coef / model$se[var_name]
  p_val <- summary(model)$coeftable[,4][var_name]
  stars <- case_when(
    p_val < 0.001 ~ "***",
    p_val < 0.01 ~ "**",
    p_val < 0.05 ~ "*",
    p_val < 0.1 ~ ".",
    TRUE ~ ""
  )
  return(c(sprintf("%.3f%s", coef, stars), sprintf("[%.2f]", t_val)))
}

# Function to create a table for a set of regressions
create_table <- function(models, var_names, titles) {
  
  results <- matrix(nrow = 2*(length(models)/5), ncol = 16)
  
  for (i in 1:length(models)) {
    
    firm_coef_t <- extract_coef_t_sig(models[[i]], var_names[1])
    comp_coef_t <- extract_coef_t_sig(models[[i]], var_names[2])
    comp_non_adopt_coef_t <- extract_coef_t_sig(models[[i]], var_names[3])
    
    
    row_indicator <- ifelse(i%%5 != 0, i %% 5, 5)
    
    results[2*floor((i-1)/5) + 1,c(row_indicator, row_indicator + 5, row_indicator + 10)] <- c(firm_coef_t[1], comp_coef_t[1], comp_non_adopt_coef_t[1])
    results[2*floor((i-1)/5) + 2,c(row_indicator, row_indicator + 5, row_indicator + 10)] <- c(firm_coef_t[2], comp_coef_t[2], comp_non_adopt_coef_t[2])
    results[2*floor((i-1)/5) + 1,16] <- titles[floor((i-1)/5) + 1]
    results[2*floor((i-1)/5) + 2,16] <- titles[floor((i-1)/5) + 1] 
  }
  
  
  aux_names_vector <- rle(results[,16])[[1]]
  names(aux_names_vector) <- rle(results[,16])[[2]]
  
  
  table <- kable(results[,-16], format = "latex", booktabs = TRUE, align = "c", caption = paste0(titles, collapse = ' '), label = paste0("tab:")) %>%
    kable_styling(latex_options = c("scale_down", "hold_position")) %>%
    add_header_above(c("1" = 1, "2" = 1, "3" = 1, "4" = 1, "5" = 1,
                       "1" = 1, "2" = 1, "3" = 1, "4" = 1, "5" = 1,
                       "1" = 1, "2" = 1, "3" = 1, "4" = 1, "5" = 1)) %>%
    add_header_above(c("Firm (Horizon)" = 5, "Competitors (Horizon)" = 5, "Competitors Non-Adopters (Horizon)" = 5)) %>%
    pack_rows(index = aux_names_vector) 
  
  
  return(table)
}

# Create tables for each set of regressions
table1 <- create_table(models = all_models[c(1:5,16:20,31:35,46:50)], 
                       var_names = c("firm_imp_auto", "firm_auto_norm_exp", "firm_auto_norm_exp:is_non_adopter"), 
                       titles = c("Employment","Sales","Employment on Sales",'Productivity'))


table2 <-  create_table(all_models[c(6:10,21:25,36:40,51:55)], 
                        c("firm_imp_auto", "firm_wgt_dip_auto_norm_exp", "firm_wgt_dip_auto_norm_exp:is_non_adopter"),
                        titles = c("Employment","Sales","Employment on Sales",'Productivity'))

table3 <-  create_table(all_models[c(11:15,26:30,41:45,56:60)],
                        c("firm_imp_auto", "firm_wgt_sales_auto_norm_exp", "firm_wgt_sales_auto_norm_exp:is_non_adopter"),
                        titles = c("Employment","Sales","Employment on Sales",'Productivity'))


# Combine all tables into one LaTeX file
cat(
  "\\documentclass{article}",
  "\\usepackage{booktabs}",
  "\\usepackage{caption}",
  "\\begin{document}", 
  table1, table2, table3,
  "\\end{document}",
  file = "competition_horizon_tables_NC2_FE.tex",
  sep = "\n\n"
)


### Run the same battery of regressions without sectoral fixed effects ####

all_models <- NULL

## Running linear models with fixest

for(dep_var in c('ln_dip','ln_sales','ln_dip_on_sales','ln_prod'))
{
  for(explan_var in c('firm_imp_auto + firm_auto_norm_exp + firm_auto_norm_exp',
                      'firm_imp_auto + firm_wgt_dip_auto_norm_exp + firm_wgt_dip_auto_norm_exp',
                      'firm_imp_auto + firm_wgt_sales_auto_norm_exp + firm_wgt_sales_auto_norm_exp'))
  {
    
    lhs_members <- paste0(dep_var,'_',1:5)
    
    rhs_controls <- c('first_size')
    
    regr_list <- apply(rbind(lhs_members,rhs_controls),2,as.list)
    
    estimation_by_char <- function(lhs_and_controls)
    {
      print(lhs_and_controls)
      
      formula <- as.formula(paste0(lhs_and_controls[[1]] , '- ', dep_var,' ~ ',lhs_and_controls[[2]],' + ',
                                   explan_var,':is_non_adopter',
                                   #' + ',
                                   #paste(c('first_sh_titolo','first_sh_occupation','first_sh_eta',
                                   #       'first_salary'), collapse = ' + '),
                                   '|  factor(anno) '))
      model <- feols(formula,
                     cluster = ~ ateco,
                     data = dataset_firm)
      return(model)
    }
    
    models_list <- lapply(regr_list,estimation_by_char)
    
    names(models_list) <- paste0(lhs_members)
    
    
    all_models <- c(all_models, models_list)
  }
}


library(stargazer)
library(dplyr)

extract_coef_t_sig <- function(model, var_name) {
  coef <- model$coefficients[var_name]
  t_val <- coef / model$se[var_name]
  p_val <- summary(model)$coeftable[,4][var_name]
  stars <- case_when(
    p_val < 0.001 ~ "***",
    p_val < 0.01 ~ "**",
    p_val < 0.05 ~ "*",
    p_val < 0.1 ~ ".",
    TRUE ~ ""
  )
  return(c(sprintf("%.3f%s", coef, stars), sprintf("[%.2f]", t_val)))
}

# Function to create a table for a set of regressions
create_table <- function(models, var_names, titles) {
  
  results <- matrix(nrow = 2*(length(models)/5), ncol = 16)
  
  for (i in 1:length(models)) {
    
    firm_coef_t <- extract_coef_t_sig(models[[i]], var_names[1])
    comp_coef_t <- extract_coef_t_sig(models[[i]], var_names[2])
    comp_non_adopt_coef_t <- extract_coef_t_sig(models[[i]], var_names[3])
    
    
    row_indicator <- ifelse(i%%5 != 0, i %% 5, 5)
    
    results[2*floor((i-1)/5) + 1,c(row_indicator, row_indicator + 5, row_indicator + 10)] <- c(firm_coef_t[1], comp_coef_t[1], comp_non_adopt_coef_t[1])
    results[2*floor((i-1)/5) + 2,c(row_indicator, row_indicator + 5, row_indicator + 10)] <- c(firm_coef_t[2], comp_coef_t[2], comp_non_adopt_coef_t[2])
    results[2*floor((i-1)/5) + 1,16] <- titles[floor((i-1)/5) + 1]
    results[2*floor((i-1)/5) + 2,16] <- titles[floor((i-1)/5) + 1] 
  }
  
  
  aux_names_vector <- rle(results[,16])[[1]]
  names(aux_names_vector) <- rle(results[,16])[[2]]
  
  
  table <- kable(results[,-16], format = "latex", booktabs = TRUE, align = "c", caption = paste0(titles, collapse = ' '), label = paste0("tab:")) %>%
    kable_styling(latex_options = c("scale_down", "hold_position")) %>%
    add_header_above(c("1" = 1, "2" = 1, "3" = 1, "4" = 1, "5" = 1,
                       "1" = 1, "2" = 1, "3" = 1, "4" = 1, "5" = 1,
                       "1" = 1, "2" = 1, "3" = 1, "4" = 1, "5" = 1)) %>%
    add_header_above(c("Firm (Horizon)" = 5, "Competitors (Horizon)" = 5, "Competitors Non-Adopters (Horizon)" = 5)) %>%
    pack_rows(index = aux_names_vector) 
  
  
  return(table)
}

# Create tables for each set of regressions
table1 <- create_table(models = all_models[c(1:5,16:20,31:35,46:50)], 
                       var_names = c("firm_imp_auto", "firm_auto_norm_exp", "firm_auto_norm_exp:is_non_adopter"), 
                       titles = c("Employment","Sales","Employment on Sales",'Productivity'))


table2 <-  create_table(all_models[c(6:10,21:25,36:40,51:55)], 
                        c("firm_imp_auto", "firm_wgt_dip_auto_norm_exp", "firm_wgt_dip_auto_norm_exp:is_non_adopter"),
                        titles = c("Employment","Sales","Employment on Sales",'Productivity'))

table3 <-  create_table(all_models[c(11:15,26:30,41:45,56:60)],
                        c("firm_imp_auto", "firm_wgt_sales_auto_norm_exp", "firm_wgt_sales_auto_norm_exp:is_non_adopter"),
                        titles = c("Employment","Sales","Employment on Sales",'Productivity'))


# Combine all tables into one LaTeX file
cat(
  "\\documentclass{article}",
  "\\usepackage{booktabs}",
  "\\usepackage{caption}",
  "\\begin{document}", 
  table1, table2, table3,
  "\\end{document}",
  file = "competition_horizon_tables_no_sector_FE.tex",
  sep = "\n\n"
)
