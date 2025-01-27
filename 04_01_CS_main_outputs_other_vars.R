#######################################
####### Firm-level event study ########
#######################################


# This R script conducts the benchmark Difference-in-Differences (DiD) exercise of the paper accessible at 
# the DOI: https://doi.org/10.1016/j.euroecorev.2024.104943. 
# The code analyzes the impact of specific technological spikes on various economic outcomes.

# Inputs:
# - Data is imported using the `data_import()` from the folder 'elaborated_datasets' containing the dataset 'firm_level_clean_complete_rv24.csv'
# - The script cycles through predefined values for `spike_type` (set to 'spike2' -> within spike) and `tech_type` (multiple types including 'autn_rob_ai').
# - For definitions of spike types and technological classes, please refer to the aforementioned paper.

# Outputs:
# - The results of the event study are saved to disk in the form of RData files. 
# - Each set of results is stored in a directory named 'DiD_results/main_outputs_other_vars', with filenames reflecting the spike identification and variable types.
# - The final output is a consolidated dataset saved as 'complete_CS_att_results.RData' in the same directory.


# Outcome variables analyzed in the cycle:
# The analysis includes the following key variables:
# - "ln_vadd": Log of value added
# - "ln_lab_share": Log of labor share
# - "ln_avg_retr_sett": Log of average weekly salaries
# - "ln_top10_bottom10": Log ratio of top 10 to bottom 10 earnings
# - "sh_Manager": Share of managers
# - "sh_Middle_management": Share of middle management
# - "sh_Blue_collar": Share of blue-collar workers
# - "sh_White_collar": Share of white-collar workers
# - "sh_Trainee": Share of trainees
# - "sh_permanent": Share of permanent employees
# - "sh_temporary": Share of temporary employees
# - "sh_full_time": Share of full-time employees
# - "sh_part_time": Share of part-time employees


# The script employs various functions for data reshaping, subsampling, and statistical estimation, 
# which are sourced from an auxiliary R script (`AUX_DiD_exercise_functions.R`). 
# It also maintains logs of the processing time for monitoring purposes.



#### Clean the environment ####
rm(list = ls())

###### Import libraries ####
library(here)


##### Source the functions for data handling and estimation ####
source(here::here('AUX_DiD_exercise_functions.R'))


##### Import data #####
base_dt <- data_import()

folder_exercise <- 'main_outputs_other_vars'
dir.create(here::here('DiD_results',folder_exercise))

results_par <- list()
results_dts <- list()  



for(spike_type in c('spike2')) # cycle over the spike type 'spike2' -> within spike, 'spike3' -> between spike, 'spike4' -> within-between spike
{
  for(tech_type in c('autn_rob_ai'))#, cycle over the technologies 'aut_rob', 'aut_rob_ai', 'automn', 'autn_rob', 'autn_rob_ai', 'ndt'
  {  
    
    identification_spike <- paste0(spike_type,'_',tech_type)
    
    
    #### Reshape data according to spike definitions ####
    df <- data_reshaping(base_dt,
                         spike_type,
                         tech_type)
    
    
    rec <- 'Re-exporters exclusion - Minimal'
    tp <- 'Total economy'
    
    ## Select sample types
    sample_types <- c('Total economy - Adopters only',
                      'Total economy')
    
    ## Select reexporte exclusion
    reexporter_check <- c('Re-exporters exclusion - Minimal')
    
    
    for(rec in reexporter_check)
    {  
      
        cat(rec,' - ',as.character(Sys.time()),'\n')  
      

        for(tp in sample_types)
        {
        
          
          cat('..',tp,' - ',as.character(Sys.time()),'\n')  
            
          ### Subsample data according to reexporting restriction and sampling definitions
          sub_sample <- subsampling(df, 
                                    rec = rec,
                                    tp = tp)
          
          
          
          ## Define var_list
          var_list <-   c("ln_dip",#,
                          'ln_prod',
                          'ln_sales',
                          'ln_vadd', 
                          "ln_lab_share",
                          "ln_avg_retr_sett",
                          "ln_top10_bottom10",
                          "sh_Manager","sh_Middle_management","sh_Blue_collar","sh_White_collar","sh_Trainee",
                          "sh_permanent","sh_temporary","sh_full_time","sh_part_time")
                          
          for(vr in var_list)
          {  
          
            cat('....',tp,' - ',as.character(Sys.time()),'\n')
               redux_dts_list <- matching(sub_sample,
                                         tp = tp, 
                                         vr = vr) 
                
               redux_sample <- redux_dts_list$redux ### select matching with ratio equal 2
                
              
    
              ####### Compute the Callaway Sant'Anna group-time estimator without weights after systematic pretesting
              
              ant_param <- 1
              for(anticipation_num in 0:ant_param)
              {  
                
                ant_label <- paste0('Ant. years = ',anticipation_num)
                
                cat('......',ant_label,'\n')  
                
                if(grepl('Adopters only',tp))
                {
                  control_type <- 'notyettreated'
                }else{
                  control_type <- 'nevertreated'
                }  
                
                estimation_label <- paste0(identification_spike,'-',vr,'-',tp,'-',rec,'-',ant_label)
                estimation_label <- gsub('/','_',estimation_label)
                
                
                formula_string <- '~ is_exporter + initial_gr_dip + ln_prod + ln_sales + sh_titolo_low + sh_White_collar + sh_full_time' 
                
                ## Exclude the output variable from the list of controls if present
                formula_string <- gsub(paste0('+ ', vr),'',formula_string)
                
                ## Exclude companion variables
                formula_string <- ifelse(vr %in% c("sh_Manager","sh_Middle_management","sh_Blue_collar","sh_White_collar","sh_Trainee"),
                                         gsub('sh_White_collar \\+ ','',formula_string), formula_string)
                
                formula_string <- ifelse(vr %in% c("sh_full_time","sh_part_time"),
                                         gsub(' \\+ sh_full_time','',formula_string), formula_string)
                
                
                estimates_dt <- estimation(redux_sample,
                                          identification_spike,
                                          vr, tp, rec,
                                          control_type = 'nevertreated', 
                                          anticipation_num,
                                          estimation_label,
                                          formula_string,
                                          wgt= NULL,
                                          folder_exercise) 
                
                results_par[[estimation_label]] <- estimates_dt ## store the estimates
                
           }
          }
      }
    }
    
    
    export_dt <- do.call(rbind,results_par)
    setDT(export_dt)
    results_dts[[paste0(identification_spike)]] <- export_dt
    
    dir.create(here('DiD_results',folder_exercise))
    print(here('DiD_results',folder_exercise,paste0(identification_spike,'_all_vars_CS_att_results.RData')))
    save(export_dt, file = here('DiD_results',folder_exercise,paste0(identification_spike,'_all_vars_CS_att_results.RData')))

  }
}

full_results_dataset <- do.call(rbind,results_dts)
dir.create(here('DiD_results',folder_exercise))
print(here('DiD_results',folder_exercise,'complete_CS_att_results.RData'))
save(full_results_dataset, file = here('DiD_results',folder_exercise,'complete_CS_att_results.RData'))



