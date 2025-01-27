#######################################
######### Dataset preparation #########
#######################################

## This script prepares the dataset to be used for the main DiD estimates
## It integrates three firm-level datasets from different sources:
## 1. Elaborated international trade data with import automation and spike variables (coe0519_coll_newspk_clean_red.dta)
## 2. Elaborated data with firm-level information on wages, workers and occupational structure (firm_dataset_leed_variables.csv)
## 3. Integrate firm-level balance sheet variables from FRAME-SBS register (a set of datasets from the universe)


## Main output: firm_level_clean_complete_rv24.csv
## Stored in the folder: elaborated_datasets

#### Clean the environment ####
rm(list = ls())

#### Import libraries ####

library(data.table)
library(here)
library(haven)


#### 1. Import firm-level dataset with international trade and automation-spike variables ####

i_am('Task_3.2_revisions.Rproj')
dt_spike <- read_dta(here('source_datasets','coe0519_coll_newspk_clean_red.dta'))
setDT(dt_spike)

summary(dt_spike)


### Calculate the number of firms in dt_spike
### length(unique(dt_spike$codice_unita))

### Calculate the number of firms and years in dt_spike
### nrow(unique(dt_spike[, list(anno,codice_unita)]))


### 2. Import firm level dataset containing workers related information ####

dt <- fread(here('source_datasets','firm_dataset_leed_variables.csv'))
dt$fl_idbase <- as.factor(dt$fl_idbase)

dt[,N_dip_indip := .N, by = list(codice_unita,anno)]
dt <- dt[(N_dip_indip == 1)|(N_dip_indip == 2 & lavoratore == 'DIP'),] ### keep only the variables for dip or indip if no dip are availables

dt[,N_dip_indip := NULL]

### Calculate the number of firms in dt
### length(unique(dt[fl_idbase == 1]$codice_unita))

### Calculate the number of firms and years in dt_spike
### nrow(unique(dt[fl_idbase == 1, list(anno,codice_unita)]))


#### 3. Merge the dt with spike variables and the dt with LEED-derived variables ####
dt <- dt[,c('anno','codice_unita',setdiff(colnames(dt),intersect(colnames(dt),colnames(dt_spike)))),with = F]
dt <- merge(dt_spike,dt, by = c('anno','codice_unita'), all.x = T)

### Calculate the number of firms in the merged dt
### length(unique(dt$codice_unita))

### Calculate the number of firms and years in the merged dt
### nrow(unique(dt[,list(anno,codice_unita)]))



#### 4. Import the dataset with additional variables from frame ####

i_am('desktop.ini')
file_list <- list.files(here('Task_3.2','dataset_univ'),pattern = 'frame*')
zip_list <- list.files(here('Task_3.2','dataset_univ'), pattern = 'zip')

file_list <- setdiff(file_list,zip_list)

dt_frame_list <- list()
for(fold in file_list)
{
  dt_frame_list[[fold]] <- fread(here('Task_3.2','dataset_univ',fold,paste0(fold,'.csv')))
}

dt_frame <- do.call(rbind,dt_frame_list)
dt_frame <- dt_frame[codice_unita %in% unique(dt$codice_unita)]

#### Select the variables from frame which are not already contained in the original dataset
dt <- dt[,c('anno','codice_unita',setdiff(colnames(dt),intersect(colnames(dt),colnames(dt_frame)))),
         with = F]

#### 5. Merge the dt containing spike variables and LEED-derived variable with additional balance-sheet firm level variables ####
dt <- merge(dt,dt_frame, by = c('anno','codice_unita'), all = T)

### Calculate the number of firms in the merged dt
### length(unique(dt$codice_unita))

### Calculate the number of firms and years in the merged dt
### nrow(unique(dt[,list(anno,codice_unita)]))

i_am('Task_3.2_revisions.Rproj')


#### 6. Filter intermediaries ####

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


#### 6. Store the dataset ####
fwrite(dt, file = here('elaborated_datasets','firm_level_clean_complete_rv24.csv'))

### Calculate the number of firms in the final dt
### length(unique(dt$codice_unita))

### Calculate the number of firms and years in the final dt
### nrow(unique(dt[,list(anno,codice_unita)]))

