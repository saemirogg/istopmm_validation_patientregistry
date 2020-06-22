## This script pulls out individuals with and without diagnoses that have been chosen for validation.
library(dplyr)
library(readxl)
library(stringr)

#We use the diagnosis data available from a previous script
source("diagnosis_data.R")

#Creating distinct codes for individuals
id_data <- diagnosis_data %>%
  filter(coding_system=="ICD-10",
         str_detect(code,"^[A-Z]")) %>% #only valid ICD-10 codes
  distinct(study_id,code) %>%
  filter(!is.na(code))

#import diagnostic codes
diagnostic_codes <- read_xlsx("codes_validation.xlsx") %>%
  group_by(comorbidity) %>%
  summarise(search_string=paste(code,collapse="|")) 

#Generate random sample for each comorbidity
for(i in 1:nrow(diagnostic_codes)){
  set.seed(5) #reproducible randomness
  
  #We create a single file for each comorbidity we plan to validate
  pos_data <- id_data %>%
    mutate(comorbidity=str_detect(code,diagnostic_codes$search_string[i]))%>%
    filter(comorbidity) %>%
    filter(!duplicated(study_id)) #In order to retrieve unique ids

  #Skipping sampling to exclude positive individuals from the sampling of negativs
  
  set.seed(5)
  neg_data <- id_data %>%
    mutate(comorbidity=str_detect(code,diagnostic_codes$search_string[i]))%>%
    filter(!comorbidity, !(study_id %in% pos_data$study_id)) %>% #Removing individuals who have other diagnoses than the one of interest
    filter(!duplicated(study_id))%>%
    sample_n(30)
  
  set.seed(5)
  #Now we can sample from the positive dataset
  pos_data <- pos_data %>%
    sample_n(30)
    
    write.xlsx(bind_rows(pos_data,neg_data),file=str_replace_all(paste(diagnostic_codes$comorbidity[i],"_validation_dataset_",Sys.Date(),".xlsx",sep=""),"-",""))
  
}

#cleanup
rm(pos_data,neg_data,id_data,diagnostic_codes,diagnosis_data,i)
