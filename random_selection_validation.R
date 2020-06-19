## This script pulls out individuals with and without diagnoses that have been chosen for validation.
library(dplyr)
library(readxl)
library(stringr)

#We use the diagnosis data available from a previous script
source(diagnosis_data.R)

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
  exp_data <- id_data %>%
    mutate(comorbidity=str_detect(code,diagnostic_codes$search_string[i]))%>%
    group_by(comorbidity)%>%
    filter(!duplicated(study_id))%>% #In order to retrive unique ids i.e. not repeated ids
    sample_n(30) %>%
    data.frame() %>% #resolving formatting issue for write.xlsx function
    write.xlsx(.,file=str_replace_all(paste(diagnostic_codes$comorbidity[i],"_validation_dataset_",Sys.Date(),".xlsx",sep=""),"-",""))
  
}
