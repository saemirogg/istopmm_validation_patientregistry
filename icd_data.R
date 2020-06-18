## Here we clean up the diagnosis_data with regard to non-valid ICD codes and create a table with aggregated codes for simplified manual assessment
## Much of the code is reused from code_table.R
library(tidyverse)
library(stringr)

#generating diagnosis_data
source("diagnosis_data.R")

#There are some codes in the dataset that are not ICD 10 codes. They are not usable. Let's cound these codes

diagnosis_data %>%
  filter(coding_system=="ICD-10",!str_detect(code,"^[A-Z]")) %>%
  count()
#951 codes among >7million codes. These codes are not comparable to other codes and are therefore not interpretable.

#All ICD-10 codes start on a capital letter let's filter them in
icd_data <- diagnosis_data %>%
  filter(coding_system=="ICD-10",str_detect(code,"^[A-Z]"))%>%
  mutate(simple_code= str_extract(code,"^.{3}")) #Extracting only first three letters


#n of different visits, admissions etc. Since all have at least 1 code. We can count the number of first codes for each type
n_visits <- diagnosis_data %>%
  group_by(src) %>%
  summarise(n=sum(code_order==1))

#Table
icd_table <- icd_data %>%
  group_by(simple_code) %>%
  summarise(n_all=n(),
            n_admissions=sum(src=="hospital_admission"),
            n_outp=sum(src=="hospital_outpatient"),
            n_pc=sum(src=="primary_care")
  ) %>%
  mutate(per100k_all=round(n_all*100000/sum(n_visits$n),digits = 2),
         per100k_admissions=round(n_admissions*100000/as.numeric(n_visits[1,2]),digits = 2),
         per100k_outp=round(n_outp*100000/as.numeric(n_visits[2,2]),digits = 2),
         per100k_pc=round(n_pc*100000/as.numeric(n_visits[3,2]),digits = 2)
         )

n_participants = 80752 #Number of participants that registered. Maually entered.

#Adding data on data on individual level rather than n of diagnoses
icd_table <- distinct(icd_data,study_id,simple_code) %>%
  group_by(simple_code) %>%
  summarise(n_ids=n(),
            per100k_ids=round(n()*100000/n_participants,digits=2)
  ) %>%
  right_join(icd_table)

library(xlsx)

write.xlsx(icd_table,file="icd_table.xlsx")
