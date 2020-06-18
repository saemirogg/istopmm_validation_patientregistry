## R script that creates a table of all the different diagnostic codes in the diagnosis_data dataset

#Subset of data with only ICD-10 codes
code_data <- diagnosis_data %>%
  filter(coding_system=="ICD-10")

#n of different visits, admissions etc. Since all have at least 1 code. We can count the number of first codes for each type
n_visits <- diagnosis_data %>%
  group_by(src) %>%
  summarise(n=sum(code_order==1))

n_visits

#Listing the rate of different codes. I.e. total rate
code_table <- diagnosis_data %>%
  filter(coding_system=="ICD-10") %>%
  group_by(code) %>%
  summarise(n_all=n(),
            n_admissions=sum(src=="hospital_admission"),
            n_outp=sum(src=="hospital_outpatient"),
            n_pc=sum(src=="primary_care")
  ) %>%
  mutate(per100k_all=round(n_all*100000/sum(n_visits$n),digits = 2),
         per100k_admissions=round(n_admissions*100000/as.numeric(n_visits[1,2]),digits = 2),
         per100k_outp=round(n_outp*100000/as.numeric(n_visits[2,2]),digits = 2),
         per100k_pc=round(n_pc*100000/as.numeric(n_visits[3,2]),digits = 2))

n_participants = 80752 #Number of participants that registered. Maually entered.
  
  
#Looking at number of individuals with the different diagnostic codes
id_table <- distinct(code_data,study_id,code) %>%
  group_by(code) %>%
  summarise(n_ids=n(),
            per100k_ids=round(n()*100000/n_participants,digits=2)
  )

total_table <- left_join(code_table,id_table,by="code")

library(xlsx)

write.xlsx(total_table,file="codes_table.xlsx")
