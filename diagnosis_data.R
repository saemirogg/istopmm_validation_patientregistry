#install.packages("RMySQL")
#install.packages("tidyverse")

library(RMySQL)
library(dplyr)
library(lubridate)
library(tidyr)


db_connection <- dbConnect(MySQL(), user="saemi", password="PASSWORD (ATH ÞETTA ER EKKI PASSWORDIÐ)" , dbname="istopmm_data", host="127.0.0.1")

dbListTables(db_connection)

#Information about each admission including study ID
legur <- tbl(db_connection,"sjukrahus_legur_nytt") %>%
  data.frame() %>%
  select(LeguID,studyID,Innritun,Utskrift) %>%
  rename(adm_id=LeguID,study_id=studyID,admission_date=Innritun,discharge_date=Utskrift)

#Codes for admissions, all types
legur_codes <- tbl(db_connection,"sjukrahus_legur_kodar_nytt") %>%
  data.frame() %>%
  select(LeguID,Kodakerfi,Leitarkodi,KodaRod) %>%
  rename(adm_id=LeguID,coding_system=Kodakerfi,code=Leitarkodi,code_order=KodaRod)

#Joining the two datasets
admission_data <- left_join(legur_codes,legur,by="adm_id") %>%
  select(-adm_id)%>%
  mutate(src="hospital_admission", #Source of the data for a combined dataset
         admission_date=ymd_hms(admission_date),
         discharge_date=ymd_hms(discharge_date) #fixing date variables from char to POSIX
         )

#Same for outpatient visits at hospitals
komur <- tbl(db_connection,"sjukrahus_komur") %>%
  data.frame() %>%
  select(studyid,KOMU_AUDKENNI,KOMU_DAGUR,HEITI_THJONUSTUFLOKKS) %>%
  rename(study_id=studyid,outp_id=KOMU_AUDKENNI,admission_date=KOMU_DAGUR,outp_service=HEITI_THJONUSTUFLOKKS) #New variable with the name of the specialty service provided in Icelandic.

komur_codes <- tbl(db_connection,"sjukrahus_komur_kodar") %>%
  data.frame() %>%
  select(KOMU_AUDKENNI,KODAKERFI,LEITARKODI,KODA_ROD) %>%
  rename(outp_id=KOMU_AUDKENNI, coding_system=KODAKERFI,code=LEITARKODI,code_order=KODA_ROD)

outp_data <- left_join(komur_codes,komur,by="outp_id") %>%
  select(-outp_id) %>%
  mutate(src="hospital_outpatient",
         admission_date=dmy(admission_date)) %>%
  filter(!is.na(study_id)) #there are <10 lines without study_id that are inherited from the original data. Excluded

#Now creating a similar dataset for primary care data

pc_visits <- tbl(db_connection,"primarycare_consults") %>%
  data.frame() %>%
  filter(staff_occupation =="Læ") %>% #Geymum bara kóða sem eru skráðir af læknum. Ath að Sl eru sjúkraliðar skv breytulista landlæknis
  select(studyid,consult_id,consult_date)%>%
  rename(study_id=studyid,admission_date=consult_date)

pc_codes <- tbl(db_connection,"primarycare_consult_codes") %>%
  data.frame() %>%
  select(consult_id,coding_system,code_clean,code_order) %>%
  rename(code=code_clean)

primarycare_dat <- left_join(pc_codes,pc_visits,by="consult_id") %>%
  select(-consult_id) %>%
  mutate(src="primary_care",
         admission_date=ymd(admission_date)) %>%
  filter(!is.na(study_id))

#Since we only keep codes by doctors and there are a few NA (<30) in the original data with regard to study_id we exclude all lines of codes without study id

#Now combining the datasets into a master dataset of all previous medical diagnoses

diagnosis_data <- bind_rows(admission_data,outp_data,primarycare_dat) %>%
  select(study_id,admission_date,discharge_date,coding_system, code,code_order,outp_service,src) #Ordering columns neatly
