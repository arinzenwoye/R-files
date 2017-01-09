rm(list=ls())
library(plyr)
library(dplyr)
library(reshape)
library(reshape2)
library(tidyr)
project <- read.csv("/Users/arinzenwoye/Desktop/CKStudentDataFA2016.csv", stringsAsFactors = FALSE, na.strings=c("","NA"))
sch_code <- read.csv("/Users/arinzenwoye/Desktop/schCode.csv", stringsAsFactors = FALSE, na.strings=c("","NA"))
student_state <- read.csv("/Users/arinzenwoye/Desktop/Student.State.csv", stringsAsFactors = FALSE, na.strings=c("","NA"))

#School of Arts and Science
sas_data <- filter(project, project[,28]==1)
nrow(sas_data)/nrow(project) # percent of sas population
table(sas_data[,5]) #gender
table(sas_data[,5])[2]/nrow(sas_data)
table(sas_data[,5])[3]/nrow(sas_data)
sas_data_US <- filter(sas_data, sas_data[,20]=="U.S. Citizen") #sas us_citizen
table(sas_data_US[,12])

major_dta <- as.data.frame(table(sas_data[,19]))
colnames(sch_code)[1] <- c("Student.Account..School") 
sch_code$Student.Account..School <- as.character(sch_code$Student.Account..School)
project <- left_join(project, sch_code, by="Student.Account..School") #newbrunswick campus
nb_data <- filter(project, project[,33]=="NB") #newbrunswick data
major_dta1 <- as.data.frame(table(nb_data[,19]))
#write.csv(major_dta1, "/Users/arinzenwoye/Desktop/nb_majors.csv")

major_dta3 <- as.data.frame(table(project[,19]))
#write.csv(major_dta3, "/Users/arinzenwoye/Desktop/rutgers_majors.csv")
table(project[,20])
project <- left_join(project, student_state, by="student..Student.ID")
athlete_data <- as.data.frame(table(project[,36]))

top_majors_sas <- arrange(major_dta, desc(Freq)) 
top_majors_sas <- top_majors_sas[1:20,]

#black data
black_sas_dta <- filter(sas_data, sas_data[,12]=="Black Non-Hispanic")
black_sas_maj <- as.data.frame(table(black_sas_dta[,19])) 
black_sas_maj <-  filter(black_sas_maj, Var1 %in% as.character(top_majors_sas[,1]))
black_sas_maj <- left_join(top_majors_sas, black_sas_maj, by="Var1")
write.csv(black_sas_maj, "/Users/arinzenwoye/Desktop/black_sas_majors.csv")
black_dta <- filter(project, project[,12]=="Black Non-Hispanic")
black_maj <- as.data.frame(table(black_dta[,19])) %>% 
  arrange(desc(Freq))
write.csv(black_maj, "/Users/arinzenwoye/Desktop/black_majors.csv")
black_state <- as.data.frame(table(black_dta[,37]))              
black_ath <- as.data.frame(table(black_dta[,36]))                   
black_vet <- as.data.frame(table(black_dta[,35]))                   
                   
#Asian Data
asn_sas_dta <- filter(sas_data, sas_data[,12]=="Asian")
asn_sas_maj <- as.data.frame(table(asn_sas_dta[,19]))
asn_sas_maj <-  filter(asn_sas_maj, Var1 %in% as.character(top_majors_sas[,1]))
asn_sas_maj <- left_join(top_majors_sas, asn_sas_maj, by="Var1")
write.csv(asn_sas_maj, "/Users/arinzenwoye/Desktop/asian_sas_majors.csv")
asn_dta <- filter(project, project[,12]=="Asian")
asn_maj <- as.data.frame(table(asn_dta[,19])) %>% 
  arrange(desc(Freq))                   
write.csv(asn_maj, "/Users/arinzenwoye/Desktop/asian_majors.csv")  
asn_state <- as.data.frame(table(asn_dta[,37])) 
asn_ath <- as.data.frame(table(asn_dta[,36])) 
asn_vet <- as.data.frame(table(asn_dta[,35]))                   
  
#Hispanic data
his_sas_dta <- filter(sas_data, sas_data[,12]=="Hispanic Non-Puerto-Rican" | sas_data[,12] == "Puerto Rican")
his_sas_maj <- as.data.frame(table(his_sas_dta[,19]))
his_sas_maj <-  filter(his_sas_maj, Var1 %in% as.character(top_majors_sas[,1]))
his_sas_maj <- left_join(top_majors_sas, his_sas_maj, by="Var1")
write.csv(his_sas_maj, "/Users/arinzenwoye/Desktop/his_sas_majors.csv")
his_dta <- filter(project, project[,12]=="Hispanic Non-Puerto-Rican" | project[,12] == "Puerto Rican")
his_maj <- as.data.frame(table(his_dta[,19])) %>% 
  arrange(desc(Freq))                   
write.csv(his_maj, "/Users/arinzenwoye/Desktop/his_majors.csv")  
his_state <- as.data.frame(table(his_dta[,37])) 
his_ath <- as.data.frame(table(his_dta[,36])) 
his_vet <- as.data.frame(table(his_dta[,35]))  

#international students
intl_sas_dta <- filter(sas_data, sas_data[,29]=="F1")
intl_sas_maj <- as.data.frame(table(intl_sas_dta[,19]))
intl_sas_maj <-  filter(intl_sas_maj, Var1 %in% as.character(top_majors_sas[,1]))
intl_sas_maj <- left_join(top_majors_sas, intl_sas_maj, by="Var1")
write.csv(intl_sas_maj, "/Users/arinzenwoye/Desktop/intl_sas_majors.csv")
intl_dta <- filter(project, project[,29]=="F1")
intl_maj <- as.data.frame(table(intl_dta[,19])) %>% 
  arrange(desc(Freq))                   
write.csv(intl_maj, "/Users/arinzenwoye/Desktop/intl_majors.csv")  

#white students
white_sas_dta <- filter(sas_data, sas_data[,12]=="White Non-Hispanic")
white_sas_maj <- as.data.frame(table(white_sas_dta[,19]))
white_sas_maj <-  filter(white_sas_maj, Var1 %in% as.character(top_majors_sas[,1]))
white_sas_maj <- left_join(top_majors_sas, white_sas_maj, by="Var1")
write.csv(white_sas_maj, "/Users/arinzenwoye/Desktop/white_sas_majors.csv")
white_dta <- filter(project, project[,12]=="White Non-Hispanic")
white_maj <- as.data.frame(table(white_dta[,19])) %>% 
  arrange(desc(Freq))                   
write.csv(white_maj, "/Users/arinzenwoye/Desktop/white_majors.csv")  
white_state <- as.data.frame(table(white_dta[,37])) 
white_ath <- as.data.frame(table(white_dta[,36]))                    
white_vet <- as.data.frame(table(white_dta[,35]))     

#female students
fem_sas_dta <- filter(sas_data, sas_data[,5]=="F")
fem_sas_maj <- as.data.frame(table(fem_sas_dta[,19]))
fem_sas_maj <-  filter(fem_sas_maj, Var1 %in% as.character(top_majors_sas[,1]))
fem_sas_maj <- left_join(top_majors_sas, fem_sas_maj, by="Var1")
write.csv(fem_sas_maj, "/Users/arinzenwoye/Desktop/fem_sas_majors.csv")
fem_dta <- filter(project, project[,5] == "F")
fem_maj <- as.data.frame(table(fem_dta[,19])) %>% 
  arrange(desc(Freq))                   
write.csv(fem_maj, "/Users/arinzenwoye/Desktop/fem_majors.csv")  
fem_state <- as.data.frame(table(fem_dta[,37])) 
fem_ath <- as.data.frame(table(fem_dta[,36]))                    
fem_vet <- as.data.frame(table(fem_dta[,35]))  

                 
                  

