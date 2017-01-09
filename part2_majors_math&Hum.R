rm(list=ls())
library(plyr)
library(dplyr)
library(reshape2)
library(tidyr)
math_dta <- read.csv("/Users/arinzenwoye/Desktop/work files/Math and Science Majors.csv", stringsAsFactors = FALSE, na.strings=c("","NA"))
Humsoc_dta <- read.csv("/Users/arinzenwoye/Desktop/work files/Humanities & Soc Science Majors.csv", stringsAsFactors = FALSE, na.strings=c("","NA"))
student_dta <- read.csv("/Users/arinzenwoye/Desktop/work files/Student_data2015.csv", stringsAsFactors = FALSE, na.strings=c("","NA"))
student_dta1 <- select(student_dta, student..Student.ID, Student.Profile..GPA)
math_dta <- math_dta[!is.na(math_dta$post_grad_status),] 
math_dta <- filter(math_dta, post_grad_status == "Employed full-time, full-time entrepreneur, full-time post-graduation internship, full-time fellowship, etc. (on average 30 hours or more per week)" | post_grad_status == "Seeking employment")
Humsoc_dta <- Humsoc_dta[!is.na(Humsoc_dta$post_grad_status),]
Humsoc_dta <- filter(Humsoc_dta, post_grad_status == "Employed full-time, full-time entrepreneur, full-time post-graduation internship, full-time fellowship, etc. (on average 30 hours or more per week)"| post_grad_status == "Seeking employment")
colnames(student_dta1)[1] <- "RUID"
#Remove duplicates by RUID
math_dta <- math_dta[!duplicated(math_dta$RUID),]
Humsoc_dta <- Humsoc_dta[!duplicated(Humsoc_dta$RUID),]
student_dta1 <- student_dta1[!duplicated(student_dta1$RUID),]
#participated in experiential learning
math_dta$experiential1 <- ifelse(math_dta$Experiential.Learning==1, "Yes", "No")
toMatch <- c("research", "internship")
math_dta$test1 = ifelse(grepl(paste(toMatch,collapse="|"), math_dta$experiences, ignore.case = TRUE), "Yes", "No")
Humsoc_dta$experiential1 <- ifelse(grepl(paste(toMatch,collapse="|"), Humsoc_dta$experiences, ignore.case = TRUE), "Yes", "No")
sum(is.na(math_dta$experiences))
math_dta <- math_dta[!is.na(math_dta$experiences),]
Humsoc_dta <- Humsoc_dta[!is.na(Humsoc_dta$experiences),]
#population
math_dta$popType <- rep("Math & Sciences", 292)
Humsoc_dta$popType <- rep("Humanities & Social Sciences", 568)
#new math and hum data
#math
math_new <- select(math_dta, RUID,salary_base, experiential1, popType, gender, ethnicity, post_grad_status)
student_dta1$RUID <- as.integer(student_dta1$RUID)
math_new <- left_join(math_new, student_dta1, by = "RUID")
sum(is.na(math_new$RUID))
sum(is.na(math_new$Student.Profile..GPA))
#math_new <- math_new[math_new$salary_base > 6000,]
#math_new <- math_new[!is.na(math_new$RUID),]
#Humanities
Humsoc_new <- select(Humsoc_dta, RUID,salary_base, experiential1, popType, gender, ethnicity, post_grad_status)
Humsoc_new <- left_join(Humsoc_new, student_dta1, by = "RUID")
sum(is.na(Humsoc_new$RUID))
sum(is.na(Humsoc_new$Student.Profile..GPA))
#Humsoc_new <- Humsoc_new[Humsoc_new$salary_base < 200000 & Humsoc_new$salary_base >= 12000,]
#combine math and humanities
Humsoc_new$salary_base <- as.numeric(Humsoc_new$salary_base)
full_majors <- union(math_new, Humsoc_new)
write.csv(full_majors, "/Users/arinzenwoye/Desktop/stata files/full_majors.csv")

fulltime = filter(full_majors, post_grad_status == "Employed full-time, full-time entrepreneur, full-time post-graduation internship, full-time fellowship, etc. (on average 30 hours or more per week)")
