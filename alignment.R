rm(list=ls())
library(plyr)
library(dplyr)
library(reshape2)
library(tidyr)
oct_dta <- read.csv("/Users/arinzenwoye/Desktop/Oct.Jan.Dataset.csv", stringsAsFactors = FALSE, na.strings=c(""))
may_dta <- read.csv("/Users/arinzenwoye/Desktop/MayFollowup.csv", stringsAsFactors = FALSE, na.strings=c(""), fileEncoding="latin1")
align <-  read.csv("/Users/arinzenwoye/Desktop/ColumnAlignment.csv", stringsAsFactors = FALSE, na.strings=c(""), fileEncoding="latin1")

#May Followup
may_dta$ID <- NA
may_dta$RegStatus <- NA
may_dta$EthnicityCode <- NA
may_dta$GradMonth <- NA
may_dta$GradYear <- NA
may_dta$CURRIC_DESCR1 <- NA
may_dta$CURRIC_DESCR2 <- NA
may_dta$Matriculation <- NA
may_dta$OriginYearAdmit <- NA
may_dta$OriginTermAdmit <- NA
may_dta$TransfertypeCode <- NA
may_dta$Non_trad_student <- NA
may_dta$ADD1 <- NA
may_dta$Work <- NA
may_dta1 <- select(may_dta, c(1:11), ID, RegStatus, student..Gender, EthnicityCode, student..Ethnicity, GradMonth, 
                   GradYear, School_ID, Student.Profile..School, UG_CUR1, CURRIC_DESCR1, UG_CUR2, CURRIC_DESCR2,
                   Matriculation, Student.Profile..GPA, Student.Profile..Degree.Level, OriginYearAdmit, OriginTermAdmit,
                   TransfertypeCode, Student.Profile..Transfer.Indicator, Non_trad_student, ADD1, student..Birth.Date,
                   student..Logins, student..NetID, Work, Student.Profile..Visa.Status, ADD3, ADD2, UG_DEG.MTH, ADD4,
                   ADD5, c(34:390))
may_dta1$EthnicityCode <- may_dta1$student..Ethnicity
may_dta1$EthnicityCode <- mapvalues(may_dta1$EthnicityCode, from = c("Asian", "Hispanic Non-Puerto-Rican", 
                                    "Puerto Rican", "White Non-Hispanic"), to = c(8, 4, 5, 6))
may_dta1$student..Ethnicity <- mapvalues(may_dta1$student..Ethnicity, from = c("Asian", "Hispanic Non-Puerto-Rican", 
                                        "Puerto Rican", "White Non-Hispanic"), to = c("Asian only", "Hispanic, Non-Puerto-Rican",
                                        "Puerto Rican", "White, Non-Hispanic"))
may_dta1$Student.Profile..School <- mapvalues(may_dta1$Student.Profile..School, from = c(1, 11, 14, 16, 17, 19, 38), 
                                              to = c("School of Arts and Sciences", "School of Environmental and Biological Sciences",
                                                     "School of Engineering", "Graduate School of new Brunswick", 
                                                     "School of Communication and Information", "School of Social Work",
                                                     "School of Management and Labor Relations"))
#write.csv(may_dta1, "/Users/arinzenwoye/Desktop/cleaned_MayFollowup.csv")

#Oct.Jan Dataset
oct_dta$EthnicityCode <- NA
oct_dta$Grad_Month <- NA
oct_dta$Grad_Year <- NA
oct_dta$School <- NA
oct_dta$CURRIC_CD1 <- NA
oct_dta$CURRIC_DESCR1 <- NA
oct_dta$CURRIC_CD2 <- NA
oct_dta$CURRIC_DESCR2 <- NA
oct_dta$CUM_GPA_UG_TO_DATESUM <- NA
oct_dta$OrignYearAdmit <- NA
oct_dta$OriginTermAdmit <- NA
oct_dta$TransferTypecode <- NA
oct_dta$Transtype <- NA
oct_dta$Non_trad_student_ind <- NA
oct_dta$ADD1 <- NA 
oct_dta$BirthDay <- NA
oct_dta$NumLogins <- NA 
oct_dta$Work <- NA
oct_dta$ADD3 <- NA 
oct_dta$ADD2 <- NA
oct_dta$DegCode <- NA
oct_dta$ADD4 <- NA
oct_dta$ADD5 <- NA
oct_dta$Please_select_the_year_and_semester_of_admittance_to_Rutgers_University_for_your_current_degree <- NA
oct_dta$Year_of_admittance_Year <- NA
oct_dta$Semester_of_admittance_Semester <- NA
oct_dta$did_you_participate_student_access <- NA
oct_dta$Rutgers.University.Post.Graduation.Survey.1 <- NA 
oct_dta$Rutgers.University.Post.Graduation.Survey.2 <- NA 
oct_dta1 <- select(oct_dta, c(1:12), RegStatus, Gender, EthnicityCode, Ethnicity, Grad_Month, Grad_Year, School_ID,
                   School, CURRIC_CD1, CURRIC_DESCR1, CURRIC_CD2, CURRIC_DESCR2, Matriculation, CUM_GPA_UG_TO_DATESUM, DegreeType,
                   OrignYearAdmit, OriginTermAdmit, TransferTypecode, Transtype, Non_trad_student_ind, ADD1,
                   BirthDay, NumLogins, NETID, Work, Visa, ADD3, ADD2, DegCode, ADD4, ADD5, c(32:41), 
                   Please_select_the_year_and_semester_of_admittance_to_Rutgers_University_for_your_current_degree,
                   Year_of_admittance_Year, Semester_of_admittance_Semester, c(42:140), Rutgers.University.Post.Graduation.Survey.2,  
                   Approximately.how.many.job.applications.have.you.submitted.within.the.12.months.before.graduation.,
                   Rutgers.University.Post.Graduation.Survey.1, 
                   Approximately.how.many.job.interviews.have.you.attended.within.the.12.months.before.graduation., c(141:155), 
                   did_you_participate_student_access, c(156:232))

oct_dta1$EthnicityCode <- oct_dta1$Ethnicity
oct_dta1$EthnicityCode <- mapvalues(oct_dta1$EthnicityCode, from = c("Asian", "White", "Hispanic", "Black", "Puerto Rican",
                                    "Multiple", "Hawaiian/PI", "Unknown", "No Response"), to = c(8, 6, 4, 3, 5, "T", 9, "U", "N"))
oct_dta1$Ethnicity <- mapvalues(oct_dta1$Ethnicity, from = c("Asian", "White", "Hispanic", "Black", "Puerto Rican", "Multiple",
                                "Hawaiian/PI", "Unknown", "No Response"), to = c("Asian only", "White, Non-Hispanic", 
                                "Hispanic, Non-Puerto-Rican", "Black, Non-Hispanic", "Puerto Rican", "More than one race",
                                "Native Hawaiian or Pacific Islander/Non-Asian", "Unknown", "No Response"))
oct_dta1$School <- oct_dta1$School_ID
oct_dta1$School <- mapvalues(oct_dta1$School, from = c(1, 7, 11, 14, 16, 17, 19, 33, 37, 61), to = c("School of Arts and Sciences",
                              "Mason Gross School of The Arts", "School of Environmental and Biological Sciences",
                              "School of Engineering", "Graduate School of new Brunswick", "School of Communication and Information",
                              "School of Social Work", "Rutgers Business School-newark/new Brunswick - New Brunswick Campus",
                              "School of Management and Labor Relations", "University College - New Brunswick"))
#write.csv(oct_dta1, "/Users/arinzenwoye/Desktop/cleaned_Oct.Jan.Dataset.csv")









