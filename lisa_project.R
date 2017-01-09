rm(list=ls())
library(plyr)
library(dplyr)
library(reshape)
library(reshape2)
library(tidyr)
#project <- read.csv("/Users/arinzenwoye/Desktop/Lsa_data.csv", stringsAsFactors = FALSE, na.strings=c("","NA"))
project <- read.csv("/Users/arinzenwoye/Desktop/employers_data1.csv", stringsAsFactors = FALSE, na.strings=c("","NA"))
project$OCR.Schedule.Data..Date <- as.Date(project$OCR.Schedule.Data..Date)
project<- select(project, Employer..Organization.Name, Job..Job.Title, Contact..Full.Name, OCR.Schedule.Data..Date, Notes..Body)
project1 <- filter(project, Employer..Organization.Name %in% c("Bank of America", "Bank of America Merrill Lynch", 
                                            "Barclays Capital", "The Bank of NY Mellon", "Citi", "Credit Suisse", 
                                            "Deutsche Bank", "Goldman Sachs", "JPMorgan Chase & Co.", "Morgan Stanley", 
                                            "Nomura", "PNC", "TD Bank", "Investors Bank", 
                                            "Société Générale Corporate & Investment Bank", "M&T Bank Corporation"))
project1$Employer..Organization.Name[project1$Employer..Organization.Name == "Bank of America Merrill Lynch"] <- "Bank of America"
project1$Fall_2014 <- NA
project1$Spring_2015 <- NA
project1$Fall_2015 <- NA
project1$Spring_2016 <- NA
project1$Fall_2016 <- NA
fall <- c("09", "10", "11", "12")
spring <- c("01", "02", "03", "04", "05")
for(i in 1:length(project1[,"OCR.Schedule.Data..Date"])){
  if(format(project1["OCR.Schedule.Data..Date"][i,], '%Y') == "2014" & format(project1["OCR.Schedule.Data..Date"][i,], '%m') %in% c(fall)){
    project1["Fall_2014"][i,] <- as.character(project1["OCR.Schedule.Data..Date"][i,])
  } 
  else if(format(project1["OCR.Schedule.Data..Date"][i,], '%Y') == "2015" & format(project1["OCR.Schedule.Data..Date"][i,], '%m') %in% c(spring)){
    project1["Spring_2015"][i,] <- as.character(project1["OCR.Schedule.Data..Date"][i,])
  }
  else if(format(project1["OCR.Schedule.Data..Date"][i,], '%Y') == "2015" & format(project1["OCR.Schedule.Data..Date"][i,], '%m') %in% c(fall)){
    project1["Fall_2015"][i,] <- as.character(project1["OCR.Schedule.Data..Date"][i,])
  }
  else if(format(project1["OCR.Schedule.Data..Date"][i,], '%Y') == "2016" & format(project1["OCR.Schedule.Data..Date"][i,], '%m') %in% c(spring)){
    project1["Spring_2016"][i,] <- as.character(project1["OCR.Schedule.Data..Date"][i,])
  }
  else if(format(project1["OCR.Schedule.Data..Date"][i,], '%Y') == "2016" & format(project1["OCR.Schedule.Data..Date"][i,], '%m') %in% c(fall)){
    project1["Fall_2016"][i,] <- as.character(project1["OCR.Schedule.Data..Date"][i,])
  }
}
project1 <- arrange(project1, Employer..Organization.Name)
project1 <- select(project1, Employer..Organization.Name, Job..Job.Title, Contact..Full.Name, Fall_2014, Spring_2015, Fall_2015,
                   Spring_2016, Fall_2016, Notes..Body, OCR.Schedule.Data..Date)
project1 <- project1[!duplicated(project1[c("Employer..Organization.Name", "Contact..Full.Name", "OCR.Schedule.Data..Date")]),]
project1 <- select(project1, Employer..Organization.Name, Job..Job.Title, Contact..Full.Name, Fall_2014, Spring_2015, Fall_2015,
                   Spring_2016, Fall_2016, Notes..Body)
write.csv(project1, "/Users/arinzenwoye/Desktop/Lisa_project1.csv", na = "")










