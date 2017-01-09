rm(list=ls())
library(plyr)
library(dplyr)
library(tidyr)
alluniv <- read.csv("/Users/arinzenwoye/Desktop/Research Files/alluniversity_data.csv", stringsAsFactors = FALSE, na.strings=c("","NA"))
skool <- read.csv("/Users/arinzenwoye/Desktop/Research Files/skooldta.csv", stringsAsFactors = FALSE, na.strings=c("","NA"))
skool$school[skool$school == "Marian University"] <- "Marian University-Indianapolis"
skool$school[skool$school == "The Richard Stockton College of New Jersey"] <- "Stockton University"
skool$school[skool$school == "Buffalo State SUNY"] <- "SUNY Buffalo State"
skool$school[skool$school == "Westminster College"] <- "Westminster College-Fulton"
skool$school[skool$school == "Mount St. Joseph University"] <- "Mount Saint Joseph University"
skool$school[skool$school == "Wheaton College"] <- "Wheaton College-Wheaton"
skool$school[skool$school == "University of St Thomas"] <- "St Thomas University"
skool$school[skool$school == "University of Phoenix-Madison Campus"] <- "University of Phoenix-Florida"
univ1 <- select(alluniv, NAME, BASIC2005)
colnames(univ1)[1] <- "school"
skool1 <- left_join(skool, univ1, by = "school")
sum(is.na(skool1$BASIC2005))
sum(is.na(skool$school))
#write.csv(skool1, "/Users/arinzenwoye/Desktop/skool2.csv")
skool1 <- skool1[!is.na(skool1$school),] #remove missing values in school
skool_code <- data.frame(table(skool1$BASIC2005))
skool1$classSchool <- skool1$BASIC2005
skool1$classSchool <- mapvalues(skool1$classSchool, from = c(-3, 2, 3, 4, 9, 12, 15:23, 29:31),
                                to = c("not classified", ))