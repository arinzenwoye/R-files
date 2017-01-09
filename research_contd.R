rm(list=ls())
library(plyr)
library(dplyr)
library(tidyr)
library(nnet)
library(mlogit)
research_dta <- read.csv("/Users/arinzenwoye/Desktop/Research Files/test12.csv", stringsAsFactors = FALSE, na.strings=c("","NA"))
#research_dta <- read.csv("/Users/arinzenwoye/Desktop/Research Files/test12.csv", na.strings=c("","NA"))
attach(research_dta)
research_dta$searchmethod1 <- mapvalues(research_dta$mthresource1, from = c("Did not use", "Extremely useful",
                                        "Not at all useful","Somewhat useful", "Very useful"),
                                        to = c(0, 4, 1, 2, 3))
test1 <- research_dta[,44:58]
searchvar<- c("career_job fairs", "employer websites", "employer reps on campus", "info_sessions",
              "recruitment_brochures_new", "ads in mags and publications", "social_network_new",
              "professional associations", "virtual fairs","career services", "faculty members", "friends_new", 
              "parents_relatives_new", "alumni_new", "articles in news")
for (i in 1:length(searchvar)){
   test1[,i] <- mapvalues(test1[,i], from = c("Did not use", "Extremely useful",
                            "Not at all useful","Somewhat useful", "Very useful"),
                            to = c(0, 4, 1, 2, 3))
   test1[,i] <- as.numeric(test1[,i])
}
colnames(test1) <- searchvar
research_dta <- cbind(research_dta, test1)
test2 <- cbind(research_dta[,2],test1)
colnames(test2)[1]<-c("school")
write.csv(test2, "/Users/arinzenwoye/Desktop/stata Files/test2.csv")


#Regression
regbinom <- glm(offer ~ cgpa_1 + internship + not_firstgen + sex + Age + ethnicity + duration + 
                major_stem2 + school_alt + resource1 + resource2 + resource3 + resource4 + resource5 +
                resource6 + resource7 + resource8 + resource9 + resource10 + resource11 + resource12 +
                resource13 + resource14 + resource15, family=binomial(link="logit"),data=research_dta)
summary(regbinom) #coefficients here are log odds
options(scipen = 100)
options(digits = 2)
exp(coef(regbinom)) #gives the odds
regbinom1 <- glm(offer ~ cgpa_1 + internship + not_firstgen + sex + Age + ethnicity + duration + 
                  major_stem2 + school_alt, family=binomial(link="logit"),data=research_dta) 
testdata <- data.frame(ethnicity=c("African-American","Asian-American", "Hispanic-American",
                                   "International", "Multiracial", "Native American", "White"), 
                       age1=mean(research_dta$Age),
                       internship1=mean(research_dta$internship), sex1=mean(research_dta$sex),
                       duration1=mean(research_dta$duration), 
                       major_stem3=mean(research_dta$majorstem2), 
                       school_alt1=mean(research_dta$school_alt), 
                       not_firstgen1=mean(research_dta$not_firstgen), 
                       cgpa_2=mean(research_dta$cgpa_1))
