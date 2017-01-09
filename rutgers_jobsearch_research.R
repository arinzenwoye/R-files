rm(list=ls())
library(plyr)
library(dplyr)
library(reshape)
library(reshape2)
library(tidyr)
library(Hmisc)
library(pastecs)
library(ggplot2)
library(nnet)
library(mlogit)
library(gdata)
project <- read.csv("/Users/arinzenwoye/Desktop/Research Files/plswrk3.csv", stringsAsFactors = FALSE, na.strings=c("","NA"))
project$fva <- project[,16] - project[,20]
#project <- read.csv("/Users/arinzenwoye/Desktop/plswrk1.csv", stringsAsFactors = FALSE, na.strings=c("","NA"))
project <- project[c(-1,-2),]
project <- select(project, c(4,12,15,17,21,27,35,46,53,54,65,89:94,96:100,
                             102:107,109:116,154,156,158,162:176,191:195,23,177:180,22,2,26,138,38))
colnames(project) <- c("name", "ruid", "gender", "ethnicity", "school",
                       "gpa","birthday","postgrad_status", "first_choice", 
                       "first_gen", "start_gross_salary", 
                       "jobsearch_skills_1","jobsearch_skills_2","jobsearch_skills_3",
                       "jobsearch_skills_4", "jobsearch_skills_5", "jobsearch_skills_6",
                       "jobsearch_skills_7", "jobsearch_skills_8", "jobsearch_skills_9",
                       "jobsearch_skills_10", "jobsearch_skills_11", "jobsearch_skills_12",
                       "jobsearch_skills_13", "jobsearch_skills_14", "jobsearch_skills_15",
                       "jobsearch_skills_16", "jobsearch_skills_17",
                       "jobsearch_method_print", "jobsearch_method_media", 
                       "jobsearch_method_network", "jobsearch_method_ucs", 
                       "jobsearch_method_website", "jobsearch_method_otherpvt",
                       "jobsearch_method_otherpub", "jobsearch_method_other",
                       "no_joffers", "start_salary", "no_japps", "internship/co_op",
                       "extracur_1", "extracur_2", "extracur_3", "extracur_4", "extracur_5",
                       "extracur_6", "extracur_7", "extracur_8", "extracur_9", "extracur_10",
                       "extracur_11", "extracur_12", "extracur_13", "extracur_14", 
                       "internship_summer", "internship_fall", "internship_spring", 
                       "internship_no", "internship_over1yr","Major", "internships_freshman",
                       "internships_sophomore", "internships_junior", "internships_senior", "Major_code","Response_ID",
                       "Matriculation", "desc_position", "Work")
#creating age variable
project$birthday <- as.Date(project$birthday, "%m/%d/%Y")
birth_year <- as.numeric(format(project$birthday,'%Y')) 
project <- mutate(project, age = 2016 - birth_year)
#recoding first_choice, first_gen and jobsearch_skills
cart_list <- c(9, 10, 12:36)
for (i in cart_list){
  project[,i] <- as.numeric(revalue(project[,i], c("Yes"="1", "No"="0")))
}
#recoding extracurricular activities
cart_list1 <- c(40:59)
for(j in cart_list1){
  project[,j] <- ifelse(project[,j]=="",0,1)
}
#recoding internship/co-op
project[,40][is.na(project[,40])] <- 0
#aggregating jobsearch_skills & extracurr activities
#project <- mutate(project, basic_skills_agg = apply(project[,c(12:17,23:28)],1,sum, na.rm = TRUE),
#                  active = apply(project[,41:54],1,sum, na.rm = TRUE))
project <- mutate(project, resume_skills = apply(project[,c(12:17)],1,sum, na.rm = TRUE), netwrk_skills = apply(project[,c(23:28)], 1,sum, na.rm = TRUE),
                                   active = apply(project[,41:54],1,sum, na.rm = TRUE))
project$int_dummy <- NA 
for(i in 1:length(project[,18])){
  if(is.na(project[,18][i])==TRUE & is.na(project[,19][i])==TRUE & is.na(project[,20][i])==TRUE & is.na(project[,22][i])==TRUE){
    project[,74][i] <-  0
  } else {
    project[,74][i] <- 1
  }
}
project <- mutate(project, int_skills = apply(project[,c(18:22)], 1, sum, na.rm = TRUE))
#renaming ethnicity
project[,4] <- mapvalues(project[,4], from = c("More than one race", 
                "Native Hawaiian or Pacific Islander/Non-Asian", "Unknown", 
                "American Indian or Alaskan Native","Hispanic, Non-Puerto-Rican",
                "Puerto Rican", "No Response"),
                to = c(rep("other",4), rep("Hispanic/Puerto-Rican ", 2), NA))
#renaming majors
project$Major[project$Major == "Business and Science"] <- "Business And Science"
project$Major[project$Major == "Cell Biology and Neuroscience"] <- "Cell Biology And Neuroscience"
project$Major[project$Major == "Exercise Science and Sport Studies"] <- "Exercise Science And Sport Studies"
project$Major[project$Major == "Information Technology and Informatics"] <- "Information Technology And Informatics"
project$Major[project$Major == "Labor and Employment Relations"] <- "Labor And Employment Relations"
project$Major[project$Major == "Labor Studies and Employment Relations"] <- "Labor Studies And Employment Relations"
project$Major[project$Major == "Materials Science and Engineering"] <- "Materials Science And Engineering"
project$Major[project$Major == "Journalism and Media Studies"] <- "Journalism And Media Studies"
# #creating categorical variables
# f_list <- c(3,4,5,39,60)
# for(i in f_list){
#   project[,i] <- factor(project[,i], exclude = NA)
#
#changing variables to numeric and outliers missing
for (i in c(6,61:64)){
  project[,i] <- as.numeric(project[,i])
  project[,i][project[,i]>4] <- NA
}
project <- mutate(project, total_internships = apply(project[,61:64],1,sum, na.rm = TRUE))
#create internship_related
project <- mutate(project, intership_related = apply(project[,c(55:57,59)], 1, sum, na.rm = TRUE))
project$intership_related <- ifelse(project$intership_related == 0, 0, 1) # 1 if related_internship, 0 if not
#creating broad_cart
broad_cart <- read.csv("/Users/arinzenwoye/Desktop/Research Files/broad_cart.csv", stringsAsFactors = FALSE)
project_1 <- merge(project, broad_cart, by = "Major", all = TRUE)
project_2 <- project_1[!is.na(project_1$Response_ID),] 
#model 
attach(project_2)
model <- project_2 %>% 
  filter(project_2$postgrad_status %in% 
  c("Employed full-time, full-time entrepreneur, full-time post-graduation internship, full-time fellowship, etc. (on average 30 hours or more per week)",
    "Employed part-time, part-time entrepreneur, part-time post-graduation internship, part-time fellowship, etc. (on average less than 30 hours per week)",
    "Seeking employment") & Matriculation == "Undergraduate") %>% 
  select(name, ruid, postgrad_status, gender, ethnicity, gpa, first_gen, active, `internship/co_op`, total_internships, intership_related,
         Broad_Category, resume_skills, netwrk_skills, int_dummy, int_skills, no_japps, jobsearch_method_ucs, jobsearch_method_media, 
         jobsearch_method_media, jobsearch_method_print, jobsearch_method_network, jobsearch_method_website, jobsearch_skills_1:jobsearch_skills_17,
         jobsearch_method_otherpub, jobsearch_method_otherpvt, jobsearch_method_other, Major, age, Matriculation, desc_position, Work) 
detach(project_2)
attach(model)
#sum of missing values
blanks = is.na(model)
missing_bycol = as.data.frame(apply(blanks,2,sum))
model <- mutate(model, missing_byrow = apply(blanks,1,sum))
##check: sapply(airquality, function(x) sum(is.na(x)))

### Descriptive Statistics

#Basic Statistics
options(scipen = 100)
options(digits = 2)
sum_stas <- stat.desc(model)
#cartegory frequency & modification
b_freq = as.data.frame(sort(table(model$Broad_Category), decreasing = TRUE))
model$Broad_Category <- mapvalues(model$Broad_Category, from = c("Architecture and Related Services",
                                  "Philosophy and Religious Studies", 
                                  "Public Administration and Social Service Professions"), 
                                  to = c(rep("Other", 3)))
write.csv(model, "/Users/arinzenwoye/Desktop/new_model4.csv")

#########################################





#table of position description by status 
pos_status <- data.frame(table(model$desc_position, model$postgrad_status))
pos_status <- spread(pos_status, Var2, Freq)
#creating dependent variable
model$code_position <- NA
model$code_position <- model$desc_position 
model$code_position <- ifelse (model$code_position == "A college degree is NOT required for this job, BUT the degree helped me to get it." |
                                   model$code_position == "Having a college degree made no difference in getting this job.", 1, 0) #1(underemployed), 0(fullyemployed)
model$dep_var <- NA
model$dep_var[model$code_position == 0 & model$postgrad_status == "Employed full-time, full-time entrepreneur, full-time post-graduation internship, full-time fellowship, etc. (on average 30 hours or more per week)"] <- "Full-Employed"
model$dep_var[model$code_position == 1 | model$postgrad_status == "Employed part-time, part-time entrepreneur, part-time post-graduation internship, part-time fellowship, etc. (on average less than 30 hours per week)"] <- "Underemployed"
model$dep_var[model$postgrad_status == "Seeking employment"] <- "Unemployed"
#creating intensity
model <- mutate(model, intensity = apply(model[,15:22], 1, sum))  
freq_searchmth <- data.frame(apply(model[,15:22], 2, sum))
write.csv(model, "/Users/arinzenwoye/Desktop/model_data1.csv")
#crosstables
attach(model) 

search_mth = c("jobsearch_method_media", "jobsearch_method_network", "jobsearch_method_other",
  "jobsearch_method_otherpub", "jobsearch_method_otherpvt", "jobsearch_method_print",
  "jobsearch_method_ucs", "jobsearch_method_website") 

#start of cross_tables function
cross_tables <- function(data = model, xtric, vars, cast = 0){  
  cast_xtric  = xtric
  if (cast != 0){
    freq <-data.frame(rowSums(xtric[,2:length(xtric)], na.rm = TRUE))
    print(freq)
  }
  j = 1
  if(cast == 0){
    melt_xtric = melt(data, id = xtric, measure = vars)
    print(head(melt_xtric))
    melt_xtric <- filter(melt_xtric, value == 1)
    cast_xtric = dcast(melt_xtric, paste(xtric,"~variable"))
    cast_xtric = cast_xtric[!is.na(cast_xtric[,1]),]
    print(cast_xtric)
    freq = data.frame(table(data[xtric]))
    print(freq)
    j = 2
  }
  new_table = c()  
  print(freq[,j])  
  for (i in 2:length(cast_xtric)){
    new_table = cbind(new_table, cast_xtric[,i]/freq[,j])
  }
  return(new_table)
}   

#table dep_var
tab_dep_var <- data.frame(table(model$dep_var))
#crosstables of broad category on postgraduate status 
broadByStatus = data.frame(table(model$Broad_Category, model$postgrad_status))
broad_status = spread(broadByStatus, Var2, Freq)
prop_broad_status <- cross_tables(data = model, broad_status, colnames(broad_status), cast = 1) #broad_status by proportion

#Transformation of variables to factors
# attach(model)
# f_list <- c(3,4,5,7,9,11,12,14:23,29)
# for(i in f_list){
#   model[,i] <- factor(model[,i], exclude = NA)
# }
# model$dep_var <- factor(model$dep_var, levels = c("Unemployed", "Underemployed", "Full-Employed"))
# model$gender <- factor(gender, levels = c("Female", "Male"))

### Regression using 
# model$dep_var <- relevel(model$dep_var, ref = "Unemployed")
# test <- multinom(dep_var ~ gender + ethnicity + gpa + first_gen + `internship/co_op`
#                  + jobsearch_skills_agg, data = model)
# summary(test)
# z <- summary(test)$coefficients/summary(test)$standard.errors
# p <- (1 - pnorm(abs(z), 0, 1))*2

#### Regression using Mlogit Package
model <- mutate(model, intern_var = model[,9])
attach(model)
tdta <- mlogit.data(model, shape = "wide", choice = "dep_var")
emp <- mlogit(dep_var ~ 0 | gender + ethnicity + gpa + jobsearch_skills_agg + first_gen +
                intern_var + total_internships + Broad_Category + intensity +
                age + active + jobsearch_method_media + jobsearch_method_network + jobsearch_method_ucs +
                jobsearch_method_website + jobsearch_method_print,
                reflevel = "Unemployed", heterosc = TRUE, data = tdta)
summary(emp)
B <- coef(emp)
z <- data.frame(with(tdta, apply(tdta, 2, mean, na.rm = TRUE)))
apply(fitted(emp, outcome=FALSE), 2, mean)

#binomial model
model$dep_var_1 <- ifelse(model$postgrad_status == "Seeking employment", 0, 1)
reg_binom <- glm(dep_var ~ gender + ethnicity + gpa + jobsearch_skills_agg + first_gen +
                   intern_var + total_internships + Broad_Category + intensity +
                   age + active + jobsearch_method_media + jobsearch_method_network + jobsearch_method_ucs +
                   jobsearch_method_website + jobsearch_method_print)

#Regression model with missingByRow function
reg_model <- model[,c(4:13,15:22,24,29,31)]
missingByRow <- function(data){ 
  blanks = is.na(data)
  data = mutate(data, missing_byrow1 = apply(blanks, 1, sum))
  return(data)
} 
test_missing <- missingByRow(reg_model)

###UCS Model
#workshop Data
workshop_data <- read.csv("/Users/arinzenwoye/Desktop/Research Files/Workshops_Sep 2013 - May 2016.csv", 
                                     stringsAsFactors = FALSE, na.strings=c("","NA","-"))
workshop_data_1 <- workshop_data[!duplicated(workshop_data[c("student..Student.ID", 
                                "Workshop..Symplicity.Workshop.ID", "student..Logins")]),]
student_wrkshop <- data.frame(table(workshop_data_1$student..Student.ID))
colnames(student_wrkshop) <- c("ruid", "workshops")
ucs_model <- merge(model, student_wrkshop, by = c("ruid"), all = TRUE) %>% 
  filter(!is.na(postgrad_status) & jobsearch_method_ucs == 1) 
blank_ucs = is.na(ucs_model$workshops) 
table(blank_ucs) 
#ck_logins
logins_data <- read.csv("/Users/arinzenwoye/Desktop/Research Files/CK_Logins.csv", 
                       stringsAsFactors = FALSE, na.strings=c("","NA","-"))
logins_data_1 <- logins_data[!duplicated(logins_data[c("student..Student.ID")]),]
student_login <- logins_data_1[,c(1,2)]
colnames(student_login) <- c("ruid", "logins")
student_login <- student_login[!is.na(student_login$ruid),]
ucs_model <- merge(ucs_model, student_login, by = c("ruid"), all = TRUE) %>% 
  filter(!is.na(postgrad_status) & jobsearch_method_ucs == 1) 
blank_login <- is.na(ucs_model$logins)
table(blank_login)
#counselling data
cousn_data <- read.csv("/Users/arinzenwoye/Desktop/Research Files/cousn_data.csv", 
                        stringsAsFactors = FALSE, na.strings=c("","NA","-"))
cousn_data_1 <- cousn_data[!duplicated(cousn_data[c("student..Student.ID", "Career.Counseling..Career.Counseling.ID")]),]
student_cousn <- cousn_data_1[,c(1,6)]
student_cousn1 <- data.frame(table(student_cousn$student..Student.ID))
colnames(student_cousn1) <- c("ruid", "sessions")
ucs_model <- merge(ucs_model, student_cousn1, by = c("ruid"), all = TRUE) %>% 
  filter(!is.na(postgrad_status) & jobsearch_method_ucs == 1) 
blank_sessions <- is.na(ucs_model$sessions)
table(blank_sessions)
#CareerFair Data
cfair_data <- read.csv("/Users/arinzenwoye/Desktop/Research Files/careerfair_data.csv", 
                       stringsAsFactors = FALSE, na.strings=c("","NA","-"))
cfair_data_1 <- cfair_data[!duplicated(cfair_data[c("student..Student.ID", "Kiosk.Swipe.Log..Career.Fair")]),]
student_cfair <- cfair_data_1[,c(3,6)]
student_cfair1 <- data.frame(table(student_cfair$student..Student.ID))
colnames(student_cfair1) <- c("ruid", "cfairs")
ucs_model <- merge(ucs_model, student_cfair1, by = c("ruid"), all = TRUE) %>% 
  filter(!is.na(postgrad_status) & jobsearch_method_ucs == 1) 
blank_cfairs <- is.na(ucs_model$cfairs)
table(blank_cfairs)
#utilization
ucs_model <- mutate(ucs_model, utilization = apply(ucs_model[,31:34], 1, sum, na.rm = TRUE))
blank_uti <- is.na(ucs_model$utilization)
table(blank_uti)
histogram(ucs_model$utilization, ucs_model)
#combine jobsearch methods: informal
ucs_model$informal <- ucs_model$jobsearch_method_network
#combine jobsearch methods: formal
ucs_model$formal <- ifelse(ucs_model$jobsearch_method_website == 1 | ucs_model$jobsearch_method_print == 1 |
                          ucs_model$jobsearch_method_media == 1, 1, 0)
ucs_model$status2 <- ifelse(ucs_model$postgrad_status == "Seeking employment", 0, 1)
write.csv(ucs_model, "/Users/arinzenwoye/Desktop/ucs_model.csv")






##crosstables of broad category on 
# y = cross_tables(model, "ethnicity", search_mth, cast = 0)
# t_broad = data.frame(table(model$Broad_Category))
# y1 = cross_tables(model, "gender")
# y2 = cross_tables(model, "first_gen")
# table(model$first_gen, model$`internship/co_op`)
# boxplot(model$jobsearch_skills_agg ~ model$Broad_Category, data = model)
# y3 = cross_tables(model, "Broad_Category")

###Data Visualization 

#distribution of gpa using histogram
ggplot(data = model, aes(x = gpa)) + geom_histogram()
ggplot(data = model, aes(x = gpa)) + stat_density()
#Small Multiples chart of postgrad status by broadcategory
ggplot(data=model, aes(x=postgrad_status)) +
  geom_bar() +
  facet_wrap(~Broad_Category) +
  ggtitle("Status by broad category") +
  theme(plot.title = element_text(family="Trebuchet MS", face="bold", size=20, hjust=0, color="#555555")) +
  theme(axis.text.x = element_text(angle=90)) 


ggplot(data = b1, aes(x = Var1, y = Freq)) + geom_bar()


barplot(freq_uti, main = "Simple Bar Plot", xlab = Var1, ylab = Freq)

library(ggplot2)
ggplot(b, aes(x=Var1, y=Freq)) + geom_bar(stat="identity") + 
labs(x="logins", y="Frequency")

























# test_1 <- mlogit(dep_var ~ 0 | gender + ethnicity + gpa + jobsearch_skills_agg + first_gen +
#                 intern_var + total_internships + Broad_Category + no_japps +
#                 age + active + jobsearch_method_media + jobsearch_method_network + jobsearch_method_ucs +
#                 jobsearch_method_website + jobsearch_method_print, 
#                 reflevel = "Unemployed", heterosc = TRUE, data = tdta)
# 
# test_1 <- mlogit(dep_var ~ 0 | gender + ethnicity + gpa + jobsearch_skills_agg + first_gen + intern_var + total_internships + age + total_internships + active + jobsearch_method_media + jobsearch_method_network + jobsearch_method_ucs + jobsearch_method_website + jobsearch_method_print + Broad_Category, reflevel = "Unemployed", heterosc = TRUE, data = tdta)
# summary(test_1)









# search_name <- c("media", "network", "other", "otherpub", "otherpvt", "print", "ucs", "website")
# search_mth <- c("jobsearch_method_media", "jobsearch_method_network", "jobsearch_method_other",
#                 "jobsearch_method_otherpub", "jobsearch_method_otherpvt", "jobsearch_method_print",
#                 "jobsearch_method_ucs", "jobsearch_method_website")
# 








