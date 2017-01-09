rm(list=ls())
library(plyr)
library(dplyr)
library(reshape)
library(reshape2)
library(tidyr)
library(Hmisc)
library(pastecs)
project <- read.csv("/Users/arinzenwoye/Desktop/plswrk.csv", stringsAsFactors = FALSE, na.strings=c("","NA"))
project <- project[c(-1,-2),]
project <- select(project, c(4,12,15,17,21,27,35,46,53,54,65,89:94,96:100,
                             102:107,109:116,154,156,158,162:176,191:195,23,177:180,22,2,26))
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
                       "Matriculation")
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
project <- mutate(project, jobsearch_skills_agg = apply(project[,12:28],1,sum, na.rm = TRUE),
                  active = apply(project[,41:54],1,sum, na.rm = TRUE))
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
# }
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
broad_cart <- read.csv("/Users/arinzenwoye/Desktop/broad_cart.csv", stringsAsFactors = FALSE)
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
         Broad_Category, jobsearch_skills_agg, no_japps, jobsearch_method_ucs, jobsearch_method_media, 
         jobsearch_method_media, jobsearch_method_print, jobsearch_method_network, jobsearch_method_website,
         jobsearch_method_otherpub, jobsearch_method_otherpvt, jobsearch_method_other, Major, age, Matriculation) 
detach(project_2) 
attach(model) 
#sum of missing values
blanks = is.na(model)
missing_bycol = as.data.frame(apply(blanks,2,sum))
model <- mutate(model, missing_byrow = apply(blanks,1,sum))
##check: sapply(airquality, function(x) sum(is.na(x)))

### Descriptive Statistics
options(scipen = 100)
options(digits = 2)
sum_stas <- stat.desc(model)
#cartegory frequency & modification
b_freq = as.data.frame(sort(table(model$Broad_Category), decreasing = TRUE))
model$Broad_Category <- mapvalues(model$Broad_Category, from = c("Architecture and Related Services",
                                  "Philosophy and Religious Studies", 
                                  "Public Administration and Social Service Professions"), 
                                  to = c(rep("Other", 3)))
#crosstables
attach(model)

search_mth = c("jobsearch_method_media", "jobsearch_method_network", "jobsearch_method_other",
  "jobsearch_method_otherpub", "jobsearch_method_otherpvt", "jobsearch_method_print",
  "jobsearch_method_ucs", "jobsearch_method_website")

t3 = data.frame(table(model$Broad_Category, model$postgrad_status))
t_bstatus = spread(t3, Var2, Freq)


#start of function

cross_tables <- function(data = model, xtric, vars, cast = 0){  
  cast_xtric  = xtric
  freq = data.frame(rowSums(xtric[,2:length(xtric)], na.rm = TRUE))
  j = 1
  if(cast ==0){
    melt_xtric = melt(data, id = xtric, measure = vars)
    print(head(melt_xtric))
    melt_xtric <- filter(melt_xtric, value == 1)
    cast_xtric = dcast(melt_xtric, paste(xtric,"~variable"))
    #print(cast_xtric)
    #cast_eth.rownames <- data.frame(cast_eth[,-1], row.names=cast_eth[,1], na.rm = TRUE)
    cast_xtric = cast_xtric[!is.na(cast_xtric[,1]),]
    print(cast_xtric)
    freq = data.frame(table(data[xtric]))
    j = 2
  }
  new_table = c()  
  #print(cast_xtric[,2]) 
  print(freq[,j])  
  for (i in 2:length(cast_xtric)){
    new_table = cbind(new_table, cast_xtric[,i]/freq[,j])
  }
  return(new_table)
}   


t_10 = cross_tables(data = model, t_bstatus, colnames(t_bstatus), cast = 1)




t_broad = data.frame(table(model$Broad_Category))

y = cross_tables(model, "ethnicity", search_mth)

y1 = cross_tables(model, "gender")

y2 = cross_tables(model, "first_gen")

table(model$first_gen, model$`internship/co_op`)

boxplot(model$jobsearch_skills_agg ~ model$Broad_Category, data = model)

y3 = cross_tables(model, "Broad_Category")
















# search_list <- c(15:22)
# m1 <- data.frame(c("White, Non-Hispanic","Hispanic/Puerto-Rican ","Asian only","Black, Non-Hispanic",   
#                    "other"))
# colnames(m1) <- c("ethnicity")
# 
# big_merge <- function(a,b, x = "ethnicity"){
#   return(merge(a, b, by = x, all = TRUE))
# }
# 
# for (i in search_list){
#   s_name <- paste("table:", i)
#   #assign(s_name, data.frame(xtabs(~ethnicity+model[,i], data=model)))
#   #print(class(s_name))
#   m1 <- merge(m1, assign(s_name, data.frame(xtabs(~ethnicity+model[,i], data=model))), by = "ethnicity", all = TRUE)
# }


















# search_name <- c("media", "network", "other", "otherpub", "otherpvt", "print", "ucs", "website")
# search_mth <- c("jobsearch_method_media", "jobsearch_method_network", "jobsearch_method_other",
#                 "jobsearch_method_otherpub", "jobsearch_method_otherpvt", "jobsearch_method_print",
#                 "jobsearch_method_ucs", "jobsearch_method_website")
# 









