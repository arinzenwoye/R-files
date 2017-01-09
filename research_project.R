rm(list=ls())
project <- read.csv("/Users/arinzenwoye/Desktop/plswrk.csv", stringsAsFactors = FALSE)
project <- select(project, c(4,12,15,17,21,23,27,35,46,53,54,65,89:94,96:100,
                  102:107,109:116,154,156,158,162:176,191:195))
colnames(project) <- c("name", "ruid", "gender", "ethnicity", "school","Major",
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
                       "internship_no", "internship_over1yr")
project$birthday <- as.Date(project$birthday, "%m/%d/%Y")
birth_year <- as.numeric(format(project$birthday,'%Y')) 
project <- mutate(project, age = 2016 - birth_year)
cart_list <- c(9, 10, 12:36)
for (i in cart_list){
  project[,i] <- revalue(project[,i], c("Yes"="1", "No"="0"))
}
cart_list1 <- c(40:59)
for(j in cart_list1){
  project[,j] <- ifelse(project[,j]=="",0,1)
}
for(k in 12:28){
  project[,k] <- as.numeric(project[,k]) 
}
project <- mutate(project, jobsearch_skills_agg = apply(project[,12:28],1,sum))



  





























# project1 <- select(project, RUID, Gender, School, cum_gpa, Degree, Visa, emp_status, 
#                    first_choice, first_gen, industry, start_salary, 
#                    starts_with("jobsearch_"), number_joboffers, number_joboffers,
#                    number_jobinterviews, number_jobapplications, 
#                    starts_with("extracurricular"), internship.s..Co.op.s.,
#                    starts_with("internship_"))
