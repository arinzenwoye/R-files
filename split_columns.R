# sc_majors <- read.csv("/Users/arinzenwoye/Desktop/work files/Students.04.18.2016.csv")
# attach(sc_majors)

split_cs_columns <- function(x){
  #split comma seperated columns into seperate rows
  student_maj <- strsplit(as.character(x), ',')
  student_maj <- as.data.frame(unlist(student_maj))
  freq_student_maj <- as.data.frame(table(student_maj))
  return(freq_student_maj)
}

# student_maj <- strsplit(as.character(Student.Profile..Major), ',')
# student_maj <- as.data.frame(unlist(student_maj))
# write.csv(student_maj, "/Users/arinzenwoye/Desktop/test2.csv")
# 
# 
# t2 <- read.csv("/Users/arinzenwoye/Desktop/test2-csv.csv")
# attach(t2)