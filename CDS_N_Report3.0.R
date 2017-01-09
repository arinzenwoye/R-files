
#### Counseling 

cdata <- read.csv("C:/Users/avn18/Desktop/Counseling Data1.csv")
attach(cdata)
cdata <- subset(cdata, Career.Counseling..Created.By..Counselor.!="")
cdata_1 <- data.frame(do.call('rbind', 
                              strsplit(as.character(Career.Counseling..Created.By..Counselor.),'-',fixed=TRUE)))
names(cdata_1) <- c("loc", "CDS")
CDS <- cdata_1["CDS"]
ctest <- cbind(cdata,CDS)
write.csv(ctest, "C:/Users/avn18/Desktop/ctestdta1.csv")
#ctest csv file is trimmed and read into R
ctest <- read.csv("C:/Users/avn18/Desktop/ctestdta1.csv")
attach(ctest)
myvars <- c("Career.Counseling..Counseling.Date", "Career.Counseling..Counseling.Type", 
            "Career.Counseling..Created.By..Counselor.","Career.Counseling..Student.Contact.Type",
            "student..Student.ID", "Student.Profile..Class.Level", "CDS")
new_ctest <- ctest[myvars]
attach(new_ctest)
ctest_1 <- new_ctest[!duplicated(new_ctest[c("Career.Counseling..Counseling.Date", 
                                              "student..Student.ID")]),]
detach(ctest)
attach(ctest_1)
CDS_1 <- c("Amanda Choo","Andrew Seguel","Colin Liebtag","Joe Scott", 
           "Larry Jacobs", "Mindy O'Mealia", "Monica Bryant", "Sue Pye", "Sylvia Cordero",
           "Tamara Peters")
for (i in CDS_1){
  nam <- paste("CDS_name:", i, sep = "")
  assign(nam, subset(ctest_1, CDS==i)) 
}

dflist <- list(`CDS_name:Amanda Choo`,`CDS_name:Andrew Seguel`,`CDS_name:Colin Liebtag`,
               `CDS_name:Joe Scott`,`CDS_name:Larry Jacobs`,`CDS_name:Mindy O'Mealia`,
               `CDS_name:Monica Bryant`,`CDS_name:Sue Pye`,`CDS_name:Sylvia Cordero`,
               `CDS_name:Tamara Peters`)

#Counseling Type
for(i in 1:length(dflist)){
  DF <-dflist[[i]]["Career.Counseling..Counseling.Type"]  
  freq_nam <- paste("CounsType:", CDS_1[i], sep = "")
  assign(freq_nam, as.data.frame(table(DF))) 
}

#Contact Type
for(i in 1:length(dflist)){
  DF <-dflist[[i]]["Career.Counseling..Student.Contact.Type"]  
  freq_nam <- paste("ContactType:", CDS_1[i], sep = "")
  assign(freq_nam, as.data.frame(table(DF))) 
}

#Class Level
for(i in 1:length(dflist)){
  DF <-dflist[[i]]["Student.Profile..Class.Level"]  
  freq_nam <- paste("Class_level:", CDS_1[i], sep = "")
  assign(freq_nam, as.data.frame(table(DF))) 
}

#### Workshops

workshop <- read.csv("/Users/arinzenwoye/Desktop/Workshops Data.csv",stringsAsFactors=FALSE)
attach(workshop)
myvars <- c("student..Student.ID", "Workshop..Symplicity.Workshop.ID", 
            "Workshop..Name", "Workshop..Session.Start", "Workshop..Staff.Member.1",
            "Workshop..Staff.Member.2")
newworkshop <- workshop[myvars]
detach(workshop)
attach(newworkshop)
workshop_1 <- newworkshop[!duplicated(newworkshop[c("student..Student.ID", "Workshop..Session.Start", 
                                                    "Workshop..Symplicity.Workshop.ID")]),]
detach(newworkshop)
attach(workshop_1)
#Recoding
workshop_1$Workshop..Staff.Member.1[workshop_1$Workshop..Staff.Member.1=="Amanda"] <- "Amanda Choo"
workshop_1$Workshop..Staff.Member.1[workshop_1$Workshop..Staff.Member.1=="Andrew"] <- "Andrew Seguel"
workshop_1$Workshop..Staff.Member.1[workshop_1$Workshop..Staff.Member.1=="Joe"] <- "Joe Scott"
workshop_1$Workshop..Staff.Member.1[workshop_1$Workshop..Staff.Member.1=="Larry"] <- "Larry Jacobs"
workshop_1$Workshop..Staff.Member.1[workshop_1$Workshop..Staff.Member.1=="Mindy"] <- "Mindy O'Melia"
workshop_1$Workshop..Staff.Member.1[workshop_1$Workshop..Staff.Member.1=="Sylvia"|
            workshop_1$Workshop..Staff.Member.1=="Sylvia D. Cordero"| 
            workshop_1$Workshop..Staff.Member.1=="Sylvia D Cordero"] <- "Sylvia Cordero"
workshop_1$Workshop..Staff.Member.1[workshop_1$Workshop..Staff.Member.1=="Tamara"] <- "Tamara Peters"
workshop_1$Workshop..Staff.Member.2[workshop_1$Workshop..Staff.Member.2=="Joe"] <- "Joe Scott"
workshop_1$Workshop..Staff.Member.2[workshop_1$Workshop..Staff.Member.2=="Larry"] <- "Larry Jacobs"
workshop_1$Workshop..Staff.Member.2[workshop_1$Workshop..Staff.Member.2=="Mindy"] <- "Mindy O'Melia"
workshop_1$Workshop..Staff.Member.2[workshop_1$Workshop..Staff.Member.2=="Monica"] <- "Monica Bryant"
workshop_1$Workshop..Staff.Member.2[workshop_1$Workshop..Staff.Member.2=="Tamara"] <- "Tamara Peters"
attach(workshop_1)

WDS_1 <- c("Amanda Choo","Andrew Seguel","Colin Liebtag","Joe Scott", 
           "Larry Jacobs", "Mindy O'Melia", "Monica Bryant", "Sue Pye", "Sylvia Cordero",
           "Tamara Peters")
for (i in WDS_1){
  nam <- paste("WDS_name:", i, sep = "")
  assign(nam, subset(workshop_1, Workshop..Staff.Member.1==i | Workshop..Staff.Member.2==i )) 
}

df1list <- list(`WDS_name:Amanda Choo`,`WDS_name:Andrew Seguel`,`WDS_name:Colin Liebtag`,
                          `WDS_name:Joe Scott`,`WDS_name:Larry Jacobs`,`WDS_name:Mindy O'Melia`,
                          `WDS_name:Monica Bryant`,`WDS_name:Sue Pye`,`WDS_name:Sylvia Cordero`,
                          `WDS_name:Tamara Peters`)

#freq by workshopID
for(i in 1:length(df1list)){
  DF <-df1list[[i]]["Workshop..Symplicity.Workshop.ID"]  
  freq_nam <- paste("workshops:", WDS_1[i], sep = "")
  assign(freq_nam, as.data.frame(table(DF))) 
}
myvars1 <- c("Workshop..Symplicity.Workshop.ID", 
             "Workshop..Name", "Workshop..Session.Start")
workshop_2 <- workshop_1[myvars1]
wrk_name <- workshop_2[!duplicated(workshop_2[c("Workshop..Symplicity.Workshop.ID")]),]
colnames(wrk_name)[1] <- "DF"

df2list <- list(`workshops:Amanda Choo`,`workshops:Andrew Seguel`,`workshops:Joe Scott`,
                `workshops:Larry Jacobs`,`workshops:Mindy O'Melia`,`workshops:Monica Bryant`,
                `workshops:Sue Pye`,`workshops:Sylvia Cordero`,`workshops:Tamara Peters`)
WDS_2 <- c("Amanda Choo","Andrew Seguel","Joe Scott","Larry Jacobs", "Mindy O'Melia", "Monica Bryant", 
           "Sue Pye", "Sylvia Cordero","Tamara Peters")

for(i in 1:length(df2list)){
  freq1_nam <- paste("name_workshops:", WDS_2[i], sep = "")
  assign(freq1_nam, merge(df2list[[i]], wrk_name, by = c("DF"), all = FALSE)) 
}




















# Table_func <- function(DF,Var){
#   Col<- DF[Var]
#   Freq <- table(Col)
#   return(Freq
#   
# }
# 
# dflist_1 <- lapply(dflist[Career.Counseling..Counseling.Type], function(df) {
#   #(table(dflist[Career.Counseling..Counseling.Type]))
# })
# 
# 
# dflist_1 <- lapply(dflist[Career.Counseling..Counseling.Type],table)
# 
# h <- 1 
# for(j in CDS_1){
#   nam_1 <- paste("consult_", j, sep="")
#   assign(nam_1, dflist_1[[h]])
#   h = h + 1
# }
#  
# 






# amanda_consult <- dflist_1[[1]]
# andrew_consult <- 
# 
# CDS_Frame <- as.data.frame(c(`CDS_name:Sue Pye`,`CDS_name:Andrew Seguel`))