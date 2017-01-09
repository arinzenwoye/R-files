# toidta <- read.csv("C:/Users/avn18/Desktop/toidta.csv")
# attach(toidta)
# summary(Contact.Type)
# summary(Services.Provided)
write.csv(tdata,"C:/Users/avn18/Desktop/tdta.csv")
workshop <- read.csv("C:/Users/avn18/Desktop/workshop_data.csv")
attach(workshop)
myvars <- c("student..Student.ID", "Workshop..Symplicity.Workshop.ID", 
            "Workshop..Name", "Workshop..Session.Start", "Workshop..Staff.Member.1",
            "Workshop..Staff.Member.2")
newworkshop <- workshop[myvars]
detach(workshop)
attach(newworkshop)
workshop_1 <- newworkshop[!duplicated(newworkshop[c("student..Student.ID", "Workshop..Session.Start", 
                                                    "Workshop..Symplicity.Workshop.ID")]),]
workshop_2 <- subset(workshop_1, Workshop..Staff.Member.1=="Toi" | Workshop..Staff.Member.1=="Toi Tyson")
#testworkshop <- subset(workshop_1, Workshop..Staff.Member.1=="Sylvia" | Workshop..Staff.Member.1=="Sylvia Cordero" |  
#                         Workshop..Staff.Member.2=="Sylvia Cordero") if the staff appears in both member1 & member 2
detach(newworkshop)
attach(workshop_2)
toi <- as.data.frame(table(Workshop..Symplicity.Workshop.ID))
myvars1 <- c("Workshop..Symplicity.Workshop.ID", 
            "Workshop..Name", "Workshop..Session.Start")
workshop_3 <- workshop_2[myvars1]
x2 <- workshop_3[!duplicated(workshop_3[c("Workshop..Symplicity.Workshop.ID")]),]
TT_data <- merge(toi, x2, by = c("Workshop..Symplicity.Workshop.ID"), all = FALSE)
write.csv(TT_data, "C:/Users/avn18/Desktop/TT_data.csv")
