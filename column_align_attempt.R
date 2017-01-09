rm(list=ls())
quest1 <- c(5,5,5)
quest2 <- c(5,5,5)
quest3<- c("a","b","c")
quest4 <- c(7,7,7)
quest5 <- c(8,8,8)

frame1 <- data.frame(quest1,quest2,quest3)
frame2 <- data.frame(quest4,quest5)
frame3 <- data.frame(quest3,quest1,quest2)
frame4 <- data.frame(quest3, quest1, quest4)

# if(names(frame1)[1]%in%names(frame2))
#   print(names(frame1)[1])
# }
# 
# match(names(frame1), names(frame3))

List <- list()
for (i in 1:length(names(frame1))){
  if(names(frame1)[i]%in%names(frame3)){
    normF <- frame1[names(frame1)[i]]
    List[[i]] <- normF
  }
}
new_frame <- do.call(cbind, List)

match(names(frame1), names(frame4))


