PCA<-read.csv("C:/Users/avn18/Desktop/PCA.data.csv")

PCA_Result<- function(YEAR){
  print(c("Data Result for year:", YEAR))
  PCA = PCA[PCA$Year==YEAR,]
  PCA$DATE = as.Date(PCA$DATE,"%m/%d/%y")
  mth=months(PCA$DATE)
  PCA["mth"] = mth
  mth_unique=unique(mth)
  empty_df = as.data.frame(unique(PCA$name))
  colnames(empty_df) = "name"
  
  for(i in 1:length(mth_unique)){
    print(mth_unique[i])
    DF = PCA[PCA$mth==mth_unique[i],]
    y = table(DF$name)
    y = data.frame(y)
    colnames(y)= c("name",paste("freq",mth_unique[i],"Year_",YEAR))
    result = merge(y,empty_df,by = "name", all = TRUE)
    empty_df = result
    }
print(empty_df)
}
year_1 = PCA_Result(1)
year_2 = PCA_Result(2)

PCA_Result = merge(year_1,year_2, by = "name", all = TRUE)
write.csv(PCA_Result,"C:/Users/ch627/Desktop/PCADataResutl.csv")
