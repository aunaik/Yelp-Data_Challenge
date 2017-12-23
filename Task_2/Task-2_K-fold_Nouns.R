# Authors: Akshay Naik and Saniya Ambavanekar

#K fold cross validation for Search

tm<-read.csv("train_matrix.csv",header=TRUE)
View(tm)

brk_point<-seq(from=0,to=nrow(tm),len=6)
group_data<-cut(1:nrow(tm),brk_point,labels=F)
group_data
tm<-cbind(tm,group_data)
tm

#generating train and test files
for(i in seq(1:5))
{
  noun_test_data<-tm[tm$group_data %in% i,]
  noun_test_data
  x<-setdiff(seq(1:5),i)
  noun_train_data<-tm[tm$group_data %in% x,]
  noun_train_data
  
  write.csv(noun_test_data,paste0("noun_test_data_",i,".csv"),row.names = F)
  write.csv(noun_train_data,paste0("noun_train_data_",i,".csv"),row.names = F)
  
}
