# Authors: Akshay Naik and Saniya Ambavanekar

#K_fold with frequency greater than 100

tm<-read.csv("adv_train_data_freq100.csv",header=TRUE)

#tm_names<-read.csv("Adjv.csv",header=TRUE)
#print(tm_names)
#colnames(tm)<-tm_names$Var1
#colnames(tm)[ncol(tm)]<-"star_rate"

View(tm)

brk_point<-seq(from=0,to=nrow(tm),len=6)
group_data<-cut(1:nrow(tm),brk_point,labels=F)
group_data
tm<-cbind(tm,group_data)
tm

#generating train and test files
for(i in seq(1:5))
{
  na_test_data<-tm[tm$group_data %in% i,]
  na_test_data
  x<-setdiff(seq(1:5),i)
  na_train_data<-tm[tm$group_data %in% x,]
  na_train_data
  
  write.csv(na_test_data,paste0("a100_test_data_",i,".csv"),row.names = F)
  write.csv(na_train_data,paste0("a100_train_data_",i,".csv"),row.names = F)
  
}
