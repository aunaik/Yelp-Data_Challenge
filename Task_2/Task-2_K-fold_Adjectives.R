# Authors: Akshay Naik and Saniya Ambavanekar

# K-fold on Adjectives

tm<-read.csv("adv_train_data.csv",header=TRUE)

tm_names<-read.csv("Adjv.csv",header=TRUE)
print(tm_names)
colnames(tm)<-tm_names$Var1
colnames(tm)[ncol(tm)]<-"star_rate"

View(tm)

brk_point<-seq(from=0,to=nrow(tm),len=6)
group_data<-cut(1:nrow(tm),brk_point,labels=F)
group_data
tm<-cbind(tm,group_data)
tm

#generating train and test files
for(i in seq(1:5))
{
  adj_test_data<-tm[tm$group_data %in% i,]
  adj_test_data
  x<-setdiff(seq(1:5),i)
  adj_train_data<-tm[tm$group_data %in% x,]
  adj_train_data
  
  write.csv(adj_test_data,paste0("adj_test_data_",i,".csv"),row.names = F)
  write.csv(adj_train_data,paste0("adj_train_data_",i,".csv"),row.names = F)
  
}
