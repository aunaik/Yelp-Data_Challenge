# Authors: Akshay Naik and Saniya Ambavanekar

#Prediction On Nouns

#Performing Regression 

require(randomForest)
require(ModelMetrics)
require(caret)
require(e1071)

#k=1
err_lm<-c(0)
err_svm<-c(0)
err_dt<-c(0)
err_rf<-c(0)
for(k in seq(1:5))
{

  filename1=paste("noun_train_data",k,sep="_")
  filename2=paste("noun_test_data",k,sep = "_")
  ext=".csv"
  filename1=paste(filename1,ext,sep="")
  filename2=paste(filename2,ext,sep="")
  print(filename1)
  print(filename2)
  
  new_train_matrix<-read.csv(filename1,header = TRUE)
  new_test_matrix<-read.csv(filename2,header = TRUE)
  
  new_train_matrix$group_data<-NULL
  new_test_matrix$group_data<-NULL
  
  
#Linear Regression

#new_train_target<-new_train_matrix[,ncol(new_train_matrix)]
#train_d<-new_train_matrix[,-ncol(new_train_matrix)]

model<-lm(new_train_matrix$star_rate~.,data=new_train_matrix)
m.predict<-predict(model,new_test_matrix[,1:(ncol(new_test_matrix)-1)],se.fit=TRUE)
m.predict<-round(m.predict$fit)


rmse_lm<-rmse(new_test_matrix[,ncol(new_test_matrix)],m.predict)
print(rmse_lm)

lm_accuracy<-sum(m.predict==new_test_matrix[,ncol(new_test_matrix)])/length(m.predict)
print(lm_accuracy)

err_lm[k]<-rmse_lm

#random Forest
rf.model<-randomForest(x=new_train_matrix[,1:(ncol(new_train_matrix)-1)],y=new_train_matrix[,ncol(new_train_matrix)])
rf.predict<-predict(rf.model,new_test_matrix[,1:(ncol(new_test_matrix))-1])
print(rf.predict)
rf.predict<-round(rf.predict)
rf_accuracy<-(sum(rf.predict==new_test_matrix[,ncol(new_test_matrix)]))/length(rf.predict)
print(rf_accuracy)
rmse_rf<-rmse(new_test_matrix[,ncol(new_test_matrix)],rf.predict)
print(rmse_rf)
err_rf[k]<-rmse_rf

#SVM
svm_model <-svm(new_train_matrix$star_rate~.,data=new_train_matrix,scale=FALSE)
predict_svm <- predict(svm_model,new_test_matrix[,1:(ncol(new_test_matrix)-1)])
predict_svm<-round(predict_svm)
#SVM Evaluation
rmse_svm<-rmse((new_test_matrix[,ncol(new_test_matrix)]),(predict_svm))
print(rmse_svm)


svm_accuracy<-(sum(predict_svm==new_test_matrix[,ncol(new_test_matrix)]))/length(predict_svm)
print(svm_accuracy)

err_svm[k]<-rmse_svm


#decision trees
dt_model<-rpart(new_train_matrix$star_rate~.,data=new_train_matrix,method = "anova")
predict_dt<-predict(dt_model,new_test_matrix[,1:(ncol(new_test_matrix)-1)])
predict_dt<-round(predict_dt)

rmse_dt<-rmse(new_test_matrix[,ncol(new_test_matrix)],predict_dt)
print(rmse_dt)

dt_accuracy<-(sum(predict_dt==new_test_matrix[,ncol(new_train_matrix)]))/length(predict_dt)
print(dt_accuracy)

err_dt[k]<-rmse_dt

}

print(err_lm)
print(err_rf)
print(err_svm)
print(err_dt)

an_df<-as.data.frame(cbind(seq(1:5),err_lm,err_rf,err_svm,err_dt))
ggplot(an_df,aes(x=an_df$V1))+
  geom_line(aes(y = an_df$err_lm, colour="lm")) + 
  geom_line(aes(y = an_df$err_rf, colour = "rf")) + 
  geom_line(aes(y = an_df$err_svm, colour = "svm")) + 
  geom_line(aes(y = an_df$err_dt, colour = "dt"))+
  scale_color_manual(values = c("lm"="blue", "rf"="red","svm"="orange","dt"="green"))+
  ylab(label="RMSE") + 
  xlab(label="Number of Folds")

