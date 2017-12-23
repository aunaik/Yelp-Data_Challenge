# Authors: Akshay Naik and Saniya Ambavanekar

#Prediction On Adjectives

#Performing Regression 

require(randomForest)
require(ModelMetrics)
require(caret)
require(e1071)
require(rpart)

#k=2
err_lm<-c(0)
err_svm<-c(0)
err_dt<-c(0)
err_rf<-c(0)
accuracy_lm<-c(0)
accuracy_rf<-c(0)
accuracy_dt<-c(0)
accuracy_svm<-c(0)

for(k in seq(1:5))
{
  
  filename1=paste("adj_train_data",k,sep="_")
  filename2=paste("adj_test_data",k,sep = "_")
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
  m.predict<-predict(model,new_test_matrix[,-ncol(new_test_matrix)],se.fit=TRUE)
  m.predict<-round(m.predict$fit)
  
  
  rmse_lm<-rmse(new_test_matrix[,ncol(new_test_matrix)],m.predict)
  print(rmse_lm)
  
  lm_accuracy<-sum(m.predict==new_test_matrix[,ncol(new_test_matrix)])/length(m.predict)
  print(lm_accuracy)
  
  err_lm[k]<-rmse_lm
  accuracy_lm[k]<-lm_accuracy
  
  #random Forest
  rf.model<-randomForest(x=new_train_matrix[,-ncol(new_train_matrix)],y=new_train_matrix[,ncol(new_train_matrix)])
  rf.predict<-predict(rf.model,new_test_matrix[,-ncol(new_test_matrix)])
  print(rf.predict)
  rf.predict<-round(rf.predict)
  rf_accuracy<-(sum(rf.predict==new_test_matrix[,ncol(new_test_matrix)]))/length(rf.predict)
  print(rf_accuracy)
  rmse_rf<-rmse(new_test_matrix[,ncol(new_test_matrix)],rf.predict)
  print(rmse_rf)
  err_rf[k]<-rmse_rf
  accuracy_rf[k]<-rf_accuracy
  
  #SVM
  svm_model <-svm(new_train_matrix$star_rate~.,data=new_train_matrix,scale=FALSE)
  predict_svm <- predict(svm_model,new_test_matrix[,-ncol(new_test_matrix)])
  predict_svm<-round(predict_svm)
  #SVM Evaluation
  rmse_svm<-rmse((new_test_matrix[,ncol(new_test_matrix)]),(predict_svm))
  print(rmse_svm)
  
  
  svm_accuracy<-(sum(predict_svm==new_test_matrix[,ncol(new_test_matrix)]))/length(predict_svm)
  print(svm_accuracy)
  
  err_svm[k]<-rmse_svm
  accuracy_svm[k]<-svm_accuracy
  
  #decision trees
  dt_model<-rpart(new_train_matrix$star_rate~.,data=new_train_matrix,method = "anova")
  predict_dt<-predict(dt_model,new_test_matrix[,-ncol(new_test_matrix)])
  predict_dt<-round(predict_dt)
  
  rmse_dt<-rmse(new_test_matrix[,ncol(new_test_matrix)],predict_dt)
  print(rmse_dt)
  
  dt_accuracy<-(sum(predict_dt==new_test_matrix[,ncol(new_train_matrix)]))/length(predict_dt)
  print(dt_accuracy)
  
  err_dt[k]<-rmse_dt
  accuracy_dt[k]<-dt_accuracy
  
  
}

print(err_lm)
print(err_rf)
print(err_svm)
print(err_dt)
avg_err<-c(0)
avg_err[1]<-mean(err_lm)
avg_err[2]<-mean(err_rf)
avg_err[3]<-mean(err_svm)

avg_err[4]<-mean(err_dt)

print(avg_err)

print(accuracy_lm)
print(accuracy_rf)
print(accuracy_svm)
print(accuracy_dt)
