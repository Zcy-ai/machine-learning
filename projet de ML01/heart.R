library(MASS)
library('nnet')
heart <- read.table("C:/Users/Dell/Desktop/projet_final/heart_failure_clinical_records_dataset.csv",
                   header=TRUE, sep=",")
n<- nrow(heart)
p<-ncol(heart)-1

ntrain<-round(2*n/3)
ntest<-n-ntrain
train<-sample(n,ntrain)
heart.train<-heart[train,]
heart.test<-heart[-train,]


lda.heart<- lda(DEATH_EVENT~.,data=heart.train)
pred.heart.lda<-predict(lda.heart,newdata=heart.test)

perf <-table(heart.test$DEATH_EVENT,pred.heart.lda$class)
print(perf)

1-sum(diag(perf))/ntest  # error rate

library(pROC)
citation("pROC")
roc_curve<-roc(heart.test$DEATH_EVENT,as.vector(pred.heart.lda$x))
plot(roc_curve)

loss<-Inf
for(i in 1:10){
  nn<- nnet(DEATH_EVENT ~ ., data=heart.train, size=30, linout = TRUE,maxit=200,trace=FALSE)
  print(c(i,nn$value))
  if(nn$value<loss){
    loss<-nn$value
    nn.best<-nn
  }
}
pred.test<-predict(nn.best,newdata=heart.test)
mse<-mean((pred.test-heart.test$DEATH_EVENT)^2)
Kmax<-20
ERR100<-matrix(0,Kmax)
ERR500<-ERR100
for(k in 1:Kmax){
  ypred<-knn(heart.train[ ,1:12],heart.test[ ,1:12],factor(heart.train$DEATH_EVENT),k=k)
  ERR100[k]<-mean(heart.test$DEATH_EVENT != ypred)
  ypred<-knn(heart.train[ ,c(1,5,8,12)],heart.test[ ,c(1,5,8,12)],factor(heart.train$DEATH_EVENT),k=k)
  ERR500[k]<-mean(heart.test$DEATH_EVENT != ypred)
}

plot(1:Kmax,ERR100,type="b",ylim=range(0,0.8))
lines(1:Kmax,ERR500,col="red")

#knn
ypred<-knn(heart.train[ ,1:12],heart.test[ ,1:12],factor(heart.train$DEATH_EVENT),k=4)
table(heart.test$DEATH_EVENT,ypred)
errk<-mean(heart.test$DEATH_EVENT != ypred)
print(errk)

#knn
ypred<-knn(heart.train[ ,c(1,5,8,12)],heart.test[ ,c(1,5,8,12)],factor(heart.train$DEATH_EVENT),k=8)
table(heart.test$DEATH_EVENT,ypred)
errk2<-mean(heart.test$DEATH_EVENT != ypred)
print(errk2)

