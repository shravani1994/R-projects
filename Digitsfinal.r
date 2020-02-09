library(nnet)
test<-read.csv('C:/Users/Nagabhushanam/Downloads/R/test.csv',header=F)
train<-read.csv('C:/Users/Nagabhushanam/Downloads/R/train.csv',header=F)
train1<-train[,2:785]
test1<-test[,2:785]
seq4<-function(x) seq(x,x+3,1)
seq28<- function(x) seq(x,x+3*28,28)
w1<-lapply(seq(1,28,4),function(a) lapply(seq28(a),function(x) seq4(x)))
w2<-lapply(seq(112+1,112+1+24,4),function(a) lapply(seq28(a),function(x) seq4(x)))
w3<-lapply(seq((2*112)+1,(2*112)+1+24,4),function(a) lapply(seq28(a),function(x) seq4(x)))
w4<-lapply(seq((3*112)+1,(3*112)+1+24,4),function(a) lapply(seq28(a),function(x) seq4(x)))
w5<-lapply(seq((4*112)+1,(4*112)+1+24,4),function(a) lapply(seq28(a),function(x) seq4(x)))
w6<-lapply(seq((5*112)+1,(5*112)+1+24,4),function(a) lapply(seq28(a),function(x) seq4(x)))
w7<-lapply(seq((6*112)+1,(6*112)+1+24,4),function(a) lapply(seq28(a),function(x) seq4(x)))
f<-function(r){
  row1<-lapply(seq(1:7),function(q) sum(train1[r,unlist(w1[q])]))
  row2<-lapply(seq(1:7),function(q) sum(train1[r,unlist(w2[q])]))
  row3<-lapply(seq(1:7),function(q) sum(train1[r,unlist(w3[q])]))
  row4<-lapply(seq(1:7),function(q) sum(train1[r,unlist(w4[q])]))
  row5<-lapply(seq(1:7),function(q) sum(train1[r,unlist(w5[q])]))
  row6<-lapply(seq(1:7),function(q) sum(train1[r,unlist(w6[q])]))
  row7<-lapply(seq(1:7),function(q) sum(train1[r,unlist(w7[q])]))
  m<-cbind(row1,row2,row3,row4,row5,row6,row7)
  return(unlist(m))
}
f1<-function(r){
  row1<-lapply(seq(1:7),function(q) sum(test1[r,unlist(w1[q])]))
  row2<-lapply(seq(1:7),function(q) sum(test1[r,unlist(w2[q])]))
  row3<-lapply(seq(1:7),function(q) sum(test1[r,unlist(w3[q])]))
  row4<-lapply(seq(1:7),function(q) sum(test1[r,unlist(w4[q])]))
  row5<-lapply(seq(1:7),function(q) sum(test1[r,unlist(w5[q])]))
  row6<-lapply(seq(1:7),function(q) sum(test1[r,unlist(w6[q])]))
  row7<-lapply(seq(1:7),function(q) sum(test1[r,unlist(w7[q])]))
  m<-cbind(row1,row2,row3,row4,row5,row6,row7)
  return(unlist(m))
}
train7<-lapply(seq(1:2000),function(i) f(i))
train7<-lapply(train7,function(a) return(a/4080))
test7<-lapply(seq(1:1000),function(i) f1(i))
test7<-lapply(test7,function(a) return(a/4080))
df7train<-data.frame(train[,1],matrix(unlist(train7),nrow=2000,byrow=T))
colnames(df7train)<-c(seq(1:50))
df7train<-round(df7train)
df7test<-data.frame(test[,1],matrix(unlist(test7),nrow=1000,byrow = T))
colnames(df7test)<-c(seq(1:50))
df7test<-round(df7test)
best<-100000
for(i in 1:3){
  nn<-nnet(as.factor(df7train[,1])~.,data=df7train[,2:50],maxit = 1000,size=10,linout=T,skip=T,MaxNWts=1500)
  if(nn$value<best)
  {
    best<-nn$value
    nnsave<-nn
    print(best)
  }
}
pred<-predict(nnsave,df7test[,2:50],type='class')
pred<-round(as.numeric(pred))
plot(df7test[,1],pred)
print(table(as.integer(df7test[,1]),as.integer(pred)))