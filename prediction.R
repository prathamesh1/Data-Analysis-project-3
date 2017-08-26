dating<-read.csv("Speed Dating Data.csv")
dating<-dating[,c('dec','attr','sinc','intel','fun','amb','shar','like')]
dating<-na.omit(dating)
dating$dec<-factor(dating$dec,levels=c(0,1))
set.seed(1)
indices<-sample(7017,3508)
train<-dating[indices,]
test<-dating[-indices,]

fit<-glm(dec~attr+sinc+intel+fun+amb+shar+like,data=train,family='binomial')
test$probabilities<-predict(fit,newdata=test,type='response')
test$classification<-0
test$classification[which(test$probabilities>0.5)]<-1
length(which(test$dec==test$classification))

#everything 
#fit<-glm(dec~.,data=train,family='binomial')
#everything except intel
fit<-glm(dec~.-fun,data=train,family='binomial')
