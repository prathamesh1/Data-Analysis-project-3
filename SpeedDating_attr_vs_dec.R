dating<-read.csv('Speed Dating Data.csv')
data<-dating[c('dec','attr')]
which(is.na(data$attr))

data<-data%>%
  filter(!is.na(attr))

data$yn<-'yes'
data$yn[which(data$dec==0)]<-'no'
data$yn<-factor(data$yn,levels=c('yes','no'))

set.seed(1)
indices<-sample(8176,4088,replace=FALSE)
train<-data[indices,]
test<-data[-indices,]

glm(yn~attr,data=train,family='binomial')

f<-function(x){
  return(1/(1+exp(4.6818-0.6879*x)))
}

domain<-data.frame(x=c(0:10))

ggplot()+
  geom_point(data=test,aes(x=attr,y=dec),position=position_jitter(height = 0.1))+
  stat_function(data=domain,aes(x=x),fun=f)

classify<-function(x){
  return(as.numeric(f(x)>=.5))
}
test$classification<-classify(test$attr)

length(which(test$dec!=test$classification))
1121/4088
