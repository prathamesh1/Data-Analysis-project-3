dating<-read.csv('Speed Dating Data.csv')
data<-dating[c('dec','like')]
data$yn<-'yes'
indices<-which(data$dec==0)
data$yn[indices]<-'no'
data$yn<-factor(data$yn,levels=c('yes','no'))

data<-data%>%
  filter(!is.na(like))

set.seed(1)
indices<-sample(8138,4069,replace=FALSE)
train<-data[indices,]
test<-data[-indices,]

glm(yn~like,data=train,family='binomial')
f<-function(x){
  return(1/(1+exp(5.6539-0.8464*x)))
}

domain<-data.frame(x=c(0,10))

ggplot()+
  geom_point(data=test,aes(x=like,y=dec),position=position_jitter(height=0.1))+
  stat_function(data=domain,aes(x=x),fun=f)

classify<-function(x){
  as.numeric(f(x)>=.5)
}

test$classification<-classify(test$like)
tp<-length(which(test$dec==1 & test$classification==1))
fp<-length(which(test$dec==0 & test$classification==1))
tn<-length(which(test$dec==0 & test$classification==0))
fn<-length(which(test$dec==1 & test$classification==0))
pp<-tp+fp
pn<-tn+fn
p<-tp+fn
n<-tn+fp

M<-matrix(,nrow=3,ncol=3)
colnames(M)<-c('positive','negative','')
rownames(M)<-c('predicted positive','predicted negative','')
#first row
M[1,1]<-tp
M[1,2]<-fp
M[1,3]<-pp
#second row
M[2,1]<-fn
M[2,2]<-tn
M[2,3]<-pn
#third row
M[3,1]<-p
M[3,2]<-n
M[3,3]<-p+n