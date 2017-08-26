data<-read.csv('Speed Dating Data.csv')
data<-data[c('dec','attr','sinc','intel','fun','amb','shar','like')]
data$dec<-factor(data$dec,levels=c(0,1))

set.seed(1)
indices<-sample(8378,4189,replace=FALSE)
train<-data[indices,]
test<-data[-indices,]

dataTree<-tree(dec~.,data=train,split='gini') 
#gini is the most common classifcation foro tree
#another one we used last year: entropy
plot(dataTree)
text(dataTree)

cv.tree(dataTree, FUN=prune.misclass)

pruneDataTree<-prune.tree(dataTree,method='misclass',best=3)
plot(pruneDataTree)
text(pruneDataTree)

test$prediction<-predict(pruneDataTree,newdata = test,type='class')
length(which(test$prediction!=test$dec))
1000/4189
