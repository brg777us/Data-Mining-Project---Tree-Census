#Getting data
rm(list = ls())
nytree=read.csv("tree_lr.csv",na.strings=c("", "NA"),header=T)
nytree = na.omit(nytree)

library(tree)
library(dplyr)
names(nytree)
attach(nytree)
library(class)

summary(nytree)
glimpse(nytree)


#*****Logistic regression model******


glm.fit1=glm(status~.,family="binomial",data=nytree)
summary(glm.fit1)
coef(glm.fit1)
exp(coef(glm.fit1))

#Hold Out, 80-20
set.seed(1)
train=sample(nrow(nytree),nrow(nytree)*0.8)
nytree.test=nytree[-train, ]
test.truevalue=status[-train]
glm.fit2=glm(status~.,data=nytree,subset=train,family =binomial)
summary(glm.fit2)
exp(coef(glm.fit2))
glm.probs2=predict(glm.fit2,nytree.test, type="response")
glm.pred2=rep("Alive",136758)
glm.pred2[glm.probs2>(31615/683788)]="DeadorStump"

#logistic Regression - Accuracy and error rate
table(glm.pred2,test.truevalue)
mean(glm.pred2==test.truevalue)
mean(glm.pred2!=test.truevalue)

#5 fold CV
k=5
folds=sample(1:k,nrow(nytree),replace=TRUE)
accuracy=rep(0,k)
for(i in 1:k)
{
    glm.fit3=glm(status~.,family="binomial",data=nytree[folds!=i,])
    nytree.test=nytree[folds==i, ]
    glm.probs3 =predict(glm.fit3,nytree.test, type="response")
    glm.pred3=rep("Alive",nrow(nytree[folds==i,]))
    glm.pred3[glm.probs3>(31615/683788)]="DeadorStump"
    test.truevalue=status[folds==i]
    accuracy[i]=mean(glm.pred3==test.truevalue)
}

#5 fold CV - Accuracy and error rate
mean(accuracy)


#***TREE*****
detach(nytree)
rm(list = ls())
nytree=read.csv("nytree_tree2.csv",na.strings=c("", "NA"),header=T)
nytree = na.omit(nytree)
attach(nytree)
names(nytree)
glimpse(nytree)
summary(nytree)

#1st Model - Hold out 80-20 for classification tree model
library(tree)
set.seed(1)
train=sample(nrow(nytree),nrow(nytree)*0.8)
tree.model=tree(status~.,nytree,subset =train)
nytree.test=nytree[-train,]
status.test=status[-train]

#Pruning tree
cv.model=cv.tree(tree.model,K=10,FUN=prune.misclass)
cv.model

#Selectng level of nodes
prune.model=prune.tree(tree.model,best=4)# size 4 or 2 has the lowest deviance
plot(prune.model)
text(prune.model,pretty=0)

#prediction based on classification tree model
prunetree.pred=predict(prune.model,nytree.test,type="class")

#confusion matrix
table(prunetree.pred,status.test)
mean(prunetree.pred==status.test)
mean(prunetree.pred!=status.test)

