####################################
#######Classifier Project###########
######Logistic Regression###########
###########LDA,QDA,KNN##############
########Stock Market Analysis#######
##########Xiaowei Cheng#############
####################################

#preparation
library(ISLR)
library(MASS)
library(class)

#get to know with the dataset
names(Smarket)
dim(Smarket)
head(Smarket)
summary(Smarket)
pairs(Smarket)

#check correlation
cor(Smarket[,-9])
#little correlation between today and previous day return
#substantial correlationa is between Year and Volume

#plot Volume
plot(Smarket$Volume)
#Volume increased from 2001 to 2005

#############################
#Logistic Regression

#training set
train<-(Smarket$Year<2005)
Smarket.2005<-Smarket[!train,]
dim(Smarket.2005)
Direction.2005<-Smarket$Direction[!train]

#fit model
glm.fit<-glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,data=Smarket,family=binomial,subset=train)
summary(glm.fit)
glm.probs<-predict(glm.fit,Smarket.2005,type="response")

#setup Up and Down
glm.pred<-rep("Down",252)
glm.pred[glm.probs>.5]="Up"

#confusion matrix
table(glm.pred,Direction.2005)

#model accuracy
mean(glm.pred==Direction.2005)
mean(glm.pred!=Direction.2005)
#result is very bad
#p-value is large as well

####################
#Linear Discriminant Analysis(LDA)

#fit moel
lda.fit<-lda(Direction~Lag1+Lag2,data=Smarket,subset=train)
lda.fit

#predict model
lda.pred<-predict(lda.fit,Smarket.2005)
lda.class<-lda.pred$class

#confusion matrix
table(lda.class,Direction.2005)

#accuracy
mean(lda.class==Direction.2005)
#56%
#not very good result as well

################################
#Quadratic Discriminant Analysis(QDA)

#fit model
qda.fit<-qda(Direction~Lag1+Lag2,data=Smarket,subset=train)
qda.fit

#predict
qda.class<-predict(qda.fit,Smarket.2005)$class

#confusion matrix
table(qda.class,Direction.2005)

#accuracy
mean(qda.class==Direction.2005)
#60%

#############################
#K-Nearest Neighbors(KNN)

#tran and test
train.X<-cbind(Smarket$Lag1,Smarket$Lag2)[train,]
test.X<-cbind(Smarket$lag1,Smarket$Lag2)[!train,]
train.Direction<-Smarket$Direction[train]

set.seed(1)
#model 1 k=1
knn.pred<-knn(data.frame(train.X),data.frame(test.X),train.Direction,k=1)
talbe(knn.pred,Direction.2005)
mean(knn.pred==Direction.2005)

#model 2 k=3
knn.pred<-knn(data.frame(train.X),data.frame(test.X),train.Direction,k=3)
talbe(knn.pred,Direction.2005)
mean(knn.pred==Direction.2005)

#both result is not relativley good
###################################
#Conclusion
#Overall, the QDA model has the best accuracy and best performance


