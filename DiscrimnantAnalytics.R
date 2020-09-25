
mydata = read.csv(file.choose(), header = TRUE)
attach(mydata)
names(mydata)
X=mydata[,2:3]
Y=mydata[,4]
install.packages("DiscriMiner")
library(DiscriMiner)
Fisher<-desDA(X,Y)


## ---------------- Z score of Altman----- DescrimnantAnalytics-----------------------
mydata <-read.csv(file.choose(), header = TRUE)
attach(mydata)
names(mydata)
install.packages("Mass")
library(DiscriMiner)
X<-mydata[,2:6]
Y<-mydata[,7]
Fisher <-desDA(X,Y)
Fisher

Fisher$power
X1 <-as.matrix(mydata[,2:6])
Y1 <-as.vector(mydata[,7])
Manova = manova(X1~Y1,data=mydata)
summary(Manova,test="Wilks")
summary.aov(Manova)
Mahalanobis<-linDA(X,Y)
Mahalanobis
Scores = data.frame((Mahalanobis$scores))
Scores
write.csv(Scores,file="F:/PosteriorProb.csv")
##------------------------------------------

disData <-read.csv(file.choose(),header = TRUE)
attach(disData)
names(disData)
X2<-disData[,2:4]
Y2<-disData[,1]
DiscrimAna <-desDA(X2,Y2)
DiscrimAna
DiscrimAna$power
X3 <-as.matrix(disData[,2:4])
Y3<-as.matrix(disData[,1])
Manova1 <-manova(X3~Y3,data=disData)
summary(Manova1,test = "Wilks")
summary.aov(Manova1)
Mahalanobis1 <-linDA(X2,Y2)
Mahalanobis1
Scores1 <-data.frame(Mahalanobis1$scores)
Scores1
write.csv(Scores1,file="F:/disDataMah.csv")
install.packages("MASS")
library(MASS)
attach(disData)
X <-as.matrix(disData[,2:4])
Y <- as.matrix(disData[,1])
Jackknife <- lda(Y~X,cv=TRUE)
confusiontable = table(Community,Jackknife)
