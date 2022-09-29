#https://datacadamia.com/data/set
#https://datacadamia.com/data_mining/weather#data_set  golf dataset

#S??n??fland??rma ID3, C4.5 ??cretli (J48 ??cretsiz java), 

install.packages("C50")
library(C50)
playgolf<-read.csv("C:/Users/golf.csv",sep=";")
x=playgolf[,-5]
y=playgolf$Play
#sonuc <- C5.0(x = playgolf[, -5], y = playgolf$Play)
sonuc<-C5.0(x,y)
sonuc
summary(sonuc)
plot(sonuc,type=c("simple"))

#CART Algoritmas??
install.packages("rpart")
library(rpart)
sonuc<-rpart(Play ~ Outlook + Temperature + Humidity + Windy, method = "class",data = playgolf,
             parms = list(split='gini'),control = rpart.control(minsplit = 1))
plot(sonuc,uniform = TRUE,main="Decision Tree - Play?")
text(sonuc, use.n = TRUE, all=TRUE,cex=.8)

#CHAID algoritmas??
install.packages("CHAID")
library(CHAID)
#kredi datas?? yok
#veri<-read.csv("C:/veri.csv,sep=";")
chaid_control(alpha2=0.05,alpha3=-1,alpha4=0.05,minsplit=20,minbucket=7,minprob=0.01)
A<-chaid(Kredi~Cinsiyet+Medeni+Yas, data=veri, control=chaid_control())
plot(A)

#ZeroR y??ntemi
ZeroR<-function(X,targetId) {
  if(is.character(X[,targetId])|is.factor(X[,targetId]))
  {
    u.x<-unique(X[,targetId])
    u.x.temp<-c()
    for (i in u.x) {
      u.x.temp<-c(u.x.temp,sum(X[,targetId]==i))
    }
    names(u.x.temp)<-u.x
    return(c(max(u.x.temp),names(u.x.temp)[which.max(u.x.temp)]))
  }  
    return(NULL)
}

ZeroR(playgolf,5)

#OneR y??ntemi
install.packages("OneR")
library(OneR)
model<-OneR(formula=Play~.,data=playgolf,verbose=TRUE)
summary(model)

#Bayes
install.packages("naivebayes")
library(naivebayes)
nb<-naive_bayes(x,y,prior = NULL,laplace = 0,usekernel = FALSE)
plot(nb,ask = TRUE)
head(predict(nb,c("sunny","hot","normal","false"),type="prob"))
predict(nb,c("sunny","hot","normal","false"))

#k kom??u y??ntemi
install.packages("class")
library(class)
Ornek<-read.csv("C:/Users/Ornek.csv",sep=";")
x<-Ornek[,-3]
y<-Ornek$Y
knn(x,c(8,4),y,k=3,prob=TRUE)

#Birliktelik Kural??
install.packages("arules")
library(arules)
tv<-read.csv("C:/Users/tv.csv",sep=";")
rules<-apriori(tv,parameter = list(supp=0.5,conf=0.9,target="rules"))
inspect(rules)

#K??meleme Y??ntemleri
install.packages("cluster")
library(cluster)
#kmeans kortalamalar y??ntemi
x<-matrix(c(4,10,3,2,5,6,7,9,11,3,10,2,4,4,8,3,9,7,7,6),10,2)
kmeans(x,2)

#Birle??tirici Hiyerar??ik K??meleme AGNES
x<-read.delim("C:/Users/x.txt",row.names =1)
dv<-agnes(x,diss = FALSE,metric="euclidean",stand = FALSE)
plot(dv)

#DIANA y??ntemi
dv<-diana(x,diss = FALSE,metric="euclidean",stand = FALSE)
plot(dv)

#DBSCAN y??ntemi
install.packages("DBSCAN")
library(DBSCAN)
res<-dbscan(x,eps = 20,minPts=2)
plot(x,col=res$cluster,cex=2,pch=res$cluster)

#K??meleme Kalitesi Se??imi
install.packages("clusterCrit")
library(clusterCrit)
x<-matrix(c(4,10,3,2,5,6,7,9,11,3,10,2,4,4,8,3,9,7,7,6),10,2)
cl<-kmeans(x,2)
#C Index
intCriteria(x,cl$cluster,c("C_index"))

#Calinski_Harabasz
intCriteria(x,cl$cluster,c("Calinski_Harabasz"))

#Tüm Ölçütler
#Calinski_Harabasz
intCriteria(x,cl$cluster,c("all"))









