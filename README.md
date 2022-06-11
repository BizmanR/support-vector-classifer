# support-vector-classifer
getwd()
setwd("C:/Users/CEO/Desktop/sample_project")

library(ISLR)
library(gmodels)
sample_data <- OJ
View(sample_data)
summary(OJ)
attach(OJ)

mydat = OJ[,c(1,11,12)]

# SUPPORT VECTOR CLASSIFIER
library(e1071)
# Support vector classifier is a classification method. Let Direction as the response variable. Try cost = 0.0001, 0.001, 0.01, 0.1, 1, 10
plot(mydat$SalePriceMM,mydat$SalePriceCH , col=factor(mydat$Purchase))


library(e1071)
set.seed(0)
classifier = tune(svm, Purchase~ SalePriceMM+SalePriceCH, data= mydat,kernel = "linear",
          range = list(cost=c(0.01,0.1,1,5,10)))
bestmodel = classifier$best.model
summary(bestmodel)

plot(bestmodel,mydat)
vc<- table(predict(bestmodel), mydat$Purchase)

#Accuracy
acc_vc <- sum(diag(vc))/sum(vc)
acc_vc


# SUPPORT VECTOR MACHINE
classifier2 = tune(svm, Purchase~ SalePriceMM+SalePriceCH, data= mydat,kernel = "radial",
                  range = list(cost=c(0.01,0.1,1,5,10), gamma= c(0.1,1,5)))
bestmodel2 = classifier2$best.model
summary(bestmodel2)

plot(bestmodel2,mydat)
vm<-table(predict(bestmodel2), mydat$Purchase)
acc_vm <- sum(diag(vm))/sum(vm)
acc_vm


# K- MEANS

km = kmeans(mydat[,c(2,3)],2,nstart = 20)
#km$cluster
plot(mydat[,c(2,3)], col = km$cluster)
k <- table(km$cluster, mydat$Purchase)
acc_k <- sum(diag(k))/sum(k)
acc_k


# Hierarchical Clustering
hc = hclust(dist(mydat[,c(2,3)]), method = "complete")
plot(hc)
h<- table(cutree(hc,2), mydat$Purchase)
acc_h <- sum(diag(h))/sum(h)
acc_h



# PCA
pca = prcomp(mydat[,c(2,3)])
summary(pca)
biplot(pca)


pdata = predict(pca, mydat[,c(2,3)])
km = kmeans(pdata[,2],2,nstart = 20)
plot(mydat[,c(2,3)], col = km$cluster)


pca1<- table(km$cluster, mydat$Purchase)
acc_pca1 <- sum(diag(pca1))/sum(pca1)
acc_pca1



pdata = predict(pca, mydat[,c(2,3)])
km = kmeans(pdata,3,nstart = 20)
plot(mydat[,c(2,3)], col = km$cluster)


pca2<-table(km$cluster, mydat$Purchase)
acc_pca2 <- sum(diag(pca2))/sum(pca2)
acc_pca2


accuracy <- data.frame(acc_vc, acc_vm, acc_k, acc_h, acc_pca1, acc_pca2)
accuracy
