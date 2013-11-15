setwd("~/classes/dataAnalysis002")
#install.packages('ElemStatLearn')
library(ElemStatLearn)
data(marketing)
plot(bone$age,bone$spnbmd,pch=19,col=((bone$gender=="male")+1))
boxplot(marketing$Income ~ marketing$Marital,col="grey",xaxt="n",ylab="Income",xlab="")
axis(side=1,at=1:5,labels=c("Married","Living together/not married","Divorced or separated","Widowed","Nevermarried"),las=2)

boxplot(marketing$Income ~ marketing$Marital,col="grey",xaxt="n",ylab="Income",xlab="", varwidth=TRUE)
axis(side=1,at=1:5,labels=c("Married","Living together/not married","Divorced or separated","Widowed","Nevermarried"),las=2)

library(datasets)
data(iris)
irisSubset = iris[,1:4]
irisClust=hclust(dist(irisSubset))
plot(irisClust)
abline(h=3, col='red', lwd=2)

fUrl = 'https://spark-public.s3.amazonaws.com/dataanalysis/quiz3question4.rda'
download.file(fUrl, "data/qz3q4.rda", method='curl')
load("data/qz3q4.rda", verbose=TRUE)
plot(dataSet$x, dataSet$y)
km=kmeans(dataSet, centers=2, iter.max=10, nstart=5)
plot(dataSet$x, dataSet$y, col=km$cluster)

library(ElemStatLearn)
data(zip.train)
# Create an image matrix for the 3rd row, which is a 4
im = zip2image(zip.train,3)
image(im)

inspectImage <- function(zip.train, ndx) {
  par(mfrow=c(2,3))
  im = zip2image(zip.train, ndx)
  image(im)
  imSvd = svd(im)
  plot(imSvd$u[,1], nrow(imSvd$u):1, xlab="Rows", ylab="First left singular vector", pch=19)
  plot(imSvd$v[,1], xlab="Columns", ylab="First right singular vector", pch=19)

  plot(imSvd$d,xlab="Column",ylab="Singluar value",pch=19)
  plot(imSvd$d^2/sum(imSvd$d^2),xlab="Column",ylab="Percent of variance explained",pch=19)
  imSvd$d[1]^2/sum(imSvd$d^2)
}
inspectImage(zip.train, 3)

inspectImage(zip.train, 8)
inspectImage(zip.train, 18)
