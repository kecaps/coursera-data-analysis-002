
library(ElemStatLearn)
data(SAheart)
set.seed(8484)
train = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2, replace=F)
trainSA = SAheart[train,]
testSA = SAheart[-train,]
logit_model = glm(chd ~ age + alcohol + obesity + tobacco + typea + ldl, data=trainSA, family='binomial')

missClass = function(values, prediction) { sum(((prediction > 0.5)*1) != values)/length(values) }
missClass(trainSA$chd, predict(logit_model, trainSA, type='response'))
missClass(testSA$chd, predict(logit_model, testSA, type='response'))


# install.packages('pgmm')
library(pgmm)
data(olive)
olive = olive[,-1]

library(tree)
predTree = tree(factor(Area) ~ ., data=olive)
plot(predTree)
text(predTree)
newdata = as.data.frame(t(colMeans(olive)))
predict(predTree, newdata)

pruneTree = prune.tree(predTree, best=6)
plot(pruneTree)
text(pruneTree)
newdata = data.frame(Palmitic = 1200, Palmitoleic = 120, Stearic=200,Oleic=7000,Linoleic = 900, Linolenic = 32, Arachidic=60,Eicosenoic=6)
predict(pruneTree, newdata)
predict(pruneTree, newdata,type='class')
