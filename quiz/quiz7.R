set.seed(53535)
xValues = seq(0,2*pi,length=100)
yValues = rnorm(100) + sin(xValues)

library(splines)

nsList = lapply(1:10, ns, x=xValues)
models = lapply(seq_along(nsList), function(ndx) { lm(yValues ~ nsList[[ndx]]) })

rmse = function(m) { sqrt(mean(m$residuals^2)) }

nsRmse = sapply(models, rmse)
diff(nsRmse)
which.min(diff(nsRmse))

library(simpleboot)
data(airquality)
attach(airquality)
quantile(Wind, .75)
set.seed(883833)
x <- one.boot(Wind, quantile, 1000, probs=.75)
sd(x$t)
plot(density(x$t), lwd=3, col='blue')
abline(v=quantile(Wind, .75), col='red')


library(tree)
data(Cars93,package="MASS")
set.seed(7363)
trees <- replicate(3, tree(DriveTrain ~ Price + Type, data=Cars93[sample(1:dim(Cars93)[1],replace=TRUE),]), simplify=FALSE)
newdata = data.frame(Type = 'Large', Price=20)
sapply(trees, predict, newdata=newdata, type='class')
tree1 = tree(DriveTrain ~ Price + Type, data=Cars93[sample(1:dim(Cars93)[1],replace=TRUE),])
tree2 = tree(DriveTrain ~ Price + Type, data=Cars93[sample(1:dim(Cars93)[1],replace=TRUE),])
tree3 = tree(DriveTrain ~ Price + Type, data=Cars93[sample(1:dim(Cars93)[1],replace=TRUE),])
predict(tree1, newdata=newdata, type='class')
predict(tree2, newdata=newdata, type='class')
predict(tree3, newdata=newdata, type='class')


library(ElemStatLearn)
data(vowel.train)
data(vowel.test) 
set.seed(33833)

library(devtools)
install_github("medley","mewo2")
install.packages('e1071')

library(ElemStatLearn)
library(randomForest)
library(medley)
library(e1071)
data(vowel.train)
data(vowel.test) 
vowel.train$y = factor(vowel.train$y)
vowel.test$y = factor(vowel.test$y)
set.seed(33833)
rf = randomForest(y ~ ., data=vowel.train)
svm1 <- svm(y ~ ., data=vowel.train)
1-mean(vowel.test$y == predict(rf, vowel.test))
1-mean(vowel.test$y == predict(svm1, vowel.test))
agree_idx <- predict(rf, vowel.test) == predict(svm1, vowel.test)
1-mean(vowel.test[agree_idx,]$y == predict(svm1, vowel.test[agree_idx,]))
