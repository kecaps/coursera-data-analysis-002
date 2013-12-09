setwd("~/classes/dataAnalysis002/proj2")

# remote data
rdaUrl <- "https://spark-public.s3.amazonaws.com/dataanalysis/samsungData.rda"
descUrl <- "http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones"

dataDir <- 'data'

if (!file.exists(dataDir)) {
  dir.create(dataDir)
}

getDownloadedFile <- function(url, dir=dataDir) {
  file <- paste(dir, basename(url), sep='/')
  if (!file.exists(file)) {
    download.file(url, file, method='curl')
  }
  invisible(file)
}

rdaFile <- getDownloadedFile(rdaUrl)
load(rdaFile, verbose=TRUE)

str(samsungData)
names(samsungData)

# clean up column names making them syntactic names for ease of use
v <- gsub("\\()",     "",   names(samsungData)) # remove () in names
v <- gsub("-",        ".",  v)                  # remove dashes
v <- gsub("[\\(\\)]", "..", v)                  # remove parantheses
v <- gsub(",",        "_",  v)                  # remove commas
names(samsungData) = v

# clean up data by making subject and activity factors
samsungData$subject = factor(samsungData$subject)
samsungData$activity = factor(samsungData$activity)

subject_ndx <- which(names(samsungData)=='subject')
activity_ndx <- which(names(samsungData)=='activity')
factor_ndx <- c(subject_ndx, activity_ndx)
l <- split(samsungData[,-factor_ndx], samsungData$subject)
l <- lapply(l, function(df) { scale(df) })
ssData <- data.frame(do.call('rbind',l))
names(ssData) = paste0("ss.", names(ssData))
allData = cbind(samsungData, ssData)

reserved_subjects = c(27,28,29,30)
reserved_data <- subset(allData, allData$subject %in% reserved_subjects)
remain_data <- subset(allData, !allData$subject %in% reserved_subjects)
table(remain_data$subject,remain_data$activity)

train_subjects = c(1,3,5,6)
train_subjects = seq(1,10)
test_subjects = seq(11,20)
train_data = subset(remain_data, remain_data$subject %in% train_subjects)
test_data = subset(remain_data, remain_data$subject %in% test_subjects)

d <- train_data
# name cleanup & uniqueness
v <-names(d)
names(d) <- paste0("v", 1:length(v), ".", v)

just.data <- d[,-factor_ndx]
dim(just.data)
standardized <- scale(just.data) # scale and center columns before svd
svd1 <- svd(standardized)
str(svd1)
# svd1$d
trunc_ndx = diff(cumsum(svd1$d^2)/sum(svd1$d^2)) > 0.01
trunc_d = svd1$d[trunc_ndx]
length(trunc_d)
dim(svd1$v)
trunc_v = svd1$v[,trunc_ndx]
boxplot(trunc_v)
stats_v <- apply(trunc_v, 2, boxplot.stats)

sum(sapply(stats_v, function(stats_cv) { any(stats_cv$out == 0) }))
v_ndx = lapply(seq_along(stats_v), 
               function(ndx) { which(trunc_v[,ndx] %in% 
                                       c(stats_v[[ndx]]$out,
                                         stats_v[[ndx]]$stats[c(1,5)]) )})
v_ndx = lapply(seq_along(v_ndx),
               function(ndx) { v_ndx[[ndx]][order(abs(trunc_v[v_ndx[[ndx]],ndx]))] })

# now eliminate those that are highly correlated
trim_correlated = function(df, cols, cor.threshold) {
  corr = cor(df[,cols])
  trimmed_ndxs = c()
  for (ndx in seq_along(cols)) {
    if (all(abs(corr[ndx, trimmed_ndxs]) < cor.threshold)) {
      trimmed_ndxs = c(trimmed_ndxs, ndx)
    }
  }
  return(cols[trimmed_ndxs])
}
cor.threshold=0.6
v_trimmed_ndx = lapply(v_ndx, trim_correlated, df=standardized, cor.threshold=cor.threshold)
all_v_trimmed_ndx = unique(c(v_trimmed_ndx, recursive=TRUE))

reduced_ndx = trim_correlated(all_v_trimmed_ndx, df=standardized, cor.threshold=cor.threshold)
reduced_variables = dimnames(standardized)[[2]][reduced_ndx]
length(reduced_variables)

plot(train_data[,reduced_ndx[1:5]],col=train_data$activity, pch=19)

library(tree)
tr <- tree(training_data$activity ~., training_data[,reduced_ndx])
plot(tr)
text(tr)

pred_activity <- predict(tr, training_data, type="class")
acc <- sum(training_data$activity == pred_activity)/length(pred_activity)
acc

pred_activity <- predict(tr, test_data, type='class')
acc <- sum(test_data$activity == pred_activity)/length(pred_activity)
acc

misclass.tree(tr, detail=TRUE)
dim(test_data)
dim(training_data)

tree_accuracy <- function(tr, data) {
  pr <- predict(tr, data, type='class')
  sum(data$activity == pr)/nrow(data)
}

which.max(sapply(seq(2,14), USE.NAMES=TRUE, function(x) { tree_accuracy(prune.tree(tr, best=x), test_data)}))
prune.tree(tr, best=14)

library(randomForest)
rf = randomForest(activity ~., data=train_data[-subject_ndx], 
                  importance=TRUE, proximity=TRUE, keep.inbag=TRUE,
                  do.trace=TRUE)

tree_accuracy(rf, train_data)
tree_accuracy(rf, test_data)

tree_accuracy(rf, reserved_data)

misclass_ndx = test_data$activity != predict(rf, test_data)
length(misclass_ndx)

mc_probs=cbind(test_data$activity, predict(rf, test_data, type='prob'))[misclass_ndx,]
mc_err = max(mc_prob[,2:ncol(mc_prob)])-apply(mc_prob, 1, function(r) { r[r[1]+1] })

library(RColorBrewer)
colors = c(1, brewer.pal(ncol(rf$err.rate)-1, 'Set1'))
matplot(1:rf$ntree, 1+rf$err.rate, type='l', log='y', lty=1, col=colors)
legend('topright', lty=1, col=colors, text.col=colors, legend=dimnames(rf$err.rate)[[2]])

varImpPlot(rf, n.var=15, type=1, pch=19,col='blue')
rf_imp = importance(rf)
order_imp = apply(rf_imp, 2, order, decreasing=TRUE)
vars_imp = dimnames(rf_imp)[[1]][apply(rf_imp, 2, which.max)]
order_imp[1:5,]

partialPlot(rf, train_data, x.var=vars_imp[1], which.class=dimnames(rf_imp)[[2]][1])

plot(train_data[,dimnames(rf_imp)[[1]][c(603,615,1121)]], col=brewer.pal(9, 'Set1')[train_data$activity])

l = tapply(1:nrow(samsungData), samsungData$subject, function(ndx) { scale(samsungData[ndx,1:(ncol(samsungData)-2)]) }, simplify=FALSE)
l = aggregate(samsungData[,1:(ncol(samsungData)-2)], by=samsungData$subject, scale)

l = split(samsungData[,1:(ncol(samsungData)-2)], samsungData$subject)
l = lapply(l, function(df) { scale(df) })
sdSubjScaled <- data.frame(do.call("rbind", l))
names(sdSubjScaled) = paste0("subj_scaled.",names(sdSubjScaled))
training_data_subj_scaled = subset(sdSubjScaled, samsungData$subject %in% training_subjects)

all_training_data = cbind(training_data, training_data_subj_scaled)
rf_all = randomForest(activity ~., data=all_training_data[-subject_ndx], 
                      importance=TRUE, proximity=TRUE, keep.inbag=TRUE,
                      do.trace=TRUE)


x = seq(1,100000)
y = lapply(seq(1,500), function(abc) { sample(x, length(x)*2/3, replace=TRUE)})
xs <- x
for (s in y) {
  xs <- setdiff(xs, s)
  print(length(xs))
}
rem <- sapply(y, function(yy) { length(setdiff(x,yy)) })

norm_err = apply(samsungData[-factor_ndx], 2,
                 function(col) { sum( (qnorm((1:99)/100, mean=mean(col), sd=sd(col)) - quantile(col, (1:99)/100))^2)/var(col) })

pv <- apply(samsungData[,-factor_ndx],2,
      function(col) { shapiro.test(sample(col, size=5000,replace=TRUE))$p.value })



cvRandomForest = function(x, f, cv.fold=5, keep.forest=TRUE, ...) {
  n <- length(f)
  nlvl <- table(f)
  idx <- numeric(n)
  for (i in 1:length(nlvl)) {
    idx[which(f == levels(f)[i])] <-  sample(rep(1:cv.fold, length=nlvl[i]))
  }
  rfs = vector("list", cv.fold)
  for (i in 1:cv.fold) {
    xtest=x[idx==i, , drop=FALSE]
    ytest=f[idx==i]
    rfs[[i]] <- randomForest(x=x[idx!=i, , drop=FALSE], 
                             y=f[idx!=i], 
                             xtest=xtest, ytest=ytest,
                             keep.forest=keep.forest, ...)
    rfs[[i]]$test$x = xtest
    rfs[[i]]$test$y = ytest
  }
  return(rfs)
}

rfs = cvRandomForest(train_data[,1:(min(factor_ndx)-1)], train_data$activity)
all_rfs = cvRandomForest(train_data[,-factor_ndx], train_data$activity)

mean(sapply(rfs, function(rf) { rf$test$err.rate[500,1]}))
mean(sapply(all_rfs, function(rf) { rf$test$err.rate[500,1]}))
rfs[[1]]$test

# cut a factor into parts trying to keep the size of cuts
# roughly the same
cut_factor <- function(f, num_cuts) {
  f_counts <- table(f)
  f_order <- order(f_counts, decreasing=TRUE)
  
  f_cuts <- vector('numeric', nlevels(f))
  cut_buckets <- vector('numeric', num_cuts)
  cut_names <- vector('list', num_cuts)
  for (f_ndx in f_order) {
    cnt <- f_counts[f_ndx]
    if (cnt == 0) { 
      break
    }
    b_ndx = which.min(cut_buckets)
    f_cuts[f_ndx] <- b_ndx
    cut_buckets[b_ndx] <- cut_buckets[b_ndx] + cnt
    cut_names[[b_ndx]] <- c(cut_names[[b_ndx]], names(f_counts)[f_ndx])
  }
  cut_names = sapply(cut_names, function(names) { paste(collapse=",", sort(names)) })
  factor(cut_names)[f_cuts][f]
}

# 
cvRandomForest = function(x, f, cv.fold=5, keep.forest=TRUE, ...) {
  idx <- numeric(n)
  if (is.factor(cv.fold)) {
    idx <- as.numeric(cv.fold)
    cv.fold <- nlevels(cv.fold)    
  } else {
    n <- length(f)
    nlvl <- table(f)
    for (i in 1:length(nlvl)) {
      idx[which(f == levels(f)[i])] <-  sample(rep(1:cv.fold, length=nlvl[i]))
    }
  }
  rfs = vector("list", cv.fold)
  for (i in 1:cv.fold) {
    xtest=x[idx==i, , drop=FALSE]
    ytest=f[idx==i]
    rfs[[i]] <- randomForest(x=x[idx!=i, , drop=FALSE], 
                             y=f[idx!=i], 
                             xtest=xtest, ytest=ytest,
                             keep.forest=keep.forest, ...)
    rfs[[i]]$test$x = xtest
    rfs[[i]]$test$y = ytest
  }
  return(rfs)
}

subject_cut = cut_factor(train_data$subject, 5)
rfs = cvRandomForest(train_data[,1:(min(factor_ndx)-1)], train_data$activity, subject_cut)
all_rfs = cvRandomForest(train_data[,-factor_ndx], train_data$activity, subject_cut)

mean(sapply(rfs, function(rf) { rf$test$err.rate[500,1]}))
mean(sapply(all_rfs, function(rf) { rf$test$err.rate[500,1]}))

# pull out the final error rate from the test
rfTestErrRate = function(rf) { 
  err.rate = rf$test$err.rate
  err.rate[nrow(err.rate),1]
}

# return the mean test error rate across all random forests in the list
rfsTestErrRate = function(rfs) { 
  errRates = sapply(rfs, rfTestErrRate)
  testLens = sapply(rfs, function(rf) { length(rf$test$predicted) })
  sum(errRates * testLens)/sum(testLens)
}

rfsTestErrRate(rfs)
rfsTestErrRate(all_rfs)

sapply(rfs, rfTestErrRate)
sapply(rfs, function(rf) { 1-mean(predict(rf, rf$test$x)==rf$test$y)})

cvNVarRandomForests <- function(x,y, rfs=NULL, cv.fold=NULL, scale="log", step=0.5,
                                mtry=function(p) max(1, floor(sqrt(p))), recursive=FALSE,
                                ...) {
  p = ncol(x)
  if (scale == "log") {
    k <- floor(log(p, base=1/step))
    n.var <- round(p * step^(0:(k-1)))
    same <- diff(n.var) == 0
    if (any(same)) n.var <- n.var[-which(same)]
    if (! 1 %in% n.var) n.var <- c(n.var, 1)
  } else {
    n.var <- seq(from=p, to=1, by=step)
  }
  k <- length(n.var)
  rfsSet <- vector(k, mode="list")
  attrs <- vector(k, mode="list")

  attrs[[1]] = seq(p)
  if (is.null(rfs)) {
    rfs = cvRandomForests(x,y,cv.fold, importance=TRUE,mtry=mtry(ncol(x)),...)
  }
  if (is.null(cv.fold)) {
    cv.fold = length(rfs)
  }
  rfsSet[[1]] = rfs
  for (i in 2:k) {
    attrOrder = bestOrder(lapply(rfsSet[[i-1]], function(rf) { rf$importance[,ncol(rf$importance)] }),
                          decreasing=TRUE)
    attrs[[i]] = attrs[[i-1]][attrOrder[1:n.var[i]]]
    rfsSet[[i]] = cvRandomForest(x[,attrs[[i]], drop=FALSE],y,cv.fold,importance=TRUE,mtry=mtry(n.var[i]),...)
  }
  error.cv <- sapply(rfsSet, rfsTestErrRate)
  names(error.cv) <- names(rfsSet) <- names(attrs) <- n.var
  list(n.var=n.var, error.cv=error.cv, attrs=attrs, rfsSet=rfs)
}

bestOrder <- function(sets, ...) {
  ranks = sapply(sets, rank)
  order(rowSums(ranks), ...)
}

bestOrder2 <- function(sets, decreasing=FALSE,...) {
  ranks = sapply(sets, rank)
  bo <- rep(0, length=length(ranks))
  for (ndx in seq_along(bo)) {
    best <- order(rowSums(ranks), decreasing=decreasing, ...)[1]
    for (c_ndx in seq(ncol(ranks))) {
      idx = ranks[,c_ndx] > ranks[best,c_ndx]
      ranks[idx,c_ndx] = ranks[idx,c_ndx]-1
    }
    ranks[best,c_ndx] = ifelse(decreasing,-Inf,Inf)
    bo[ndx] = best
  }
  bo
}

  pos = matrix(0, nrow=length(sets), ncol=max(sapply(sets, length)))
  
}