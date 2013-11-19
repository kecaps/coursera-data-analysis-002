setwd("~/classes/dataAnalysis002/proj1")

# remote data
dataCsvUrl <- "https://spark-public.s3.amazonaws.com/dataanalysis/loansData.csv"
dataRdaUrl <- "https://spark-public.s3.amazonaws.com/dataanalysis/loansData.rda"
codebookUrl <- "https://spark-public.s3.amazonaws.com/dataanalysis/loansCodebook.pdf"
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

getDownloadedFile(dataCsvUrl)
getDownloadedFile(codebookUrl)

dataRdaFile <- getDownloadedFile(dataRdaUrl)
load(dataRdaFile, verbose=TRUE)
# loads loansData

dim(loansData)
names(loansData)
str(loansData)
table(loansData$FICO.Range, useNA='ifany')

##########
# munge data
##########
# store off unmunged data
rawData = loansData

loansData$Interest.Rate = as.numeric(sub('%$','', as.character(loansData$Interest.Rate)))
loansData$Debt.To.Income.Ratio = as.numeric(sub('%$','', as.character(loansData$Debt.To.Income.Ratio)))

ficoRangeMeans <- sapply(lapply(strsplit(levels(loansData$FICO.Range), '-'), as.numeric), mean)
loansData$FICO <- ficoRangeMeans[unclass(loansData$FICO.Range)]
loansData$FICO.Scale <- (loansData$FICO - 300) / (850-300)
# loansData$Loan.Length <- as.integer(sub(" months$", "", unclass(loansData$Loan.Length)))

empLengthLevels = levels(loansData$Employment.Length)
empLengthNumeric = as.numeric(gsub('n/a', '0',
                                   gsub('^< 1$','0.1', 
                                   gsub('\\+? years?$', '', 
                                        empLengthLevels))))
loansData$Employment.Length.Ordered = ordered(loansData$Employment.Length, 
                                              levels(reorder(empLengthLevels, empLengthNumeric)))
loansData$Employment.Length = empLengthNumeric[loansData$Employment.Length]


##########
# scatter plots
##########

plot(jitter(loansData$FICO, amount=1), loansData$Interest.Rate, col=loansData$Loan.Length, pch=19, cex=0.6)
legend("topright", legend=levels(loansData$Loan.Length)[-1], col=2:nlevels(loansData$Loan.Length), pch=10, cex=0.6)

plot(loansData$Amount.Requested, loansData$Interest.Rate, col=loansData$Loan.Length, pch=19, cex=0.6)

plot(loansData$Amount.Requested, loansData$Interest.Rate, col=loansData$Loan.Length, pch=19, cex=0.6)

##########
# evaluate different models
##########

modelResidual <- function(data, m, f=function(x) { x }) {
  if (!is.null(m$na.action)) {
    data=data[-1*m$na.action]
  }
  sum((data - f(m$fitted.values))^2)
}

# univariate models for Interest.Rate
intRateNdx = which(names(loansData)=='Interest.Rate')

models = lapply(loansData[,-intRateNdx],
                function(x) { lm(loansData$Interest.Rate ~ x) })
names(models) = names(loansData)[-intRateNdx]
anovas = lapply(models, anova)
residuals <- sapply(anovas, function(a) { a[["Sum Sq"]][length(a[["Sum Sq"]])] }, USE.NAMES=TRUE)
anovas <- anovas[order(residuals)]
models <- models[order(residuals)]
(residuals <- sort(residuals))

logModels = lapply(loansData[,-intRateNdx],
                   function(x) { lm(log(loansData$Interest.Rate) ~ x) })
names(logModels) <- names(loansData)[-intRateNdx]
logResiduals <- sapply(logModels, 
                       function(m) {  modelResidual(loansData$Interest.Rate, m, exp) })
logModels <- logModels[order(logResiduals)]
(logResiduals <- sort(logResiduals))

numericData <- loansData[,-intRateNdx]
numericData <- numericData[,sapply(numericData, is.numeric)]
numericData <- numericData[,sapply(numericData, function(vals) { sum(vals <= 0, na.rm=T)==0 })]
logLogModels = lapply(numericData,
                      function(x) { lm(log(loansData$Interest.Rate) ~ log(x)) })
names(logLogModels) <- paste("log", names(numericData))
logLogResiduals <- sapply(logLogModels, 
                       function(m) {  modelResidual(loansData$Interest.Rate, m, exp) })
logLogModels <- logLogModels[order(logLogResiduals)]
(logLogResiduals <- sort(logLogResiduals))


# multivariate models for Interest.Rate including FICO

ficoNdx = grep("^FICO", names(loansData))
data <- loansData[,-1*c(intRateNdx, ficoNdx)]
models = lapply(data,
                function(x) { lm(loansData$Interest.Rate ~ loansData$FICO + x) })
names(models) = names(data)
anovas = lapply(models, anova)
residuals <- sapply(anovas, function(a) { a[["Sum Sq"]][length(a[["Sum Sq"]])] }, USE.NAMES=TRUE)
anovas <- anovas[order(residuals)]
models <- models[order(residuals)]
(residuals <- sort(residuals))

invFico = loansData$FICO.Scale^-3
ficoNdx = grep("^FICO", names(loansData))
data <- loansData[,-1*c(intRateNdx, ficoNdx)]
models = lapply(data,
                function(x) { lm(loansData$Interest.Rate ~ invFico + x) })
names(models) = names(data)
anovas = lapply(models, anova)
residuals <- sapply(anovas, function(a) { a[["Sum Sq"]][length(a[["Sum Sq"]])] }, USE.NAMES=TRUE)
anovas <- anovas[order(residuals)]
models <- models[order(residuals)]
(residuals <- sort(residuals))

data <- data[,-grep("Loan.Length", names(loansData))]
models = lapply(data,
                function(x) { lm(loansData$Interest.Rate ~ invFico + loansData$Loan.Length + x) })
names(models) = names(data)
anovas = lapply(models, anova)
residuals <- sapply(anovas, function(a) { a[["Sum Sq"]][length(a[["Sum Sq"]])] }, USE.NAMES=TRUE)
anovas <- anovas[order(residuals)]
models <- models[order(residuals)]
(residuals <- sort(residuals))

data <- data[,-grep("Amount.Requested", names(loansData))]
models = lapply(data,
                function(x) { lm(loansData$Interest.Rate ~ invFico + loansData$Loan.Length + loansData$Amount.Requested + x) })
names(models) = names(data)
anovas = lapply(models, anova)
residuals <- sapply(anovas, function(a) { a[["Sum Sq"]][length(a[["Sum Sq"]])] }, USE.NAMES=TRUE)
anovas <- anovas[order(residuals)]
models <- models[order(residuals)]
(residuals <- sort(residuals))


ficoNdx = grep("^FICO", names(loansData))
logModels = lapply(loansData[,-1*c(intRateNdx,ficoNdx)],
                function(x) { lm(log(loansData$Interest.Rate) ~ loansData$FICO + x) })
names(logModels) = names(loansData)[-1*c(intRateNdx, ficoNdx)]
logAnovas = lapply(logModels, anova)
logResiduals <- sapply(logAnovas, function(a) { a[["Sum Sq"]][length(a[["Sum Sq"]])] })
logAnovas <- logAnovas[order(logResiduals)]
logModels <- logModels[order(logResiduals)]
(logResiduals <- sort(logResiduals))
expLogResiduals <- sapply(logModels, function(m) { sum((loansData$Interest.Rate - exp(m$fitted.values))^2)})

residuals <- sapply(logModels, function(m) { sum((loansData$Interest.Rate[-1*c(m$na.action)] - exp(m$fitted.values))^2) })
logModels <- logModels[order(residuals)]
(residuals <- sort(residuals))


modelResidual <- function(data, m, f=function(x) { x }) {
  if (!is.null(m$na.action)) {
    data=data[-1*m$na.action]
  }
  sum((data - f(m$fitted.values))^2)
}

model <- lm(Interest.Rate ~ FICO + Loan.Length + FICO*Loan.Length + Amount.Requested + Amount.Requested*Loan.Length, data=loansData)
modelResidual(loansData$Interest.Rate, model)
model2 <- lm(Interest.Rate ~ FICO + Loan.Length + Amount.Requested, data=loansData)
modelResidual(loansData$Interest.Rate, model2)

logModel <- lm(log(Interest.Rate) ~ FICO + Loan.Length + FICO*Loan.Length + Amount.Requested + Amount.Requested*Loan.Length, data=loansData)
modelResidual(loansData$Interest.Rate, logModel, exp)
logModel2 <- lm(log(Interest.Rate) ~ FICO + Loan.Length + Amount.Requested, data=loansData)
modelResidual(loansData$Interest.Rate, logModel2, exp)

model <- lm(Interest.Rate ~ log(FICO) + Loan.Length + log(FICO)*Loan.Length + log(Amount.Requested) + log(Amount.Requested)*Loan.Length, data=loansData)
modelResidual(loansData$Interest.Rate, model)
model2 <- lm(Interest.Rate ~ log(FICO) + Loan.Length + Amount.Requested, data=loansData)
modelResidual(loansData$Interest.Rate, model2)

plot(jitter(loansData$FICO, amount=1), loansData$Interest.Rate, col=loansData$Loan.Length, pch=19, cex=0.6)
legend("topright", legend=levels(loansData$Loan.Length)[-1], col=2:nlevels(loansData$Loan.Length), pch=10, cex=0.6)

m <- lm(Interest.Rate ~ FICO, data=loansData)
lines(loansData$FICO, m$fitted.values, col='black')
m <- lm(Interest.Rate ~ FICO + Loan.Length, data=loansData)
modelPoints <- data.frame(x=loansData$FICO, y=m$fitted.values)
modelPoints <- split(modelPoints, loansData$Loan.Length)
lines(modelPoints[[2]]$x, modelPoints[[2]]$y, col='pink')
lines(modelPoints[[3]]$x, modelPoints[[3]]$y, col='blue')

(m <- lm(log(Interest.Rate) ~ FICO + Loan.Length + Amount.Requested, data=loansData))

par(mfrow=c(1,3))
plot(jitter(loansData$FICO, amount=1), loansData$Interest.Rate, col='blue', pch=19, cex=0.6)
fico <- sort(loansData$FICO)

coefRatio = table(loansData$Loan.Length)[3]/sum(!is.na(loansData$Loan.Length))
rate <- m$coefficients[1] + fico*m$coefficients[2] + coefRatio*m$coefficients[3]
lines(fico, exp(rate), lwd=2, col='black')

residuals = loansData$Interest.Rate - exp(rate)
plot(1:nrow(loansData), residuals, col=loansData$Loan.Length, pch=19, cex=0.6)
abline(h=exp(-coefRatio*m$coefficients[3]), col='blue')
abline(h=exp((1-coefRatio)*m$coefficients[3]), col='brown')


plot(jitter(loansData$FICO, amount=1), loansData$Interest.Rate, col=loansData$Loan.Length, 
     pch=19, cex=0.6)
legend("topright", legend=levels(loansData$Loan.Length)[-1], col=2:nlevels(loansData$Loan.Length), pch=10, cex=0.6)



listFico <- split(loansData$FICO, loansData$Loan.Length)
fico <- sort(listFico[[2]])
y = m$coefficients[1] + m$coefficients[2]*fico
lines(fico, exp(y), col='green')
fico <- sort(listFico[[3]])
y = m$coefficients[1] + m$coefficients[2]*fico + m$coefficients[3]
lines(fico, exp(y), col='pink')

plot(1:nrow(loansData), loansData$Interest.Rate, pch=19)


model <- lm(log(Interest.Rate) ~ FICO + Loan.Length + Amount.Requested, data=loansData)
plot(jitter(loansData$FICO), loansData$Interest.Rate, col=loansData$Loan.Length, pch=19, cex=0.6)
loanLengthFico = split(loansData$FICO, loansData$Loan.Length)
loanLengthFico = lapply(loanLengthFico, sort)
fitted = model$coefficients[1] + model$coefficients[2]*loanLengthFico[[2]]
lines(loanLengthFico[[2]], exp(fitted), col='blue')
fitted = model$coefficients[1] + model$coefficients[2]*loanLengthFico[[3]] + model$coefficients[3]
lines(loanLengthFico[[3]], exp(fitted), col='blue')

fitted = model$coefficients[1] + model$coefficients[2]*loansData$FICO + model$coefficients[3] * (loansData$Loan.Length == '60 months')
residuals = loansData$Interest.Rate - exp(fitted)
plot(loansData$Amount.Requested, residuals, col='blue', pch=19, cex=0.6)


png(file="loanFinal.png", height=480, width=3*480)
invFico <- loansData$FICO.Scale^-3
model <- lm(loansData$Interest.Rate ~ invFico)
par(mfrow=c(1,3))
plot(jitter(loansData$FICO), loansData$Interest.Rate, col='blue', pch=19,
     cex=0.6, cex.lab=1.3, cex.axis=1.3,
     xlab='FICO score', ylab='Interest Rate (%)')
lines(sort(loansData$FICO), model$fitted.values[order(loansData$FICO)])
mtext(text='(a)', side=3, line=1)

plot(loansData$Amount.Requested, model$residuals, col=loansData$Loan.Length, pch=19, 
     cex=0.6, cex.lab=1.3, cex.axis=1.3,
     xlab='Amount Requested ($)', ylab='Residual Interest Rate (%)')
legend('top', legend=c('Length of loan:',levels(loansData$Loan.Length)[-1]), 
       col=c(0, 2:nlevels(loansData$Loan.Length)), pch=19)
mtext(text='(b)', side=3, line=1)


model2 <- lm(loansData$Interest.Rate ~ invFico + loansData$Loan.Length + loansData$Amount.Requested)

plot(loansData$Amount.Requested, model2$residuals, col=loansData$Loan.Length, pch=19, 
     cex=0.6, cex.lab=1.3, cex.axis=1.3,
     xlab='Amount Requested ($)', ylab='Residual Interest Rate (%)')
legend('topleft', legend=c('Length of loan:',levels(loansData$Loan.Length)[-1]), 
           col=c(0, 2:nlevels(loansData$Loan.Length)), pch=19)

mtext(text='(c)', side=3, line=1)

dev.off()
ar = loansData$Amount.Requested/1000

model2 <- lm(loansData$Interest.Rate ~ invFico + loansData$Loan.Length + ar)
summary(model2)
confint(model2)

