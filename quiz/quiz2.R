setwd("~/classes/dataAnalysis002")

fUrl <-'http://simplystatistics.tumblr.com/'
con <- url(fUrl, 'r')
simplyStats <-readLines(con)
sapply(c(2,45,122), function(ndx) { nchar(simplyStats[ndx])})
close(con)

fUrl <- "https://spark-public.s3.amazonaws.com/dataanalysis/ss06hid.csv"
download.file(fUrl, 'data/hid.csv', method='curl')

df <- read.csv('data/hid.csv', header=TRUE)

table(df$VAL, useNA='ifany')
sum(df$VAL==24, na.rm=TRUE)
table(df$RMS, df$BDS)

agriculturalLogic <- df$AGS >= 6 & df$ACR >= 3
head(which(agriculturalLogic),3)
indexes <- which(agriculturalLogic)

sdf <- df[indexes,]
sum(is.na(sdf$MRGX))

strsplit(names(df), 'wgtp')[[123]]

fUrl="https://spark-public.s3.amazonaws.com/dataanalysis/ss06pid.csv"
download.file(fUrl, 'data/pid.csv', method='curl')

populationData <- read.csv('data/pid.csv', header=TRUE)
housingData <- read.csv('data/hid.csv', header=TRUE)
mergedData <- merge(housingData, populationData,all=TRUE,by='SERIALNO')
dim(mergedData)

