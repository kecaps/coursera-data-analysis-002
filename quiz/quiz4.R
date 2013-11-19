setwd("~/classes/dataAnalysis002")


url = "https://spark-public.s3.amazonaws.com/dataanalysis/movies.txt"
download.file(url, "data/movies.txt", method="curl")

movies <- read.table("data/movies.txt", sep="\t", header=TRUE, quote="")
str(movies)

movies$X <- as.character(movies$X)
model <- lm(score ~ box.office, data=movies)
summary(model)

summary(movies$box.office)
confint(model, level=.9)

model <- lm(score ~ box.office + running.time, data=movies)
summary(model)

par(mfrow=c(1,1))
plot(movies$running.time, movies$score, pch=19, col='blue')
movies2 <- movies[-which(movies$running.time > 200),]
model2 <- lm(score ~ box.office + running.time, data=movies2)
summary(model2)
# plot(movies2$running.time, movies2$score, pch=19, col='blue')


model <- lm(score ~ rating + running.time + rating*running.time, data=movies)
summary(model)
model$coefficients['running.time'] + model$coefficients['ratingPG:running.time']

data(warpbreaks)
model <- lm(warpbreaks$breaks ~ relevel(warpbreaks$tension, 'M'))
summary(model)
confint(model)
