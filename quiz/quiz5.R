
data(warpbreaks)

wb_aov = aov(breaks ~ wool + tension, data=warpbreaks) 
summary(wb_aov)

p = 0.2
log(p/(1-p))

library(glm2)
data(crabs)
str(crabs)
model <- glm(Satellites ~  Width, data=crabs, family="poisson")
summary(model)
exp(model$coefficients[2])

w <- c(21,22)
sat <- exp(model$coefficients[2]*w + model$coefficients[1])

library(MASS)
data(quine) 
lm1 = lm(log(Days + 2.5) ~.,data=quine)
step(lm1)
