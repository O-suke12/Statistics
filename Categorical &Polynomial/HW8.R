library(alr4)
cakes
attach(cakes)
model = lm(Y~X1+X2+I(X1^2)+I(X2^2)+I(X1*X2))
summary(model)
library(rgl)
scatter3d(X1,Y,X2)

factor(block)
model_block = lm(Y~X1+X2+block*X1+block*X2+I(X1^2)+I(X2^2)+I(X1*X2))
summary(model_block)
refine_model = lm(Y~X1+X2+I(X1^2)+I(X2^2)+I(X1*X2))
summary(refine_model)



head(MinnLand)
attach(MinnLand)
year = as.factor(year)
model_a = lm(log(acrePrice)~year+region)
model_b = lm(log(acrePrice)~year+region+year:region)
summary(model_a)
summary(model_b)

effect_int = effect(c("year*region"), model_b)
plot(effect_int)

financing = as.factor(financing)
model_b = lm(log(acrePrice)~year+region+year:region+financing)
summary(model_b)
confint(model_b)

model = lm(acrePrice~financing)
summary(model)
plot(acrePrice~financing)




domedata
attach(domedata)
Cond = as.factor(Cond)
is.factor(Cond)
model = lm(Dist~Velocity+Angle+BallWt+BallDia+Cond+Velocity:Cond)
summary(model)
plot(Dist~Velocity)
abline(model)
effect_int = effect(c("Cond*Velocity"), model)
plot(effect_int)



x <- c(0, 0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 0, 0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 0, 0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4)
y <- c(9.8, 16.075, 17.7, 17.675, 19.75, 18.175, 18.95, 22.575, 27.3, 12.05, 13.825, 17.95, 19.925, 18.75, 18.425, 16.95, 22.575, 26.8, 12.05, 15.825, 17.45, 19.925, 17.25, 18.175, 19.2, 22.075, 27.05)
model = lm(y~poly(x,8))
summary(model)
pf(62.23, 8,18,lower.tail = FALSE)

xseq = seq(0,4, length=1000)
yhat = predict(model, newdata=data.frame(x=xseq))
plot(x,y)
lines(xseq, yhat, col=2, lwd=2)




x_new <- c(0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2, 2.25, 2.5, 2.75)
y_new <-
  c(9.2, 9.5175, 9.91, 9.9975, 10.12, 10.08, 10.1, 10.08, 10.18, 10.12, 10.08)
plot(x_new,y_new,pch=19)
library(splines)
spline_reg <- lm(y_new ~ bs(x_new, df=8))
summary(spline_reg)
#
# Plot the curve
#
xseq <- seq(0.25, 3.0, length=1000)
yhat <- predict(spline_reg,
                newdata=data.frame(x_new=xseq))
plot(x_new, y_new, pch=19)
lines(xseq, yhat, col=2, lwd=2)
