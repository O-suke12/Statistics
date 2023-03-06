library(alr4)
Mitchell
attach(Mitchell)
plot(Month, Temp, asp=4)
title("Month & Soil Temperture")

oldfaith
attach(oldfaith)
plot(Duration, Interval)
title("Eruption Duration & Interval")

water
pairs(water)
title("Water runoff in the Sierras")

Htwt
attach(Htwt)
plot(ht, wt)
sxx = 472.09
syy = 731.96
sxy = 274.79
xbar = 165.52
ybar = 59.47
b1 = sxy/sxx
b0 = ybar - b1*xbar
plot(ht, wt)
abline(a=b0, b=b1)    

rss=syy-(sxy^2)/sxx
variance=rss/8
covariance=-variance*(xbar/sxx)
varb1 = variance/sxx
varb0 = variance*(1/10+(xbar^2)/sxx)
seb0 = sqrt(varb0)
seb1 = sqrt(varb1)
t0 = b0/seb0
t1 = b1/seb1
weight.reg = lm(wt~ht, data=Htwt)
summary(weight.reg)
2*pt(-0.572, 8)
2*pt(-1.496, 8)
