library(alr4)

UBSprices
attach(UBSprices)
plot(bigmac2003,bigmac2009)
abline(a=0, b=1)
bigmac.model = lm(bigmac2009~bigmac2003)
abline(bigmac.model)
bigmac_y = log(bigmac2009)
bigmac_x = log(bigmac2003)
plot(bigmac_x, bigmac_y)
bigmac_log.model = lm(bigmac_y~bigmac_x, )
bigmac_log.model
abline(bigmac_log.model)
summary((bigmac_log.model))

RSS = sum(residuals(bigmac_log.model)^2)
n = length(bigmac_x)
est_var2 =RSS/(n-2) 
xbar = mean(bigmac_x)
sxx = sum((bigmac_x-xbar)^2)
seb1 = sqrt(est_var2/sxx)
b1 = 0.80293 
t = qt(0.005, n-2, lower.tail = FALSE)
b1_inter = c(b1-t*seb1, b1+t*seb1)
confint(bigmac_log.model, level=0.99)
b1_inter
ybar = mean(bigmac_y)
syy = sum((bigmac_y-ybar)^2)
Rsquare = (syy-RSS)/syy
Rsquare 

pairs(ftcollinstemp)
attach(ftcollinstemp)
temp_model = lm(winter~fall)
plot(fall, winter)
abline(temp_model)
summary(temp_model)
RSS = sum(residuals(temp_model)^2)
n = length(fall)
est_sigma2 = RSS/(n-2)
xbar = mean(fall)
sxx = sum((fall-xbar)^2)
seb1 = sqrt(est_sigma2/sxx)
t = 0.3132/seb1
pval = 2*pt(2.049,n-2, lower.tail = FALSE)

#2.13.1
Heights
attach(Heights)
plot(mheight, dheight)
height.model = lm(dheight~mheight)
abline(height.model)
b0=29.91744 
b1=0.54175
height.model
length(mheight)
xbar = mean(mheight)
ybar = mean(dheight)

syy = sum((dheight-ybar)^2)
RSS = sum(residuals(height.model)^2)
R2 = (syy-RSS)/syy

n = length(mheight)
est_variance = RSS/(n-2)

sxx = sum((mheight-xbar)^2)
seb1 = sqrt(est_variance/sxx)
seb0 = sqrt(est_variance+((1/n)+(mean(dheight)^2)/sxx))
summary(height.model)

#2.13.2
t = qt(0.005, df=n-2, lower.tail = FALSE)
con_interval_b1 = c(b1-t*seb1, b1+t*seb1)
con_interval_b1
confint(height.model, level=0.99)

#2.13.3
pred_daughter = 29.9174 + 0.5417*64
var_pred = est_variance+est_variance*((1/n)+((64-xbar)^2)/sxx)
se_pred = sqrt(var_pred)
t = qt(0.005, df=n-2, lower.tail = FALSE)
pred_interval = c(pred_daughter-t*se_pred, pred_daughter+t*se_pred)
predict(height.model, 
        newdata = data.frame(mheight=64),
        interval = "prediction",
        se.fit = TRUE)
pred_interval



