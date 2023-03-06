A = matrix(c(4,3,2,1),nrow=2, ncol=2)
B = matrix(c(2,0,1,3,0,1), nrow=2, ncol=3)
C = matrix(c(1,3,0,2,4,1,4,8,2), nrow=3,ncol=3)
D = matrix(c(3,-1,5,-2),nrow=4, ncol=1)
t(D)%*%D
rankMatrix(C)
library(Matrix)
solve(C)

#2.14
library(alr4)
Heights
set.seed(1000)
n = length(Heights[,1])
train_index <- sample(1:n, size=n*2/3, replace=FALSE)
heights_train = Heights[train_index,]
heights_valid = Heights[-train_index,]
y=heights_valid[,1]
x=heights_valid[,2]
heights_model=lm(y~x)
plot(y~x)
abline(heights_model)
summary(heights_model)
residual=mean((residuals(heights_model))^2)
residual

#2.15
wblake
attach(wblake)
wblake.model = lm(Length~Age)
plot(Length~Age)
abline(wblake.model)
b0 = 65.53
b1 = 30.32
n=length(wblake[,1])
x = wblake[,1]
RSS = sum(residuals(wblake.model)^2)
est_sigma2 = RSS/(n-2)
xebar = mean(x)
sxx = sum((x-xbar)^2)

var_line2 = est_sigma2*((1/n)+((2-xbar)^2)/sxx)
se_line2 = sqrt(var_line2)
pred_length2 = b0+b1*2
t = qt(0.025, n-2, lower.tail = FALSE)
two_inter = c(pred_length2-t*se_line2,pred_length2+t*se_line2)
predict(wblake.model,
        newdata=data.frame(Age=2),
        interval="confidence",
        se.fit=TRUE)

var_line4 = est_sigma2*((1/n)+((4-xbar)^2)/sxx)
se_line4 = sqrt(var_line4)
pred_length4 = b0+b1*4
t = qt(0.025, n-2, lower.tail = FALSE)
four_inter = c(pred_length4-t*se_line4,pred_length4+t*se_line4)
predict(wblake.model,
        newdata=data.frame(Age=4),
        interval="confidence",
        se.fit=TRUE)

var_line6 = est_sigma2*((1/n)+((6-xbar)^2)/sxx)
se_line6 = sqrt(var_line6)
pred_length6 = b0+b1*6
t = qt(0.025, n-2, lower.tail = FALSE)
six_inter = c(pred_length6-t*se_line6,pred_length6+t*se_line6)
predict(wblake.model,
        newdata=data.frame(Age=6),
        interval="confidence",
        se.fit=TRUE)

var_line9 = est_sigma2*((1/n)+((9-xbar)^2)/sxx)
se_line9 = sqrt(var_line9)
pred_length9 = b0+b1*9
t = qt(0.025, n-2, lower.tail = FALSE)
inter9 = c(pred_length9-t*se_line9,pred_length9+t*se_line9)
predict(wblake.model,
        newdata=data.frame(Age=9),
        interval="confidence",
        se.fit=TRUE)



#2.16
UN11
attach(UN11)
x = log(ppgdp)
y = log(fertility)
data = data.frame(x,y)
plot(x,y)
ferpp_model = lm(y~x)
abline(ferpp_model)

n = length(ppgdp)
RSS = sum(residuals(ferpp_model)^2)
est_var = RSS/(n-2)
xbar = mean(x)
sxx = sum((x-xbar)^2)
se = sqrt(est_var/sxx)
b1 = -0.2071
t = -0.20715/se
summary(ferpp_model)
p_value=pt(q=-14.79, n-2, lower.tail = TRUE)
p_value
t.test(ferpp_model, alternative = "less")

y_bar = mean(y)
syy = sum((y-y_bar)^2)
R_squared = (syy-RSS)/syy
x_star = log(1000, base = exp(1))
var_pred = est_var+est_var*((1/n)+((x_star-xbar)^2)/sxx)
se_pred = sqrt(var_pred)
ferpp_model
y_star =-0.2071*x_star+2.6655
t = qt(0.025,n-2, lower.tail = FALSE)
pred_inter = c(y_star-t*se_pred, y_star+t*se_pred)
predict(ferpp_model,
        newdata=data.frame(x=x_star),
        interval="prediction",
        se.fit=TRUE)
exp(0.6258791)
exp(1.843256)

head(UN11)
notlog_model=lm(fertility~ppgdp)
notlog_model
predict(notlog_model,
        newdata=data.frame(ppgdp=1000),
        interval="prediction",
        se.fit=TRUE)

head(UN11)
UN11[fertility==max(fertility),]
UN11[fertility==min(fertility),]
residual_list = residuals(ferpp_model)
df = UN11
UN11$residuals=residuals(ferpp_model)
max1 = max(residuals(ferpp_model))
max2 = max(residuals(ferpp_model)[residuals(ferpp_model)!=max1])
max2
min1 = min(residuals(ferpp_model))
min2 = min(residuals(ferpp_model)[residuals(ferpp_model)!=min1])
min2
UN11[UN11$residuals==min1,]
head(UN11)
df

#3.2
UN11
attach(UN11)
log_ppgdp = log(ppgdp)
data2 = data.frame(fertility, log_ppgdp ,pctUrban)
pairs(data2)
ferpp_model = lm(fertility~log_ppgdp)
plot(fertility~log_ppgdp)
ferpct_model = lm(fertility~pctUrban)
plot(fertility~pctUrban)
abline(ferpct_model)
summary(ferpp_model)
abline(ferpp_model)
ferpct_model = lm(fertility~pctUrban)
summary(ferpct_model)

ferpp_resid = ferpp_model$residuals
pct_pp = lm(pctUrban~log_ppgdp)
pct_pp_resid = pct_pp$residuals
plot(ferpp_resid~pct_pp_resid,xlab="pctUrban~log(ppgdp)", ylab="Fertility~log(ppgdp)")
added_model1 = lm(ferpp_resid~pct_pp_resid)
abline(added_model1)
summary(added_model1)

ferpct_resid = ferpct_model$residuals
pp_pct = lm(log_ppgdp~pctUrban)
pp_pct_resid = pp_pct$residuals
plot(ferpct_resid~pp_pct_resid,xlab="log(ppgdp)~pctUrban", ylab="Fertility~pctUrban")
added_model2 = lm(ferpct_resid~pp_pct_resid)
abline(added_model2)
summary(added_model2)
