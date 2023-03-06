library(alr4)

#Problem1
head(water)
attach(water)
df = data.frame(BSAAM, OPBPC, OPRC, OPSLAKE)
pairs(df)
water_model = lm(BSAAM~OPBPC+OPRC+OPSLAKE)
cor(df)
summary(water_model)

#Problem2
y <- c(1.5, 2.0, 2.3, 3.2, 3.8)
x1 <- c(2,5,9,8,11)
x2 <- c(16,12,9,4,6)
cbind(y,x1,x2)


#Problem3
df = read.table('/Users/RS/Downloads/abrasion.txt', header = T,,sep='')
attach(df)
df
pairs(df)
cor(df)
model = lm(y~x1+x2)
summary(model)
n=length(df[,1])
RSS = sum(residuals(modle)^2)
sigma2 = RSS/(n-3)
sigma = sqrt(sigma2)
ones<-rep(1,n)
var_matrix= sigma2*solve(t(cbind(ones,x1,x2))%*%cbind(ones,x1,x2))

b1 = -6.5708
x1bar = mean(x1)
varb1 = sigma2/sum((x1-x1bar)^2)
seb1 = sqrt(varb1)
seb1=sqrt(var_matrix[2,2])
t1 = b1/seb1
2*pt(abs(t1), df=n-3, lower.tail = FALSE)

b2 = -1.3743
x2bar = mean(x2)
varb2 = sigma2/sum((x2-x2bar)^2)
seb2 = sqrt(varb2)
seb2=sqrt(var_matrix[3,3])
t2 = b2/seb2
2*pt(abs(t2), df=n-3, lower.tail = FALSE)

predict(model, 
        newdata=data.frame(x1=70, x2=200), 
        interval="confidence", 
        se.fit=TRUE) 

predict(model, 
        newdata=data.frame(x1=70, x2=200), 
        interval="prediction", 
        se.fit=TRUE) 


