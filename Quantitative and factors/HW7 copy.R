library(alr4)
grades = read.csv("/Users/RS/Downloads/grades.txt", sep=" ")
head(grades)
attach(grades)
scatterplot(GPA~ ACT|Major, smooth=F, reg.line=F)

Major = as.factor(Major)
is.factor(Major)
model = lm(GPA~ACT+Major+ACT*Major)
summary(model)
abline(model)
abline(a=3.226318-1.649577, b=0.062245-0.002757)

grades_mj = grades[grades$Major == 1, 1:3]
attach(grades_mj)
mj_model = lm(GPA~ ACT)
summary(mj_model)

predict(mj_model, newdata = data.frame(ACT=20), interval = "confidence")
predict(mj_model, newdata = data.frame(ACT=30), interval = "confidence")
grades_nmj = grades[grades$Major == 0, 1:3]
attach(grades_nmj)
nmj_model = lm(GPA~ ACT)
summary(nmj_model)
predict(nmj_model, newdata = data.frame(ACT=20), interval = "confidence")
predict(nmj_model, newdata = data.frame(ACT=30), interval = "confidence")
abline(a=35.07880+13.32748  , b= 1.05895-0.18463 )


head(BGSall)
attach(BGSall)
scatterplot(HT18~HT9|Sex, smooth=F, reg.line=F)
Sex = as.factor(Sex)
is.factor(Sex)
model = lm(HT18~HT9+Sex+HT9*Sex)
summary(model)
predict(model, newdata = data.frame(ACT=20, Sex=0), interval = "confidence")
predict(model, newdata = data.frame(ACT=30, Sex), interval = "confidence")


salary
head(salary)
attach(salary)
scatterplot(salary~year,smooth=F)
scatterplot(salary~ysdeg, smooth=F)
degree = as.factor(degree)
sex = as.factor(sex)
rank = as.factor(rank)
boxplot(salary~sex*rank*degree)

sex_model = lm(salary~sex)
summary(sex_model)
ciboxplot(salary~sex)

t.test(salary~sex, var.equal = FALSE)
confint(model)
