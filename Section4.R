library(alr4)

#Problem one
property = read.csv("/Users/RS/Downloads/property.csv")
property
attach(property)
pro_model = lm(Price~Imprv+Area)
summary(pro_model)

cor(Price, pro_model$fitted.values)^2



#Problem two
head(UN11)



#Problem three
body = read.csv("/Users/RS/Downloads/body.csv")
pairs(body)
cor(body)


attach(body)
body_model = lm(bodyfat~triceps+thigh+midarm)
summary(body_model)
body_model2 = lm(bodyfat~triceps+midarm)
summary(body_model2)

triceps2 = triceps-midarm
thigh2 = thigh-midarm
body_model3 = lm(bodyfat~thigh)
summary(body_model3)




#Problem four
Hubble = read.csv("/Users/RS/Downloads/Hubble.csv")
Hubble
attach(Hubble)
hubble.slr = lm(velocity~distance, data=Hubble)
plot(Hubble)
summary(hubble.slr)
abline(hubble.slr)

hubble.noint = lm(velocity~ -1+distance)
summary(hubble.noint)
abline(hubble.noint)
plot(distance, velocity, pch=16)
abline(c(0, hubble.noint$coefficients), lty=1)
abline(hubble.slr$coefficients, lty=2)
legend(0,1000,c("origin reg", "SLR"), lty=1:2)
