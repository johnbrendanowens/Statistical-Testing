#Part 1
data("Cars93")
attach(Cars93)

#1.A

w.usa = subset(Width, Origin == "USA" )
w.nonusa = subset(Width, Origin =="non-USA")
shapiro.test(w.usa)
shapiro.test(w.nonusa)
par(mfrow = c(1, 2))
hist(w.usa, prob=TRUE)
lines(density(w.usa))
hist(w.nonusa, prob=TRUE)
lines(density(w.nonusa))
par(mfrow = c(1, 1))

boxplot(w.usa, w.nonusa, las=1, names = c("USA", "Non-USA"), xlab = "Origins",
        ylab = "Width",
        main = "Width between Cars of USA and Non-USA Origin")

t.test(Width~Origin, alternative = "two.sided")

t.test(Width~Origin, alternative = "greater")

#1.B
log.price = log10(Price) #get Log Price
#Correlation, Plot w/ Regression Line, and Regression Eqt/Prediction for Horsepower
hp.price.cor = cor(Horsepower, log.price)
hp.price.cor
plot (Horsepower, log.price, main="Scatterplot of HorsePower vs. Log Price", 
      ylab=" LogPrice ", xlab="HorsePower")
abline(lm(log.price~Horsepower), col="red") # regression line (y~x) 

HP.price.reg = lm(log.price~Horsepower)
summary(HP.price.reg)
lprice.150  = predict(HP.price.reg, data.frame(Horsepower=150))
lprice.150
actualprice.150 = 10^lprice.150
actualprice.150
Cars93[which(Horsepower==150),]
#Correlation, Plot w/ Regression Line, and Regression Eqt/Prediction for MPG City
mpg.price.cor = cor(MPG.city, log.price)
mpg.price.cor
plot (MPG.city, log.price, main="Scatterplot of MPG City vs. Log Price", 
      ylab="Price ", xlab="MPG City")
abline(lm(log.price~MPG.city), col="red") # regression line (y~x) 

MPG.price.reg = lm(log.price~MPG.city)
summary(MPG.price.reg)
lprice.20 = predict(MPG.price.reg, data.frame(MPG.city=20))
lprice.20
actualprice.20 = 10^lprice.20
actualprice.20
Cars93[which(MPG.city==20),]
#1.C
p.usa = subset (Passengers, Origin == "USA")
p.nonusa = subset (Passengers, Origin == "non-USA")
shapiro.test(p.usa)
shapiro.test(p.nonusa)
par(mfrow = c(1, 2))
hist(p.usa, prob=TRUE)
lines(density(p.usa))
hist(p.nonusa, prob=TRUE)
lines(density(p.nonusa))

t.test(Passengers~Origin, alternative ="two.sided")

wilcox.test(Passengers~Origin, conf.int=T, conf.level = 0.95)  

#1.D
p = 1/6
p
type.table = table(Type)
type.table
type.chitest = chisq.test(type.table, p = c(p,p,p,p,p,p))
type.chitest
type.chitest$expected
difference= type.chitest$expected-type.table
difference

detach(Cars93)