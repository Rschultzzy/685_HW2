# Stat Methods I
# 2020 Fall 
# Class 7

library(faraway)


########################################
#Slides 4,5
#A perfect simple linear regression model for n=1000 sample data

id<-c(1:1000)
set.seed(312)
x<-rnorm(1000,15,5)
set.seed(135)
e<-rnorm(1000,0,4)
y<-1+3*x+e

perfect<-as.data.frame(cbind(id,y,x))

mod_perfect<-lm(y~x,perfect)


summary(mod_perfect)
summary(mod_perfect)$sigma
shapiro.test(resid(mod_perfect))
qqnorm(resid(mod_perfect))

anova(mod_perfect)

jpeg("C:/Users/sungheel/Box Sync/2020 Stat Methods 1/LaTeX/Class7_PerfectRegression.jpg", 
     width = 450, height = 450)
plot(perfect$x,perfect$y,pch=18,col="blue",cex=.7,
     main="Perfect Linear Regression Model (n=1000)")
abline(lm(y~x,perfect),col="red",lty=4,lwd=2)
dev.off() 



########################################
#Slide 8

data(cheddar, package="faraway")
#Data description available from https://cran.r-project.org/web/packages/faraway/faraway.pdf

summary(cheddar)
cheddar$H2S_O<-exp(cheddar$H2S)
summary(cheddar$H2S);summary(cheddar$H2S_O)

mod_cheddar_O<-lm(taste~Acetic+H2S_O+Lactic,cheddar)
summary(mod_cheddar_O)

#Brief Check on the Distribution of the residuals
#Numeric \hat{\epsilon}
summary(resid(mod_cheddar_O))   #-->Mean? Constant Variance? Normal Distribution?
#Graphic \hat{\epsilon}
hist(resid(mod_cheddar_O))      #-->Mean? Constant Variance? Normal Distribution?
plot(density(resid(mod_cheddar_O)),
     main="Density Plot")       #-->Mean? Constant Variance? Normal Distribution?

#Graphic: Zero mean and constant variance
plot(fitted(mod_cheddar_O),resid(mod_cheddar_O))
abline(h=0,col="blue")
#Numeric: Constant variance
cor(fitted(mod_cheddar_O),resid(mod_cheddar_O))
summary(lm(resid(mod_cheddar_O)~fitted(mod_cheddar_O)))
var.test(resid(mod_cheddar_O)[fitted(mod_cheddar_O)<=20],
         resid(mod_cheddar_O)[fitted(mod_cheddar_O)>20])

plot(fitted(mod_cheddar_O),sqrt(abs(resid(mod_cheddar_O))))
cor(fitted(mod_cheddar_O),sqrt(abs(resid(mod_cheddar_O))))

plot(cheddar$Acetic,resid(mod_cheddar_O))
cor(cheddar$Acetic,resid(mod_cheddar_O))

plot(cheddar$H2S_O,resid(mod_cheddar_O))
cor(cheddar$H2S_O,resid(mod_cheddar_O))
summary(cheddar)
var.test(resid(mod_cheddar_O)[cheddar$H2S_O<=207],
         resid(mod_cheddar_O)[cheddar$H2S_O>207])

plot(cheddar$Lactic,resid(mod_cheddar_O))
cor(cheddar$Lactic,resid(mod_cheddar_O))

#Graphic: Normal distribution
qqnorm(resid(mod_cheddar_O))
#NUmeric: Normal distribution
shapiro.test(resid(mod_cheddar_O))

#What we you conclude about the residuals? 
#How about inferences based on the output of summary(mod_cheddar_O)?


##########################################################
#Take-Home:  Try these residual diagnostics on
#lm(taste~Acetic+H2S+Lactic,cheddar)

mod_cheddar<-lm(taste~Acetic+H2S+Lactic,cheddar)
summary(mod_cheddar)

#Brief Check on the Distribution of the residuals
#Numeric \hat{\epsilon}
summary(resid(mod_cheddar))   #-->Mean? Constant Variance? Normal Distribution?
#Graphic \hat{\epsilon}
hist(resid(mod_cheddar))      #-->Mean? Constant Variance? Normal Distribution?
plot(density(resid(mod_cheddar)),
     main="Density Plot")       #-->Mean? Constant Variance? Normal Distribution?

#Graphic: Zero mean and constant variance
plot(fitted(mod_cheddar),resid(mod_cheddar)) 
abline(h=0,col="blue")
#Numeric: Constant variance
cor(fitted(mod_cheddar),resid(mod_cheddar))
summary(lm(resid(mod_cheddar)~fitted(mod_cheddar)))
var.test(resid(mod_cheddar)[fitted(mod_cheddar)<=20],resid(mod_cheddar)[fitted(mod_cheddar)>20])
#How is the graphical approach useful? How about numeric/statistical approaches?

plot(fitted(mod_cheddar),sqrt(abs(resid(mod_cheddar))))
cor(fitted(mod_cheddar),sqrt(abs(resid(mod_cheddar))))

plot(cheddar$Acetic,resid(mod_cheddar))
cor(cheddar$Acetic,resid(mod_cheddar))

plot(cheddar$H2S_O,resid(mod_cheddar))
cor(cheddar$H2S_O,resid(mod_cheddar))

plot(cheddar$Lactic,resid(mod_cheddar))
cor(cheddar$Lactic,resid(mod_cheddar))

#Graphic: Normal distribution
qqnorm(resid(mod_cheddar))
#NUmeric: Normal distribution
shapiro.test(resid(mod_cheddar))

#What we you conclude about the residuals?
#How about inferences based on the output of summary(mod_cheddar)?




########################################
#Slides 9,10 
#Data for the perfect model with an addition of new observations
summary(perfect)

x<-c("id","y","x")
new1<-as.data.frame(setNames(
  list(c(1001:1020),
       c(sample(c(56:60),10,rep=T),sample(c(41:45),10,rep=T)),
       c(sample(5:10,10,rep=T),sample(25:30,10,rep=T))),x))
perfect_new1<-rbind(perfect,new1)


jpeg("C:/Users/sungheel/Box Sync/2020 Stat Methods 1/LaTeX/Class7_PerfectNew1Regression.jpg", 
     width = 450, height = 450)
plot(perfect_new1$x,perfect_new1$y,pch=18,col="blue",cex=.7,
     main="perfect + new1 (n=1020)")
abline(lm(y~x,perfect),col="red",lty=4,lwd=2)
abline(lm(y~x,perfect_new1),col="black",lwd=3)
dev.off() 


new2<-as.data.frame(setNames(
  list(c(1021:1040),
       c(sample(c(180:200),20,rep=T)),
       c(sample(c(40:50),20,rep=T))),x))
perfect_new2<-rbind(perfect,new2)

new3<-as.data.frame(setNames(
  list(c(1041:1060),
       c(sample(c(180:200),20,rep=T)),
       c(sample(c(10:20),20,rep=T))),x))
perfect_new3<-rbind(perfect,new3)

new4<-as.data.frame(setNames(
  list(c(1061:1080),
       c(sample(c(40:60),20,rep=T)),
       c(sample(c(40:50),20,rep=T))),x))
perfect_new4<-rbind(perfect,new4)
 


jpeg("C:/Users/sungheel/Box Sync/2020 Stat Methods 1/LaTeX/Class7_PerfectNew234Regression.jpg", 
     width = 600, height = 300)
par(mfrow=c(1,3))

plot(perfect_new2$x,perfect_new2$y,pch=18,col="blue",cex=.8,
     main="perfect + new2 (n=1020)")
abline(lm(y~x,perfect),col="red",lty=4,lwd=2)
abline(lm(y~x,perfect_new2),col="black",lwd=3)


plot(perfect_new3$x,perfect_new3$y,pch=18,col="blue",cex=.8,
     main="perfect + new3 (n=1020)")
abline(lm(y~x,perfect),col="red",lty=4,lwd=2)
abline(lm(y~x,perfect_new3),col="black",lwd=3)

plot(perfect_new4$x,perfect_new4$y,pch=18,col="blue",cex=.8,
     main="perfect + new4 (n=1020)",
     xlim=50,ylim=200)
abline(lm(y~x,perfect),col="red",lty=4,lwd=2)
abline(lm(y~x,perfect_new4),col="black",lwd=3)
dev.off() 





########################################
#Slides 14,15

#Leverage
hat<-hatvalues(mod_cheddar_O)
sum(hat)
mean(hat)
hat>2*mean(hat)
#Half-normal plot of leverage
halfnorm(hat)
#Below, you will get the same plot as above 
#but creating id may be helpful if rownames exist
#cheddarid<-row.names(cheddar)
#halfnorm(hat,labs=cheddarid,ylab="h_i")
cheddar[24,]


#Standardized residuals
rstandard(mod_cheddar_O)
rstandard(mod_cheddar_O)[which.max(abs(rstandard(mod_cheddar_O)))]
qt(.05/2,30-4)
qqnorm(rstandard(mod_cheddar_O))
abline(0,1,col="red")


########################################
#Slides 17

#Studentized residuals
rstudent(mod_cheddar_O)
rstudent(mod_cheddar_O)[which.max(abs(rstudent(mod_cheddar_O)))]
qt(.05/(2*30),30-4)


########################################
#Slides 19
cooks.distance(mod_cheddar_O)
halfnorm(cooks.distance(mod_cheddar_O))
cheddar[15,]
cheddar[30,]
summary(cheddar)


plot(mod_cheddar_O)
#What do we conclude about the cheddar data? 


########################################
#Slides 20
#Checking relationship structure

#Partial Regression
rd_taste<-residuals(lm(taste~Acetic+Lactic,cheddar)) #--> What would rd_taste mean?
rd_H2SO<-residuals(lm(H2S_O~Acetic+Lactic,cheddar))  #--> What would rd_H2SO mean?
#Partial Regression Plot
plot(rd_H2SO,rd_taste) 
abline(0,coef(mod_cheddar_O)['H2S_O']) #-->Is the relationship linear?

#Partial Residual Plot
termplot(mod_cheddar_O,partial.resid=T, terms=2)#-->Is the relationship linear?

#Regression on subseted data
mod1<-lm(taste~Acetic+H2S_O+Lactic,subset(cheddar,H2S_O<=207))
mod2<-lm(taste~Acetic+H2S_O+Lactic,subset(cheddar,H2S_O>207))
coef(mod1); coef(mod2); coef(mod_cheddar_O) #--> What do you observe in the estimated coef of H2S_O in these models?


##########################################
#Group Discussion
#Error Distribution
## Zero mean 
## Constant variance
## Normal distribution

#Unusual observations
## Anything to remove?

#Relationship structure
## Do H2S_O have a linear relationship with the outcome variable?
