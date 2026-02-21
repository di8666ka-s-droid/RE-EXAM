library(glmmTMB)
dat=read.table("reexam2025.txt",header=T)
names(dat)

dat$sex <- factor(dat$sex)
dat$age <- factor(dat$age)
dat$cohort <- factor(dat$cohort)

dat$HornMean <- (dat$hornL+dat$hornR)/2
dat$logHorn <- log(dat$HornMean)
dat$logMass <- log(dat$mass)

par(mfrow=c(1,2))
#For males
Males <- subset(dat,sex=="M")

m_Males <- glmmTMB(logHorn~logMass+(1|cohort),
                   data=Males,family=gaussian())
summary(m_Males)

#Convert to normal scale

newdat <- data.frame(
  logMass=seq(min(Males$logMass),max(Males$logMass),length.out=400))

newdat$pred_log <- predict(m_Males,newdata = newdat,re.form=NA)

newdat$mass <- exp(newdat$logMass)
newdat$horn <- exp(newdat$pred_log)

Males$mass <- exp(Males$logMass)
Males$horn <- exp(Males$logHorn)

#plot raw data
plot(Males$mass,Males$horn,pch=16,col="grey",
     xlab="Body Mass(kg)",ylab="Horn length(mm)",
     main="Male Chamois")
#plot predicted data
lines(newdat$mass,newdat$horn,col="blue",lwd=3)
legend("topleft",legend = c("Raw Data","Fitted Data"),
       pch=c(16,NA),lty=c(NA,1),col=c("grey","blue"),bty="n")

#For females
Females <- subset(dat,sex=="F")

m_Females <- glmmTMB(logHorn~logMass+(1|cohort),
                     data=Females,family=gaussian())
summary(m_Females)
#Convert to normal scale

newdat <- data.frame(
  logMass=seq(min(Females$logMass),max(Females$logMass),length.out=400))

newdat$pred_log <- predict(m_Females,newdata = newdat,re.form=NA)

newdat$mass <- exp(newdat$logMass)
newdat$horn <- exp(newdat$pred_log)

Females$mass <- exp(Females$logMass)
Females$horn <- exp(Females$logHorn)

#plot raw data
plot(Females$mass,Females$horn,pch=16,col="grey",
     xlab="Body Mass(kg)",ylab="Horn length(mm)",
     main="Female Chamois")
#plot predicted data
lines(newdat$mass,newdat$horn,col="red",lwd=3)
legend("topleft",legend = c("Raw Data","Fitted Data"),
       pch=c(16,NA),lty=c(NA,1),col=c("grey","red"),bty="n")
