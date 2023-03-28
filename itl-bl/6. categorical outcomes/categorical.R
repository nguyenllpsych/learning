############################################################################################################
#Chapter 6 Analyses: Modeling the Causal Process of Categorical Outcomes
#Note: The code assumes that the data file categorical.csv has been copied to your default directory
############################################################################################################

############################################################################################################
#load the nlme package
library(nlme)
############################################################################################################

############################################################################################################
#Read a csv file containing the data
categorical <- read.csv('categorical.csv')
############################################################################################################

############################################################################################################
#create a upper-level data frame with id and amangcb
catlev2<-aggregate(amangcb ~ id, data=categorical, mean)
############################################################################################################

#Figure 6.2: Female Partner Anger and Male Partner Conflict Report: Raw Data Only
#pdf(file="pconf-by-amanx-panels-raw.pdf", width=14, height=8)
#windows(16,26)
par(mfrow=c(6,6))
#par(mfrow=c(11,6))
for (i in catlev2$id){
  plot(categorical$amang[categorical$id==i], categorical$pconf[categorical$id==i], 
       ylab="Male Conflict", xlab="Female Anger", type="p", pch=1, xlim=c(-.2, 10.2), ylim=c(-.1,1.1), 
       main=paste("id =", i, sep = " "))
}
mtext("Male Partner Conflict by Female Partner Morning Anger", side=3, outer=TRUE, line=-1.2)
#dev.off()

############################################################################################################

#Run random effect logistic model linear growth model with AR(1) errors 
catmodel <- lme(fixed=pconf ~ amang + confcw*relqual + confcb*relqual, data=categorical, random=~1 | id, correlation = corAR1())
summary(catmodel)

#No random effects: AIC 1092 
glm(pconf ~ amangcw + amangcb + lpconfc + time7c, 
     family = binomial, data = categorical)

#Random intercept: AIC 1085
catmodel <- lmer(pconf ~ amangcw + amangcb + lpconfc + time7c +
        (1 | id), family = binomial, data = categorical)
summary(catmodel)

#Specify random intercept and slope. Two cannot be distinguished, correlation is 1.0
#and fit is effectively unchanged: AIC 1084
lmer(pconf ~ amangcw + amangcb + lpconfc + time7c +
       (amangcw | id), family = binomial, data = categorical)


#lmer apprears to ignore the AR1 specification; results are the same
#as specification with it: AIC 1085
lmer(pconf ~ amangcw + amangcb + lpconfc + time7c +
       (1 | id), family = binomial, data = categorical, correlation = corAR1())

############################################################################################################

#Put the EBLUPs of the random intercept into a separate dataet
rint<-ranef(catmodel)
rint$id<-as.numeric(rownames(rint))
#Merge with 
catlev2<-merge(catlev2, rint, all=TRUE, by="id")
names(catlev2)
names(catlev2)[c(3)] <- c("ebint")
catlev2$intercept<-(-1.90219 + catlev2$ebint)
catlev2$slope<-0.21574

############################################################################################################

#Merge upper-level variables with the process data frame
categorical<-merge(categorical, catlev2, all=TRUE, by="id")
#Create predicted values based on within-subject causal model (adjusting for time
#and removing between-subjects variation)
categorical$pred<-(1/(1 + exp(-categorical$intercept - categorical$slope*categorical$amangcw)))
categorical$predM<-(1/(1 + exp(-(-1.90219 + 0.21574*categorical$amangc))))

############################################################################################################

#To create graphs based on observed data only: Sort the dataset by relationship quality, ID, and daily conflict
ordcat<-categorical[order(categorical$id, categorical$amang),]

############################################################################################################

#Figure 6.4: Female Partner Anger and Male Partner Conflict Report: Raw Data and Model Predictions
#Note: Confidence Bounds for Predictions are not Available for lme() models

pdf(file="pconf-by-amanx-panels-pred.pdf", width=16, height=12)
par(mfrow=c(6,11))
for (i in catlev2$id){  
  plot(ordcat$amang[ordcat$id==i], ordcat$pconf[ordcat$id==i], 
       ylab="Male Conflict", xlab="Female Anger", type="p", pch=1, xlim=c(-.2, 10.2), ylim=c(-.1,1.1), 
       main=paste("id =", i, sep = " "))
  lines(ordcat$amang[ordcat$id==i], ordcat$pred[ordcat$id==i]) 
}
mtext("Male Conflict by Female Anger", side=3, outer=TRUE, line=-1.2)
dev.off()


############################################################################################################

#To create graphs based on observed data only: Sort the dataset by female morning anger
ordcat1<-categorical[order(categorical$amangc),]

############################################################################################################
#Figure 6.5: Female Partner Anger and Male Partner Conflict Report: Raw Data and Predictions for Typical Couple
#Note: Confidence Bounds for Predictions are not Available for lme() Models

pdf(file="pconf-by-amanx-panels-avge-pred.pdf", width=6, height=4)
par(mfrow=c(1,1))
plot(jitter(ordcat1$amang, 2), jitter(ordcat1$pconf, .5), 
     ylab="Male Conflict", xlab="Female Anger", type="p", pch=1, xlim=c(-.2, 10.2), ylim=c(-.1,1.1))
lines(ordcat1$amang, ordcat1$predM, col="Red", lwd=4) 
mtext("Male Conflict by Female Anger", side=3, outer=TRUE, line=-1.2)
dev.off()

############################################################################################################

#Spaghetti Plot (Not in Book)
pdf(file="catspag.pdf", width=6, height=4)
par(lwd=.75)
plot(jitter(ordcat1$amang, 2), jitter(ordcat1$pconf, .5), 
     ylab="Male Conflict", xlab="Female Anger", type="n", pch=1, xlim=c(-.2, 10.2), ylim=c(-.1,1.1))
for (i in catlev2$id){
  lines(ordcat$amang[ordcat$id==i], ordcat$pred[ordcat$id==i])
}
  lines(ordcat1$amang, ordcat1$predM, col="Red", lwd=4) 
dev.off()

############################################################################################################



