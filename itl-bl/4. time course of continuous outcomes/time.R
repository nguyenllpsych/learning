#######################################################
# Chapter 4                                          ##
# Modeling the Time Course of Continuous Outcomes    ##
# File in repository: time.csv                       ##
# Authors: Niall Bolger and Jean-Philippe Laurenceau ##
#######################################################

# META -------------------------------------------------------------------------
#load the nlme package
library(nlme)
library(grDevices) # for windows()

#Read a csv file containing the data
# id:        subjects 1 to 50
# time:      assessment times 0 to 15 per participant
# intimacy:  questionnaire ratings 0 to 10
# treatment: grouping variable 0 or 1
# time01:    standardized time divided by 15 -> range from 0 to 1
time <- read.csv('time.csv')

# DESCRIPTIVES -----------------------------------------------------------------
#Figure 4.2: Panel plots for Control Group
# different panel for each participant
# organized in 5x5 matrix
par(mfcol=c(5,5))
for (i in 1:25){
  plot(time$time[time$id==i], time$intimacy[time$id==i], 
       ylab = "intimacy", xlab="time", 
       main = paste("id =", i, sep = " "),
       # o for both lines and points overplotted
       type="o", pch=4, 
       ylim=c(0,8))
}
mtext("Control Group", side=3, outer=TRUE, line=-1.2)

#Figure 4.2: Panel plots for Treatment Group
par(mfcol=c(5,5))
for (i in 26:50){
  plot(time$time[time$id==i], time$intimacy[time$id==i], 
       ylab="intimacy", xlab="time", 
       type="o", pch=4, 
       ylim=c(0,8), 
       main=paste("id =", i, sep = " "))
}
mtext("Treatment Group", side=3, outer=TRUE, line=-1.2)

# ANALYSES ---------------------------------------------------------------------

#Run linear growth model with AR(1) errors 
lgmodel <- lme(
  # fixed effect structure
  # interaction between time01 and treatment group
  fixed = intimacy ~ time01*treatment, 
  
  # random effect structure: both intercept and slope for participant
  random=~time01 | id, 
  
  # error autocorrelation structure
  correlation = corAR1(),
  
  # data frame
  data = time)
summary(lgmodel)

# set up dataset with id, intimacy (DV)
# and the empirical bayes (EB) estimates of random slopes and intercepts
coefs<-data.frame(lgmodel$coefficients$random$id)
names(coefs)[c(2,1)] <- c("ebslope","ebintercept")
coefs$id <- with(coefs, 1:50)
coefs$treatment<-aggregate(treatment ~ id, data=time, mean)[, 2]
# calculate individual estimated slope from the fixed and random effects est
# 0.7352012: fixed effect slope for time (divided by 15)
# 0.9214365: time*treatment effect
coefs$slope <- with(coefs, ebslope + 0.7352012 + 0.9214365*treatment)
coefs

# ordered slopes within treatment group
ordcoefs<-coefs[order(coefs$treatment, coefs$slope),]
ordcoefs

#Find percentiles for slope distribution for Control Group
quantile(coefs$slope[coefs$treatment==0], c(0.0, .05, .25, .50, .75, .95, 1.0))

#Find percentiles for slope distribution for Treatment Group
quantile(coefs$slope[coefs$treatment==1], c(0.0, .05, .25, .50, .75, .95, 1.0))

# VISUALIZATION ----------------------------------------------------------------

#Figure 4.4: Panel plots for Control Group with Actual and Predicted Time Course
# 5x5 matrix for each participant
par(mfcol=c(5,5))
for (i in 1:25){
  plot(time$time[time$id==i], time$intimacy[time$id==i], 
       ylab="intimacy", xlab="time", main=paste("id =", i, sep = " "),
       type="o", pch=4, 
       ylim=c(0,8))
  # add regression line (predicted) for each participant
  lines(time$time[time$id==i], fitted(lgmodel)[time$id==i], col = "red")
}
mtext("Control Group", side=3, outer=TRUE, line=-1.2)

#Figure 4.4: Panel plots for Treatment Group with Actual and Predicted Time Course
par(mfcol=c(5,5))
for (i in 26:50){
  plot(time$time[time$id==i], time$intimacy[time$id==i], 
       ylab="intimacy", xlab="time", main=paste("id =", i, sep = " "),
       type="o", pch=4, 
       ylim=c(0,8))
  # add regression line (predicted) for each participant
  lines(time$time[time$id==i], fitted(lgmodel)[time$id==i], col = "blue")
}
mtext("Treatment Group", side=3, outer=TRUE, line=-1.2)

#Figure 4.5: Spaghetti Plots for Control Group and Treatment Group
# 2 columns for 2 groups

# control group
par(mfcol=c(1,2))
plot(
  # x = assessment times
  x = time$time[time$treatment==0], 
  
  # y = dependent variable
  y = time$intimacy[time$treatment==0], 
  
  ylab="intimacy", xlab="time", type="n", pch=4, ylim=c(0,8), main="Control")

for (i in 1:25){
  # populate empty plot with predicted regression line for each participant
  lines(time$time[time$id==i], fitted(lgmodel)[time$id==i])
}

# calculate the overall predicted value for control group
# 2.8989745: fixed effects intercept
# 0.7352012: fixed effect slope for time (divided by 15)
# 15:        number of assessment waves
predc<-2.8989745 + ((0.7352012)/15)*time$time

# add aggregate fixed effects regression line
lines(time$time, predc, lwd=4, col = "red")

# treatment group
plot(
  x = time$time[time$treatment==1], 
  y = time$intimacy[time$treatment==1], 
  ylab="intimacy", xlab="time", type="n", pch=4, ylim=c(0,8), main="Treatment")
for (i in 26:50){
  lines(time$time[time$id==i], fitted(lgmodel)[time$id==i])
}

# calculate the overall predicted value for treatment group
# -0.0564426: difference between treatment and control in intercept
# +0.9214365: difference between treatment and control in slope
predt<-(2.8989745 - 0.0564426) + ((0.7352012 + 0.9214365)/15)*time$time
lines(time$time, predt, lwd=4, col = "blue")

#Figure 2 of example write-up for Chapter 4: 
# Panel plots for five selected IDs in Control Group
# Note: In order to match the book, ID=8 is chosen for the 25th percentile, 
# whereas the more correct choice would have been ID=17
# windows(14,3)

# 5 columns for 5 random participants
par(mfcol=c(1,5))
for (i in c(22, 8, 12, 15, 4)){
  plot(time$time[time$id==i], time$intimacy[time$id==i], 
       ylab="intimacy", xlab="time",main=paste("id =", i, sep = " "),
       type="o", pch=4, ylim=c(0,8))
  # add predicted line
  lines(time$time[time$id==i], fitted(lgmodel)[time$id==i], col = "red")
}
mtext("Control Group", side=3, outer=TRUE, line=-1.2)

#Figure 2 of example write-up for Chapter 4: 
# Panel plots for five selected IDs in Treatment Group
# Note: In order to match the book, ID=50 is chosen for the 25th percentile, 
# whereas the more correct choice would have been ID=39
# windows(14,3)
par(mfcol=c(1,5))
for (i in c(46, 50, 26, 49, 37)){
  plot(time$time[time$id==i], time$intimacy[time$id==i], 
       ylab="intimacy", xlab="time", main=paste("id =", i, sep = " "),
       type="o", pch=4, ylim=c(0,8))
  # add predicted line
  lines(time$time[time$id==i], fitted(lgmodel)[time$id==i], col = "blue")
}
mtext("Treatment Group", side=3, outer=TRUE, line=-1.2)

