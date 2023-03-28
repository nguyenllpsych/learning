dyads<-read.csv("dyads.csv")

#****************************************************************************************************
#Figure 8.4: Panel plots of time course for 25 of 100 dyads (obtained by choosing every fourth dyad)
#****************************************************************************************************

pdf(file="Figure-8-point-4.pdf", width=14, height=14)
par(mfrow=c(5,5))
for (i in c(1, 5, 9, 13, 17, 21, 25, 29, 33, 37, 41, 45, 49, 53, 57,
            61, 65, 69, 73, 77, 81, 85, 89, 93, 97)){ 
  plot(dyads$time[dyads$coupleid==i & dyads$female==1], dyads$reldis[dyads$coupleid==i & dyads$female==1], 
       ylab="Relationship Dissatisfaction", xlab="Time", type="l", xlim=c(-1, 22), ylim=c(0,9), main=paste("id =", i, sep = " "))
  par(new=TRUE)  #Turn on overlay plotting
  plot(dyads$time[dyads$coupleid==i & dyads$female==0], dyads$reldis[dyads$coupleid==i & dyads$female==0],
       ylab=" ", xlab=" ", type="l",  xlim=c(-1, 22), ylim=c(0,9), lwd=2, col="grey")
  par(new=FALSE) #Turn off overlay plotting
}
mtext("Time Course of Relationship Dissatisfaction for 25 Couples (Male=Grey, Female=Black)", side=3, outer=TRUE, line=-1.2)
dev.off()


#****************************************************************************************************
#Table 8.1 and Table 1 (p. 169): Use lme to estimate causal process model for distinguishable dyads
#****************************************************************************************************

#It is not currently feasible, within the R package lme at least, to estimate the model for dyads
#with the level-1 covariance structure presented in Chapter 8, Eq. 8.3. Here male and female partners had distinct
#level-1 error variances, a contemporaneous covariance and a common ar(1) autocorrelation parameter.
#Instead, it is feasible to estimate a model where the male and female
#level-1 error variances are constrained to be equal, there is no contemporaneous correlation
#and there is a common ar(1) autocorrelation parameter.

#Such a model can be estimated in two difference ways, neither of which
#is satisfactory. The first (dmod1, see below) specifies coupleid as the grouping structure
#and gives the best estimates of the upper-level random effects. However, the autocorrelation parameter is
#based on data from both the male and female partners in each dyad and is likely biased toward zero. 

#The second (dmod1a) specifies coupleid\gender (gender within coupleid) as the grouping structure This gives 
# the best estimates of the lower-level random effects. It, however, forces the male and female intercepts and slopes
#to be uncorrelated, which is a major drawback.

#Because there is no evidence of autocorrelation in either model, and because there is evidence of
#correlation between male and female intercepts and male and female slopes, we recommend dmod1
#as the best overall.

#Run mixed effects model with parallel male and female random intercepts and slopes.
#First-order autocorrelation is specified, but will give incorrect results because it treats
#all the data from a couple as being in a single temporal order.
#As noted, this version of the model provides the best estimates of the upper-level random effects
dmod1 <- lme(fixed=reldis ~ male + female + male:time7c + female:time7c +
                male:wrkstrscw + female:wrkstrscw + male:wrkstrscb + female:wrkstrscb -1, 
              control=list(maxIter=1000), data=dyads,
              random=~male + female + male:wrkstrscw + female:wrkstrscw -1| coupleid, correlation=corAR1())
summary(dmod1)

#Second version of the model: This gives the best estimates of the lower-level random effects and of autocorrelation
dmod1a <- lme(fixed=reldis ~ male + female + male:time7c + female:time7c +
               male:wrkstrscw + female:wrkstrscw + male:wrkstrscb + female:wrkstrscb -1, 
             control=list(maxIter=1000), data=dyads,
             random=~male + female + male:wrkstrscw + female:wrkstrscw -1| coupleid/female, correlation=corAR1())
summary(dmod1a)



#****************************************************************************************************
#Figure 8.5: Panel plots with raw data, predicted values and confidence limits for 25 of 100 dyads
#****************************************************************************************************


#Short-cut to getting predicted values for plots: Because time is the only within-subjects variable
#and because it is specified as a fixed effect only, by subtracting the time effect from
#the model-generated predicted values, one can obtain prediced values that can be directly plotted
#to show effects of work stressors.

#Get predicted values from model
dyads$predm1<-predict(dmod1)
#Subtract the time effect separately for male and female partners
dyads$predadj[dyads$female==0]<-dyads$predm1[dyads$female==0] - 0.010479*dyads$time7c[dyads$female==0]
dyads$predadj[dyads$female==1]<-dyads$predm1[dyads$female==1] + 0.024976*dyads$time7c[dyads$female==1]

#Sort dataset by couple, person, and level of work stressors
orddyads<-dyads[order(dyads$coupleid, dyads$female, dyads$wrkstrs),]


pdf(file="Figure-8-point-5.pdf", width=14, height=14)
par(mfrow=c(5,5))
for (i in c(1, 5, 9, 13, 17, 21, 25, 29, 33, 37, 41, 45, 49, 53, 57,
            61, 65, 69, 73, 77, 81, 85, 89, 93, 97)){ 
  plot(orddyads$wrkstrs[orddyads$coupleid==i & orddyads$female==1], orddyads$reldis[orddyads$coupleid==i & orddyads$female==1], 
       ylab="Relationship Dissatisfaction", xlab="Work Stessors", type="p", pch=19, xlim=c(0, 5), ylim=c(0,9), main=paste("id =", i, sep = " "))
  lines(orddyads$wrkstrs[orddyads$coupleid==i & orddyads$female==1], orddyads$predadj[orddyads$coupleid==i & orddyads$female==1])
  par(new=TRUE)  #Turn on overlay plotting
  plot(orddyads$wrkstrs[orddyads$coupleid==i & orddyads$female==0], orddyads$reldis[orddyads$coupleid==i & orddyads$female==0],
       ylab=" ", xlab=" ", type="p", pch=1,  xlim=c(0, 5), ylim=c(0,9))
  lines(orddyads$wrkstrs[orddyads$coupleid==i & orddyads$female==0], orddyads$predadj[orddyads$coupleid==i & orddyads$female==0], lwd=2, col="grey")
  par(new=FALSE) #Turn off overlay plotting
}
mtext("Predicted Relationship Dissatisfaction for 25 Couples (Male=Grey, Female=Black)", side=3, outer=TRUE, line=-1.2)
dev.off()


#****************************************************************************************************
#Figure 8.6 and Figure 1 (p. 168): Spaghetti plot of work stressors and predicted rel. dissatisfaction
#****************************************************************************************************

pdf(file="Figure-8-point-6.pdf", width=10, height=8)
par(mfcol=c(1,2))
par(lwd=.5)

plot(orddyads$wrkstrs, orddyads$reldis, 
     ylab="Relationship Dissatisfaction", xlab="Work Stessors", type="n", xlim=c(1, 5), ylim=c(2,7), main="Female Partners")
for (i in 1:100){
  lines(orddyads$wrkstrs[orddyads$coupleid==i & orddyads$female==1], orddyads$predadj[orddyads$coupleid==i & orddyads$female==1], lwd=1.4)
}
predf<-4.6475 + 0.1599*orddyads$wrkstrs[orddyads$female==1] -0.1599*2.9904762 #fixed line for female partners
lines(orddyads$wrkstrs[orddyads$female==1], predf, col="black", lwd=7)

plot(orddyads$wrkstrs, orddyads$reldis, 
     ylab="Relationship Dissatisfaction", xlab="Work Stessors", type="n", xlim=c(1, 5), ylim=c(2,7), main="Male Partners")
for (i in 1:100){
  lines(orddyads$wrkstrs[orddyads$coupleid==i & orddyads$female==0], orddyads$predadj[orddyads$coupleid==i & orddyads$female==0], lwd=1.4)
}
predm<-5.0857 + 0.1091*orddyads$wrkstrs[orddyads$female==0] -0.1091*2.9904762 #fixed line for male partners
lines(orddyads$wrkstrs[orddyads$female==0], predm, col="black", lwd=7)
dev.off()


#****************************************************************************************************
#Figure 8.7: Using lme Estimates, 
#Create Population 95% Confidence Ellipses for Male and Female Intercepts and Slopes 
#Note the version in Bolger & Laurenceau (p.164) is based on results from SAS PROC MIXED
#and are very slightly different from the results of lme.
#****************************************************************************************************

#get random effects and add them to the fixed effects for male and female intercepts and slopes
dyadef<-ranef(dmod1)
dyadef$mint<- (5.085628 + dyadef$male) 
dyadef$fint<- (4.647326 + dyadef$female) 
dyadef$mslope<- (0.108009 + dyadef$"male:wrkstrscw")
dyadef$fslope<- (0.161456 + dyadef$"female:wrkstrscw") 

#Create mean vector covariance matrices for male and female intercepts
muII=c(5.085628, 4.647326)
sigmaII=matrix(c(1.0127881^2, 0.267*1.0127881*0.9626197, 0.267*1.0127881*0.9626197, 0.9626197^2), nrow=2, byrow=TRUE)
rownames(sigmaII) <- c("Male Intercept","Female Intercept")

#Create mean vector covariance matrices for male and female slopes
muSS=c(0.108009, 0.161456)
sigmaSS=matrix(c(0.1511865^2, 0.503*0.1511865*0.1462955, 0.503*0.1511865*0.1462955, 0.1462955^2), nrow=2, byrow=TRUE)
rownames(sigmaSS) <- c("Male Slope","Female Slope")

#load ellipse package to draw estimated population ellipses
library(ellipse)
pdf(file="Figure-8-point-7.pdf", width=14, height=7)
par(mfrow=c(1,2))
#Figure 8.7
plot(dyadef$mint, dyadef$fint, xlim=c(1.6,7.9), ann=FALSE, ylim=c(1.6,7.9), pch=1, cex=1.8, lwd=1.6)
title(main="Male and Female Intercepts")
text(1.8, 7.7, "r = 0.27", cex=1.5, adj = c(0,0))
par(new=TRUE)
plot(ellipse(sigmaII, centre = muII, level=0.95, npoints=1000), xlim=c(1.6,7.9), ylim=c(1.6,7.9), type="l", lwd=4)
abline(a=0, b=1, lwd=4, col="grey")
text(7.2, 7.8, expression(45^{"o"}), cex=1.5)
par(new=FALSE)
plot(dyadef$mslope, dyadef$fslope, ann=FALSE, xlim=c(-0.39,0.61), ylim=c(-0.22,0.54), pch=1, cex=1.8, lwd=1.6)
title(main="Male and Female Slopes")
par(new=TRUE)
plot(ellipse(sigmaSS, centre = muSS, llevel=0.95, npoints=1000), xlim=c(-0.39,0.61), ylim=c(-0.22,0.54), type="l", lwd=4)
abline(a=0, b=1, lwd=4, col="grey")
text(-0.35, 0.53, "r = 0.50", cex=1.5, adj = c(0,0))
text(0.45, 0.53, expression(45^{"o"}), cex=1.5)
dev.off() 
#****************************************************************************************************

