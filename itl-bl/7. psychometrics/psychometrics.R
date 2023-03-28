############################################################################################################
#Chapter 7 Analyses: Modeling the Causal Process of Continuous Outcomes
#Note: The code assumes that the data file psychometrics.csv has been copied to your default directory
############################################################################################################


############################################################################################################
#load the lme4 package
library(lme4)
############################################################################################################

############################################################################################################
#Read a csv file containing the data
psychometrics <- read.csv('psychometrics.csv')
############################################################################################################

############################################################################################################
#Create versions of person, time and item that R treats as factors
psychometrics$catitem <- as.factor(psychometrics$item)
psychometrics$catperson <- as.factor(psychometrics$person)
psychometrics$cattime <- as.factor(psychometrics$time)
############################################################################################################


############################################################################################################
#Estimate a model with crossed random effects of person, time, item and their interactions
mod1<-lmer(y ~  1 + (1|catperson) + (1|cattime) + (1|catitem)
     + (1|catperson:cattime) + (1|catperson:catitem) + (1|cattime:catitem), data=psychometrics)
summary(mod1)
############################################################################################################


############################################################################################################
#Extract the specific variance estimates
VarCorr(mod1)[[1]][1,1] #catperson:cattime 

VarCorr(mod1)[[2]][1,1] #catperson:catitem 

VarCorr(mod1)[[3]][1,1] #catperson 

VarCorr(mod1)[[4]][1,1] #cattime:catitem 

VarCorr(mod1)[[5]][1,1] #cattime 

VarCorr(mod1)[[6]][1,1] #catitem 

sigma(mod1)^2            #Residual
############################################################################################################


############################################################################################################
#Use specific components to calculate Rc, the reliability of change
#Formula: Var(person by item)/(Var(person by item) + (Var(person by item by time) + Var(error))/4)
Rc<-(VarCorr(mod1)[[1]][1,1])/((VarCorr(mod1)[[1]][1,1]) + (sigma(mod1)^2)/4)
Rc
############################################################################################################
