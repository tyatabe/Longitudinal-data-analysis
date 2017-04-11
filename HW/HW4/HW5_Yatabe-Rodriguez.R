setwd("C:/Users/tyatabe/OneDrive/Docs/PhD Epi/Spring 2016/EPI 226/HW/HW5")
setwd("C:/Users/Tadaishi/SkyDrive/Docs/PhD Epi/Spring 2016/EPI 226/HW/HW5")

###########################################################################################
############################ Problem 1 ####################################################
###########################################################################################
#Reading in the data from a stata .dta file
install.packages("foreign")
library(foreign)

ex <- read.dta("exercise.dta")
#Tidying it up

# Transforming from wide to long
d <- reshape(ex, varying=c("y0", "y2", "y4", "y6", "y8", "y10", "y12"), direction="long", v.names="strength",
             idvar="id", timevar="time", time=0:6)
d <- d[order(d$id),]
row.names(d) <- NULL
# Checking it out
d[1:20,]
str(d)

# Subsetting to get baseline and days 4, 6, 8, 12
d <- subset(d, time!=1 & time!=5)
str(d)
summary(d)
d[1:20,]

# Re-scaling back the measurement times and make it a categorical factor
d$day <- d$time*2
d$day.f <- factor(d$day, c(0,4,6,8,12))
d$group.f <- factor(d$group, c(1,2))

# Scaling time for covariance structure (needs to be sequence of consecutive integers)
d$newtime <- d$time
d$newtime[d$time==0] <- 1
d$newtime[d$time==2] <- 2
d$newtime[d$time==3] <- 3
d$newtime[d$time==4] <- 4
d$newtime[d$time==6] <- 5 

# Running the saturated model with UN cov structure
library(nlme)
m1 <- gls(strength~group.f*day.f, data=d, na.action=na.omit, 
          corr=corSymm(, form= ~ newtime | id),weights = varIdent(form = ~ 1 | newtime),
          method="REML")
summary(m1)

# Getting the covariance matrix
getVarCov(m1)
# Getting correlation matrix
cov2cor(getVarCov(m1))

# Running the saturated model with AR1 cov structure
m2 <- gls(strength~group.f*day.f, data=d, na.action=na.omit, 
          corr=corAR1(, form= ~ newtime | id), method="REML")
summary(m2)

# Getting the covariance matrix
getVarCov(m2)
# Getting correlation matrix
cov2cor(getVarCov(m2))

# Running the saturated model with exponential covariance structure
m3 <- gls(strength ~ group.f*day.f, data=d, na.action=na.omit, 
          corr=corExp(, form= ~ day | id), method="REML")
summary(m3)  
  
# Getting the covariance matrix
getVarCov(m3)
# Getting correlation matrix
cov2cor(getVarCov(m3))  

# Getting -2logREML and AIC for each model
#Deviances
dev1 <- -2*logLik(m1)
dev2 <- -2*logLik(m2)
dev3 <- -2*logLik(m3)
#AIC's...these are different than the ones computed by SAS...I think SAS is wrong
aic1 <- AIC(m1)
aic2 <- AIC(m2)
aic3 <- AIC(m3)

# Making a dataframe of it:
model <- c("UN", "AR1", "Exp")
dev <- c(dev1, dev2, dev3)
aic <- c(aic1, aic2, aic3)
table34 <- data.frame(model, dev, aic)

###########################################################################################
############################ Problem 2 ####################################################
###########################################################################################
#Getting Var and corr for AR1
getVarCov(m2) # Getting the variance from the main diag
m2$modelStruct$corStruct # Getting the correlation

#Getting Var and corr for Exp
getVarCov(m2)# This is for the variance (main diagonal)


###########################################################################################
############################ Problem 3 ####################################################
###########################################################################################
# Getting the LRT between UN and AR1
anova(m1, m2)

# Getting the LRT between UN and Exp
anova(m1, m3)

###########################################################################################
############################ Problem 4 ####################################################
###########################################################################################
# Running a model with heterogeneus exponential covariance structure

m4 <- gls(strength ~ group.f*day.f, data=d, na.action=na.omit, 
          corr=corExp(, form= ~ day | id), 
          weights = varIdent(form = ~ 1 | newtime), method="REML")

getVarCov(m4)
# LRT of UN with heterogeneus Exp
anova(m1, m4)

###########################################################################################
############################ Problem 4 ####################################################
###########################################################################################
# Can't do it in R...apparently I need to use GEE...it might not work as well
# I'll do it in SAS
# creating file for SAS
write.csv(d, file="exercise.csv")

# SAS code
"proc import datafile="C:\Users\tyatabe\Downloads\exercise.csv"
out=exercise
dbms=csv
replace;
getnames=yes;
run;

proc print;
run;

PROC MIXED;
CLASS id group time;
MODEL strength=group time group*time /S CHISQ;
REPEATED time / TYPE=SP(EXP)(day) SUBJECT=id R RCORR;
run;

PROC MIXED EMPIRICAL;
CLASS id group time;
MODEL strength=group time group*time /S CHISQ;
REPEATED time / TYPE=SP(EXP)(day) SUBJECT=id R RCORR;
run;"
