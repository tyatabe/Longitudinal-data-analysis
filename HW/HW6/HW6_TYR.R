setwd("C:/Users/tyatabe/OneDrive/Docs/PhD Epi/Spring 2016/EPI 226/HW/HW6")
setwd("C:/Users/Tadaishi/SkyDrive/Docs/PhD Epi/Spring 2016/EPI 226/HW/HW6")

###########################################################################################
############################ Problem 1 ####################################################
###########################################################################################
#Reading in the data from a stata .dta file
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
#Changing the reference level to group 2
d<- within(d, group.f <- relevel(group.f, ref = 2))
summary(d)

# LME model with random intercepts and random slopes
library(nlme)
m1 <- lme(strength~group.f*day, data=d, na.action=na.omit, random= ~ day|id)
summary(m1)
anova(m1)

# Getting the covariance matrices
getVarCov(m1, type = "random.effects")# Cov of randome effects
covm <- getVarCov(m1, type = "marginal")# Cov of observations
# Getting correlation matrix
cov2cor(covm[[1]])


###########################################################################################
############################ Problem 2 ####################################################
###########################################################################################
## Getting the random effects for each subject
random.effects(m1)

# Gettinf the subject specific response profiles
pred <- data.frame(m1[14]$fitted[,2])# subject specific predicted
res <- data.frame(m1[15]$residuals[,2])# Subject specific residual
# Setting up table
slide411 <- m1[18]$data[complete.cases(m1[18]$data),1:4]
slide411$predicted <- pred[,1]
slide411$residual <- res[,1]


###########################################################################################
############################ Problem 3 ####################################################
###########################################################################################
# Reading in the data
fat <- read.dta("fat.dta")

# Creating the time variable after the knot:
fat$time0 <- fat$time*I(fat$time>=0)

# Creating the model
m2 <- lme(pbf ~ time + time0, data=fat, random= ~ time + time0 | id)
summary(m2)

# Getting the Covariance matrix of the random effects
getVarCov(m2, type = "random.effects")
sqrt(diag(m2$apVar))# Trying to get SE's anywas...but don't match SAS ones
# Trying to get HPD intervals for variance estimates
install.packages("MCMCglmm")
library(MCMCglmm)# would need to run a bayesian model...no time

###########################################################################################
############################ Problem 4 ####################################################
###########################################################################################
# Reading in the data
fev <- read.dta("fev1.dta")
fev <- subset(fev, id!=197) # Removing outlier

# Creating log(fev1/height)
fev$y <- fev$logfev1 - 2*(log(fev$ht))


# Decomposing Between- and Within-Subject Effects
# Setting up the data:
# making a vector of mean age for each subject
fev$mage <- rep(tapply(fev$age,fev$id,mean),table(fev$id))
# Making a vector mean centered age
fev$cage <- fev$age - fev$mage

# Running the model
m3 <- lme(y ~ mage + cage, random= ~ 1 | id, data=fev)
summary(m3)

# Contrasting if beta cross-sectional = beta longitudinal
library(multcomp)
# Specifying the contrast
K <- matrix(c(0, -1, 1), 1)

#Running it
t <- glht(m3, linfct = K)
summary(t)


###########################################################################################
############################ Problem 5 ####################################################
###########################################################################################
dental <- read.dta("dental.dta")
# Transforming from wide to long
d2 <- reshape(dental, varying=c("y1", "y2", "y3", "y4"), direction="long", v.names="distance",
             idvar="id", timevar="age", time=c(8, 10, 12, 14))
d2 <- d2[order(d2$id),]
row.names(d2) <- NULL
# Checking it out
d2[1:20,]
str(d2)
# Writing csv for SAS
write.csv(d2, file="dental.csv")


###########################################################################################
############################ Problem 6 ####################################################
###########################################################################################
m4 <- lm(y ~ age + factor(id) - 1, data=fev)
summary(m4)

