setwd("C:/Users/tyatabe/OneDrive/Docs/PhD Epi/Spring 2016/EPI 226/HW/HW8")
setwd("C:/Users/Tadaishi/SkyDrive/Docs/PhD Epi/Spring 2016/EPI 226/HW/HW8")
###########################################################################################
############################ Problem 1 ####################################################
###########################################################################################
#Reading in the data from a stata .dta file
library(foreign)
lep <- read.dta("leprosy.dta")

# Table 48
# Making the summary table
library(doBy)
# Change ref level for table
lep1 <- within(lep, drug <- relevel(drug, ref = 2))

#Doing the summary table
t48 <- summaryBy(y1 + y2 ~ drug, data=lep1, FUN=c(mean, var))
colnames(t48) <- c("Treatment", "t0 mean", "t1 mean", "t0 var", "t1 var")


# Table 50 Parameter estimates from a log-linear model of # bacili given treatment
# Reshape to long format
d <- reshape(lep, idvar="id", varying=c("y1","y2"), v.names="y", timevar="time", 
             time=0:1, direction="long")
d <- d[order(d$id, d$time),]

# Creating drug by time variables for interactions in the model
d$drugn <- as.numeric(d$drug)
d$timeA <- d$time*(d$drugn == 2)
d$timeB <- d$time*(d$drugn == 3)
d$timeAB <- d$time*I(d$drugn != 1)

# Running the model
install.packages("geepack")
library(geepack)
m1 <- geeglm(y ~ time + timeA + timeB, data=d, id = id, waves=time, family=poisson("log"),
             corstr="exch", std.err="san.se")
summary(m1)

# Table 51 Parameter estimates from a log-linear model of # bacili given treatment
# with merged treatments (antibiotic versus no antibiotic treatment)

m2 <- geeglm(y ~ time + timeAB, data=d, id = id, waves=time, family=poisson("log"), 
             corstr="exch", std.err="san.se")
summary(m2)


# Table 52: toe nail data (gee logistic link)
toe <- read.dta("toenail.dta")
# Fit the model
m3 <- geeglm(y ~ month + I(trt*month), data=toe, id = id, waves=visit, family=binomial("logit"), 
             corstr="ar1", std.err="san.se")
summary(m3)

###########################################################################################
############################ Problem 2 ####################################################
###########################################################################################
# Fitting a GLMM to the toe nail data
library(lme4)
m4 <- glmer(y ~ month + I(trt*month)+ (1 | id), data=toe, family=binomial, nAGQ=100, 
            na.action=na.omit)
summary(m4)


###########################################################################################
############################ Problem 3 ####################################################
###########################################################################################
# Read in data

epi <- read.dta("epilepsy.dta")
d <- reshape(epi, idvar="id", varying=c("y0","y1","y2","y3","y4"), v.names="y", 
             timevar="visit", time=0:4, direction="long")

d$time <- as.numeric(d$visit!=0)# Dichotomizing time into baseline (0) and post-baseline (1)
d$ltime <- d$time
d$ltime[d$visit==0] <- log(8)# Offset fot baseline (log of 8 weeks)
d$ltime[d$visit!=0] <- log(2)# Offset of post-baseline (log of 2 weeks)


m5 <- glmer(y ~ offset(ltime) + time + trt + time:trt + (1 + time | id), data=d, family=poisson, 
            nAGQ=1, na.action=na.omit)
summary(m5)


###########################################################################################
############################ Problem 5 ####################################################
###########################################################################################
# Reading in data
ame <- read.dta("amenorrhea.dta")
ame$time2 <- ame$time^2
ame$trt.time <- ame$trt*ame$time
ame$trt.time2 <- ame$trt*ame$time2

#Fitting the GLMM model
m6 <- glmer(y ~ time + time2 + trt.time + trt.time2 + (1 | id), data=ame,
            family=binomial, nAGQ=100, na.action=na.omit)
summary(m6)
vcov(m6)# Getting cov matrix for fixed effects

#Fitting the GEE model
m7 <- geeglm(y ~ time + time2 + trt.time + trt.time2, data=ame, id = id,
             waves=time, family=binomial("logit"), corstr="unstructured",
             std.err="san.se", scale.fix=T)
summary(m7)
# Getting the Cov matrix of parameter estimates
m7$geese$vbeta



