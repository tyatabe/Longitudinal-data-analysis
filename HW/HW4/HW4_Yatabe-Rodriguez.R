setwd("C:/Users/tyatabe/OneDrive/Docs/PhD Epi/Spring 2016/EPI 226/HW/HW4")
setwd("C:/Users/Tadaishi/SkyDrive/Docs/PhD Epi/Spring 2016/EPI 226/HW/HW4")

###########################################################################################
############################ Problem 2 ####################################################
###########################################################################################
#Reading in the data
tlc <- read.table("tlc-data.txt")
#Tidying it up
colnames(tlc) <- c("ID", "group","Base", "wk1", "wk4", "wk6")
# Checking it out
head(tlc)
str(tlc)
summary(tlc)
# Transforming from wide to long
d <- reshape(tlc, varying=c("Base", "wk1", "wk4", "wk6"), direction="long", v.names="lead",
             idvar="ID")
d <- d[order(d$ID),]
#Checking it out
row.names(d) <- NULL
d[1:20,]
str(d)

### Table 19 (Strategy 1: GLS for analysis of response profiles):
# Running a generalized least squares model with the tlc data
# Creating a factor for weeks
d$week <- d$time
d$week[d$time==1] <- 0
d$week[d$time==2] <- 1
d$week[d$time==3] <- 4
d$week[d$time==4] <- 6
d$week <- factor(d$week, c(0,1,4,6))
summary(d)

# Changing the reference level of group variable to P
d<- within(d, group <- relevel(group, ref = 2))

# Running the model with UN corr and different var per week
library(nlme)
m1 <- gls(lead~ group*week, data=d, correlation= corSymm(form = ~ time | ID),
          weights = varIdent(form = ~ 1 | time), method="REML")
anova(m1)
summary(m1)
getVarCov(m1)# Getting cov matrix
cov2cor(getVarCov(m1))# Getting cor matrix

### Table 20 (strategy 2: GLS for reponse profiles assuming equal value at baseline,
### i.e. assuming group effect at baseline = 0)

m2 <- gls(lead~ I(week==1) + I(week==4) + I(week==6) + I(week==1 & group=="A") + 
            I(week==4 & group=="A") + I(week==6 & group=="A"),
          data=d, correlation= corSymm(form = ~ time | ID),
          weights = varIdent(form = ~ 1 | time), method="REML")

anova(m2)
summary(m2)


### Table 21 (Strategy 3: subtracting baseline response from postbaseline 
### responses, and fitting a GLS to this new outcome)
# Creating the new variable D (difference from baseline)

tlc$baseline <- tlc$Base
d2 <- reshape(tlc, varying=c("Base", "wk1", "wk4", "wk6"), direction="long", v.names="lead",
             idvar="ID")
d2 <- d2[order(d2$ID),]
#Checking it out
row.names(d2) <- NULL
d2[1:20,]
str(d2)
# Subsetting to remove baseline obs
d2 <- subset(d2, time > 1)

# Creating categorical variable for week
d2$week <- d2$time
d2$week[d2$time==2] <- 1
d2$week[d2$time==3] <- 4
d2$week[d2$time==4] <- 6
d2$week <- factor(d2$week, c(1,4,6))
summary(d2)
# Changing the ref level for group to be "P"
d2<- within(d2, group <- relevel(group, ref = 2))

# Subtracting 1 from time var to make it a sequence of consecutive integers
# for gls's corSym
d2$time <- d2$time - 1

# Creating the difference variable
d2$D <- d2$lead-d2$baseline

# Running the model
m3 <- gls(D ~ group*week, data=d2, correlation= corSymm(form = ~ time | ID),
          weights = varIdent(form = ~ 1 | time), method="REML")
anova(m3)
summary(m3)

### Table 22 (fourth strategy: adjusting for the baseline value)
# We need to center the baseline value, for the intercept to be meaningful (why?)
# not mentioned, but the table is about differences adjusted by baseline
d2$cbaseline <- d2$baseline - mean(d2$baseline)

# Running the model
m4 <- gls(D ~ cbaseline + group*week, data=d2, correlation= corSymm(form = ~ time | ID),
          weights = varIdent(form = ~ 1 | time), method="REML")
anova(m4)
summary(m4)


###########################################################################################
############################ Problem 3 ####################################################
###########################################################################################
m4 <- gls(D ~ cbaseline + group*week, data=d2, correlation= corSymm(form = ~ time | ID),
          weights = varIdent(form = ~ 1 | time), method="ML")
m5 <- gls(D ~ cbaseline + week, data=d2, correlation= corSymm(form = ~ time | ID),
          weights = varIdent(form = ~ 1 | time), method="ML")
anova(m4, m5)# This yields a L. ratio test of 76.14.

#Need to install this package to get a wald test
install.packages("lmtest")
library(lmtest)
waldtest(m4, m5)# Virtually same result as SAS


###########################################################################################
############################ Problem 4 ####################################################
###########################################################################################
#Reading in the data
smoke <- read.table("smoking-data.txt")
#Tidying it up
colnames(smoke) <- c("ID", "smoker","time", "fev1")
# Checking it out
head(smoke)
str(smoke)
summary(smoke)

# Making time variable (i.e. a sequence of consecutive integers) from variable year
smoke$year <- smoke$time
smoke$year[smoke$time==0] <- 1
smoke$year[smoke$time==3] <- 2
smoke$year[smoke$time==6] <- 3
smoke$year[smoke$time==9] <- 4
smoke$year[smoke$time==12] <- 5
smoke$year[smoke$time==15] <- 6
smoke$year[smoke$time==19] <- 7



# Running the linear trend model
# Table 23
m6 <- gls(fev1 ~ smoker*time, data=smoke, correlation= corSymm(form = ~ year | ID),
          weights = varIdent(form = ~ 1 | year), method="REML")

anova(m6)
summary(m6)

# Running the two models (linear and quadratic trend) for table 24, using ML
m6 <- gls(fev1 ~ smoker*time, data=smoke, correlation= corSymm(form = ~ year | ID),
          weights = varIdent(form = ~ 1 | year), method="ML")

m7 <- gls(fev1 ~ smoker*time + smoker*I(time^2), data=smoke, correlation= corSymm(form = ~ year | ID),
          weights = varIdent(form = ~ 1 | year), method="ML")
summary(m7)
anova(m7, m6)

# Table 25
# Setting the data for the piecewise linear model assuming equal values at baseline (i.e. 
# common intercept)
d$week <- d$time
d$week[d$time==1] <- 0
d$week[d$time==2] <- 1
d$week[d$time==3] <- 4
d$week[d$time==4] <- 6

# Building the variables week, week-1 and its interactions with treatment
d$week1 <- (d$week-1)*I(d$week>=1)
d$trt.week <- d$week*I(d$group=="A") 
d$trt.week1 <- d$week1*I(d$group=="A")   

# Running the model
m8 <- gls(lead ~ week + week1 + trt.week + trt.week1, data=d, corr=corSymm(, form= ~ time | ID), 
             weights = varIdent(form = ~ 1 | time), method="REML")
summary(m8)

###########################################################################################
############################ Problem 5 ####################################################
###########################################################################################
# First fit a quadratic model for tlc data (with common intercept)
m9 <- gls(lead ~ week + week:group + I(week^2) + I(week^2):group, data=d, corr=corSymm(, form= ~ time | ID), 
          weights = varIdent(form = ~ 1 | time), method="ML")
summary(m9)
# Running piecewise linear model with ML for log likelihood comparisson
m8 <- gls(lead ~ week + week1 + trt.week + trt.week1, data=d, corr=corSymm(, form= ~ time | ID), 
          weights = varIdent(form = ~ 1 | time), method="ML")

# Comparing the -2log likelihood of piecewise and quadratic model
dev8<- -2*logLik(m8)
dev9 <- -2*logLik(m9)# Sligihtly smaller than in book
