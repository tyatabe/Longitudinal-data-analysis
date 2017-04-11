setwd("C:/Users/tyatabe/OneDrive/Docs/PhD Epi/Spring 2016/EPI 226/HW/HW2")
setwd("C:/Users/Tadaishi/SkyDrive/Docs/PhD Epi/Spring 2016/EPI 226/HW/HW2")

###############################################################################################
################### Exercise 1 ################################################################
###############################################################################################
lead <- read.table("lead-data.txt")
#Tidying it up
colnames(lead) <- c("ID", "Base", "wk1", "wk4", "wk6")
# Checking it out
head(lead)
summary(lead)

# Transforming from wide to long
d <- reshape(lead, varying=c("Base", "wk1", "wk4", "wk6"), direction="long", v.names="lead",
             idvar="ID")
d <- d[order(d$ID),]
#Checking it out
d[1:20,]
str(d)
# Creating a factor for weeks
d$week <- d$time
d$week[d$time==1] <- 0
d$week[d$time==2] <- 1
d$week[d$time==3] <- 4
d$week[d$time==4] <- 6
d$week <- factor(d$week, c(0,1,4,6))

summary(d)


# Changing to make week 6 to be the ref level
d<- within(d, week <- relevel(week, ref = 4))

# Running a generalized least squares model with the tlc data

library(nlme)
m1 <- gls(lead~week, data=d, correlation= corSymm(form = ~ time | ID),
          weights = varIdent(form = ~ 1 | time), method="REML")
summary(m1)
anova(m1)
getVarCov(m1)

#Getting correlation estimates
vcov(m1)

# Null model likelihood ration test: rUnning two models, one with time and a null model, both
# Fitted using ML instead of REML
m2 <- gls(lead~week, data=d, correlation= corSymm(form = ~ time | ID),
          weights = varIdent(form = ~ 1 | time), method="ML")
m3 <- gls(lead~1, data=d, correlation= corSymm(form = ~ time | ID),
          weights = varIdent(form = ~ 1 | time), method="ML")
# Comparing two models:
anova(m3, m2, test=T)

# Doing a contrast of week 6 vs (minus) week 0...given how the model is specified it makes no sense
# it's already in the coefficient for week 0 (since the ref level is week 6)
install.packages("multcomp")
library(multcomp)
# Specifying the contrast...here it does not make sense to use a (1, -1) type of vector,
# as the regression coeff for week 6 is already contrasted against week 0
K <- matrix(c(0, -1, 1, 0), 1)

#Running it
t <- glht(m1, linfct = K)
summary(t)

# Running a model that does no take into account the correlation of the data
m4 <- gls(lead~week, data=d, method="REML")
summary(m4)
anova(m4)

# TO get the ss table I need to run a lm model (not gls)
m5 <- lm(lead~week, data=d)
anova(m5)

# And the contrast
K2 <- matrix(c(0, -1, 0, 0), 1)
t2 <- glht(m5, linfct = K2)
summary(t2)

###############################################################################################
########################### Problem 2 #########################################################
###############################################################################################
# Linear mixed effects model with random intercepts
m6 <- lme(lead~week, data=d, random= ~ 1|ID)
summary(m6)
anova(m6)

# Linear mixed effects model with random intercepts, with predictors week and group

m7 <- lme(lead~week, data=d, random= ~ 1|ID)
summary(m6)
anova(m6)

# ANOVA (one outcome at a time)
m7.1 <- aov(lead ~ week, data=d)
summary(m7.1)

# Some posthoc
TukeyHSD(m7.1)

# Repeated measures ANOVA
# Need to transform ID variable into a factor for repeated measures ANOVA
d$id <- factor(d$ID)
m7.2 <- aov(lead ~ week + Error(id/week), data = d)
summary(m7.2)

# Repeated measures analysis by MANOVA
# Need to use data in the wide format
# Need to create a response matrix (it takes only matrices)
d.2 <- as.matrix(lead[,2:5])

# Creating contrast matrix
m <- rbind(rep(-1, 3), c(1, 0, 0), c(0, 1, 0), c(0, 0, 1))

# Difference between times matrix
diff <- d.2 %*% m

# Creating a multivariate intercept only model (which estimates if differences are 
# significantly different from zero)
m7.3 <- lm(diff ~ 1)
summary(m7.3)
anova(m7.3, test="Hotelling")


###############################################################################################
########################### Problem 3 #########################################################
###############################################################################################
# Reading the tlc-data in
tlc <- read.table("tlc-data.txt")
#Tidying it up
colnames(tlc) <- c("ID", "group","Base", "wk1", "wk4", "wk6")
# Checking it out
head(tlc)
summary(tlc)

# Preparing data:
lead.w1 <- tlc[,4]
lead.base <- tlc[,3]
group <- tlc[,2]
b.w2 <-  lead.base-lead.w1


# ANOVA of w1 lead by group
m8 <- aov(lead.w1 ~ group)
summary(m8)
anova(m8)

#Change analysis
m8.1 <- lm(b.w2 ~ group)
summary(m8.1)
anova(m8.1)
# ANCOVA
m8.2 <- aov(lead.w1 ~ group+lead.base)
summary(m8.2)
anova(m8.2)

# Paired t-test
# make a data frame
ttest <- data.frame(cbind(group, b.w2))
str(ttest)
head(ttest)
succ.diff <- ttest[ttest$group==1,2]
cont.diff <- ttest[ttest$group==2,2]
#Running the t-test
t.test(succ.diff,mu=0)
t.test(cont.diff,mu=0)

###############################################################################################
########################### Problem 4 #########################################################
###############################################################################################

## Adjusted means
install.packages("lsmeans")
library(lsmeans)

lsmeans(m8.2, "group")


###############################################################################################
########################### Problem 6 #########################################################
###############################################################################################


# predictor random variable parameters
n=60


set.seed(0)
x <- c(rep(0,10), rep(10,10), rep(20, 10), rep(30, 10), rep(40, 10), rep(50, 10))
plot(density(x))
#True regression parameters
b <- 20 
b1 <- 20
sigma=300
set.seed(0)
error <- rnorm(n,0, sigma)
  
#Getting the observed values of y
yobs <- b + b1*x + error
#checking it out
summary(yobs)
plot(density(yobs))

# Fitting a regression model to estimate regression coefficients and fitted values of y
m9 <- lm(yobs ~ x)
summary(m9)
anova(m9)

# Checking if r2 = (cor(yobs, x))^2
(cor(yobs, x))^2 # it's equal

# Checking if r2 = cor(yvar, x)
#getting the ybars
# making a data frame of y and x
data <- data.frame(cbind(yobs, x))
ybar0 <- mean(data[data$x==0,1])
ybar10 <- mean(data[data$x==10,1])
ybar20 <- mean(data[data$x==20,1])
ybar30 <- mean(data[data$x==30,1])
ybar40 <- mean(data[data$x==40,1])
ybar50 <- mean(data[data$x==50,1])
# Adding a column with ybar for each level of x
data$ybar <- c(rep(ybar0,10), rep(ybar10,10), rep(ybar20, 10), rep(ybar30, 10), 
               rep(ybar40, 10), rep(ybar50, 10))

# Finally, seeing if the correlation is equal to r2
(cor(data$x, data$ybar))^2 # This is much larger

# Proving that R2 could be high while fitted reg line could be almost flat

set.seed(10)
x <- rnorm(100, 10, 0.5)
plot(density(x))
#True regression parameters
b <- 10 
b1 <- 0.01
error <- 0

#Getting the observed values of y
yobs <- b+ b1*x + error 
#checking it out
summary(yobs)
plot(density(yobs))

# Fitting a regression model to estimate regression coefficients and fitted values of y
m9 <- lm(yobs ~ x)
summary(m9)
anova(m9)

# Plotting x vs y and reg line
plot(x, yobs)
abline(m9)
