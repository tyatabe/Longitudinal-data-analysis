setwd("C:/Users/tyatabe/OneDrive/Docs/PhD Epi/Spring 2016/EPI 226/HW/HW7")
setwd("C:/Users/Tadaishi/SkyDrive/Docs/PhD Epi/Spring 2016/EPI 226/HW/HW7")
###########################################################################################
############################ Problem 1 ####################################################
###########################################################################################
#Reading in the data from a stata .dta file
library(foreign)

bpd <- read.dta("bpd.dta")

# Weight (in grams × 10^-2),

bpd$weight <- bpd$weight/100

# Probability of BPD as a function of birth weight
m1 <- glm(bpd ~ weight, family=binomial(link="logit"), data=bpd) 
summary(m1)
# Probability of BPD as a function of birth weight, gestational age and toxemia
m2 <- glm(bpd ~ weight + gestage + toxemia, family=binomial(link="logit"), data=bpd) 
summary(m2)

# Comparing model's fit
anova(m1, m2, test="LRT")

# Area under the ROC for model 2:
prob=predict(m2,type=c("response"))
bpd$prob=prob
library(pROC)
g <- roc(bpd ~ prob, data = bpd)
plot(g) 


###########################################################################################
############################ Problem 2 ####################################################
###########################################################################################
# Reading in the data
chd <- read.dta("chd.dta")

# Creating the offset (log(T))
chd$logpyrs <- log(chd$pyrs) 

# Model of CHD rate as a function of cigarretes smoked
m3 <- glm(chd ~ smoke + offset(logpyrs), family=poisson(link="log"), data=chd) 
summary(m3)

# Model of CHD rate as a function of cigarretes smoked, behaviour type and 
# blood pressure
m4 <- glm(chd ~ smoke + behavior + bp + offset(logpyrs), 
          family=poisson(link="log"), data=chd) 
summary(m4)
anova(m3, m4, test="LRT")

###########################################################################################
############################ Problem 3 ####################################################
###########################################################################################
# fitting the same model using negative binomial
library(MASS)
m5 <- glm.nb(chd ~ smoke + behavior + bp + offset(logpyrs), init.theta=1504,
             data=chd)# had to set initial theta to previous run value, as 
                      #iteration limit was reached. This is allegedly trustworthy now
# Package VGAM could have a different algorithm for fitting the model (which could converge)

summary(m5)

# Simpler model with only smoking
m6 <- glm.nb(chd ~ smoke + offset(logpyrs), data=chd)
summary(m6)

###########################################################################################
############################ Problem 4 & 5 ####################################################
###########################################################################################
art <- read.dta("arthritis.dta")
# Running the model of score on age an trt 
# R runs a model where the interpretation as the logit of p(Yi >= k)

m7 <- polr(factor(y4) ~ age + trt, data=art) 
summary(m7)

# Fitting a model with age/10
m8 <- polr(factor(y4) ~ I(age/10) + trt, data=art) 
summary(m8)

