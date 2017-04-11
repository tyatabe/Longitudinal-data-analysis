setwd("C:/Users/tyatabe/OneDrive/Docs/PhD Epi/Spring 2016/EPI 226/HW/HW3")
setwd("C:/Users/Tadaishi/SkyDrive/Docs/PhD Epi/Spring 2016/EPI 226/HW/HW3")

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

# Transformig long to wide
d2 <- reshape(d, timevar="time", v.names="lead", times="time", direction="wide",
              idvar="ID")
colnames(d2) <- c("ID", "group","Base", "wk1", "wk4", "wk6")
#Checking it out
row.names(d2) <- NULL
head(d2)

###########################################################################################
############################ Problem 3 ####################################################
###########################################################################################
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
# Trying to get Chisq test as in SAS output...seems impossible (at least for me)


#########################################################################################
############################## Problem 5 ################################################
#########################################################################################
# Create a csv file for SAS to read
write.csv(d, file="tlclong.csv")
# I'll run the code in SAS...I quite don't appreciate that though