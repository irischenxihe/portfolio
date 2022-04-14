#WFHS data set n=3684 employees from 160 study groups

data <- read.csv("/Users/chenxihe/Desktop/Fall 2021/QAC 312/wfhs.csv")
colnames(data) <- tolower(colnames(data))
names(data)

library(descr)
library(Hmisc)
library(ggplot2)
library(readr)

# descriptive statistics
library(Hmisc)
library(descr)

describe(data$studygroup)

#creating new variables
#wfconflict is the response variable Work-Family Conflict
data$wfconflict <-data$scwm_ftwci+data$scwm_wtfci
describe(data$wfconflict)
#schecon&disauth are the level 1 predictors Schedule Control and Decision Authority
data$schecon <- data$scwm_cwhi
describe(data$schecon)
data$disauth <- data$scwm_jstrda
describe(data$disauth)
#fssb&wwfc are the level 2 predictors Family-Supportive Supervisor Behaviors and Workplace Work-Family Climate
data$fssb <- data$scwm_fssbi
describe(data$fssb)
data$wwfc <- data$scwm_ocli
describe(data$wwfc)

freq(data$wfconflict, plot=F) 
freq(data$schecon, plot=F)  
freq(data$disauth, plot=F) 
freq(data$fssb, plot=F)
freq(data$wwfc, plot=F)

hist(data$fssb)
hist(data$wwfc)
# subset data to exclude observations with missing data
var.keep <- c("adminlink", "studygroup", "wfconflict", "schecon", "disauth", "fssb", "wwfc")
data2<- data[ , var.keep]
data3 <- na.omit(data2)

# number of level 2 units
length(unique(data3$studygroup))

# average number of level 1 observations (employees) per level 2 unit (studygroup)
library(plyr)
numlev1 <- ddply(data3, c("studygroup"), summarise, N=sum(!is.na(wfconflict)))
describe(numlev1$N)

#adding the number of employees in each studygroup
describe(numlev1$N)

# plot mean work-family conflict and error bars by studygroup
library(ggplot2)
cdata <- ddply(data3, c("studygroup"), summarise,
               N    = sum(!is.na(wfconflict)),
               mean = mean(wfconflict, na.rm=TRUE), #remove the NA
               sd   = sd(wfconflict, na.rm=TRUE),
               se   = sd / sqrt(N))
# reorder observations by mean work-family conflict
cdata$studygroup <- factor(cdata$studygroup, levels = cdata$studygroup[order(cdata$mean)])

# plot 
p <- ggplot(cdata, aes(x=studygroup, y=mean))
p + geom_point() + geom_errorbar(aes(ymin=mean-se, ymax=mean+se)) + xlab("Study Group") + ylab("Mean Work-Family Conflict Score")

# random intercept model (random effects ANOVA)  
library(lme4)
hlm1 <- lmer(wfconflict ~ 1 + (1|studygroup), data3, REML=F) #allow the b0 to vary between groups
summary(hlm1)

# ICC=0.103
totalvar<-0.174+1.517
icc<-0.174/totalvar
print(icc)

# confidence intervals
hlm1ci <- confint(hlm1, method="profile")
print(hlm1ci)

# design effect=7.581
deseff=1+0.103*(64.89-1)
print(deseff)
# amount of bias in standard errors based on design effect
deft=sqrt(deseff)
print(deft)
# shows that if clustering is ignored then standard errors are 3 times smaller than they
# would be if clustering were taken into account

# effective sample size=485.952
effsize=3684/7.581
print(effsize)

# plot group level random effects (standard deviations with confidence intervals)
library(merTools)
randoms <- REsim(hlm1, n.sims = 500)
plotREsim(randoms)

#################################################################################################
# Adding level 1 predictor with a random intercept but fixed slope
#################################################################################################

# group mean center quantitative Level 1 predictor (studygroup schedule control)
# calculate study group mean schedule control
mean1<- ddply(data3, c("studygroup"), summarise, mean_schecon=mean(schecon))

# merge back with data3
data3 <- join(data3, mean1, by='studygroup', type='left', match='all')
data3$schecon_c<-data3$schecon-data3$mean_schecon
describe(data3$schecon_c)

# add employee level schedule control with random intercept
hlm2 <- lmer(wfconflict ~ 1 + schecon_c + (1|studygroup), data3, REML=F)
summary(hlm2)

# Likelihood ratio chi-square difference test for adding a level 1 predictor
# anova(simple model; complex model)
anova(hlm1,hlm2)

# confidence intervals
hlm2ci <- confint(hlm2, method="profile", oldNames=F)
print(hlm2ci)

# intraclass correlation coefficient
library(sjstats)
performance::icc(hlm2)

# plot association between schedule control and work-family conflict 
# first get predicted work-family conflict
predwfconflict <- fitted(hlm2)
# combine studygroup ID and centered schedule control variables with predicted scores
datapred <- unique(data.frame(cbind(predwfconflict = predwfconflict, schecon_c =
                                      data3$schecon_c, studygroup = data3$studygroup)))
# generate plot
library(lattice)
xyplot(predwfconflict ~ schecon_c, data = datapred, groups = studygroup, type = c("p","l"), col = "blue", 
       xlab="Employee Schedule Control (group mean centered)", ylab="Predicted Work-Family Conflict Score",
       title="Slope of Association of Employee Schedule Control on Work-Family Conflict by Studygroup")

# plot random effects
randoms2 <- REsim(hlm2, n.sims = 500) 
plotREsim(randoms2)

##############################################################################################
#Add a level 1 predictor with a random slope
##############################################################################################

hlm3 <- lmer(wfconflict ~ 1 + schecon_c + (1+schecon_c|studygroup), data3, REML=F)
summary(hlm3)

#chi-square difference test for adding a random slope
anova(hlm2,hlm3)

# confidence intervals
hlm3ci <- confint(hlm3, method="profile", oldNames=F)
print(hlm3ci)

# plot association between schedule control and conflict score
# Step 1: get predicted conflict scores
predwfconflict3 <- fitted(hlm3)
# combine study group id and centered schedule contro variables with predicted scores
datapred3 <- unique(data.frame(cbind(predwfconflict3 = predwfconflict3, schecon_c =
                                       data3$schecon_c, studygroup = data3$studygroup)))
# generate plot
xyplot(predwfconflict3 ~ schecon_c, data = datapred3, groups = studygroup, type = c("p","l"), col = "blue", 
       xlab="Employee Schedule Control (centered)", ylab="Predicted Work Family Conflict Score by Studygroup")

###################################################################################################
# add another Level 1 predictor with a fixed slope
data3$disauth2<-as.numeric(data3$disauth)
hlm4a <- lmer(wfconflict ~ 1 + schecon_c + disauth2 + (1+schecon_c|studygroup), data3, REML=F)
summary(hlm4a)

# Likelihood ratio chi-square difference test for adding another level 1 predictor
# anova(simpler model; complex model)
anova(hlm2,hlm4a)

# adding random slope for decision authority
hlm4b <- lmer(wfconflict ~ 1 + schecon_c + disauth2 + (1+disauth2|studygroup), data3, REML=F)
summary(hlm4b)

# remove decision authority random slope (nonconvergence)
hlm4b <- lmer(wfconflict ~ 1 + schecon_c + disauth2 + (1|studygroup), data3, REML=F)
summary(hlm4b)

# Likelihood ratio chi-square difference test for adding a random slope for level 1 predictor
# anova(simpler model; complex model)anova(hlm4a,hlm4b)
anova(hlm4a,hlm4b)

# confidence interval
hlm4ci <- confint(hlm4a, method="profile", oldNames=F)
print(hlm4ci)

library(sjstats)
performance::icc(hlm4a)

library(MuMIn)
r.squaredGLMM(hlm4a)
r.squaredLR(hlm4a)

###################################################################################################
# plot association between decision authority and work family conflict score
# Step 1: get predicted work family conflict scores
predwfconflict4 <- fitted(hlm4a)
# combine studygroup id and centered schedule control variables with predicted scores
datapred4 <- data.frame(cbind(predwfconflict4 = predwfconflict4,schecon_c=data3$schecon_c, studygroup = data3$studygroup))
# generate plot
xyplot(predwfconflict4 ~ schecon_c, data = datapred4, groups = studygroup, type = c("p","l"), col = "blue", 
       xlab="Employee Schedule Control (centered)", ylab="Predicted Work Family Conflict Score by Studygroup")

# plot random effects
reEx <- as.data.frame(REsim(hlm4a))
head(reEx)
p1 <- plotREsim(reEx)
p1

#########################################################################
# ADDING LEVEL 2 PREDICTORS
#########################################################################
#add level 2 predictor family-supportive supervisor behavior
hlm5a <- lmer(wfconflict ~ 1 + schecon_c + disauth2 +fssb+ (1+schecon_c|studygroup), data3, REML=F)
summary(hlm5a)
anova(hlm4a,hlm5a)

#add random slope for family-supportive supervisor behavior
hlm5b <- lmer(wfconflict ~ 1 + schecon_c + disauth2 + fssb+ (1+schecon_c+fssb|studygroup), data3, REML=F)
summary(hlm5b)
#remove random slope fssb

##add interaction term family-supportive supervisor behavior*schedule control
hlm5c <- lmer(wfconflict ~ 1 + schecon_c + disauth2 + fssb + schecon_c*fssb + (1+schecon_c|studygroup), data3, REML=F)
summary(hlm5c)
anova(hlm5a,hlm5c)

#add level 2 predictor workplace work-family climate
hlm6a <- lmer(wfconflict ~ 1 + schecon_c + disauth2 +fssb+wwfc+ (1+schecon_c|studygroup), data3, REML=F)
summary(hlm6a)
anova(hlm5a,hlm6a)

#add random slope for workplace work-family climate
hlm6b <- lmer(wfconflict ~ 1 + schecon_c + disauth2 +fssb+wwfc+ (1+schecon_c+wwfc|studygroup), data3, REML=F)
summary(hlm6b)
#remove random slope wwfc

##add interaction term workplace work-family climate*schedule control
hlm6c<- lmer(wfconflict ~ 1 + schecon_c + disauth2 +fssb+wwfc+schecon_c*wwfc+ (1+schecon_c|studygroup), data3, REML=F)
summary(hlm6c)

anova(hlm6a,hlm6c)

#check residuals
layout(matrix(c(1,2,3,4),2,2))
plot(hlm6a)

# FINAL MODEL IS HLM6a?

#compute confidence intervals for final model
hlm6ci <- confint(hlm6a, method="profile", oldNames=F)
print(hlm6ci)

# intraclass correlation coefficient
library(sjstats)
performance::icc(hlm6a)

# R-square for final model
MuMIn::r.squaredGLMM(hlm6a)
#R2M marginal-fixed, R2C conditional-both fixed and random

# plot interaction
library(effects) 
plot(effect("schecon_c:wwfc",hlm6c))

# alternative interaction plot using ggplot
int1<-effect("schecon_c:wwfc",hlm6c)
summary(int1)
# save effects as a data frame
int1data <- as.data.frame(int1)
int1data
# plot
ggplot(int1data, aes(schecon_c, fit, color=wwfc)) + geom_point() + geom_line() +
  geom_errorbar(aes(ymin=fit-se, ymax=fit+se), width=0.4) + theme_bw(base_size=12)

# final model random effects plot
randoms6 <- REsim(hlm6a, n.sims = 500)
plotREsim(randoms6)


