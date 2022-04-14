# Import data
library(readxl)
data <- read_excel("Desktop/Fall 2021/QAC 307/data.xlsx")

# Data Management
data$Sound<-as.factor(data$Sound)
data$Noti<-as.factor(data$Noti)
data$ID<-as.character(data$ID)

# Interaction Plot
require(ggplot2)
ggplot(data=data)+
  stat_summary(aes(x=Sound,y=Time,
                   color=Noti,group=Noti),fun="mean",geom="line")+
  stat_summary(aes(x=Sound,y=Time,
                   color=Noti,group=Noti),fun="mean",geom="point")+
  ylab("Time Taken (in seconds)")

# ANOVA model
model1=aov(Time~ID+Sound+Noti+Sound*Noti,data=data)
summary(model1)

# Residual Plots
layout(matrix(c(1,2,3,4),2,2))
plot(model1)
# Breusch-Pagan Test
library(lmtest)
bptest(model1)

# Box plot Analyzing Sound Effect
ggplot(data=data)+
  geom_boxplot(aes(x=Sound,y=Time,fill=Sound),alpha=0.5)+
  scale_fill_brewer(palette="Set1")

# Box plot Analyzing Notification Effect
ggplot(data=data)+
  geom_boxplot(aes(x=Noti,y=Time,fill=Noti),alpha=0.5)+
  scale_fill_brewer(palette="Set1")

# Study Notification Effect separately in each group
pairwise.t.test(data$Time,data$Noti,p.adj="none")

# Calculate sample needed to get significant result
Grand_Mean<-matrix(rep(mean(data$Time),times=6),
                   nrow=2, ncol=3)
Grand_Mean
Noti_Effect<-matrix(rep(c(tapply(data$Time, data$Noti, FUN=mean)["1"], 
                          tapply(data$Time, data$Noti, FUN=mean)["2"],
                          tapply(data$Time, data$Noti, FUN=mean)["3"]), each=2),
                    nrow=2, ncol=3)-Grand_Mean
tapply(data$Time, data$Noti, FUN=mean)-mean(data$Time)
Noti_Effect
Sound_Effect<-matrix(rep(c(tapply(data$Time, data$Sound, FUN=mean)["0"],
                           tapply(data$Time, data$Sound, FUN=mean)["1"]),times=3),
                     nrow=2,ncol=3)-Grand_Mean
Sound_Effect
tapply(data$Time, data$Sound, FUN=mean)-mean(data$Time)
Partial_Fit<-Grand_Mean+Sound_Effect+Noti_Effect
Cell_Averages<-matrix(tapply(data$Time, 
                             list(data$Sound,data$Noti), 
                             FUN=mean), nrow=2, ncol=3)
Interaction_Effect<-Cell_Averages-Partial_Fit
Interaction_Effect
n1=2*(qnorm(1-0.05/2)+qnorm(0.8))^2/((8.23-(-13.83))/sqrt(1279))^2
# Number of replicates needed for notification effect to be significant
n1
n2=2*(qnorm(1-0.05/2)+qnorm(0.8))^2/((4.35-(-4.35))/sqrt(1279))^2
# Number of replicates needed for interaction effect to be significant
n2




