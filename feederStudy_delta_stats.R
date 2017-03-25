#import data
birds<-read.csv(file.choose()) #use Pierce's file Captive_feeder_analysis.csv
#calculate condition
Condit <- lm(Mass ~ Tarsus, data=birds, na.action=na.exclude)
#put residuals in dataframe
birds$Condition <- residuals(Condit)

#new dataframe with only these weeks included (labeled by cycle)
new.birds <-birds[which((birds$Week ==1 & birds$FeederStatus == "Dirty") |
                          (birds$Week ==3 & birds$FeederStatus == "Dirty")|
                          (birds$Week ==5 & birds$FeederStatus == "Clean")|
                          (birds$Week ==7 & birds$FeederStatus == "Dirty")|
                          (birds$Week ==9 & birds$FeederStatus == "Clean")),]

#used dplyr package to produce "difference" or "delta" values in Condition and Coccidia
#between each row and the row before it 
require(dplyr)
new.birds<-new.birds %>% 
  group_by(ID) %>%
  mutate(dCond = Condition - lag(Condition, default = Condition[1])) %>%
  mutate(dCocc = Coccidia  - lag(Coccidia, default = Coccidia[1]))

#COULD DO THIS IN ORIGINAL new.birds DATAFRAME
#create new dataframe with only "delta" or "difference" variables 
delta<-new.birds[,c(1,3,4,15,16)]
#create column with key to what each difference variable refers to 
delta$cycle<-0
delta$cycle[delta$Week==3]<-"dirty1"
delta$cycle[delta$Week==5]<-"clean1"
delta$cycle[delta$Week==7]<-"dirty2"
delta$cycle[delta$Week==9]<-"clean2"

delta$absdCond<-abs(delta$dCond)
delta$absdCocc<-abs(delta$dCocc)

#SUMMARY STATS
#create stats dataframe for difference in condition, by cycle
dCond.mean<-aggregate(dCond ~ cycle, FUN=mean, data=delta) #calculate mean diff per cycle
dCond.sd<-aggregate(dCond ~ cycle, FUN=sd, data=delta) #calculate sd diff per cycle
names(dCond.sd)[names(dCond.sd)=="dCond"]<-"dCond.sd" #rename columns for merge
names(dCond.mean)[names(dCond.mean)=="dCond"]<-"dCond.mean" #rename columns for merge
dCond.stats<-cbind(dCond.mean,dCond.sd) #merge both dataframes to a single stats dataframe
dCond.stats<-dCond.stats[-c(3)] #remove second "cycle" label

#create a stats dataframe for difference in coccidia, by cycle
#same as above, but for coccidia difference
dCocc.mean<-aggregate(dCocc ~ cycle, FUN=mean, data=delta) 
dCocc.sd<-aggregate(dCocc ~ cycle, FUN=sd, data=delta) 
names(dCocc.sd)[names(dCocc.sd)=="dCocc"]<-"dCocc.sd"
names(dCocc.mean)[names(dCocc.mean)=="dCocc"]<-"dCocc.mean" 
dCocc.stats<-cbind(dCocc.mean,dCocc.sd) 
dCocc.stats<-dCocc.stats[-c(3)] 

#(removed absolute value stats) 

#create overall dStats dataframe
dStats<-cbind(dCond.stats,dCocc.stats)
dStats<-dStats[-c(4,7,10)] #removes excess label columns

#CONFIDENCE INTERVALS
dStats$dCond.ll<-dStats$dCond.mean-2*dStats$dCond.sd
dStats$dCond.ul<-dStats$dCond.mean+2*dStats$dCond.sd
dStats$dCocc.ll<-dStats$dCocc.mean-2*dStats$dCocc.sd
dStats$dCocc.ul<-dStats$dCocc.mean+2*dStats$dCocc.sd

dStats$week<-c(2,4,1,3) #defines weeks for dStats
  
#for sorting cycles in correct chronological order during graphing 
#otherwise, use "Week" to sort
dStats$cycleNo[dStats$cycle=="dirty1"]<-1
dStats$cycleNo[dStats$cycle=="clean1"]<-2
dStats$cycleNo[dStats$cycle=="dirty2"]<-3
dStats$cycleNo[dStats$cycle=="clean2"]<-4

delta<-delta[!(delta$Week=="1"),]  #gets rid of week 1 "0" values that skew distributions
dStats<-dStats[!(dStats$cycle==0),]  #same for dStats summary table


#GRAPHS
library(ggplot2)
# ggplot(dStats, aes(x=cycleNo, y=dCond.mean)) + 
#   geom_bar(position=position_dodge(), stat="identity") 
# 
# ggplot(dStats, aes(x=cycleNo, y=dCocc.mean)) + 
#   geom_bar(position=position_dodge(), stat="identity") 
# 
# ggplot(dStats, aes(x=cycleNo, y=absdCond.mean)) + 
#   geom_bar(position=position_dodge(), stat="identity") 


delta$Week<-as.factor(delta$Week)
#boxplots of change in condition and change in coccidia
ggplot(delta, aes(x=Week, y=dCond, fill=FeederStatus)) + geom_boxplot() + ylab("Change in Condition")
ggplot(delta, aes(x=Week, y=dCocc, fill=FeederStatus)) + geom_boxplot() + ylab("Change in Coccidia score")

#histograms 
#change in condition, looks ~ normal, left skewed
ggplot(delta, aes(x=dCond)) + geom_histogram(bins=30,colour="black", fill="white") +xlab("Change in Condition")
#change in coccidia, ~normal around 0 (but discrete)
ggplot(delta, aes(x=dCocc)) + geom_histogram(bins=30,colour="black", fill="white") +xlab("Change in Coccidia score")

#histogram with density plot for change in condition
ggplot(delta, aes(x=dCond)) + 
  geom_histogram(aes(y=..density..),      
                 binwidth=.5,
                 colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666") 

#bar plots with confidence intervals 
#condition
ggplot(dStats, aes(x=week, y=dCond.mean, fill=cycle)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=dCond.ll, ymax=dCond.ul),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9)) +
  ylab("Change in Body condition")+
  xlab("Week")
#coccidia
ggplot(dStats, aes(x=week, y=dCocc.mean, fill=cycle)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=dCocc.ll, ymax=dCocc.ul),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9)) + 
  ylab("Change in Coccidia score")+
  xlab("Week")

#STATS
library(nlme)
library(lme4)
library(car)
#siginficant for week, but not FeederStatus or the interaction
f1 <- lmer(dCond ~ FeederStatus*factor(Week) + (1|ID), #delta dataframe does not give other random variables
           data=delta)  #interaction with week, or time?
summary(f1)
Anova(f1, type="3")

#no significant effect for either
f2 <- lmer(dCocc ~ FeederStatus*factor(Week) + (1|ID), #delta dataframe does not give other random variables
           data=delta)  #interaction with week, or time?
summary(f2)
Anova(f2, type="3")

#plain jane ANOVAs show no effect of either feeder status or week
#mean difference not significant across cycles
summary(aov(dCond ~ FeederStatus*Week + Error(ID), data = delta))
summary(aov(dCocc ~ FeederStatus*Week + Error(ID), data = delta))
