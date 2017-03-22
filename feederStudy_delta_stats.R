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

#create a stats dataframe for ABSOLUTE value difference in coccidia, by cycle
#same as above
absdCocc.mean<-aggregate(absdCocc ~ cycle, FUN=mean, data=delta) 
absdCocc.sd<-aggregate(absdCocc ~ cycle, FUN=sd, data=delta) 
names(absdCocc.sd)[names(absdCocc.sd)=="absdCocc"]<-"absdCocc.sd"
names(dCocc.mean)[names(dCocc.mean)=="absdCocc"]<-"absdCocc.mean" 
absdCocc.stats<-cbind(absdCocc.mean,absdCocc.sd) 
absdCocc.stats<-absdCocc.stats[-c(3)] 

#same as above
absdCond.mean<-aggregate(absdCond ~ cycle, FUN=mean, data=delta) 
absdCond.sd<-aggregate(absdCond ~ cycle, FUN=sd, data=delta) 
names(absdCond.sd)[names(absdCond.sd)=="absdCond"]<-"absdCond.sd"
names(dCond.mean)[names(dCond.mean)=="absdCond"]<-"absdCond.mean" 
absdCond.stats<-cbind(absdCond.mean,absdCond.sd) 
absdCond.stats<-absdCond.stats[-c(3)]

#create overall dStats dataframe
dStats<-cbind(dCond.stats,dCocc.stats,absdCond.stats,absdCocc.stats)
dStats<-dStats[-c(4,7,10)] #removes excess label columns

#for sorting cycles in correct chronological order during graphing 
#otherwise, use "Week" to sort
dStats$cycleNo[dStats$cycle=="dirty1"]<-1
dStats$cycleNo[dStats$cycle=="clean1"]<-2
dStats$cycleNo[dStats$cycle=="dirty2"]<-3
dStats$cycleNo[dStats$cycle=="clean2"]<-4

#GRAPHS
# library(ggplot2)
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