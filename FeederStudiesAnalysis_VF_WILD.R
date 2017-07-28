### Spring 2016 Feeder study analysis ###

#read data
fwild <- read.csv(file.choose())  #use Wild_feeder_analysis_corrected.csv

#body condition
#calculate condition
Condit <- lm(Mass ~ Tarsus, data=fwild, na.action=na.exclude)
#significant and positive - good
summary(Condit)
#put residuals in dataframe
fwild$Condition <- residuals(Condit)

#UNIQUE ID SORTING WITH RANDOM NUMBERS
#to analyse with only one measurement per bird, randomly chosen...
#HAVE ALREADY CALCULATED CONDITION BEFORE THIS STEP!
#random number generator - to sort and select only one reading from recapped birds.
fwild$randomID <- sample(100, size = nrow(fwild), replace = TRUE)
#sort samples by random ID
uniqWild<-fwild[with(fwild, order(ID, randomID)), ]
#get rid of duplicate IDs, keeping only the first ID in random order
uniqWild<-uniqWild[ !duplicated(uniqWild$ID), ]
#count sample size per week
table(uniqWild$Week)

#USING CYCLE TIME instead of WEEK
fwild$time<-0
fwild$time[fwild$Week==0]<-"NA"
fwild$time[fwild$Week==1]<-"1"
fwild$time[fwild$Week==2]<-"1"
fwild$time[fwild$Week==3]<-"1"
fwild$time[fwild$Week==4]<-"1"
fwild$time[fwild$Week==5]<-"2"
fwild$time[fwild$Week==6]<-"2"
fwild$time[fwild$Week==7]<-"2"
fwild$time[fwild$Week==8]<-"2"

#load modules
library(lme4)
library(nlme)
library(car)
library(lsmeans)
library(lmerTest)

#ANOVAS
f3 <- lmer(Condition ~ FeederStatus*Week + (1|ID),data=fwild)
Anova(f3)
Anova(lmer(Condition ~ FeederStatus*Week + (1|Date), data = uniqWild)) #using only unique reads (no RECAPs)
#no matter how you slice the cake, Week is significant, FeederStatus is not. No interaction
f4 <- lmer(Condition ~ FeederStatus*time + (1|ID),data=fwild)
f5 <- lmer(Coccidia ~ FeederStatus*time + (1|ID), data=fwild)

#Pierce's method for visualizing the time component
lsmeans(f4, list(pairwise ~ FeederStatus|time))
lsmip(f4, FeederStatus ~ time, ylab="Condition", xlab="Time", type="response")
#for 2nd cycle, clean and dirty were significantly different, but not in the 1st cycle. 
lsmeans(f5, list(pairwise ~ FeederStatus|time))
lsmip(f5, FeederStatus ~ time, ylab="Coccidia", xlab="Time", type="response")
#but not significantly different in coccidians. 

#GRAPHS
#summarize data function
data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}
#running data summary function
fwild.data <- data_summary(fwild, varname="Condition", 
                    groupnames=c("FeederStatus", "Week"))
# Convert Week to a factor variable
fwild.data$Week=as.factor(fwild.data$Week)
head(fwild.data)
fwild.data2 <-data_summary(fwild, varname="Coccidia",
                           groupnames=c("FeederStatus", "time"))

#ggplot GRAPHS
library(ggplot2)
p<-ggplot(fwild.data, aes(x=Week, y=Condition, fill=FeederStatus)) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=Condition, ymax=Condition+sd), width=.2,
                position=position_dodge(.9)) 
print(p)
p2<-ggplot(fwild.data, aes(x=FeederStatus, y=Condition, fill=FeederStatus)) + 
  geom_bar(stat="summary", color="black", 
           position=position_dodge(), fun.y= mean) 
print(p2)

#coccidia graph
ggplot(fwild.data2, aes(x=FeederStatus, y=Coccidia, fill=time)) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=Coccidia, ymax=Coccidia+sd), width=.2,
                position=position_dodge(.9)) 
