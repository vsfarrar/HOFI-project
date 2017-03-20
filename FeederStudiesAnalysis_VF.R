### Spring 2016 Feeder study analysis ###
## Farrar - Frumkin - Hutton - McGraw ##

#read data
##data already in long format
fwild <- read.csv(file.choose())
fcapt <- read.csv(file.choose())

#convert from numerical to factor
fcapt$CageNumber <- as.factor(fcapt$CageNumber)
fcapt$Replicate <- as.factor(fcapt$Replicate)

#load modules
library(lme4)
library(nlme)
library(car)
library(lsmeans)

## Feeder captive study ##
#some individuals switch from diseased to non-diseased state
interaction.plot(response=fcapt$Coccidia, trace.factor=fcapt$ID, x.factor=fcapt$Week)

#visualizing by cage
boxplot(Coccidia ~ Time*FeederStatus, data=fcapt[fcapt$CageNumber==1,])
boxplot(Coccidia ~ Time*FeederStatus, data=fcapt[fcapt$CageNumber==2,])
boxplot(Coccidia ~ Time*FeederStatus, data=fcapt[fcapt$CageNumber==3,])
boxplot(Coccidia ~ Time*FeederStatus, data=fcapt[fcapt$CageNumber==4,])

#distributions reflect poisson curves
hist(fcapt[fcapt$Week=="1",]$Coccidia)
hist(fcapt[fcapt$Week=="2",]$Coccidia)
hist(fcapt[fcapt$Week=="3",]$Coccidia)
hist(fcapt[fcapt$Week=="4",]$Coccidia)
hist(fcapt[fcapt$Week=="5",]$Coccidia)
hist(fcapt[fcapt$Week=="6",]$Coccidia)
hist(fcapt[fcapt$Week=="7",]$Coccidia)
hist(fcapt[fcapt$Week=="8",]$Coccidia)
hist(fcapt[fcapt$Week=="9",]$Coccidia)

#test how coccidia changes over time within each feeder treatment
#random effects control for individual, cage location, cycle replicate
#poisson distribution of response variable
f1 <- glmer(Coccidia ~ FeederStatus*factor(Time) + 
              (1|ID) + (1|CageNumber) + (1|Replicate),
      family="poisson",
      data=fcapt)

#trend towards time effect - unexpected 
summary(f1)
Anova(f1, type="3")

#visualize time trend
lsmeans(f1, list(pairwise ~ FeederStatus|Time))
lsmip(f1, FeederStatus ~ Time, ylab="Coccidia", xlab="Time", type="response")

#coccidia presence
#binomial term for Y/N response
f2 <- glmer(CoccidiaPresence ~ factor(Time)*FeederStatus + 
              (1|ID)+ (1|CageNumber) + (1|Replicate),
          family="binomial",
          data=fcapt)

#no time*status effect, but removing interaction causes both main effects to be sig
summary(f2)
Anova(f2, type="3")

#vis - birds just had higher chance of infection during clean treatment??
lsmeans(f2, list(pairwise ~ FeederStatus|Time), type="response")
lsmip(f2, FeederStatus ~ Time, ylab="Coccidia", xlab="Time", type="response")

#body condition
#calculate condition
Condit <- lm(Mass ~ Tarsus, data=fcapt, na.action=na.exclude)
#significant and positive - good
summary(Condit)
#put residuals in dataframe
fcapt$Condition <- residuals(Condit)

f3 <- lmer(Condition ~ FeederStatus*factor(Time) + (1|ID) + (1|CageNumber) + (1|Replicate),
            data=fcapt)

#trend towards interaction effect
summary(f3)
Anova(f3, type="3")

#condition increases in middle of cycle?
lsmeans(f3, list(pairwise ~ FeederStatus|Time))
lsmip(f3, FeederStatus ~ Time, ylab="Body condition", xlab="Time", type="response")



