#import data (mass only)  USE tarsusmass.csv
df.birds<-read.csv(file.choose())
#captive/rural birds only
df.rur<-data.frame(df.birds[df.birds$pop=="Rural",])
#calculating condition (stolen from Pierce's code)
condition <- lm(mass ~ avg_tarsus, data=df.rur, na.action=na.exclude)
summary(condition)
#adding condition to the data frame
df.rur$condition<-residuals(condition)
#renaming dataframe so I can reuse old code...
birdmass<-df.rur
#annotating and adding week numbers for rural birds
birdmass$week[birdmass$date_weighed=="2016-01-17"]<-1
birdmass$week[birdmass$date_weighed=="2016-01-22"]<-2
birdmass$week[birdmass$date_weighed=="2016-01-29"]<-3
birdmass$week[birdmass$date_weighed=="2016-02-05"]<-4
birdmass$week[birdmass$date_weiglihed=="2016-02-12"]<-5
birdmass$week[birdmass$date_weighed=="2016-02-22"]<-6
birdmass$week[birdmass$date_weighed=="2016-02-26"]<-7
birdmass$week[birdmass$date_weighed=="2016-03-04"]<-8
birdmass$week[birdmass$date_weighed=="2016-03-11"]<-9

#model comparison
library(nlme)
#Neither week or cycle alone better fits to the data than baseline model. 
#Interaction between week and cycle performs better than baseline. 
#From RM ANOVA tutorial: http://www.jason-french.com/tutorials/repeatedmeasures.html
baseline <- lme(condition ~ 1, random = ~1 | band_no1/week, data = birdmass, method = "ML")
timefx <- lme(condition ~ week, random = ~1 | band_no1/week, data = birdmass,
                  method = "ML")
cyclefx <- lme(condition ~ cycle, random = ~1 | band_no1/cycle, data = birdmass,
              method = "ML")
interactionfx <- lme(condition ~ cycle*week, random = ~1 | band_no1/cycle, data = birdmass,
               method = "ML")
anova(baseline, timefx)
anova(baseline, cyclefx)
anova(baseline, interactionfx)

#Plain-jane ANOVA...finds that the interaction is not significant. 
#Pierce found that interaction between time and cycle was significant - why am I not finding that?
summary(aov(condition ~ week*cycle + Error(band_no1), data = birdmass))

condmeans <-data.frame(with(birdmass, tapply(condition, week, mean)))


