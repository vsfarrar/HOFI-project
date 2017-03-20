
#import data
birds<-read.csv(file.choose()) #use Pierce's file
#calculate condition
Condit <- lm(Mass ~ Tarsus, data=birds, na.action=na.exclude)
#put residuals in dataframe
birds$Condition <- residuals(Condit)

#new dataframe with only these weeks in it (could've done it by week >.>)
new.birds <-birds[which(birds$Week == 1 |
                          birds$Week == 3 |
                          birds$Week == 5 |
                          birds$Week == 7 |
                          birds$Week == 9),]
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

##WIDE FORMAT ATTEMPT
#tried wide format
wide.birds<-read.csv(file.choose()) #use birdmasswide.csv
nw.birds<-wide.birds[which(wide.birds$pop == "Rural"),] #select only rural birds
#get rid of unneccesary columns
nw.birds$X = NULL
nw.birds$wk0 = NULL
#add tarsus data
nw.birds$tarsus =c(19.32,19.35,19.96,18.7,19.58,19.79,19.67,19.02,19.46,18.92,19.4,18.76,19.35,19.24,18.99,
                   18.96,19.55,19.38,18.92,19.54,19.26,19.38,19.18)
#ISSUE: cannot reliably use all mass data in the regression to produce conditionals for widedataset. 
#Condition metrics not the same in the wide dataset. 