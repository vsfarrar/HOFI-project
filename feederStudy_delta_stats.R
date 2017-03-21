#import data
birds<-read.csv(file.choose()) #use Pierce's file
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

