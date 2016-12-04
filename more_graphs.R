library(dplyr)
mean_data<-group_by(birdmass.l,pop,sex,week)%>%
            summarise(mass=mean(mass,na.rm=TRUE), max =max(mass, na.rm=TRUE), min = min(mass, na.rm=TRUE), sd=sd(mass,na.rm=TRUE))

library(ggplot2)
ggplot(na.omit(mean_data), aes(x = week))+
         geom_line(aes(y=mass),colour=pop)+
         geom_line(aes(y=mass),colour=sex)


ggplot(mean_data,aes(x=week, y=mass, colour=pop)) +
  geom_line() + geom_point()


ggplot(mean_data,aes(x=week, y=mass, colour=sex)) +
  geom_line() + 
  geom_point()

ggplot(mean_data, aes(x=week)) +
  geom_line(aes(y=mass), colour=pop)
  

