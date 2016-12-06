library(DT)
library(ggplot2)
library(shinythemes)
library(rmarkdown)
#access database
library(RPostgreSQL)
drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, dbname="housefinches", host= "localhost", user="postgres", password="postgres")
dbListTables(con)
birdmass <- dbGetQuery(con, "SELECT birds.prefix, birds.band_no1, birds.population, birds.sex, mass.band_no2, mass.date_weighed, mass.mass FROM birds INNER JOIN mass ON (band_no1 = band_no2);")

#Dataframe cleanup
birdmass$band_no2<-NULL
birdmass<-birdmass[order(birdmass$population,birdmass$band_no1, birdmass$date_weighed),]
#create week column based on date. Urban bird dates:
birdmass$week[birdmass$date_weighed<="2016-01-13"]<-0
birdmass$week[birdmass$date_weighed=="2016-01-14"]<-1
birdmass$week[birdmass$date_weighed=="2016-01-21"]<-2
birdmass$week[birdmass$date_weighed=="2016-01-28"]<-3
birdmass$week[birdmass$date_weighed=="2016-02-04"]<-4
birdmass$week[birdmass$date_weighed=="2016-02-11"]<-5
birdmass$week[birdmass$date_weighed=="2016-02-18"]<-6
birdmass$week[birdmass$date_weighed=="2016-02-25"]<-7
birdmass$week[birdmass$date_weighed=="2016-03-03"]<-8
birdmass$week[birdmass$date_weighed=="2016-03-10"]<-9
#Rural bird dates:
birdmass$week[birdmass$date_weighed=="2016-01-17"]<-1
birdmass$week[birdmass$date_weighed=="2016-01-22"]<-2
birdmass$week[birdmass$date_weighed=="2016-01-29"]<-3
birdmass$week[birdmass$date_weighed=="2016-02-05"]<-4
birdmass$week[birdmass$date_weighed=="2016-02-12"]<-5
birdmass$week[birdmass$date_weighed=="2016-02-22"]<-6
birdmass$week[birdmass$date_weighed=="2016-02-26"]<-7
birdmass$week[birdmass$date_weighed=="2016-03-04"]<-8
birdmass$week[birdmass$date_weighed=="2016-03-11"]<-9
birdmass$pop[birdmass$pop=="Rural "]<-"Rural"

##add missing weeks for birds without measurements at certain weeks
birdmass.l<-birdmass
library(data.table)
DT=as.data.table(birdmass)
setkey(DT,band_no1,week)
bm2<- DT[CJ(unique(band_no1),seq(min(week),max(week)))]
bm2$date_weighed<-NULL

#reshape from long to wide
library(reshape2)
birdmass.w<-dcast(bm2,prefix + band_no1 + population + sex ~ week, value.var = "mass", fun.aggregate= mean, na.rm = TRUE)

#Wide dataframe cleanup
birdmass.w<-birdmass.w[1:221,]
colnames(birdmass.w)[5:14]<-c("wk0","wk1","wk2","wk3","wk4","wk5","wk6","wk7","wk8","wk9")
colnames(birdmass.w)[3]<-c("pop")

#long dataframe cleanup
birdmass.l$ID<-as.numeric(factor(birdmass.l$band_no1,levels=unique(birdmass.l$band_no1)))
birdmass.l$ID<-as.factor(birdmass.l$ID)
birdmass.l$population<-as.factor(birdmass.l$population)
#DATAFRAME READY FOR OPERATION!

pops<-as.character(unique(unlist(birdmass.w$pop))) #creates list of pop variables
sexes<-as.character(unique(unlist(birdmass.w$sex)))

#test function
#graphz<-function(x) ifelse(x=="Urban",print("Urban selected"),print("Rural selected"))


on.exit(dbDisconnect(con))