install.packages("RPostgreSQL")
library(RPostgreSQL)
drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, dbname="housefinches", host= "localhost", user="postgres", password="postgres")
#accesses db
#open pgAdminIII first!! for some reason, makes the server access easier/faster.
dbListTables(con)
#all masses of birds with population / sex data
birdmass <- dbGetQuery(con, "SELECT birds.prefix, birds.band_no1, birds.population, birds.sex, mass.band_no2, mass.date_weighed, mass.mass FROM birds INNER JOIN mass ON (band_no1 = band_no2);")
#originally did not work b/c band_no is redundant
#in pgAdmin3 changed birds.band_no to birds.band_no1, mass.band_no to mass.band_no2, pox.band_no to pox.band_no3
View(birdmass)
urb.mass<- birdmass[which(birdmass$population=='Urban'),] #mass of all urban birds
rur.mass<- birdmass[which(birdmass$population=='Rural'),] #mass of all rural birds
View(urb.mass)
View(rur.mass)
#want a db with mass of all urban recapture birds.
#want to conver these mass db's from long to wide. 
urb.recap.mass<-urb.mass[urb.mass$band_no1 %in% unique(urb.mass$band_no1[duplicated(urb.mass$band_no1)]),]
#this gets recaps only - excludes urban birds without repeated measures
urb.recap.mass$band_no2<-NULL
urb.recap.mass.sort <-urb.recap.mass[order(urb.recap.mass$band_no1, urb.recap.mass$date_weighed),]
#sorts dataframe by band number and then date weighed. 
write.csv(urb.recap.mass.sort, file = "urb_recap_mass_sort.csv")
#as a backup, this csv file now exists so don't need to access pgAdmin / postgres if not working.

##Need to add missing data rows and transform dates to weeks BEFORE reshaping. 
##Will make re-shaping easier.
rwild<-urb.recap.mass.sort
##make name shorter ;)
rwild$week[rwild$date_weighed<="2016-01-13"]<-0
rwild$week[rwild$date_weighed=="2016-01-14"]<-1
rwild$week[rwild$date_weighed=="2016-01-21"]<-2
rwild$week[rwild$date_weighed=="2016-01-28"]<-3
rwild$week[rwild$date_weighed=="2016-02-04"]<-4
rwild$week[rwild$date_weighed=="2016-02-11"]<-5
rwild$week[rwild$date_weighed=="2016-02-18"]<-6
rwild$week[rwild$date_weighed=="2016-02-25"]<-7
rwild$week[rwild$date_weighed=="2016-03-03"]<-8
rwild$week[rwild$date_weighed=="2016-03-10"]<-9
##keep rwild$week numeric (not a factor for data.table functions)
##these values add a week categorical variable instead of dates
#NOTE: These week values only valid for URBAN sampling weeks.See below for rural weeks.
##NOW to add missing weeks for birds without measurements at certain weeks
rw2<-rwild
library(data.table)
DT=as.data.table(rw2)
setkey(DT,band_no1,week)
rw3<- DT[CJ(unique(band_no1),seq(min(week),max(week)))]
#DO NOT set roll=TRUE, roll causes the last value to be "filled down" as in Excel, want NA instead!
rw3$date_weighed<-NULL
library(reshape2)
rwild.w<-dcast(rw3,prefix + band_no1 + population + sex ~ week, value.var = "mass", fun.aggregate= mean, na.rm = TRUE)
#wide version of long dataset
#defines prefix+band_no1 + population + sex as id variables (one column each)
#define mass as the value.var that will fill the cells
#define week as the variable that differs over time / represents timepoints
#fun.aggregate calculates the mean mass for each timepoint per individual (for our data should be only one, so mean = value)
#note that week 0 values (all weeks before study began) will be means if recaptured more than once before study began.
#na.rm should remove NA values...but didn't really work here. Ok. 
rwild.w<-rwild.w[1:53,]
#clips the NA rows from the end of the output
colnames(rwild.w)[5:14]<-c("wk0","wk1","wk2","wk3","wk4","wk5","wk6","wk7","wk8","wk9")
colnames(rwild.w)[3]<-c("pop")
#cleaning up column names so we know these refer to weeks of the study. 
write.csv(rwild.w, file = "urban_recaps_mass_wideFinal.csv")
#exporting our final product for backup