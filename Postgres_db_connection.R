install.packages("RPostgreSQL")
library(RPostgreSQL)
drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, dbname="housefinches", host= "localhost", user="postgres", password="postgres")#accesses db
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
urb.recap.mass$band_no2<-NULL
urb.recap.mass.sort <-urb.recap.mass[order(urb.recap.mass$band_no1, urb.recap.mass$date_weighed),]
install.packages("reshape") 
library(reshape)
urb.recap.mass.wide <-data.frame(cast(urb.recap.mass.sort,urb.recap.mass.sort$date_weighed, value = "mass", fun=mean))
