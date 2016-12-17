library(ggplot2)
library(RPostgreSQL)
drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, dbname="housefinches", host= "localhost", user="postgres", password="postgres")
dbListTables(con)
tarmass <- dbGetQuery(con, "SELECT birds.prefix, birds.band_no1, birds.avg_tarsus, birds.population, birds.sex, mass.band_no2, mass.date_weighed, mass.mass FROM birds INNER JOIN mass ON (band_no1 = band_no2);")
#clean-up dataframe
tarmass$band_no2<-NULL
tarmass<-na.omit(tarmass)
tarmass$population[tarmass$population!="Urban"]<-"Rural"
tarmass$sex[tarmass$sex!="M"]<-"F"

#defines dirty and clean cycles
dirty<-as.Date(c("2016-01-14","2016-01-22","2016-01-29","2016-02-04","2016-02-11","2016-03-03","2016-03-10","2016-02-22","2016-02-26"))
clean<-as.Date(c("2016-01-17","2016-01-21","2016-01-28","2016-02-05","2016-02-12","2016-02-18","2016-02-25","2016-03-04","2016-03-11"))

tarmass$cycle[tarmass$date_weighed<="2016-01-13"]<-"dirty"
tarmass$cycle[tarmass$date_weighed %in% dirty]<-"dirty"
tarmass$cycle[tarmass$date_weighed %in% clean]<-"clean"

#regression
lm.r<-lm(tarmass$mass~tarmass$avg_tarsus)
b1<-coef(summary(lm.r))[2,1]
b0<-coef(summary(lm.r))[1,1]
tarmass$residual<-as.vector(resid(lm.r))
#plot regression
p1<-ggplot(tarmass,aes(x=avg_tarsus,y=mass))+
  list(
    ylab("Mass(g)"),
    xlab("Average tarsus length(mm)"),
    ggtitle("Regression of mass vs tarsus"),
    NULL
  )
p1
p1<-p1 + list(
  geom_point(aes(color=sex),size=2),
  geom_smooth(method=lm,se=FALSE, color="black"),
  scale_color_manual(values=c("steelblue2","tan")),
  NULL
)
p1
#plot residuals
p2<-ggplot(tarmass,aes(x=avg_tarsus,y=residual, color=population))+
  list(
    geom_point(size=2,pch=1),
    geom_hline(aes(yintercept=0),color="black"),
    ylab("Residuals"),
    xlab("Average tarsus length(mm)"),
    ggtitle("Residual plot"),
    NULL
  )

#boxplots of sex
p3<-ggplot(data=tarmass, aes(x=sex,y=residual))+ geom_boxplot()

#violin plot of population and cycle
p4 <- ggplot(tarmass, aes(x=population, y=residual)) + 
  list(
    geom_violin(aes(fill=cycle),position=position_dodge(1)),
    geom_dotplot(aes(color=cycle),binaxis='y', stackdir='center', dotsize=0.25, position=position_dodge(1), fill="white", bin),
    geom_boxplot(aes(fill=cycle),width=0.2,position=position_dodge(1)),
    scale_fill_manual(values=c("steelblue2","tan")),
    xlab("Population"),
    ylab("Condition index (Residuals)"),
    NULL
  )
p4
#violin plot of population and sex
p5 <- ggplot(tarmass, aes(x=population, y=residual)) + 
  list(
    geom_violin(aes(fill=sex),position=position_dodge(1)),
    geom_boxplot(aes(fill=sex),width=0.2,position=position_dodge(1)),
    scale_fill_manual(values=c("pink","turquoise")),
    NULL
  )
p5

