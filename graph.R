rwild<-read.csv("C:/Users/Vicky-T/Documents/R/urb_recap_mass_sort.csv", header = TRUE)
rwild$date_weighed=as.Date(rwild$date_weighed)
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
#import and correctly edit rwild
rwild-> rwild.l
rwild.l$ID<-as.numeric(factor(rwild.l$band_no1,levels=unique(rwild.l$band_no1)))
#creates a numeric ID for each unique band number, that is smaller (starts at 1)

#####The below graph works, but is hideous!USE GGPLOT2 INSTEAD #########
nbirds<-max(rwild.l$ID)
xrange<-range(rwild.l$week)
yrange<-range(rwild.l$mass,na.rm=TRUE)
plot(xrange, yrange, type="n", xlab="week of study",
     ylab="Mass (g)" )
colors <- rainbow(nbirds)
linetype <- c(1:nbirds)
#plotchar <- seq(18,18+nbirds,1)

for (i in 1:nbirds) {
  bird <- subset(rwild.l, ID==i)
  lines(bird$week, bird$mass, type="b", lwd=1.5,
        lty=linetype[i], col=colors[i])
} 

title("Birds over time", "testing testing")
legend(xrange[1], yrange[2], 1:nbirds, cex=0.8, col=colors,
       lty=linetype, title="Bird")

####GGPLOT2
#working bare bones ggplot - lines per indiv.
library(ggplot2)
rwild.l$ID<-as.factor(rwild.l$ID)
ggplot(data = rwild.l, aes(x=week,y=mass)) +
  list(
    geom_line(aes(colour=ID)),
    geom_point(aes(colour=ID)),
    scale_x_continuous(breaks=c(1:10), labels=c("Pre-study","1","2","3","4","5","6","7","8","9")),
    NULL
)


##working with background bars for dirty-clean cycles  
#NOTE: these dirty-clean bars are for URBAN/WILD birds only.
ggplot(data = rwild.l, aes(x=week,y=mass)) +
  list(
    annotate("rect",xmin=-Inf,xmax=3,ymin=-Inf,ymax=Inf, fill ="red", alpha = 0.1),
    annotate("rect",xmin=3,xmax=5,ymin=-Inf,ymax=Inf, fill ="blue", alpha = 0.1),
    annotate("rect",xmin=5,xmax=7,ymin=-Inf,ymax=Inf, fill ="red", alpha = 0.1),
    annotate("rect",xmin=7,xmax=9,ymin=-Inf,ymax=Inf, fill ="blue", alpha = 0.1),
    annotate("rect",xmin=9,xmax=Inf,ymin=-Inf,ymax=Inf, fill ="red", alpha = 0.1),
    geom_line(aes(colour=ID)),
    geom_point(aes(colour=ID)),
    scale_x_continuous(breaks=c(0:9), labels=c("Pre-study","1","2","3","4","5","6","7","8","9")),
    ylab("Mass(g)"),
    xlab("Week of the study"),
    theme(legend.position = "none"),
    NULL
  )