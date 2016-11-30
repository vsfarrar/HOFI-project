rwild-> rwild.l
rwild.l$ID<-as.numeric(factor(rwild.l$band_no1,levels=unique(rwild.l$band_no1)))
#creates a numeric ID for each unique band number, that is smaller (starts at 1)

#The below graph works, but is hideous!
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