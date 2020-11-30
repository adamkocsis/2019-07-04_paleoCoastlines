# distance

library(rgdal)
library(chronosphere)
library(restools)
library(icosa)
library(rgeos)
library(divDyn)
data(stages)


# working dir
	workdir<-file.path(Sys.getenv("WorkSpace"), "2019-07-04_paleoCoastlines", sep="")
	setwd(workdir)

# load the functions
	funky()

# version
	ver <- "paper_2020-07-29"
	setver(ver)
	results()

# Testing: one reconstructions
# stick-length method
savepdf("stick0.pdf", width=20, height=10)
	st0 <- StickLength(lCoast[["0"]], stick=100)
dev.off()

# stick lengths used in this instance
sticks=c(300, 500, 1000, 1500)
stores(sticks)


# all the coastline lengths
# ORIGINAL coastlines
oldSticks <- AllSticks(dem0, name="stickCoastOld", sticks=sticks)
stores(oldSticks)

# Updated coastlines
newSticks <- AllSticks(lCoast, name="stickCoastNew",sticks=sticks)
stores(newSticks)

# Plotting of colorrs
colInd <- c(1,2,4,5)


# coastline lengths wit various stick lengths
savepdf("coastlineUpdate.pdf", width=7, height=6)
	timeplot(ylab="Coastline length (km) measured with 500km sticks", ylim=c(0,250000))
	lines(as.numeric(rownames(oldSticks)), oldSticks[,2], col="red",  lwd=2, lty=2)
	lines(as.numeric(rownames(newSticks)), newSticks[,2], col="black",  lwd=2)
	legend("topleft", inset=c(0.05, 0.05), col=c("red", "black"), lwd=2, lty=c(2,1),
		legend=c("Paleocoastlines implied by PaleoDEMs", "Updated paleocoastlines"), bg="white")
dev.off()


# coastline lengths wit various stick lengths
savepdf("coastlineTime.pdf", width=7, height=6)
	timeplot(ylab="Coastline length (km)", ylim=c(0,250000))
	for (i in 1:ncol(newSticks))lines(as.numeric(rownames(newSticks)), newSticks[,i], col=gradinv(5)[colInd[i]],  lwd=2)
	legend("topleft", inset=c(0.05, 0.05), col=gradinv(5)[colInd], lwd=2, 
		legend=paste(sticks, "km"), bg="white")
dev.off()

#########################################################################################
# one normally used shoreline length.
shore <-  newSticks[, 2] # 500km
# relationship to area
cor.test(, shelfAreaNew$rgeos)

# FIrst differences
cor.test(diff(newSticks[,3]), diff(shelfAreaNew$rgeos))

# as should be
cor.test(newSticks[,3], landAreaNew$rgeos)

######
cor.test(newSticks[1:25,3], shelfAreaNew$rgeos[1:25])
######
cor.test(diff(newSticks[1:25,3]), diff(shelfAreaNew$rgeos[1:25]))


mod<-lm(newSticks[,3] ~ landAreaNew$rgeos)

timeplot(ylab="Residuals coastline-length/land area", ylim=range(mod$residuals))
lines(as.numeric(rownames(newSticks)), mod$residuals, col="black",  lwd=2)
