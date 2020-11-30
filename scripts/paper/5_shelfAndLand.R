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

# 1. calculate shelf area -
#shelfArea <- AreaOverTime(lShelf, icosa=NULL, proj="+proj=cea")


# 2. VALIDATE with ICOSA
#	# shelf area
#	savepdf("shelfArea.pdf", width=20, height=10)
#		shelfArea <- AreaOverTime(lShelf, icosa=c(4,10), plot=TRUE)
#	dev.off()
#	savepdf("landAreaIcosa.pdf", width=20, height=10)
#		landAreaNew <- AreaOverTime(lCoast, icosa=c(4,10), plot=TRUE)
#	dev.off()

#timeplot(ylab="Proportion of flooded shelf area", ylim=c(0, 0.25))
#lines(as.numeric(rownames(shelfArea)), shelfArea$rgeos, col="red")
#lines(as.numeric(rownames(shelfArea)), shelfArea$icosa, col="green")


savepdf("ceaLand.pdf")
	landAreaCEA <- AreaOverTime(lCoast, icosa=NULL, proj="+proj=cea", rgeosplot=TRUE)
dev.off()



# 3. Proper contrast
shelfAreaOld <- AreaOverTime(demShelf, icosa=NULL, proj="+proj=cea")
stores(shelfAreaOld)
shelfAreaNew <- AreaOverTime(lShelf, icosa=NULL, proj="+proj=cea")
stores(shelfAreaNew)

savepdf("shelfArea.pdf", width=7, height=6)
timeplot(ylab="Proportion of flooded shelf area", ylim=c(0, 0.25))
polylines(as.numeric(rownames(shelfAreaNew)), shelfAreaNew$rgeos, col="#87cef6AA",  border="black", last=550)
lines(as.numeric(rownames(shelfAreaOld)), shelfAreaOld$rgeos, col="black",  lty=2)
legend("bottomleft", inset=c(0.03, 0.1), legend=c("PaleoDEMs", "Updated Paleocoastlines")
	, bg="white", cex=0.8, lty=c(2,1))
box()
dev.off()




# land areae
landAreaOld <- AreaOverTime(dem0, icosa=NULL, proj="+proj=cea")
stores(landAreaOld)
landAreaNew <- AreaOverTime(lCoast, icosa=NULL, proj="+proj=cea")
stores(landAreaNew)

savepdf("landArea.pdf", width=7, height=6)
timeplot(ylab="Proportion of land area", ylim=c(0, 0.4))
polylines(as.numeric(rownames(landAreaNew)), landAreaNew$rgeos, col="#94391cAA",  border="black", last=550)
lines(as.numeric(rownames(landAreaOld)), landAreaOld$rgeos, col="black", lty=2)
legend("bottomright", inset=c(0.03, 0.1), legend=c("PaleoDEMs", "Updated Paleocoastlines")
	, col=c("black", "black"),bg="white", cex=0.8, lty=c(2,1))
box()
dev.off()


# check land area with icosa


# belts
# SHELF
shelfBelts <- AreaInBelts(lShelf, plot="shelfbelts30.pdf")
shelfBeltCumul <- cumulcol(shelfBelts)
stores(shelfBelts)
stores(shelfBeltCumul)


savepdf("shelfBeltOverTime.pdf", width=7, height=6)
shelfCols <- c("#c3e0fa", "#65aff1", "#0086ff", "#0086ff", "#65aff1", "#c3e0fa")
timeplot(ylab="Proportion of Earth's area", ylim=c(0, 0.25))
for(i in 1:ncol(shelfBeltCumul)){
	if(i==1){
		bottom <- 0
	}else{
		bottom <- shelfBeltCumul[,i-1]
	}

	polylines(x=as.numeric(rownames(shelfBeltCumul)), bottom=bottom, 
		y= shelfBeltCumul[,i], col=paste0(shelfCols[i], 99), last=550, border=NA)
}
lines(as.numeric(rownames(shelfBeltCumul)),shelfBeltCumul[,3], lwd=2, col="#0086ffAA")
lines(as.numeric(rownames(shelfBeltCumul)),shelfBeltCumul[,i], lwd=2)
legend("topright", inset=c(0.25,0.05), legend=c("Tropical (0-30°)", "Temperate (30-60°)", "Polar (60-90°)"),
	fill=c("#0086ff99", "#65aff199", "#c3e0fa99"), bg="white")
#lines(as.numeric(rownames(shelfAreaOld)), shelfAreaOld$rgeos, col="red", lty=2)
box()
par(xpd=TRUE)
text(x=-10, y=shelfBeltCumul[1,], label=paste0(c(-60,-30, 0, 30, 60, 90), "°"), cex=0.5)
dev.off()

# LAND
landBelts <- AreaInBelts(lCoast, plot="landbelts30.pdf")
landBeltCumul <- cumulcol(landBelts)
stores(landBelts)
stores(landBeltCumul)

savepdf("landBeltOverTime.pdf", width=7, height=6)
landCols <- c("#bb8572", "#b85a3a", "#ba2f01", "#ba2f01", "#b85a3a", "#bb8572")
timeplot(ylab="Proportion of Earth's area", ylim=c(0, 0.35))
for(i in 1:ncol(landBeltCumul)){
	if(i==1){
		bottom <- 0
	}else{
		bottom <- landBeltCumul[,i-1]
	}

	polylines(x=as.numeric(rownames(landBeltCumul)), bottom=bottom, 
		y= landBeltCumul[,i], col=paste0(landCols[i], "99"), last=550, border=NA)
}
lines(as.numeric(rownames(landBeltCumul)),landBeltCumul[,3], lwd=2, col="#ba2f01aa")
lines(as.numeric(rownames(landBeltCumul)),landBeltCumul[,i], lwd=2)
legend("topleft", inset=c(0.05,0.08), legend=c("Tropical (0-30°)", "Temperate (30-60°)", "Polar (60-90°)"),
	fill=c("#ba2f0199", "#b85a3a99", "#bb857299"), bg="white")
#lines(as.numeric(rownames(shelfAreaOld)), shelfAreaOld$rgeos, col="red", lty=2)
box()
par(xpd=TRUE)
text(x=-10, y=landBeltCumul[1,], label=paste0(c(-60,-30, 0, 30, 60, 90), "°"), cex=0.5)
dev.off()




######################################################
# MODELLING
time <- as.numeric(rownames(shelfAreaNew))
both <- landAreaNew$rgeos+shelfAreaNew$rgeos

land <- landAreaNew$rgeos
shelf <- shelfAreaNew$rgeos
landCol <- landCols[1]
shelfCol <- shelfCols[2]

# correlations?
cor.test(landAreaNew$rgeos, shelfAreaNew$rgeos)

# LAND
cor.test(land, both)
mod <- lm(land ~ time)

# percent
coef(mod)[2]*100

# percent /100my
coef(mod)[2]*100*100



# BOTH
# model


savepdf("both.pdf", width=7, height=6)
	# colors used before
	landCol <- "#b85a3a"
	shelfCol <- "#65aff1"

	# variables used
	time <- as.numeric(rownames(shelfAreaNew))
	both <- landAreaNew$rgeos+shelfAreaNew$rgeos
	
	# simple corr test andmodel
	cor.test(time, both)
	mod <- lm(both ~ time)

	# percent
	coef(mod)[2]*100

	# percent /100my
	coef(mod)[2]*100*100

	timeplot(ylab="Proportion of Earth's surface area", ylim=c(0, 0.5))
	polylines(x=as.numeric(rownames(landAreaNew)), bottom=0, 
			y= landAreaNew$rgeos, col=paste0(landCol, 99), last=550, border=NA)
	polylines(x=as.numeric(rownames(shelfAreaNew)), bottom=landAreaNew$rgeos, 
			y= shelfAreaNew$rgeos+landAreaNew$rgeos, col=paste0(shelfCol, 99), last=550, border=NA)
	
	#lines(as.numeric(rownames(shelfAreaOld)),(landAreaOld[rownames(shelfAreaOld), "rgeos"]+shelfAreaOld$rgeos), lwd=2, col="red")
	lines(as.numeric(rownames(landAreaNew)),(landAreaNew$rgeos+shelfAreaNew$rgeos), lwd=2)
	
	
	abline(mod, lty=2)
	legend("bottomright", inset=c(0.1,0.1), legend=c("Land", "Flooded shelf"), fill=c(landCol, shelfCol), bg="white")
	
	text(x=100, y=0.45, label=paste0(round(coef(mod)[1],2),"", round(coef(mod)[2],6), " x age"))
dev.off()
