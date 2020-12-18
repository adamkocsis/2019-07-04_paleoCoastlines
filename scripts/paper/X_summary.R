
library(rgdal)
library(chronosphere)
library(restools)
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

#####################################
# PBDB sumamry statistics
length(unique(paleoColls[paleoColls$marine, "collection_no"]))
length(unique(paleoColls[!paleoColls$marine, "collection_no"]))

#####################################


# Main display items
# Fig 1 - Paleocoastlines with occurrences - all_maps
# Final plots!
# get the set of dems
dems <- fetch("paleomap", "dem",res=0.5 ,datadir="data/chronosphere/")

# Old- before corrections
dummyScore <- Score(
	margins=dem1400,
	coasts=dem0, 
	pbdb=paleoColls,
	plot="Summary/oldCollPlots.pdf",
	icosa=NULL,
	buffer=0,
	decide="marine", legbin=c(22, 43, 61, 85, 94),
	method="demmoll",bg=dems
#	,presPoly=presPG, presLine=presPL,
#	presbin=c(4, 25,  46, 67, 88)
)

dummyScore <- Score(
	margins=lPlates,
	coasts=lCoast, 
	pbdb=paleoColls,
	plot="Summary/newCollPlots.pdf",
	icosa=NULL,
	buffer=0,
	decide="marine", legbin=c(24, 45, 66, 87, 95),
	method="changemoll", oldCoast=dem0
#	,presPoly=presPG, presLine=presPL,
#	presbin=c(4, 25,  46, 67, 88)
)




# Accuracy of marine 
pan21(
	name=paste0("Summary/Fig_2_marine_score"), 
	a=expression({
		timeplot(ylab="Proportion of marine collections", 
			plot.args=list(main="PaleoDEMs before corrections"))
		polylines(stages$mid, marineOldScore$propCollsOnShelf, col="#6296d6AA", first=550)
		polylines(x=stages$mid, bottom=marineOldScore$propCollsOnShelf, 
			y= marineOldScore$propCollsOnShelf+marineOldScore$propCollsOnLand, col="#d97b7b99", first=550)
		lines(stages$mid, marineOldScore100$propCollsOnShelf, lty=2)

	}),
	b=expression({	
		timeplot(ylab="Proportion of marine collections",plot.args=list(main="Updated Paleocoastlines"))
		polylines(stages$mid, marineNewScore$propCollsOnShelf, col="#6296d6AA", first=550)
		polylines(x=stages$mid, bottom=marineNewScore$propCollsOnShelf, 
			y= marineNewScore$propCollsOnShelf+marineNewScore$propCollsOnLand, col="#d97b7b99", first=550)
		lines(stages$mid, marineNewScore100$propCollsOnShelf, lty=2)
		legend("bottomleft", inset=c(0.1, 0.1), legend=c("Continental flooding", "Land"),
			fill=c("#6296d6AA", "#d97b7b99"), cex=2, bg="white")
	}), 
	format="pdf", 
	width=c(7), height=c(6,6),
	intop=1,pind=c("A", "B")
)

# Scores calculated before
# OLD collections
	# total in percentage (in marine)
	sum(marineOldScore$collsOnShelf, na.rm=T)/sum(marineOldScore$collections, na.rm=T)
	
	# With the buffer total in percentage (in marine)
	sum(marineOldScore100$collsOnShelf, na.rm=T)/sum(marineOldScore100$collections, na.rm=T)
	

	# in terrestrial
	sum(terrOldScore$collsOnLand, na.rm=T)/sum(terrOldScore$collections, na.rm=T)
	
	# mixed
	(sum(marineOldScore$collsOnShelf, na.rm=T)+
	sum(terrOldScore$collsOnLand, na.rm=T)
		)/(sum(marineOldScore$collections, na.rm=T)+sum(terrOldScore$collections, na.rm=T))
	

# NEW collections
	# total in percentage (in marine)
	sum(marineNewScore$collsOnShelf, na.rm=T)/sum(marineNewScore$collections, na.rm=T)
	sum(marineNewScore100$collsOnShelf, na.rm=T)/sum(marineNewScore100$collections, na.rm=T)

	# total in percentage (in terrestrial)
	sum(terrOldScore$collsOnLand, na.rm=T)/sum(terrOldScore$collections, na.rm=T)
	sum(terrNewScore$collsOnLand, na.rm=T)/sum(terrNewScore$collections, na.rm=T)

#--------------------------------------------------------------------------------------------------
# Figs 3-10: Coastline reconstructions (3_udpatedShapes.R)

#--------------------------------------------------------------------------------------------------
# Figs 11: Coastline length 

# coastline lengths wit various stick lengths
savepdf("Summary/Fig_11_coastlineUpdate.pdf", width=7, height=6)
	timeplot(ylab="Coastline length (km) measured with 500km yardsticks", ylim=c(0,250000))
	lines(as.numeric(rownames(oldSticks)), oldSticks[,2], col="red",  lwd=2, lty=2)
	lines(as.numeric(rownames(newSticks)), newSticks[,2], col="black",  lwd=2)
	legend("topleft", inset=c(0.05, 0.05), col=c("red", "black"), lwd=2, lty=c(2,1),
		legend=c("PaleoDEM coastlines (uncorrected)", "Updated paleocoastlines"), bg="white")
dev.off()


#--------------------------------------------------------------------------------------------------
# Figs 12: Areas over time

pan22(
	name="Summary/Fig_12_areaOverTime", 
	a=expression({
		timeplot(ylab="Proportion of Earth's surface area", ylim=c(0, 0.25))
		polylines(as.numeric(rownames(shelfAreaNew)), shelfAreaNew$rgeos, col="#c3e0faAA",  border="black", last=550)
		lines(as.numeric(rownames(shelfAreaNew)), shelfAreaNew$rgeos, lwd=2)
		lines(as.numeric(rownames(shelfAreaOld)), shelfAreaOld$rgeos, col="red",  lty=2, lwd=2)
		text(x=530, y=0.23, label=c("Flooded continental area"), pos=4, cex=2)
		box()
	}),
	b=expression({
		shelfCols <- c("#c3e0fa", "#65aff1", "#0086ff", "#0086ff", "#65aff1", "#c3e0fa")
		timeplot(ylab="", ylim=c(0, 0.25))
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
		legend("topright", inset=c(0.18,0.05), legend=c(
			expression(paste("Tropical (0-30",degree,")")), 
			expression(paste("Temperate (30-60",degree,")")), 
			expression(paste("Polar (60-90",degree,")"))),
			fill=c("#0086ff99", "#65aff199", "#c3e0fa99"), cex=1.5,bg="white")
		#lines(as.numeric(rownames(shelfAreaOld)), shelfAreaOld$rgeos, col="red", lty=2)
		box()
		par(xpd=TRUE)
	#	text(x=-10, y=shelfBeltCumul[1,], label=paste0(c(-60,-30, 0, 30, 60, 90), "°"), cex=1)
		# platform-indpeendent solution
		degs <- c(-60,-30, 0, 30, 60, 90)
		for(i in 1:ncol(shelfBeltCumul)){
			text(x=-10, y=shelfBeltCumul[1,i], label=bquote(.(degs[i])*degree), cex=1)
		}


		par(xpd=FALSE)
		text(x=530, y=0.23, label=c("Flooded continental area"), pos=4, cex=2)
		
	}), 
	c=expression({
		timeplot(ylab="Proportion of Earth's surface area", ylim=c(0, 0.4))
		polylines(as.numeric(rownames(landAreaNew)), landAreaNew$rgeos, col="#bb8572AA",  border="black", last=550, lwd=1)
		lines(as.numeric(rownames(landAreaNew)), landAreaNew$rgeos, lwd=2)
		lines(as.numeric(rownames(landAreaOld)), landAreaOld$rgeos, col="red", lty=2, lwd=2)
		text(x=530, y=0.36, label=c("Land"), pos=4, cex=2)
		legend("bottomright", inset=c(0.03, 0.1), legend=c("PaleoDEMs", "Updated Paleocoastlines")
			, bg="white", cex=1.5, lty=c(2,1), col=c("red", "black"), lwd=2)
		
		box()
	}), 
	d=expression({
		landCols <- c("#bb8572", "#b85a3a", "#ba2f01", "#ba2f01", "#b85a3a", "#bb8572")
		timeplot(ylab="", ylim=c(0, 0.4))
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
		legend("topleft", inset=c(0.05,0.15), legend=c(
			expression(paste("Tropical (0-30",degree,")")), 
			expression(paste("Temperate (30-60",degree,")")), 
			expression(paste("Polar (60-90",degree,")"))),
			fill=c("#ba2f0199", "#b85a3a99", "#bb857299"), cex=1.5,bg="white")
		#lines(as.numeric(rownames(shelfAreaOld)), shelfAreaOld$rgeos, col="red", lty=2)
		box()
		par(xpd=TRUE)
	#	text(x=-10, y=landBeltCumul[1,], label=paste0(c(-60,-30, 0, 30, 60, 90), "°"), cex=1)
		# platform-indpeendent solution
		degs <- c(-60,-30, 0, 30, 60, 90)
		for(i in 1:ncol(landBeltCumul)){
			text(x=-10, y=landBeltCumul[1,i], label=bquote(.(degs[i])*degree), cex=1)
		}
		par(xpd=FALSE)
		text(x=530, y=0.36, label=c("Land"), pos=4, cex=2)
		
	}), 
	format="pdf", 
	width=c(7,7), height=c(6,6),
	intop=0.4,pind=c("A", "B", "C", "D"), inright=0.1, inleft=0.7,pcx=0.6
)

#############################################################################################x
# Fig. S11: Areas over time

# TERRESTRIAL accuracy
pan21(
	name=paste0("Summary/Fig_S11_terrestrial_score"), 

	a=expression({
		timeplot(ylab="Proportion of terrestrial collections", plot.args=list(main="PaleoDEMs before corrections"))
		polylines(stages$mid, terrOldScore$propCollsOnLand, col="#d97b7b99", first=NULL)
		polylines(x=stages$mid, bottom=terrOldScore$propCollsOnLand, 
			y= terrOldScore$propCollsOnShelf+terrOldScore$propCollsOnLand, col="#6296d6AA", first=NULL)
		lines(stages$mid, terrOldScore100$propCollsOnLand, lty=2)

	}),
	b=expression({		
		timeplot(ylab="Proportion of terrestrial collections",plot.args=list(main="Updated Paleocoastlines"))
		polylines(stages$mid, terrNewScore$propCollsOnLand, col="#d97b7b99", first=NULL)
		polylines(x=stages$mid, bottom=terrNewScore$propCollsOnLand, 
			y= terrNewScore$propCollsOnLand+terrNewScore$propCollsOnShelf, col="#6296d6AA", first=NULL)
		lines(stages$mid, terrNewScore100$propCollsOnLand, lty=2)
		legend("bottomright", inset=c(0.1, 0.1), legend=c("Flooded area", "Land"), fill=c("#6296d6AA", "#d97b7b99"), cex=1.5, bg="white")
	}), 
	format="pdf", 
	width=c(7), height=c(6,6),
	intop=1,pind=c("A", "B")
)

# coastline lengths wit various stick lengths
colInd <- c(1,2,4,5)
savepdf("Summary/Fig_S12_coastlineTime.pdf", width=7, height=6)
	timeplot(ylab="Coastline length (km)", ylim=c(0,250000))
	for (i in 1:ncol(newSticks))lines(as.numeric(rownames(newSticks)), newSticks[,i], col=gradinv(5)[colInd[i]],  lwd=2)
	legend("topleft", inset=c(0.05, 0.05), col=gradinv(5)[colInd], lwd=2, 
		legend=paste(sticks, "km"), bg="white")
dev.off()


savepdf("Summary/Fig_S13_bothArea.pdf", width=7, height=6)
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
	legend("bottomright", inset=c(0.1,0.1), legend=c("Land", "Flooded areas"), fill=c(landCol, shelfCol), bg="white")
	
	text(x=100, y=0.45, label=paste0(round(coef(mod)[1],2),"", round(coef(mod)[2],6), " x age"))
dev.off()
