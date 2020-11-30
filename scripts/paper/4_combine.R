# Paleocoastlines - paper.
# 1_demToShapes.R - The purpose of this script is to get the original
# plate margins and coastlines implied by the PaleoDEMs. These are 
# exported as results. 
# 2020-08-09. 
# Ádám T. Kocsis, Erlangen, CC-BY

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


##############################################################################
# C. Put collections on the maps
# 1. Old shapefiles 
# 1a. Marine collections
marineOldScore <- Score(
	margins=dem1400,
	coasts=dem0, 
	pbdb=paleoColls[paleoColls$marine,],
	plot="marineOldColls.pdf",
	icosa=NULL,
	buffer=0
)
stores(marineOldScore)

# 1b. Terrestrial collections
terrOldScore <- Score(
	margins=dem1400,
	coasts=dem0, 
	pbdb=paleoColls[!paleoColls$marine,],
	plot="terrOldColls.pdf",
	icosa=NULL,
	buffer=0
)
stores(terrOldScore)

# 2. New shapefiles 
# 2a. Marine collections
marineNewScore <- Score(
	margins=lPlates,
	coasts=lCoast, 
	pbdb=paleoColls[paleoColls$marine,],
	plot="marineNewColls.pdf",
	icosa=NULL,
	buffer=0
)
stores(marineNewScore)

# 2b. Terrestrial collections
terrNewScore <- Score(
	margins=lPlates,
	coasts=lCoast, 
	pbdb=paleoColls[!paleoColls$marine,],
	plot="terrNewColls.pdf",
	icosa=NULL,
	buffer=0
)
stores(terrNewScore)

# total number of collections ()
nCollMarine <- length(unique(paleoColls[paleoColls$marine, "collection_no"]))
nCollTerrestrial <- length(unique(paleoColls[!paleoColls$marine, "collection_no"]))


timeplot(ylab="Proportion of marine collections falling on shelf area")
lines(stages$mid, marineOldScore$propCollsOnShelf)
lines(stages$mid, marineNewScore$propCollsOnShelf, lwd=2)

#########################################x
# Buffered
# 1a. Marine collections
marineOldScore200 <- Score(
	margins=dem1400,
	coasts=dem0, 
	pbdb=paleoColls[paleoColls$marine,],
	plot="marineOldColls200.pdf",
	icosa=NULL,
	buffer=200
)
stores(marineOldScore200)

# 2. New shapefiles 
# 2a. Marine collections
marineNewScore200 <- Score(
	margins=lPlates,
	coasts=lCoast, 
	pbdb=paleoColls[paleoColls$marine,],
	plot="marineNewColls200.pdf",
	icosa=NULL,
	buffer=100
)
stores(marineNewScore200)


# 1a. Terrestrial collections
terrOldScore100 <- Score(
	margins=dem1400,
	coasts=dem0, 
	pbdb=paleoColls[!paleoColls$marine,],
	plot="terrOldOldColls100.pdf",
	icosa=NULL,
	buffer=100
)
stores(terrOldScore100)

# 2. New shapefiles 
# 2a. Marine collections
terrNewScore100 <- Score(
	margins=lPlates,
	coasts=lCoast, 
	pbdb=paleoColls[!paleoColls$marine,],
	plot="terrNewColls100.pdf",
	icosa=NULL,
	buffer=100
)
stores(terrNewScore100)


timeplot(ylab="Proportion of marine collections falling on shelf area")
lines(stages$mid, marineOldScore$propCollsOnShelf)
lines(stages$mid, marineNewScore$propCollsOnShelf, lwd=2)
lines(stages$mid, marineOldScore100$propCollsOnShelf, lty=2)
lines(stages$mid, marineNewScore100$propCollsOnShelf, lwd=2, lty=2)

# MARINE accuracy
# version 1 - two panel plot comparing the original vs the new one (main text)
savepdf("scoreDirect_1.pdf", width=7, height=11)
par(mfrow=c(2,1))
timeplot(ylab="Proportion of marine collections", plot.args=list(main="Paleocoastlines implied by PaleoDEMs"))
polylines(stages$mid, marineOldScore$propCollsOnShelf, col="#6296d6AA", first=550)
polylines(x=stages$mid, bottom=marineOldScore$propCollsOnShelf, 
	y= marineOldScore$propCollsOnShelf+marineOldScore$propCollsOnLand, col="#d97b7b99", first=550)
lines(stages$mid, marineOldScore100$propCollsOnShelf, lty=2)

timeplot(ylab="Proportion of marine collections",plot.args=list(main="Updated Paleocoastlines"))
polylines(stages$mid, marineNewScore$propCollsOnShelf, col="#6296d6AA", first=550)
polylines(x=stages$mid, bottom=marineNewScore$propCollsOnShelf, 
	y= marineNewScore$propCollsOnShelf+marineNewScore$propCollsOnLand, col="#d97b7b99", first=550)
lines(stages$mid, marineNewScore100$propCollsOnShelf, lty=2)
legend("bottomleft", inset=c(0.1, 0.1), legend=c("Flooded shelf", "Land"), fill=c("#6296d6AA", "#d97b7b99"), cex=1.5, bg="white")
dev.off()

# TERRESTRIAL accuracy
# version 1 - two panel plot comparing the original vs the new one (SOM
savepdf("scoreDirect_Terrestrial.pdf", width=7, height=11)
par(mfrow=c(2,1))
timeplot(ylab="Proportion of terrestrial collections", plot.args=list(main="Paleocoastlines implied by PaleoDEMs"))
polylines(stages$mid, terrOldScore$propCollsOnLand, col="#d97b7b99", first=NULL)
polylines(x=stages$mid, bottom=terrOldScore$propCollsOnLand, 
	y= terrOldScore$propCollsOnShelf+terrOldScore$propCollsOnLand, col="#6296d6AA", first=NULL)
lines(stages$mid, terrOldScore100$propCollsOnLand, lty=2)

timeplot(ylab="Proportion of terrestrial collections",plot.args=list(main="Updated Paleocoastlines"))
polylines(stages$mid, terrNewScore$propCollsOnLand, col="#d97b7b99", first=NULL)
polylines(x=stages$mid, bottom=terrNewScore$propCollsOnLand, 
	y= terrNewScore$propCollsOnLand+terrNewScore$propCollsOnShelf, col="#6296d6AA", first=NULL)
lines(stages$mid, terrNewScore100$propCollsOnLand, lty=2)
legend("bottomright", inset=c(0.1, 0.1), legend=c("Flooded shelf", "Land"), fill=c("#6296d6AA", "#d97b7b99"), cex=1.5, bg="white")
dev.off()


lines(stages$mid, marineNewScore$propCollsOnShelf, lwd=2)


# OLD collections
# total in percentage (in marine)
sum(marineOldScore$collsOnShelf, na.rm=T)/sum(marineOldScore$collections, na.rm=T)

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



#	# Marine OldCols vs Marine NewColls
#	# Select maps
#	Visean 340
#	Wuchiapingian 255
#	Bajocian		170
#	CEnomanian 95
#	Lutetian 45


write.table(marineOldScore, file=file.path("export", ver, "marineOldScore.csv"), row.names=TRUE, sep=",")
write.table(marineNewScore, file=file.path("export", ver, "marineNewScore.csv"), row.names=TRUE, sep=",")
write.table(terrOldScore, file=file.path("export", ver, "terrOldScore.csv"), row.names=TRUE, sep=",")
write.table(terrNewScore, file=file.path("export", ver, "terrNewScore.csv"), row.names=TRUE, sep=",")
