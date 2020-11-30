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




#	# Marine OldCols vs Marine NewColls
write.table(marineOldScore, file=file.path("export", ver, "marineOldScore.csv"), row.names=TRUE, sep=",")
write.table(marineNewScore, file=file.path("export", ver, "marineNewScore.csv"), row.names=TRUE, sep=",")
write.table(terrOldScore, file=file.path("export", ver, "terrOldScore.csv"), row.names=TRUE, sep=",")
write.table(terrNewScore, file=file.path("export", ver, "terrNewScore.csv"), row.names=TRUE, sep=",")
