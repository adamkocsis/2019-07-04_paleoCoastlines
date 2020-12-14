# Paleocoastlines - paper.
# 2_prepare_PBDB.R - The purpose of this script is to load the updated paleocoastlines
# and continental margin shapefiles into a more conveniently useable format 
# (binary .RData files).
# exported as results. 
# 2020-08-09. 
# Ádám T. Kocsis, Erlangen, CC-BY

library(rgdal)
library(rgeos)
library(restools)
library(raster)

################################################################################
workdir<-file.path(Sys.getenv("WorkSpace"), "2019-07-04_paleoCoastlines", sep="")
	setwd(workdir)
	funky()

# session version
ver <- "paper_2020-07-29"
	setver(ver)
	results()

# MOST UP-TO-DATE shapes	
datpath <- "data/PaleoMAP/2020-08-10_v7"

# 1. load the coastlines
	all <- list.files(file.path(datpath, "CS"))
	csSHX <- all[grep(".shx",all)]
	
	# create a numeric vector
	csAge <- as.numeric(unlist(lapply(strsplit(csSHX, "Ma"), function(x) x[1])))

	# reorder everything to make sure things are really aligned
	csSHX <- csSHX[order(csAge)]
	csName <- unlist(lapply(strsplit(csSHX, "\\."), function(x) x[1]))
	csAge <- csAge[order(csAge)]

	# append directory name to the coastline paths
	csSHX <- file.path( datpath, "CS", csSHX)

# 2. Load the continental margins
	all <- list.files(file.path(datpath, "CM"))
	cmSHX <- all[grep(".shx",all)]
	
	# create a numeric vector
	cmAge <- as.numeric(unlist(lapply(strsplit(cmSHX, "Ma"), function(x) x[1])))

	# get those entries for which we have coastlines
	names(cmSHX) <- cmAge
	cmSHX<- cmSHX[as.character(csAge)]

	# get layer name without extension
	cmName <- unlist(lapply(strsplit(cmSHX, "\\."), function(x) x[1]))
	
	# append directory name to the CS paths
	cmSHX <- file.path(datpath, "CM", cmSHX)

	# the whole Earth to create an inverse from
		# matrix of points
		whole <- matrix(
			c(
				180, 90,
				180, -90,
				-180, -90,
				-180,90
	
			), ncol=2, byrow=TRUE)

		# spdf object
		earth <- SpatialPolygonsDataFrame(
			SpatialPolygons(list(Polygons(list(Polygon(whole)), ID="1"))),
			data=data.frame(1)
		)
		earth@proj4string <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")


##################################################################################
# I. Equirectangular plots
	# storage containers
	lWater <- list()
	lCoast <- list()
	lPlates <- list() 
	lShelf <- list()

# I.A. PDF
savepdf("Summary/Data/equirectangular.pdf", height=10, width=20)
	for(i in 1:length(csName)){
		# load the coastlines
		lCoast[[i]] <- readOGR(csSHX[i], csName[i])
		
		# load the isobaths
		lPlates[[i]]  <-readOGR(cmSHX[i], cmName[i])
		
		# plot these two to make sure that everything looks alright
		pairplot(lPlates[[i]], lCoast[[i]], pdf=F,age=csAge[i])
		rect(xleft=-180, xright=180, ytop=90, ybottom=-90, col=NA, border="black")

		# the shelf area
		lShelf[[i]] <- gDifference(lPlates[[i]], lCoast[[i]])

		# the difference between the whole earth and the rest of coast
		lWater[[i]] <- gDifference(earth, lCoast[[i]])

#		# plot the present day coastlines too
#		plot(presPL[[as.character(csAge[i])]], add=TRUE)
#		plot(presPG[[as.character(csAge[i])]], add=TRUE)

		cat(i, "\r")
		flush.console()
	}

dev.off()


# I. B. PNG
for(i in 1:length(csName)){
	# .png equirectangular

	savepng(paste0("Summary/Data/png_equirect/equirectangular_",as.character(csAge[i]),".png"), height=1800, width=3600)
	
	# plot these two to make sure that everything looks alright
	pairplot(lPlates[[i]], lCoast[[i]], pdf=F,age=NULL)
	rect(xleft=-180, xright=180, ytop=90, ybottom=-90, col=NA, border="black")
	
	# plot the present day coastlines too
	plot(presPL[[as.character(csAge[i])]], add=TRUE)
	plot(presPG[[as.character(csAge[i])]], add=TRUE)
	cat(i, "\r")
	flush.console()
	dev.off()
}

## II. Molweide
## II.A. pdf

savepdf("Summary/Data/mollweide.pdf", height=10, width=18)
for(i in 1:length(csName)){
	# .png equirectangular
	
	proj <- CRS("+proj=moll")

	# correct edges
	cm <- PolyResample(lPlates[[i]], mi=3)
	cs <- PolyResample(lCoast[[i]], mi=3)

	# the edge of the map
	frameSP <- spTransform(frameSPlonglat, proj)

	# transform the basic shapes
	cm <- spTransform(cm, proj)
	cs <- spTransform(cs, proj)

		
	plot(frameSP, border="black", col=NA, main=paste0("Age: ", csAge[i], "Ma"))
	pairplot(cm, cs, pdf=F, age="", xlim=NULL, ylim=NULL, add=T)
	plot(frameSP, border="gray40", col=NA, add=T, lwd=2)

	cat(i, "\r")
	flush.console()
	
}
dev.off()



# II.B. png - mollweide
for(i in 1:length(csName)){
	# .png equirectangular

	savepng(paste0("Summary/Data/png_mollweide/mollweide_",as.character(csAge[i]),".png"), height=1800, width=3600)

	proj <- CRS("+proj=moll")

	# correct edges
	cm <- PolyResample(lPlates[[i]], mi=3)
	cs <- PolyResample(lCoast[[i]], mi=3)

	# the edge of the map
	frameSP <- spTransform(frameSPlonglat, proj)

	# transform the basic shapes
	cm <- spTransform(cm, proj)
	cs <- spTransform(cs, proj)

		
	plot(frameSP, border="black", col=NA)
	pairplot(cm, cs, pdf=F, age="", xlim=NULL, ylim=NULL, add=T)
	plot(frameSP, border="gray20", col=NA, add=T, lwd=3)

	pl <- presPL[[as.character(csAge[i])]]
	pg <- presPG[[as.character(csAge[i])]]

	# add proj4 string
	pl@proj4string <- CRS("+proj=longlat")
	pg@proj4string <- CRS("+proj=longlat")

	pl <- spTransform(pl, proj)
	pg <- spTransform(pg, proj)

	# plot the present day coastlines too
	plot(pl, add=TRUE)
	plot(pg, add=TRUE)
	cat(i, "\r")
	flush.console()
	dev.off()

}




# # PROJ4 strings should still be corrected!!!!

# rename and store the final objects
names(lPlates) <- csAge
stores(lPlates)
names(lCoast) <- csAge
stores(lCoast)
names(lShelf) <- csAge
stores(lShelf)
names(lWater) <- csAge
stores(lWater)
	
# plates proper
plateModel <- fetch("paleomap", "model", ver="v19o_r1c", datadir=file.path(workdir, "data/chronosphere"))
lPlateProper <- recontstruct("plates", age=as.numeric(names(lPlates)), mod=plateModel)
stores(lPlateProper)








