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
library(inlmisc)

# working dir
	workdir<-file.path(Sys.getenv("WorkSpace"), "2019-07-04_paleoCoastlines")
	setwd(workdir)

# load the functions
	funky <- function(){
		a<- list.files("scripts/methods/")
		for(i in 1:length(a)) source(paste0("scripts/methods/", a[i]))
	} 
	funky()

# version
ver <- "paper_2020-07-29"
setver(ver)

# load the original dems
dems <- fetch("paleomap","dem", res=0.5, datadir=file.path(workdir, "data/chronosphere"))


##############################################################
# 1. Transform the Rasters to shapes


allAge <- names(dems)

# high resolution template for smoother results
HRtemp<-raster()
res(HRtemp)<-0.05
crs(HRtemp)<-crs(dems[1])

pdf(file.path("export", ver, "demToShape.pdf"), height=10, width=20)

for(i in 1:length(allAge)){

		# current dem 
		currDem <-dems[allAge[i]]
	
		# resample to get shelf environment on high slopes too
		demHR <- raster::resample(currDem, HRtemp)
		
	# 1. The continental margins
		# the continental margin
			plate <- demHR

		# the plate as presence absence
			raster::values(plate)[which(raster::values(plate)>= (-1400))]  <- 1
			raster::values(plate)[raster::values(plate)!=1] <- 0

	 	# convert to topographic polygons
			poly2<- inlmisc::Grid2Polygons(plate, cuts=1)

			# select land topography
			land <- poly2[2,]

		# smooth the raster corners
			land2 <- smoothr::smooth(land, method="ksmooth", smoothness=12)

			# simplify geometry
			land3 <- gSimplify(land2, tol=0.1)

			# standard shit
			land3@polygons[[1]]@ID <- "0"
			
			df <- data.frame(0)
			rownames(df)<-"0"
			land3df<-SpatialPolygonsDataFrame(land3, data=data.frame(df))
			plot(demHR)
			plot(land3, col="#55555599",  border="blue", add=TRUE)
		

			# save the end object
			lay <- paste(allAge[i], "Ma_DEM1400", sep="")
			writeOGR(land3df, dsn=paste("export/", ver, "/dem1400/",lay, ".shp",sep=""), layer=lay, driver="ESRI Shapefile")

	# 2. The coastlines
		# the continental margin
			coast <- demHR

		# the plate as presence absence
			raster::values(coast)[which(raster::values(coast)>= (0))]  <- 1
			raster::values(coast)[raster::values(coast)!=1] <- 0

	 	# convert to topographic polygons
			poly2<- inlmisc::Grid2Polygons(coast, cuts=1)

			# select land topography
			land <- poly2[2,]

		# smooth the raster corners
			land2 <- smoothr::smooth(land, method="ksmooth", smoothness=12)

			# simplify geometry
			land3 <- gSimplify(land2, tol=0.1)

			# standard shit
			land3@polygons[[1]]@ID <- "0"
			
			df <- data.frame(0)
			rownames(df)<-"0"
			land3df<-SpatialPolygonsDataFrame(land3, data=data.frame(df))
			plot(land3, col="#FFFFFF99",  border="brown", add=TRUE)
		

			# save the end object
			lay <- paste(allAge[i], "Ma_DEM0", sep="")
			writeOGR(land3df, dsn=paste("export/", ver, "/dem0/",lay, ".shp",sep=""), layer=lay, driver="ESRI Shapefile")


	cat("DEM to shape", i ,"\r")
	flush.console()
}
dev.off()

##############################################################
# 2. Load shapes and save them in binary lists

# plot and save everything 
pdf(file.path("export", ver, "demToShape_onlyShape.pdf"), height=10, width=20)

dem0 <- list()
dem1400 <- list()
demShelf <- list()
for(i in 1:length(allAge)){
	
	# coastlines
	lay <- paste(allAge[i], "Ma_DEM0", sep="")
	this <- readOGR(paste("export/", ver,"/dem0/", lay, ".shx",sep=""), layer=lay)
	dem0[[i]] <- this

	# dem1440
	lay <- paste(allAge[i], "Ma_DEM1400", sep="")
	that <- readOGR(paste("export/", ver,"/dem1400/", lay, ".shx",sep=""), layer=lay)
	dem1400[[i]] <- that

	demShelf[[i]] <- gDifference(that, this)

	# plot the entire set
#	plot(that, col="#4a79d1", main=paste(allAge[i], "Ma", sep=""), border="blue")
#	plot(this, col="#873e0dCC", add=TRUE, border=NA)
	cat(i, "\r")
	flush.console()
}

dev.off()

names(dem0) <- allAge
names(dem1400) <- allAge
names(demShelf) <- allAge

# store them for later use
stores(dem0)
stores(dem1400)
stores(demShelf)

##############################################################
#3. For plotting: reconstruct the present-day coastlines!
# Calculate the present day coastlines
presentPolygon <- readOGR("data/PaleoMAP/Present-day/ne_10m_admin_0_countries_lakes_wpid_19o_polygon.shx")
presentPolyline <- readOGR("data/PaleoMAP/Present-day/ne_10m_admin_0_countries_lakes_wpid_19o_polyline.shx")

# reconstruction model
mod <- fetch("paleomap","model", ver="v19o_r1c", datadir=file.path(workdir, "data/chronosphere"))


presPG <- list()
presPL <- list()
for(i in 1:length(dem0)){
	# the current age
	current <- as.numeric(names(dem0)[i])

	# the polygons
	presPG[[i]] <- reconstruct(presentPolygon, age=current, model=mod)

	# the polylines
	presPL[[i]] <- reconstruct(presentPolyline, age=current, model=mod)
	cat(i, "\r")
	flush.console()
}

names(presPG) <- names(dem0)
names(presPL) <- names(dem0)
stores(presPG)
stores(presPL)
