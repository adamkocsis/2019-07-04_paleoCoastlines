# Functions relevant to area calculations
# Ádám T. Kocsis
# Erlangen, 2020
# CC-BY 4.0

#' Calculate the are covered by shapes over time
#' @param shapes The list of shapes.
#' @param icosa Should an icosahedral grid basis be used? (not used)
#' @param plot A time-slice-specific plot be drawn?
#' @param proj projection used for area calculations (Equal area) 
#' @param rgeosplot Plot to check the proper projection.
#' @return A data.frame.
AreaOverTime<-function(shapes, icosa=NULL, plot=FALSE, proj="+proj=cea", rgeosplot=FALSE){
	
	# icosa
	if(is.null(icosa)){
		earth<-hexagrid(c(4,7))
		eartharea <- sum(surfacearea(earth))*1000000
	}else{
		earth<-hexagrid(icosa, sp=TRUE)
		eartharea <- sum(surfacearea(earth))*1000000
		hr <- raster()
		res(hr) <- 0.1
		raster::values(hr) <- 1
	}

	# time intervals
	allTime <- names(shapes)
	rgeos <- rep(NA, length(shapes))
	geosphere <- rep(NA, length(shapes))
	cells <- rep(NA, length(shapes))

	# for loop over all time slices
	for(i in 1:length(allTime)){
		# first, use gArea
		# transform to equal area
		trans <-spTransform(shapes[[i]], CRS(proj))

		# calcualte area
		rgeos[i] <- rgeos::gArea(trans)
		if(rgeosplot) plot(trans, col="red")
		# 2. use geosphere
		geosphere[i]<- geosphere::areaPolygon(shapes[[i]])

		# 3. with icosa
		if(!is.null(icosa)){
			vals <- newMask(hr, shapes[[i]], inverse=FALSE)
			ps <- raster::xyFromCell(vals, cell=which(as.logical(raster::values(vals))))
			cellNames <- unique(locate(earth, ps))
			cells[i] <- length(cellNames)
			if(plot){
				plot(vals)
				plot(shapes[[i]], add=TRUE, col="#FF000044")
				plot(earth[cellNames], add=TRUE, col="#0000FF44")
			}

		}
		cat(i, "\r")
		flush.console()
	}
	a <- data.frame(rgeos=rgeos/eartharea, geosphere=geosphere/eartharea, icosa=cells/nrow(earth@faces))
	rownames(a) <- allTime
	return(a)
}


#' Function to calculate the area of shapes cut to belts
#' @param shapes List of SpatialPolygons*
#' @parma breaks Where the boundaries of the latitudinal bounds are.
#' @plot Should be plotted?
AreaInBelts <- function(shapes, breaks=c(-90, -60, -30, 0, 30, 60, 90 ), plot="belts.pdf"){
	
	# some colors
	allHex <- c("#F21414", "#07F80C", "#0712F8", "#F8F500", "#9E00F8", "#FF9C00" ,"#00F0FF",
	"#F00FE3", "#744932", "#A5EA21")

	earth<-icosa::hexagrid(c(4,7))
	eartharea <- sum(icosa::surfacearea(earth))*1000000


	# iterate for all time intervals
	allTime <- names(shapes)

	# matrix of all the output
	output <- matrix(NA, ncol=c(length(breaks)-1), nrow=length(allTime))
	rownames(output) <- allTime

	# list to hold the belt boundaries
	beltAreas <- list()

	# generate a list of bounding boxes
	for(j in 2:length(breaks)){
		# matrix of points
		whole <- matrix(
			c(
				180, breaks[j],
				180, breaks[j-1],
				-180, breaks[j-1],
				-180,breaks[j]
	
			), ncol=2, byrow=TRUE)
		belt <- SpatialPolygonsDataFrame(
			SpatialPolygons(list(Polygons(list(Polygon(whole)), ID="1"))),
			data=data.frame(1)
		)
		belt@proj4string <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

		# stores it in a shape
		beltAreas[[j-1]] <- belt
	}

	if(!is.null(plot)) savepdf(plot, width=20, height=10)

	# output
	for(i in 1:length(shapes)){

		# current time interval
		current <- allTime[i]

		# current shape
		curShape <- shapes[[current]]
		if(!is.null(plot)){
			plot(curShape, xlim=c(-180, 180), ylim=c(-90, 90))
			mtext(side=3, text=paste0(current, "Ma"))
			rect(xleft=c(-180), xright=180, ybottom=-90, ytop=90)
		}
		# for every latitudinal belt
		for(j in 1:length(beltAreas)){
			# one transect
			suppressWarnings(oneBelt <- gIntersection(curShape, beltAreas[[j]]))

			# in case there is an intersection!!!
			if(!is.null(oneBelt)){
				if(!is.null(plot)) plot(oneBelt, col=allHex[j], add=TRUE)

				# calculate area
				oneTrans <- spTransform(oneBelt, CRS("+proj=cea"))

				# calculate the area
				output[i, j]<- rgeos::gArea(oneTrans)
			}

		}
		cat(i, "\r")
		flush.console()

	}

	# close the plot
	if(!is.null(plot)) dev.off()

	return(output/eartharea)
}


# utility function 
cumulcol <- function(x){
	for(i in 2:ncol(x)){
		x[,i] <- x[,i]+x[,i-1]
		# make sure there are no NAs
		x[is.na(x[,i]),i] <-x[is.na(x[,i]),i-1] 
	}
	x
}
