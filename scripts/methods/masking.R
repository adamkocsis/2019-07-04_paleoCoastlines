# Default masking methods are vulnerable to holes in polygons. Some corrections are implemented here.
# Ádám T. Kocsis
# Erlangen, 2020
# CC-BY 4.0

# Function to mask a raster with a SpatialPolygons-type object 
#' @param SP*
#' @param rl Rasterlayer
waterLayer <- function(coast, rl=r){
	# do the rasterization: layer by layer
	fullStack <- stack()
	for(i in 1:length(coast@polygons)){
		this <- coast@polygons[[i]]
		for(j in 1:length(this@Polygons)){
			current <- this@Polygons[[j]]
			
			tempPolygons <- SpatialPolygons(list(Polygons(list(current), ID=1)))

			maskCurrent <- mask(rl, tempPolygons, updatevalue=1)
			fullStack<- stack(fullStack, maskCurrent)

		}
	}
	
	# the maximum is the basic sea
	a<- calc(fullStack, sum)

	# sea and land alternates as even and odd numbers
	basic <- max(values(a))

	# is sea even or odd?
	remain <- basic%%2

	# take the same parts
	take <- raster::values(a)%%2==remain

	raster::values(rl) <- take

	return(rl)
}

#' The general version of waterLayer()
#' @param x RasterLayer
#' @param sp THe SpatialPolygons
#' @inverse FALSE: returns values that are covered by polygons, TRUE: what are not covered
newMask <- function(x, sp, inverse=FALSE){
	y<-x
	raster::values(y) <- 0
	maskRaster <- waterLayer(sp, y)

	# use the raster-raster method to do proper masking
	res<-x
	if(!inverse){
		raster::values(res)[as.logical(raster::values(maskRaster))] <- NA
	}else{
		raster::values(res)[!as.logical(raster::values(maskRaster))] <- NA
	}
	return(res)
}

