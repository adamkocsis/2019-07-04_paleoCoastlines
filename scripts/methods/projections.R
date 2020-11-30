# Functions to make map bounds and to project simple matrices.
# Ádám T. Kocsis
# Erlangen, 2020
# CC-BY 4.0


#' Function to create a detailed boundary of the map
#' @param x The number of points to base the boundary on in the x dimension
#' @param y The number of points to base the boundary on in the y dimension
detailedBounds <- function(x,y){
	rbind(
		cbind(seq(-180, +180, length.out=x), rep(90, x)),
		cbind(rep(180, y), seq(90, -90, length.out=y)),
		cbind(seq(180, -180, length.out=x), rep(-90, x)),
		cbind(rep(-180, y), seq(-90, 90, length.out=y))
	)
}

# the normal frame
frameSPlonglat <- SpatialPolygons(list(Polygons(list(Polygon(detailedBounds(x=3600, y=1800))), ID="0")), proj4string=crs("+proj=longlat +datum=WGS84"))
		
# function to transform individual coordinates
matProj <- function(m, proj){
	if(nrow(m)>0){
		# create a spatialPoints
		m <- SpatialPoints(m, proj4string=CRS("+proj=longlat"))
	
		# function to tranform all the coordinates
		trans <-spTransform(m, proj)
	
		ret <- trans@coords
	}else{
		ret <-m
	}

	return(ret)

}
