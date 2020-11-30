# Functions to increase point density in polygons. Necessary for accurate projection.
# Ádám T. Kocsis
# Erlangen, 2020
# CC-BY 4.0


#' # Function to resample polygons in long-lat space if the distance between two points is greater than a given value
#' @param sp SpatialPolygons*
#' @param mi Maximum interval length
PolyResample <- function(sp, mi=3){
	sep <- sp@polygons
	# Polygons
	for(i in 1:length(sep)){
		# Polygons
		thisS <- sep[[i]]@Polygons

		for(j in 1:length(thisS)){
			# the coordinates
			co <- thisS[[j]]@coords

			if(nrow(co)>4){
				
				sp@polygons[[i]]@Polygons[[j]]@coords <-  AddPoints(co, mi)
			}
		}

	}

	return(sp)

}

#' Insert points between two points in line if the distance is greater than a given value
#' @param matrix of points that define a line in 2d space
#' @param t the maximum distance threshold
AddPoints<-function(m, t){
	d <- XYdistAlong(m)
	# this will be painfully slow....
	corr <- d>t

	new <- m[0,]

	for(i in 1:length(corr)){
		# copy the starting point
		new <- rbind(new, m[i,])

		# if further interpolation is necessary
		if(corr[i]){
			# next item - goes round
			if(i!=length(corr)){
				nex <-  m[i+1,]
			}else{
				nex <- m[1]
			}

			new <- rbind(new, InsertPoints(m[i,], nex, ceiling(d[i]/t)))


		}

	}
	return(new)

}

#' Insert n number of points between two points using linear interpolation in 2d space
#' @param v1 Position vector of point 1
#' @param v2 Position vector of point 2
#' @param n number of points to insert
InsertPoints <- function(v1, v2, n){
	d <- v2-v1
	d <- d/(n+1)

	allDiff <- rep(d, n)* rep(1:n, each=2)

	orig <- rep(v1, n)

	matrix(orig+allDiff, ncol=2, byrow=T)

}


#' Distance of two points in 2d (planar) space
#' @param v1 Pos. vec. 1
#' @param v1 Pos. vec. 2
XYdist <- function(v1, v2){
	sqrt(sum((v2-v1)^2))
}

#' Calculate distances between a string of points (closed)
#' @param m matrix defining a string of points
XYdistAlong <- function(m){
	
	ds <- rep(NA, nrow(m))
	
	for(i in 1:nrow(m)){
		if(i==nrow(m)){
			ds[i] <- XYdist(m[i,], m[1,])
		}else{
			ds[i]<- XYdist(m[i,], m[i+1,])
		}
	}

	return(ds)

}