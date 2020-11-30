# Functions to calculate shoreline length.
# Adam T. Kocsis
# Erlangen, 2020-08-23
# CC-BY

#' Function to iterate coastline length and plotting calculation for every reconstruction
#' @param all List of SpatialPolygonsDataFrame objects.
#' @param sticks Stick lengths in km. 
#' @param name The name of the pdf file containing all the plots (without extension).
AllSticks <- function(all, sticks=c(100, 200, 500, 1000), name){
	# NEW COASTLINES
	# allSticks 
	
	stickCoast <- matrix(NA, nrow=length(all), ncol=length(sticks))
	rownames(stickCoast) <- names(all)
	colnames(stickCoast) <- sticks
	
	for(j in 1:length(sticks)){
		savepdf(paste0(name,sticks[j],".pdf"), width=20, height=10)
			for(i in 1:length(all)){
				stickCoast[i, j] <- StickLength(all[[i]], stick=sticks[j])
				cat(i, "\r")
				flush.console()
			}
			# sclae the lengths appropriately
			stickCoast[, j] <- stickCoast[, j] *sticks[j]
	
		dev.off()
	}	 

	return(stickCoast)
}

# THESE FOLLOWING TWO FUNCTIONS WERE ADAPTED FROM MATERIAL FOUND ON THE WEB:
# https://rspatial.org/raster/rosu/Chapter1.html
# Function to calculate shoreline with one reconstruction and one stick length. 
StickLength <- function(one, stick=100, plot=TRUE){
	require(obigeo)
	data(allHex)
	# temp.
	if(!is.null(plot)) plot(one)
	
	# apply this procedure to every polygon
	total <- 0
	
	# repeat for every Polygons
	for(j in 1:length(one@polygons)){
	
		# select the appropriate polygons object
		poly1 <- one@polygons[[j]]
	
		# repeat for every polygon
		for (k in 1:length(poly1@Polygons)){
	
			# focus polygon
			poly2 <- poly1@Polygons[[k]]
	
			# coordinates of focus polygon
			g <- coordinates(poly2)
			# reverse the order (to start at the top rather than at the bottom)
			g <- g[nrow(g):1, ]
			
			# return the indices of the stick endpoints
			y <- stickpoints(g, stick*1000, TRUE)
			if(length(y)> 2) {
				# plot the stiks
				if(!is.null(plot)){
					lines(g[y, ], col=allHex[j+k], lwd=2)
					text(x=g[y[1], 1], y=g[y[1], 2], label=paste0(j,"_",k))
				}
				# add up the total number of sticks
				total <- total + length(y)
			}
	
		}
		
	}
	return(total)
	
}

# Function to divide on polygon to sticks
#' @param x One SpatialPolygonsDataFrame-class object.
#' @param stickLength Stick length in km.
#' @param lonlat logical argument of raster::pointDistance
stickpoints <- function(x, sticklength, lonlat) {
    # x is a matrix with two columns (x and y coordinates)
    nr <- nrow(x)
    pts <- 1
    pt <- 0
    while(TRUE) {
        pd <- pointDistance(x[1,], x, lonlat)
        # i is the first point further than the yardstick
        i <- which(pd > sticklength)[1]
        # if we cannot find a point within yardsitck distance we
        # break out of the loop
        o <- nrow(x)
        if (is.na(i) | o<(i+1)) break
        # remove the all points we have passed
       
        x <- x[(i+1):nrow(x), , drop=FALSE]
        pt <- pt + i
        pts <- c(pts, pt)
    }
    pts
}

