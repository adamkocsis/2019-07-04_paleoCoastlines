# version 2
linesToPoly <- function(x){
	# Lines-lsit
	newX <- lapply(x@lines, FUN=function(y){
	#	y<- x@lines[[1]]
		# if multiple lines exist in a single Lines just rbind them
		newCoords <- NULL
		for(u in 1:length(y@Lines)){
			newCoords<- rbind(newCoords,y@Lines[[u]]@coords)
		}
		newPart <- list(Polygon(newCoords))
		
		newY <- Polygons(newPart, y@ID)
		return(newY)
	})
	spPoly<- SpatialPolygons(newX)
	
	result <- SpatialPolygonsDataFrame(spPoly, data=x@data)
	return(result)
}


#' @param shp SpatialPolygonsDF.
#' @param holes character matrix, first column: IDs of future holes, second:IDs where the holes snhould be

makeHole <- function(shp, holes, hole=TRUE){
	
	# iterate for all holes
	hosts <- as.character(holes[,2])
	holes <- as.character(holes[, 1])

	# polygons in which we will put the polygons
	unHosts<- unique(hosts)

	# loop through all hosts
	for(j in 1:length(unHosts)){
		# which polygons are holes
		thisHost <- holes[hosts==unHosts[j]]
		
		newAllHoles<-list()
		# loop through all hole polygons
		for(i in 1:length(thisHost)){
			hole1<- shp[as.character(thisHost[i]),]
			newAllHoles[[i]] <- Polygon(hole1@polygons[[1]]@Polygons[[1]]@coords, hole=hole)
		}

		# get polygon IDs
		pID <- unlist(lapply(shp@polygons, function(x) x@ID))

		# which number is this?
		hostIndex <- which(pID==unHosts[j])

		shp@polygons[[hostIndex]] <- Polygons(
									c(shp@polygons[[hostIndex]]@Polygons, newAllHoles), 
									ID=shp@polygons[[hostIndex]]@ID)

		# remove polygons
		shp<- shp[which(!pID%in%thisHost), ]
	}

	return(shp)
}

# selectID
selid <- function(shp, id){
	pID <- unlist(lapply(shp@polygons, function(x) x@ID))
	which(pID%in%id)
}

# function to omit entire polygonses objects
omitsp <- function(shp, id){
	shp <- shp[-selid(shp,id),]
	shp
}

# Function to omit polygons in a polygonses element of SPDF
omitpoly <- function(shp, i, ps=1){
	polys<- shp@polygons[[ps]]@Polygons
	newPolys<- polys[-i]

	shp@polygons[[ps]]<- Polygons(newPolys, ID=shp@polygons[[ps]]@ID)
	return(shp)
}

# function to join Polygons with other Polygons and make them a single Polygon in a Polygons-object
joinPolygonParts <- function(x, join, reverse=NULL){
		if(is.null(reverse)){
			reverse <- rep(F, length(join))
		}
		# strat with the first
		first <- x[join[1],] @polygons[[1]]@Polygons[[1]]@coords
		if(reverse[1]) first <- first[nrow(first):1,]

		# go through the subsequent entries, reverse them if necessary
		for(i in 2: length(join)){
			part <- x[join[i],] @polygons[[1]]@Polygons[[1]]@coords
			if(reverse[i])  part <- part[nrow(part):1,]
			first <- rbind(first, part)

		}
		lowest <- sort(join)[1]

		# make new polygon store it in the lowest id
		x@polygons[[selid(x, lowest)]]@Polygons[[1]]<- Polygon(first)


		# omit original polygons
		x <- omitsp(x, join[join!=lowest])

		return(x)
	}

# @param keep keep the original spdf data?
restructure <- function(x, keep=T, addproj=CRS("+proj=longlat +datum=WGS84")){
	oldPolygonses <- x@polygons

	newPolygonses <-list()
	# loop through all polygons(es)
	for(i in 1:length(oldPolygonses)){
		oldPolygons <- oldPolygonses[[i]]
		ID <- oldPolygons@ID

		newPolygons <- list()
		# loop through all polygon(s)
		for(j in 1:length(oldPolygons@Polygons)){
			oneOldPoly <- oldPolygons@Polygons[[j]]
			newPolygons[[j]]<-Polygon(oneOldPoly@coords)
		}

		# recreate Polygonses
		newPolygonses[[i]] <- Polygons(newPolygons, ID=ID)
	}

	sp <- SpatialPolygons(newPolygonses, proj4string=addproj)

	if(keep) sp <- SpatialPolygonsDataFrame(sp, data=x@data)
	return(sp)

}


# 	# example for hole testing
# 	a<-lCoast[["0"]]
# 	
# 	b<- a@polygons[[selid(a, "0")]]
# 	
# 	# host
# 	u <- b@Polygons[[1]]
# 	# non-host
# 	v <- a@polygons[[selid(a, "1")]]@Polygons[[1]]
# 	
# 	# holes
# 	i <- b@Polygons[[2]]
# 	o <- b@Polygons[[2]]
# 	isXinY(i, u) # TRUE
# 	isXinY(i, v) # FALSE

# temporarily make a single polygon a spatialPolygons ojbect
tempsp <- function(x, ID=0){
	if(x@hole){
		x<- Polygon(x@coords[nrow(x@coords):1, ])
	}
	SpatialPolygons(list(Polygons(list(x), ID=ID)))
}


isXinY <- function(x, y){
	if(class(x)!="Polygon" | class(y)!="Polygon") stop("x or y is not a 'Polygon'-class object. ")
	res <-  over(tempsp(x), tempsp(y))

	over <- NA

	if(length(res)==1){
		# not overlapping
		if(is.na(res)) over <- FALSE else over <- TRUE

	}else{
		stop("The length of 'res' was not 1. ")
	}

	return(over)
}



ForceSinglepart <- function(x){
	# e.g. 170
	#x <- lCoast[["160"]]

	# get all the IDs
	allID <- unlist(lapply(x@polygons, function(y) y@ID))
	
	# places to hold everything - very memory inefficient, but we don't know the future length
	newEntities <- list()
	holes <- NULL
	allDF <- data.frame()

	# go through all polygon elements and extract the Polygon elements
	for(i in 1:length(allID)){
		# first Polygons-class object
		thisPolygons <- x@polygons[[i]]

		# list of polygons
		polygonList <- thisPolygons@Polygons

		# if there is just one entity in the list, conserve the original data entries
		if(length(polygonList)==1){
			# get this particular data chunk
			partDF <- x@data[thisPolygons@ID, ]
			partDF$index <- length(newEntities)+1
			allDF <- rbind(allDF, partDF)
		}


		for(j in 1:length(polygonList)){

			# add to structureless list of Polygon(s)
			newEntities <- c(newEntities, polygonList[j])

			# register hole or not
			holes<- c(holes, polygonList[[j]]@hole)
		}
	}

	# create a new dummy data.frame
	if(ncol(allDF) > 0){
		dummyDF <- matrix(NA, ncol=ncol(allDF)-1, nrow=length(holes))
		colnames(dummyDF) <- colnames(allDF)[-ncol(allDF)]
		dummyDF <- as.data.frame(dummyDF)
		# insert known entries
		for(i in 1:nrow(allDF)){
			for(j in 1:ncol(dummyDF)){
				dummyDF[allDF$index[i], j] <- allDF[i,j]
			}
			
		}

		# nothing registered
	}else{
		dummyDF <- data.frame(X=rep(NA, length(holes))) 
	}

	
	# now restructure the hole thing
	noHole <- newEntities[!holes]
	allHoles <-  newEntities[holes]

	actualDF <- dummyDF[!holes, , drop=F]

	# first recreate the basic object
	polygonsList <- list()
	for(i in 1:length(noHole)){
		# start with 0 indexing
		polygonsList[[i]] <- Polygons(noHole[i], ID=i-1)
	}

	rownames(actualDF)<- unlist(lapply(polygonsList, function(y) y@ID))

	# recreate the polygons with holes
	# if there are holes!!
	if(length(allHoles)>0){
		# go through every hole
		for(i in 1:length(allHoles)){

			# register which polygons have been cross-checked
			belong <- rep(NA, length(polygonsList))

			# loop through every polygons object
			for(j in 1:length(noHole)){
				# current polygons class object
				thisPolygons <- polygonsList[[j]]

				# there can only be one which is not a hole
				polygonList <- thisPolygons@Polygons
				
				# check with this
				checkPolygon <- NULL
				for(k in 1:length(polygonList)){
					if(!polygonList[[k]]@hole) checkPolygon <- polygonList[[k]]
				}

				# store which Polygons-class object should contain this hole
				belong[j] <- isXinY(allHoles[[i]],checkPolygon)
			}

			# if there are islands they can aslo be overlapping - get the one that is the biggest
			if(sum(belong)>1){
				pot <- which(belong)
				areas <- unlist(lapply(polygonsList[belong], function(y) y@area))

				# remove all found
				belong[]<- FALSE

				# be safe
				bMax <- max(areas)==areas
				if(sum(bMax)>1) stop("Multiple biggest polygons detected. Duplicates?")
				
				# put the hole in this!
				belong[pot[which(bMax)]] <- TRUE

			}

			# now rebuild that particular polygons
				# get the original parts
				tempPolygons <- polygonsList[[which(belong)]]

				# add the hole and overwrite original
				polygonsList[[which(belong)]] <- Polygons(c(tempPolygons@Polygons, allHoles[[i]]), ID=tempPolygons@ID)

		}
	}

	# now create original object
	newspatial <- SpatialPolygons(polygonsList, proj4string=x@proj4string)

	newFinal <- SpatialPolygonsDataFrame(newspatial, data=actualDF)

	return(newFinal)

}

