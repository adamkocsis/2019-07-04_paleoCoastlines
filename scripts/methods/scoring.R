# Function to calculate how well the shapefiles match the paleodb collection data.


# the over function iterated with a buffer around the shape

OverBuffer <- function(colls, shape, buffer=0){
	# make it sp type
	spColl <- SpatialPoints(colls, proj4string=shape@proj4string)

	# find those that fall on the shape, that means they are definitely OK,
	# and should be kept
	indSure <- which(!is.na(over(spColl, shape)))

	# if there is a buffer, continue!
	if(buffer>0){

		# convert the shape to points
		spp <- extractCoords(shape)
		# get the indices of those entries that are not sure
		unInd <- which(!1:nrow(colls)%in%indSure)
		
		# select those that have to be assessed
		uncertains <- colls[unInd, ,drop=FALSE]
		
		# cross check them one-by the shape verices
		# returns a single logical vector
		boolLP<-apply(uncertains, 1, function(x){
			# for a single point
			# calculate its distance from every vertex of the shape (vector)
			dists<-sp::spDistsN1(pts=spp, pt=x, longlat=T)

			# if the focal point is closer to at least one point than the buffer distance
			# this should be TRUE, else FALSE 
			any(dists<=buffer)
		})

		# add these indices to the rest
		indSure <- c(indSure, unInd[boolLP])

	}

	return(indSure)

}

extractCoords <- function(sp.df)
{
    results <- list()
    for(i in 1:length(sp.df@polygons[[1]]@Polygons))
    {
        results[[i]] <- sp.df@polygons[[1]]@Polygons[[i]]@coords
    }
    results <- Reduce(rbind, results)
    results
}



Score <- function(margins, coasts, pbdb, link="mapage", plot=NULL, icosa=NULL, buffer=0 
	,all.col="purple", trueList=list(right.col="darkblue", wrong.col="red", right.pch=16, wrong.pch=3), decide=NULL, 
	falseList=list(right.col="orange", wrong.col="green", right.pch=16, wrong.pch=3), legbin=NULL, 
	method="equirect", bg=NULL, presPoly=NULL, presLines=NULL, presbin=NULL, oldCoast=NULL
){

#	margins <- lPlates
#	coasts <- lCoast
#	pbdb <- paleoColls[paleoColls$marine,]
#	link <- "mapage"
#	icosa <- NULL
#	buffer <- 0
#	plot <- "collsOnMaps.pdf"


	# How many stages are there
	allTime <- sort(unique(pbdb$stg))
	
	# if icsoa is wanted
	if(!is.null(icosa)){
		# create an operative grid
		gr <- hexagrid(icosa, sp=TRUE)
		## use this raster as baseline for calculations
		HRtemp<-raster()
		raster::res(HRtemp)<-0.1
		raster::values(HRtemp) <- 1

	}	

	# reorder everything with matchtime
	# match order to stages
	margins <- margins[matchtime(names(margins), stages$mid)]
	coasts <- coasts[matchtime(names(coasts), stages$mid)]	

	# important information
	# PaleoDB colls
	nColl <- rep(NA, max(allTime)) # number of paleoDB collections
	nCollShelf <-rep(NA, max(allTime)) # number of PaleoDB collections on shelf (vector)
	nCollTerr<-rep(NA, max(allTime)) # number of PaleoDB collections on land

	# paleodb coords
	nCoord <-rep(NA, max(allTime)) # number of paleoDB coordinates
	nCoordShelf <- rep(NA, max(allTime)) # number of paleoDB coordinates on shelf 
	nCoordTerr<-rep(NA, max(allTime))# number of PaleoDB coordinates on land
	
	# icosa
	nShelfIcosa <- rep(NA, max(allTime)) # number of cells on shelf
	nCoordShelfIcosa <- rep(NA,max(allTime)) # number of paleoDB coordinates on shelf cells
	nCollShelfIcosa <-rep(NA, max(allTime)) # number of PaleoDB collections on shelf cells

	# plot the pdf
	if(!is.null(plot)) savepdf(plot, width=21, height=10)

	for(i in 1:max(allTime)){ # final version
		# i<- 85
		# current age
			currAge <- names(coasts)[i]
		
		# current coastline
			cs <- coasts[[i]]

		# current margin
			cm <- margins[[i]]

			if(!is.null(bg)){
				# dem background
				thisBG <- bg[currAge]
			}

			# plotting the present-day coastlines
			presPolygon <- NULL
			presLine <- NULL

			if(i%in%presbin){
				if(!is.null(presPoly)){
					presPolygon <- presPoly[[currAge]]
				}
				if(!is.null(presLines)){
					presLine <- presLines[[currAge]]
				}
			}

			if(!is.null(oldCoast)){
				thisOld <- oldCoast[[currAge]]
			}


		# current shelf
			suppressWarnings(shelf <- rgeos::gDifference(cm, cs))

		# the PaleoDB data
		# slice-specific data
		datSlice <- pbdb[pbdb$stg==i, ]
		if(!is.null(decide)){
			# terrestrial
			terr <- datSlice[!datSlice[,decide], ]
			
			# marine
			datSlice <- datSlice[datSlice[,decide], ]

		}

		plotTRUE <- FALSE
		# only execute is there arey
		if(nrow(datSlice)>0){
			
			
			if(!is.null(icosa)){
			# look up shelf	
				cmRast <- newMask(x=HRtemp, sp=cm)
				csRast <- newMask(HRtemp, cs)
				raster::values(cmRast)[is.na(raster::values(cmRast))] <- 0
				raster::values(csRast)[is.na(raster::values(csRast))] <- 0

				# the shelf coordinates
				shelfRast <- cmRast-csRast
				shelfCoords <- xyFromCell(shelfRast, which(as.logical(raster::values(shelfRast))))

				#look them up with icosa
				shelfCells <-unique(locate(gr, shelfCoords))
				nShelfIcosa[i] <- length(shelfCells)
			}

			# the number of paleoDB collections total
			paleoColl <- cbind(datSlice$paleolng, datSlice$paleolat)[!duplicated(datSlice$collection_no),, drop=F]
			nColl[i] <- nrow(paleoColl) 

			# paleoCoordinates total
			paleoCoord <- unique(paleoColl)
			nCoord[i] <-  nrow(paleoCoord)

			#	# Spatialpoints
			#	spColl <- SpatialPoints(paleoColl, proj4string=shelf@proj4string)
			#	spCoord <- SpatialPoints(paleoCoord,proj4string=shelf@proj4string)
				
				# directly falling on shelf area
				cs <- SpatialPolygons(cs@polygons,proj4string=cs@proj4string)
					
					# indices which
				#	overColl <- !is.na(over(spColl, shelf))
				#	overCoord <- !is.na(over(spCoord, shelf))
					overColl <- OverBuffer(colls=paleoColl, shape=shelf, buffer=buffer)
					overCoord <- OverBuffer(colls=paleoCoord, shape=shelf, buffer=buffer)

					# coiunt the number of coordinates and collectinos
					nCollShelf[i] <- length(overColl)
					nCoordShelf[i] <- length(overCoord)

				
				# directly falling on land area
					# indices which
				#	overCollTerr <- !is.na(over(spColl, cs))
				#	overCoordTerr <- !is.na(over(spCoord, cs))

					# strict=land, seach in the vicinity of this
					overCollTerr <- OverBuffer(colls=paleoColl, shape=cs, buffer=buffer)
					overCoordTerr <- OverBuffer(colls=paleoCoord, shape=cs, buffer=buffer)

					# coiunt the number of coordinates and collectinos
					nCollTerr[i] <- length(overCollTerr)
					nCoordTerr[i] <- length(overCoordTerr)
			
			# replace in the list
			trueList$restCoords <- paleoCoord[!(1:nrow(paleoCoord)%in%c(overCoord, overCoordTerr)), , drop=FALSE]
			trueList$right <- paleoCoord[overCoord,, drop=F]
			trueList$wrong <- paleoCoord[overCoordTerr,,drop=F ]
			
			plotTRUE<- TRUE
			
			if(!is.null(icosa)){
				# icosa - locate these
				collCell <- locate(gr, paleoColl)
				coordCell <- locate(gr, paleoCoord)

				# how many are on shelf
				nCoordShelfIcosa[i] <- sum(coordCell%in%shelfCells)
				nCollShelfIcosa[i] <- sum(collCell%in%shelfCells)
			}

			

			gc()
		}

		plotFALSE <- FALSE
		
		# in case the terrestrial should be put on the maps too!
		if(!is.null(decide)){
			datSlice <- terr
			if(nrow(datSlice)>0){
				paleoColl <- cbind(datSlice$paleolng, datSlice$paleolat)[!duplicated(datSlice$collection_no),, drop=F]
			
				paleoCoord <- unique(paleoColl)
				# plot these 
				overCoord <- OverBuffer(colls=paleoCoord, shape=shelf, buffer=buffer)
				overCoordTerr <- OverBuffer(colls=paleoCoord, shape=cs, buffer=buffer)

				falseList$restCoords <- paleoCoord[!(1:nrow(paleoCoord)%in%c(overCoord, overCoordTerr)), , drop=FALSE]
				falseList$right <- paleoCoord[overCoordTerr,, drop=F]
				falseList$wrong <- paleoCoord[overCoord,,drop=F ]

				plotFALSE <- TRUE
				
			}
		}

		# Plot
		# create an empty plot
		if(!is.null(plot)) {
			# wow, this is an ugly solution
			if(method=="equirect"){
				# single plotting function
				PlotOccsSimple(cm, cs, true=plotTRUE, false=plotFALSE, tList=trueList, fList=falseList,
					age=paste0(stages$stage[i], ", ", currAge), pcex=0.5, ppch=16, p.col.all=all.col)
			}
			if(method=="mollweide"){
				# single plotting function
				PlotOccsProj(cm, cs, true=plotTRUE, false=plotFALSE, tList=trueList, fList=falseList,
					age=paste0(stages$stage[i], ", ", currAge), pcex=0.5, ppch=16, p.col.all=all.col)
			}
			if(method=="demmoll"){				
				PlotOccsProjDEM(cm, cs, true=plotTRUE, false=plotFALSE, tList=trueList, fList=falseList,
					age=paste0(stages$stage[i], ", ", currAge), pcex=0.5, ppch=16, p.col.all=all.col,
					bg=thisBG, pline=presLine, ppoly=presPolygon)
			}
			if(method=="changemoll"){				
				PlotOccsProjChange(cm, cs, true=plotTRUE, false=plotFALSE, tList=trueList, fList=falseList,
					age=paste0(stages$stage[i], ", ", currAge, " Ma"), pcex=0.5, ppch=16, p.col.all=all.col,
					old=thisOld, pline=presLine, ppoly=presPolygon)
			}
		}

		cat(i, "\r")
		flush.console()

		if(!is.null(legbin)){
			if(i%in%legbin){
				legend("left", inset=0.1, legend=c("Marine on shelf", "Marine on land", "Terrestrial on shelf", "Terrestrial on land", "Deep water"),
				col=c(trueList$right.col, trueList$wrong.col, falseList$wrong.col, falseList$right.col,all.col), 
				pch=c(trueList$right.pch, trueList$wrong.pch, falseList$wrong.pch, falseList$right.pch,16),
				bg="#FFFFFFAA", cex=2)
			}

		}
	}

	dev.off()

	# export everything
	all <- data.frame(
		stage=stages$stage, 
		map=names(margins),
		coordinates=nCoord, 
		coordsOnShelf=nCoordShelf, 
		coordsOnLand=nCoordTerr,
		collections=nColl, 
		collsOnShelf=nCollShelf, 
		collsOnLand=nCollTerr, 
		propCoordsOnShelf=round(nCoordShelf/nCoord,2), 
		propCollsOnShelf=round(nCollShelf/nColl,2),
		propCoordsOnLand=round(nCoordTerr/nCoord,2), 
		propCollsOnLand=round(nCollTerr/nColl,2))
	
	return(all)
}
