# Functions to used through the corrections and later for plotting
# Ádám T. Kocsis
# Erlangen, 2020
# CC-BY 4.0

# function to plot SpatialLines
#' @param splevel should it display the ID of the polygonses ("sp") or the polygon index?
splines <- function(x, cex=1, splevel=TRUE){
	if(splevel){
		if(class(x)=="SpatialLinesDataFrame"){
			for(o in 1:length(x)){
				actCol <- allHex[o]
				y<-x@lines[[o]]
				lines(y, col=actCol)
				start <- y@Lines[[1]]@coords[1,, drop=FALSE]
				text(start, label=y@ID, col=actCol, cex=cex)
			}
		}

		if(class(x)=="SpatialPolygonsDataFrame"){
			for(o in 1:length(x)){
				actCol <- allHex[o]
				y<-x@polygons[[o]]
				plot(SpatialPolygons(list(y)), col=NA, border=actCol, add=TRUE)
				start <- y@Polygons[[1]]@coords[1,, drop=FALSE]
				
				# plot direction if possible
				if(nrow(y@Polygons[[1]]@coords)>=10){
					direct <- y@Polygons[[1]]@coords[10,, drop=FALSE]
				}else{
					direct <- NULL
				}	
				if(!is.null(direct)) points(direct, pch=5, col=actCol)
				points(start, pch=3, col=actCol)

				start2 <- start
				start2[2] <- start2[2]+2
				text(start2, label=y@ID, col=actCol, cex=cex)

			}
		}
	}else{
		if(class(x)=="SpatialPolygonsDataFrame"){
			ps <- y<-x@polygons[[1]]@Polygons
			for(i in 1:length(ps)){
				current <- ps[[i]]
				actCol <- allHex[i]
				lines(current, col=actCol)
				start <- current@coords[1,, drop=FALSE]
				points(start, pch=3, col=actCol)

				start2 <- start
				start2[2] <- start2[2]+2
				text(start2, label=i, col=actCol, cex=cex)

			}
		}
	}
	
}

# temporary plotting
plotemp<- function(shp, splevel=TRUE, pdf=T,...){
	if(pdf) pdf("export/maps/tempor.pdf", height=8, width=20)
	plot(shp, col="gray",...)
	splines(shp, cex=0.3, splevel=splevel)
	if(pdf) dev.off()
}
	

#' @param cm Continental margins
#' @param cl Coastlines
#' @param cm.col color or cm
#' @param cl.col color of cl
#' @param age numeric entry, age plotted as the main label
#' @param cm.border cm borders
#' @param cl.border cl borders
# old cl.col= "#94391cAA"
#af9191AA
pairplot <- function(cm, cl, cm.col="#87cef6", cl.col="#94391cAA",
 age=NULL,cm.border=FALSE, cl.border=FALSE, 
 pdf=TRUE, xlim=c(-180,180), ylim=c(-90,90), add=FALSE){
	if(pdf){
		pdf("export/maps/pairplot.pdf", height=8, width=20)
	}
	if(!is.null(age)){
		mainLab <- paste0("Age: ",age, "Ma")
	}else{
		mainLab <- NULL
	}
	if(!is.null(xlim) & !is.null(ylim)){
		plot(cm, col=cm.col,  border=cm.border, main=mainLab, xlim=xlim, ylim=ylim,add=add)
		plot(cl, col=cl.col, border=cl.border, add=TRUE, xlim=xlim, ylim=ylim)
	}else{
		plot(cm, col=cm.col,  border=cm.border, main=mainLab, add=add)
		plot(cl, col=cl.col, border=cl.border, add=TRUE)

	}

	if(pdf) dev.off()
}


# shorthand function to put things in the right directory.
savepdf <- function(name, height, width){
	pdf(file.path("export", ver, name), height=height, width=width)
}
# shorthand function to put things in the right directory.
savepng <- function(name, height, width){
	png(file.path("export", ver, name), height=height, width=width)
}


# plot the chaning proportion of things over time
timeplot <- function(...){
	tsplot(stages, boxes="sys", shading="series", xlim=4:95,shading.col=c("white", "gray90"), ...)
}


# polygon plotting
polylines  <- function(x, y,first=NULL, last=NULL, bottom=0, ...){
	if(length(bottom)==1){
		bBoth <- !is.na(x) & !is.na(y)
		x<- x[bBoth]
		y <- y[bBoth]
		if(is.null(first)) first <- x[1]
		if(is.null(last)) last <-x[length(x)]
		
		newx <- c(x, last, last, first, first)
		newy <- c(y, y[length(y)], bottom, bottom, y[1])
	}else{
		bBoth <- !is.na(x) & !is.na(y) & !is.na(bottom)
		x<- x[bBoth]
		y <- y[bBoth]
		bottom <- bottom[bBoth]
		if(is.null(first)) first <- x[1]
		if(is.null(last)) last <-x[length(x)]
		
		newx <- c(x, last, last, rev(x), first, first)
		newy <- c(y, y[length(y)], bottom[length(bottom)], rev(bottom), bottom[1], y[1])
	}
	polygon(x=newx, y=newy, ...)

}

###################################################################
# DIfferent ways to plot the occurrences on the maps - to be used in Score()

# Simple Occurrence plotting - Equirectangular projection
#' @param cm Continental margin SPDF.
#' @param cs PaleoCoastline SPDF.
#' @param true First group of occurrences
#' @param false Second group of occurrences
#' @param tList Plotting paramters of occurrence group 1
#' @param fList Plotting paramters of occurrence group 2
#' @param age Plotting title - age
#' @param pcex General point size
#' @param ppch General point type
#' @param p.coll.all all Plot color (visible in case of deep water)
PlotOccsSimple <- function(cm, cs, true=plotTRUE, false=plotFALSE,
	tList=trueList, fList=falseList, age="", pcex=0.4, 
	ppch=16,p.col.all="purple"){
	if(true){

		pairplot(cm, cs, pdf=F, age=age)
		rect(xleft=-180, xright=180, ytop=90, ybottom=-90, col=NA, border="black")

		# plot the coordinates (true)
		points(tList$restCoords, col=p.col.all, pch=ppch, cex=pcex)
		points(tList$right, col=tList$right.col, pch=tList$right.pch, cex=pcex)
		points(tList$wrong, col=tList$wrong.col, pch=tList$wrong.pch, cex=pcex)
	}

	if(false){
		# false statements
		points(fList$restCoords, col=p.col.all, pch=ppch, cex=pcex)
		points(fList$right, col=fList$right.col, pch=fList$right.pch, cex=pcex)
		points(fList$wrong, col=fList$wrong.col, pch=fList$wrong.pch, cex=pcex)
	}
}

# Simple Occurrence plotting - Using a pre-defined projection projection
#' @param cm Continental margin SPDF.
#' @param cs PaleoCoastline SPDF.
#' @param true First group of occurrences
#' @param false Second group of occurrences
#' @param tList Plotting paramters of occurrence group 1
#' @param fList Plotting paramters of occurrence group 2
#' @param age Plotting title - age
#' @param pcex General point size
#' @param ppch General point type
#' @param p.coll.all all Plot color (visible in case of deep water)
#' @param proj The Projection in CRS.
PlotOccsProj <- function(cm, cs, true=plotTRUE, false=plotFALSE,
	tList=trueList, fList=falseList, age="", pcex=0.4, 
	ppch=16,p.col.all="purple", proj=CRS("+proj=moll")){
	if(true){
		
	#	# devel
	#	cm<- lPlates[["160"]]
	#	cs<-lCoast[["160"]]

		# correct edges
		cm <- PolyResample(cm, mi=3)
		cs <- PolyResample(cs, mi=3)

		# the edge of the map
		frameSP <- spTransform(frameSPlonglat, proj)

		# transform the basic shapes
		cm <- spTransform(cm, proj)
		cs <- spTransform(cs, proj)

		
		plot(frameSP, border="black", col=NA, main=age)

		pairplot(cm, cs, pdf=F, age="", xlim=NULL, ylim=NULL, add=T)
		
		plot(frameSP, border="gray20", col=NA, add=T, lwd=3)

		# plot the coordinates (true)
		points(matProj(tList$restCoords, proj), col=p.col.all, pch=ppch, cex=pcex)
		points(matProj(tList$right, proj), col=tList$right.col, pch=tList$right.pch, cex=pcex)
		points(matProj(tList$wrong, proj), col=tList$wrong.col, pch=tList$wrong.pch, cex=pcex)
	}

	if(false){
		# false statements
		points(matProj(fList$restCoords, proj), col=p.col.all, pch=ppch, cex=pcex)
		points(matProj(fList$right, proj), col=fList$right.col, pch=fList$right.pch, cex=pcex)
		points(matProj(fList$wrong, proj), col=fList$wrong.col, pch=fList$wrong.pch, cex=pcex)
	}
}


# Simple Occurrence plotting - Using a pre-defined projection projection and the DEMs as background
#' @param cm Continental margin SPDF.
#' @param cs PaleoCoastline SPDF.
#' @param true First group of occurrences
#' @param false Second group of occurrences
#' @param tList Plotting paramters of occurrence group 1
#' @param fList Plotting paramters of occurrence group 2
#' @param age Plotting title - age
#' @param bg The background raster (PaleoDEM)
#' @param pcex General point size
#' @param ppch General point type
#' @param p.coll.all all Plot color (visible in case of deep water)
#' @param proj The Projection in CRS.
#' @param pline Reconstructed present-day coastlines - Polyline part
#' @param ppoly Reconstructed present-day coastlines - Polygon part
PlotOccsProjDEM <- function(cm, cs, true=plotTRUE, false=plotFALSE,
	tList=trueList, fList=falseList, age="", pcex=0.4, bg, 
	ppch=16,p.col.all="purple", proj=CRS("+proj=moll"), pline=NULL, ppoly=NULL){
	if(true){
		
	#	# devel
	#	cm<- lPlates[["160"]]
	#	cs<-lCoast[["160"]]

		bgProj <- projectRaster(bg, crs=proj)

		# limit to ensure complete coverage
		raster::values(bgProj)[1] <- 10500
		raster::values(bgProj)[2] <- -8500

		# plotting correction
#		raster::values(bgProj)[raster::values(bgProj)> -10  & raster::values(bgProj) <1 ] <- 10
		raster::values(bgProj)[raster::values(bgProj)==0 ] <- 1

		# correct edges
		cm <- PolyResample(cm, mi=3)
		cs <- PolyResample(cs, mi=3)

		# the edge of the map
		frameSP <- spTransform(frameSPlonglat, proj)

		# transform the basic shapes
		cm <- spTransform(cm, proj)
		cs <- spTransform(cs, proj)

		
		mapplot(bgProj, legend=FALSE, col="earth")
		mtext(text=age, side=3, line=1, cex=2.5)

		rect(ytop=9583592, ybottom=8958605, xleft= -17888435, xright=-18513422, col="white", border=NA)


		plot(cm, add=T, border="gray40", col=NA)
		plot(cs, add=T, border="gray40", col=NA)

		plot(frameSP, border="gray20", col=NA, add=T, lwd=3)

		# present day
		if(!is.null(ppoly) & !is.null(pline)){
			ppoly <- spTransform(ppoly, proj)
			pline <- spTransform(pline, proj)

			plot(gSimplify(ppoly, 0.1), add=T)
			plot(gSimplify(pline, 0.1), add=T)
		
		}

		# plot the coordinates (true)
		points(matProj(tList$restCoords, proj), col=p.col.all, pch=ppch, cex=pcex)
		points(matProj(tList$right, proj), col=tList$right.col, pch=tList$right.pch, cex=pcex)
		points(matProj(tList$wrong, proj), col=tList$wrong.col, pch=tList$wrong.pch, cex=pcex)
	}

	if(false){
		# false statements
		points(matProj(fList$restCoords, proj), col=p.col.all, pch=ppch, cex=pcex)
		points(matProj(fList$right, proj), col=fList$right.col, pch=fList$right.pch, cex=pcex)
		points(matProj(fList$wrong, proj), col=fList$wrong.col, pch=fList$wrong.pch, cex=pcex)
	}
}

# Simple Occurrence plotting - Using a pre-defined projection and highlighting changes from before
#' @param cm Continental margin SPDF.
#' @param cs PaleoCoastline SPDF.
#' @param true First group of occurrences
#' @param false Second group of occurrences
#' @param tList Plotting paramters of occurrence group 1
#' @param fList Plotting paramters of occurrence group 2
#' @param age Plotting title - age
#' @param old The uncorrected paleocoastlines.
#' @param pcex General point size
#' @param ppch General point type
#' @param p.coll.all all Plot color (visible in case of deep water)
#' @param proj The Projection in CRS.
#' @param pline Reconstructed present-day coastlines - Polyline part
#' @param ppoly Reconstructed present-day coastlines - Polygon part
PlotOccsProjChange <- function(cm, cs, true=plotTRUE, false=plotFALSE,
	tList=trueList, fList=falseList, age="", pcex=0.4, old, 
	ppch=16,p.col.all="purple", proj=CRS("+proj=moll"), pline=NULL, ppoly=NULL){
	if(true){
		
	#	# devel
	#	cm<- lPlates[["160"]]
	#	cs<-lCoast[["160"]]

	#	old <- PolyResample(old, mi=3)
		# correct edges
		cm <- PolyResample(cm, mi=3)
		cs <- PolyResample(cs, mi=3)

		newSea <- suppressWarnings(gDifference(old, cs))
		newLand <- suppressWarnings(gDifference(cs, old))

		newSea <- PolyResample(newSea, mi=3)
		newLand <- PolyResample(newLand, mi=3)

		
		newSea <- spTransform(newSea, proj)
		newLand <- spTransform(newLand, proj)


		# the edge of the map
		frameSP <- spTransform(frameSPlonglat, proj)

		# transform the basic shapes
		cm <- spTransform(cm, proj)
		cs <- spTransform(cs, proj)
		old <- spTransform(old, proj)

		# calculate the difference


		plot(frameSP, border="black", col=NA)

		pairplot(cm, cs, pdf=F, age="", xlim=NULL, ylim=NULL, add=T)
		mtext(text=age, side=3, line=1, cex=2.5)

		# new elements on top of the originally rendered map
		plot(newSea, border=NA, col="#318dec", add=T)
		plot(newLand, border=NA, col="#62372e", add=T)
		
		plot(frameSP, border="gray20", col=NA, add=T, lwd=3)

		# present day
		if(!is.null(ppoly) & !is.null(pline)){
			ppoly <- spTransform(ppoly, proj)
			pline <- spTransform(pline, proj)

			plot(gSimplify(ppoly, 0.1), add=T)
			plot(gSimplify(pline, 0.1), add=T)
		
		}

		# plot the coordinates (true)
		points(matProj(tList$restCoords, proj), col=p.col.all, pch=ppch, cex=pcex)
		points(matProj(tList$right, proj), col=tList$right.col, pch=tList$right.pch, cex=pcex)
		points(matProj(tList$wrong, proj), col=tList$wrong.col, pch=tList$wrong.pch, cex=pcex)
	}

	if(false){
		# false statements
		points(matProj(fList$restCoords, proj), col=p.col.all, pch=ppch, cex=pcex)
		points(matProj(fList$right, proj), col=fList$right.col, pch=fList$right.pch, cex=pcex)
		points(matProj(fList$wrong, proj), col=fList$wrong.col, pch=fList$wrong.pch, cex=pcex)
	}
}
