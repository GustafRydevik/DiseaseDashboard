# helper for TreeMapApp
# S J Lycett
# 9 July 2016

library(ape)
library(maps)
library(mapdata)
library(mapproj)

source("Rcode//getEl.R")
source("Rcode//calcDecimalDate.R")
source("Rcode//get_BEAST_cols.R")
source("Rcode//read_BEAST_tree_latlon.R")
source("Rcode//read_MCC_tree.R")

getLimitsFromTree	<- function( tr ) {
	
	ntips	<- length(tr$tip.label)
	
	minlat <- floor( min( tr$latlon[,1] ) )
	maxlat <- ceiling( max( tr$latlon[,1] ) )
	minlon <- floor( min( tr$latlon[,2] ) )
	maxlon <- ceiling( max( tr$latlon[,2] ) )
	
	mintime <- floor( min(tr$nodeTimes ) )
	maxtime <- ceiling( max(tr$nodeTimes ) )
	
	sample_mintime <- floor( min(tr$nodeTimes[1:ntips]) )
	sample_maxtime <- ceiling( max(tr$nodeTimes[1:ntips]) )
	
	xlim <- c(minlon,maxlon)
	ylim <- c(minlat,maxlat)
	tlim <- c(mintime,maxtime)
	slim <- c(sample_mintime,sample_maxtime)
	wlim <- c(sample_mintime,sample_maxtime)
	
	return( list(xlim=xlim, ylim=ylim, tlim=tlim, 
					slim=slim, wlim=wlim,ntips=ntips) )
}


getFromToLatLon <- function( tr ) {
	
	fromNode	<-tr$edge[,1]
	toNode		<-tr$edge[,2]
	
	fromLat		<- tr$latlon[fromNode,1]
	fromLon		<- tr$latlon[fromNode,2]
	fromTime	<- tr$nodeTimes[fromNode]
	
	toLat		<- tr$latlon[toNode,1]
	toLon		<- tr$latlon[toNode,2]
	toTime		<- tr$nodeTimes[toNode]
	
	tbl			<- cbind(fromLat,toLat,fromLon,toLon,fromTime,toTime)
	return( tbl )
	
}


getCoords <- function( tr=tr, tbl=getFromToLatLon( tr ), 
								projection="mercator", orientation=c(90,0,0), 
								arrowHeadLength=0.1 ) {
	
	if  (projection=="x" & orientation[1]=="x") {
		coords	   <- list(x=tr$latlon[,2], y=tr$latlon[,1])
		fromCoords <- list(x=tbl[,3], y=tbl[,1])
		toCoords   <- list(x=tbl[,4], y=tbl[,2])
	} else {						
		coords		<- mapproject(tr$latlon[,2], tr$latlon[,1], projection=projection, orientation=orientation)
		fromCoords <- mapproject(tbl[,3], tbl[,1], projection=projection, orientation=orientation)
		toCoords   <- mapproject(tbl[,4], tbl[,2], projection=projection, orientation=orientation)
	}
	
	nedges	  	<- length(fromCoords$x)
	dist		<- sqrt( (fromCoords$x-toCoords$x)^2 + (fromCoords$y-toCoords$y)^2 )
	arrowLen	<- array(arrowHeadLength, nedges)
	jj		    <- which(dist == 0)
	if (length(jj) > 0) {
		arrowLen[jj] <- 0
	}
	
	return( list(coords=coords, fromCoords=fromCoords, toCoords=toCoords, 
					arrowLen=arrowLen, tbl=tbl, tr=tr,
					projection=projection, orientation=orientation) )
}


getPointsInWindow <- function( trCoords, windowStart=windowStart, windowEnd=windowEnd,
									inWindowCol=hsv(0,0.9,0.8), outWindowCol=hsv(0,0.2,1.0), inWindowCol2="black") {
	
	tr				<- trCoords$tr
	tbl				<- trCoords$tbl
	coords			<- trCoords$coords
	ntips			<- length(tr$tip.label)
	
	nodeInds 		<- which( (tr$nodeTimes >= windowStart) & (tr$nodeTimes <= windowEnd) )
	branchInds	<- which( (tbl[,6] >= windowStart) & (tbl[,5] <= windowEnd) )
	
	ppch					<- array(1, length(coords$x))
	ppch[1:ntips] 			<- 21
	tcols1					<- array(outWindowCol, length(coords$x))
	tcols2					<- array(outWindowCol, length(coords$x))
	tcols1[nodeInds] 		<- inWindowCol
	tcols2[nodeInds] 		<- inWindowCol
	tcols2[intersect(nodeInds,1:ntips)] <- inWindowCol2
	bcols					<- array(outWindowCol, length(tbl[,1]))
	bcols[branchInds]		<- inWindowCol
	
	ccex					<- 1
	
	trCoords$ppch			<- ppch
	trCoords$tcols1			<- tcols1
	trCoords$tcols2			<- tcols2
	trCoords$bcols 			<- bcols
	trCoords$ccex			<- ccex
	trCoords$nodeInds		<- nodeInds
	trCoords$branchInds		<- branchInds
	
	return( trCoords )

}

doMapPlot <- function( trCoords=trCoords, lims=lims, mapFillCol="grey90", mapBackCol="white") {

	#map("worldHires", xlim=lims$xlim, ylim=lims$ylim, 
	#	projection=trCoords$projection, orientation=trCoords$orientation, 
	#	fill=TRUE, bg=mapBackCol, col=mapFillCol, border=mapFillCol)
	
	if (trCoords$projection=="x" & trCoords$orientation[1]=="x") {
		map("worldHires", xlim=lims$xlim, ylim=lims$ylim, 
		fill=TRUE, bg=mapBackCol, col=mapFillCol, interior=FALSE, mar=c(0,0,0,0))	
	} else {
		map("worldHires", xlim=lims$xlim, ylim=lims$ylim, 
		projection=trCoords$projection, orientation=trCoords$orientation, 
		fill=FALSE, bg=mapBackCol, col=mapFillCol, interior=FALSE, mar=c(0,0,0,0))	
	}
	
	arrows(trCoords$fromCoords$x, trCoords$fromCoords$y, trCoords$toCoords$x, trCoords$toCoords$y,
	 				angle=15, length=trCoords$arrowLen, col=trCoords$bcols)
	points(trCoords$coords, pch=trCoords$ppch, cex=trCoords$ccex, bg=trCoords$tcols1, col=trCoords$tcols2)	
		
}



##################################################################################
# example for testing (non-app)

doExample <- FALSE
if (doExample) {
	
	projection 		<- "mercator"
	orientation		<- c(90,0,0)
	
	windowStart		<- 1980
	windowEnd			<- 2015
	
	mapFillCol 		<- "grey90"
	mapBackCol 		<- "white"
	inWindowCol	   	<- hsv(0, 0.9, 0.8)
	outWindowCol		<- hsv(0, 0.2, 1.0)
	
	#path		<- "Documents//SHINY//TreeMapApp//"
	#setwd(path)
	path		<- ""
	
	# read in tree
	path2		<- "example_data//"
	name 		<- "fmdv_A_africa_142_empiricalsky_hbr_1.with_export.p0.tre"
	treeName	<- paste(path,path2,name,sep="")
	tr			<- read_latlon_mcc_tr( trName=treeName )
	
	# get limits from tree
	lims 		<- getLimitsFromTree( tr )
	
	# convert coordinates - of tip & internal nodes and of from to branches
	trCoords	<- getCoords(tr=tr,tbl=getFromToLatLon(tr),projection=projection,orientation=orientation)
	
	# get points within the time window and colours of points
	trCoords	<- getPointsInWindow(trCoords, 
										windowStart=windowStart, windowEnd=windowEnd,
										inWindowCol=inWindowCol, outWindowCol=outWindowCol)
	
	# do plot
	doMapPlot(trCoords=trCoords,lims=lims, mapFillCol=mapFillCol, mapBackCol=mapBackCol)
	
}