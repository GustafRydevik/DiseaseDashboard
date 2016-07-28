# functions to read traits mcc trees
# S J Lycett
# 15 Feb 2016
# 9 July 2016 - cleaning for R SHINY App

# assume have already compiled read_BEAST_tree_latlon.R in SHINY

read_latlon_mcc_tr <- function( trName=trName, trTxt=readLines(trName) ) {

	#trTxt <- readLines(trName)
	trLine<- trTxt[ length(trTxt) - 1]
	trLine<- strsplit(trLine, "\\[\\&R\\] ")[[1]][2]
	pos1	<- gregexpr("\\[\\&",trLine)
	pos2  <- gregexpr("\\]",trLine)
	nnodes<- length(pos1[[1]])
	nodeDetails <- array(0, nnodes)
	for (j in 1:nnodes) {
		nodeDetails[j] <- substring(trLine, pos1[[1]][j], pos2[[1]][j])
	}
	for (j in 1:nnodes) {
		trLine <- gsub(nodeDetails[j], paste("_nodeDetails_",j,sep=""), trLine, fixed=TRUE)
	}
	tr <- read.tree(text=trLine)
	tr$tip.details <- as.integer(apply(as.matrix(tr$tip.label), 1, getEl, ind=1, fromEnd=TRUE, sep="_"))
	tr$node.details<- as.integer(apply(as.matrix(tr$node.label), 1, getEl, ind=1, fromEnd=TRUE, sep="_"))
	tr$tip.details <- nodeDetails[tr$tip.details]
	tr$node.details<- nodeDetails[tr$node.details]
	tr$tip.label   <- apply(as.matrix(tr$tip.label), 1, getEl, ind=1, sep="_")
	
	translateTbl <- getTranslation(trTxt)
	tr$tip.label   <- translateTbl[match(tr$tip.label, translateTbl[,1]),2]
	tr$node.label  <- NULL

	#tr2 <- read.nexus(trName)
	#dist.topo(tr2,tr)

	tr$details	   <- c(tr$tip.details, tr$node.details)
	
	decDates	   <- as.numeric(apply(as.matrix(tr$tip.label), 1, getEl, ind=1, sep="\\|", fromEnd=TRUE))
	youngestTip	   <- max(decDates)
	tr		   <- nodeTimes(tr, youngestTip=youngestTip)

	#all(tr$nodeTimes[1:length(tr$tip.label)]==decDates)
	all( abs(tr$nodeTimes[1:length(tr$tip.label)]-decDates) < 1e-5 )
	
	latlon1	   <- apply(as.matrix(tr$details), 1, getEl, ind=2, sep="latlon1=")
	latlon1	   <- as.numeric(apply(as.matrix(latlon1), 1, getEl, ind=1, sep=","))

	latlon2	   <- apply(as.matrix(tr$details), 1, getEl, ind=2, sep="latlon2=")
	latlon2	   <- as.numeric(apply(as.matrix(latlon2), 1, getEl, ind=1, sep=","))

	tr$latlon	   <- cbind(latlon1,latlon2)
	colnames(tr$latlon) <- c("lat","lon")

	return( tr ) 

}

read_discrete_mcc_tr	<- function( trName=trName, trTxt=readLines(trTxt), trait="Subtype" ) {

	#trTxt <- readLines(trName)
	trLine<- trTxt[ length(trTxt) - 1]
	trLine<- strsplit(trLine, "\\[\\&R\\] ")[[1]][2]
	pos1	<- gregexpr("\\[\\&",trLine)
	pos2  <- gregexpr("\\]",trLine)
	nnodes<- length(pos1[[1]])
	nodeDetails <- array(0, nnodes)
	for (j in 1:nnodes) {
		nodeDetails[j] <- substring(trLine, pos1[[1]][j], pos2[[1]][j])
	}
	for (j in 1:nnodes) {
		trLine <- gsub(nodeDetails[j], paste("_nodeDetails_",j,sep=""), trLine, fixed=TRUE)
	}
	tr <- read.tree(text=trLine)
	tr$tip.details <- as.integer(apply(as.matrix(tr$tip.label), 1, getEl, ind=1, fromEnd=TRUE, sep="_"))
	tr$node.details<- as.integer(apply(as.matrix(tr$node.label), 1, getEl, ind=1, fromEnd=TRUE, sep="_"))
	tr$tip.details <- nodeDetails[tr$tip.details]
	tr$node.details<- nodeDetails[tr$node.details]
	tr$tip.label   <- apply(as.matrix(tr$tip.label), 1, getEl, ind=1, sep="_")
	
	translateTbl <- getTranslation(trTxt)
	tr$tip.label   <- translateTbl[match(tr$tip.label, translateTbl[,1]),2]
	tr$node.label  <- NULL

	#tr2 <- read.nexus(trName)
	#dist.topo(tr2,tr)

	tr$details	   <- c(tr$tip.details, tr$node.details)
	
	decDates	   <- as.numeric(apply(as.matrix(tr$tip.label), 1, getEl, ind=1, sep="\\|", fromEnd=TRUE))
	youngestTip	   <- max(decDates)
	tr		   <- nodeTimes(tr, youngestTip=youngestTip)

	#all(tr$nodeTimes[1:length(tr$tip.label)]==decDates)
	all( abs(tr$nodeTimes[1:length(tr$tip.label)]-decDates) < 1e-5 )
	
	traitVals	<- apply(as.matrix(tr$details), 1, getEl, ind=2, sep=paste(trait,"=",sep=""))
	traitVals	<- gsub("'","",traitVals)
	traitVals	<- apply(as.matrix(traitVals), 1, getEl, ind=1, sep=",")
	traitVals	<- gsub("\"","",traitVals)	   

	
	tr$state	<- traitVals

	return( tr ) 

}
