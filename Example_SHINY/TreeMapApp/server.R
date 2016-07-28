# Shiny server.R for TreeMapApp
# S J Lycett
# 9  July 2016
# 12 July 2016
# 21 July 2016
# 28 July 2016

############################################################
# compile custom functions
# only run once, e.g. read in data files etc

source("helper.R")

# global variables

# default colours
mapFillCol 		<- "grey80"	#"grey90"
mapBackCol 		<- "white"
inWindowCol	   	<- hsv(0, 0.9, 0.8)
outWindowCol	<- hsv(0, 0.2, 1.0)

colourValues				<- reactiveValues()
colourValues$mapFillCol 	<- mapFillCol
colourValues$mapBackCol 	<- mapBackCol
colourValues$inWindowCol	<- inWindowCol
colourValues$outWindowCol	<- outWindowCol

# filename of example tree
#treeName 		<- "example_data//fmdv_A_africa_142_empiricalsky_hbr_1.with_export.p0.tre"
treeName		<- "example_data//H5N6_outbreak_61_empiricalsky_latlon_hbr_1.tre"
tr				<- read_latlon_mcc_tr( trName=treeName )
tbl				<- getFromToLatLon( tr )

# internal values for App; for the tree and coordinate system
values			<- reactiveValues()
values$projection 	<- "x"
values$orientation	<- c("x")

values$tr			<- tr
values$tbl			<- tbl
values$trCoords 		<- getCoords(tr, tbl, projection="x",orientation=c("x"))
values$lims			<- getLimitsFromTree( tr )
values$origLims		<- getLimitsFromTree( tr )
		
############################################################
# SHINY server

shinyServer(function(input, output, session) {

	# this is compiled on launch
	# also for user session information

	#observe({
	#	updateSliderInput(session,"timelim",value=values$origLims$slim)
	#})

	observe({
		temp <- values$origLims$tlim
		updateSliderInput(session,"timelim",min=temp[1],max=temp[2],value=values$origLims$slim)
	})

	observe({
		updateSliderInput(session,"lonlim",value=values$origLims$xlim)
	})

	observe({
		updateSliderInput(session,"latlim",value=values$origLims$ylim)
	})
	
	##############################################
	# reactives
		
	readTree <- reactive({
		
			if (length(input$inputFile$datapath) == 0) {
				trName <- treeName
			} else {
				trName <- input$inputFile$datapath
			}
			tr		 <- read_latlon_mcc_tr( trName=trName )
			tbl		 <- getFromToLatLon(tr)
			
			# number of tips
			#print( length(tr$tip.label) )
			
			# get limits from tree
			origLims 	<- getLimitsFromTree( tr )
			lims		<- origLims
			
			# convert coordinates - of tip & internal nodes and of from to branches
			trCoords	<- getCoords(tr=tr,tbl=tbl,
								projection=values$projection,orientation=values$orientation)
			
			# update the internal values object
			values$tr 		<- tr
			values$tbl		<- tbl
			values$trCoords <- trCoords
			values$lims		<- lims
			values$origLims	<- origLims
	})
	
	setLimitsFromSliders <- reactive({				
		values$lims$xlim <- input$lonlim
		values$lims$ylim <- input$latlim
		values$lims$wlim <- input$timelim
	})
	
	setProjectionFromSelector <- reactive({
		if ( input$projectionSelect == 0 ) {
			projection 	<- "x"
			orientation <- c("x")
			updateSliderInput(session,"lonlim",value=values$origLims$xlim)
			updateSliderInput(session,"latlim",value=values$origLims$ylim)
		}
		if (input$projectionSelect== 1) {
			projection <- "mercator"
			orientation<- c(90,0,0)
			updateSliderInput(session,"lonlim",value=values$origLims$xlim)
			updateSliderInput(session,"latlim",value=values$origLims$ylim)
		}
		if (input$projectionSelect == 2) {
			projection <- "azequalarea"
			orientation<- c(90,0,0)
			updateSliderInput(session,"lonlim",value=c(-180,180))
			updateSliderInput(session,"latlim",value=c(-89,90))
		}
		
		# convert coordinates - of tip & internal nodes and of from to branches
		tr			<- values$tr
		tbl			<- values$tbl
		trCoords	<- getCoords(tr=tr,tbl=tbl,
						projection=projection,orientation=orientation)
						
		values$projection <- projection
		values$orientation<- orientation
		values$trCoords	  <- trCoords
		
	})
	
	setColoursFromSliders <- reactive({
		hs <- input$hue
		ss <- input$sat
		vs <- input$val		# brightness 1 - the lower (darker) value is for inWindowCol
		colourValues$outWindowCol <- hsv(hs[1],ss[1],vs[2])
		colourValues$inWindowCol  <- hsv(hs[2],ss[2],vs[1])
		
		mapCols <- c("white","grey80","grey90","black")
		colourValues$mapBackCol <- mapCols[as.integer(input$mapBackSelect)]
		colourValues$mapFillCol <- mapCols[as.integer(input$mapForeSelect)]
	})
	
	#############################################

	output$treeMapPlot <- renderPlot({
		
		# this is done every time the ui changes
		
		# read tree from file and set up initial coordinates					
		readTree()			
					
		# number of tips
		#print( length(values$tr$tip.label) )
		#print( values$lims$ntips )
					
		# convert coordinates - of tip & internal nodes and of from to branches
		setProjectionFromSelector()
		
		# set limits from sliders
		setLimitsFromSliders()
		
		# set colours from sliders
		setColoursFromSliders()
		
		# get points within the time window and colours of points
		values$trCoords	<- getPointsInWindow(values$trCoords, 
							windowStart=values$lims$wlim[1], windowEnd=values$lims$wlim[2],
							inWindowCol=colourValues$inWindowCol, 
							outWindowCol=colourValues$outWindowCol)
						
		
        # plot the tree on a map
		doMapPlot(trCoords=values$trCoords, lims=values$lims, 
					mapFillCol=colourValues$mapFillCol, mapBackCol=colourValues$mapBackCol)
		
     })
     
     output$treePlot	<- renderPlot({
     	# this is done every time the ui changes
		
		# read tree from file and set up initial coordinates					
		readTree()
		
		# set limits from sliders
		setLimitsFromSliders()
		
		# set colours from sliders
		setColoursFromSliders()
		
		# get points within the time window and colours of points
		getPointsInWindow(values$trCoords, 
							windowStart=values$lims$wlim[1], windowEnd=values$lims$wlim[2],
							inWindowCol=colourValues$inWindowCol, 
							outWindowCol=colourValues$outWindowCol)
		
		# do plot of ladderized tree with same colours as the map plot
		tr 		<- values$tr
		tr 		<- ladderize(tr)
		ntips	<- length(tr$tip.label)
		nnodes	<- length(tr$nodeTimes)
		ppch 	<- values$trCoords$ppch
		ccex	<- values$trCoords$ccex
		tcols1	<- values$trCoords$tcols1
		tcols2	<- values$trCoords$tcols2
				
		plot(tr, show.tip=FALSE, no.margin=TRUE)
		tiplabels(pch=ppch[1:ntips], bg=tcols1[1:ntips], col=tcols2[1:ntips])
		nodelabels(pch=ppch[(ntips+1):nnodes], bg=tcols1[(ntips+1):nnodes], col=tcols2[(ntips+1):nnodes])
     	
     })
     
	
})