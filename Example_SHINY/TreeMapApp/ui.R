# Shiny ui.R for TreeMapApp
# S J Lycett
# 10 July 2016
# 12 July 2016
# 28 July 2016


shinyUI(fluidPage(
  titlePanel("R-Phylogeography: TreeMapApp"),
  
  #fluidRow(
  #
  #	wellPanel(
  #  		h3("Input File"),
  #  		fileInput("inputFile","Tree File", multiple = FALSE, accept = NULL)
  #  		)
  #),
  
  fluidRow(
  	column(6, 
  		wellPanel(
  			h3("Tree Display"),
    		plotOutput("treePlot")
    		#h3("Map Display"),
      		#plotOutput("treeMapPlot")
      		)),
  	
  	column(6, 
  		wellPanel(
  			h3("Map Display"),
      		plotOutput("treeMapPlot")
    		#h3("Tree Display"),
    		#plotOutput("treePlot")
    	))
  ),
  
  fluidRow(
  	column(6,
  	wellPanel(
    		h3("Input File"),
    		fileInput("inputFile","Tree File", multiple = FALSE, accept = NULL)
    		)),
  
    column(3, 
    		
    		wellPanel(
    		h3("Map Controls"),
    		selectInput("projectionSelect","Projection:",
    			choices=list(	"None" = 0,
    							"Mercator" = 1, 
    							"AzEqualArea" = 2),
    			selected = 0),
    		
      		#sliderInput("lonlim",
                #  	"Longitude Zoom:",
                #  	min = -180,
                #  	max = 180,
                #  	value = c(-20,50), 
			#		step = 1),

      		#sliderInput("latlim",
                #  	"Latitude Zoom:",
                #  	min = -90,
                #  	max = 90,
                #  	value = c(-10,40),
			#		step = 1),
					
			#sliderInput("timelim",
			#		"Time scale zoom:",
			#		min=1900,
			#		max=2016,
			#		value = c(1930,2016),
			#		step = 1),

      		sliderInput("lonlim",
                  	"Longitude Zoom:",
                  	min = -180,
                  	max = 180,
                  	value = c(-180,180), 
					step = 1),

      		sliderInput("latlim",
                  	"Latitude Zoom:",
                  	min = -90,
                  	max = 90,
                  	value = c(-89,89),
					step = 1),
					
			sliderInput("timelim",
					"Time scale zoom:",
					min=1900,
					max=2017,
					value = c(1930,2017),
					step = 0.1),
                  
      		
      		submitButton("Submit")
    		)
    	),
    		
    column(3,
    		
    		wellPanel(
    		h3("Colour Control"),
    		
    		sliderInput("hue",
    				"Points Hue:",
    				min=0,
    				max=1,
    				value=c(0,0),
    				step=0.05),
    				
    		sliderInput("sat",
    				"Points Saturation:",
    				min=0,
    				max=1,
    				value=c(0.2,0.9),
    				step=0.05),
    				
    		sliderInput("val",
    				"Points Brightness:",
    				min=0,
    				max=1,
    				value=c(0.8,1),
    				step=0.05),
    		
    		selectInput("mapBackSelect","Map Background:",
    			choices=list(	"White" = 1,
    							"Grey90" = 2, 
    							"Grey80" = 3,
    							"Black" = 4),
    			selected = 1),
    			
    		selectInput("mapForeSelect","Map Foreground:",
    		choices=list(	"White" = 1,
    						"Grey90" = 2, 
    						"Grey80" = 3,
    						"Black" = 4),
    			selected = 3),
    		
    		submitButton("Submit")
    		)
    	)
    
  )
  
))


doSideBarLayout <- FALSE
if (doSideBarLayout) {
	
shinyUI(fluidPage(
  titlePanel("R-Phylogeography: TreeMapApp"),
  
  sidebarLayout(
  
  sidebarPanel(
  
    		fileInput("inputFile", "Tree File", multiple = FALSE, accept = NULL),
    		
    		selectInput("projectionSelect","Projection:",
    			choices=list(	"None" = 0,
    							"Mercator" = 1, 
    							"AzEqualArea" = 2),
    			selected = 0),
    		
      		sliderInput("lonlim",
                  	"Longitude Zoom:",
                  	min = -180,
                  	max = 180,
                  	value = c(-20,50), 
					step = 1),

      		sliderInput("latlim",
                  	"Latitude Zoom:",
                  	min = -90,
                  	max = 90,
                  	value = c(-10,40),
					step = 1),
					
			sliderInput("timelim",
					"Time scale zoom:",
					min=1900,
					max=2016,
					value = c(1930,2016),
					step = 1),
                  
      		
      		submitButton("Submit")
    ),
    
    mainPanel(
      plotOutput("treeMapPlot", width = 600, height = 600)
      #plotOutput("treeMapPlot")
    )
    
  )
  
))

}

