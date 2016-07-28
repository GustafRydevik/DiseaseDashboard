# functions to get BEAST discrete trait colours
# S J Lycett
# 22 Dec 2015
# see H5NX work

# these are very slightly darker than the defaults below
get_Host_cols <- function() {
	# beast colours from figTree
	hCols		<- c(rgb(204/255,82/255,82/255),  rgb(143/255,204/255,82/255), rgb(82/255,204/255,204/255), rgb(143/255,82/255,204/255) )
	return( hCols )
}

get_BEAST_cols <- function( nstates, sat=0.7, bright=0.9 ) {
	ucols		<- hsv( (0:(nstates-1))/nstates, sat, bright)
	return( ucols )
}