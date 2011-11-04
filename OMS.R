# TODO: Add comment
# 
# Author: Marozau
###############################################################################

setClass("OMS", representation(
			tick_data = "CTrade_data",
			liquidity_thereshold <- "numeric"
		),
)


setMethod(
		f="initialize",
		signature="OMS",
		definition=function( .Object,  tick_data, liquidity_thereshold)
		{
			if ( class( tick_data )[1]  == "CTrade_data") 
				.Object@tick_data <- tick_data
			else
				stop("Tick_data: wrong class!")			
			
			.Object@liquidity_thereshold <- liquidity_thereshold
			return ( invisible (.Object))			
		}
)

setMethod( "make_trade",
		signature( object = "OMS"),
		function( object, i, instrument)
		{
			data <- tick_data[ i ]
			
			
			
			
		}
)