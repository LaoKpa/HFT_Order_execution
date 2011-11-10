# TODO: Add comment
# 
# Author: Marozau
###############################################################################

setClass("OMS", representation(
			tick_data = "CTrade_data"
			#liquidity_thereshold <- "numeric"
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
			
			#.Object@liquidity_thereshold <- as.numeric( liquidity_thereshold )
			return ( invisible (.Object))			
		}
)

setMethod( "make_trade",
		signature( object = "OMS"),
		function( object, 
				time, 
				instrument, 
				price_change, 
				data_depth, 
				filter_window, 
				latency = 0, 
				av_depth, 
				spread_sensitivity, 
				side )
		{			
			data <- object@tick_data[ time ]
			
			if ( data@current_time %% 60 != 0 ) stop( "time must multiply by minutes!")
			
			data_liquidity <- .calc.liquidity( data, data@current_time, instrument,  price_change, data_depth)		
			
			price <- .get_price( data, instrument, latency, filter_window, av_depth, spread_sensitivity, side )
			
			return ( price )
		}
)


