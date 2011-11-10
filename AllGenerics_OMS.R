# TODO: Add comment
# 
# Author: Marozau
###############################################################################

setClass("OMS", representation(
				tick_data = "CTrade_data"
				#liquidity_thereshold <- "numeric"
		),
)

setGeneric( "make_trade", function( object, time, instrument, price_change, data_depth, filter_window, latency, av_depth_time, spread_sensitivity, side ) standardGeneric( "make_trade" ) )


