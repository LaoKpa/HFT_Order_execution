# TODO: Add comment
# 
# Author: Marozau
###############################################################################

setClass("OMS", representation(
				tick_data = "CTrade_data"
		),
)

setGeneric( "make_trade", function(object, i ) standardGeneric( "make_trade" ) )


