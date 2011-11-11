# TODO: Add comment
# 
# Author: Marozau
###############################################################################

#path = "E://Projects//DB//dfast_trader_1.log"

path = "D://DB//Quotes//dfast_trader_1.log"


tickdata <- data.fun( path ) 

tickdata <- read.data( tickdata, 200000 )

z <- tickdata["13:46:47"]

.plot( z, 300, instrument = "QQQ" )

therashold <- 100000

oms <- new(Class = "OMS", tick_data = tickdata)

ins <- "QQQ"
make_trade( object = oms,
		time = "13:50:00",
		instrument = ins, 
		price_change = 64, 
		data_depth = 256, 
		filter_window = 19, 
		latency = 0, 
		av_depth_time = 2, 
		spread_sensitivity = 4, 
		side = 1)

data <- oms@tick_data[ "13:50:00" ]
.plot( data, instrument = ins)