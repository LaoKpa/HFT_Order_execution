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


ts <- data@trades[ data@trades["ID"]=="QQQ", ]
prices <- ts[, "price"]
chart_serie <- xts( runmed( as.numeric( prices ), k = 9), as.POSIXct( as.numeric( ts[,"time"]) - 10800, origin = Sys.Date()) ) 
chartSeries( chart_serie , subset='2011-11-16 13:48:02::2011-11-16 13:48:03')

ts_trades <- data@trades[ data@trades["ID"]=="QQQ", ]

ts_quotes <- data@quotes[ data@quotes["ID"]=="QQQ", ]
quotes_imbalance <- as.numeric( ts_quotes[,"ask"] ) * as.numeric( ts_quotes[,"ask_size"] ) -  as.numeric( ts_quotes[,"bid"] ) * as.numeric( ts_quotes[,"bid_size"] )
ts_quotes_imbalance <- cbind( as.numeric( ts_quotes[ ,"time" ] ), quotes_imbalance)


par( mfrow = c(2,1))
to_plot <- 100:6400
#plot( as.numeric( ts_trades[,"time"] )[to_plot], as.numeric( ts_trades[,"price"] )[to_plot], type = "l")
plot( as.numeric( ts_quotes[,"time"] )[to_plot], runmed( ts_quotes[,"ask"], k = 19 )[to_plot], type = "l")
plot( as.numeric( ts_quotes_imbalance[,1])[to_plot], runSum( ts_quotes_imbalance[ ,2], n = 100 )[to_plot], type = "l" )
abline( h = 0, col = "red")
