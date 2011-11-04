# TODO: Add comment
# 
# Author: Marozau
###############################################################################
library(xts)
file.trades = "D://DB//Quotes//2011-04-15//trades_StatArb_2011-04-15.txt"
file.quotes = "D://DB//Quotes//2011-04-15//qoutes_StatArb_2011-04-15.txt"

data <- data.fun(file.trades = file.trades,
		file.quotes = file.quotes,
		instruments = c("DBA", "DBC", "DIA", "EEM", "EFA", "EWA", "EWC", "EWG", "EWJ", "EWT", "EWW", "EWY", "EWZ", "FXI", "GDX", "GLD", "IVV", "IWD", "IWF", "IWM" , "IWN", "IWO", "IYR", "MDY", "MOO", "OIH", "QLD", "QQQ", "RSX", "SLV",  "SMH", "SPY", "TLT", "UNG", "USO", "VNQ", "VTI", "VWO", "XLB", "XLE", "XLF", "XLI", "XLK", "XLP", "XLU", "XLV", "XLY", "XME", "XOP", "XRT"))

system.time(data <- data["10:00:50"])

data@trades <- read.trades(data, 256, 50000)
data@quotes <- read.quotes( data )

trades <- data@trades[ data@trades[ "ID" ] == "QQQ" & data@trades[ "price" ] !=  0, ] 
quotes <- data@quotes[ data@quotes[ "ID" ] == "QQQ" & data@quotes[ "ask" ] !=  0 & data@quotes[ "bid" ] !=  0, ]

sign_trades = c(1, 3, 4)
trades_numeric <- do.call( "cbind", lapply(sign_trades, function(k) as.numeric( trades[, k] )))
colnames( trades_numeric ) <- c( "time", "price", "size" )
sign_quotes <-  c(1, 3, 4, 5, 6)
quotes_numeric <- do.call( "cbind", lapply(sign_quotes, function(k) as.numeric( quotes[, k] )))
colnames( quotes_numeric ) <- c( "time", "ask", "ask_size", "bid", "bid_size" )

number_to_plot = 100
ymax <- max( quotes_numeric[ 1:number_to_plot, c("bid.price", "ask.price" )] ) 
ymin <- min( quotes_numeric[ 1:number_to_plot, c("bid.price", "ask.price" )] ) 

plot( quotes_numeric[ 1:number_to_plot, "bid.time"], quotes_numeric[ 1:number_to_plot, "ask.price"], type = "l", col = "red", ylim = c(ymin, ymax))
lines( quotes_numeric[ 1:number_to_plot,"bid.time"], quotes_numeric[ 1:number_to_plot, "bid.price"], type = "l", col = "green")

points( trades_numeric[ ,"trade.time"], trades_numeric[ , "trade.price"], type = "p", col = "blue", lwd = 3)

plot( quotes_numeric[ 1:number_to_plot, "ask.price"], type = "l", col = "red", ylim = c(ymin, ymax))
lines( quotes_numeric[ 1:number_to_plot, "bid.price"], type = "l", col = "green")

########################################
############ MEDIAN FILTERING ##########
########################################
start_to_plot = 1
number_to_plot = 200

quotes_ask_median_filter <- runmed( quotes_numeric[ ,"ask.price"], k = 101 )
quotes_bid_median_filter <- runmed( quotes_numeric[ ,"bid.price"], k = 101 )

ymax <- max( quotes_numeric[ start_to_plot:number_to_plot, c("bid.price", "ask.price" )] ) 
ymin <- min( quotes_numeric[ start_to_plot:number_to_plot, c("bid.price", "ask.price" )] )

plot( quotes_numeric[ start_to_plot:number_to_plot, "ask.price"], type = "l", col = "red", ylim = c(ymin, ymax))
lines( quotes_numeric[ start_to_plot:number_to_plot, "bid.price"], type = "l", col = "green")

lines( quotes_ask_median_filter[ start_to_plot:number_to_plot ], type = "l", col = "red", ylim = c(ymin, ymax), lwd = 2)
lines( quotes_bid_median_filter[ start_to_plot:number_to_plot ], type = "l", col = "green", lwd = 2)


########################################
############ AVERAGE FILTERING ##########
########################################
library( "TTR" )
start_to_plot = 100
number_to_plot = 200

quotes_ask_median_filter <- SMA( quotes_numeric[ ,"ask.price"], n = 25 )
quotes_bid_median_filter <- SMA( quotes_numeric[ ,"bid.price"], n = 25 )

ymax <- max( quotes_numeric[ start_to_plot:number_to_plot, c("bid.price", "ask.price" )] ) 
ymin <- min( quotes_numeric[ start_to_plot:number_to_plot, c("bid.price", "ask.price" )] )

plot( quotes_numeric[ start_to_plot:number_to_plot, "ask.price"], type = "l", col = "red", ylim = c(ymin, ymax))
lines( quotes_numeric[ start_to_plot:number_to_plot, "bid.price"], type = "l", col = "green")

lines( quotes_ask_median_filter[ start_to_plot:number_to_plot ], type = "l", col = "red", ylim = c(ymin, ymax), lwd = 2)
lines( quotes_bid_median_filter[ start_to_plot:number_to_plot ], type = "l", col = "green", lwd = 2)


########################################
############ New class with filtering
########################################
path = "D://DB//Quotes//dfast_trader_1.log"

tickdata <- data.fun( path ) 

tickdata <- read.data( tickdata, 1000000 )

z <- tickdata["13:46:47"]

.plot( z, 300, instrument = "QQQ" )

therachold <- 100000

