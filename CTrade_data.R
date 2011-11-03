# TODO: Add comment
# 
# Author: Marozau
###############################################################################

setClass("CTrade_data", representation(instruments = "character",
				trades   = "data.frame",
				quotes		= "data.frame",	
				orders = "data.frame",
				signals = "data.frame",
				current.time = "numeric",
				file.path = "character"			
		),
		
		prototype(instruments = character(),
				trades   = data.frame(),
				quotes = data.frame(),
				orders = data.frame(),
				signals = data.frame(),
				current.time = 0,
				file.path	= character()				
		)
)

setMethod(
		f="initialize",
		signature="CTrade_data",
		definition=function(.Object, file.path)
		{
			.Object@instruments = .get_instruments( file.path );
			.Object@file.path = file.path
			
			return ( invisible (.Object))			
		}
)


setMethod("[",
		signature(x = "CTrade_data", i = "character"),
		function(x, i){			
			
			start_time = .to.ms(i)
			stop_time = start_time + 60
			
			x@trades <- .get_data( x@trades, start_time, stop_time )
			x@quotes <- .get_data( x@quotes, start_time, stop_time )
			x@orders<- .get_data( x@orders, start_time, stop_time )
					
			return(x)
		}
)

setMethod("read.data",
		signature(x = "CTrade_data"),
		function( x, n ){
			
			print( system.time( x <- .read.data( x, n ) ) )		
			
			invisible( x )
		}
)

setMethod("calc.liquidity",
		signature(x = "CTrade_data"),
		function(x, N1, N2){	
			
			cat("~~~ calc.liqudury: start ~~~~~~~~~~~~\n")
			cat("Parameters:\n")
			cat("N1 = ", N1, "\n")
			cat("N2 = ", N2, "\n")
			
			instr.liquidity <- c()
			
			for (i in 1:length(x@instruments)) {
				
#				print(x@instruments[i])
				liquidity <- .calc.liquidity(x, x@instruments[i], N1, N2, thresholds = list(spread.price = 0.05, spread.sd = 0.05, spread.ma = 0.1))
				
				instr.liquidity <- cbind(instr.liquidity, liquidity)
			}
			
			colnames(instr.liquidity) <- x@instruments
			instr.liquidity <- instr.liquidity[,order(instr.liquidity)]
			instr.liquidity <- as.data.frame(instr.liquidity)
			
			cat("~~~ calc.liqudury: stop ~~~~~~~~~~~~~\n")
			invisible(instr.liquidity)
		}
)

setMethod("quantile.liquidity",
		signature(x = "CTrade_data"),
		function(x, N1, N2, step) {	
			
			if (x@file.trades.index == 1) invisible(x)
			
			x@trades <- read.trades(x, N2, step)
			
			x@quotes <- read.quotes(x)
			
			x@instr.liquidity <- calc.liquidity(x, N1, N2)
			
			invisible(x)
		}
)

setMethod("show",
		signature(object = "CTrade_data"),
		function(object) {
			
			cat("~~~ Show class CTrade_data: start... \n")
			cat("Trade File : "); print(object@file.trades)
			cat("quotes File : "); print(object@file.quotes)	
			cat("Instruments : "); print(object@instruments)
			cat("Current time = "); print(object@current.time)
			cat("Trade file index = "); print(object@file.trades.index)
			cat("Trade data length = "); print(nrow(object@trades))
			cat("quotes data length = "); print(nrow(object@quotes))
			cat("Liquidity : "); print(object@instr.liquidity)
			cat("~~~ Show class CTrade_data: end. \n")
		}
)

setMethod("make_data_feed",
		signature(x = "CTrade_data"),
		function(x) {
			
			.make_data_feed(x)
		}
)
## Next trade is easy to write using past calculated data. 

#setMethod("next.trade",
#		signature(object = "quotes"),
#		function(object){
#			## .get.next.trade finds the next trade
#			
#			n <- .get.next.trade(object@file, object@file.index, object@instrument, type = "[T]")
#			
#			invisible(read.orders(object, n))
#		}
#)


