# TODO: Add comment
# 
# Author: Marozau
###############################################################################

setClass("CTrade_data", representation(instruments = "character",
				trades   = "data.frame",
				quotes		= "data.frame",	
				orders = "data.frame",
				signals = "data.frame",
				current_time = "numeric",
				file.path = "character"			
		),
		
		prototype(instruments = character(),
				trades   = data.frame(),
				quotes = data.frame(),
				orders = data.frame(),
				signals = data.frame(),
				current_time = 0,
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

setMethod("read.data",
		signature(x = "CTrade_data"),
		function( x, n ){
			
			print( system.time( x <- .read.data( x, n ) ) )		
			
			invisible( x )
		}
)


setMethod("[",
		signature(x = "CTrade_data", i = "character"),
		function(x, i){			
			
			i_to_list <- strsplit( i, split = "::")[[1]]
			if ( length( i_to_list ) == 2)
			{
				if ( i[ 1] != "") 
				{
					start_time = .to.ms( i[ 1 ] )
				}
				else 
				{
					start_time = 1					
				}
				stop_time = .to.ms( i[ 2 ] )				
			}
			
			if ( length( i_to_list ) == 1)
			{
				start_time = 1				
				stop_time = .to.ms( i[ 1 ] ) + 60
				x@current_time = .to.ms( i[ 1 ] )
			}			
			
			if ( start_time > stop_time ) stop( "wrong time format!")			
			
			x@trades <- .get_data( x@trades, start_time, stop_time )
			x@quotes <- .get_data( x@quotes, start_time, stop_time )
			x@orders<- .get_data( x@orders, start_time, stop_time )
			x@signals <- .get_data( x@signals, start_time, stop_time )
			
			return( x )
		}
)

setMethod("calc.liquidity",
		signature(x = "CTrade_data"),
		function(x, time, N1, N2){	
			
			cat("~~~ calc.liqudury: start ~~~~~~~~~~~~\n")
			cat("Parameters:\n")
			cat("N1 = ", N1, "\n")
			cat("N2 = ", N2, "\n")
			
			if ( is.character( time ) )
			{
				time = .to.ms( time )
			}			
		
			instr.liquidity <- c()			
			for (i in 1:length(x@instruments)) {				
#				
				liquidity <- .calc.liquidity(x, time, x@instruments[i], N1, N2)
				
				instr.liquidity <- cbind(instr.liquidity, liquidity)
			}
			
			colnames(instr.liquidity) <- x@instruments
			instr.liquidity <- instr.liquidity[,order(instr.liquidity)]
			instr.liquidity <- as.data.frame(instr.liquidity)
			
			cat("~~~ calc.liqudury: stop ~~~~~~~~~~~~~\n")
			invisible(instr.liquidity)
		}
)

setMethod("show",
		signature(object = "CTrade_data"),
		function(object) {
			
			cat("~~~ Show class CTrade_data: start... \n")
			cat("Data File : "); print(object@file.path)				
			cat("Instruments : "); print(object@instruments)
			cat("Current time = "); print(object@current_time)
			
			cat("Trade data length = "); print(nrow(object@trades))
			cat("Quotes data length = "); print(nrow(object@quotes))
			cat("Orders data length = "); print(nrow(object@orders))
			cat("Signals data length = "); print(nrow(object@signals))
			cat("Start time  = "); cat( .to.time( first( object@quotes[,"time"]) ), "(", first( object@quotes[,"time"]), ")\n" )
			cat("End time  = "); cat( .to.time( last( object@quotes[,"time"]) ), "(", last( object@quotes[,"time"]), ")\n" )
			
			cat("~~~ Show class CTrade_data: end. \n")
		}
)

