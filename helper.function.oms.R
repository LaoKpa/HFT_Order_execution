# TODO: Add comment
# 
# Author: Marozau
###############################################################################
.get_instruments <- function( file.path )
{
	instruments <- character();
	
	## Open file to read
	file <- file(file.path, open = "r")
	stopifnot(isOpen(file, "r"))
	
	instruments <- scan( file, nline = 1, sep = " ", what = "character", quiet = TRUE, skip = 0)
	
	return ( invisible ( instruments[-1]) )
}

.read.data <- function( object, n = 50000 ) {
	
	## Open file to read	
	file <- file(object@file.path, open = "r")	
	trades <- c()
	quotes <- c()
	orders <- c()
	signals <- c()
	
	## Header of the data
	dimnames <- scan(file, nline = 3, sep = ",", what = "character", quiet = TRUE, skip = 0)
	
	## Write trades in a matrix
	cat( "File reading start...\n")
	print( system.time( x <- scan(file, nlines = n, sep = "\t", what = "character", quiet = TRUE, skip = 0) ) )
	cat( "File reading completed.\n")
	
	cat( "Parsing data start...\n")
	start_index = 1
	stop_index = 1
	length_x <- length( x )
	while ( start_index <=  length_x )
	{		
		if ( x[ start_index ] == "5")
		{
			while ( x[ stop_index ] == "5" & stop_index <= length_x )
			{
				stop_index <- stop_index + 8	
			}
			quotes <- c( quotes, x[ c( start_index:( stop_index - 1 ) ) ] )
			start_index <- stop_index
			next
		}
		if ( x[ start_index ] == "4")
		{
			while ( x[ stop_index ] == "4" & stop_index <= length_x )
			{
				stop_index <- stop_index + 7	
			}
			trades <- c( trades, x[ c( start_index:( stop_index - 1) ) ] )
			start_index <- stop_index
			next
		} 
		if ( x[ start_index ] == "1" || x[ start_index ] == "2")
		{
			signals <- c( signals, x[ c( ( start_index ):( start_index + 2 ) ) ] )
			start_index <- start_index + 3
			stop_index <- stop_index + 3
			next
		}
		if ( x[ start_index ] == "6")
		{
			while ( x[ stop_index ] == "6" & stop_index <= length_x )
			{
				stop_index <- stop_index + 16	
			}
			orders <- c( orders, x[ c( ( start_index ):( stop_index - 1 ) ) ] )
			start_index <- stop_index			
			next
		}
		
		start_index <- start_index + 1
		stop_index <- stop_index + 1		
	}
	if ( n!= -1)
	{
		cat(n, " lines handled.\n")
	}
		
	close(file)
	
	if ( length( quotes ) != 0) 
	{	
		quotes_matrix <- matrix( quotes, ncol = 8,  byrow = TRUE )[, -c(1,8)]
		colnames( quotes_matrix ) <- c("time", "ID", "bid", "bid_size", "ask", "ask_size")		
		object@quotes <- as.data.frame(quotes_matrix, stringsAsFactors = FALSE)		
	}
	
	if ( length( trades ) != 0)
	{
		trades_matrix <- matrix( trades, ncol = 7,  byrow = TRUE )[, -c(1,7)]
		colnames( trades_matrix ) <- c("time", "ID", "price", "size", "cum_size")
		object@trades <- as.data.frame(trades_matrix, stringsAsFactors = FALSE)
	}
	
	if ( length( orders ) != 0)
	{
		orders_matrix <- matrix( orders, ncol = 16, byrow = TRUE )[, -1 ]
		colnames( orders_matrix ) <- c("time", "order_id", "cl_order_id", "exec_id", "exec_trans_type", "exec_type", "ord_status", "ticker_symbol", "side", "order_qty", "leaves_qty", "cum_qty", "avg_px", "last_price", "last_shares")
		object@orders <- as.data.frame(orders_matrix, stringsAsFactors = FALSE)
	}
	if ( length( signals ) != 0)
	{
		signals_matrix <- matrix( signals, ncol = 3, byrow = TRUE)
		colnames( signals_matrix ) <- c("type", "time", "side")
		object@signals <- as.data.frame(signals_matrix, stringsAsFactors = FALSE)
	}	
	
	cat( "Parsing data completed\n")
	return ( object )
}

## Conver HH::MM::SS.SSS to numeric format in ms
.to.ms <- function(x){
	
	x <- strsplit(x, split = ":")[[1]]
	x <- ((as.numeric(x[1])) * 3600
				+ as.numeric(x[2]) * 60
				+ as.numeric(x[3]) )
	
	return(signif(x, 8))
	
}

.plot <- function( object , n = 100, type = "filtered", instrument, window = 7)
{		
	if ( n <= 0 ) 
	{
		cat("n is les then zero. Set to 100.\n")
		n <- 100
	}
	
	instrument_in_data <- which( object@instruments %in% instrument)
	if ( identical(instrument_in_data, integer(0) ) ) 
	{
		cat( "No such instruments.\n")
		cat("Try one of:", object@instruments, "\n")
		return ()
	}
	
	trades <- object@trades[ object@trades[ "ID" ] == instrument & object@trades[ "price" ] !=  0, ] 
	quotes <- object@quotes[ object@quotes[ "ID" ] == instrument & object@quotes[ "ask" ] !=  0 & object@quotes[ "bid" ] !=  0, ]
	
	if ( nrow( quotes ) < n ) 
	{
		quotes_to_plot <- quotes
	} else 
	{
		start_to_plot <-  nrow( quotes ) - ( n - 1 ) 
		stop_to_plot <-  nrow( quotes )
		quotes_to_plot <- quotes[ start_to_plot:stop_to_plot,]
	}
	
	if ( object@current_time != 0 )
	{
		quotes_to_plot <- quotes[ quotes[ ,"time"] >=  object@current_time - n/10 & quotes[ ,"time"] < object@current_time + n/10, ]
		if ( nrow( quotes_to_plot) == 0 ) stop(" bad time to data")
	}	

	trades_numeric <- .data_to_numeric( trades )
	quotes_numeric <- .data_to_numeric( quotes_to_plot )	

	ymax <- mean( quotes_numeric[ , "ask"] ) + sd( quotes_numeric[ , "ask"] ) 
	ymin <- mean( quotes_numeric[ , "bid"] ) - sd( quotes_numeric[ , "bid"] )
	
	plot( quotes_numeric[ , "time"], quotes_numeric[ , "ask"], type = "o", col = "green", ylim = c(ymin, ymax))
	lines( quotes_numeric[ ,"time"], quotes_numeric[ , "bid"], type = "o", col = "red")
	
	points( trades_numeric[ ,"time"], trades_numeric[ , "price"], type = "p", col = "blue", lwd = 3)
	
	if ( type == "filtered")
	{
		filtered_quotes <- .filtering_quotes( quotes_numeric, window)	
		
		lines( filtered_quotes[ ,"time"], filtered_quotes[ , "bid"], type = "l", col = "black", lwd = 2)
		lines( filtered_quotes[ ,"time"], filtered_quotes[, "ask"], type = "l", col = "black", lwd = 2)
	}
	
	if ( object@current_time != 0 ) abline( v = object@current_time, col = "dark blue" )
}

## Conver ms to time format HH::MM::SS !!!! without ms !!!!
.to.time <- function(x)
{	
	x <- as.numeric( x )
	x <- as.POSIXct(x - 10800, origin = Sys.Date())
	
	return(format(x, format = "%H:%M:%S"))
	
}

.calc.liquidity <- function(object, time, instr,  N1, N2) {
	
	trades <- .get_trades_data_befor_time( object, time, instr)
	if ( nrow( trades) == 0) return ( 1 )
	
	price_triangles <- .get_price_triangles( trades, N1)	
	average_spread <- mean(as.numeric( price_triangles ) )
	sigma_price <- sqrt(sum( price_triangles^2))
	
	current_price <- as.numeric( last ( trades[, "price"]))	
	
	if ( nrow( trades ) <= N2 )
	{
		N2 <- nrow( trades ) - 1
	}
	
	duration <- as.numeric( trades[ nrow( trades ), "time"] ) - as.numeric( trades[ (nrow( trades ) - ( N2 - 1 )), "time"] )
	if ( duration != 0 ) T <- sqrt( duration * 1000 )
	else T <- 1
	
	average_trade_vol <- mean( as.numeric( trades[ ( nrow( trades ) - N2):( nrow( trades ) ), "size"] ) )
	
	factor1 = average_spread / current_price;
	factor2 = average_trade_vol / sigma_price;
	result =  ( factor2 / factor1 ) / T;
	if ( result < 1 ) result = 1;	
	
	return (result)	
}

.get_price_triangles <- function( trades, N )
{
	triangles <- data.frame()
	i <- 2:nrow( trades )
	price_change <- trades[ trades [ i, "price"] != trades[ i - 1, "price" ], ]	
	for (k in nrow( price_change ):3) {
		triangles <- rbind( triangles, price_triangle(price_change, k))		
	}
	if ( nrow( triangles ) < N ) N <- nrow( triangles )
	return ( invisible ( triangles[ 1:N, ] ) )
}

price_triangle <- function( trades, k )
{	
	change <- (as.numeric(trades[k , "price"]) - as.numeric(trades[(k - 1), "price"])) *
			(as.numeric(trades[(k - 1), "price"]) - as.numeric(trades[(k - 2), "price"]))
	if (change < 0) invisible (abs(as.numeric(trades[k , "price"]) - as.numeric(trades[(k - 1), "price"])))
}

.get_data <- function(data, start_time, stop_time)
{	
	if ( nrow(data) == 0) return ( invisible ( data.frame() ) )
	return ( invisible ( data[ as.matrix( as.numeric( data[, "time"]) ) >= start_time & as.matrix ( as.numeric( data[, "time"] ) ) < stop_time, ] ) ) 
}

.get_trades_data_befor_time <- function( object, time, instr, latency = 0 )
{
	time <- time + latency
	
	trades <- object@trades[ object@trades[,"ID"] %in% instr, ]	
	position <- which( time >= as.matrix( as.numeric( trades[ ,"time" ] ) ) )
	if ( length( position ) == 0)
		return ( data.frame() )
	position <- last( position )
	
	## Crop unused data
	trades <- trades[1:position,]	
	trades <- trades[ trades[ ,"price" ] !=  0, ]	
	
	trades_numeric <- .data_to_numeric( trades )	
	
	return ( invisible ( trades_numeric ))
}

.get_quotes_data_befor_time <- function( object, time, instr, latency = 0 )
{
	time <- time + latency
	
	quotes <- object@quotes[ object@quotes[,"ID"] %in% instr, ]	
	position <- which( time >= as.matrix( as.numeric( quotes[ ,"time" ] ) ) )
	if ( length( position ) == 0)
		return ( data.frame() )
	position <- last( position )
	
	## Crop unused data
	quotes <- quotes[1:position,]	 
	quotes <- quotes[ quotes[ ,"ask" ] !=  0 & quotes[ ,"bid" ] !=  0, ]
	
	
	quotes_numeric <- .data_to_numeric( quotes )	
	
	return ( invisible ( quotes_numeric ))
}

.get_data_befor_time <- function( object, time, instr )
{	
	current_time <- time
	seconds_in_a_minute <- 60
	time <- time + seconds_in_a_minute
	
	trades <- object@trades[ object@trades[,"ID"] %in% instr, ]	
	position <- which( time >= as.matrix( as.numeric( trades[ ,"time" ] ) ) )
	if ( length( position ) == 0)
	{
		trades <-  data.frame()
	}
	position <- last( position )
	
	## Crop unused data
	trades <- trades[1:position,]	
	object@trades <- trades

	quotes <- object@quotes[ object@quotes[,"ID"] %in% instr, ]	
	position <- which( time >= as.matrix( as.numeric( quotes[ ,"time" ] ) ) )
	if ( length( position ) == 0)
	{
		quotes <- data.frame()
	}
	position <- last( position )
	
	## Crop unused data
	quotes <- quotes[1:position,]

	object@quotes <- quotes
	
	object@current_time <- current_time
	
	return ( invisible ( object ))
}

.data_to_numeric <- function( data )
{
	if ( ncol( data ) == 5)
	{
		sign_trades = c(1, 3, 4)
		trades_numeric <- do.call( "cbind", lapply(sign_trades, function(k) as.numeric( data[, k] )))
		colnames( trades_numeric ) <- c( "time", "price", "size" )
	
		return ( invisible ( trades_numeric ) )
	}
	
	if ( ncol( data ) == 6 )
	{
		sign_quotes <-  c(1, 3, 4, 5, 6)
		quotes_numeric <- do.call( "cbind", lapply(sign_quotes, function(k) as.numeric( data[, k] )))
		colnames( quotes_numeric ) <- c( "time", "bid", "bid_size", "ask", "ask_size" )
		return ( invisible ( quotes_numeric ))
	}
	
	return ( invisible  ( data ) )
}

.median_filter <- function( data, window )
{
	filtered_data <- runmed( data[ length(data):1], k = window,  endrule = "constant")
	return ( invisible ( filtered_data[ length(filtered_data):1] ) )
}

.filtering_quotes <- function( x, window)
{	
	quotes <- x
	
	quotes_ask_median_filter <- .median_filter( quotes[ ,"ask"], window )
	quotes_bid_median_filter <- .median_filter( quotes[ ,"bid"], window )
	
	quotes[,"ask"] <- quotes_ask_median_filter
	quotes[,"bid"] <- quotes_bid_median_filter
	
	return ( invisible ( quotes ) )
	
}

.get_price <- function( data, instrument, latency, filter_window, av_depth_time, spread_sensitivity, side )
{	
	pretrade_quotes <- .get_quotes_data_befor_time( data, data@current_time, instrument, latency )
	pretrade_trades <- .get_trades_data_befor_time( data, data@current_time, instrument, latency )
	
	trade_time <- data@current_time + latency
	
	pretrade_filtered_quotes <- .filtering_quotes( pretrade_quotes, filter_window)
	
	data_to_average <- pretrade_filtered_quotes[ pretrade_filtered_quotes[ ,"time"] >= ( data@current_time - av_depth_time), ]
	data_to_average <- data_to_average[ -c( ( nrow( data_to_average ) - filter_window %/% 2 + 1 ):nrow( data_to_average ) ), ]
#	average_window <- ( nrow( pretrade_filtered_quotes ) - av_depth ):( nrow( pretrade_filtered_quotes) - filter_window %/% 2 + 1 ) 
	average_spread <- mean( data_to_average[ , "ask"] - data_to_average[ , "bid"])	
	average_bid <- mean( data_to_average[ , "bid"])
	average_ask <- mean( data_to_average[ , "ask"])
	current_ask <- last( data_to_average[,"ask"])
	current_bid <- last( data_to_average[,"bid"])
	current_real_ask <- last( pretrade_quotes[,"ask"])
	current_real_bid <- last( pretrade_quotes[,"bid"])	
	
	cat( "average_spread: =" , average_spread, "\n")
	cat( "average_bid: =", average_bid, "\n")
	cat( "current bid: =" , current_bid, "\n")
	cat( "average_ask: =", average_ask, "\n")
	cat( "current ask: =", current_ask, "\n")	
	
	if ( round( average_spread, 2 ) < ( current_ask - current_bid ) && round( average_spread, 2 ) > 0)
	{
		if ( abs( current_ask - average_ask ) < 0.001 )
		{
			ask = current_ask
			bid = ask - round( average_spread, 2 )		
		} else		
			if ( abs( current_bid - average_bid ) < 0.001 )
			{
				bid = current_bid
				ask = bid + round( average_spread, 2 )
			} else
				if( abs( current_real_bid - average_bid ) < 0.001 )
				{
					bid = current_real_bid
					ask = bid + round( average_spread, 2 )
				} else
					if ( abs( current_real_ask - average_ask ) < 0.001 )
					{
						ask = current_real_ask
						bid = ask - round( average_spread, 2 )
					} else
					{			
						last_trade <- last( pretrade_trades[,"price"])
						
						trade_ask <- current_ask - last_trade
						trade_bid <- last_trade - current_bid
						if( trade_ask > 0 & trade_bid > 0 )
						{
							ask <-  current_ask
							bid <-  current_bid
						} else
							if( trade_bid < 0 )
							{
								bid <- last_trade 
								ask <- bid + round( average_spread, 2 )
							} else
							 	if ( trade_ask < 0) 
								{
									ask <- last_trade 
									bid <- ask - round( average_spread, 2 )
								}
					}
	} else
	{
		ask <- current_ask
		bid <- current_bid
	}
	
	
	cat( "bid = ", bid, "ask = ", ask, "\n")

	if ( side == 1 )
	{
		return ( bid )
	} else
		if ( side == -1)
		{
			return ( ask )
		} else
		{
			return ( 0 )
		}
}


# caculation of minutes start
#.to.time( 14123 - ( 14123 %% 60 ) )

.market_status <- function( data, instrument )
{
	
}


bars_open <- pretrade_trades[ pretrade_trades[ ,"time"] >=  data@current_time & pretrade_trades[ ,"time"] <=  trade_time, ]
if ( nrow( bars_open ) == 0 )
{
	bars_open = 0
	
} else
{
	bars_open <- first( bars_open["price"])
}

