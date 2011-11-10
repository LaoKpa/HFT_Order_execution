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
		start_to_plot <-  1
		stop_to_plot <-  nrow( quotes )
	} else 
	{
		start_to_plot <-  nrow( quotes ) - ( n - 1 ) 
		stop_to_plot <-  nrow( quotes )
	}
	
	if ( object@current_time != 0 )
	{
		position <- which( object@current_time >= as.matrix( as.numeric( quotes[ ,"time" ] ) ) )
		if ( length( position ) != 0 )	
		{
			position <- last( position )
			start_to_plot <-  position - n
			if ( position < 1) start_to_plot <- 1
			
			stop_to_plot <-  position + n
			if ( position > nrow( quotes ) ) stop_to_plot <- nrow( quotes )
		}
	}	

	trades_numeric <- .data_to_numeric( trades )
	quotes_numeric <- .data_to_numeric( quotes )	

	ymax <- max( quotes_numeric[ start_to_plot:stop_to_plot, c("bid", "ask" )] ) 
	ymin <- min( quotes_numeric[ start_to_plot:stop_to_plot, c("bid", "ask" )] ) 
	
	plot( quotes_numeric[ start_to_plot:stop_to_plot, "time"], quotes_numeric[ start_to_plot:stop_to_plot, "ask"], type = "o", col = "red", ylim = c(ymin, ymax))
	lines( quotes_numeric[ start_to_plot:stop_to_plot,"time"], quotes_numeric[ start_to_plot:stop_to_plot, "bid"], type = "o", col = "green")
	
	points( trades_numeric[ ,"time"], trades_numeric[ , "price"], type = "p", col = "blue", lwd = 3)
	
	if ( type == "filtered")
	{
		filtered_quotes <- .filtering_quotes( quotes_numeric, window)	
		
		lines( filtered_quotes[ start_to_plot:stop_to_plot,"time"], filtered_quotes[ start_to_plot:stop_to_plot, "bid"], type = "l", col = "black", lwd = 2)
		lines( filtered_quotes[ start_to_plot:stop_to_plot,"time"], filtered_quotes[start_to_plot:stop_to_plot, "ask"], type = "l", col = "black", lwd = 2)
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
	trades <- object@trades[ object@trades[ "price" ] !=  0, ]	
	
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
	quotes <- quotes[ quotes[ "ask" ] !=  0 & quotes[ "bid" ] !=  0, ]
	
	
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
		colnames( quotes_numeric ) <- c( "time", "ask", "ask_size", "bid", "bid_size" )
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

.get_price <- function( data, instrument, latency, av_depth, spread_sensitivity, side )
{	
	pretrade_quotes <- .get_quotes_data_befor_time( data, data@current_time, instrument, latency )
	pretrade_trades <- .get_trades_data_befor_time( data, data@current_time, instrument, latency )
	
	trade_time <- data@current_time + latency
	
	pretrade_filtered_quotes <- .filtering_data( pretrade_quotes, filter_window)
	average_window <- ( nrow( pretrade_filtered_quotes ) - 24 ):nrow( pretrade_filtered_quotes )
	average_spread <- mean( pretrade_filtered_quotes[ average_window, "ask"] - pretrade_filtered_quotes[ average_window, "bid"], )	
	average_bid <- mean( pretrade_filtered_quotes[ average_window, "bid"])
	average_ask <- mean( pretrade_filtered_quotes[ average_window, "ask"])
	current_ask <- last( pretrade_quotes["ask"])
	current_bid <- last( pretrade_quotes["bid"])
	
	if ( ( current_ask - current_bid ) > average_spread * spread_sensitivity )
	{
		if ( current_ask == average_ask )
		{
			ask = average_ask
			bid = ask - average_spread
		} else		
			if ( current_bid == average_bid )
			{
				bid = average_bid
				ask = bid + average_spread
			} else
			{
				last_trade <- last( pretrade_trades["price"])
				trade_ask <- ask - trade
				trade_bid <- trade - bid
				if( trade_ask < trdae_bid)
				{
					ask <-  last_trade
					bid <-  last_trade - average_spread
				} else
					if( trade_ask < trade_bid )
					{
						bid <- last_trade 
						ask <- bid + average_spread
					} else
					{
						ask <- last_trade + ( average_spread / 2 )
						bid <- last_trade - ( average_spread / 2 )
					}
			}
	}
	
	cat( "bid = ", bid, "ask = ", ask, "\n")
	
	bars_open <- pretrade_trades[ pretrade_trades[ ,"time"] >=  data@current_time & pretrade_trades[ ,"time"] <=  trade_time, ]
	if ( nrow( bars_open ) == 0 )
	{
		bars_open = 0
		
	} else
	{
		bars_open <- first( bars_open["price"])
	}
	
	if ( side == 1 )
	{
		reurn ( bid )
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
