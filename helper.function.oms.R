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

.plot <- function( object , n = 100, type = "", instrument)
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
		number_to_plot = nrow( quotes)
	}
	else number_to_plot = n
	
	sign_trades = c( 1, 3, 4 )
	trades_numeric <- do.call( "cbind", lapply(sign_trades, function(k) as.numeric( trades[, k] )))
	colnames( trades_numeric ) <- c( "time", "price", "size" )
	sign_quotes <-  c( 1, 3, 4, 5, 6 )
	quotes_numeric <- do.call( "cbind", lapply(sign_quotes, function(k) as.numeric( quotes[, k] )))
	colnames( quotes_numeric ) <- c( "time", "bid", "bid_size", "ask", "ask_size" )

	ymax <- max( quotes_numeric[ 1:number_to_plot, c("bid", "ask" )] ) 
	ymin <- min( quotes_numeric[ 1:number_to_plot, c("bid", "ask" )] ) 
	
	plot( quotes_numeric[ 1:number_to_plot, "time"], quotes_numeric[ 1:number_to_plot, "ask"], type = "l", col = "red", ylim = c(ymin, ymax))
	lines( quotes_numeric[ 1:number_to_plot,"time"], quotes_numeric[ 1:number_to_plot, "bid"], type = "l", col = "green")
	
	points( trades_numeric[ ,"time"], trades_numeric[ , "price"], type = "p", col = "blue", lwd = 3)
	
#	plot( quotes_numeric[ 1:number_to_plot, "ask"], type = "l", col = "red", ylim = c(ymin, ymax))
#	lines( quotes_numeric[ 1:number_to_plot, "bid"], type = "l", col = "green")	
}

## Conver ms to time format HH::MM::SS !!!! without ms !!!!
.to.time <- function(x)
{	
	x <- as.POSIXct(x - 10800, origin = Sys.Date())
	
	return(format(x, format = "%H:%M:%S"))
	
}

.calc.liquidity <- function(x, instr,  N1, N2, thresholds) {
	
	trades <- x@trades[x@trades[,"ID"] %in% instr,]
	quotes <- x@quotes[x@quotes[,"ID"] %in% instr,]
	
	## Check valididty of parameters N1 and N2
	N2 <- min(nrow(trades), N2)
	N1 <- min(N2, N1)
	
	## Crop unused data
	trades <- trades[1:N2,]
	
	# sd <- sd(as.numeric(trades[, "trade.price"]))
	
	sd.another <- c()
	for (k in nrow(trades):3) {
		sd.another <- rbind(sd.another, isPriceChange(k, trades))
	}
	#sd.another <- do.call("rbind", lapply(k = 3:nrow(trades), FUN = isPriceChange(k, trades)))
	
	mean <- mean(as.numeric(trades[, "trade.size"]))
	
	price <- as.numeric(last(trades[, "trade.price"]))
	
	spread.ma <- mean(sd.another)
	
	sd = sqrt(sum(sd.another^2))
	
	tick.sigma <<- rbind(tick.sigma, cbind(instr, sd)) 
#	spread.sum <- 0
#	empty <- 0
#	for (i in 1:N1) {
#		
#		position <- which(as.numeric(quotes[, "bid.time"]) < as.numeric(trades[i, "trade.time"]))
#		if (!identical(position, integer(0))) {
#			quote <- quotes[first(position), ]
#		} else 	quote <- quotes[1,]
#		
#				
#		spread <- abs(as.numeric(quote[, "ask.price"]) - as.numeric(quote[, "bid.price"]))
#		
#		if ((spread / price) > thresholds$spread.price) {  
#			empty <- empty + 1
#			next
#		}
#		else 
#			spread.sum <- spread.sum + spread
#	}
#	spread.ma <- spread.sum / (N1 - empty)
#	
#	if (spread.ma == Inf) return (-1)
#	
#	if (sd == 0) sd <- sd + thresholds$spread.ma * spread.ma
#	if (spread.ma == 0) spread.ma <- sd / thresholds$spread.ma	
#	if (sd < thresholds$spread.sd * spread.ma) sd <- sd + thresholds$spread.ma * spread.ma
	
	T <- sqrt(as.numeric(first(trades[, "trade.time"])) - as.numeric(last(trades[, "trade.time"])))
	
	factor2 <- mean / sd
	factor1 <- spread.ma / price
	factor <- factor2 / (factor1 * T)
	#	cat(instr, " : ", spread.sum, " ", factor1, "\n")
	
	return (factor)	
}

isPriceChange <- function(k, trades)
{
	change <- (as.numeric(trades[k , "trade.price"]) - as.numeric(trades[(k - 1), "trade.price"])) *
			(as.numeric(trades[(k - 1), "trade.price"]) - as.numeric(trades[(k - 2), "trade.price"]))
	if (change < 0) invisible (abs(as.numeric(trades[k , "trade.price"]) - as.numeric(trades[(k - 1), "trade.price"])))
}

.get_data <- function(data, start_time, stop_time)
{	
	return ( invisible ( data[ as.matrix( as.numeric( data[, "time"]) ) > start_time & as.matrix ( as.numeric( data[, "time"] ) ) < stop_time, ] ) ) 
}
