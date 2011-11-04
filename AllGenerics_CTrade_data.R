# TODO: Add comment
# 
# Author: Marozau
###############################################################################

setClass("CTrade_data", representation(instruments = "character",
				trades   = "data.frame",
				quotes	= "data.frame",	
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

setGeneric("read.data", function( x, n ) standardGeneric("read.data"))

setGeneric("calc.liquidity", function( x, time, N1, N2) standardGeneric("calc.liquidity"))

setGeneric("quantile.liquidity", function(x, N1, N2, step) standardGeneric("quantile.liquidity"))

setGeneric("make_data_feed", function(x) standardGeneric("make_data_feed"))
