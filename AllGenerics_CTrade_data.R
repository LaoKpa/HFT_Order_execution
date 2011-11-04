# TODO: Add comment
# 
# Author: Marozau
###############################################################################

setClass("CTrade_data", representation(instruments = "character",
				trades   = "data.frame",
				quotes	= "data.frame",	
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

setGeneric("read.data", function( x, n ) standardGeneric("read.data"))

setGeneric("calc.liquidity", function( x, time, N1, N2) standardGeneric("calc.liquidity"))
