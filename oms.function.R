# TODO: Add comment
# 
# Author: Marozau
###############################################################################


data.fun <- function(file.path = NULL )
{
	
	## Create an empty current order book data frame
	
	empty.trade <- data.frame()
	
	## Check to see that the file is valid and can be opened
	
	obfile <- file(file.path, open = "r")
	
	stopifnot(isOpen(obfile, "r"))
	
	close(obfile)
	
	## Return a new quotes object.
	
	invisible(new("CTrade_data",								
					file.path = file.path
			))
}

