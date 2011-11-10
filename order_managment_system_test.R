# TODO: Add comment
# 
# Author: Marozau
###############################################################################

path = "E://Projects//DB//dfast_trader_1.log"

tickdata <- data.fun( path ) 

tickdata <- read.data( tickdata, 200000 )

z <- tickdata["13:46:47"]

.plot( z, 300, instrument = "QQQ" )

therachold <- 100000

