# TODO: Add comment
# 
# Author: Marozau
###############################################################################
library(xts)
file.trades = "D://DB//Quotes//2011-04-15//trades_StatArb_2011-04-15.txt"
file.quotes = "D://DB//Quotes//2011-04-15//qoutes_StatArb_2011-04-15.txt"


q <- quotes(file.trades = file.trades,
		file.quotes = file.quotes,
		instruments = c("DBA", "DBC", "DIA", "EEM", "EFA", "EWA", "EWC", "EWG", "EWJ", "EWT", "EWW", "EWY", "EWZ", "FXI", "GDX", "GLD", "IVV", "IWD", "IWF", "IWM" , "IWN", "IWO", "IYR", "MDY", "MOO", "OIH", "QLD", "QQQ", "RSX", "SLV",  "SMH", "SPY", "TLT", "UNG", "USO", "VNQ", "VTI", "VWO", "XLB", "XLE", "XLF", "XLI", "XLK", "XLP", "XLU", "XLV", "XLY", "XME", "XOP", "XRT"))

q <- q["9:50:00"]

q <- read.trades(q, 128, 3000)

q <- read.quotes(q)

res <- calc.liquidity(q)

res[,order(res)]


################################################
########### TESTS ##############################
################################################
189663

test <- function()
{
	q <- quotes.fun(file.trades = file.trades,
			file.quotes = file.quotes,
			instruments = c("DBA", "DBC", "DIA", "EEM", "EFA", "EWA", "EWC", "EWG", "EWJ", "EWT", "EWW", "EWY", "EWZ", "FXI", "GDX", "GLD", "IVV", "IWD", "IWF", "IWM" , "IWN", "IWO", "IYR", "MDY", "MOO", "OIH", "QLD", "QQQ", "RSX", "SLV",  "SMH", "SPY", "TLT", "UNG", "USO", "VNQ", "VTI", "VWO", "XLB", "XLE", "XLF", "XLI", "XLK", "XLP", "XLU", "XLV", "XLY", "XME", "XOP", "XRT"))
	
	q <- q["9:50:00"]
	
	q@trades <- read.trades(q, 128, 50000)
	
	q@quotes <- read.quotes(q)
	
	q@instr.liquidity <- calc.liquidity(q)
	
	show(q)
}
system.time(res <- test())


## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 
## ## ## ## ##  TEST #################################

q.test <- quotes.fun(file.trades = file.trades,
		file.quotes = file.quotes,
		instruments = c("DBA", "DBC", "DIA", "EEM", "EFA", "EWA", "EWC", "EWG", "EWJ", "EWT", "EWW", "EWY", "EWZ", "FXI", "GDX", "GLD", "IVV", "IWD", "IWF", "IWM" , "IWN", "IWO", "IYR", "MDY", "MOO", "OIH", "QLD", "QQQ", "RSX", "SLV",  "SMH", "SPY", "TLT", "UNG", "USO", "VNQ", "VTI", "VWO", "XLB", "XLE", "XLF", "XLI", "XLK", "XLP", "XLU", "XLV", "XLY", "XME", "XOP", "XRT"))
system.time(q.test <- q.test["10:00:50"])

#system.time(q <- quantile.liquidity(q.test, 64, 256, 50000))

res1 <- matrix(0, nrow = 50)
colnames <- c("0")
N1 <- c(128, 64, 32, 16)
N2 <- c(256, 128)
for (i in 1:2) {
	for (j in 1:4) {
		system.time(q <- quantile.liquidity(q.test, N1[j], N2[i], 50000))
		res1 <- cbind(res1, cbind(rownames(q@instr.liquidity), q@instr.liquidity[,1]))
		colnames <- cbind(colnames, cbind(paste(N1[j], "_", N2[i]), paste(N1[j], "_", N2[i])))
	}
}

colnames(res1) <- as.character(colnames)
write.table(res, "D://DB//quotes//qoutes_volatility_stat_10_50.csv", sep = ",", col.names = TRUE)

for (i in seq(2,16, 2)) {
	for (j in seq(i,16, 2)) {
		
		if (i == j) next
		pos <- which(res[1:30,i] %in% res1[1:30,j])
		no.element <- which(!(c(1:30) %in% pos))
		cat("[", i, ",", j, "]: no element: ", res[no.element, i], "\n")
	}
}
### count number of lines
#if(.Platform$OS.type=="windows"){
#	system.time({
#				cmd<-system(paste('"D://Program Files//RKWard//R//Rtools//bin//wc" -l',"D://DB//Quotes//SpryMsg_2011-04-15.log"), intern=TRUE)
#				cmd<-strsplit(cmd, " ")[[1]][1]
#			})
#} 
#
#number.of.lines <- 178464800


#strptime(x[1,"trade.time"], "%H:%M:%OS")
#op <- options(digits.secs=3)
#options(op)
x<- q.test

x@trades <- read.trades(q.test, 256, 50000)

x@quotes <- read.quotes(x)

x@instr.liquidity <- calc.liquidity(x, 64, 256)

data_feed <- make_data_feed(x)

#data <- write.table(data_feed, file = "D://DB//Quotes//2011-04-15//statarb_system.tl", sep = "\t", col.names = FALSE, row.names = FALSE, append = TRUE, quote = FALSE)


#file = file("D://DB//Quotes//2011-04-15//test.txt", "w")
#close(file)
#index <- cbind(seq(nrow(data)- 1, 1, -2), seq(nrow(data), 1, -2)) 
#for (i in 1:nrow(index)) 
#	write.table(data[index[i, ],], file = "D://DB//Quotes//2011-04-15//test.txt", col.names = FALSE, row.names = FALSE, append = TRUE, quote = FALSE)

for (i in 1:nrow(liq)){
	if (as.numeric(liq[i,]) > 94988.123) liq[i,] <- 1
	else liq[i,] = 0
}

res <- calc.liquidity(q, 64, 256)
tick.sigma

lga <- geometric.mean(log(res))
names <- rownames(res)
sa <- c()

for (i in 1:nrow(res)) {
	sa = rbind(sa, cbind(names[i] ,0.4 * (log(res[i,]) / lga)^2 * as.numeric(tick.sigma[which(tick.sigma[,1] %in% names[i]), 2])))
}
sa.order <- sa[order(sa[,2]),]


file <- file(path, open = "r")	

## Header of the data
dimnames <- scan(file, nline = 3, sep = ",", what = "character", quiet = TRUE, skip = 0)
x <- scan(file, nline = 1, sep = "\t", what = "character", quiet = TRUE, skip = 0)		
