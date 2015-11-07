## funkcijo ki vzame enako st ur dneva in noci

izenaciNoc <- function(realizacij, ## realizacija proizvodnje (xts)
					stObd = 2 ## stevilo ur dneva/noci ki jih obdrzi
						)
				## funkcija ki iz podatkov izbere enako stevilo ur 
				## ponoci in podnevi (stObd nakljucno izbranih vsak dan)
				## vrne xts z NA na mestih ki niso izbrani
						
	{				
	realDan <- realizacija
	## ali je realizacija 0
	proizvaja <- realizacija != 0
	## zacetek pregledovanja
	zacetek <- as.Date(index(realizacija)[1])
	obdrziOdstrani <- NULL
	for (i in 1:(ndays(realizacija)-1)){
		dan <- zacetek + i
		poglej <- paste0(dan, '/', dan)
		danNoc <- proizvaja[poglej]
		noc <- (1:length(danNoc))[danNoc == FALSE & !is.na(danNoc)]
		dan <- (1:length(danNoc))[danNoc == TRUE & !is.na(danNoc)]
		obdrzi <- danNoc
		## izberi nocne ure, ki jih obdrzis v ucni mnozici
		if(length(noc) >= stObd & length(dan) >= stObd){
			obdrzi[] <- FALSE
			izberiNoc <- sample(noc, stObd)
			izberiDan <- sample(dan, stObd)
			obdrzi[izberiNoc] <- TRUE
			obdrzi[izberiDan] <- TRUE
		}
		obdrzi[is.na(danNoc)] = TRUE
		obdrziOdstrani <- rbind(obdrziOdstrani, obdrzi)
	}
	
	realDan[!obdrziOdstrani] <- NA
	return(realDan)				
}

