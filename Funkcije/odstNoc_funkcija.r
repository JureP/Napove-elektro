## funkcija ki odstrani obdobja ko elektrarna ne deluje 

odstNoc <- function(realizacij, ## realizacija proizvodnje (xts)
					stObd = 2 ## stevilo ur na noc, ki jih obdrzi
						)
				## funkcija ki iz podatkov odstrani obdobje, 
				## noci, razen nakljucno izbranih (stObd na dan)
				## vrne xts z NA namesto 0
						
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
		obdrzi <- proizvaja[poglej]
		obdrzi[is.na(obdrzi)] = TRUE
		noc <- (1:length(obdrzi))[obdrzi == FALSE]
		## izberi nocne ure, ki jih obdrzis v ucni mnozici
		if(length(noc) >= stObd){
			izberiNoc <- sample(noc, stObd)
			obdrzi[izberiNoc] <- TRUE
		}
		obdrziOdstrani <- rbind(obdrziOdstrani, obdrzi)
	}
	
	realDan[!obdrziOdstrani] <- NA
	return(realDan)				
}