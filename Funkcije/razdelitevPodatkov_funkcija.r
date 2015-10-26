razdelitevNapovedi <- function(
	## vrne seznam razdeljenih napovedi vremenskeih parametrov.
	## podatki so razdeljeni na train in test del (naceloma 2 
	## 2 leti za train, 1 leto za test)
	vremenskaNapoved, ## xts matrika napovedi vremenskih parametrov
	zacetek = as.POSIXct('2012-01-01 00:00:00',format='%Y-%m-%d %H:%M:%S'), ## zacetek train mnozice
	konecTrain = as.POSIXct('2014-01-01 00:00:00',format='%Y-%m-%d %H:%M:%S'), ## kone train mnozice
	konecTest = as.POSIXct('2014-12-31 23:00:00',format='%Y-%m-%d %H:%M:%S') ## konec testne mnozice
	){
	
	xTrain <- vremenskaNapoved[index(vremenskaNapoved) >= zacetek 
						& index(vremenskaNapoved)  < konecTrain, ]
	xTest <- vremenskaNapoved[index(vremenskaNapoved) >= konecTrain 
						& index(vremenskaNapoved)  <= konecTest, ]
	
	loceni <- list('xTrain' = xTrain, 'xTest' = xTest)
	return(loceni)
	}
	
	
razdelitevRealizacije <- function(
	## vrne seznam razdeljenih realizacij proizvodnje.
	## podatki so razdeljeni na train in test del (naceloma 2 
	## 2 leti za train, 1 leto za test)
	realizacija, ## xts matrika realizacije proizvodnje
	zacetek = as.POSIXct('2012-01-01 00:00:00',format='%Y-%m-%d %H:%M:%S'), ## zacetek train mnozice
	konecTrain = as.POSIXct('2014-01-01 00:00:00',format='%Y-%m-%d %H:%M:%S'), ## kone train mnozice
	konecTest = as.POSIXct('2014-12-31 23:00:00',format='%Y-%m-%d %H:%M:%S') ## konec testne mnozice
	){
	
	yTrain <- realizacija[index(realizacija) >= zacetek 
						& index(realizacija)  < konecTrain, ]
	yTest <- realizacija[index(realizacija) >= konecTrain 
						& index(realizacija)  <= konecTest, ]
	loceni <- list('yTrain' = yTrain, 'yTest' = yTest)
	return(loceni)
	}
