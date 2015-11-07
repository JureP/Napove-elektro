razdelitevNapovedi <- function(
	## vrne seznam razdeljenih napovedi vremenskeih parametrov (oziroma feature matriko).
	## podatki so razdeljeni na train in test del (naceloma 2 
	## 2 leti za train, 1 leto za test)
	vremenskaNapoved, ## xts matrika napovedi vremenskih parametrov
	zacetek = as.POSIXct('2012-01-01 00:00:00',format='%Y-%m-%d %H:%M:%S'), ## zacetek train mnozice
	konecTrain = as.POSIXct('2014-01-01 00:00:00',format='%Y-%m-%d %H:%M:%S'), ## kone train mnozice
	konecValid = as.POSIXct('2014-06-01 00:00:00',format='%Y-%m-%d %H:%M:%S'), ## konec valid mnozice
	konecTest = as.POSIXct('2014-12-31 23:00:00',format='%Y-%m-%d %H:%M:%S') ## konec testne mnozice
	){
	
		xTrain <- vremenskaNapoved[index(vremenskaNapoved) >= zacetek 
						& index(vremenskaNapoved)  < konecTrain, ]
		xValid <- vremenskaNapoved[index(vremenskaNapoved) >= konecTrain 
						& index(vremenskaNapoved)  < konecValid, ]
		xTest <- vremenskaNapoved[index(vremenskaNapoved) >= konecValid 
						& index(vremenskaNapoved)  <= konecTest, ]
	
		loceni <- list('xTrain' = xTrain, 'xValid' = xValid, 'xTest' = xTest)
		return(loceni)
	}
	
	
razdelitevRealizacije <- function(
	## vrne seznam razdeljenih realizacij proizvodnje.
	## podatki so razdeljeni na train in test del (naceloma 2 
	## 2 leti za train, 1 leto za test)
	realizacija, ## xts matrika realizacije proizvodnje
	zacetek = as.POSIXct('2012-01-01 00:00:00',format='%Y-%m-%d %H:%M:%S'), ## zacetek train mnozice
	konecTrain = as.POSIXct('2014-01-01 00:00:00',format='%Y-%m-%d %H:%M:%S'), ## kone train mnozice
	konecValid = as.POSIXct('2014-06-01 00:00:00',format='%Y-%m-%d %H:%M:%S'), ## konec valid mnozice
	konecTest = as.POSIXct('2014-12-31 23:00:00',format='%Y-%m-%d %H:%M:%S') ## konec testne mnozice
	){
	
		yTrain <- realizacija[index(realizacija) >= zacetek 
						& index(realizacija)  < konecTrain, ]
		yValid <- realizacija[index(realizacija) >= konecTrain 
						& index(realizacija)  < konecValid, ]
		yTest <- realizacija[index(realizacija) >= konecValid 
						& index(realizacija)  <= konecTest, ]
		loceni <- list('yTrain' = yTrain, 'yValid' = yValid, 'yTest' = yTest)
		return(loceni)
	}
