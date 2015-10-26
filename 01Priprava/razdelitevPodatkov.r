## pretvorba csv-jev v rds
library(xts)

## KEJR JE SHRANJENO
## okolje kjer je shranjena realizacija v RDS
OkoljeRealizacijaRDS <- 'C:/Users/Podlogar/Documents/Projekt Elektro/00_Podatki/PodatkiRDS/realizacija'
## okolje kjer so shranjene napovedi vremenskih parametrov v RDS
OkoljeFM <- 'C:/Users/Podlogar/Documents/Projekt Elektro/00_Podatki/PodatkiRDS/featureMatrix'

## KAMOR SE SHRANI
## okolje makor se shrani train set realizacije
OkoljeYtrain <- 'C:/Users/Podlogar/Documents/Projekt Elektro/00_Podatki/realizacija/Train'  
## okolje kamor se shranijo test set realizacije
OkoljeYtest <- 'C:/Users/Podlogar/Documents/Projekt Elektro/00_Podatki/realizacija/Test'
## okolje kamor se shrani train set feature matrike
OkoljeXtrain <- 'C:/Users/Podlogar/Documents/Projekt Elektro/00_Podatki/featureMatrix/Train'
## okolje kamor se shrani test set feature matrike
OkoljeXtest <- 'C:/Users/Podlogar/Documents/Projekt Elektro/00_Podatki/featureMatrix/Test'

## okolje kjer so funkcije
OkoljeFunkcije <- 'C:/Users/Podlogar/Documents/Projekt Elektro/Funkcije'

setwd(OkoljeFunkcije)
for(funkcija in dir()){
	print(funkcija)
	source(funkcija)
}

## razdelitev relizacije na train in test set
setwd(OkoljeRealizacijaRDS)
for(elekt in dir()){
	if(substr(elekt, 1, 4) == '[id]'){
		realizacija <- readRDS(elekt)
		razdeljen <- razdelitevRealizacije(realizacija)
		imeElekt <- substr(elekt, 1,nchar(elekt) -4)
		koncnica <- substr(elekt, nchar(elekt)-3,nchar(elekt))
		imeTrain <-paste0(imeElekt, '_Train' ,koncnica)
		imeTest <- paste0(imeElekt, '_Test' ,koncnica)
		setwd(OkoljeYtrain)
		saveRDS(razdeljen$yTrain, imeTrain)
		setwd(OkoljeYtest)
		saveRDS(razdeljen$yTest, imeTest)
		setwd(OkoljeRealizacijaRDS)
	}
}

## razdelitev relizacije na train in test set
setwd(OkoljeFM)
for(kraj in dir()){
	if(substr(kraj, nchar(kraj)-5, nchar(kraj)-4) == 'FM'){
		featureMatrix <- readRDS(kraj)
		razdeljen <- razdelitevNapovedi(featureMatrix)
		imeKraj <- substr(kraj, 1,nchar(kraj) -4)
		koncnica <- substr(kraj, nchar(kraj)-3,nchar(kraj))
		imeTrain <-paste0(imeKraj, '_Train' ,koncnica)
		imeTest <- paste0(imeKraj, '_Test' ,koncnica)
		setwd(OkoljeXtrain)
		saveRDS(razdeljen$xTrain, imeTrain)
		setwd(OkoljeXtest)
		saveRDS(razdeljen$xTest, imeTest)
		setwd(OkoljeFM)
	}
}



