## napoved s pomocjo osnovnih napovedi kot featurji in random forest.
library(xts)
library(randomForest)
library(caret)

## okolje v katerem so mape z napovedmi na validacijski mnozici
OkoljeValid <- 'C:/Users/Podlogar/Documents/Projekt Elektro/03_Ucenje modela/Sonce/VseNapovedi_valid'

## okolje kjer so realizacije (valid)
OkoljeReal_valid <- 'C:/Users/Podlogar/Documents/Projekt Elektro/00_Podatki/realizacija/Valid'  

## okolje kamor naj se ensemble randomForest model shrani
OkoljeEnsRF <- 'C:/Users/Podlogar/Documents/Projekt Elektro/07Ensemble/Izbor_RF_zNekaj0'



## okolje kjer so funckije 
OkoljeFunkcije <- 'C:/Users/Podlogar/Documents/Projekt Elektro/Funkcije'

setwd(OkoljeFunkcije)
source('odstNoc_funkcija.r')
source('nedelovanje_funkcija.r')

setwd(OkoljeReal_valid)
for(elekt in dir()){
	realizacija <- readRDS(elekt)
	if (gsub(".*T]|_.*", "", elekt) == 'Sonce'){
		print(elekt)
		## sestavljanje feature matrike (brez ur ko je realizacija 0)
		setwd(OkoljeValid)
		stModelov <- length(dir())
		## odstrani obdobje ko elektrarna ne dela
		realizacija <- nedelovanje(realizacija, 1)
		## odstranitev dela noci
		realizacija <- odstNoc(realizacija, 2)
		matrikaNapovedi <- data.frame(matrix(NA, length(na.omit(realizacija)), stModelov))
		stevec <- 1
		for(mdl in dir()){
			## loadanje napovedi modela mdl
			Okolje <- paste0(OkoljeValid,'/',mdl)
			setwd(Okolje)
			imeNapoved <- dir()[substr(dir(),4,8) == substr(elekt, 4,8)]
			napoved <- readRDS(imeNapoved)
			## sharnjevanje v matrikaNapovedi (z selekciniranimi 0 pri realizaciji)
			matrikaNapovedi[, stevec] <- napoved[!is.na(realizacija)]
			names(matrikaNapovedi)[stevec] <- mdl
			stevec <- stevec + 1
		}
		model <- randomForest(matrikaNapovedi, realizacija[!is.na(realizacija)], ntree = 500, do.trace = TRUE)
		setwd(OkoljeEnsRF)
		imeSave <- paste0(substr(elekt, 1, 8), '_EsnembleRF.rds' )
		saveRDS(model, imeSave)
	}
	setwd(OkoljeReal_valid)
}

