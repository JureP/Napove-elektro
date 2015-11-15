#### napovedi ensemblov na validacijski mnozici
library(xts)
library(randomForest)
library(gbm)
library(e1071)

## okolje v kateri so mape z vsemi modeli 
OkoljeModel <- 'C:/Users/Podlogar/Documents/Projekt Elektro/03_Ucenje modela/Sonce/VsiModeli'
## okolje kjer so shranjene realizacije (valid)
OkoljeRealizacija <- 'C:/Users/Podlogar/Documents/Projekt Elektro/00_Podatki/realizacija/Valid'
## okolje kjer so feature matrike valid dela mnozice
OkoljeFM <- 'C:/Users/Podlogar/Documents/Projekt Elektro/00_Podatki/featureMatrix/Valid'
## okolje kjer so shranjeni delezi v ensemblu
OkoljeEnsemble <- 'C:/Users/Podlogar/Documents/Projekt Elektro/07Ensemble/Izbor'
## okolje kjer so shranjeni ensembleRF modeli (ucenje z nekaj 0)
OkoljeEnsembleRF_zNekaj0 <- 'C:/Users/Podlogar/Documents/Projekt Elektro/07Ensemble/Izbor_RF_zNekaj0'
## okolje kamor naj se shranijo mape (pozameznih modlev) z napovedmi
OkoljeTest <- 'C:/Users/Podlogar/Documents/Projekt Elektro/03_Ucenje modela/Sonce/VseNapovedi_valid'

## Okolje kjer so funkcije
OkoljeFunkcije <- 'C:/Users/Podlogar/Documents/Projekt Elektro/Funkcije'

setwd(OkoljeFunkcije)
source("napovej_funkcija.r")

## napoved cez vse elektrarne
setwd(OkoljeRealizacija)
for(elekt in dir()){
	realizacija <- readRDS(elekt)
	if (gsub(".*T]|_.*", "", elekt) == 'Sonce'){
		print(elekt)
		## loadanje delezov modelov v ensemblu
		setwd(OkoljeEnsemble)
		ensembleIme <- dir()[substr(elekt,5,8) == substr(dir(),5,8)]
		ensemble <- readRDS(ensembleIme)
		## izracun utezenega povprecja napovedi
		napovedi <- rep(0, length(realizacija))
		osnovneNapovedi <- NULL
		setwd(OkoljeModel)
		for(mdl in dir()){
			## loadanje 
			OkoljeSave <- paste0(OkoljeTest, '/', mdl)
			setwd(OkoljeSave)
			imeNapoved <- dir()[substr(elekt,5,8) == substr(dir(),5,8)]
			osnovneNapovedi <- cbind(osnovneNapovedi ,readRDS(imeNapoved))
			names(osnovneNapovedi)[ncol(osnovneNapovedi)] <- mdl
		}
		napoved <- rowSums(t(t(osnovneNapovedi)*(ensemble/sum(ensemble))))
		napoved <- xts(napoved, index(osnovneNapovedi))
		# plot(napoved, ylim = c(-2, as.numeric(gsub(".*M]|_.*", "", elekt))) ,main = paste('ensemble', substr(elekt, 5,8)))
		# lines(realizacija, col = 'red', lty = 3)
		#lines(osnovneNapovedi[,2], col = 'green')
		# line <- readline()
		## shranjevanje napovedi 
		OkoljeSave <- paste0(OkoljeTest, '/Ensemble')
			## ustvari mapo 
		dir.create(OkoljeSave, showWarnings = TRUE, recursive = FALSE, mode = "0777")
		setwd(OkoljeSave)
		imeSave <- paste0(substr(elekt,1,8),'_Ensemble_napoved_valid.rds')
		saveRDS(napoved, imeSave)
	}
	setwd(OkoljeRealizacija)
}

################################################################################
################################################################################
## ensembleRF z nekaj 0 (pri ucenju ne odstrani vseh 0 pri realizaciji)


## napoved cez vse elektrarne
setwd(OkoljeRealizacija)
for(elekt in dir()){
	realizacija <- readRDS(elekt)
	if (gsub(".*T]|_.*", "", elekt) == 'Sonce'){
		print(elekt)
		## loadanje RF modela za sestavo ensembla
		setwd(OkoljeEnsembleRF_zNekaj0)
		ensembleIme <- dir()[substr(elekt,5,8) == substr(dir(),5,8)]
		ensembleModel <- readRDS(ensembleIme)
		## izracun utezenega povprecja napovedi
		osnovneNapovedi <- NULL
		setwd(OkoljeModel)
		for(mdl in dir()){
			## loadanje 
			OkoljeSave <- paste0(OkoljeTest, '/', mdl)
			setwd(OkoljeSave)
			imeNapoved <- dir()[substr(elekt,5,8) == substr(dir(),5,8)]
			osnovneNapovedi <- cbind(osnovneNapovedi ,readRDS(imeNapoved))
			names(osnovneNapovedi)[ncol(osnovneNapovedi)] <- mdl
		}
		napoved <- predict(ensembleModel, osnovneNapovedi)
		napoved <- xts(napoved, index(osnovneNapovedi))
			# plot(napoved, ylim = c(-2, as.numeric(gsub(".*M]|_.*", "", elekt))) ,main = paste('ensemble', substr(elekt, 5,8)))
			# lines(realizacija, col = 'red', lty = 3)
			# print('press [Enter]')
			# line <- readline()
		## shranjevanje napovedi 
		OkoljeSave <- paste0(OkoljeTest, '/Ensemble_RF_zNekaj0')
			## ustvari mapo 
		dir.create(OkoljeSave, showWarnings = TRUE, recursive = FALSE, mode = "0777")
		setwd(OkoljeSave)
		imeSave <- paste0(substr(elekt,1,8),'_EnsembleRF_zNekaj0_napoved_valid.rds')
		saveRDS(napoved, imeSave)
	}
	setwd(OkoljeRealizacija)
}