## napovedi modelov na validacijski mnozici (ENSEMBLE VODA)
library(xts)
library(randomForest)
library(gbm)
library(e1071)

## okolje v kateri so mape z vsemi modeli 
OkoljeModel <- 'C:/Users/Podlogar/Documents/Projekt Elektro/03_Ucenje modela/Hidro/VsiModeli'
## okolje kjer so shranjene realizacije (test)
OkoljeRealizacija <- 'C:/Users/Podlogar/Documents/Projekt Elektro/00_Podatki/realizacija/Valid'
## okolje kjer so shranjeni delezi v ensemblu
OkoljeEnsemble <- 'C:/Users/Podlogar/Documents/Projekt Elektro/07Ensemble(voda)/Izbor'
## okolje kamor naj se shranijo mape (pozameznih modlev) z napovedmi
OkoljeTest <- 'C:/Users/Podlogar/Documents/Projekt Elektro/03_Ucenje modela/Hidro/vseValidacije'

## Okolje kjer so funkcije
OkoljeFunkcije <- 'C:/Users/Podlogar/Documents/Projekt Elektro/Funkcije'

setwd(OkoljeFunkcije)
source("napovej_funkcija.r")



##############################
## ENSEMBLE ##################


## napoved cez vse elektrarne
setwd(OkoljeRealizacija)
for(elekt in dir()){
	realizacija <- readRDS(elekt)
	if (gsub(".*T]|_.*", "", elekt) == 'Voda'){
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
		imeSave <- paste0(substr(elekt,1,8),'_Ensemble_napovedValid_voda.rds')
		saveRDS(napoved, imeSave)
	}
	setwd(OkoljeRealizacija)
}
