## napovedi modelov na validacijski mnozici (VODA)
library(xts)
library(randomForest)
library(gbm)
library(e1071)

## okolje v kateri so mape z vsemi modeli 
OkoljeModel <- 'C:/Users/Podlogar/Documents/Projekt Elektro/03_Ucenje modela/Hidro/VsiModeli'
## okolje kjer so shranjene realizacije (valid)
OkoljeRealizacija <- 'C:/Users/Podlogar/Documents/Projekt Elektro/00_Podatki/realizacija/Valid'
## okolje kjer so feature matrike validacijski dela mnozice
OkoljeFM <- 'C:/Users/Podlogar/Documents/Projekt Elektro/00_Podatki/featureMatrix_voda/Valid'
## okolje kamor naj se shranijo mape (pozameznih modlev) z napovedmi
OkoljeSaveD <- 'C:/Users/Podlogar/Documents/Projekt Elektro/03_Ucenje modela/Hidro/VseValidacije'


## okolje kjer je feature selection s pomocjo korelacije
OkoljeKor <- 'C:/Users/Podlogar/Documents/Projekt Elektro/00_Podatki/featureMatrix_voda/Selected_hc'
## okolje kjer je feature selection s pomocjo RFE
OkoljeRFE <- 'C:/Users/Podlogar/Documents/Projekt Elektro/00_Podatki/featureMatrix_voda/Selected'


## Okolje kjer so funkcije
OkoljeFunkcije <- 'C:/Users/Podlogar/Documents/Projekt Elektro/Funkcije'

setwd(OkoljeFunkcije)
source("napovej_funkcija.r")


## napoved cez vse elektrarne
setwd(OkoljeRealizacija)
for(elekt in dir()){
	realizacija <- readRDS(elekt)
	if (gsub(".*T]|_.*", "", elekt) == 'Voda'){
		print(elekt)
		## napoved vsakega modela
		setwd(OkoljeModel)
		for(mdl in dir()){
			print(mdl)
			## loadanje modela mdl za elektrarno elekt
			Okolje <- paste0(OkoljeModel, '/', mdl)
			setwd(Okolje)
			modelElekt <- dir()[substr(elekt,4,8) == substr(dir(),4,8)]
			model <- readRDS(modelElekt)
			## ladanje feature matrike za elektrarno elekt
			setwd(OkoljeFM)
			imeFM <- dir()[substr(elekt, 1, 8) == substr(dir(), 1, 8)]
			## izbira prave feature matrike
			if(gsub(".*T]|_.*", "", mdl) == 'celFM'){
				setwd(OkoljeFM)
				FM <- readRDS(imeFM)
			}
			if(gsub(".*T]|_.*", "", mdl) == 'corFM'){
				setwd(OkoljeFM)
				FM <- readRDS(imeFM)
				setwd(OkoljeKor)
				imeKor <- dir()[substr(elekt, 1,8) == substr(dir(),1,8)]
				hc <- readRDS(imeKor)
				FM <- FM[, -hc]
			}			
			if(gsub(".*T]|_.*", "", mdl) == 'featureSelectedFM'){
				setwd(OkoljeFM)
				FM <- readRDS(imeFM)
				setwd(OkoljeRFE)
				imeRFE <- dir()[substr(elekt, 1,8) == substr(dir(),1,8)]
				fsRFE <- readRDS(imeRFE)
				FM <- FM[, fsRFE$optVariables]
			}
			FM <- na.omit(FM)
			napoved <- napovej(model, as.matrix(FM), gsub("|_.*", "", mdl))
			napoved <- xts(napoved, index(FM))
			plot(napoved, main = paste(mdl, substr(elekt, 5,8)), ylim = c(0, min(max(na.omit(realizacija)))))
			lines(realizacija, col = 'red')
			# cat(paste0("Press [enter] to continue from", mdl))
			# line <- readline()
			## shranjevanje napovedi 
			OkoljeSave <- paste0(OkoljeSaveD, '/', mdl)
				## ustvari mapo 
			dir.create(OkoljeSave, showWarnings = TRUE, recursive = FALSE, mode = "0777")
			setwd(OkoljeSave)
			imeSave <- paste0(substr(elekt,1,8),'_', mdl, '_napovedValidacija.rds')
			saveRDS(napoved, imeSave)
		}
	}
	setwd(OkoljeRealizacija)
}
