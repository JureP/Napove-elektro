## ucenje gbm modela z uporabo tunanja (SAMO SONCNE ELEKTRARNE)
## odstrani obdobja ko elektrarna ne deluje
## iz nocnih ur vzame samo nekaj nakljucnih ur na dan
library(gbm)
library(caret)
library(xts)
library(plyr)

## okolje kjer so funckije 
OkoljeFunkcije <- 'C:/Users/Podlogar/Documents/Projekt Elektro/Funkcije'

## okolje kjer so feature matrike (train)
OkoljeFM <- 'C:/Users/Podlogar/Documents/Projekt Elektro/00_Podatki/featureMatrix/Train'
## okolje kjer so feature matrike (valid)
OkoljeFM_valid <- 'C:/Users/Podlogar/Documents/Projekt Elektro/00_Podatki/featureMatrix/Valid'
## okolje kjer so realizacije (train)
OkoljeReal <- 'C:/Users/Podlogar/Documents/Projekt Elektro/00_Podatki/realizacija/Train'  
## okolje kjer so realizacije (valid)
OkoljeReal_valid <- 'C:/Users/Podlogar/Documents/Projekt Elektro/00_Podatki/realizacija/Valid'  

## okolje kjer se ustvari mapa v katero se shranijo modeli
OkoljeModel <- 'C:/Users/Podlogar/Documents/Projekt Elektro/03_Ucenje modela/Sonce/GBM/Model'
## okolje kjer se ustvari mapa v katero se shranijo rezultati na validacijski mnozici
OkoljeValidacija <- 'C:/Users/Podlogar/Documents/Projekt Elektro/03_Ucenje modela/Sonce/GBM/Validacija'


setwd(OkoljeFunkcije)
for(fun in dir()){
	print(fun)
	source(fun)
}


## ustvari mapo datum in nacin za shranjevanje modela
Okolje1 <- paste0(OkoljeModel, '/', Sys.Date(), '_brezNEDELA')
dir.create(Okolje1, showWarnings = TRUE, recursive = FALSE, mode = "0777")
## ustvari mapo z datuom in nacinom za shranjevanje validacije
Okolje2 <- paste0(OkoljeValidacija, '/', Sys.Date(), '_brezNEDELA')
dir.create(Okolje2, showWarnings = TRUE, recursive = FALSE, mode = "0777")

Okolje1 <- paste0(OkoljeModel, '/2015-11-03_brezNEDELA')
Okolje2 <- paste0(OkoljeValidacija, '/2015-11-03_brezNEDELA')

setwd(OkoljeFM)
for(kraj in dir()[5]){
	print(kraj)
	## nalozi vremensko napvoed za kraj
	if(substr(kraj, 1, 5) == 'vreme'){
		## ucna matrika
		featureMatrix <- readRDS(kraj)
		## validacijska matrika
		setwd(OkoljeFM_valid)
		krajValid <- paste0(substr(kraj, 1, nchar(kraj)-9), 'Valid.rds')
		featureMatrixValid <- readRDS(krajValid)
		setwd(OkoljeReal)
		## za vse elektrarne v blizini kraja kraj uporabi vremensko napvoed
		for(elekt in dir()){
			if (gsub(".*vreme_|_FM.*", "", kraj)  == gsub(".*K]|_.*", "", elekt) &
			gsub(".*T]|_.*", "", elekt) == 'Sonce' &
			as.numeric(substr(elekt, 5,8)) == 2239){
				print(elekt)
				realizacija <- readRDS(elekt)
				## ciscenje pdoatkov
				## odstrani obdobje ko elektrarna ne dela
				realizacija <- nedelovanje(realizacija, 1)
				## odstranitev dela noci
				realizacija <- odstNoc(realizacija, 2)
				## odstranjevanje NA-jev
				X <- as.matrix(featureMatrix)
				Y <- as.vector(realizacija)
				A <- cbind(X,Y)
				A <- na.omit(A)
				Y <- A[,10]
				X <- A[,1:9]
				## tunanje in ucenje modela
				fitControl <- trainControl(method = "cv", number = 3)
				gbmGrid <-  expand.grid(interaction.depth = c(2,3,4),
										n.trees = (5:20)*500,
										shrinkage = c(0.1, 0.01, 0.001),
										n.minobsinnode = c(10,20))
				op <- train(X, Y, method = "gbm", trControl = fitControl, tuneGrid = gbmGrid, verbose = FALSE)
				model <- op$finalModel
				## validacija modela
				napovedGBM <- predict(model, featureMatrixValid, n.trees = stDreves)
				napovedGBM <- xts(napovedGBM, index(featureMatrixValid))
				## shranjevanje modela
				setwd(Okolje1)
				## ime modela
				imeModel <- paste0(substr(elekt, 1, 8), '_model_Tune_GBM_cistTune.rds')
				saveRDS(op, imeModel)
				## shranjevanje validacije
				setwd(Okolje2)
				## ime validacije
				imeValid <- paste0(substr(elekt, 1, 8), '_validacija_Tune_GBM_cist.rds')
				saveRDS(napovedGBM, imeValid)
				setwd(OkoljeReal)
			}
		}
	}
	setwd(OkoljeFM)
}

