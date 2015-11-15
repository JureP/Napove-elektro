## RF model za napovedovanje hidroelektrarne
library(xts)
library(caret)
library(randomForest)
library(TTR)

## okolje kjer so shranjene feature matrike z vsemi featurji
OkoljeFM_k <- 'C:/Users/Podlogar/Documents/Projekt Elektro/00_Podatki/featureMatrix_voda/Train'

## okolje kjer so realizacije (train)
OkoljeReal <- 'C:/Users/Podlogar/Documents/Projekt Elektro/00_Podatki/realizacija/Train'  

## okolje kjer je shranjen feature selection za vsak model
OkoljeFeatureSelection <- 'C:/Users/Podlogar/Documents/Projekt Elektro/00_Podatki/featureMatrix_voda/Selected'

## okolje kamor naj se shranijo modlei ki uporablja celotno FM
OkoljeCelFM <- 'C:/Users/Podlogar/Documents/Projekt Elektro/03_Ucenje modela/Hidro/RandomForest/Modeli/celFM'
## okolje kamor naj se shranijo modlei ki uporablja FM brez koreliranih featurjev
OkoljeCorFM <- 'C:/Users/Podlogar/Documents/Projekt Elektro/03_Ucenje modela/Hidro/RandomForest/Modeli/corFM'
## okolje kamor naj se shranijo modlei ki uporablja featurje iz featur selectiona
OkoljeFS <- 'C:/Users/Podlogar/Documents/Projekt Elektro/03_Ucenje modela/Hidro/RandomForest/Modeli/featureSelectedFM'



## okolje kamor naj se shrani napovedi na validacijski mnozici (celFM)
OkoljeValid_CelFM <- 'C:/Users/Podlogar/Documents/Projekt Elektro/03_Ucenje modela/Hidro/RandomForest/Validacija/celFM'
## okolje kamor naj se shrani napovedi na validacijski mnozici (corFM)
OkoljeValid_CorFM <- 'C:/Users/Podlogar/Documents/Projekt Elektro/03_Ucenje modela/Hidro/RandomForest/Validacija/corFM'
## okolje kamor naj se shrani napovedi na validacijski mnozici (FS)
OkoljeValid_FS <- 'C:/Users/Podlogar/Documents/Projekt Elektro/03_Ucenje modela/Hidro/RandomForest/Validacija/featureSelectedFM'



stDreves = 500
setwd(OkoljeReal)
for(elekt in rev(dir())){
	setwd(OkoljeCelFM)
	if (gsub(".*T]|_.*", "", elekt) == 'Voda' &
	!(substr(elekt, 5,8) %in% substr(dir(),5,8))){
		print(elekt)
		setwd(OkoljeReal)
		realizacija <- readRDS(elekt)
		setwd(OkoljeFM_k)
		imeFM <- dir()[substr(elekt,5,8) == substr(dir(),5,8)]
		featureMatrix <- readRDS(imeFM)
		## razni modeli:
		## uporaba celotne feature matrike
			A <- cbind(featureMatrix, realizacija)
			A <- na.omit(A)
			model <- randomForest(as.matrix(A[, colnames(A) != 'realizacija']),
									as.vector(A$realizacija),
									ntrees = stDreves,
									do.trace = TRUE)
			## shranjevanje modela
			imeSave <- paste0(substr(elekt, 1,8), '_RF_celaFM.rds')
			## okolje kamor shrani
			setwd(OkoljeCelFM)
			saveRDS(model, imeSave)
			rm(model)
			gc()
		## uporaba nekorelirane feature matrike
			CRm <- cor(na.omit(featureMatrix))
			hc <- findCorrelation(CRm, cutoff = 0.75)
			A <- cbind(featureMatrix[, -hc], realizacija)
			A <- na.omit(A)			
			model <- randomForest(as.matrix(A[, colnames(A) != 'realizacija']),
									as.vector(A$realizacija),
									ntrees = stDreves,
									do.trace = TRUE)
			## shranjevanje modela
			imeSave <- paste0(substr(elekt, 1,8), '_RF_corFM.rds')
			## okolje kamor shrani
			setwd(OkoljeCorFM)
			saveRDS(model, imeSave)
			rm(model)
			gc()
		## uporaba feature selected feature matrike
			## loadanje feature slectiona
			setwd(OkoljeFeatureSelection)
			imeFS <- dir()[substr(elekt,1,8) == substr(dir(),1,8)]
			izbor <- readRDS(imeFS)
			izbor <- izbor$optVariables
			A <- cbind(featureMatrix[, izbor], realizacija)
			A <- na.omit(A)			
			model <- randomForest(as.matrix(A[, colnames(A) != 'realizacija']),
									as.vector(A$realizacija),
									ntrees = stDreves,
									do.trace = TRUE)
			## shranjevanje modela
			imeSave <- paste0(substr(elekt, 1,8), '_RF_fsFM.rds')
			## okolje kamor shrani
			setwd(OkoljeFS)
			saveRDS(model, imeSave)
			rm(model)
			gc()
		
	}
	setwd(OkoljeReal)
}



