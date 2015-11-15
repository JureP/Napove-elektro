## GBM model za napovedovanje hidroelektrarne
library(xts)
library(caret)
library(gbm)
library(TTR)


## okolje kjer so shranjene feature matrike z vsemi featurji
OkoljeFM_k <- 'C:/Users/Podlogar/Documents/Projekt Elektro/00_Podatki/featureMatrix_voda/Train'

## okolje kjer so realizacije (train)
OkoljeReal <- 'C:/Users/Podlogar/Documents/Projekt Elektro/00_Podatki/realizacija/Train'  

## okolje kjer je shranjen feature selection za vsak model
OkoljeFeatureSelection <- 'C:/Users/Podlogar/Documents/Projekt Elektro/00_Podatki/featureMatrix_voda/Selected'

## okolje kamor naj se shranijo modlei ki uporablja featurje iz featur selectiona
OkoljeFS <- 'C:/Users/Podlogar/Documents/Projekt Elektro/03_Ucenje modela/Hidro/GBM/Modeli/featureSelectedFM_tune'


stDreves = 5000
setwd(OkoljeReal)
for(elekt in rev(dir())){
	if (gsub(".*T]|_.*", "", elekt) == 'Voda'){
		print(elekt)
		realizacija <- readRDS(elekt)
		setwd(OkoljeFM_k)
		imeFM <- dir()[substr(elekt,5,8) == substr(dir(),5,8)]
		featureMatrix <- readRDS(imeFM)
		## uporaba feature selected feature matrike
			## loadanje feature slectiona
			setwd(OkoljeFeatureSelection)
			imeFS <- dir()[substr(elekt,1,8) == substr(dir(),1,8)]
			izbor <- readRDS(imeFS)
			izbor <- izbor$optVariables
			A <- cbind(featureMatrix[, izbor], realizacija)
			A <- na.omit(A)
			## tunanje in ucenje modela
			fitControl <- trainControl(method = "cv", number = 3)
			gbmGrid <-  expand.grid(interaction.depth = c(2,4),
									n.trees = c(5,10,15,20)*500,
									shrinkage = c(0.01, 0.001),
									n.minobsinnode = c(10,20))
			op <- train(as.matrix(A[, colnames(A) != 'realizacija']),
							as.vector(A$realizacija),
							method = "gbm",
							trControl = fitControl,
							tuneGrid = gbmGrid,
							verbose = TRUE)
			model <- op$finalModel			
			## shranjevanje modela
			imeSave <- paste0(substr(elekt, 1,8), '_GBM_fsFM_TUNE.rds')
			## okolje kamor shrani
			setwd(OkoljeFS)
			saveRDS(op, imeSave)
		
	}
	setwd(OkoljeReal)
}



