## feature selection
library(xts)
library(caret)
library(randomForest)

## okolje kjer so shranjene feature matrike z vsemi featurji
OkoljeFM_k <- 'C:/Users/Podlogar/Documents/Projekt Elektro/01_Priprava podatkov/FeatureMatrix_voda/FM'

## okolje kjer so realizacije (train)
OkoljeReal <- 'C:/Users/Podlogar/Documents/Projekt Elektro/00_Podatki/realizacija/Train'  

## okolje kamor shrani feature matrike po feature selectnu
OkoljeFeatureSelection <- 'C:/Users/Podlogar/Documents/Projekt Elektro/01_Priprava podatkov/FeatureMatrix_voda/Feature selection FM'


setwd(OkoljeReal)
for(elekt in dir()[gsub(".*T]|_.*", "", dir()) == 'Voda']){
	setwd(OkoljeFeatureSelection)
	if(!(substr(elekt,5,8) %in% substr(dir(),5,8))){
		setwd(OkoljeReal)
		print(elekt)
		## loadanje realizacije elektrarne
		realizacija <- readRDS(elekt)
		setwd(OkoljeFM_k)
		## loadanje feature matrike 
		imeFM <- dir()[substr(elekt,5,8) == substr(dir(),5,8)]
		featureMatrix <- readRDS(imeFM)
		fiksenDel <- featureMatrix[,1:9]
		precisti <- featureMatrix[,10:ncol(featureMatrix)]

		## odstranitev visoko koleriranih featurjev
		## primerjaj samo podatke ki so smiselni (ne odstranjuj prvega dela ...)
		CRm <- cor(na.omit(precisti))
		hc <- findCorrelation(CRm, cutoff = 0.75)

		featureMatrix <- merge(fiksenDel, precisti[, -hc])

		## odstranitev na 
		naRealizacija <- !(is.na(realizacija))
		naFM <- complete.cases(featureMatrix)
		naJi <- naRealizacija & naFM
		realizacija <- realizacija[naJi]
		featureMatrix <- featureMatrix[naJi,]
		## feature selection z caret
		X <- as.matrix(featureMatrix)
		Y <- as.vector(realizacija)
		
		start <- Sys.time()
		cont <- rfeControl(functions=rfFuncs, method="cv", number= 3, verbose = TRUE)
		rezultat <- rfe(X, Y,
						size = seq(20, 80,10),
						rfeControl=cont)
		stop <- Sys.time() 
		cas <- stop - start
		## shranjevanje rezulatov
		imeFS <- paste0(substr(elekt,1,8), '_FeatureMatrixSelected.rds')
		imeCas <- paste0(substr(elekt,1,8), '_Cas.rds')
		setwd(OkoljeFeatureSelection)
		saveRDS(rezultat, imeFS)
		saveRDS(cas, imeCas)
		setwd(OkoljeReal)	
	}
	setwd(OkoljeReal)	
}

setwd(OkoljeReal)
for(elekt in rev(dir())){
	print(elekt)
}

elekt <- dir()[1]
realizacija <- readRDS(elekt)

setwd(OkoljeFM_k)
ime <- dir()[1]

print(cas)

## rezultat
setwd(OkoljeFeatureSelection)				
i <- dir()[2]
fs <- readRDS(i)
print(fs)			

