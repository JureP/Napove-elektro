## svm model
library(e1071)
library(xts)

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
Okolje1 <- 'C:/Users/Podlogar/Documents/Projekt Elektro/03_Ucenje modela/Sonce/VkljuciValid/Model/SVM_vkljuciValid'



setwd(OkoljeFunkcije)
for(fun in dir()){
	print(fun)
	source(fun)
}




cost = 8
epsilon = 0.1
setwd(OkoljeFM)
for(kraj in dir()){
	print(kraj)
	## nalozi vremensko napvoed za kraj
	if(substr(kraj, 1, 5) == 'vreme'){
		## ucna matrika
		featureMatrix <- readRDS(kraj)
		## validacijska matrika
		setwd(OkoljeFM_valid)
		krajValid <- paste0(substr(kraj, 1, nchar(kraj)-9), 'Valid.rds')
		featureMatrixValid <- readRDS(krajValid)
		## zdruzevanje matrik
		featureMatrix <- rbind(featureMatrix, featureMatrixValid)
		setwd(OkoljeReal)
		## za vse elektrarne v blizini kraja kraj uporabi vremensko napvoed
		for(elekt in dir()){
			if (gsub(".*vreme_|_FM.*", "", kraj)  == gsub(".*K]|_.*", "", elekt) &
			gsub(".*T]|_.*", "", elekt) == 'Sonce'){
				print(elekt)
				realizacija <- readRDS(elekt)
				setwd(OkoljeReal_valid)
				imeValid <- dir()[substr(dir(),1,8) == substr(elekt,1,8)]
				realizacijaValid <- readRDS(imeValid)
				realizacija <- c(realizacija, realizacijaValid)
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
				## ucenje modela
				model <- svm(A[,1:(ncol(A)-1)],	A[, ncol(A)], epsilon = epsilon, cost = cost) ## glej clanek
				## validacija modela
				newdata <- data.frame(X = I(featureMatrixValid))
				napovedSVM <- predict(model, newdata)
				napovedSVM <- xts(napovedSVM, index(featureMatrixValid))
				## shranjevanje modela
				setwd(Okolje1)
				## ime modela
				imeModel <- paste0(substr(elekt, 1, 8), '_model_', epsilon, '_', cost,'_SVM_vkljuciValid.rds')
				saveRDS(model, imeModel)
			}
		}
	}
	setwd(OkoljeFM)
}


