## ucenje linearnega modela (SAMO SONCNE ELEKTRARNE)
## odstrani obdobja ko elektrarna ne deluje
## iz nocnih ur vzame samo nekaj nakljucnih ur na dan
library(gbm)
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
OkoljeModel <- 'C:/Users/Podlogar/Documents/Projekt Elektro/03_Ucenje modela/Sonce/OLS/Model'
## okolje kjer se ustvari mapa v katero se shranijo rezultati na validacijski mnozici
OkoljeValidacija <- 'C:/Users/Podlogar/Documents/Projekt Elektro/03_Ucenje modela/Sonce/OLS/Validacija'


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
		setwd(OkoljeReal)
		## za vse elektrarne v blizini kraja kraj uporabi vremensko napvoed
		for(elekt in dir()){
			if (gsub(".*vreme_|_FM.*", "", kraj)  == gsub(".*K]|_.*", "", elekt) &
			gsub(".*T]|_.*", "", elekt) == 'Sonce'){
				print(elekt)
				realizacija <- readRDS(elekt)
				## ciscenje pdoatkov
				## odstrani obdobje ko elektrarna ne dela
				realizacija <- nedelovanje(realizacija, stDni = 1)
				## odstranitev dela noci
				realizacija <- odstNoc(realizacija, 2)
				## odstranjevanje NA-jev
				X <- as.matrix(featureMatrix)
				Y <- as.vector(realizacija)
				A <- cbind(X,Y)
				A <- na.omit(A)
				## ucenje modela
				X <- A[,1:(ncol(A)-1)]
				model <- lm(A[, ncol(A)] ~ X)
				## validacija modela
				newdata <- data.frame(X = I(featureMatrixValid))
				napovedOLS <- predict(model, newdata)
				napovedOLS <- xts(napovedOLS, index(featureMatrixValid))
				## shranjevanje modela
				setwd(Okolje1)
				## ime modela
				imeModel <- paste0(substr(elekt, 1, 8), '_model_OLS_cist.rds')
				saveRDS(model, imeModel)
				## shranjevanje validacije
				setwd(Okolje2)
				## ime validacije
				imeValid <- paste0(substr(elekt, 1, 8), '_valid_OLS_cist.rds')
				saveRDS(napovedOLS, imeValid)
				setwd(OkoljeReal)
			}
		}
	}
	setwd(OkoljeFM)
}

