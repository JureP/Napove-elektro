## ucenje linearnega modela (SAMO SONCNE ELEKTRARNE)
library(gbm)
library(xts)

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


## ustvari mapo datum in nacin za shranjevanje modela
Okolje1 <- paste0(OkoljeModel, '/', Sys.Date(), '_vsi')
dir.create(Okolje1, showWarnings = TRUE, recursive = FALSE, mode = "0777")
## ustvari mapo z datuom in nacinom za shranjevanje validacije
Okolje2 <- paste0(OkoljeValidacija, '/', Sys.Date(), '_vsi')
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
				X <- featureMatrix
				## ucenje modela
				model <- lm(realizacija ~ X)
				## validacija modela
				newdata <- data.frame(X = I(featureMatrixValid))
				napovedOLS <- predict(model, newdata)
				napovedOLS <- xts(napovedOLS, index(featureMatrixValid))
				## shranjevanje modela
				setwd(Okolje1)
				## ime modela
				imeModel <- paste0(substr(elekt, 1, 8), '_model_OLS.rds')
				saveRDS(model, imeModel)
				## shranjevanje validacije
				setwd(Okolje2)
				## ime validacije
				imeValid <- paste0(substr(elekt, 1, 8), '_valid_OLS.rds')
				saveRDS(napovedOLS, imeValid)
				setwd(OkoljeReal)
			}
		}
	}
	setwd(OkoljeFM)
}

