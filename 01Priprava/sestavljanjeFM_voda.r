## sestavljanje feature matrike za hidroelektranre (dnevni podatki za dez in sneg na relavantih krajih)
library(xts)
library(TTR)



## okolje kjer so funckije 
OkoljeFunkcije <- 'C:/Users/Podlogar/Documents/Projekt Elektro/Funkcije'

setwd(OkoljeFunkcije)
source('razdelitevPodatkov_funkcija.r')

## okolje kjer so feature matrike (cela)
OkoljeFM <- 'C:/Users/Podlogar/Documents/Projekt Elektro/00_Podatki/PodatkiRDS/featureMatrix'
## okolje kjer so realizacije (cela)
OkoljeReal <- 'C:/Users/Podlogar/Documents/Projekt Elektro/00_Podatki/PodatkiRDS/realizacija'  


## okolje kjer je csv s podatki o pomembnih okoliskih krajih
OkoljeOkolica <- 'C:/Users/Podlogar/Documents/Projekt Elektro/01_Priprava podatkov/FeatureMatrix_voda'
## okolje kjer so podatki o dezju in snegu za pomembne okoliske kraje
OkoljePodatki <- 'C:/Users/Podlogar/Documents/Projekt Elektro/01_Priprava podatkov/FeatureMatrix_voda/DnevniVreme'

## okolje kamor naj se shranijo feature matrike z vsemi featurji
## train mnozica
OkoljeFMc_Train<- 'C:/Users/Podlogar/Documents/Projekt Elektro/00_Podatki/featureMatrix_voda/Train'
## validacijska mnozica
OkoljeFMc_Valid<- 'C:/Users/Podlogar/Documents/Projekt Elektro/00_Podatki/featureMatrix_voda/Valid'
## test mnozica
OkoljeFMc_Test<- 'C:/Users/Podlogar/Documents/Projekt Elektro/00_Podatki/featureMatrix_voda/Test'


## podatki o pomembnih okoliskih krajih (iz vidika vremena)
setwd(OkoljeOkolica)
podatkiOkolica <- read.csv('realizacijaDez.csv')
okolica <- data.frame(matrix(NA, nrow(podatkiOkolica), 3))
colnames(okolica) <- c('id', 'dez', 'sneg')
okolica[, 1] <-  podatkiOkolica[,1]
okolica[, 2] <-  podatkiOkolica[,3]
okolica[, 3] <-  podatkiOkolica[,4]
## sestavljanje vseh moznih featurejv v matriko
setwd(OkoljeFM)
for(kraj in dir()){
	print(kraj)
	## nalozi vremensko napvoed za kraj
	if(substr(kraj, 1, 5) == 'vreme'){
		vremeNapoved <- readRDS(kraj)
		setwd(OkoljeReal)
		## za vse elektrarne v blizini kraja kraj uporabi vremensko napvoed
		for(elekt in dir()){
			if (gsub(".*T]|_.*", "", elekt) == 'Voda' &
				gsub(".*vreme_|_FM.*", "", kraj) == gsub(".*K]|_.*", "", elekt)){
					print(elekt)
					## sestavljanje osnove fm
					setwd(OkoljeFM)
					featureMatrix <- readRDS(kraj)
					## dodaj featureMatrix povprecno vrednost prejsnega dne in pobprecno spremembo
					## loadanje realizacije 
					setwd(OkoljeReal)
					realizacija <- readRDS(elekt)
					## vrednosti realizacije zamaknjene za 1 dan 
					lagR <- lag(realizacija, 24)
					diff_lagR <- diff(lagR)
					## povprecna realizacija zadnjih pretih ur
					movAvg <- SMA(na.omit(lagR), 5)
					## povprecje spremembe v zadnjem dnevu (zadnjih 4 ur)
					diffMovAvg <- SMA(na.omit(diff_lagR), 4)
					featureMatrix <- merge(featureMatrix, movAvg)
					featureMatrix <- merge(featureMatrix, diffMovAvg)
					## dodajanje zamikov dezja za 4 dni nazaj
					dez <- featureMatrix[,2]
					for(zamik in 1:(24*4)){
						lagDez <- lag(dez, zamik)
						featureMatrix <- merge(featureMatrix, lagDez)
						ime <- paste0('NapovedDez_zamik_',zamik)
						names(featureMatrix)[ncol(featureMatrix)] <- ime
						
					}
					idSt <- as.numeric(substr(elekt, 5,8))
					setwd(OkoljeReal)
					## pomembne lokacije za kolicino dezja
					kjeDez <- okolica[okolica[,1] == idSt,2]
					kjeDez <- strsplit(as.character(kjeDez), split = ' ')[[1]]
					kjeDez <- kjeDez[kjeDez != '']
					for(i in kjeDez){
						ime <- paste0(i, '.csv')
						setwd(OkoljePodatki)
						podatki <- read.csv(ime)
						podatki <- xts(podatki$količina.padavin..mm., as.POSIXct(podatki$valid))
						## dodajanje padavin za do 20 dni nazaj
						for(zamik in 1:20){							
							lagPod <- lag(podatki,zamik)
							zacetek <- as.Date(index(featureMatrix))[2]
							konec <- tail(as.Date(index(featureMatrix)),1)
							razprsi <- lagPod[paste0(zacetek, '/', konec)]
							featureMatrix <- merge(featureMatrix, razprsi)
							featureMatrix[,ncol(featureMatrix)] <- na.locf(featureMatrix$razprsi)
							## ime novega stolpca
							ime <- paste0(i, '_zamikDez_',zamik)
							names(featureMatrix)[ncol(featureMatrix)] <- ime
						}
						
					}
					## pomembne lokacije za kolicino snega
					kjeSneg <- okolica[okolica[,1] == idSt,3]
					kjeSneg <- strsplit(as.character(kjeSneg), split = ' ')[[1]]
					kjeSneg <- kjeSneg[kjeSneg != '']
					for(j in kjeSneg){
						ime <- paste0(j, '.csv')
						setwd(OkoljePodatki)
						podatki <- read.csv(ime)
						podatki <- xts(podatki$vi�.ina.snežne.odeje..cm., as.POSIXct(podatki$valid))
						## dodanjanje snega za 40 dni nazaj
						for(zamik in 1:40){
							difPodatki <- diff(podatki)
							lagPod <- lag(difPodatki,zamik)
							zacetek <- as.Date(index(featureMatrix))[2]
							konec <- tail(as.Date(index(featureMatrix)),1)
							razprsi <- lagPod[paste0(zacetek, '/', konec)]
							featureMatrix <- merge(featureMatrix, razprsi)
							featureMatrix[,ncol(featureMatrix)] <- na.locf(featureMatrix$razprsi)
							## ime novega stolpca
							ime <- paste0(j, 'zamikSneg_',zamik)
							names(featureMatrix)[ncol(featureMatrix)] <- ime
						}
					}
					## razdelitev feature matrike na train, valida in test del 
					## s pomocjo funkcije razdelitevNapovedi iz razdelitevPodatkov_funkcija.r
					FM_razdeljena <- razdelitevNapovedi(featureMatrix)
					## shranjevanje feature matrik (train)
					setwd(OkoljeFMc_Train)
					imeMatrike_Train <- paste0(substr(elekt, 1,8), 'featureMatrikaVSI_Train.rds')					
					saveRDS(FM_razdeljena$xTrain, imeMatrike_Train)
					## shranjevanje feature matrik (valid)
					setwd(OkoljeFMc_Valid)
					imeMatrike_Valid <- paste0(substr(elekt, 1,8), 'featureMatrikaVSI_Valid.rds')					
					saveRDS(FM_razdeljena$xValid, imeMatrike_Valid)
					## shranjevanje feature matrik (test)
					setwd(OkoljeFMc_Test)
					imeMatrike_Test <- paste0(substr(elekt, 1,8), 'featureMatrikaVSI_Test.rds')					
					saveRDS(FM_razdeljena$xTest, imeMatrike_Test)
			}
			setwd(OkoljeReal)
		}
	}
	setwd(OkoljeFM)
}


					

					
