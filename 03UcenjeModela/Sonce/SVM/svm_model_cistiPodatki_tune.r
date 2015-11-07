## svm model s tunanjem hyperparametrov
## odstrani obdobja ko elektrarna ne deluje
## iz nocnih ur vzame samo nekaj nakljucnih ur na dan
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
OkoljeModel <- 'C:/Users/Podlogar/Documents/Projekt Elektro/03_Ucenje modela/Sonce/SVM/Model'
## okolje kjer se ustvari mapa v katero se shranijo rezultati na validacijski mnozici
OkoljeValidacija <- 'C:/Users/Podlogar/Documents/Projekt Elektro/03_Ucenje modela/Sonce/SVM/Validacija'


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
		setwd(OkoljeReal)
		## za vse elektrarne v blizini kraja kraj uporabi vremensko napvoed
		for(elekt in dir()){
			if (gsub(".*vreme_|_FM.*", "", kraj)  == gsub(".*K]|_.*", "", elekt) &
			gsub(".*T]|_.*", "", elekt) == 'Sonce'){
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
				Y <- A[, ncol(A)]
				X <- A[,1:(ncol(A)-1)]	
				## tunanje hyperparametrov in ucenje modela
				tc <- tune.control(cross = 3)
				print('tune')
				op <- tune(svm, X, Y, ranges = list(epsilon = seq(0,0.5,0.1), cost = c(2,4,6,8,12,14,16,18,20)), tunecontrol = tc)
				model <- op$best.model
				## validacija modela
				print('validacija')
				newdata <- data.frame(X = I(featureMatrixValid))
				napovedSVM <- predict(model, newdata)
				napovedSVM <- xts(napovedSVM, index(featureMatrixValid))
				## shranjevanje modela
				setwd(Okolje1)
				## ime modela
				imeModel <- paste0(substr(elekt, 1, 8), '_model_', epsilon, '_', cost,'_SVM_cist.rds')
				saveRDS(op, imeModel)
				## shranjevanje validacije
				setwd(Okolje2)
				## ime validacije
				imeValid <- paste0(substr(elekt, 1, 8), '_validacija_', epsilon, '_', cost,'_SVM_cist.rds')
				saveRDS(napovedSVM, imeValid)
				setwd(OkoljeReal)
			}
		}
	}
	setwd(OkoljeFM)
}





## ucenje
setwd(OkoljeFM)
kraj <- dir()[2]
featureMatrix <- readRDS(kraj)
setwd(OkoljeReal)
elekt <- dir()[116]
realizacija <- readRDS(elekt)

X <- featureMatrix
start1 <- Sys.time()
model <- svm(realizacija ~ X, cost = 2)
stop1 <- Sys.time()
cas1 <- stop1 - start1


start2 <- Sys.time()
model <- svm(realizacija ~ X, cost = 5)
stop2 <- Sys.time()
cas2 <- stop2 - start2
print(cas1)
print(cas2)

## validacija 

setwd(OkoljeFM_valid)
kraj <- dir()[2]
featureMatrixValid <- readRDS(kraj)
setwd(OkoljeReal_valid)
elekt <- dir()[116]
realizacijaValid <- readRDS(elekt)


newdata <- data.frame(X = I(featureMatrixValid))
napovedSVR <- predict(model, newdata)
kakoDobro <- xts(napovedSVR, index(realizacijaValid))
plot(kakoDobro[1:500])
lines(realizacijaValid[1:500], col = 'red')


## tune

neNic <- (realizacija != 0)
Y <- realizacija[neNic]
X <- featureMatrix[neNic, ]
tc <- tune.control(cross = 3)
start <- Sys.time()
op <- tune(svm, Y ~ X,  ranges = list(epsilon = seq(0,1,0.3), cost = 2^(c(0,2,4,6))), tunecontrol = tc)
stop <- Sys.time()
cas <- stop - start
setwd(OkoljeModel)
saveRDS(cas, 'casTunanja.rds')
saveRDS(op, 'testniSVMmodel.rds')
##op <- tune(svm, realizacija ~ featureMatrix,  ranges = list(epsilon = seq(0,1,0.3), cost = 2^(2:4)), tunecontrol = tc)
print(op)
plot(op)


bestModel <- op$best.model
model <- bestModel