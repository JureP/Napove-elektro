## napovedi modelov (KI ZA UCENJE UPROABLJAJO TUDI VALIDACIJSKO MNOZICO) na testni mnozici 
library(xts)
library(randomForest)
library(gbm)
library(e1071)

## okolje v kateri so mape z vsemi modeli 
OkoljeModel <- 'C:/Users/Podlogar/Documents/Projekt Elektro/03_Ucenje modela/Sonce/VkljuciValid/Model'
## okolje kjer so shranjene realizacije (test)
OkoljeRealizacija <- 'C:/Users/Podlogar/Documents/Projekt Elektro/00_Podatki/realizacija/Test'
## okolje kjer so feature matrike test dela mnozice
OkoljeFM <- 'C:/Users/Podlogar/Documents/Projekt Elektro/00_Podatki/featureMatrix/Test'
## okolje kjer so shranjeni delezi v ensemblu
OkoljeEnsemble <- 'C:/Users/Podlogar/Documents/Projekt Elektro/07Ensemble/Izbor'
## okolje kjer so shranjeni ensembleRF modeli (ucenje z nekaj 0)
OkoljeEnsembleRF_zNekaj0 <- 'C:/Users/Podlogar/Documents/Projekt Elektro/07Ensemble/Izbor_RF_zNekaj0'
## okolje kamor naj se shranijo mape (pozameznih modlev) z napovedmi
OkoljeTest <- 'C:/Users/Podlogar/Documents/Projekt Elektro/08_Test/RazsirjenoUcenje_napovedi'

## Okolje kjer so funkcije
OkoljeFunkcije <- 'C:/Users/Podlogar/Documents/Projekt Elektro/Funkcije'

setwd(OkoljeFunkcije)
source("napovej_funkcija.r")


## napoved cez vse elektrarne
setwd(OkoljeRealizacija)
for(elekt in dir()){
	realizacija <- readRDS(elekt)
	if (gsub(".*T]|_.*", "", elekt) == 'Sonce'){
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
			imeFM <- dir()[gsub(".*K]|_.*", "", elekt) == gsub(".*vreme_|_FM.*", "", dir())]
			FM <- readRDS(imeFM)
			napoved <- napovej(model, FM, gsub("|_.*", "", mdl))
			napoved <- xts(napoved, index(FM))
			# plot(napoved, main = paste(mdl, substr(elekt, 5,8)))
			# lines(realizacija, col = 'red')
			#cat(paste0("Press [enter] to continue from", mdl))
			#line <- readline()
			## shranjevanje napovedi 
			OkoljeSave <- paste0(OkoljeTest, '/', mdl)
				## ustvari mapo 
			dir.create(OkoljeSave, showWarnings = TRUE, recursive = FALSE, mode = "0777")
			setwd(OkoljeSave)
			imeSave <- paste0(substr(elekt,1,8),'_', mdl, '_napoved_razsirjenoUcenje.rds')
			saveRDS(napoved, imeSave)
		}
	}
	setwd(OkoljeRealizacija)
}



### Povprecenje napake


## okolje kjer so shranjene mape (pozameznih modlev) z napovedmi
OkoljeTest <- 'C:/Users/Podlogar/Documents/Projekt Elektro/08_Test/RazsirjenoUcenje_napovedi'
## okolje kjer so shranjene realizacije (test)
OkoljeRealizacija <- 'C:/Users/Podlogar/Documents/Projekt Elektro/00_Podatki/realizacija/Test'
## okolje kjer je klasifikator (napoved 0 realizacije na testni mnozici)
OkoljeKlasif <- 'C:/Users/Podlogar/Documents/Projekt Elektro/06_Klasifikacija/TestRF'


## okolje kamor se shranijo rezultati (ce za napoved nedelovanja uporabis klasifikator)
OkoljeRezultat_1 <- 'C:/Users/Podlogar/Documents/Projekt Elektro/08_Test/napake_RazsirjenoUcenje_napovedi'


## Okolje kjer so funkcije
OkoljeFunkcije <- 'C:/Users/Podlogar/Documents/Projekt Elektro/Funkcije'

setwd(OkoljeFunkcije)
source("nedelovanje_funkcija.r" )


## cez vse modele
setwd(OkoljeTest)
for(mdl in dir()){
	print(mdl)
	Okolje <- paste0(OkoljeTest,'/',mdl)
	## data.frame v katerega se shranijo MSE
	napake <- data.frame(matrix(NA, 100 ,2))
	colnames(napake) <- c('root mean square error', 'var of error')
	imenaElektrarn <- NULL
	vrstica <- 1
	## cez napovedi za vsako elektrarno
	setwd(Okolje)
	for(elekt in dir()){
		print(elekt)
		## loadanje napovedi
		napoved <- readRDS(elekt)
		## loadanje klasifikatorja
		setwd(OkoljeKlasif)
		imeKlas <- dir()[substr(dir(),4,8) == substr(elekt, 4,8)]
		napovedKlasifikator <- readRDS(imeKlas)
		## loadanje dejanskih realizacij
		setwd(OkoljeRealizacija)
		imeE <- dir()[substr(dir(),4,8) == substr(elekt, 4,8)]
		realizacija <- readRDS(imeE)
		## odstrani dni ko elektrana ne proizvaja
		realizacija <- nedelovanje(realizacija)
		## nastavi napoved 0 klasifikator napove 0
		napoved[napovedKlasifikator == 0] <- 0
		## napoves 0 ko je napoved manjsa od 0
		napoved[napoved < 0] <- 0
		## napoves neto moc ko je napoved vecja od neto moci 
		netoMoc <- as.numeric(gsub(".*M]|_.*", "", imeE))
		napoved[napoved > netoMoc] <- netoMoc
		## izracun napake
			## odstrani dneve ko elektrarna ne proizvaja
			napoved[is.na(realizacija)] <- NA
			napoved <- na.omit(napoved)
			realizacija <- na.omit(realizacija)
		## graf primerjave napovedi in realizacije
		 # plot(napoved, ylim = c(-2,netoMoc), main = paste(mdl, substr(elekt, 1,8)))
		 # lines(realizacija, lty = 3, col = 'red')
		 # cat(paste0("Press [enter] to continue from", mdl, substr(elekt, 1,8)))
		 # line <- readline()
		napake[vrstica, 1] <- (sqrt(sum((napoved - realizacija)^2)/length(napoved)))/max(realizacija)
		napake[vrstica, 2] <- var(napoved - realizacija)/max(realizacija)
		rownames(napake)[vrstica] <- substr(elekt, 1, 8)
		vrstica <- vrstica + 1
		setwd(Okolje)
	}
	setwd(OkoljeRezultat_1)
	imeNapake <- paste0(mdl,'_napakaTest_brezNedela_ucenjeValid.rds')
	saveRDS(napake, imeNapake)
	setwd(OkoljeTest)
}

