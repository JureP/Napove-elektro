## pregled rezultatov napovedi na testni mnozici za hidro elektrarne
library(xts)


## okolje kjer so shranjene mape (pozameznih modlev) z napovedmi
OkoljeTest <- 'C:/Users/Podlogar/Documents/Projekt Elektro/08_Test(voda)/NapovedTest'
## okolje kjer so shranjene realizacije (test)
OkoljeRealizacija <- 'C:/Users/Podlogar/Documents/Projekt Elektro/00_Podatki/realizacija/Test'
## Okolje kjer so maksimumi na train mnozici
OkoljeMax <- 'C:/Users/Podlogar/Documents/Projekt Elektro/03_Ucenje modela/Hidro/MaxRalizacija'


## okolje kamor se shranijo rezultati osnovnega modela
OkoljeRezultat_1 <- 'C:/Users/Podlogar/Documents/Projekt Elektro/08_Test(voda)/Napake_normalizirane'
## okolje kamor se shranijo rezultati omejenega modela
OkoljeRezultat_2 <- 'C:/Users/Podlogar/Documents/Projekt Elektro/08_Test(voda)/NapakeOmejene_normalizirane'


## Okolje kjer so funkcije
OkoljeFunkcije <- 'C:/Users/Podlogar/Documents/Projekt Elektro/Funkcije'

setwd(OkoljeFunkcije)
source("nedelovanje_funkcija.r" )


#########################################################################################
#########################################################################################
## Test brez omejitve maksimuma

## cez vse modele
setwd(OkoljeTest)
for(mdl in dir()){
	print(mdl)
	Okolje <- paste0(OkoljeTest,'/',mdl)
	## data.frame v katerega se shranijo MSE
	napake <- data.frame(matrix(NA, 16 ,2))
	colnames(napake) <- c('root mean square error', 'var of error')
	imenaElektrarn <- NULL
	vrstica <- 1
	## cez napovedi za vsako elektrarno
	setwd(Okolje)
	for(elekt in dir()){
		## loadanje napovedi
		napoved <- readRDS(elekt)
		## loadanje dejanskih realizacij
		setwd(OkoljeRealizacija)
		imeE <- dir()[substr(dir(),4,8) == substr(elekt, 4,8)]
		realizacija <- readRDS(imeE)
		## napoves 0 ko je napoved manjsa od 0
		napoved[napoved < 0] <- 0
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
	imeNapake <- paste0(mdl,'_napakaTest_brezNedela.rds')
	saveRDS(napake, imeNapake)
	setwd(OkoljeTest)
}


setwd(OkoljeRezultat_1)
for(i in dir()){
	print(i)
	print(summary(readRDS(i)))
}


#########################################################################################
#########################################################################################
## Test z omejitvijo z maksimumom iz train mnozice

## cez vse modele
setwd(OkoljeTest)
for(mdl in dir()){
	print(mdl)
	Okolje <- paste0(OkoljeTest,'/',mdl)
	## data.frame v katerega se shranijo MSE
	napake <- data.frame(matrix(NA, 16 ,2))
	colnames(napake) <- c('root mean square error', 'var of error')
	imenaElektrarn <- NULL
	vrstica <- 1
	## cez napovedi za vsako elektrarno
	setwd(Okolje)
	for(elekt in dir()){
		## loadanje napovedi
		napoved <- readRDS(elekt)
		## loadanje dejanskih realizacij
		setwd(OkoljeRealizacija)
		imeE <- dir()[substr(dir(),4,8) == substr(elekt, 4,8)]
		realizacija <- readRDS(imeE)
		## napoves 0 ko je napoved manjsa od 0
		napoved[napoved < 0] <- 0
		## omejitev na maksimum na train mnozici
		setwd(OkoljeMax)
		imeMax <- dir()[substr(elekt,1,8) == substr(dir(),1,8)]
		maxProizvodnja <- readRDS(imeMax)
		napoved[napoved > maxProizvodnja] <- maxProizvodnja
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
	setwd(OkoljeRezultat_2)
	imeNapake <- paste0(mdl,'_napakaTest_Omejen_brezNedela.rds')
	saveRDS(napake, imeNapake)
	setwd(OkoljeTest)
}



setwd(OkoljeRezultat_2)
for(i in dir()){
	print(i)
	print(summary(readRDS(i)))
}







