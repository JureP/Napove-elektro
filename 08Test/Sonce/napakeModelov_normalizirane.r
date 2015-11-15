## pregled rezultatov napovedi na testni mnozici za soncne elektrarne
library(xts)


## okolje kjer so shranjene mape (pozameznih modlev) z napovedmi
OkoljeTest <- 'C:/Users/Podlogar/Documents/Projekt Elektro/08_Test/NapovedTest'
## okolje kjer so shranjene realizacije (test)
OkoljeRealizacija <- 'C:/Users/Podlogar/Documents/Projekt Elektro/00_Podatki/realizacija/Test'
## okolje kjer je klasifikator (napoved 0 realizacije na testni mnozici)
OkoljeKlasif <- 'C:/Users/Podlogar/Documents/Projekt Elektro/06_Klasifikacija/TestRF'


## okolje kamor se shranijo rezultati (ce za napoved nedelovanja uporabis klasifikator)
OkoljeRezultat_1 <- 'C:/Users/Podlogar/Documents/Projekt Elektro/08_Test/Napake_normalizirane'
## okolje kamor se shranijo rezultati (ce ves kdaj elektrarna ne dela)
OkoljeRezultat_2 <- 'C:/Users/Podlogar/Documents/Projekt Elektro/08_Test/Napake_normalizirane_brez0'
## okolje kamor se shranijo rezultati samo osnovnega napovednega modela
OkoljeRezultat_3 <- 'C:/Users/Podlogar/Documents/Projekt Elektro/08_Test/Napake_normalizirane_osnovni'

## Okolje kjer so funkcije
OkoljeFunkcije <- 'C:/Users/Podlogar/Documents/Projekt Elektro/Funkcije'

setwd(OkoljeFunkcije)
source("nedelovanje_funkcija.r" )

#########################################################################################
#########################################################################################
## test z uporabo klasifikatorja in odstranjenimi deli ko je proizvodnja 0 vec kot en dan

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
	imeNapake <- paste0(mdl,'_napakaTest_brezNedela.rds')
	saveRDS(napake, imeNapake)
	setwd(OkoljeTest)
}




#########################################################################################
#########################################################################################
## test z uporabo popolnega vedenja o nicelni proizvodnji elektrarne

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
		## loadanje napovedi
		napoved <- readRDS(elekt)
		## loadanje dejanskih realizacij
		setwd(OkoljeRealizacija)
		imeE <- dir()[substr(dir(),4,8) == substr(elekt, 4,8)]
		realizacija <- readRDS(imeE)
		## nastavi napoved 0 ko je dejanska realizacija 0
		napoved[realizacija == 0] <- 0
		## napoves 0 ko je napoved manjsa od 0
		napoved[napoved < 0] <- 0
		## napoves neto moc ko je napoved vecja od neto moci 
		netoMoc <- as.numeric(gsub(".*M]|_.*", "", imeE))
		napoved[napoved > netoMoc] <- netoMoc
		## graf primerjave napovedi in realizacije
		# plot(napoved, ylim = c(-2,netoMoc), main = paste(mdl, substr(elekt, 1,8)))
		# lines(realizacija, lty = 3, col = 'red')
		# cat(paste0("Press [enter] to continue from", mdl, substr(elekt, 1,8)))
		# line <- readline()
		## izracun napake
		napake[vrstica, 1] <- (sqrt(sum((napoved - realizacija)^2)/length(napoved)))/max(realizacija)
		napake[vrstica, 2] <- var(napoved - realizacija)/max(realizacija)
		rownames(napake)[vrstica] <- substr(elekt, 1, 8)
		vrstica <- vrstica + 1
		setwd(Okolje)
	}
	setwd(OkoljeRezultat_2)
	imeNapake <- paste0(mdl,'_napakaTest.rds')
	saveRDS(napake, imeNapake)
	setwd(OkoljeTest)
}

#########################################################################################
#########################################################################################
## test z uporabo samo osnovnega modela

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
	setwd(OkoljeRezultat_3)
	imeNapake <- paste0(mdl,'_napakaTest_osnovni.rds')
	saveRDS(napake, imeNapake)
	setwd(OkoljeTest)
}





