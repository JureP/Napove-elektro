## pregled rezultatov na validacijski mnozici
library(xts)

## okolje v katerem so mape z napovedmi na validacijski mnozici
OkoljeValid <- 'C:/Users/Podlogar/Documents/Projekt Elektro/03_Ucenje modela/Hidro/vseValidacije'

## okolje kjer so realizacije (valid)
OkoljeReal_valid <- 'C:/Users/Podlogar/Documents/Projekt Elektro/00_Podatki/realizacija/Valid'  

## Okolje kjer so maksimumi na train mnozici
OkoljeMax <- 'C:/Users/Podlogar/Documents/Projekt Elektro/03_Ucenje modela/Hidro/MaxRalizacija'

## okolje kamor naj se shranijo podati o napakah na validacijski mnozici
OkoljeNapaka <- 'C:/Users/Podlogar/Documents/Projekt Elektro/04_Validacija modela(Voda)/Napake_normalizirane'


## cez vse modele
setwd(OkoljeValid)
for(mdl in dir()){
	print(mdl)
	Okolje <- paste0(OkoljeValid,'/',mdl)
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
		setwd(OkoljeReal_valid)
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
	setwd(OkoljeNapaka)
	imeNapake <- paste0(mdl,'_napakaValid_brezNedela.rds')
	saveRDS(napake, imeNapake)
	setwd(OkoljeValid)
}



## primerjava rezultatov
setwd(OkoljeNapaka)
for(i in dir()){
	print(i)
	print(summary(readRDS(i)))
}



## vse napake v matriko
napakeSkupaj <- NULL
imenaStolpcev <- NULL
for(i in dir()){
	napake <- readRDS(i)
	imenaStolpcev <- c(imenaStolpcev, i)
	napakeSkupaj <- cbind(napakeSkupaj, napake[,1])
}
rownames(napakeSkupaj) <- rownames(napake)
colnames(napakeSkupaj) <- imenaStolpcev
View(napakeSkupaj)
najmanjsi <- NULL
for(vrstica in 1:nrow(napakeSkupaj)){
	najmanjsi <- c(najmanjsi, names(which.min(napakeSkupaj[vrstica,])))
}
najmanjsi
names(najmanjsi) <- rownames(napakeSkupaj)
setwd('C:/Users/Podlogar/Documents/Projekt Elektro/08Test')
saveRDS(najmanjsi, 'najboljsiModeliValidacija.rds')
