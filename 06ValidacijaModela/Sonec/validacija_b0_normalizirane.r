## pregled rezultatov na validacijski mnozici
library(xts)

## okolje v katerem so mape z napovedmi na validacijski mnozici
OkoljeValid <- 'C:/Users/Podlogar/Documents/Projekt Elektro/03_Ucenje modela/Sonce/VseNapovedi_valid'

## okolje kjer so realizacije (valid)
OkoljeReal_valid <- 'C:/Users/Podlogar/Documents/Projekt Elektro/00_Podatki/realizacija/Valid'  

## okolje kamor naj se shranijo podati o napakah na validacijski mnozici
OkoljeNapaka <- 'C:/Users/Podlogar/Documents/Projekt Elektro/04_Validacija modela/Napake_normalizirane'

##  cez vse datoteke v OkoljeValid
setwd(OkoljeValid)
for(mdl in dir()){
	print(mdl)
	Okolje <- paste0(OkoljeValid,'/',mdl)
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
		setwd(OkoljeReal_valid)
		imeE <- dir()[substr(dir(),4,8) == substr(elekt, 4,8)]
		realizacija <- readRDS(imeE)
		## odstrannitev vrednosti ko je dejanska realizacija 0
		napoved <- napoved[realizacija != 0]
		realizacija <- realizacija[realizacija != 0]
		napake[vrstica, 1] <- (sqrt(sum((napoved - realizacija)^2)))/max(realizacija)
		napake[vrstica, 2] <- var(napoved - realizacija)/max(realizacija)
		rownames(napake)[vrstica] <- substr(elekt, 1, 8)
		vrstica <- vrstica + 1
		setwd(Okolje)
	}
	setwd(OkoljeNapaka)
	imeNapake <- paste0(mdl,'_napaka.rds')
	saveRDS(napake, imeNapake)
	setwd(OkoljeValid)
}
