## pretvorba csv-jev v rds
library(xts)

## okolje kjer so shranjene realizacije proizvodnje
OkoljeRealizacija <- 'C:/Users/Podlogar/Documents/Projekt Elektro/01_Priprava podatkov/Podatki/Proizvodnja'
## realizacija shranjena v RDS
OkoljeRealizacijaRDS <- 'C:/Users/Podlogar/Documents/Projekt Elektro/01_Priprava podatkov/Podatki/Proizvodnja/RDS'
## okolje kjer so shranjene napovedi vremenskih parametrov
OkoljeVreme <- 'C:/Users/Podlogar/Documents/Projekt Elektro/01_Priprava podatkov/Podatki/RNF - Meritve in ARSO podatki'
## okolje kjer so shranjene napovedi vremenskih parametrov
OkoljeVremeRDS <- 'C:/Users/Podlogar/Documents/Projekt Elektro/01_Priprava podatkov/Podatki/RNF - Meritve in ARSO podatki/RDS'


## iz csv-ja realizacije ustvari xts in ga shrani kot RDS
setwd(OkoljeRealizacija)
for(elekt in dir()){
	## izbor relavantnih filov
	if(substr(elekt, 1, 4) == '[id]'){
		print(elekt)
		setwd(OkoljeRealizacija)
		realizacija <- read.csv(elekt)
		## tvorjenje xts-a
		realizacija <- as.xts(realizacija[,2], as.POSIXct(realizacija[,1],format='%Y-%m-%d %H:%M:%S'))
		names(realizacija) <- 'realizacija'
		## ime za RDS file
		imeRDS <- paste0(substr(elekt, 1, nchar(elekt)-3), 'rds')
		setwd(OkoljeRealizacijaRDS)
		## shranjevanje rds-a
		saveRDS(realizacija, imeRDS)
	}
}


## iz csv-ja vremenskih napovedi ustvari xts in ga shrani kot RDS
setwd(OkoljeVreme)
for(kraj in dir()){
	## izbor relavantnih failov
	if(substr(kraj, 1, 5) == 'vreme'){
		setwd(OkoljeVreme)
		## ime rds-a
		imeRDS <- paste0(substr(kraj, 1, nchar(kraj)-3), 'rds')
		setwd(OkoljeVreme)
		napoved <- read.csv(kraj)
		## tvorjenje xts-a
		datumi <- as.POSIXct(paste0(as.character(napoved[,1]), ' ', as.character(napoved[,2])), format = '%Y-%m-%d %H:%M:%S')
		napovedVreme <- xts(napoved[,3:ncol(napoved)], datumi)
		setwd(OkoljeVremeRDS)
		saveRDS(napovedVreme ,imeRDS)		
	}
}
