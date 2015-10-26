## pretvorba csv-jev v rds
## tvori feature matrix
library(xts)
library(lubridate)

## okolje kjer so shranjene realizacije proizvodnje
OkoljeRealizacija <- 'C:/Users/Podlogar/Documents/Projekt Elektro/01_Priprava podatkov/Podatki/Proizvodnja'
## okolje v katerem se shrani RDS realizacije
OkoljeRealizacijaSAVE <- 'C:/Users/Podlogar/Documents/Projekt Elektro/00_Podatki/PodatkiRDS/realizacija'
## okolje kjer so shranjene napovedi vremenskih parametrov
OkoljeVreme <- 'C:/Users/Podlogar/Documents/Projekt Elektro/01_Priprava podatkov/Podatki/RNF - Meritve in ARSO podatki'
## okolje kjer so shranjene napovedi vremenskih parametrov
OkoljeVremeSAVE <- 'C:/Users/Podlogar/Documents/Projekt Elektro/00_Podatki/PodatkiRDS/vremenskaNapoved'
OkoljeFM_SAVE <- 'C:/Users/Podlogar/Documents/Projekt Elektro/00_Podatki/PodatkiRDS/featureMatrix'


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
		setwd(OkoljeRealizacijaSAVE)
		## shranjevanje rds-a
		saveRDS(realizacija, imeRDS)
	}
}


## iz csv-ja vremenskih napovedi ustvari xts in ga shrani kot RDS
## ustvari feature matrix
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
		setwd(OkoljeVremeSAVE)
		saveRDS(napovedVreme ,imeRDS)
		## tvorjenje feature matrix
		## dodajanje dneva v letu in ure kot featurja
		napoved[, 'dan v letu'] <- yday(datumi)
		napoved[, 'ura v dnevu']<- hour(datumi)
		## alternativno mesec in dan v mesecu (namesto dneva v letu)
		## month(datumi)
		## mday(datumi)
		## tvorjenje xts-a		
		featureMatrix <- xts(napoved[,3:ncol(napoved)], datumi)
		setwd(OkoljeFM_SAVE)
		imeFM <- paste0(substr(kraj, 1, nchar(kraj)-4), '_FM.rds')
		saveRDS(featureMatrix ,imeFM)		
	}
}
