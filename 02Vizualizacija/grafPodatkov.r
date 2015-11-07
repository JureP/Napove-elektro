## vizualizavija podatkov 
library(xts)

## okolje kjer je shranjena realizacija v RDS
OkoljeRealizacijaRDS <- 'C:/Users/Podlogar/Documents/Projekt Elektro/00_Podatki/PodatkiRDS/realizacija'
## okolje kjer so shranjene napovedi vremenskih parametrov v RDS
OkoljeVremeRDS <- 'C:/Users/Podlogar/Documents/Projekt Elektro/00_Podatki/PodatkiRDS/vremenskaNapoved'


## risanje grafov realizacije
setwd(OkoljeRealizacijaRDS)
for(elekt in dir()){
	imeRDS <- paste0(substr(elekt, 1, nchar(elekt)-3), 'rds')
	realizacija <- readRDS(imeRDS)
	plot(realizacija)
	cat (paste0("Press [enter] to continue from", elekt))
    line <- readline()
	#print(elekt)
	#Sys.sleep(2)
}



## risanje grafov realizacije v obdobju enega leta
setwd(OkoljeRealizacijaRDS)
for(elekt in dir()){
	imeRDS <- paste0(substr(elekt, 1, nchar(elekt)-3), 'rds')
	realizacija <- readRDS(imeRDS)
	plot(realizacija[1:(356*24)])
	#cat (paste0("Press [enter] to continue from", elekt))
    #line <- readline()
	print(elekt)
	Sys.sleep(2)
}

## realizacija v odvisnosti od vermenske napovedi

setwd(OkoljeVremeRDS)
for(kraj in dir()){
	print(kraj)
	## nalozi vremensko napvoed za kraj
	if(substr(kraj, 1, 5) == 'vreme'){
		vremeNapoved <- readRDS(kraj)
		setwd(OkoljeRealizacijaRDS)
		## za vse elektrarne v blizini kraja kraj uporabi vremensko napvoed
		for(elekt in dir()){
			#print(elekt)
			if (gsub(".*_|.rds.*", "", kraj) == gsub(".*K]|_.*", "", elekt)){
				realizacija <- readRDS(elekt)
				## narisi graf za realizavije v odvisnosti od napovedi vremena
				for(i in 1:ncol(vremeNapoved)){
					plot(as.vector(vremeNapoved[,i]), as.vector(realizacija), ylab = 'realizacija', xlab = names(vremeNapoved[,i]))
					cat(paste0("Press [enter] to continue from", elekt))
					line <- readline()
				}
			}
		}
		setwd(OkoljeVremeRDS)		
	}
}


## povezava med vremenskimi parametri
setwd(OkoljeVremeRDS)
for(kraj in dir()){
	if(substr(kraj, 1, 5) == 'vreme'){
		vremeNapoved <- readRDS(kraj)		
		plot(data.frame(vremeNapoved))
		cat(paste0("Press [enter] to continue from", elekt))
		line <- readline()
	}	
}





