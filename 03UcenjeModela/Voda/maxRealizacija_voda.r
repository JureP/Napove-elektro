## maksimalna realizacija na train mnozici

## okolje kjer so realizacije (test)
OkoljeReal <- 'C:/Users/Podlogar/Documents/Projekt Elektro/00_Podatki/realizacija/Train'  

## okolje kamor se shranijo maksimalne realizacije
OkoljeSave <- 'C:/Users/Podlogar/Documents/Projekt Elektro/03_Ucenje modela/Hidro/MaxRalizacija'  

setwd(OkoljeReal)
for(elekt in dir()){
	if (gsub(".*T]|_.*", "", elekt) == 'Voda'){
		realizacija <- readRDS(elekt)
		print(elekt)
		maRe <- max(na.omit(realizacija))
		setwd(OkoljeSave)
		ime <- paste0(substr(elekt,1,8), 'maksimalnaRealizacija.rds')
		saveRDS(maRe, ime)
	}
	setwd(OkoljeReal)
}
