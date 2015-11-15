## sestavljanje ensembla za napovedovanje realizacije soncnih elektraren
library(xts)

## okolje v katerem so mape z napovedmi na validacijski mnozici
OkoljeValid <- 'C:/Users/Podlogar/Documents/Projekt Elektro/03_Ucenje modela/Hidro/vseValidacije'

## okolje kjer so realizacije (valid)
OkoljeReal_valid <- 'C:/Users/Podlogar/Documents/Projekt Elektro/00_Podatki/realizacija/Valid'  


## okolje kamor naj se shrani ensemble vektor 
OkoljeEnsemble <- 'C:/Users/Podlogar/Documents/Projekt Elektro/07Ensemble(voda)/Izbor'

## stevilo nakljucno izbranih modelov na vsakme koraku
stIzbor <- 5
## kolikokrat izbere najboljsi model
stPon <- 100
## cez vse elektrarne
setwd(OkoljeReal_valid)
for(elekt in dir()){
	realizacija <- readRDS(elekt)
	if (gsub(".*T]|_.*", "", elekt) == 'Voda'){
		print(elekt)
		## utezi ensembla
		setwd(OkoljeValid)
		ensemble <- rep(0, length(dir()))
		names(ensemble) <- dir()
		## dodajanje modelov ensemblu
		for(ponovitev in 1:stPon){
			#print(ponovitev)
			## napovedi trenutnega ensemble
			setwd(OkoljeValid)
			mdl <- dir()[1]
			Okolje <- paste0(OkoljeValid,'/',mdl)
			setwd(Okolje)
			imeNapoved <- dir()[substr(dir(),4,8) == substr(elekt, 4,8)]
			napoved <- readRDS(imeNapoved)
			realizacija <- realizacija[index(napoved)]
			napovedEnsembla <- rep(0, length(realizacija[realizacija != 0]))
			setwd(OkoljeValid)
			for(mdl in dir()){
				## loadanje napovedi modela mdl
				Okolje <- paste0(OkoljeValid,'/',mdl)
				setwd(Okolje)
				imeNapoved <- dir()[substr(dir(),4,8) == substr(elekt, 4,8)]
				napoved <- readRDS(imeNapoved)
				realizacija <- realizacija[index(napoved)]
				## sharnjevanje v matrikaNapovedi (brez napovedi pri katerih je realizacija 0)
				napovedEnsembla <- napovedEnsembla + ensemble[mdl]/(sum(ensemble)+1)*napoved[realizacija != 0]
			}
			## napovedi (5-tih) izbranih modelov (brez 0 pri realizaicija)
			matrikaNapovedi <- data.frame(matrix(0, length(realizacija[realizacija != 0]), stIzbor))
			## cez vse model 5 nakljucno izbrane modele
			setwd(OkoljeValid)
			izbor <- sample(dir(),stIzbor)
			stevec <- 1
			for(mdl in izbor){
				## loadanje napovedi modela mdl
				Okolje <- paste0(OkoljeValid,'/',mdl)
				setwd(Okolje)
				imeNapoved <- dir()[substr(dir(),4,8) == substr(elekt, 4,8)]
				napoved <- readRDS(imeNapoved)
				realizacija <- realizacija[index(napoved)]
				## sharnjevanje v matrikaNapovedi (brez napovedi pri katerih je realizacija 0)
				matrikaNapovedi[, stevec] <- napovedEnsembla + 1/(sum(ensemble)+1)*napoved[realizacija != 0]
				names(matrikaNapovedi)[stevec] <- mdl
				stevec <- stevec + 1
			}
			## zamenjava nesmiselnih napovedi: napovd manja od 0, napoved vecja od neto moci??
			matrikaNapovedi[matrikaNapovedi < 0] <-  0
			mMoc <- as.numeric(gsub(".*M]|_.*", "", elekt))
			matrikaNapovedi[matrikaNapovedi > mMoc] <-  mMoc
			napaka <- matrikaNapovedi - t(realizacija[realizacija != 0])
			napaka2 <- napaka^2
			mse  <- sqrt(colMeans(napaka2))
			## dodaj najboljsega ensemblu
			ensemble[names(ensemble) == names(which.min(mse))] <- ensemble[names(ensemble) == names(which.min(mse))] + 1
		}
		setwd(OkoljeEnsemble)
		imeEnsemble <- paste0(substr(elekt, 1,8), '_ensemble.rds')
		saveRDS(ensemble/stPon, imeEnsemble)
		
	}
	setwd(OkoljeReal_valid)
}

realizacija <- realizacija[index(napoved)]
napoved <- napoved[!is.na(realizacija)]
realizacija <- na.omit(realizacija)		
