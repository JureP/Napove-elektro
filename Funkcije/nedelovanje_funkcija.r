## funkcija ki odstrani obdobja ko elektrarna ne deluje 

nedelovanje <- function(realizacij, ## realizacija proizvodnje (xts)
						stDni = 1 ## st dni 0, da se elekt proglasi za nedelujoco
						)
				## funkcija ki iz podatkov odstrani obdobje, ko 
				## elektrarna ne deluje (realizacija = 0 za stDni)
				## vrne realizacijo z NA na mestu "nedelovanja"
						
	{				
	realDelujoc <- realizacija
	## zaradi delovanja xts['od/do']
	stDni <- stDni - 1
	## ali je realizacija 0
	nedelo <- realizacija == 0
	## zacetek pregledovanja
	zacetek <- as.Date(index(nedelo)[1])
					

	for (i in 1:(ndays(realizacija) - stDni)){
		dan <- zacetek + i
		poglej <- paste0(dan, '/', dan + stDni)
		if(sum(na.omit(nedelo[poglej])) >= 23){
			realDelujoc[poglej] <- NA
		}
	}
	
	return(realDelujoc)				
}