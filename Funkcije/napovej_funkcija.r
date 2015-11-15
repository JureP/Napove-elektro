napovej <- function(model, ## model ki se uporablja za napovedovanje
					featureMatrix, ## feature matrika za napoved 
					tipModel ## tip modela model ki je uporabljen za ucenje
					){
				## funkcija ki zdruzi funkcijo predict za razlicne modele
				
				if(tipModel == 'OLS'){
					newdata <- data.frame(X = I(featureMatrix))
					napoved <- predict(model, newdata)
				}
				if(tipModel == 'GBM'){
					if(class(model) == 'gbm'){
						napoved <- predict(model, featureMatrix, model$n.trees)
					} else {
						napoved <- predict(model, featureMatrix)
					}
				}
				if(tipModel == 'SVM'){
					if(class(model) == 'tune'){
						newdata <- data.frame(X = I(featureMatrix))
						napoved <- predict(model$best.model, featureMatrix)
					} else{
						newdata <- data.frame(X = I(featureMatrix))
						napoved <- predict(model, newdata)
					}
				}
				if(tipModel == 'RF'){
					napoved <- predict(model, featureMatrix)
				}
				return(napoved)
					
}

