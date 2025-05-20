# ##################################################################
# ####       Function to age the pop matrix
# ################################################################## 
AgingFunc<- function(population) {
  
  Age.smAd<-0.07
  Age.subAd<-0.30
  Age.juv<-0.5
  
  
  # Age the fish
  population[,7] <- round(population[,7] + (Age.smAd * population[,6]))
  population[,6]<-  round((population[,6]* (1-Age.smAd)) + (Age.subAd * population[,5]))
  population[,5] <- round((population[,5] * (1-Age.subAd)) + (Age.juv *  population[,4]))
  population[,4] <- round((population[,4] * (1-Age.juv)) +  population[,3])
  population[,3] <-  population[,2]
  population[,2] <-  population[,1]
  population[,1] <- 0  # New Age0 fish will be added separately
  
  return(population)
}
#AgingFunc(population=st.fish)
