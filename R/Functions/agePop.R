# ##################################################################
# ####       Function to age the pop matrix
# ################################################################## 
AgingFunc<- function(population) {
  Age.smAd<-0.07
  Age.subAd<-0.1
  Age.juv<-0.5 #This means they're averaging 2 years here
  
  # Age the fish
  aging_smAd_to_lgAd <- floor(Age.smAd * population[,6])
  population[,7] <- population[,7] + aging_smAd_to_lgAd
  
  aging_subAd_to_smAd <- floor(Age.subAd * population[,5])
  population[,6] <- population[,6] - aging_smAd_to_lgAd + aging_subAd_to_smAd
  
  aging_juv_to_subAd <- floor(Age.juv * population[,4])
  population[,5] <- population[,5] - aging_subAd_to_smAd + aging_juv_to_subAd
  
  # Age 2 to juvenile is simple - all fish move up
  population[,4] <- population[,4] - aging_juv_to_subAd + population[,3]
  
  # Straightforward aging for the younger age classes
  population[,3] <- population[,2]
  population[,2] <- population[,1]
  population[,1] <- 0  # New Age0 fish will be added separately
  
  return(population)
}
#AgingFunc(population=st.fish)
