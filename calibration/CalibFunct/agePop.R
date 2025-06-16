# ##################################################################
# ####       Function to age the pop matrix
# ################################################################## 
AgingFunc<- function(population,mode="sim",int=NULL) {
  
  if (mode=="cal") {Age.smAd=int[1]} else {Age.smAd=0.16}
  if (mode=="cal") {Age.subAd=int[2]} else {Age.smAd=0.28}
  if (mode=="cal") {Age.juv=int[3]} else {Age.juv=0.48}
 
  
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
