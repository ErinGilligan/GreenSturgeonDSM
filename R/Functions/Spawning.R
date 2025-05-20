#This is a reproductive success function
#Takes in small and large adults from the multidimensional array in their spawning trib
#It returns the number of larvae to be produced in each trib
#spawnHab can be used to cut egg production off if we decide to do it here instead of adults

spawn <- function(smAd, lgAd, discharge, passage, temp, Egg.S, hab.cap, prev.pass, stochastic) {
  
  #First apply passage to Feather Spawners
  smAd[2]<-if (stochastic){rbinom(1,smAd[2],passage)} else {floor((smAd[2])*passage)}
  lgAd[2]<-if (stochastic){rbinom(1,lgAd[2],passage)} else {floor((lgAd[2])*passage)}
  
  smAd[2]<-prev.pass[1]+smAd[2]
  lgAd[2]<-prev.pass[2]+lgAd[2]
  
  male_ratio<-1.38 / (1 + 1.38) #Van Eenennaam et al. 2006 1:1.38 spawning sex ratio
  female.ratio <- if(stochastic) {
    rbeta(1, 100 * (1 - male_ratio), 100 * male_ratio)} else {1-male_ratio} # randomized sex ratio or 0.579
  smfemales <- if (stochastic){rbinom(1,smAd,female.ratio)} else {floor(smAd * female.ratio)}
  lgfemales <- if (stochastic){rbinom(1,lgAd,female.ratio)} else {floor(lgAd * female.ratio)}
  
  Sm.eggs <- numeric(5)
  Lg.eggs <- numeric(5)
  
  for(i in 1:5) {
    # Check if discharge and temperature are within the specified ranges for each index
    if (discharge[i] < 3496 || discharge[i] > 40717 || temp[i] < 9.6 || temp[i] > 17.6) {
      Sm.eggs[i] <- 0
      Lg.eggs[i] <- 0
    } else {
      if (stochastic) {
        Sm.eggs[i] <- round(sum(runif(smfemales[i], 39600, 130278)))
        Lg.eggs[i] <- round(sum(runif(lgfemales[i], 130278, 242000)))
      } else {
        Sm.eggs[i] <- smfemales[i] * 84939
        Lg.eggs[i] <- lgfemales[i] * 186139
      }
    }
  }
  
  Egg.prod <- Sm.eggs + Lg.eggs
  
  for(i in 1:5) {
    if(hab.cap[i] == "over") {Egg.prod[i] = 0}
  }
  
  N.larvae <- c()
  for(i in 1:5) {
    N.larvae[i] <- if (stochastic) {
      rbinom(1, size = Egg.prod[i], prob = Egg.S[i])
    } else {
      floor(Egg.prod[i] * Egg.S[i])
    }
  }
  
  return(list(N.larvae,
              smAd[2],
              lgAd[2]))
}
#spawn(smAd=c(100,20,10,0,3),lgAd=c(50,50,50,50,50),passage=0.35,discharge=c(5000,5000,5000,5000,5000),temp=c(10,14,10,10,10),
#           Egg.S=c(0.6,0.6,0.8,0.4,0.3),hab.cap=c("under","under","under","under","under"),prev.pass=1,stochastic=0)


##Graphs for SIT
# # Set up the data
# discharge <- seq(0, 45000, by = 100)
# temperature <- seq(5, 25, by = 0.1)
# 
# # Create a matrix to store spawning conditions
# spawning <- outer(discharge, temperature, function(d, t) {
#   ifelse(d >= 3496 & d <= 40717 & t >= 9.6 & t <= 17.6, 1, 0)
# })
# 
# # Set up the plot
# plot(NULL, xlim = range(discharge), ylim = range(temperature),
#      xlab = "Discharge (cfs)", ylab = "Temperature (°C)",
#      main = "Green Sturgeon Spawning Conditions")
# 
# # Add color to the plot
# image(discharge, temperature, spawning, add = TRUE, col = c("light gray", "dark gray"))
# 
# # Add a legend
# legend("topright", legend = c("Spawning Allowed", "No Spawning"),
#        fill = c("dark gray", "light gray"), bty = "n")
# 
# # Add grid lines
# grid()
# 
# # Add text to highlight the exact ranges
# text(x = mean(range(discharge)), y = max(temperature) - 1,
#      labels = "Discharge: 3496-40717 cfs", adj = 0.5)
# text(x = mean(range(discharge)), y = min(temperature) + 1,
#      labels = "Temperature: 9.6-17.6 °C", adj = 0.5)
