#This is a reproductive success function
#Takes in small and large adults from the multidimensional array in their spawning trib
#It returns the number of larvae to be produced in each trib
#spawnHab can be used to cut egg production off if we decide to do it here instead of adults
spawn<-function(smAd, lgAd, discharge, passage, temp, Egg.S, hab.cap, prev.pass, stochastic,parms,incr){
 
X.eggSm<-84939; if(sum(parms == "sm.Fecund")) X.eggSm<- qunif(incr [parms=="sm.Fecund"],39600,130278)
X.eggLg<-186139; if(sum(parms == "lg.Fecund")) X.eggLg<- qunif(incr [parms=="lg.Fecund"],130278,242000)
  


#First apply passage to Feather Spawners
smAd[2]<-if (stochastic){rbinom(1,smAd[2],passage)} else {floor((smAd[2])*passage)}
lgAd[2]<-if (stochastic){rbinom(1,lgAd[2],passage)} else {floor((lgAd[2])*passage)}

smAd[2]<-prev.pass[1]+smAd[2]
lgAd[2]<-prev.pass[2]+lgAd[2]

male_ratio<-1.38 / (1 + 1.38) ; if(sum(parms == "sex.ratio")) male.ratio<- qbeta(incr [parms=="sex.ratio"],100 * (1 - male_ratio),100 * male_ratio)
female.ratio <- if(stochastic) {
  rbeta(1, 100 * (1 - male_ratio), 100 * male_ratio)} else {1-male_ratio} # randomized sex ratio or 0.579
smfemales <- if (stochastic){rbinom(1,smAd,female.ratio)} else {floor(smAd * female.ratio)}
lgfemales <- if (stochastic){rbinom(1,lgAd,female.ratio)} else {floor(lgAd * female.ratio)}

Sm.eggs <- numeric(5)
Lg.eggs <- numeric(5)

# Adding discharge lower limit sensitivity
X.dischargeLow <- 3496; if(sum(parms == "SpawnQ.low")) X.dischargeLow <- qunif(incr[parms=="SpawnQ.low"], 2000, 5000)

# Adding discharge upper limit sensitivity
X.dischargeHigh <- 40717; if(sum(parms == "SpawnQ.high")) X.dischargeHigh <- qunif(incr[parms=="SpawnQ.high"], 35000, 45000)

# Adding temperature lower limit sensitivity
X.tempLow <- 9.6; if(sum(parms == "Spawntemp.low")) X.tempLow <- qunif(incr[parms=="Spawntemp.low"], 8.0, 11.0)

# Adding temperature upper limit sensitivity
X.tempHigh <- 17.6; if(sum(parms == "Spawntemp.high")) X.tempHigh <- qunif(incr[parms=="Spawntemp.high"], 16.0, 19.0)

# Then modify your original spawning code to use these parameters
for(i in 1:5) {
  # Check if discharge and temperature are within the specified ranges for each index
  if (discharge[i] < X.dischargeLow || discharge[i] > X.dischargeHigh || 
      temp[i] < X.tempLow || temp[i] > X.tempHigh) {
    Sm.eggs[i] <- 0
    Lg.eggs[i] <- 0
  } else {
    if (stochastic) {
      Sm.eggs[i] <- round(sum(runif(smfemales[i], 39600, 130278)))
      Lg.eggs[i] <- round(sum(runif(lgfemales[i], 130278, 242000)))
    } else {
      Sm.eggs[i] <- smfemales[i] * X.eggSm
      Lg.eggs[i] <- lgfemales[i] * X.eggLg
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

#try<-spawn(smAd=st.fish[1:5,6],lgAd=st.fish[1:5,7],Egg.S=Egg.S,hab.cap=cap.reached,
#           stochastic=0,parms=c("sex.ratio","lg.Fecund"),incr=c(0.1,1000))

