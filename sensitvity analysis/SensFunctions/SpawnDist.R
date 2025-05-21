#######################################################
#####     Distribution of spawners in the Green sturgeon model
######################################################
spawndist<-function(spawners,Qmeans,decis,stochastic,parms,incr){
  #B0 = -10.186 #bear
  #B1 = 0.0031 #bear
  F0 = -8.0655; if(sum(parms == "Feather.spawnInt")) F0<-qnorm(incr[parms == "Feather.spawnInt"], F0, abs(F0*0.2))
  F1 = 0.0022; if(sum(parms == "FeatherQspawn")) F1<- qnorm(incr[parms == "FeatherQspawn"], F1, abs(F1*0.1))
  Y0 = -9.8833; if(sum(parms == "Yuba.spawnInt")) Y0<- qnorm(incr[parms == "Yuba.spawnInt"], Y0, abs(Y0*0.2))
  Y1 = 0.0028; if(sum(parms == "YubaQspawn")) Y1<- qnorm(incr[parms == "YubaQspawn"], Y1, abs(Y1*0.1))
  Sj0 = -7.2096; if(sum(parms == "SJ.spawnInt")) Sj0<- qnorm(incr[parms == "SJ.spawnInt"], Sj0, abs(Sj0*0.2))
  Sj1 = 0.0023; if(sum(parms == "SJQspawn")) Sj1<-qnorm(incr[parms == "SJQspawn"], Sj1, abs(Sj1*0.1))
  
#First calc the prob that spawners can enter each trib (Upper Sac is always 1)
#turn bear off
  Spawn.Bear<-0 #1/(1+exp(-(B0 + B1*Qmeans[1])))
  Spawn.Feath<-1/(1+exp(-(F0 + F1*Qmeans[2])))
  if (Qmeans[3]>=Qmeans[2]) {Spawn.Yuba<-1/(1+exp(-(Y0 + Y1*Qmeans[3])))}else {Spawn.Yuba=0}
  Spawn.SJ<-1/(1+exp(-(Sj0 + Sj1*Qmeans[4])))
  Spawn.Upsac<-1

  if (20 %in% decis) {Spawn.Yuba<-1} else {Spawn.Yuba<-Spawn.Yuba}

  #Bernoulli coin flip based on probs
  if (stochastic){spawnTribs<-c(rbinom(1,1,Spawn.Upsac),rbinom(1,1,Spawn.Feath),rbinom(1,1,Spawn.Yuba),
                                rbinom(1,1,Spawn.Bear),rbinom(1,1,Spawn.SJ))
  }else{
    spawnTribs <- c(
      Spawn.Upsac >= 0.5,
      Spawn.Feath >= 0.5,
      Spawn.Yuba >= 0.5,
      Spawn.Bear >= 0.5,
      Spawn.SJ >= 0.5
    )
  }
#Proportion the fish
X.Feath=0.15; if(sum(parms == "SpawnProp.feather")) X.Feath<- qbeta(incr [parms=="SpawnProp.feather"], 1.61,17.11)
X.Yuba=0.1; if(sum(parms == "SpawnProp.yuba")) X.Yuba<- qbeta(incr [parms=="SpawnProp.yuba"], 1.37,15.8)
X.SJ=0.05; if(sum(parms == "SpawnProp.sj")) X.SJ<- qbeta(incr [parms=="SpawnProp.sj"], 1.37,15.8)

if (spawnTribs[2]==TRUE){prop.Feath=X.Feath}else{prop.Feath=0}
if (spawnTribs[3]==TRUE){prop.Yuba=X.Yuba}else{prop.Yuba=0}
if (spawnTribs[4]==TRUE){prop.Bear=0}else{prop.Bear=0}
if (spawnTribs[5]==TRUE){prop.SJ=X.SJ}else{prop.SJ=0}
prop.UpSac<-1-(prop.Feath+prop.Yuba+prop.Bear+prop.SJ)
distspawners<-c(prop.UpSac,prop.Feath,prop.Yuba,prop.Bear,prop.SJ)

#Sample in a multinomial
if (stochastic){sm.fishdist<-rmultinom(1,spawners[1],prob=distspawners)
lg.fishdist<-rmultinom(1,spawners[2],prob=distspawners)
distribfish<-cbind(sm.fishdist,lg.fishdist)
} else {
  sm.fishdist <- floor(spawners[1] * distspawners)
  lg.fishdist <- floor(spawners[2] * distspawners)
  
  # Adjust for rounding errors to ensure total matches input
  sm.fishdist[1] <- sm.fishdist[1] + (spawners[1] - sum(sm.fishdist))
  lg.fishdist[1] <- lg.fishdist[1] + (spawners[2] - sum(lg.fishdist))
  distribfish <- cbind(sm.fishdist, lg.fishdist)
}

return(distribfish)
  
}

# spawndist(spawners=spawners,Qmeans=Qmean[tribsDist,5,10],decis=2,
#           parms=c("SJQspawn","SpawnProp.sj"),incr=c(1,0.5))
