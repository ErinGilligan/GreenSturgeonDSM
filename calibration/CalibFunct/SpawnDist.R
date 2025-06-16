#######################################################
#####     Distribution of spawners in the Green sturgeon model
######################################################
  spawndist<-function(spawners,Qmeans,decis,stochastic){
    #B0 = -10.186 #bear
    #B1 = 0.0031 #bear
    F0 = -8.0655 #Feather
    F1 = 0.0022 #Feather
    Y0 = -9.8833 #Yuba
    Y1 = 0.0028 #Yuba
    Sj0 = -7.2096 #san Joaquin
    Sj1 = 0.0023 #San Joaquin
    
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
    
    if (spawnTribs[2]==TRUE){prop.Feath=0.15}else{prop.Feath=0}
    if (spawnTribs[3]==TRUE){prop.Yuba=0.1}else{prop.Yuba=0}
    if (spawnTribs[4]==TRUE){prop.Bear=0}else{prop.Bear=0}
    if (spawnTribs[5]==TRUE){prop.SJ=0.05}else{prop.SJ=0}
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
#spawndist(spawners=c(50,25),Qmeans=c(3000,6531,2000,1500),decis=1,stochastic=1)
  