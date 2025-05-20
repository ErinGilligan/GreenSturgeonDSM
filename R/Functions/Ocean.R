#######################################################
#####Ocean Survival Submodel
######################################################

Ocean.Surv<-function(fish,decis,stochastic){

  ##Ocean Survival 
  nat.S<-if (stochastic){rbeta(1,36.25,6.62) } else {0.90}
  #Annual bycatch
  #do they get caught as bycatch?Counts by doukakis and pop by duldey et al. to get ratio
  bycatch.risk<-if (stochastic){rnorm(1,0.129,0.06)} else {0.129}
  bycatch.S<- if (stochastic){rbeta(1,36.79,6.93)} else {0.79}
  reduce.bycatch<-if (stochastic){rbeta(1,24.23,2.57)} else {0.84}
  elim.bycatch<-0.99
  
  bycatch.S <- if (18 %in% decis) {
    reduce.bycatch
  } else if (37 %in% decis) {
    elim.bycatch
  } else {
    bycatch.S
  }
  
nat.surv<-if (stochastic){
 rbinom(length(fish),fish,nat.S)}else {round(fish*nat.S)}

bycatch<-if (stochastic){
  rbinom(length(nat.surv),nat.surv,bycatch.risk)}else {round(nat.surv*bycatch.risk)}

bycatch.mort<-if (stochastic){
  rbinom(length(bycatch),bycatch,(1-bycatch.S))} else {round(bycatch*(1-bycatch.S))}

n.surv<-nat.surv-bycatch.mort
  
  return(n.surv)
}

#Ocean.Surv(fish=st.fish[14,],decis=37,stochastic=0)

