#######################################################
#####Ocean Survival Submodel
######################################################

Ocean.Surv<-function(fish,stochastic,decis,parms,incr){

  nat.S<-0.83; if(sum(parms == "natural.S")) nat.S<- qbeta(incr [parms=="natural.S"], 36.25,6.62)
  bycatch.risk<-0.129; if(sum(parms == "bycatch.risk")) bycatch.risk<-qbeta(incr [parms=="bycatch.risk"], 2.14,19.87)
  bycatch.S<-0.79; if(sum(parms == "bycatch.S")) bycatch.S<-qbeta(incr [parms=="bycatch.S"], 36.79,6.93)
  reduce.bycatch<-if (stochastic){rbeta(1,32.343,2.501)} else {0.84}
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

#Ocean.Surv(fish=c(0,0,100,500,1000,1000,800),stochastic=0,decis=19,
#           parms=c("natural.S","bycatch.S"),incr=c(0.1,0.1))

