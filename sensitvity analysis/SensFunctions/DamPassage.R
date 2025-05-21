SunsetPassage<-function(flow,year,incr,parms){
  
  if (year %in% 1:3){ #2025-2028; 2028 is estimated dam removal completion

  B0 = -7.4138 ; if(sum(parms == "sunset.int")) B0<-qnorm(incr[parms == "sunset.int"], B0, abs(B0*0.2))
  B1 = 0.0013; if(sum(parms == "sunset")) B1<- qnorm(incr[parms == "sunset"], B1, abs(B1*0.1))
  
  Pass.sunset<-1/(1+exp(-(B0 + B1*flow)))
} else {Pass.sunset<-1}
  
  return(Pass.sunset)
}
#SunsetPassage(flow=Qmean[19,month,yr],year=yr)
