SunsetPassage<-function(flow,year){
  
  if (year %in% 1:3){ #2025-2028; 2028 is estimated dam removal completion

  B0 = -7.4138 #Feather
  B1 = 0.0013 #Feather
  
  Pass.sunset<-1/(1+exp(-(B0 + B1*flow)))
} else {Pass.sunset<-1}
  
  return(Pass.sunset)
}
#SunsetPassage(flow=Qmean[19,month,yr],year=yr)
