#######################################################
#####     Habitat Capacity submodel
######################################################

#For spawning fish require 7-14 sqm per fish.
#Estimated from Wyman et al. 2017 paper
#For sensitivity analysis I'm going out to 48sqm based on a lake sturgeon paper 
#Fortin et al. 2002 in Collier et al. 2022

Capacity.sub<-function(spawners,flow,hab,stochastic){
  if(stochastic){habit=runif(1,4.6,14)}else{habit=9.3}
  #assign how many sq meters per fish and multiply by the number of fish in the section
  capacity<- c() #create an empty vector to be filled
  for(i in 1:5) {
    capacity[i]<-sum(spawners[i,])*(habit)}
  
  
  
  # predict spawning using linear approximation
  pred.spawn.UpSac=approx(x=hab$UpSac$Q..cfs,y=hab$UpSac$Spawning_sq.m, 
                          xout=flow[1], rule=2)$y
  pred.spawn.Feather=approx(x=hab$Feather$Q..cfs,y=hab$Feather$Spawning_sq.m, 
                            xout=flow[2], rule=2)$y
  pred.spawn.Yuba=approx(x=hab$Yuba$Q..cfs,y=hab$Yuba$Spawning_sq.m, 
                         xout=flow[3], rule=2)$y
  pred.spawn.Bear=approx(x=hab$Bear$Q..cfs,y=hab$Bear$Spawning_sq.m, 
                         xout=flow[4], rule=2)$y
  pred.spawn.SJ=approx(x=hab$SJ$Q..cfs,y=hab$SJ$Spawning_sq.m, 
                       xout=flow[5], rule=2)$y
  cap<- c()
  if (capacity[1]>pred.spawn.UpSac){
    cap[1]<-c("over")} else{cap[1]<-c("under")}
  if (capacity[2]>pred.spawn.Feather){
    cap[2]<-c("over")} else{cap[2]<-c("under")}
  if (capacity[3]>pred.spawn.Yuba){
    cap[3]<-c("over")} else{cap[3]<-c("under")}
  if (capacity[4]>pred.spawn.Bear){
    cap[4]<-c("over")} else{cap[4]<-c("under")}
  if (capacity[5]>pred.spawn.SJ){
    cap[5]<-c("over")} else{cap[5]<-c("under")}
  
  return(cap)
}

#Capacity.sub(spawners=st.fish[1:5,6:7],flow=Qmean[tribs2use,month,yr],
#             hab=Hab,stochastic=0)


#######################################################
#####     other life stages carrying capacity
######################################################

Capacity.River<-function(num.fish,flow,hab,stochastic){
  
  if(stochastic){habit=runif(1,7,14)}else{habit=10.5}
  #assign how many sq meters per fish and multiply by the number of fish in the section
  capacity<- c() #create an empty vector to be filled
  for(i in 1:3) {
    capacity[i]<-sum(num.fish[i,])*(habit)}
  
  
  
  # predict holding habitat using linear approximation
  pred.hold.UpMid=approx(x=hab$UpMidSac$Q..cfs,y=hab$UpMidSac$RiverUse_sq.m, 
                      xout=flow[6], rule=2)$y
  pred.hold.LowMid=approx(x=hab$MidSac$Q..cfs,y=hab$MidSac$RiverUse_sq.m, 
                         xout=flow[7], rule=2)$y
  pred.hold.Low=approx(x=hab$LowSac$Q..cfs,y=hab$LowSac$RiverUse_sq.m, 
                         xout=flow[8], rule=2)$y
  
  cap<- c()
  if (capacity[1]>pred.hold.UpMid){
    cap[1]<-c("over")} else{cap[1]<-c("under")}
  if (capacity[2]>pred.hold.LowMid){
    cap[2]<-c("over")} else{cap[2]<-c("under")}
  if (capacity[3]>pred.hold.Low){
    cap[3]<-c("over")} else{cap[3]<-c("under")}
  
  return(cap)
}

#Make this a part of juvenile movement model
#Capacity.holding(num.fish=st.fish[6,1:4],flow=Qmean[rivs,month,yr],
#             hab=Hab,stochastic=0)
