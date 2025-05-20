#################################################
### Packages and Data
################################################
library(EnvStats)
library(parallel)

#Load in Cal Sim 3
load("C:/Users/gilliger/Box/Erin Dissertation/SIT sturgeon models/Green Sturgeon Model code/CalSim3_metrics.rda")
#flows
Qmean<-35.31468*(CalSim3_metrics$mean_monthly_flow) #turn from cms to cfs
#Temps
stream_temp<-CalSim3_metrics$avg_temp
load("C:/Users/gilliger/Box/Erin Dissertation/SIT sturgeon models/Green Sturgeon Model code/DSMtemp/delta_temperature.rda")
#Water year type
WY<-CalSim3_metrics$WaterYearType

#require(DSMtemperature)
load("C:/Users/gilliger/Box/Erin Dissertation/SIT sturgeon models/Green Sturgeon Model code/DSMtemp/stream_temperature.rda")
load("C:/Users/gilliger/Box/Erin Dissertation/SIT sturgeon models/Green Sturgeon Model code/DSMtemp/delta_temperature.rda")
stream_temp<-stream_temperature$biop_itp_2018_2019

#Load in all the Green sturgeon habitat data
setwd("C:/Users/gilliger/Box/Erin Dissertation/SIT sturgeon models/Green Sturgeon Model code/data")
UpSacHab <- read.delim("habitatVsFlow_Green Sturgeon_Keswick_to_Red_Bluff.txt", header=T)
UpMidSacHab <- read.delim("habitatVsFlow_Green Sturgeon_Red_Bluff_to_Wilkins_Slough.txt", header=T)
MidSacHab <- read.delim("habitatVsFlow_Green_Sturgeon_Wilkins_Slough_to_American_River.txt", header=T)
LowSacHab <- read.delim("habitatVsFlow_Green_Sturgeon_American_River_to_Freeport.txt", header=T)
FeatherHab <- read.csv("habitatVsFlow_Green_Sturgeon_FeatherRiver.csv")
YubaHab <- read.csv("habitatVsFlow_Green_Sturgeon_YubaRiver.csv")
BearHab <- read.csv("habitatVsFlow_Green_Sturgeon_BearRiver.csv")
SJHab <- read.csv("habitatVsFlow_Green_Sturgeon_SJRiver.csv")
Hab<-list(UpSac=UpSacHab,UpMidSac=UpMidSacHab,MidSac=MidSacHab,LowSac=LowSacHab,
          Feather=FeatherHab,Yuba=YubaHab,Bear=BearHab,SJ=SJHab)

#Load in the number of diversions in each river section
require(DSMhabitat)
ls("package:DSMhabitat")
diversions<-contact_points
dltadivs<-delta_contact_points
pred<-prop_high_predation

setwd("C:/Users/gilliger/Box/Erin Dissertation/SIT sturgeon models/Green Sturgeon Model code/data")
entrain<- read.csv("EntrainmentProbs.csv")
entrain<-rbind(0,entrain)

setwd("C:/Users/gilliger/Box/Erin Dissertation/SIT sturgeon models/Green Sturgeon Model code/CalibFunct")
source("StartPopSim.R")
source("Movement.R")
source("HabCapacity.R")
source("Spawning.R")
source("Survival.R")
source("SpawnDist.R")
source("stranding.R")
#source("Entrainment.R")
source("Old.Entrainment.R")
source("Ocean.R")
source("agePop.R")
source("setArray.R")
source("CARRYINGCAP.R")
source("DamPassage.R")

tribs2use<-c(1,19,20,18,31)
tribsDist<-c(18:20,31)


#################################################
### Begin Population Function
################################################

n.init<-StartPop(n.pop=10000,maxAge=34,Natural.S=0.85)
N0<-round(matrix(c(n.init$n.age0*4,
                   n.init$n.age1*4,
                   n.init$n.age2*4,
                   n.init$n.juvs*4,
                   n.init$n.sub+n.init$n.marineJ,
                   n.init$n.sm.ad,
                   n.init$n.lg.ad),ncol=1))
# ### Set up Array
st.fish<-set.array(N0)

#Reset diversions
diversions<-contact_points
dltadivs<-delta_contact_points

#reset habitat
Hab<-list(UpSac=UpSacHab,UpMidSac=UpMidSacHab,MidSac=MidSacHab,LowSac=LowSacHab,
          Feather=FeatherHab,Yuba=YubaHab,Bear=BearHab,SJ=SJHab)


#################################################
### Begin Population Function
################################################
# run.scenarios<-function(decis.X,runz,years,stochastic,mode,vect){
# #stochastic=0
# #mode="cal"
# #vary="Nothing"
# #pctil=1
#   
#   holdr<-NULL #placeholder for all simulations
#   for(z in 1:runz){
#     
#     # #Get initial population of fish
#     # #I created these to be similar to Dudley et al. 2024 some reason younger ages are
#     # #highly under estimated so a multiplier was added to create a similar number but uses
#     # #the same strucutre as the neg binomial predicted. the study says 10,000 fish
#     # #but does not include larvae (0.0006 survival from egg to age 0)
#     n.init<-StartPop(n.pop=10000,maxAge=34,Natural.S=0.85)
#     N0<-round(matrix(c(n.init$n.age0*4,
#                        n.init$n.age1*4,
#                        n.init$n.age2*4,
#                        n.init$n.juvs*4,
#                        n.init$n.sub+n.init$n.marineJ,
#                        n.init$n.sm.ad,
#                        n.init$n.lg.ad),ncol=1))
#     # ### Set up Array
#     st.fish<-set.array(N0)
#     
#     #Reset diversions
#     diversions<-contact_points
#     dltadivs<-delta_contact_points
#     
#     #reset habitat
#     Hab<-list(UpSac=UpSacHab,UpMidSac=UpMidSacHab,MidSac=MidSacHab,LowSac=LowSacHab,
#               Feather=FeatherHab,Yuba=YubaHab,Bear=BearHab,SJ=SJHab)
#     
#################################################
### Annual dynamics begins begins here 
################################################
run.all<-function(vect=NULL,decis.X,mode='sim',stochastic){
  TARGET = c(1596, 1172, 1172, 2172, 2442, 1437, 1008) #Dudley et al.2024
  SSE=0
obj<-NULL
for(yr in 1:20){
  if (decis.X[yr] %in% c(2)) stream_temp[19,3:6,yr]<-pmax(stream_temp[19,3:6,yr],14) #incubation months in Feather
  if (decis.X[yr] %in% c(3)) stream_temp[19,3:10,yr]<-pmax(stream_temp[19,3:10,yr],16) #rearing months in Feather
  if (decis.X[yr] %in% c(4)) stream_temp[31,3:6,yr]<-pmin(stream_temp[31,3:6,yr],19.9) #incubation SJ
  if (decis.X[yr] %in% c(5)) stream_temp[31,3:10,yr]<-pmin(stream_temp[31,3:10,yr],19.9) #rearing SJ
  if (decis.X[yr] %in% c(6)) diversions[1]<-round(diversions[1]*.90) #screen up sac
  if (decis.X[yr] %in% c(7)) diversions[19]<-round(diversions[19]*.90) #screen Feather
  if (decis.X[yr] %in% c(8)) diversions[20]<-round(diversions[20]*.90) #screen yuba
  if (decis.X[yr] %in% c(9)) diversions[31]<-round(diversions[31]*.90) #screen SJ
  if (decis.X[yr] %in% c(10)) diversions[16]<-round(diversions[16]*.90) #Screen UpMid
  if (decis.X[yr] %in% c(11)) diversions[21]<-round(diversions[21]*.90) #Screen Low Mid
  if (decis.X[yr] %in% c(12)) diversions[24]<-round(diversions[24]*.90) #Screen lower sac
  if (decis.X[yr] %in% c(13)) dltadivs[1]<-round(dltadivs[1]*.90) #Screen North delta
  if (decis.X[yr] %in% c(14)) dltadivs[2]<-round(dltadivs[2]*.90) #screen south delta
  if (decis.X[yr] %in% c(17)) Hab$Feather$Spawning_sq.m<-Hab$Feather$Spawning_sq.m+8093.71 #add 2 acres of spawning habitat to feather
  if (decis.X[yr] %in% c(19)) Qmean[19,3:6,yr]<-pmax(Qmean[19,3:6,yr],6000)
  if (decis.X[yr] %in% c(20)) Qmean[20,3:6,yr]<-pmax(Qmean[20,3:6,yr],6000)
  if (decis.X[yr] %in% c(21)) Qmean[31,1:4,yr]<-pmax(Qmean[31,1:4,yr],1500)
  if (decis.X[yr] %in% c(22)) Qmean[21,1:3,yr]<-pmax(Qmean[21,1:3,yr],17700)
  if (decis.X[yr] %in% c(22)) Qmean[24,1:3,yr]<-pmax(Qmean[24,1:3,yr],31100)
  if (decis.X[yr] %in% c(23)) Hab$Yuba$Spawning_sq.m<-Hab$Yuba$Spawning_sq.m+12000
  if (decis.X[yr] %in% c(23)) Hab$Yuba$RiverUse_sq.m<-Hab$Yuba$RiverUse_sq.m+12000 #was told 12 rkm-Daguerre Dam
  if (decis.X[yr] %in% c(24)) stream_temp[1,3:6,yr]<-pmax(stream_temp[1,3:6,yr],14) #incubation UpSac temps
  if (decis.X[yr] %in% c(25)) stream_temp[20,3:6,yr]<-pmax(stream_temp[20,3:6,yr],14) #incubation Yuba
  if (decis.X[yr] %in% c(26)) stream_temp[1,3:10,yr]<-pmax(stream_temp[1,3:10,yr],16) #rear UpSac temps
  if (decis.X[yr] %in% c(27)) stream_temp[16,3:10,yr] <- pmin(pmax(stream_temp[16,3:10,yr], 16), 19.9) #UpMid rear
  if (decis.X[yr] %in% c(28)) Hab$UpSac$Spawning_sq.m<-Hab$UpSac$Spawning_sq.m+8093.71 #2 acres
  if (decis.X[yr] %in% c(29)) Hab$UpSac$RiverUse_sq.m<-Hab$UpSac$RiverUse_sq.m+8093.71 #2 acres
  if (decis.X[yr] %in% c(30)) Hab$UpMidSac$RiverUse_sq.m<-Hab$UpMidSac$RiverUse_sq.m+8093.71 #2 acres
  if (decis.X[yr] %in% c(31)) Qmean[19,3:6,yr]<-pmax(Qmean[19,3:6,yr],5000)
  if (decis.X[yr] %in% c(32)) Qmean[20,3:6,yr]<-pmax(Qmean[20,3:6,yr],5000)
  if (decis.X[yr] %in% c(33)) Hab$Yuba$Spawning_sq.m<-Hab$Yuba$Spawning_sq.m+8093.71 #2 acres
  if (decis.X[yr] %in% c(34)) Hab$SJ$Spawning_sq.m<-Hab$SJ$Spawning_sq.m+8093.71 #2 acres
  if (decis.X[yr] %in% c(35)) Hab$SJ$RiverUse_sq.m<-Hab$SJ$RiverUse_sq.m+8093.71 #2 acres
  if (decis.X[yr] %in% c(36)) Hab$Feather$RiverUse_sq.m<-Hab$Feather$RiverUse_sq.m+8093.71 #2 acres ther$RiverUse_sq.m<-Hab$Feather$RiverUse_sq.m+12545.3 #equals 3.1 acres
  
  #Every year set up a cumulative distribution for juvenile outmigration
  cumdistrib<-dist.migrators(Qmean, yr)
  cumdistrib2<-matrix(0,nrow=5,ncol=12)
  cumdistrib2[1,10:12]<-cumdistrib$UpSac[1:3]
  cumdistrib2[1,1:3]<-cumdistrib$UpSac[4:6]
  cumdistrib2[2,10:12]<-cumdistrib$Feather[1:3]
  cumdistrib2[2,1:3]<-cumdistrib$Feather[4:6]
  cumdistrib2[3,10:12]<-cumdistrib$Yuba[1:3]
  cumdistrib2[3,1:3]<-cumdistrib$Yuba[4:6]
  cumdistrib2[4,10:12]<-cumdistrib$Bear[1:3]
  cumdistrib2[4,1:3]<-cumdistrib$Bear[4:6]
  cumdistrib2[5,10:12]<-cumdistrib$SJ[1:3]
  cumdistrib2[5,1:3]<-cumdistrib$SJ[4:6]
  rear.months <- unname(sapply(cumdistrib, function(river) sum(river < 0.99)))
  
  
for(month in 1:12){
  if(month==2){
    
    #Set up ocean small adults and draw a spawning pool for the bay
    st.fish[13,6]<-round(st.fish[14,6]* 0.25)
    st.fish[14,6]<-round(st.fish[14,6]* 0.75)
    # large adults
    st.fish[13,7]<-round(st.fish[14,7]* 0.25)
    st.fish[14,7]<-round(st.fish[14,7]* 0.75)  
    
    sm.spawners.spawn<-round(rtri(sum(st.fish[13,6]), min=3, max=6,mode=4.5))
    lg.spawners.spawn<-round(rtri(sum(st.fish[13,7]), min=3, max=6,mode=4.5))
    store.spawners<-c(length(sm.spawners.spawn),(length(lg.spawners.spawn)))
  }
  
  
    if(month == 3){

 #Assign routes and distribution of spawners
  spawnroute<-spawndist(spawners=c(sum(sm.spawners.spawn == 3),sum(lg.spawners.spawn == 3)),
                        Qmeans=Qmean[tribsDist,month,yr],decis=decis.X[yr])
      
  strand.success<-strand.sub(spawn.rt=spawnroute[1:4,],
              overtopYolo=CalSim3_metrics$gates_overtopped[month,yr,2],
              overtopSutter=CalSim3_metrics$gates_overtopped[month,yr,1],
              stochastic)
  
  #Fill in everybody
  spawners<-rbind(strand.success[[1]],spawnroute[5,])
  st.fish[1:5,6:7]<-st.fish[1:5,6:7]+spawners
  st.fish[8:9,6:7]<-strand.success[[2]]
  
  #Subtract the fish that initially left
  st.fish[13,6]<-st.fish[13,6]-sum(sm.spawners.spawn == 3) 
  st.fish[13,7]<-st.fish[13,7]-sum(lg.spawners.spawn == 3)
  
  #Apply carrying capacity
  cap.reached<-Capacity.sub(spawners=st.fish[1:5,6:7],flow=Qmean[tribs2use,month,yr],hab=Hab,stochastic)
  #Survival to age 0 production
      Egg.S<-Egg.Surv(avgT=stream_temp[tribs2use,month,yr],mode="cal",int=vect[1])
  #Passage at Sunset pumps
      pass<-SunsetPassage(flow=Qmean[19,month,yr],year=yr)
  #Spawn the fish-apply carrying capacity directly if any
      larvae<-spawn(smAd=st.fish[1:5,6],lgAd=st.fish[1:5,7],discharge=Qmean[tribs2use,month,yr],
                    passage=pass,temp=stream_temp[tribs2use,month,yr],Egg.S=Egg.S,
                    hab.cap=cap.reached,prev.pass=numeric(2),stochastic)
      # Create spawning occurrence indicator
      spawning_occurred_3 <- c(as.integer(larvae[[1]] > 0))
      #Larval to age 0 survival
      age0<-Larvae.Surv(larvae=larvae[[1]],avgT=stream_temp[tribs2use,month,yr],
                        pred=c("medium","high","low","medium","high"),mode="cal",int=vect[2],stochastic) 
      st.fish[1:5,1]<-st.fish[1:5,1]+age0 #put them in the multidimensional array r 
      
      #So I don't double spawn folks
      prev.smfish<-numeric(5)
      prev.smfish[1]<-st.fish[1, 6] * spawning_occurred_3[1]
      prev.smfish[2]<-(st.fish[2, 6] - larvae[[2]])  * spawning_occurred_3[2]
      prev.smfish[3:5]<-st.fish[3:5,6] * spawning_occurred_3[3:5]
      
      prev.lgfish<-numeric(5)
      prev.lgfish[1]<-st.fish[1, 7] * spawning_occurred_3[1]
      prev.lgfish[2]<-(st.fish[2, 7] - larvae[[3]])  * spawning_occurred_3[2]
      prev.lgfish[3:5]<-st.fish[3:5,7] * spawning_occurred_3[3:5]
      
      #keep track of the successful passers but don't double spawn
      passSunset<-c(larvae[[2]]*(1-spawning_occurred_3[2]),larvae[[3]]*(1-spawning_occurred_3[2]))
      
      #Apply a survival to stranded fish 
      extract.Strand<-strand.S(stranded=st.fish[8:9,6:7],decis=decis.X[yr],stochastic)
      st.fish[8:9,6:7]<-extract.Strand
    } 
    
    if(month == 4){
      
 #send the stranded out from previous month up first
    abandoned<-abandonment(stranded=st.fish[8:9,6:7],stochastic)
      st.fish[14,6]<-st.fish[14,6] + sum(abandoned[,1]) 
      st.fish[14,7]<-st.fish[14,7] + sum(abandoned[,2])   #put them back in the ocean
      st.fish[1,6]<- st.fish[1,6] + (sum(st.fish[8:9,6])-sum(abandoned[,1])) #up to spawn
      st.fish[1,7]<- st.fish[1,7] + (sum(st.fish[8:9,7])-sum(abandoned[,2]))
      st.fish[8:9,6:7]<- 0 #clear em out

  #Assign routes and distribution of spawners
      spawnroute<-spawndist(spawners=c(sum(sm.spawners.spawn == 4),sum(lg.spawners.spawn == 4)),
                            Qmeans=Qmean[tribsDist,month,yr],decis=decis.X[yr])
      
      strand.success<-strand.sub(spawn.rt=spawnroute[1:4,],
                                 overtopYolo=CalSim3_metrics$gates_overtopped[month,yr,2],
                                 overtopSutter=CalSim3_metrics$gates_overtopped[month,yr,1],
                                 stochastic)

      #Fill in everybody
      spawners<-rbind(strand.success[[1]],spawnroute[5,])
      st.fish[1:5,6:7]<-st.fish[1:5,6:7]+spawners
      st.fish[8:9,6:7]<-strand.success[[2]]
      
      #Subtract the fish that initially left
      st.fish[13,6]<-st.fish[13,6]-sum(sm.spawners.spawn == 4) 
      st.fish[13,7]<-st.fish[13,7]-sum(lg.spawners.spawn == 4)
      
#send age 0 upper sac to upper mid sac..#leave feather, yuba, bear,SJ in their tribs
st.fish[6,1]<-st.fish[6,1]+ round(st.fish[1,1]) #*Up.mig.prob0*Up.migS.0)
st.fish[1,1]<-0 #clear out previous age 0 because they all moved down
      
#Apply carrying capacity
  cap.reached<-Capacity.sub(spawners=st.fish[1:5,6:7],flow=Qmean[tribs2use,month,yr],hab=Hab,stochastic)
  #Survival to age 0 production
  Egg.S<-Egg.Surv(avgT=stream_temp[tribs2use,month,yr],mode="cal",int=vect[1])
  #Passage at Sunset pumps
  pass<-SunsetPassage(flow=Qmean[19,month,yr],year=yr)
  #Larvae
  larvae<-spawn(smAd=st.fish[1:5,6]-prev.smfish,lgAd=st.fish[1:5,7]-prev.lgfish,
                discharge=Qmean[tribs2use,month,yr],passage=pass,temp=stream_temp[tribs2use,month,yr],
                Egg.S=Egg.S,hab.cap=cap.reached,prev.pass=passSunset,stochastic)
  # Create spawning occurrence indicator
  spawning_occurred_4 <- c(as.integer(larvae[[1]] > 0))
  
  #Larval to age 0 survival
  age0<-Larvae.Surv(larvae=larvae[[1]],avgT=stream_temp[tribs2use,month,yr],
                    pred=c("medium","high","low","medium","high"),
                    stochastic, mode="cal",int=vect[2])  
  
  st.fish[1:5,1]<-st.fish[1:5,1]+age0 #put them in the multidimensional array 
  
  #So I don't double spawn folks
  prev.smfish2<-numeric(5)
  prev.smfish2[1]<-(st.fish[1, 6]- prev.smfish[1]) * spawning_occurred_4[1]
  prev.smfish2[2]<-(st.fish[2, 6] - prev.smfish[2] - larvae[[2]]) * spawning_occurred_4[2]
  prev.smfish2[3:5]<-(st.fish[3:5,6] - prev.smfish[3:5]) * spawning_occurred_4[3:5]
  
  prev.lgfish2<-numeric(5)
  prev.lgfish2[1]<-(st.fish[1, 7]- prev.smfish[1]) * spawning_occurred_4[1]
  prev.lgfish2[2]<-(st.fish[2, 7] - prev.smfish[2] - larvae[[2]]) * spawning_occurred_4[2]
  prev.lgfish2[3:5]<-(st.fish[3:5,7] - prev.smfish[3:5]) * spawning_occurred_4[3:5]
  
  passSunset<-c(larvae[[2]]*(1-spawning_occurred_4[2]),larvae[[3]]*(1-spawning_occurred_4[2])) #keep track of the successful passers
  
  # update for future iterations
  prev.smfish <- prev.smfish + prev.smfish2
  prev.lgfish <- prev.lgfish + prev.lgfish2
  
  #Apply a survival to stranded fish 
  extract.Strand<-strand.S(stranded=st.fish[8:9,6:7],decis=decis.X[yr],stochastic)
  st.fish[8:9,6:7]<-extract.Strand
    }
    
    if(month == 5){
      
      #send the stranded out from previous month up first
      abandoned<-abandonment(stranded=st.fish[8:9,6:7],stochastic)
      st.fish[14,6]<-st.fish[14,6] + sum(abandoned[,1]) 
      st.fish[14,7]<-st.fish[14,7] + sum(abandoned[,2])   #put them back in the ocean
      st.fish[1,6]<- st.fish[1,6] + (sum(st.fish[8:9,6])-sum(abandoned[,1])) #up to spawn
      st.fish[1,7]<- st.fish[1,7] + (sum(st.fish[8:9,7])-sum(abandoned[,2]))
      st.fish[8:9,6:7]<- 0 #clear em out
      
#Assign routes and distribution of spawners
  spawnroute<-spawndist(spawners=c(sum(sm.spawners.spawn == 5),sum(lg.spawners.spawn == 5)),
                        Qmeans=Qmean[tribsDist,month,yr],decis=decis.X[yr])
      
  strand.success<-strand.sub(spawn.rt=spawnroute[1:4,],
                             overtopYolo=CalSim3_metrics$gates_overtopped[month,yr,2],
                             overtopSutter=CalSim3_metrics$gates_overtopped[month,yr,1],
                             stochastic)
      
#Fill in everybody
  spawners<-rbind(strand.success[[1]],spawnroute[5,])
  st.fish[1:5,6:7]<-st.fish[1:5,6:7]+spawners
  st.fish[8:9,6:7]<-strand.success[[2]]
      
#Subtract the fish that initially left
  st.fish[13,6]<-st.fish[13,6]-sum(sm.spawners.spawn == 5) 
  st.fish[13,7]<-st.fish[13,7]-sum(lg.spawners.spawn == 5)
  
#send age 0 upper sac to upper mid sac..#leave feather, yuba, bear,SJ in their tribs
  st.fish[6,1]<-st.fish[6,1]+ round(st.fish[1,1]) #*Up.mig.prob0*Up.migS.0)
  st.fish[1,1]<-0 #clear out previous age 0 because they all moved down
  
#Apply carrying capacity
  cap.reached<-Capacity.sub(spawners=st.fish[1:5,6:7],flow=Qmean[tribs2use,month,yr],hab=Hab,stochastic)
  #Passage at Sunset pumps
  pass<-SunsetPassage(flow=Qmean[19,month,yr],year=yr)
  #Survival to age 0 production
  Egg.S<-Egg.Surv(avgT=stream_temp[tribs2use,month,yr],mode="cal",int=vect[1]) 
  larvae<-spawn(smAd=st.fish[1:5,6]-prev.smfish,lgAd=st.fish[1:5,7]-prev.lgfish,
                discharge=Qmean[tribs2use,month,yr],passage=pass,temp=stream_temp[tribs2use,month,yr],
                Egg.S=Egg.S,hab.cap=cap.reached,prev.pass=passSunset,stochastic)
  # Create spawning occurrence indicator
  spawning_occurred_5 <- c(as.integer(larvae[[1]] > 0))
  
  #Larval to age 0 survival
  age0<-Larvae.Surv(larvae=larvae[[1]],avgT=stream_temp[tribs2use,month,yr],
                    pred=c("medium","high","low","medium","high"),
                    stochastic, mode="cal",int=vect[2]) 
  st.fish[1:5,1]<-st.fish[1:5,1]+age0 #put them in the multidimensional array 
  
  #So I don't double spawn folks
  prev.smfish2<-numeric(5)
  prev.smfish2[1]<-(st.fish[1, 6]- prev.smfish[1]) * spawning_occurred_5[1]
  prev.smfish2[2]<-(st.fish[2, 6] - prev.smfish[2] - larvae[[2]]) * spawning_occurred_5[2]
  prev.smfish2[3:5]<-(st.fish[3:5,6] - prev.smfish[3:5]) * spawning_occurred_5[3:5]
  
  prev.lgfish2<-numeric(5)
  prev.lgfish2[1]<-(st.fish[1, 7]- prev.smfish[1]) * spawning_occurred_5[1]
  prev.lgfish2[2]<-(st.fish[2, 7] - prev.smfish[2] - larvae[[2]]) * spawning_occurred_5[2]
  prev.lgfish2[3:5]<-(st.fish[3:5,7] - prev.smfish[3:5]) * spawning_occurred_5[3:5]
  
  passSunset<-c(larvae[[2]]*(1-spawning_occurred_5[2]),larvae[[3]]*(1-spawning_occurred_5[2])) #keep track of the successful passers
  
  # update for future iterations
  prev.smfish <- prev.smfish + prev.smfish2
  prev.lgfish <- prev.lgfish + prev.lgfish2
  
  #Apply a survival to stranded fish 
  extract.Strand<-strand.S(stranded=st.fish[8:9,6:7],decis=decis.X[yr],stochastic)
  st.fish[8:9,6:7]<-extract.Strand
  }

    if(month == 6){
      
      #send the stranded out from previous month up first
      abandoned<-abandonment(stranded=st.fish[8:9,6:7],stochastic)
      st.fish[14,6]<-st.fish[14,6] + sum(abandoned[,1]) 
      st.fish[14,7]<-st.fish[14,7] + sum(abandoned[,2])   #put them back in the ocean
      st.fish[1,6]<- st.fish[1,6] + (sum(st.fish[8:9,6])-sum(abandoned[,1])) #up to spawn
      st.fish[1,7]<- st.fish[1,7] + (sum(st.fish[8:9,7])-sum(abandoned[,2]))
      st.fish[8:9,6:7]<- 0 #clear em out
      
#Assign routes and distribution of spawners
  spawnroute<-spawndist(spawners=c(sum(sm.spawners.spawn == 6),sum(lg.spawners.spawn == 6)),
                        Qmeans=Qmean[tribsDist,month,yr],decis=decis.X[yr])
      
  strand.success<-strand.sub(spawn.rt=spawnroute[1:4,],
                  overtopYolo=CalSim3_metrics$gates_overtopped[month,yr,2],
                  overtopSutter=CalSim3_metrics$gates_overtopped[month,yr,1],
                  stochastic)
      
      
#Fill in everybody
  spawners<-rbind(strand.success[[1]],spawnroute[5,])
  st.fish[1:5,6:7]<-st.fish[1:5,6:7]+spawners
  st.fish[8:9,6:7]<-strand.success[[2]]
      
#Subtract the fish that initially left
  st.fish[13,6]<-st.fish[13,6]-sum(sm.spawners.spawn == 6) 
  st.fish[13,7]<-st.fish[13,7]-sum(lg.spawners.spawn == 6)

#send age 0 upper sac to upper mid sac..#leave feather, yuba, bear,SJ in their tribs
  st.fish[6,1]<-st.fish[6,1]+ round(st.fish[1,1]) #*Up.mig.prob0*Up.migS.0)
  st.fish[1,1]<-0 #clear out previous age 0 because they all moved down
        
  #Apply carrying capacity
  cap.reached<-Capacity.sub(spawners=st.fish[1:5,6:7],flow=Qmean[tribs2use,month,yr],hab=Hab,stochastic)
  #Survival to age 0 production
  Egg.S<-Egg.Surv(avgT=stream_temp[tribs2use,month,yr],mode="cal",int=vect[1]) 
  #Passage at Sunset pumps
  pass<-SunsetPassage(flow=Qmean[19,month,yr],year=yr)
  #Larvae
  larvae<-spawn(smAd=st.fish[1:5,6]-prev.smfish,lgAd=st.fish[1:5,7]-prev.lgfish,
                discharge=Qmean[tribs2use,month,yr],passage=pass,temp=stream_temp[tribs2use,month,yr],
                Egg.S=Egg.S,hab.cap=cap.reached,prev.pass=passSunset,stochastic)
  # Create spawning occurrence indicator
  spawning_occurred_6 <- c(as.integer(larvae[[1]] > 0))
  
  #Larval to age 0 survival
  age0<-Larvae.Surv(larvae=larvae[[1]],avgT=stream_temp[tribs2use,month,yr],
                    pred=c("medium","high","low","medium","high"),stochastic,mode="cal",int=vect[2]) 
  st.fish[1:5,1]<-st.fish[1:5,1]+age0 #put them in the multidimensional array 
  
  
  #So I don't double spawn folks
  prev.smfish2<-numeric(5)
  prev.smfish2[1]<-(st.fish[1, 6]- prev.smfish[1]) * spawning_occurred_6[1]
  prev.smfish2[2]<-(st.fish[2, 6] - prev.smfish[2] - larvae[[2]]) * spawning_occurred_6[2]
  prev.smfish2[3:5]<-(st.fish[3:5,6] - prev.smfish[3:5]) * spawning_occurred_6[3:5]
  
  prev.lgfish2<-numeric(5)
  prev.lgfish2[1]<-(st.fish[1, 7]- prev.smfish[1]) * spawning_occurred_6[1]
  prev.lgfish2[2]<-(st.fish[2, 7] - prev.smfish[2] - larvae[[2]]) * spawning_occurred_6[2]
  prev.lgfish2[3:5]<-(st.fish[3:5,7] - prev.smfish[3:5]) * spawning_occurred_6[3:5]
  
  passSunset<-c(larvae[[2]]*(1-spawning_occurred_6[2]),larvae[[3]]*(1-spawning_occurred_6[2])) #keep track of the successful passers
  
  # update for future iterations
  prev.smfish <- prev.smfish + prev.smfish2
  prev.lgfish <- prev.lgfish + prev.lgfish2
  
  #Apply a survival to stranded fish 
  extract.Strand<-strand.S(stranded=st.fish[8:9,6:7],decis=decis.X[yr],stochastic)
  st.fish[8:9,6:7]<-extract.Strand
  } 
    
if(month == 7){ #not sending spawners up just clearing out any delayed spawners due to entrainment if there is any
  
  #send the stranded out from previous month up first
  abandoned<-abandonment(stranded=st.fish[8:9,6:7],stochastic)
  st.fish[14,6]<-st.fish[14,6] + sum(abandoned[,1]) 
  st.fish[14,7]<-st.fish[14,7] + sum(abandoned[,2])   #put them back in the ocean
  st.fish[1,6]<- st.fish[1,6] + (sum(st.fish[8:9,6])-sum(abandoned[,1])) #up to spawn
  st.fish[1,7]<- st.fish[1,7] + (sum(st.fish[8:9,7])-sum(abandoned[,2]))
  st.fish[8:9,6:7]<- 0 #clear em out
  
#send age 0 upper sac to upper mid sac..#leave feather, yuba, bear,SJ in their tribs
  st.fish[6,1]<-st.fish[6,1]+ round(st.fish[1,1]) #*Up.mig.prob0*Up.migS.0)
  st.fish[1,1]<-0 #clear out previous age 0 because they all moved down
  
  #Apply carrying capacity
  cap.reached<-Capacity.sub(spawners=st.fish[1:5,6:7],flow=Qmean[tribs2use,month,yr],hab=Hab,stochastic)
  #Survival to age 0 production
  Egg.S<-Egg.Surv(avgT=stream_temp[tribs2use,month,yr],mode="cal",int=vect[1]) 
  #Passage at Sunset pumps
  pass<-SunsetPassage(flow=Qmean[19,month,yr],year=yr)
  #Larvae
  larvae<-spawn(smAd=st.fish[1:5,6]-prev.smfish,lgAd=st.fish[1:5,7]-prev.lgfish,
                discharge=Qmean[tribs2use,month,yr],passage=pass,temp=stream_temp[tribs2use,month,yr],
                Egg.S=Egg.S,hab.cap=cap.reached,prev.pass=passSunset,stochastic)
  # Create spawning occurrence indicator
  spawning_occurred_7 <- c(as.integer(larvae[[1]] > 0))
  
  #Larval to age 0 survival
  age0<-Larvae.Surv(larvae=larvae[[1]],avgT=stream_temp[tribs2use,month,yr],
                    pred=c("medium","high","low","medium","high"),
                    stochastic,mode="cal",int=vect[2])  
  st.fish[1:5,1]<-st.fish[1:5,1]+age0 #put them in the multidimensional array 
} 
  
  if (month==8){
    #send last of age 0 fish to the upper mid sac
    st.fish[6,1]<-st.fish[6,1]+ round(st.fish[1,1]) #*Up.mig.prob0*Up.migS.0)
    st.fish[1,1]<-0 #clear out previous age 0 because they all moved down
    
    #Kick Early adult outmigration adults out
    prop_early.up<-Adult.Early(Qmean[1,mean(5:7),1],trib=c("UpSac"),stochastic)
    prop_early.F<-Adult.Early(Qmean[19,mean(5:7),1],trib=c("Feather"),stochastic)
    prop_early.Y<-Adult.Early(Qmean[20,mean(5:7),1],trib=c("Yuba"),stochastic)
    prop_early.B<-Adult.Early(Qmean[18,mean(5:7),1],trib=c("Bear"),stochastic)
    prop_early.SJ<-Adult.Early(Qmean[31,mean(5:7),1],trib=c("SJ"),stochastic)
    prop_early<-c(prop_early.up,prop_early.F,prop_early.Y,prop_early.B,prop_early.SJ)
    # Calculate number of early migrants
    early_migrants.sm<-c()
    for(i in 1:5){
      early_migrants.sm[i]<-round(st.fish[i, 6] * prop_early[i])
    }
    early_migrants.lg<-c()
    for(i in 1:5){
      early_migrants.lg[i]<-round(st.fish[i, 7] * prop_early[i])
    }
    
    st.fish[14,6]<-st.fish[14,6]+sum(early_migrants.sm)
    st.fish[14,7]<-st.fish[14,7]+sum(early_migrants.lg)
    st.fish[1:5,6]<-st.fish[1:5,6]-early_migrants.sm
    st.fish[1:5,7]<-st.fish[1:5,7]-early_migrants.lg
    #Keep track of holding adults
    store.holders<-c(sum(st.fish[1:5,6:7]))
  }
  
for (month in c(4:9)){
  #apply entrainment mort to age 0 fish and age 1 and 2 in the lower sac
  #only occuring april to september of each year
  entrained<-entrain.sub(fish=st.fish[,1:3],prob.entrain=entrain[1:2],
                         n.div=diversions,stochastic)
  st.fish[1:10,1]<-pmax(0, st.fish[1:10,1]-entrained[[1]])
  st.fish[10,2]<-pmax(0,st.fish[10,2]-entrained[[2]])
  st.fish[10,3]<-pmax(0,st.fish[10,3]-entrained[[3]])
  #delta entrainment only apply to age 0-2 fish also entrainment vulnerability is unknown so
  #just setting it low for now
  NDelta.entrain<-delta.entrain(fish=st.fish[11,1:3], prob.entrain=entrain[1:2],
                                n.div=dltadivs[1],stochastic)
  st.fish[11,1:3]<-pmax(0,st.fish[11,1:3]-NDelta.entrain)
  SDelta.entrain<-delta.entrain(fish=st.fish[12,1:3],prob.entrain=entrain[1:2],
                                n.div=dltadivs[2],stochastic)
  st.fish[12,1:3]<-pmax(0,st.fish[12,1:3]-SDelta.entrain)
} 
  # Pre-compute common values
  month_yr_temps <- stream_temp[, month, yr]
  
  # Apply survival rates for tributaries (both juvenile and adult)
  trib_params <- list(
    Up = list(1, month_yr_temps[1]),
    Feather = list(2, month_yr_temps[19]),
    Yuba = list(3, month_yr_temps[20]),
    Bear = list(4, month_yr_temps[18]),
    SJ = list(5, month_yr_temps[31])
  )
  
  for (i in seq_along(trib_params)) {
    params <- trib_params[[i]]
    apply_juv_survival(params[[1]], params[[2]],mode="cal",int=vect[3])
    apply_adult_survival(i, params[[2]])
  }
  
  # Apply survival rates for Sac regions (juvenile only, as per original code)
  sac_regions <- list(
    UpMid = list(6, 16),
    LowMid = list(7, 21),
    Low = list(10, 24)
  )
  
  for (region in sac_regions) {
    apply_juv_survival(region[[1]], month_yr_temps[region[[2]]],mode="cal",int=vect[3])
  }
  
  # Apply Delta survival rates (juvenile only, as per original code)
  delta_regions <- list(
    N = list(11, 1),
    S = list(12, 2)
  )
  
  for (region in delta_regions) {
    apply_juv_survival(region[[1]], delta_temperature[month, yr, region[[2]]],
                       mode="cal",int=vect[4], delta = TRUE)
  }
  

  ####Movement###
  
  ########OUtmigrate juveniles from tribs (once month gets to 3 send all the leftovers down)
  
  for (month in c(10:12,1:3)){
    if (month %in% 1:3) {iter_yr <- yr + 1} else {iter_yr <- yr}
    
    # Estimate habitat for each tributary
    pred.juv.UpSac <- estimate_juv_habitat(Qmean[16,month,yr], Hab$UpSac)
    pred.juv.Feather <- estimate_juv_habitat(Qmean[19,month,yr], Hab$Feather)
    pred.juv.Yuba <- estimate_juv_habitat(Qmean[20,month,yr], Hab$Yuba)
    pred.juv.Bear <- estimate_juv_habitat(Qmean[18,month,yr], Hab$Bear)
    pred.juv.SJ <- estimate_juv_habitat(Qmean[31,month,yr], Hab$SJ)
    
    # Calculate juvenile carrying capacity
    
    Juv_capacity <- c(
      Upper_Sac = juv_carrying_capacity(pred.juv.UpSac,stochastic),
      Feather = juv_carrying_capacity(pred.juv.Feather,stochastic),
      Yuba = juv_carrying_capacity(pred.juv.Yuba,stochastic),
      Bear = juv_carrying_capacity(pred.juv.Bear,stochastic),
      SJ = juv_carrying_capacity(pred.juv.SJ,stochastic)
    )
    
    probabilityUp<-cumdistrib2[1,month]
    probabilityFeath<-cumdistrib2[2,month]
    probabilityYuba<-cumdistrib2[3,month]
    probabilityBear<-cumdistrib2[4,month]
    probabilitySJ<-cumdistrib2[5,month]
    
    #Upper Sac
    UpSacmig<-migrate_and_check_capacity(sum(st.fish[6,1]),probabilityUp,Juv_capacity["Upper_Sac"])
    st.fish[6,1] <- UpSacmig$remaining
    st.fish[7,1] <- st.fish[7,1] + UpSacmig$stepped
    st.fish[10,1] <- st.fish[10,1] + UpSacmig$cont
    
    #Feather
    Featmig<-migrate_and_check_capacity(sum(st.fish[2,1]),probabilityFeath,Juv_capacity["Feather"])
    st.fish[2,1] <- Featmig$remaining
    st.fish[7,1] <- st.fish[7,1] + Featmig$stepped
    st.fish[10,1] <- st.fish[10,1] + Featmig$cont
    
    #Yuba
    Yubamig <- migrate_and_check_capacity(sum(st.fish[3,1]), probabilityYuba, Juv_capacity["Yuba"])
    st.fish[3,1] <- Yubamig$remaining
    st.fish[7,1] <- st.fish[7,1] + Yubamig$stepped
    st.fish[10,1] <- st.fish[10,1] + Yubamig$cont
    
    #Bear
    Bearmig <- migrate_and_check_capacity(sum(st.fish[4,1]), probabilityBear, Juv_capacity["Bear"])
    st.fish[4,1] <- Bearmig$remaining
    st.fish[7,1] <- st.fish[7,1] + Bearmig$stepped
    st.fish[10,1] <- st.fish[10,1] + Bearmig$cont
    
    #SJ
    SJmig <- migrate_and_check_capacity(sum(st.fish[5,1]), probabilitySJ, Juv_capacity["SJ"])
    st.fish[5,1] <- SJmig$remaining
    st.fish[12,1] <- st.fish[12,1] + (SJmig$stepped + SJmig$cont)
  }   
  
  
  #########Lower Mid Sac to lower Sac (every month)
  LowMid.mig.prob<-0.80
  
  #send them down
  st.fish[10,1:2]<-st.fish[10,1:2]+ round(st.fish[7,1:2]*LowMid.mig.prob)
  #Keep left overs
  st.fish[7,1:2]<-round(st.fish[7,1:2]*(1-LowMid.mig.prob))
  
  #########lower Sac to North Delta
  Low.mig.prob0<-runif(1,0.05,0.1)
  Low.mig.prob1<-runif(1,0.1,0.25)
  Low.mig.prob2<-1
  Low.mig.probJuv<-1
  
  #send them down
  st.fish[11,1]<-st.fish[11,1]+ round(st.fish[10,1]*Low.mig.prob0)
  st.fish[11,2]<-st.fish[11,2]+ round(st.fish[10,2]*Low.mig.prob1)
  st.fish[11,3]<-st.fish[11,3]+ round(st.fish[10,3]*Low.mig.prob2)
  st.fish[11,4]<-st.fish[11,4]+ round(st.fish[10,4]*Low.mig.probJuv)
  
  #Keep left overs
  st.fish[10,1]<-round(st.fish[10,1]*(1-Low.mig.prob0))
  st.fish[10,2]<-round(st.fish[10,2]*(1-Low.mig.prob1))
  st.fish[10,3]<-round(st.fish[10,3]*(1-Low.mig.prob2))
  st.fish[10,4]<-round(st.fish[10,4]*(1-Low.mig.probJuv))
  
  
  #######Delta
  #Do I have anything on migration between deltas (maybe the michael thomas paper)
  
  NDelta.mig.prob<-runif(1,0.1,0.2)
  #send them down
  st.fish[12,1:4]<-st.fish[12,1:4]+ round(st.fish[11,1:4]*NDelta.mig.prob)
  #Keep left overs
  st.fish[11,1:4]<-round(st.fish[11,1:4]*(1-NDelta.mig.prob))
  
  #Back the other way
  SDelta.mig.prob<-runif(1,0.1,0.2)
  
  #send them down
  st.fish[11,1:4]<-st.fish[11,1:4]+ round(st.fish[12,1:4]*SDelta.mig.prob)
  #Keep left overs
  st.fish[12,1:4]<-round(st.fish[12,1:4]*(1-SDelta.mig.prob))
  
  
  ######Deltas to bays (kick any subadults out just incase they transitioned here)
  Delta.mig.prob<-c(0,runif(1,0.05,0.1),runif(1,0.1,0.25),runif(1,0.3,0.5),1)
  
  #first S delta
  st.fish[13,1:5]<-st.fish[13,1:5]+ round(st.fish[12,1:5]*Delta.mig.prob)
  #Keep left overs
  st.fish[12,1:5]<-round(st.fish[12,1:5]*(1-Delta.mig.prob))
  
  #next N delta
  st.fish[13,1:5]<-st.fish[13,1:5]+ round(st.fish[11,1:5]*Delta.mig.prob)
  #Keep left overs
  st.fish[11,1:5]<-round(st.fish[11,1:5]*(1-Delta.mig.prob))
  
  #allow them to go back (just slightly less unlikely for older fish)
  Bay2deltas<-c(0,runif(1,0.05,0.1),runif(1,0.1,0.25),runif(1,0.1,0.25))
  
  #first S delta
  st.fish[12,1:4]<-st.fish[12,1:4]+ round(st.fish[13,1:4]*Bay2deltas)
  #Keep left overs
  st.fish[13,1:4]<-round(st.fish[13,1:4]*(1-Bay2deltas))
  
  #next N delta
  st.fish[11,1:4]<-st.fish[11,1:4]+ round(st.fish[13,1:4]*Bay2deltas)
  #Keep left overs
  st.fish[13,1:4]<-round(st.fish[13,1:4]*(1-Bay2deltas))
  
  
  ####Bays to ocean include subadults this time
  Bay.mig.prob<-c(0,0,(runif(1,0.1,0.25)),runif(1,0.2,0.4),1)  
  #send them out
  st.fish[14,1:5]<-st.fish[14,1:5]+ round(st.fish[13,1:5]*Bay.mig.prob)
  #Keep left overs
  st.fish[13,1:5]<-round(st.fish[13,1:5]*(1-Bay.mig.prob))
  
  if (month == 11){ #send leftover adults out of the system
    st.fish[14,6]<-st.fish[14,6]+sum(st.fish[1:5,6])
    st.fish[1:5,6]<-0
    st.fish[14,7]<-st.fish[14,7]+sum(st.fish[1:5,7])
    st.fish[1:5,7]<-0
  }
  
} #end month loop
  
  st.fish<-AgingFunc(population=st.fish)
  #ocean survival function
   st.fish[14,]<-Ocean.Surv(fish=st.fish[14,],decis=decis.X[yr],stochastic)

   #### SIT metrics
   #keep track numbers annually during simulations
   obj<-rbind(obj,colSums(st.fish))
   SSE=SSE+sum((obj- TARGET)^2)
   
} #end year loop
#     
#     # #add year vector to output
#     holdr<-rbind(holdr,obj)
#     # 
#   }#end simulation loop
#   # 
#   holdr<-as.data.frame(holdr)
#   return(holdr)
#   # 
return(SSE)
 }#end function


#run.all(vect=c(1,1,1,1),decis.X = 1,mode="cal",stochastic=0)


yo.dog<-function(vect){
  run.all(vect=vect,decis.X = 1,mode="cal",stochastic=0)
}
start_time<-Sys.time()
test_single <- yo.dog(search[1,])
end_time<-Sys.time()
end_time-start_time


# 
# #time it
# start_time<-Sys.time()
# par=optim(par=c(0.80,0.13,1,1),
#           fn=yo.dog,
#           method="L-BFGS-B",
#           lower = c(0,0,0,0),
#           upper = c(1,1,1,1))
# end_time<-Sys.time()
# start_time-end_time
# par$par
# #write.csv(par,"save.calib.Results")
# try<-read.csv("save.calib.Results")

# Get all functions in environment for parallel
all_funcs <- lsf.str()
cores <- min(detectCores() - 1, 8)   #detectCores() - 1
cl <- makeCluster(cores,type="PSOCK")
clusterExport(cl, c("run.all", all_funcs,"rtri","CalSim3_metrics","yo.dog"))

clusterExport(cl, c("Qmean", "stream_temp", "Hab", "diversions", "dltadivs", "pred", 
                    "entrain", "tribs2use", "tribsDist", "N0", "st.fish",
                    "delta_temperature","contact_points","delta_contact_points",
                    "UpSacHab",'UpMidSacHab','MidSacHab','LowSacHab','FeatherHab',
                    'YubaHab','BearHab','SJHab','stream_temperature'))

#Garbage collection between runs because of memory space issues
clusterEvalQ(cl,gc())

# exhuastive search
search<-as.matrix(expand.grid(seq(0.0005,0.2,0.05),
                              seq(0.10,0.20,0.05),
                              seq(0.05,0.15,0.05),
                              seq(0.02,0.08,0.02)))

#seq(0.8,1,0.2)
#seq(-30,-20,5),
#seq(-30,-20,5)
#shot<-apply(search,1,yo.dog)
#search[which(shot==min(shot)),]

shot <- parApply(cl, search, 1, yo.dog)
stopCluster(cl)

best <- search[which.min(shot),]

