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
source("Entrainment.R")
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
set.seed(1234)
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
  #For reproducibility
  options(digits=10)
  set.seed(1234)
  
  n.init <- StartPop(n.pop=10000, maxAge=34, Natural.S=0.85)
  N0 <- round(matrix(c(n.init$n.age0*4,
                       n.init$n.age1*4,
                       n.init$n.age2*4,
                       n.init$n.juvs*4,
                       n.init$n.sub+n.init$n.marineJ,
                       n.init$n.sm.ad,
                       n.init$n.lg.ad), ncol=1))
  st.fish <- set.array(N0) 
  
  TARGET = c(1730, 1449, 1298, 2432, 2403, 1471, 960) #Dudley et al.2024
  SSE = 0
  all_pops<-matrix(0,nrow=20,ncol=7)
  
  # Debug initial population (optional - you can remove this too if you want)
  cat("=== INITIAL POPULATION ===\n")
  cat("st.fish dimensions:", dim(st.fish), "\n")
  cat("Initial colSums:", colSums(st.fish), "\n")
  cat("TARGET:", TARGET, "\n")
for(yr in 1:20){
 
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
  
  #Set up a possibility of a HAB event that wipes out the bay
  if (yr %in% 10:14) {HAB.prob <- rbeta(1,3,10)} else {HAB.prob <- 0} 
  
for(month in 1:12){
  if(month==2){
    
    #Set up ocean small adults and draw a spawning pool for the bay
    # Small adults
    ocean_small_migrants <- floor(st.fish[14,6] * 0.25)
    st.fish[13,6] <- st.fish[13,6] + ocean_small_migrants
    st.fish[14,6] <- st.fish[14,6] - ocean_small_migrants
    
    # Large adults
    ocean_large_migrants <- floor(st.fish[14,7] * 0.25)
    st.fish[13,7] <- st.fish[13,7] + ocean_large_migrants
    st.fish[14,7] <- st.fish[14,7] - ocean_large_migrants 
    
    sm.spawners.spawn<-round(rtri(sum(st.fish[13,6]), min=3, max=6,mode=4.5))
    lg.spawners.spawn<-round(rtri(sum(st.fish[13,7]), min=3, max=6,mode=4.5))
    store.spawners<-c(length(sm.spawners.spawn),(length(lg.spawners.spawn)))
  }
  
  
    if(month == 3){

      #Assign routes and distribution of spawners
      spawnroute<-spawndist(spawners=c(sum(sm.spawners.spawn == 3),sum(lg.spawners.spawn == 3)),
                            Qmeans=Qmean[tribsDist,month,yr],decis=decis.X[[yr]],stochastic)
      
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
                            Qmeans=Qmean[tribsDist,month,yr],decis=decis.X[yr],stochastic)
      
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
                        Qmeans=Qmean[tribsDist,month,yr],decis=decis.X[yr],stochastic)
      
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
                        Qmeans=Qmean[tribsDist,month,yr],decis=decis.X[yr],stochastic)
      
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
    
    #If there's a HAB event wipe out the bay in august
    if (stochastic){HAB.event<-rbinom(1,1,HAB.prob)} else {HAB.event<-as.numeric(HAB.prob>0.5)}
    if (HAB.event==1){st.fish[13,]<-0}
  }
  
  if(month >= 4 && month <= 9){
    entrained<-if (stochastic){entrain.stochastic(fish=st.fish[,1:3],prob.entrain=entrain[1:2],
                                                  n.div=diversions)
    }else{
      entrain.deterministic(fish=st.fish[,1:3],prob.entrain=entrain[1:2],
                            n.div=diversions)
    }
    st.fish[1:10,1]<-pmax(0, st.fish[1:10,1]-entrained[[1]])
    st.fish[10,2]<-pmax(0,st.fish[10,2]-entrained[[2]])
    st.fish[10,3]<-pmax(0,st.fish[10,3]-entrained[[3]])
    #delta entrainment only apply to age 0-2 fish also entrainment vulnerability is unknown so
    #just setting it low for now
    NDelta.entrain<-if (stochastic){delta.entrain.stoch(fish=st.fish[11,1:3], prob.entrain=entrain[1:2],
                                                        n.div=dltadivs[1])
    }else {
      delta.entrain.deterministic(fish=st.fish[11,1:3], prob.entrain=entrain[1:2],
                                  n.div=dltadivs[1])
    }
    st.fish[11,1:3]<-pmax(0,st.fish[11,1:3]-NDelta.entrain)
    SDelta.entrain<-if (stochastic){delta.entrain.stoch(fish=st.fish[12,1:3],prob.entrain=entrain[1:2],
                                                        n.div=dltadivs[2])
    }else {
      delta.entrain.deterministic(fish=st.fish[12,1:3], prob.entrain=entrain[1:2],
                                  n.div=dltadivs[2])
    }
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
    apply_juv_survival(params[[1]], params[[2]],delta=FALSE,curr_month=month,mode="cal",int=vect[3])
    apply_adult_survival(i, params[[2]],curr_month=month)
  }
  
  # Apply survival rates for Sac regions (juvenile only)
  sac_regions <- list(
    UpMid = list(6, 16),
    LowMid = list(7, 21),
    Low = list(10, 24)
  )
  
  for (region in sac_regions) {
    apply_juv_survival(region[[1]], month_yr_temps[region[[2]]], delta=FALSE, curr_month=month,
                       mode="cal",int=vect[3])
  }
  
  # Apply Delta survival rates
  delta_regions <- list(
    N = list(11, 1),
    S = list(12, 2)
  )
  
  for (region in delta_regions) {
    apply_juv_survival(region[[1]], delta_temperature[month, yr, region[[2]]],delta=TRUE,
                       curr_month = month, mode="cal",int=vect[4])
  }
  
  
  ####Movement###
  
  ########OUtmigrate juveniles from tribs (once month gets to 3 send all the leftovers down)
  
  if(month >= 10 || month <= 3){
    # Set iteration year as needed
    iter_yr <- if(month %in% 1:3) yr + 1 else yr
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
  LowMid.mig.prob <- if (stochastic){runif(1,0.36,1)} else {0.59}
  
  #send them down
  lowMidmigrants <- floor(st.fish[7,1:2]*LowMid.mig.prob)
  st.fish[10,1:2] <- st.fish[10,1:2] + lowMidmigrants
  #Keep left overs
  st.fish[7,1:2] <- st.fish[7,1:2] - lowMidmigrants
  
  #########lower Sac to North Delta
  Low.mig.prob0 <- if (stochastic){rbeta(1,2.16,16.31)} else {0.1}
  Low.mig.prob1 <- if (stochastic){rbeta(1,3.14,10.16)} else {0.25}
  Low.mig.prob2 <- if (stochastic){rbeta(1,6.31,0.37)} else {1}
  Low.mig.probJuv <- if (stochastic){rbeta(1,6.31,0.37)} else {1}
  
  low0.migrants <- floor(st.fish[10,1]*Low.mig.prob0)
  st.fish[11,1] <- st.fish[11,1] + low0.migrants
  st.fish[10,1] <- st.fish[10,1] - low0.migrants
  
  low1.migrants <- floor(st.fish[10,2]*Low.mig.prob1)
  st.fish[11,2] <- st.fish[11,2] + low1.migrants
  st.fish[10,2] <- st.fish[10,2] - low1.migrants
  
  low2.migrants <- floor(st.fish[10,3]*Low.mig.prob2)
  st.fish[11,3] <- st.fish[11,3] + low2.migrants
  st.fish[10,3] <- st.fish[10,3] - low2.migrants
  
  lowJuv.migrants <- floor(st.fish[10,4]*Low.mig.probJuv)
  st.fish[11,4] <- st.fish[11,4] + lowJuv.migrants
  st.fish[10,4] <- st.fish[10,4] - lowJuv.migrants
  
  #######Delta
  #Do I have anything on migration between deltas (maybe the michael thomas paper)
  NDelta.mig.prob <- if (stochastic){rbeta(1,4.26,20.51)} else {0.15}
  
  #send them down
  ndelta.migrants <- floor(st.fish[11,1:4]*NDelta.mig.prob)
  st.fish[12,1:4] <- st.fish[12,1:4] + ndelta.migrants
  #Keep left overs
  st.fish[11,1:4] <- st.fish[11,1:4] - ndelta.migrants
  
  #Back the other way
  SDelta.mig.prob <- if (stochastic){rbeta(1,4.26,20.51)} else {0.15}
  
  #send them down
  sdelta.migrants <- floor(st.fish[12,1:4]*SDelta.mig.prob)
  st.fish[11,1:4] <- st.fish[11,1:4] + sdelta.migrants
  #Keep left overs
  st.fish[12,1:4] <- st.fish[12,1:4] - sdelta.migrants
  
  ######Deltas to bays (kick any subadults out just incase they transitioned here)
  age1bay <- if (stochastic){rbeta(1,3.33, 35.096)} else {0.05}
  age2bay <- if (stochastic){rbeta(1,3.699, 15.69)} else {0.15}
  juvbay <- if (stochastic){rbeta(1,11.02, 19.24)} else {0.35}
  
  Delta.mig.prob <- c(0,age1bay,age2bay,juvbay,1)
  
  #first S delta
  sdelta2bay.migrants <- floor(st.fish[12,1:5]*Delta.mig.prob)
  st.fish[13,1:5] <- st.fish[13,1:5] + sdelta2bay.migrants
  #Keep left overs
  st.fish[12,1:5] <- st.fish[12,1:5] - sdelta2bay.migrants
  
  #next N delta
  ndelta2bay.migrants <- floor(st.fish[11,1:5]*Delta.mig.prob)
  st.fish[13,1:5] <- st.fish[13,1:5] + ndelta2bay.migrants
  #Keep left overs
  st.fish[11,1:5] <- st.fish[11,1:5] - ndelta2bay.migrants
  
  #allow them to go back (just slightly less unlikely for older fish)
  age1back <- if (stochastic){rbeta(1,3.22, 25.88)} else {0.15}
  age2back <- if (stochastic){rbeta(1,3.62,36.52)} else {0.10}
  juvback <- if (stochastic){rbeta(1,3.33, 35.096)} else {0.05}
  Bay2deltas <- c(0,age1back,age2back,juvback)
  
  #first S delta
  bay2sdelta.migrants <- floor(st.fish[13,1:4]*Bay2deltas)
  st.fish[12,1:4] <- st.fish[12,1:4] + bay2sdelta.migrants
  #Keep left overs
  st.fish[13,1:4] <- st.fish[13,1:4] - bay2sdelta.migrants
  
  #next N delta
  bay2ndelta.migrants <- floor(st.fish[13,1:4]*Bay2deltas)
  st.fish[11,1:4] <- st.fish[11,1:4] + bay2ndelta.migrants
  #Keep left overs
  st.fish[13,1:4] <- st.fish[13,1:4] - bay2ndelta.migrants
  
  ####Bays to ocean include subadults this time
  age1ocean <- if (stochastic){rbeta(1,1.55, 46.28)} else {0}
  age2ocean <- if (stochastic){rbeta(1,3.699, 15.69)} else {0.15}
  juvocean <- if (stochastic){rbeta(1,4.27, 8.9)} else {0.35}
  Bay.mig.prob <- c(0,age1ocean,age2ocean,juvocean,1)
  
  #send them out
  bay2ocean.migrants <- floor(st.fish[13,1:5]*Bay.mig.prob)
  st.fish[14,1:5] <- st.fish[14,1:5] + bay2ocean.migrants
  #Keep left overs
  st.fish[13,1:5] <- st.fish[13,1:5] - bay2ocean.migrants
} #end month loop
  
  st.fish[14,6]<-st.fish[14,6]+sum(st.fish[1:5,6])
  st.fish[1:5,6]<-0
  st.fish[14,7]<-st.fish[14,7]+sum(st.fish[1:5,7])
  st.fish[1:5,7]<-0
  
  #ocean survival function
  st.fish[14,]<-Ocean.Surv(fish=st.fish[14,],decis=decis.X[yr],stochastic)
  
  all_pops[yr, ] <- colSums(st.fish)
  
  if(mode == "cal") {
    aging_int <- vect[5:7]
    st.fish <- AgingFunc(population=st.fish, mode="cal", int=aging_int)
  } else {
    st.fish <- AgingFunc(population=st.fish, mode="sim")
  }
  
} #end year loop
  mean_pop <- colMeans(all_pops)
  SSE <- sum((mean_pop - TARGET)^2)
  
  # debug
  if(mode == "cal") {
    cat("Mean population over 20 years:\n")
    stage_names <- c("Age0", "Age1", "Age2", "Juvenile", "Subadult", "SmallAdult", "LargeAdult")
    for(i in 1:7) {
      cat(sprintf("%s: %8.0f vs %8.0f (diff: %8.0f)\n", 
                  stage_names[i], mean_pop[i], TARGET[i], mean_pop[i] - TARGET[i]))
    }
    cat("SSE:", SSE, "\n")
  }
return(SSE)
}#end function


#run.all(vect=c(1,1,1,1),decis.X = 1,mode="cal",stochastic=0)


yo.dog<-function(vect){
  decis_vector <- rep(1, 20)
  sse <- run.all(vect=vect, decis.X=decis_vector, mode="cal", stochastic=0)
  return(sse)
}


yo.dog.debug <- function(vect) {
  decis_vector <- rep(1, 20)
  sse <- run.all(vect=vect, decis.X=decis_vector, mode="cal", stochastic=0)
  return(sse)
}

# Test with debug

test_result <- yo.dog.debug(c(0.001, 0.90, 0.99, 0.99, 0.17, 0.22, 0.39))


#test_early_bottleneck1 <- yo.dog.debug(c(0.002, 0.91, 0.99, 0.99, 0.20, 0.28, 0.41)) 

# Test with different survival values - these MUST give different results
test_low <- yo.dog.debug(c(0.0008, 0.95, 0.2, 0.2, 0.2, 0.2, 0.40))   # Should give 0.1 juv, 0.14 delta
test_high <- yo.dog.debug(c(0.0008, 0.85, 0.8, 0.8, 0.2, 0.2, 0.40))  # Should give 0.4 juv, 0.56 delta

# Test exact original parameters
test_original_best <- yo.dog.debug(c(0.0005, 0.8200, 0.8200, 0.8200, 0.1600, 0.2800, 0.48))

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
#lusterEvalQ(cl,gc())

#First search coarse to find general parameter region
search_coarse <- as.matrix(expand.grid(
  seq(0.0008, 0.001, 0.0001),   #  egg survival 
  seq(0.88, 0.91, 0.01),         #  larval mortality
  seq(0.99, 0.99, 0.01),         #  juvenile survival
  seq(0.99, 0.99, 0.01),         #  delta survival
  seq(0.14, 0.18, 0.01),         #  Age.smAd
  seq(0.20, 0.25, 0.01),         #  Age.subAd  
  seq(0.34, 0.38, 0.01)          #  Age.juv
))

shot.coarse <- parApply(cl, search_coarse, 1, yo.dog)
stopCluster(cl)
best.coarse <- search_coarse[which.min(shot.coarse),]

# Refine around best regions
search_ultra_fine <- as.matrix(expand.grid(
  seq(0.0009, 0.0011, 0.00005),  # Around 0.001 ±0.0001
  seq(0.89, 0.91, 0.005),        # Around 0.90 ±0.01
  c(0.99),                       # Keep constant
  c(0.99),                       # Keep constant  
  seq(0.16, 0.18, 0.005),        # Around 0.17 ±0.01
  seq(0.21, 0.23, 0.005),        # Around 0.22 ±0.01
  seq(0.37, 0.39, 0.005)         # Around 0.38 ±0.01
))


shot <- parApply(cl, search_ultra_fine, 1, yo.dog)
stopCluster(cl)

best <- search_ultra_fine[which.min(shot),]

