#################################################
### Packages and Data
################################################
library(EnvStats)
library(parallel)

#Load in Cal Sim 3
load("~/GreenSturgeonDSM/data/CalSim3_metrics.rda")
#flows
Qmean<-35.31468*(CalSim3_metrics$mean_monthly_flow) #turn from cms to cfs
#Temps
stream_temp<-CalSim3_metrics$avg_temp
load("~/GreenSturgeonDSM/data/delta_temperature.rda")
#Water year type
WY<-CalSim3_metrics$WaterYearType

#Load in all the Green sturgeon habitat data
setwd("~/GreenSturgeonDSM/data")
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
#ls("package:DSMhabitat")
diversions<-contact_points
dltadivs<-delta_contact_points
pred<-prop_high_predation #what is high predation?

entrain<- read.csv("EntrainmentProbs.csv")
entrain<-rbind(0,entrain)

setwd("~/GreenSturgeonDSM/R/Functions")
source("StartPopSim.R")
source("Movement.R")
source("HabCapacity.R")
source("CARRYINGCAP.R")
source("Spawning.R")
source("Survival.R")
source("SpawnDist.R")
source("stranding.R")
source("Entrainment.R")
source("Ocean.R")
source("agePop.R")
source("setArray.R")
source("DamPassage.R")


tribs2use<-c(1,19,20,18,31)
tribsDist<-c(18:20,31)


#################################################
### Begin Population Function
################################################
#runz=100
#years=20
# stochastic=0
# decis.X=17 #rescue decision
#mode="sim"
#vary="Nothing"
#pctil=1
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


run.scenarios<-function(decis.X,runz,years,stochastic){

  #For reproducibility
  options(digits=10)
  set.seed(1234)

holdr<-NULL #placeholder for all simulations
for(z in 1:runz){

# #Get initial population of fish
# #I created these to be similar to Dudley et al. 2024 some reason younger ages are
# #highly under estimated so a multiplier was added to create a similar number but uses
# #the same strucutre as the neg binomial predicted. the study says 10,000 fish
# #but does not include larvae (0.0006 survival from egg to age 0)
# ### Set up Array
st.fish<-set.array(N0)

#Reset diversions
diversions<-contact_points
dltadivs<-delta_contact_points

#reset habitat
Hab<-list(UpSac=UpSacHab,UpMidSac=UpMidSacHab,MidSac=MidSacHab,LowSac=LowSacHab,
          Feather=FeatherHab,Yuba=YubaHab,Bear=BearHab,SJ=SJHab)
#Reset any temperature and flow alterations
stream_temp<-CalSim3_metrics$avg_temp
Qmean<-35.31468*(CalSim3_metrics$mean_monthly_flow)

#################################################
### Annual dynamics begins begins here
################################################
obj<-NULL #placeholder for each year
for(yr in 1:(years)){
  if (2 %in% decis.X[[yr]]) stream_temp[19,3:6,yr]<-pmax(stream_temp[19,3:6,yr],14) #incubation months in Feather
  if (3 %in% decis.X[[yr]]) stream_temp[19,3:10,yr]<-pmax(stream_temp[19,3:10,yr],16) #rearing months in Feather
  if (4 %in% decis.X[[yr]]) stream_temp[31,3:6,yr]<-pmin(stream_temp[31,3:6,yr],19.9) #incubation SJ
  if (5 %in% decis.X[[yr]])stream_temp[31,3:10,yr]<-pmin(stream_temp[31,3:10,yr],19.9) #rearing SJ
  if (6 %in% decis.X[[yr]]) diversions[1]<-round(diversions[1]*.90) #screen up sac
  if (7 %in% decis.X[[yr]]) diversions[19]<-round(diversions[19]*.90) #screen Feather
  if (8 %in% decis.X[[yr]]) diversions[20]<-round(diversions[20]*.90) #screen yuba
  if (9 %in% decis.X[[yr]]) diversions[31]<-round(diversions[31]*.90) #screen SJ
  if (10 %in% decis.X[[yr]]) diversions[16]<-round(diversions[16]*.90) #Screen UpMid
  if (11 %in% decis.X[[yr]]) diversions[21]<-round(diversions[21]*.90) #Screen Low Mid
  if (12 %in% decis.X[[yr]]) diversions[24]<-round(diversions[24]*.90) #Screen lower sac
  if (13 %in% decis.X[[yr]]) dltadivs[1]<-round(dltadivs[1]*.90) #Screen North delta
  if (14 %in% decis.X[[yr]]) dltadivs[2]<-round(dltadivs[2]*.90) #screen south delta
  if (17 %in% decis.X[[yr]]) Hab$Feather$Spawning_sq.m<-Hab$Feather$Spawning_sq.m+8093.71 #add 2 acres of spawning habitat to feather
  if (19 %in% decis.X[[yr]]) Qmean[19,3:6,yr]<-pmax(Qmean[19,3:6,yr],6000)
  if (20 %in% decis.X[[yr]]) Qmean[20,3:6,yr]<-pmax(Qmean[20,3:6,yr],6000)
  if (21 %in% decis.X[[yr]]) Qmean[31,1:4,yr]<-pmax(Qmean[31,1:4,yr],1500)
  if (22 %in% decis.X[[yr]]) Qmean[21,1:3,yr]<-pmax(Qmean[21,1:3,yr],17700)
  if (22 %in% decis.X[[yr]]) Qmean[24,1:3,yr]<-pmax(Qmean[24,1:3,yr],31100)
  if (23 %in% decis.X[[yr]]) Hab$Yuba$Spawning_sq.m<-Hab$Yuba$Spawning_sq.m+12000
  if (23 %in% decis.X[[yr]]) Hab$Yuba$RiverUse_sq.m<-Hab$Yuba$RiverUse_sq.m+12000 #was told 12 rkm-Daguerre Dam
  if (24 %in% decis.X[[yr]]) stream_temp[1,3:6,yr]<-pmax(stream_temp[1,3:6,yr],14) #incubation UpSac temps
  if (25 %in% decis.X[[yr]]) stream_temp[20,3:6,yr]<-pmax(stream_temp[20,3:6,yr],14) #incubation Yuba
  if (26 %in% decis.X[[yr]]) stream_temp[1,3:10,yr]<-pmax(stream_temp[1,3:10,yr],16) #rear UpSac temps
  if (27 %in% decis.X[[yr]]) stream_temp[16,3:10,yr] <- pmin(pmax(stream_temp[16,3:10,yr], 16), 19.9) #UpMid rear
  if (28 %in% decis.X[[yr]]) Hab$UpSac$Spawning_sq.m<-Hab$UpSac$Spawning_sq.m+8093.71 #2 acres
  if (29 %in% decis.X[[yr]]) Hab$UpSac$RiverUse_sq..m<-Hab$UpSac$RiverUse_sq..m+8093.71 #2 acres
  if (30 %in% decis.X[[yr]]) Hab$UpMidSac$RiverUse_sq..m<-Hab$UpMidSac$RiverUse_sq..m+8093.71 #2 acres
  if (31 %in% decis.X[[yr]]) Qmean[19,3:6,yr]<-pmax(Qmean[19,3:6,yr],5000)
  if (32 %in% decis.X[[yr]]) Qmean[20,3:6,yr]<-pmax(Qmean[20,3:6,yr],5000)
  if (33 %in% decis.X[[yr]]) Hab$Yuba$Spawning_sq.m<-Hab$Yuba$Spawning_sq.m+8093.71 #2 acres
  if (34 %in% decis.X[[yr]]) Hab$SJ$Spawning_sq.m<-Hab$SJ$Spawning_sq.m+8093.71 #2 acres
  if (35 %in% decis.X[[yr]]) Hab$SJ$RiverUse_sq.m<-Hab$SJ$RiverUse_sq.m+8093.71 #2 acres
  if (36 %in% decis.X[[yr]]) Hab$Feather$RiverUse_sq.m<-Hab$Feather$RiverUse_sq.m+8093.71 #2 acres
  if (38 %in% decis.X[[yr]]) Qmean[31,5,yr]<-pmax(Qmean[31,5,yr],7500) #chinook
  if (39 %in% decis.X[[yr]]) Qmean[16,3,yr]<-pmax(Qmean[16,3,yr],10800) #chinook
  if (39 %in% decis.X[[yr]]) Qmean[24,3,yr]<-pmax(Qmean[24,3,yr],10800) #chinook
  if (40 %in% decis.X[[yr]]) Qmean[16,4,yr]<-pmax(Qmean[16,4,yr],10800) #chinook
  if (40 %in% decis.X[[yr]]) Qmean[24,4,yr]<-pmax(Qmean[24,4,yr],10800) #chinook
  if (41 %in% decis.X[[yr]]) Qmean[31,4,yr]<-pmax(Qmean[31,4,yr],7500) #chinook
  if (42 %in% decis.X[[yr]]) Qmean[16,1,yr]<-pmax(Qmean[16,1,yr],10800) #chinook
  if (42 %in% decis.X[[yr]]) Qmean[24,1,yr]<-pmax(Qmean[24,1,yr],10800) #chinook
  if (43 %in% decis.X[[yr]]) stream_temp[1,5:8,yr]<-pmax(stream_temp[1,5:8,yr],20) #steelhead
  if (44 %in% decis.X[[yr]]) stream_temp[16,5:8,yr]<-pmax(stream_temp[16,5:8,yr],20) #steelhead
  if (45 %in% decis.X[[yr]]) stream_temp[19,5:8,yr]<-pmax(stream_temp[19,5:8,yr],20) #steelhead
  if (46 %in% decis.X[[yr]]) Qmean[16,11,yr]<-pmax(Qmean[16,11,yr],10800) #Steelhead
  if (46 %in% decis.X[[yr]]) Qmean[24,11,yr]<-pmax(Qmean[24,11,yr],10800) #Steelhead
  if (47 %in% decis.X[[yr]]) Qmean[16,12,yr]<-pmax(Qmean[16,12,yr],10800) #Steelhead
  if (47 %in% decis.X[[yr]]) Qmean[24,12,yr]<-pmax(Qmean[24,12,yr],10800) #Steelhead
  if (48 %in% decis.X[[yr]]) Qmean[16,1,yr]<-pmax(Qmean[16,1,yr],10800) #Steelhead
  if (48 %in% decis.X[[yr]]) Qmean[24,1,yr]<-pmax(Qmean[24,1,yr],10800) #Steelhead
  if (49 %in% decis.X[[yr]]) Qmean[16,2,yr]<-pmax(Qmean[16,2,yr],10800) #Steelhead
  if (49 %in% decis.X[[yr]]) Qmean[24,2,yr]<-pmax(Qmean[24,2,yr],10800) #Steelhead
  if (50 %in% decis.X[[yr]]) Qmean[16,3,yr]<-pmax(Qmean[16,3,yr],10800) #Steelhead
  if (50 %in% decis.X[[yr]]) Qmean[24,3,yr]<-pmax(Qmean[24,3,yr],10800) #Steelhead
  if (51 %in% decis.X[[yr]]) Qmean[31,11,yr]<-pmax(Qmean[31,11,yr],7500) #Steelhead
  if (52 %in% decis.X[[yr]]) Qmean[31,12,yr]<-pmax(Qmean[31,12,yr],7500) #Steelhead
  if (53 %in% decis.X[[yr]]) Qmean[31,1,yr]<-pmax(Qmean[31,1,yr],7500) #Steelhead
  if (54 %in% decis.X[[yr]]) Qmean[31,2,yr]<-pmax(Qmean[31,2,yr],7500) #Steelhead
  if (55 %in% decis.X[[yr]]) Qmean[31,3,yr]<-pmax(Qmean[31,3,yr],7500) #Steelhead


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

    # Calculate spawners
    sm.spawners.spawn <- round(rtri(sum(st.fish[13,6]), min=3, max=6, mode=4.5))
    lg.spawners.spawn <- round(rtri(sum(st.fish[13,7]), min=3, max=6, mode=4.5))
    store.spawners <- c(length(sm.spawners.spawn), length(lg.spawners.spawn))
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
      Egg.S<-Egg.Surv(avgT=stream_temp[tribs2use,month,yr])
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
                        pred=c("medium","high","low","medium","high"),stochastic)
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
    extract.Strand<-strand.S(stranded=st.fish[8:9,6:7],decis=decis.X[[yr]],stochastic)
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
      st.fish[13,6]<-st.fish[13,6]-sum(sm.spawners.spawn == 4)
      st.fish[13,7]<-st.fish[13,7]-sum(lg.spawners.spawn == 4)

#send age 0 upper sac to upper mid sac..#leave feather, yuba, bear,SJ in their tribs
st.fish[6,1]<-st.fish[6,1]+(st.fish[1,1]) #*Up.mig.prob0*Up.migS.0)
st.fish[1,1]<-0 #clear out previous age 0 because they all moved down

#Apply carrying capacity
  cap.reached<-Capacity.sub(spawners=st.fish[1:5,6:7],flow=Qmean[tribs2use,month,yr],hab=Hab,stochastic)
  #Survival to age 0 production
  Egg.S<-Egg.Surv(avgT=stream_temp[tribs2use,month,yr])
  #Passage at Sunset pumps
  pass<-SunsetPassage(flow=Qmean[19,month,yr],year=yr)
  #Larvae
  larvae<-spawn(smAd=st.fish[1:5,6]-prev.smfish,lgAd=st.fish[1:5,7]-prev.lgfish,
                discharge=Qmean[tribs2use,month,yr],passage=pass,
                temp=stream_temp[tribs2use,month,yr],Egg.S=Egg.S,hab.cap=cap.reached,
                prev.pass=passSunset,stochastic)
  # Create spawning occurrence indicator
  spawning_occurred_4 <- c(as.integer(larvae[[1]] > 0))

  #Larval to age 0 survival
  age0<-Larvae.Surv(larvae=larvae[[1]],avgT=stream_temp[tribs2use,month,yr],
                    pred=c("medium","high","low","medium","high"),stochastic)
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
  extract.Strand<-strand.S(stranded=st.fish[8:9,6:7],decis=decis.X[[yr]],stochastic)
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
  st.fish[13,6]<-st.fish[13,6]-sum(sm.spawners.spawn == 5)
  st.fish[13,7]<-st.fish[13,7]-sum(lg.spawners.spawn == 5)

#send age 0 upper sac to upper mid sac..#leave feather, yuba, bear,SJ in their tribs
  st.fish[6,1]<-st.fish[6,1]+ (st.fish[1,1]) #*Up.mig.prob0*Up.migS.0)
  st.fish[1,1]<-0 #clear out previous age 0 because they all moved down

#Apply carrying capacity
  cap.reached<-Capacity.sub(spawners=st.fish[1:5,6:7],flow=Qmean[tribs2use,month,yr],hab=Hab,stochastic)
  #Passage at Sunset pumps
  pass<-SunsetPassage(flow=Qmean[19,month,yr],year=yr)
#Survival to age 0 production
  Egg.S<-Egg.Surv(avgT=stream_temp[tribs2use,month,yr])
  larvae<-spawn(smAd=st.fish[1:5,6]-prev.smfish,lgAd=st.fish[1:5,7]-prev.lgfish,
                discharge=Qmean[tribs2use,month,yr],passage=pass,
                temp=stream_temp[tribs2use,month,yr],Egg.S=Egg.S,hab.cap=cap.reached,
                prev.pass=passSunset,stochastic)
  # Create spawning occurrence indicator
  spawning_occurred_5 <- c(as.integer(larvae[[1]] > 0))

#Larval to age 0 survival
  age0<-Larvae.Surv(larvae=larvae[[1]],avgT=stream_temp[tribs2use,month,yr],
                    pred=c("medium","high","low","medium","high"),stochastic)
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
  extract.Strand<-strand.S(stranded=st.fish[8:9,6:7],decis=decis.X[[yr]],stochastic)
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
  st.fish[13,6]<-st.fish[13,6]-sum(sm.spawners.spawn == 6)
  st.fish[13,7]<-st.fish[13,7]-sum(lg.spawners.spawn == 6)

#send age 0 upper sac to upper mid sac..#leave feather, yuba, bear,SJ in their tribs
  st.fish[6,1]<-st.fish[6,1]+(st.fish[1,1]) #*Up.mig.prob0*Up.migS.0)
  st.fish[1,1]<-0 #clear out previous age 0 because they all moved down

  #Apply carrying capacity
  cap.reached<-Capacity.sub(spawners=st.fish[1:5,6:7],flow=Qmean[tribs2use,month,yr],hab=Hab,stochastic)
  #Survival to age 0 production
  Egg.S<-Egg.Surv(avgT=stream_temp[tribs2use,month,yr])
  #Passage at Sunset pumps
  pass<-SunsetPassage(flow=Qmean[19,month,yr],year=yr)
  #Larvae
  larvae<-spawn(smAd=st.fish[1:5,6]-prev.smfish,lgAd=st.fish[1:5,7]-prev.lgfish,
                discharge=Qmean[tribs2use,month,yr],passage=pass,
                temp=stream_temp[tribs2use,month,yr],Egg.S=Egg.S,hab.cap=cap.reached,
                prev.pass=passSunset,stochastic)
  # Create spawning occurrence indicator
  spawning_occurred_6 <- c(as.integer(larvae[[1]] > 0))

  #Larval to age 0 survival
  age0<-Larvae.Surv(larvae=larvae[[1]],avgT=stream_temp[tribs2use,month,yr],
                    pred=c("medium","high","low","medium","high"),stochastic)
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
      extract.Strand<-strand.S(stranded=st.fish[8:9,6:7],decis=decis.X[[yr]],stochastic)
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
  st.fish[6,1]<-st.fish[6,1]+(st.fish[1,1]) #*Up.mig.prob0*Up.migS.0)
  st.fish[1,1]<-0 #clear out previous age 0 because they all moved down

  #Apply carrying capacity
  cap.reached<-Capacity.sub(spawners=st.fish[1:5,6:7],flow=Qmean[tribs2use,month,yr],hab=Hab,stochastic)
  #Survival to age 0 production
  Egg.S<-Egg.Surv(avgT=stream_temp[tribs2use,month,yr])
  #Passage at Sunset pumps
  pass<-SunsetPassage(flow=Qmean[19,month,yr],year=yr)
  #Larvae
  larvae<-spawn(smAd=st.fish[1:5,6]-prev.smfish,lgAd=st.fish[1:5,7]-prev.lgfish,
                discharge=Qmean[tribs2use,month,yr],passage=pass,
                temp=stream_temp[tribs2use,month,yr],Egg.S=Egg.S,
                hab.cap=cap.reached,prev.pass=passSunset,stochastic)
  # Create spawning occurrence indicator
  spawning_occurred_7 <- c(as.integer(larvae[[1]] > 0))

  #Larval to age 0 survival
  age0<-Larvae.Surv(larvae=larvae[[1]],avgT=stream_temp[tribs2use,month,yr],
                    pred=c("medium","high","low","medium","high"),stochastic)
  st.fish[1:5,1]<-st.fish[1:5,1]+age0 #put them in the multidimensional array

}

  if (month==8){
    #send last of age 0 fish to the upper mid sac
    st.fish[6,1]<-st.fish[6,1]+ st.fish[1,1] #*Up.mig.prob0*Up.migS.0)
    st.fish[1,1]<-0 #clear out previous age 0 because they all moved down

    #Kick Early adult outmigration adults out
    prop_early.up<-Adult.Early(mean(Qmean[1,5:7,yr]),trib=c("UpSac"),stochastic)
    prop_early.F<-Adult.Early(mean(Qmean[19,5:7,yr]),trib=c("Feather"),stochastic)
    prop_early.Y<-Adult.Early(mean(Qmean[20,5:7,yr]),trib=c("Yuba"),stochastic)
    prop_early.B<-Adult.Early(mean(Qmean[18,5:7,yr]),trib=c("Bear"),stochastic)
    prop_early.SJ<-Adult.Early(mean(Qmean[31,5:7,yr]),trib=c("SJ"),stochastic)
    prop_early<-c(prop_early.up,prop_early.F,prop_early.Y,prop_early.B,prop_early.SJ)
    # Calculate number of early migrants
    early_migrants.sm<-c()
    for(i in 1:5){
      early_migrants.sm[i]<-floor(st.fish[i, 6] * prop_early[i])
    }
    early_migrants.lg<-c()
    for(i in 1:5){
      early_migrants.lg[i]<-floor(st.fish[i, 7] * prop_early[i])
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
  #apply entrainment mort to age 0 fish and age 1 and 2 in the lower sac
  #only occuring april to september of each year
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
    apply_juv_survival(params[[1]], params[[2]],curr_month=month)
    apply_adult_survival(i, params[[2]],curr_month=month)
  }

  # Apply survival rates for Sac regions (juvenile only)
  sac_regions <- list(
    UpMid = list(6, 16),
    LowMid = list(7, 21),
    Low = list(10, 24)
  )

  for (region in sac_regions) {
    apply_juv_survival(region[[1]], month_yr_temps[region[[2]]],curr_month=month)
  }

  # Apply Delta survival rates
  delta_regions <- list(
    N = list(11, 1),
    S = list(12, 2)
  )

  for (region in delta_regions) {
    apply_juv_survival(region[[1]], delta_temperature[month, yr, region[[2]]],
                       delta = TRUE,curr_month = month)
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

#All on an Annual time step here
  st.fish<-AgingFunc(population=st.fish)
  #ocean survival function
   st.fish[14,]<-Ocean.Surv(fish=st.fish[14,],decis=decis.X[[yr]],stochastic)

#### SIT metrics
   #### SIT metrics
   popgrow<-sum(st.fish)
   recruit<-sum(colSums(st.fish[,1:4]))
   spawner.abund<-sum(store.spawners)
   SpawnStats<-rbind(spawning_occurred_3,spawning_occurred_4,spawning_occurred_5,
               spawning_occurred_6,spawning_occurred_7)
   Upsacmonths<-sum(SpawnStats[,1])
   Feathermonths<-sum(SpawnStats[,2])
   Yubamonths<-sum(SpawnStats[,3])
   Sjmonths<-sum(SpawnStats[,5])
   num.riv<-sum(c(as.integer(colSums(SpawnStats[,1:5]) > 0)))
   if (Upsacmonths==0) {UpsacRear<-0} else {UpsacRear<-rear.months[1]}
   if (Feathermonths==0) { FeatherRear<-0} else { FeatherRear<-rear.months[2]}
   if (Yubamonths==0) {YubaRear<-0} else {YubaRear<-rear.months[3]}
   if (Sjmonths==0) { SjRear<-0} else { SjRear<-rear.months[5]}
   obj<-rbind(obj,c(popgrow,recruit,spawner.abund,store.spawners[2],store.holders,
                    Upsacmonths,Feathermonths,Yubamonths,Sjmonths,num.riv,UpsacRear,FeatherRear,YubaRear,SjRear))


} #end year loop

# #add year vector to output
 obj<-cbind(obj,1:nrow(obj))
 holdr<-rbind(holdr,obj)
#
}#end simulation loop
#
holdr<-as.data.frame(holdr)
return(holdr)
#
}#end function

#Just to test
#run.scenarios(decis.X=Screening_strategy ,runz=1,years=20,stochastic=0)


# Create function to handle a single scenario
run_single_scenario <- function(params) {
  strategy_name <- params$strategy_name
  current_strategy <- params$strategy  # This should be the full strategy list for all years
  iteration <- params$iteration

  # Run the full strategy for all years
  XYz <- run.scenarios(decis.X = current_strategy, runz = 1, years = 20, stochastic = 0)
  XYz$strategy <- strategy_name
  XYz$iteration <- iteration

  return(XYz)
}

# Create list of all parameter combinations
all_params <- list()
for (strategy_name in names(decis.cmb)) {
  current_strategy <- decis.cmb[[strategy_name]]

  all_params[[length(all_params) + 1]] <- list(
    strategy_name = strategy_name,
    strategy = current_strategy,
    iteration = i
  )
}

# Set up parallel processing
cl <- makeCluster(detectCores() - 1)

# Export necessary functions and data
all_funcs <- lsf.str()
clusterExport(cl, c("run_single_scenario", "run.scenarios", all_funcs, "rtri"))
clusterExport(cl, c("Qmean", "stream_temp", "Hab", "diversions", "dltadivs", "pred",
                    "entrain", "tribs2use", "tribsDist", "N0", "st.fish",
                    "delta_temperature", "contact_points", "delta_contact_points",
                    "UpSacHab", 'UpMidSacHab', 'MidSacHab', 'LowSacHab', 'FeatherHab',
                    'YubaHab', 'BearHab', 'SJHab', 'CalSim3_metrics'))

# Also export the full decis.cmb list to the workers
clusterExport(cl, "decis.cmb")

# Run scenarios in parallel
results <- parLapply(cl, all_params, run_single_scenario)

# Stop cluster
stopCluster(cl)

# Combine results
all.decis <- do.call(rbind, results)

#Save ALL taxa
#write.csv(all.decis,file="C:/Users/gilliger/Box/Erin Dissertation/SIT sturgeon models/Green Sturgeon Model code/Results/AllTaxaStrategyResultsFinalized.csv")

#Save sturgeon-specific results
#write.csv(all.decis,file="C:/Users/gilliger/Box/Erin Dissertation/SIT sturgeon models/Green Sturgeon Model code/Results/SturgeonResultsFinalized.csv")

