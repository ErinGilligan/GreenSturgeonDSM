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
delta_temp<-delta_temperature
#Water year type
WY<-CalSim3_metrics$WaterYearType

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
pred<-prop_high_predation #what is high predation?
categorize_pred <- function(x) {
  if (x < 0.30) {
    return("low")
  } else if (x <= 0.70) {
    return("medium")
  } else {
    return("high")
  }
}


setwd("C:/Users/gilliger/Box/Erin Dissertation/SIT sturgeon models/Green Sturgeon Model code/data")
entrain<- read.csv("EntrainmentProbs.csv")
entrain<-rbind(0,entrain)

setwd("C:/Users/gilliger/Box/Erin Dissertation/SIT sturgeon models/Green Sturgeon Model code/SensitivityAnalysis")
source("StartPopSim.R")
source("Movement.R")
source("HabCapacity.R")
source("CARRYINGCAP.R")
source("Spawning.R")
source("Survival.R")
source("SpawnDist.R")
source("stranding.R")
source("Entrainment.R")
#source("Old.Entrainment.R")
source("Ocean.R")
source("agePop.R")
source("setArray.R")
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


run.scenarios<-function(decis.X,runz,years,stochastic,parms.X,incr.X){
 
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
    
    # For starting population variation
    if(sum(parms.X == "startpop.vary")) {
      # Get the multiplier from the sensitivity analysis parameter
      pop_multiplier <- incr.X[parms.X == "startpop.vary"]
      
      # Generate a new starting population with the multiplier
      n.pop_modified <- 10000 * pop_multiplier  # Modify the base population size
      
      # Generate new initial population
      set.seed(1234)  # Keep the same seed for reproducibility
      n.init <- StartPop(n.pop = n.pop_modified, maxAge = 34, Natural.S = 0.85)
      
      # Apply the new initial population
      N0 <- round(matrix(c(n.init$n.age0*4,
                           n.init$n.age1*4,
                           n.init$n.age2*4,
                           n.init$n.juvs*4,
                           n.init$n.sub+n.init$n.marineJ,
                           n.init$n.sm.ad,
                           n.init$n.lg.ad), ncol=1))
      
      # Update the fish array with the new population
      st.fish <- set.array(N0)
    }
    
    #Reset diversions
    diversions<-contact_points;if(sum(parms.X == "river.div.vary")) {diversions <- round(contact_points* incr.X[parms.X == "river.div.vary"])}
    dltadivs<-delta_contact_points;if(sum(parms.X == "delta.div.vary")) {dltadivs <- round(delta_contact_points* incr.X[parms.X == "delta.div.vary"])}
    
    #reset habitat
    # For habitat
    Hab <- list(UpSac=UpSacHab, UpMidSac=UpMidSacHab, MidSac=MidSacHab, LowSac=LowSacHab,
                Feather=FeatherHab, Yuba=YubaHab, Bear=BearHab, SJ=SJHab)
    
    if(sum(parms.X == "habitat.vary")) {
      hab_multiplier <- incr.X[parms.X == "habitat.vary"]
      
      for(river in names(Hab)) {
        # Check which columns contain habitat area data
        # Different rivers have different column names/structures
        if("Spawning_sq.m" %in% colnames(Hab[[river]])) {
          Hab[[river]][["Spawning_sq.m"]] <- Hab[[river]][["Spawning_sq.m"]] * hab_multiplier
        }
        
        # Check for both possible column names for river use
        if("RiverUse_sq.m" %in% colnames(Hab[[river]])) {
          Hab[[river]][["RiverUse_sq.m"]] <- Hab[[river]][["RiverUse_sq.m"]] * hab_multiplier
        } else if("RiverUse_sq..m" %in% colnames(Hab[[river]])) {
          Hab[[river]][["RiverUse_sq..m"]] <- Hab[[river]][["RiverUse_sq..m"]] * hab_multiplier
        }
      }
    }
    
    #Reset any temperature and flow alterations
    stream_temp<-CalSim3_metrics$avg_temp;if(sum(parms.X == "avgStreamT.vary")) {stream_temp <- (CalSim3_metrics$avg_temp) * incr.X[parms.X == "avgStreamT.vary"]}
    delta_temp<-delta_temperature;if(sum(parms.X == "avgDeltaT.vary")) {delta_temp <- delta_temperature * incr.X[parms.X == "avgDeltaT.vary"]}
    Qmean<-35.31468*(CalSim3_metrics$mean_monthly_flow);if(sum(parms.X == "Qmean.vary")) {Qmean <- (35.31468*(CalSim3_metrics$mean_monthly_flow)) * incr.X[parms.X == "Qmean.vary"]}
 
   
    # Apply the function to the selected tributaries
    pred<-prop_high_predation ;if(sum(parms.X == "Predation.vary")) {pred <- prop_high_predation* incr.X[parms.X == "Predation.vary"]} 
    pred_cats <- sapply(pred[tribs2use], categorize_pred)
    
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
   
       #Every year set up a cumulative distribution for juvenile outmigration
    cumdistrib<-dist.migrators(Qmean, yr, parms=parms.X, incr=incr.X)
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
          spawn.freq<-0.25; if(sum(parms.X == "spawn.freq")) {spawn_freq <- qunif(incr.X[parms.X == "spawn.freq"], 0.2, 0.33)}
          ocean_small_migrants <- floor(st.fish[14,6] * spawn.freq)
          st.fish[13,6] <- st.fish[13,6] + ocean_small_migrants
          st.fish[14,6] <- st.fish[14,6] - ocean_small_migrants
          
          # Large adults
          ocean_large_migrants <- floor(st.fish[14,7] * spawn.freq)
          st.fish[13,7] <- st.fish[13,7] + ocean_large_migrants
          st.fish[14,7] <- st.fish[14,7] - ocean_large_migrants
          
          sm.spawners.spawn<-round(rtri(sum(st.fish[13,6]), min=3, max=6,mode=4.5))
          lg.spawners.spawn<-round(rtri(sum(st.fish[13,7]), min=3, max=6,mode=4.5))
          store.spawners<-c(length(sm.spawners.spawn),(length(lg.spawners.spawn)))
        }
        
        
        if(month == 3){
          
          #Assign routes and distribution of spawners
          spawnroute<-spawndist(spawners=c(sum(sm.spawners.spawn == 3),sum(lg.spawners.spawn == 3)),
                                Qmeans=Qmean[tribsDist,month,yr],decis=decis.X[[yr]],stochastic,
                                parms=parms.X, incr=incr.X)
          
          strand.success<-strand.sub(spawn.rt=spawnroute[1:4,],
                                     overtopYolo=CalSim3_metrics$gates_overtopped[month,yr,2],
                                     overtopSutter=CalSim3_metrics$gates_overtopped[month,yr,1],
                                     stochastic,parms=parms.X, incr=incr.X)
          
          #Fill in everybody
          spawners<-rbind(strand.success[[1]],spawnroute[5,])
          st.fish[1:5,6:7]<-st.fish[1:5,6:7]+spawners
          st.fish[8:9,6:7]<-strand.success[[2]]
          
          #Subtract the fish that initially left
          st.fish[13,6]<-st.fish[13,6]-sum(sm.spawners.spawn == 3) 
          st.fish[13,7]<-st.fish[13,7]-sum(lg.spawners.spawn == 3)
          
          #Apply carrying capacity
          cap.reached<-Capacity.sub(spawners=st.fish[1:5,6:7], flow=Qmean[tribs2use,month,yr],
                                    hab=Hab, stochastic, parms=parms.X, incr=incr.X)
          #Survival to age 0 production
          Egg.S<-Egg.Surv(avgT=stream_temp[tribs2use,month,yr],parms=parms.X, incr=incr.X)
          #Passage at Sunset pumps
          pass<-SunsetPassage(flow=Qmean[19,month,yr],year=yr,parms=parms.X, incr=incr.X)
          #Spawn the fish-apply carrying capacity directly if any
          larvae<-spawn(smAd=st.fish[1:5,6],lgAd=st.fish[1:5,7],discharge=Qmean[tribs2use,month,yr],
                        passage=pass,temp=stream_temp[tribs2use,month,yr],Egg.S=Egg.S,
                        hab.cap=cap.reached,prev.pass=numeric(2),stochastic,parms=parms.X, incr=incr.X)
          # Create spawning occurrence indicator
          spawning_occurred_3 <- c(as.integer(larvae[[1]] > 0))
          #Larval to age 0 survival
          age0<-Larvae.Surv(larvae=larvae[[1]],avgT=stream_temp[tribs2use,month,yr],
                            predation=pred_cats,stochastic,parms=parms.X, incr=incr.X) 
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
          extract.Strand<-strand.S(stranded=st.fish[8:9,6:7],decis=decis.X[[yr]],stochastic,parms=parms.X, incr=incr.X)
          st.fish[8:9,6:7]<-extract.Strand
        } 
        
        if(month == 4){
          
          #send the stranded out from previous month up first
          abandoned<-abandonment(stranded=st.fish[8:9,6:7],stochastic,parms=parms.X, incr=incr.X)
          st.fish[14,6]<-st.fish[14,6] + sum(abandoned[,1]) 
          st.fish[14,7]<-st.fish[14,7] + sum(abandoned[,2])   #put them back in the ocean
          st.fish[1,6]<- st.fish[1,6] + (sum(st.fish[8:9,6])-sum(abandoned[,1])) #up to spawn
          st.fish[1,7]<- st.fish[1,7] + (sum(st.fish[8:9,7])-sum(abandoned[,2]))
          st.fish[8:9,6:7]<- 0 #clear em out
          
          
          #Assign routes and distribution of spawners
          spawnroute<-spawndist(spawners=c(sum(sm.spawners.spawn == 4),sum(lg.spawners.spawn == 4)),
                                Qmeans=Qmean[tribsDist,month,yr],decis=decis.X[[yr]],stochastic, 
                                parms=parms.X, incr=incr.X)
          
          strand.success<-strand.sub(spawn.rt=spawnroute[1:4,],
                                     overtopYolo=CalSim3_metrics$gates_overtopped[month,yr,2],
                                     overtopSutter=CalSim3_metrics$gates_overtopped[month,yr,1],
                                     stochastic,parms=parms.X, incr=incr.X)
          
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
          cap.reached<-Capacity.sub(spawners=st.fish[1:5,6:7],flow=Qmean[tribs2use,month,yr],
                                    hab=Hab,stochastic,parms=parms.X, incr=incr.X)
          #Survival to age 0 production
          Egg.S<-Egg.Surv(avgT=stream_temp[tribs2use,month,yr],parms=parms.X, incr=incr.X)
          #Passage at Sunset pumps
          pass<-SunsetPassage(flow=Qmean[19,month,yr],year=yr,parms=parms.X, incr=incr.X)
          #Larvae
          larvae<-spawn(smAd=st.fish[1:5,6]-prev.smfish,lgAd=st.fish[1:5,7]-prev.lgfish,
                        discharge=Qmean[tribs2use,month,yr],passage=pass,
                        temp=stream_temp[tribs2use,month,yr],Egg.S=Egg.S,hab.cap=cap.reached,
                        prev.pass=passSunset,stochastic,parms=parms.X, incr=incr.X)
          
          # Create spawning occurrence indicator
          spawning_occurred_4 <- c(as.integer(larvae[[1]] > 0))
          
          #Larval to age 0 survival
          age0<-Larvae.Surv(larvae=larvae[[1]],avgT=stream_temp[tribs2use,month,yr],
                            predation=pred_cats,stochastic,parms=parms.X, incr=incr.X)  
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
          extract.Strand<-strand.S(stranded=st.fish[8:9,6:7],decis=decis.X[[yr]],stochastic,parms=parms.X, incr=incr.X)
          st.fish[8:9,6:7]<-extract.Strand
        }
        
        if(month == 5){
          
          #send the stranded out from previous month up first
          abandoned<-abandonment(stranded=st.fish[8:9,6:7],stochastic,parms=parms.X, incr=incr.X)
          st.fish[14,6]<-st.fish[14,6] + sum(abandoned[,1]) 
          st.fish[14,7]<-st.fish[14,7] + sum(abandoned[,2])   #put them back in the ocean
          st.fish[1,6]<- st.fish[1,6] + (sum(st.fish[8:9,6])-sum(abandoned[,1])) #up to spawn
          st.fish[1,7]<- st.fish[1,7] + (sum(st.fish[8:9,7])-sum(abandoned[,2]))
          st.fish[8:9,6:7]<- 0 #clear em out
          
 
          #Assign routes and distribution of spawners
          spawnroute<-spawndist(spawners=c(sum(sm.spawners.spawn == 5),sum(lg.spawners.spawn == 5)),
                                Qmeans=Qmean[tribsDist,month,yr],decis=decis.X[[yr]],stochastic,
                                parms=parms.X, incr=incr.X)
          
          strand.success<-strand.sub(spawn.rt=spawnroute[1:4,],
                                     overtopYolo=CalSim3_metrics$gates_overtopped[month,yr,2],
                                     overtopSutter=CalSim3_metrics$gates_overtopped[month,yr,1],
                                     stochastic,parms=parms.X, incr=incr.X)
          
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
          cap.reached<-Capacity.sub(spawners=st.fish[1:5,6:7],flow=Qmean[tribs2use,month,yr],
                                    hab=Hab,stochastic,parms=parms.X, incr=incr.X)
          #Passage at Sunset pumps
          pass<-SunsetPassage(flow=Qmean[19,month,yr],year=yr,parms=parms.X, incr=incr.X)
          #Survival to age 0 production
          Egg.S<-Egg.Surv(avgT=stream_temp[tribs2use,month,yr],parms=parms.X, incr=incr.X) 
          larvae<-spawn(smAd=st.fish[1:5,6]-prev.smfish,lgAd=st.fish[1:5,7]-prev.lgfish,
                        discharge=Qmean[tribs2use,month,yr],passage=pass,
                        temp=stream_temp[tribs2use,month,yr],Egg.S=Egg.S,hab.cap=cap.reached,
                        prev.pass=passSunset,stochastic, parms=parms.X, incr=incr.X)
          
          # Create spawning occurrence indicator
          spawning_occurred_5 <- c(as.integer(larvae[[1]] > 0))
          
          #Larval to age 0 survival
          age0<-Larvae.Surv(larvae=larvae[[1]],avgT=stream_temp[tribs2use,month,yr],
                            predation=pred_cats,stochastic, parms=parms.X, incr=incr.X) 
          
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
          extract.Strand<-strand.S(stranded=st.fish[8:9,6:7],decis=decis.X[[yr]],stochastic, parms=parms.X, incr=incr.X)
          st.fish[8:9,6:7]<-extract.Strand
        }
        
        if(month == 6){
          
          #send the stranded out from previous month up first
          abandoned<-abandonment(stranded=st.fish[8:9,6:7],stochastic,parms=parms.X, incr=incr.X)
          st.fish[14,6]<-st.fish[14,6] + sum(abandoned[,1]) 
          st.fish[14,7]<-st.fish[14,7] + sum(abandoned[,2])   #put them back in the ocean
          st.fish[1,6]<- st.fish[1,6] + (sum(st.fish[8:9,6])-sum(abandoned[,1])) #up to spawn
          st.fish[1,7]<- st.fish[1,7] + (sum(st.fish[8:9,7])-sum(abandoned[,2]))
          st.fish[8:9,6:7]<- 0 #clear em out
          
          
          #Assign routes and distribution of spawners
          spawnroute<-spawndist(spawners=c(sum(sm.spawners.spawn == 6),sum(lg.spawners.spawn == 6)),
                                Qmeans=Qmean[tribsDist,month,yr],decis=decis.X[[yr]],stochastic,
                                parms=parms.X, incr=incr.X)
          
          strand.success<-strand.sub(spawn.rt=spawnroute[1:4,],
                                     overtopYolo=CalSim3_metrics$gates_overtopped[month,yr,2],
                                     overtopSutter=CalSim3_metrics$gates_overtopped[month,yr,1],
                                     stochastic,parms=parms.X, incr=incr.X)
          
          
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
          cap.reached<-Capacity.sub(spawners=st.fish[1:5,6:7],flow=Qmean[tribs2use,month,yr],
                                    hab=Hab,stochastic, parms=parms.X, incr=incr.X)
          #Survival to age 0 production
          Egg.S<-Egg.Surv(avgT=stream_temp[tribs2use,month,yr],parms=parms.X, incr=incr.X) 
          #Passage at Sunset pumps
          pass<-SunsetPassage(flow=Qmean[19,month,yr],year=yr,parms=parms.X, incr=incr.X)
          #Larvae
          larvae<-spawn(smAd=st.fish[1:5,6]-prev.smfish,lgAd=st.fish[1:5,7]-prev.lgfish,
                        discharge=Qmean[tribs2use,month,yr],passage=pass,
                        temp=stream_temp[tribs2use,month,yr],Egg.S=Egg.S,hab.cap=cap.reached,
                        prev.pass=passSunset,stochastic, parms=parms.X, incr=incr.X)
          
          # Create spawning occurrence indicator
          spawning_occurred_6 <- c(as.integer(larvae[[1]] > 0))
          
          #Larval to age 0 survival
          age0<-Larvae.Surv(larvae=larvae[[1]],avgT=stream_temp[tribs2use,month,yr],
                            predation=pred_cats,stochastic,parms=parms.X, incr=incr.X) 
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
          extract.Strand<-strand.S(stranded=st.fish[8:9,6:7],decis=decis.X[[yr]],stochastic,parms=parms.X, incr=incr.X)
          st.fish[8:9,6:7]<-extract.Strand
        } 
        
        if(month == 7){ #not sending spawners up just clearing out any delayed spawners due to entrainment if there is any
          
          #send the stranded out from previous month up first
          abandoned<-abandonment(stranded=st.fish[8:9,6:7],stochastic, parms=parms.X, incr=incr.X)
          st.fish[14,6]<-st.fish[14,6] + sum(abandoned[,1]) 
          st.fish[14,7]<-st.fish[14,7] + sum(abandoned[,2])   #put them back in the ocean
          st.fish[1,6]<- st.fish[1,6] + (sum(st.fish[8:9,6])-sum(abandoned[,1])) #up to spawn
          st.fish[1,7]<- st.fish[1,7] + (sum(st.fish[8:9,7])-sum(abandoned[,2]))
          st.fish[8:9,6:7]<- 0 #clear em out
          
          #send age 0 upper sac to upper mid sac..#leave feather, yuba, bear,SJ in their tribs
          st.fish[6,1]<-st.fish[6,1]+ round(st.fish[1,1]) #*Up.mig.prob0*Up.migS.0)
          st.fish[1,1]<-0 #clear out previous age 0 because they all moved down
          
          #Apply carrying capacity
          cap.reached<-Capacity.sub(spawners=st.fish[1:5,6:7],flow=Qmean[tribs2use,month,yr],
                                    hab=Hab,stochastic,parms=parms.X, incr=incr.X)
          #Survival to age 0 production
          Egg.S<-Egg.Surv(avgT=stream_temp[tribs2use,month,yr],parms=parms.X, incr=incr.X) 
          #Passage at Sunset pumps
          pass<-SunsetPassage(flow=Qmean[19,month,yr],year=yr,parms=parms.X, incr=incr.X)
          #Larvae
          larvae<-spawn(smAd=st.fish[1:5,6]-prev.smfish,lgAd=st.fish[1:5,7]-prev.lgfish,
                        discharge=Qmean[tribs2use,month,yr],passage=pass,
                        temp=stream_temp[tribs2use,month,yr],Egg.S=Egg.S,
                        hab.cap=cap.reached,prev.pass=passSunset,stochastic,parms=parms.X, incr=incr.X)
          
          # Create spawning occurrence indicator
          spawning_occurred_7 <- c(as.integer(larvae[[1]] > 0))
          
          #Larval to age 0 survival
          age0<-Larvae.Surv(larvae=larvae[[1]],avgT=stream_temp[tribs2use,month,yr],
                            predation=pred_cats,stochastic,parms=parms.X, incr=incr.X)  
          st.fish[1:5,1]<-st.fish[1:5,1]+age0 #put them in the multidimensional array 
          
        } 
        
        if (month==8){
          #send last of age 0 fish to the upper mid sac
          st.fish[6,1]<-st.fish[6,1]+ round(st.fish[1,1]) #*Up.mig.prob0*Up.migS.0)
          st.fish[1,1]<-0 #clear out previous age 0 because they all moved down
          
          #Kick Early adult outmigration adults out
          prop_early.up<-Adult.Early(mean(Qmean[1,5:7,yr]),trib=c("UpSac"), stochastic, parms=parms.X, incr=incr.X)
          prop_early.F<-Adult.Early(mean(Qmean[19,5:7,yr]),trib=c("Feather"), stochastic, parms=parms.X, incr=incr.X)
          prop_early.Y<-Adult.Early(mean(Qmean[20,5:7,yr]),trib=c("Yuba"), stochastic, parms=parms.X, incr=incr.X)
          prop_early.B<-Adult.Early(mean(Qmean[18,5:7,yr]),trib=c("Bear"), stochastic, parms=parms.X, incr=incr.X)
          prop_early.SJ<-Adult.Early(mean(Qmean[31,5:7,yr]),trib=c("SJ"), stochastic, parms=parms.X, incr=incr.X)
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
          entrained<-entrain.deterministic(fish=st.fish[,1:3],prob.entrain=entrain[1:2],
                                           n.div=diversions, parms=parms.X, incr=incr.X)
          st.fish[1:10,1]<-pmax(0, st.fish[1:10,1]-entrained[[1]])
          st.fish[10,2]<-pmax(0,st.fish[10,2]-entrained[[2]])
          st.fish[10,3]<-pmax(0,st.fish[10,3]-entrained[[3]])
          #delta entrainment only apply to age 0-2 fish 
          NDelta.entrain<-delta.entrain.deterministic(fish=st.fish[11,1:3], prob.entrain=entrain[1:2],
                                                      n.div=dltadivs[1],parms=parms.X, incr=incr.X)
          st.fish[11,1:3]<-pmax(0,st.fish[11,1:3]-NDelta.entrain)
          SDelta.entrain<-delta.entrain.deterministic(fish=st.fish[12,1:3], prob.entrain=entrain[1:2],
                                                      n.div=dltadivs[2],parms=parms.X, incr=incr.X)
          st.fish[12,1:3]<-pmax(0,st.fish[12,1:3]-SDelta.entrain)
        } 
        
        ####Movement###
        
        ########OUtmigrate juveniles from tribs (once month gets to 3 send all the leftovers down)
        if(month >= 10 || month <= 3){
          # Set iteration year as needed
          iter_yr <- if(month %in% 1:3) yr + 1 else yr
          
          # Estimate habitat for each tributary
          pred.juv.UpSac <- estimate_juv_habitat(Qmean[16,month,yr], Hab$UpSac);
          pred.juv.Feather <- estimate_juv_habitat(Qmean[19,month,yr], Hab$Feather)
          pred.juv.Yuba <- estimate_juv_habitat(Qmean[20,month,yr], Hab$Yuba)
          pred.juv.Bear <- estimate_juv_habitat(Qmean[18,month,yr], Hab$Bear)
          pred.juv.SJ <- estimate_juv_habitat(Qmean[31,month,yr], Hab$SJ)
          
          if(sum(parms.X == "upsac.rearhab")) {
            bounds <- range(Hab$UpSac$RiverUse_sq..m)
            pred.juv.UpSac  <- qunif(incr.X[parms.X == "upsac.rearhab"], bounds[1], bounds[2])
          }
          
          if(sum(parms.X == "feather.rearhab")) {
            pred.juv.Feather <- qunif(incr.X[parms.X == "feather.rearhab"],pred.juv.Feather * 0.25, pred.juv.Feather * 1.75)  #wide bounds because no data
          }
          
          if(sum(parms.X == "yuba.rearhab")) {
            pred.juv.Yuba  <- qunif(incr.X[parms.X == "yuba.rearhab"],pred.juv.Yuba  * 0.25, pred.juv.Yuba  * 1.75)  #wide bounds because no data
          }
          
          if(sum(parms.X == "sj.rearhab")) {
            pred.juv.SJ  <- qunif(incr.X[parms.X == "sj.rearhab"],pred.juv.SJ  * 0.25, pred.juv.SJ  * 1.75)  #wide bounds because no data
          }
          
          # Calculate juvenile carrying capacity
          
          Juv_capacity <- c(
            Upper_Sac = juv_carrying_capacity(pred.juv.UpSac,stochastic,parms=parms.X, incr=incr.X),
            Feather = juv_carrying_capacity(pred.juv.Feather,stochastic,parms=parms.X, incr=incr.X),
            Yuba = juv_carrying_capacity(pred.juv.Yuba,stochastic,parms=parms.X, incr=incr.X),
            Bear = juv_carrying_capacity(pred.juv.Bear,stochastic,parms=parms.X, incr=incr.X),
            SJ = juv_carrying_capacity(pred.juv.SJ,stochastic,parms=parms.X, incr=incr.X)
          )
          
          probabilityUp<-cumdistrib2[1,month]
          probabilityFeath<-cumdistrib2[2,month]
          probabilityYuba<-cumdistrib2[3,month]
          probabilityBear<-cumdistrib2[4,month]
          probabilitySJ<-cumdistrib2[5,month]
          
          
          #Upper Sac
          UpSacmig<-migrate_and_check_capacity(sum(st.fish[6,1]),probabilityUp,
                                               Juv_capacity["Upper_Sac"],parms=parms.X, incr=incr.X)
          st.fish[6,1] <- UpSacmig$remaining
          st.fish[7,1] <- st.fish[7,1] + UpSacmig$stepped
          st.fish[10,1] <- st.fish[10,1] + UpSacmig$cont
          
          #Feather
          Featmig<-migrate_and_check_capacity(sum(st.fish[2,1]),probabilityFeath,
                                              Juv_capacity["Feather"],parms=parms.X, incr=incr.X)
          st.fish[2,1] <- Featmig$remaining
          st.fish[7,1] <- st.fish[7,1] + Featmig$stepped
          st.fish[10,1] <- st.fish[10,1] + Featmig$cont
          
          #Yuba
          Yubamig <- migrate_and_check_capacity(sum(st.fish[3,1]), probabilityYuba,
                                                Juv_capacity["Yuba"],parms=parms.X, incr=incr.X)
          st.fish[3,1] <- Yubamig$remaining
          st.fish[7,1] <- st.fish[7,1] + Yubamig$stepped
          st.fish[10,1] <- st.fish[10,1] + Yubamig$cont
          
          #Bear
          Bearmig <- migrate_and_check_capacity(sum(st.fish[4,1]), probabilityBear,
                                                Juv_capacity["Bear"],parms=parms.X, incr=incr.X)
          st.fish[4,1] <- Bearmig$remaining
          st.fish[7,1] <- st.fish[7,1] + Bearmig$stepped
          st.fish[10,1] <- st.fish[10,1] + Bearmig$cont
          
          #SJ
          SJmig <- migrate_and_check_capacity(sum(st.fish[5,1]), probabilitySJ, 
                                              Juv_capacity["SJ"],parms=parms.X, incr=incr.X)
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
          apply_juv_survival(params[[1]], params[[2]],curr_month=month, parms=parms.X,incr=incr.X)
          apply_adult_survival(i, params[[2]],curr_month=month,parms=parms.X,incr=incr.X)
        }
        
        # Apply survival rates for Sac regions (juvenile only, as per original code)
        sac_regions <- list(
          UpMid = list(6, 16),
          LowMid = list(7, 21),
          Low = list(10, 24)
        )
        
        for (region in sac_regions) {
          apply_juv_survival(region[[1]], month_yr_temps[region[[2]]],curr_month=month, parms=parms.X,incr=incr.X)
        }
        
        # Apply Delta survival rates 
        delta_regions <- list(
          N = list(11, 1),
          S = list(12, 2)
        )
        
        for (region in delta_regions) {
          apply_juv_survival(region[[1]], delta_temp[month, yr, region[[2]]], 
                             delta = TRUE, curr_month=month, parms=parms.X,incr=incr.X)
        }
        
        
        #########Lower Mid Sac to lower Sac (every month)
        LowMid.mig.prob<-0.59;  if(sum(parms.X == "lowmid.mig")) {LowMid.mig.prob  <- qunif(incr.X[parms.X == "lowmid.mig"],0.36, 1.0)}
        
        #send them down
        lowMidmigrants <- floor(st.fish[7,1:2]*LowMid.mig.prob)
        st.fish[10,1:2] <- st.fish[10,1:2] + lowMidmigrants 
        #Keep left overs
        st.fish[7,1:2] <- st.fish[7,1:2] - lowMidmigrants 
        
        #########lower Sac to North Delta
        Low.mig.prob0<-0.1;if(sum(parms.X == "low.mig0")) {Low.mig.prob0 <- qbeta(incr.X[parms.X == "low.mig0"],2.16, 16.31)}
        Low.mig.prob1<-0.25;if(sum(parms.X == "low.mig1")) {Low.mig.prob1 <- qbeta(incr.X[parms.X == "low.mig1"],3.14, 10.16)}
        Low.mig.prob2<-1; if(sum(parms.X == "low.mig2")) {Low.mig.prob2 <- qbeta(incr.X[parms.X == "low.mig2"],6.31, 0.37)}
        Low.mig.probJuv<-1; if(sum(parms.X == "low.migjuv")) {Low.mig.probJuv <- qbeta(incr.X[parms.X == "low.migjuv"],6.31, 0.37)}
        
        
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
        
        NDelta.mig.prob<-0.15; if(sum(parms.X == "n.delta.mig")) {NDelta.mig.prob<- qbeta(incr.X[parms.X == "n.delta.mig"],4.26, 20.51)}
       
        #send them down
        ndelta.migrants <- floor(st.fish[11,1:4]*NDelta.mig.prob)
        st.fish[12,1:4] <- st.fish[12,1:4] + ndelta.migrants
        #Keep left overs
        st.fish[11,1:4] <- st.fish[11,1:4] - ndelta.migrants
        
        #Back the other way
        SDelta.mig.prob <- 0.15; if(sum(parms.X == "s.delta.mig")) {SDelta.mig.prob<- qbeta(incr.X[parms.X == "s.delta.mig"],4.26, 20.51)}
        
        #send them down
        sdelta.migrants <- floor(st.fish[12,1:4]*SDelta.mig.prob)
        st.fish[11,1:4] <- st.fish[11,1:4] + sdelta.migrants
        #Keep left overs
        st.fish[12,1:4] <- st.fish[12,1:4] - sdelta.migrants
        
        
        ######Deltas to bays (kick any subadults out just incase they transitioned here)
        age1bay<-0.05;if(sum(parms.X == "age1.bay")) {age1bay<- qbeta(incr.X[parms.X == "age1.bay"],3.33, 35.096)}
        age2bay<-0.15;if(sum(parms.X == "age2.bay")) {age2bay<- qbeta(incr.X[parms.X == "age2.bay"],3.699,15.69)}
        juvbay<-0.35;if(sum(parms.X == "juv.bay")) {juvbay<- qbeta(incr.X[parms.X == "juv.bay"],11.02, 19.24)}
        
        Delta.mig.prob<-c(0,age1bay,age2bay,juvbay,1)
        
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
        age1back<-0.15;if(sum(parms.X == "age1.back")) {age1back<- qbeta(incr.X[parms.X == "age1.back"],3.22,25.88)}
        age2back<-0.10;if(sum(parms.X == "age2.back")) {age2back<- qbeta(incr.X[parms.X == "age2.back"],3.62,36.52)}
        juvback<-0.05;if(sum(parms.X == "juv.back")) {juvback<- qbeta(incr.X[parms.X == "juv.back"],3.33, 35.096)}
        Bay2deltas<-c(0,age1back,age2back,juvback)
        
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
        age1ocean<-0;if(sum(parms.X == "age1.ocean")) {age1ocean<- qbeta(incr.X[parms.X == "age1.ocean"],1.55, 46.28)}
        age2ocean<-0.15; if(sum(parms.X == "age2.ocean")) {age2ocean<- qbeta(incr.X[parms.X == "age2.ocean"],3.699, 15.69)}
        juvocean<-0.35; if(sum(parms.X == "juv.ocean")) {juvocean<- qbeta(incr.X[parms.X == "juv.ocean"],4.27, 8.9)}
        Bay.mig.prob<-c(0,age1ocean,age2ocean,juvocean,1)  
        
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
      st.fish<-AgingFunc(population=st.fish,parms=parms.X, incr=incr.X)
      #ocean survival function
      st.fish[14,]<-Ocean.Surv(fish=st.fish[14,],decis=decis.X[[yr]],stochastic,parms=parms.X, incr=incr.X)
      
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
} #end simulation loop
 
holdr<-as.data.frame(holdr)
return(holdr)
 
}#end function 


#It Works! yay
#run.scenarios(decis.X=1,runz=1,years=1,stochastic=0,incr.X=c(0.1),parms.X=c("bycatch.risk")) 


