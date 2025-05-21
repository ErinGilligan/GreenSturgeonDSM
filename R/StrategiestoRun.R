#####################################################################
#####          Decision combinations
#####################################################################
#
# DecName<-data.frame(Decision=c(
#   "do nothing",
#   "Alter temps to increase incubation temps in Feather River",
#   "Alter temps of rearing areas in Feather River",
#   "Alter temps to increase incubation temps in San Joaquin River",
#   "Alter temps of rearing areas in San Joaquin River",
#   "Screen 10% of diversions in Upper Sacramento River",
#   "Screen 10% of diversions in Feather River",
#   "Screen 10% of diversions in Yuba River",
#   "Screen 10% of diversions in San Joaquin River",
#   "Screen 10% of diversions in Upper Mid Sacramento River",
#   "Screen 10% of diversions in Lower Mid Sacramento River",
#   "Screen 10% of diversions in Lower Sacramento River",
#   "Screen 10% of diversions in North Delta",
#   "Screen 10% of diversions in South Delta",
#   "Rescue efforts for stranding at Yolo Bypass",
#   "Rescue efforts for stranding at Sutter Bypass",
#   "Increase spawning habitat by 2 acres in Feather River",
#   "Reduce Bycatch",
#   "Attraction flow pulse to Feather River for spawning",
#   "Attraction flow pulse to Yuba River for spawning",
#   "High in Channel flows in SJ River",
#   "High in channel flows in the Sacramento River",
#   "Passage at Daguerre Point Dam",
#   "Alter temps to increase incubation temps in Upper Sacramento River",
#   "Alter temps to increase incubation temps in Yuba River",
#   "Alter temps of rearing areas in the Upper Sacramento River",
#   "Alter temps of rearing areas in the Upper Mid Sacramento River",
#   "Increase spawning habitat by 2 acres in the Upper Sacramento River",
#   "Increase rearing habitat by 2 acres in Upper Sacramento River",
#   "Increase rearing habitat by 2 acres in Upper Mid Sacramento River",
#   "Spawning flow pulse in the Feather River",
#   "Spawning flow pulse in the Yuba River",
#   "Eliminate bycatch"
# ))
#
# StratName<-data.frame(Decision=c(
# "do_nothing",
# "Hassrick_strategy",
# "spawn_temp_strategy",
# "rear_temp_strategy",
# "trib_temp_strategy",
# "mainstem_temp_strategy",
# "juv_surv_strategy",
# "WY_adapt_strategy",
# "LowerSpawn_strategy",
# "LowerSurv_strategy",
# "Spawner_strategy",
# "habitat_strategy",
# "Screening_strategy",
# "Yuba_strategy",
# "Steel1_strategy",
# "Steel2_strategy",
# "Dudley_strategy"
# ))
# StratName<- data.frame(Decision = rep(StratName$Decision, each = 20))
#Load in Cal Sim 3 for Water year type data
#load("C:/Users/gilliger/Box/Erin Dissertation/SIT sturgeon models/Green Sturgeon Model code/CalSim3_metrics.rda")
#Wy<-CalSim3_metrics$WaterYearType

yrs<-20
###Strategy vector code####
# Strategy 1: Do nothing - No restoration
do_nothing_strategy <- list()
for(i in 1:yrs) {
  do_nothing_strategy[[i]] <- c(1)
}

# Strategy 2: Increase spawning habitat in Upper Sac, Feather, and Yuba/SJ, increase
#adult survival at bypasses and as bycatch, screen diversions in the
#Feather, Upper Sac, and Upper mid sac/lower mid sac.
Hassrick_strategy <- list()
for(i in 1:yrs) {
  if(i %% 2 == 1) {  # odd years
    Hassrick_strategy[[i]] <- c(17, 28, 33, 18, 15, 16, 7, 6, 10)
  } else {           # even years
    Hassrick_strategy[[i]] <- c(17, 28, 34, 18, 15, 16, 7, 6, 11)
  }
}
# Strategy 3: Manipulate spawning temperature in Upper Sacramento and Feather River
spawn_temp_strategy <- list()
for(i in 1:yrs) {
  spawn_temp_strategy[[i]] <- c(2, 24)
}

# Strategy 4: Manipulate rearing temperatures in Upper mid Sacramento and Feather River
rear_temp_strategy <- list()
for(i in 1:yrs) {
  rear_temp_strategy[[i]] <- c(3, 27)
}

# Strategy 5: Manipulate spawning temperature in the Feather and Yuba River
trib_temp_strategy <- list()
for(i in 1:yrs) {
  trib_temp_strategy[[i]] <- c(2, 25)
}

# Strategy 6: Regulate spawning temperatures in the Upper Sacramento and
#rearing temperature in the Upper Mid Sacramento
mainstem_temp_strategy <- list()
for(i in 1:yrs) {
  mainstem_temp_strategy[[i]] <- c(24, 27)
}

# Strategy 7: Regulate rearing tempertaures in the Upper Mid Sacramento, Screen
#diversions in Upper Sac, Upper mid Sac, Lower mid Sacramento, and Lower
#Sacramento, and Feather River rotating years, increase rearing habitat in the
#Upper Mid Sacramento and the Upper Sacramento
juv_surv_strategy <- list()
for(i in 1:yrs) {
  if(i == 1 || i == 11) {
    juv_surv_strategy[[i]] <- c(6, 10, 11, 27, 29, 30)
  } else if(i == 2 || i == 12) {
    juv_surv_strategy[[i]] <- c(6, 10, 12, 27, 29, 30)
  } else if(i == 3 || i == 13) {
    juv_surv_strategy[[i]] <- c(6, 10, 7, 27, 29, 30)
  } else if(i == 4 || i == 14) {
    juv_surv_strategy[[i]] <- c(6, 11, 12, 27, 29, 30)
  } else if(i == 5 || i == 15) {
    juv_surv_strategy[[i]] <- c(6, 11, 7, 27, 29, 30)
  } else if(i == 6 || i == 16) {
    juv_surv_strategy[[i]] <- c(6, 12, 7, 27, 29, 30)
  } else if(i == 7 || i == 17){
    juv_surv_strategy[[i]] <- c(10, 11, 12, 27, 29, 30)
  } else if(i == 8 || i == 18) {
    juv_surv_strategy[[i]] <- c(10, 11, 7, 27, 29, 30)
  } else if(i == 9 || i == 19) {
    juv_surv_strategy[[i]] <- c(10, 12, 7, 27, 29, 30)
  } else {
    juv_surv_strategy[[i]] <- c(11, 12, 7, 27, 29, 30)
  }
}

# Strategy 8: Attraction flow pulse to the Feather River in dry, critical, or below normal
#years; wet and above normal years spawning temperature manipulation in the Feather and Yuba
WY_adapt_strategy <- list()
for(i in 1:yrs) {
  if(i %in% c(2, 6, 8:13, 15)) {
    WY_adapt_strategy[[i]] <- c(19)
  }else{
    WY_adapt_strategy[[i]] <- c(2,25)
  }
}


# Strategy 9: high in channel flows SJ dry years; wet years manipulate temps during
#rearing and spawning
LowerSpawn_strategy <- list()
for(i in 1:yrs) {
  if(i %in% c(2, 6, 8:13, 15)) {
    LowerSpawn_strategy[[i]] <- c(21)
  }else{
    LowerSpawn_strategy[[i]] <- c(4,5)
  }
}

# Strategy 10: manipulate rearing temperatures in San Joaquin every year, screen
#diversions in San joaquin, south delta, north delta, and lower river. Add rearing habitat in the SJ
LowerSurv_strategy <- list()
for(i in 1:yrs) {
  LowerSurv_strategy[[i]] <- c(5, 9, 12, 13, 14, 35)
}

# Strategy 11: Adult rescue efforts at yolo and sutter bypass yearly; increase spawning
#habitat in the Upper Sacramento and the Feather River, increase rearing
#habitat in the Upper Mid Sacramento; Alter spawning temperatures in
#Upper Sacramento River
Spawner_strategy <- list()
for(i in 1:yrs) {
  Spawner_strategy[[i]] <- c(15, 16, 28, 17, 30, 24)
}

# Strategy 12: Increase spawning and rearing habitat in the Upper Sacramento, increase
#rearing habitat in the Upper mid Sacramento, increase spawning and rearing habitat in the Feather River. For wet years manipulate spawning
#temp in Feather,dry years manipulate temp in Upper Sac
habitat_strategy <- list()
for(i in 1:yrs) {
  if(i %in% c(2, 6, 8:13, 15)) {
    habitat_strategy[[i]] <- c(28, 29, 30, 17, 36, 24)
  }else{
    habitat_strategy[[i]] <- c(28, 29, 30, 17, 36, 2)
  }
}

# Strategy 13: Screen 10% of diversions in every river segment every year and add rearing
#habitat in the upper mid sac
Screening_strategy <- list()
for(i in 1:yrs) {
  Screening_strategy[[i]] <- c(6:14,30)
}

# Strategy 14: Dry years attraction flow pulse to yuba; wet years manipulate
#spawning temperature in the yuba, screen diversions in yuba, lower mid sac, lower sac
Yuba_strategy <- list()
for(i in 1:yrs) {
  if(i %in% c(2, 6, 8:13, 15)) {
    Yuba_strategy[[i]] <- c(20)
  }else{
    Yuba_strategy[[i]] <- c(25,8,11,12)
  }
}

# Strategy 15: Add rearing habitat in Upper, Upper-Mid Sac,
#and Feather River. screen in upper, upper mid, feather,lower mid sac, lower sac, yuba, san joquin
Steel1_strategy <- list()
for(i in 1:yrs) {
  Steel1_strategy[[i]] <- c(29,30,36,6,10,7,11,12,8,9)
}

# Strategy 16: Dry years attraction flow pulse to yuba; wet years manipulate
#spawning temperature in the feather, screen diversions in feather, upper sac, upper mid sac,
#increase rearing habitat in the upper mid sac and feather
Steel2_strategy <- list()
for(i in 1:yrs) {
  if(i %in% c(2, 6, 8:13, 15)) {
    Steel2_strategy[[i]] <- c(20)
  }else{
    Steel2_strategy[[i]] <- c(2,7,10,6,36,30)
  }
}

# Strategy 17: Reduce bycatch mortality, reduce stranding at yolo bypass and sutter bypass
Dudley_strategy <- list()
for(i in 1:yrs) {
  Dudley_strategy[[i]] <- c(15,16,37)
}

###Decision combinations in a list####
decis.cmb<-list(
  "do_nothing" = do_nothing_strategy,
  "Hassrick_strategy" = Hassrick_strategy,
  "spawn_temp_strategy" = spawn_temp_strategy,
  "rear_temp_strategy" = rear_temp_strategy,
  "trib_temp_strategy" = trib_temp_strategy,
  "mainstem_temp_strategy"= mainstem_temp_strategy,
  "juv_surv_strategy" = juv_surv_strategy,
  "WY_adapt_strategy" = WY_adapt_strategy,
  "LowerSpawn_strategy" = LowerSpawn_strategy,
  "LowerSurv_strategy" = LowerSurv_strategy,
  "Spawner_strategy" = Spawner_strategy,
  "habitat_strategy" = habitat_strategy,
  "Screening_strategy" = Screening_strategy,
  "Yuba_strategy" = Yuba_strategy,
  "Steel1_strategy" = Steel1_strategy,
  "Steel2_strategy" = Steel2_strategy,
  "Dudley_strategy" = Dudley_strategy)
