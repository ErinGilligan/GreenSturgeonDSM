# # Function to estimate spawning habitat
# estimate_spawning_habitat <- function(flow, hab_data) {
#   approx(x = hab_data$Q..cfs, y = hab_data$Spawning_sq.m, xout = flow, rule = 2)$y
# }
# 
# # Estimate habitat for each tributary
# pred.spawn.UpSac <- estimate_spawning_habitat(Qmean[1,month,yr], Hab$UpSac)
# pred.spawn.Feather <- estimate_spawning_habitat(Qmean[19,month,yr], Hab$Feather)
# pred.spawn.Yuba <- estimate_spawning_habitat(Qmean[20,month,yr], Hab$Yuba)
# pred.spawn.Bear <- estimate_spawning_habitat(Qmean[18,month,yr], Hab$Bear)
# pred.spawn.SJ <- estimate_spawning_habitat(Qmean[31,month,yr], Hab$SJ)
# 
# spawn_space_requirement<-if(stochastic){runif(1,4.6,14)}else{9.3}
# 
# spawn_carrying_capacity <- function(habitat_area) {
#   round(habitat_area / spawn_space_requirement)
# }
# 
# spawner_capacity <- c(
#   Upper_Sac = spawn_carrying_capacity(pred.spawn.UpSac),
#   Feather = spawn_carrying_capacity(pred.spawn.Feather),
#   Yuba = spawn_carrying_capacity(pred.spawn.Yuba),
#   Bear = spawn_carrying_capacity(pred.spawn.Bear),
#   SJ = spawn_carrying_capacity(pred.spawn.SJ)
# )

##Estimate Juvenile habitat capacity####
estimate_juv_habitat <- function(flow, hab_data) {
  approx(x = hab_data$Q..cfs, y = hab_data$RiverUse_sq.m, xout = flow, rule = 2)$y
}


juv_carrying_capacity <- function(habitat_area,stochastic,incr,parms) {
  # Assuming juveniles need 1/5th of adult space (1.92 sq meters per juvenile)
  juvenile_space_requirement <- if(stochastic){runif(1,0.5,4.6)}else{1.92};if(sum(parms == "juv.cap")) juvenile_space_requirement<- qunif(incr[parms == "juv.cap"],0.5,48)
  floor(habitat_area / juvenile_space_requirement)
}



