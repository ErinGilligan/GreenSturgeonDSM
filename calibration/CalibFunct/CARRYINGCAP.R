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


juv_carrying_capacity <- function(habitat_area,stochastic) {
  # Assuming juveniles need 1/5th of adult space (1.92 sq meters per juvenile)
  juvenile_space_requirement <- if(stochastic){runif(1,0.5,4.6)}else{1.92}
  round(habitat_area / juvenile_space_requirement)
}



##Plots for SIT#### NOT USING YET (I DON'T REALLY LIKE THEM)

# # 1. Spawning Habitat vs Flow for each tributary
# par(mfrow = c(2, 3))
# tributaries <- c("UpSac", "Feather", "Yuba", "Bear", "SJ")
# for (trib in tributaries) {
#   plot(Hab[[trib]]$Q..cfs, Hab[[trib]]$Spawning_sq.m, 
#        type = "l", 
#        main = paste(trib, "Spawning Habitat"),
#        xlab = "Flow (cfs)", 
#        ylab = "Spawning Habitat (sq.m)")
#   points(Qmean[switch(trib,
#                       "UpSac" = 1,
#                       "Feather" = 19,
#                       "Yuba" = 20,
#                       "Bear" = 18,
#                       "SJ" = 31), month, yr],
#          get(paste0("pred.spawn.", trib)),
#          col = "red", pch = 19)
# }
# 
# # 2. Juvenile Habitat vs Flow for each tributary
# par(mfrow = c(2, 3))
# for (trib in tributaries) {
#   plot(Hab[[trib]]$Q..cfs, Hab[[trib]]$RiverUse_sq.m, 
#        type = "l", 
#        main = paste(trib, "Juvenile Habitat"),
#        xlab = "Flow (cfs)", 
#        ylab = "Juvenile Habitat (sq.m)")
#   points(Qmean[switch(trib,
#                       "UpSac" = 16,
#                       "Feather" = 19,
#                       "Yuba" = 20,
#                       "Bear" = 18,
#                       "SJ" = 31), month, yr],
#          get(paste0("pred.juv.", trib)),
#          col = "blue", pch = 19)
# }
# 
# # 3. Spawning vs Juvenile Carrying Capacity
# par(mfrow = c(1, 1))
# barplot(rbind(spawner_capacity, Juv_capacity), 
#         beside = TRUE, 
#         col = c("darkgreen", "lightgreen"),
#         main = "Spawning vs Juvenile Carrying Capacity",
#         xlab = "Tributary",
#         ylab = "Carrying Capacity (number of fish)",
#         names.arg = tributaries,
#         legend.text = c("Spawner", "Juvenile"))
# 
# # 4. Habitat Area vs Carrying Capacity
# par(mfrow = c(1, 2))
# plot(c(pred.spawn.UpSac, pred.spawn.Feather, pred.spawn.Yuba, pred.spawn.Bear, pred.spawn.SJ),
#      spawner_capacity,
#      xlab = "Spawning Habitat Area (sq.m)",
#      ylab = "Spawner Carrying Capacity",
#      main = "Spawning Habitat vs Capacity",
#      pch = 19, col = "darkgreen")
# text(c(pred.spawn.UpSac, pred.spawn.Feather, pred.spawn.Yuba, pred.spawn.Bear, pred.spawn.SJ),
#      spawner_capacity,
#      labels = tributaries,
#      pos = 4)
# 
# plot(c(pred.juv.UpSac, pred.juv.Feather, pred.juv.Yuba, pred.juv.Bear, pred.juv.SJ),
#      Juv_capacity,
#      xlab = "Juvenile Habitat Area (sq.m)",
#      ylab = "Juvenile Carrying Capacity",
#      main = "Juvenile Habitat vs Capacity",
#      pch = 19, col = "lightgreen")
# text(c(pred.juv.UpSac, pred.juv.Feather, pred.juv.Yuba, pred.juv.Bear, pred.juv.SJ),
#      Juv_capacity,
#      labels = tributaries,
#      pos = 4)
