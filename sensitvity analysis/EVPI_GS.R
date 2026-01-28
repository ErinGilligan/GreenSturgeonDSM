setwd("~/GreenSturgeonDSM/R")
source("strategiestoRun.R")
setwd("~/GreenSturgeonDSM/sensitivity analysis")
source("GSmodsens.R")
sen.table<-read.csv("GSsensParms.csv", stringsAsFactors = F)

library(parallel)
library(foreach)
library(doParallel)
library(actuar)

# set up parallel
cl <- makeCluster(detectCores() - 1)
registerDoParallel(cl)

# Export necessary variables and functions to the cluster
all_funcs <- lsf.str()
clusterExport(cl, c("run.scenarios", all_funcs,"rtri"))
clusterExport(cl, c("Qmean", "stream_temp", "Hab", "diversions", "dltadivs", "pred",
                    "entrain", "tribs2use", "tribsDist", "N0", "st.fish",
                    "delta_temperature","contact_points","delta_contact_points",
                    "UpSacHab",'UpMidSacHab','MidSacHab','LowSacHab','FeatherHab',
                    'YubaHab','BearHab','SJHab','CalSim3_metrics','prop_high_predation'))

parm<-sen.table$Parameter
# Create dummy vector because one way
dumm<-rep("XXX",length(parm))
parm<-cbind(parm,dumm)
incr<-cbind(seq(0.05,0.95,by=0.01),0.5)

decis.df <- data.frame(matrix(nrow=yrs, ncol=length(decis.cmb)))
colnames(decis.df) <- names(decis.cmb)

# Fill the data frame
for(strat in names(decis.cmb)) {
  decis.df[[strat]] <- sapply(decis.cmb[[strat]],
                              function(x) paste(x, collapse=","))
}

# Modify how you set up parameter combinations
param_combinations <- list()
counter <- 1
for (strat_name in names(decis.cmb)) {
  current_strategy <- decis.cmb[[strat_name]]

  for(pp in 1:nrow(parm)) {
    for(pct in 1:nrow(incr)) {
      param_combinations[[counter]] <- list(
        strategy_name = strat_name,
        strategy = current_strategy,
        param1 = parm[pp,1],
        param2 = parm[pp,2],
        incr1 = incr[pct,1],
        incr2 = incr[pct,2]
      )
      counter <- counter + 1
    }
  }
}

# Run sensitivity analysis in parallel
one.way_results <- foreach(combo = param_combinations, .combine = rbind, .packages = c("base")) %dopar% {
  # Run the scenario with the specific strategy
  util.score <- run.scenarios(
    decis.X = combo$strategy,
    runz = 1,
    years = 20,
    stochastic = 0,
    parms.X = c(combo$param1, combo$param2),
    incr.X = c(combo$incr1, combo$incr2)
  )

  # Return results with strategy name included
  data.frame(
    Strategy = combo$strategy_name,
    Util = util.score,
    parm1 = combo$param1,
    parm2 = combo$param2,
    incr1 = combo$incr1,
    incr2 = combo$incr2
  )
}

# Stop the cluster
stopCluster(cl)

#write.csv(one.way_results, "~/GreenSturgeonDSM/sensitivity analysis/EVPI_res.csv", row.names = FALSE)


###ACTUAL CALCULATION####
library(actuar)
one.way_results<-read.csv("~/GreenSturgeonDSM/sensitivity analysis/EVPI_res.csv")

# EVPI calc for population abundance (Util.V1)
#Clean up data
one.way <- one.way_results[,c("Strategy","Util.V1","parm1","incr1")]
names(one.way) <- c("decision","abundance","parameter","increment")

 strategy_means <- aggregate(one.way$abundance,
                             by = list(one.way$decision),
                             FUN = mean)
 names(strategy_means) <- c("Strategy", "Mean_Abundance")  # Give proper column names

 # This best_abundance - the maximum mean abundance under uncertainty
 best_abundance <- max(strategy_means$Mean_Abundance)

 # Now you can check
 print(paste("Best abundance under uncertainty:", best_abundance))

 # You can also see which strategy is best
 best_strategy <- strategy_means[which.max(strategy_means$Mean_Abundance), ]
 print(paste("Best strategy:", best_strategy$Strategy))

# Calculate EVPI for each parameter
# assign probabilities to each increment value
# I'm Assuming equal probability of each increment since they're uniform

x <- seq(0.05, 0.95, 0.01)

#First do spawner distribution submodel

# Set up for spawner distribution parameters
# Initialize spawner tada dataframe
spawner_tada <- data.frame()

# Feather spawn intercept (Normal distribution)
tmp <- subset(one.way, parameter=="Feather.spawnInt")
# Your code uses: qnorm(incr, F0, abs(F0*0.2)) where F0 = -8.0655
mean_val <- -8.0655
sd_val <- abs(-8.0655 * 0.2)  # 20% of absolute value
min_val <- qnorm(0.001, mean_val, sd_val)
max_val <- qnorm(0.999, mean_val, sd_val)
probs <- discretize(pnorm(x, mean_val, sd_val), method = "upper",
                    from = min_val, to = max_val, step = (max_val - min_val)/length(x))
probs <- probs/sum(probs)
plate <- data.frame(increment=x, probs=probs)
tmp <- merge(tmp, plate)
spawner_tada <- rbind(spawner_tada, tmp)


# Feather Q spawn (Normal distribution)
tmp <- subset(one.way, parameter=="FeatherQspawn")
# These come from my sensitivity code
mean_val <- 0.0022
sd_val <- abs(0.0022 * 0.1)  # 10% of absolute value
min_val <- qnorm(0.001, mean_val, sd_val)
max_val <- qnorm(0.999, mean_val, sd_val)
probs <- discretize(pnorm(x, mean_val, sd_val), method = "upper",
                    from = min_val, to = max_val, step = (max_val - min_val)/length(x))
probs <- probs/sum(probs)
plate <- data.frame(increment=x, probs=probs)
tmp <- merge(tmp, plate)
spawner_tada <- rbind(spawner_tada, tmp)

# Yuba spawn intercept (Normal distribution)
tmp <- subset(one.way, parameter=="Yuba.spawnInt")
# These come from my sensitivity code
mean_val <- -9.8833
sd_val <- abs(-9.8833 * 0.2)
min_val <- qnorm(0.001, mean_val, sd_val)
max_val <- qnorm(0.999, mean_val, sd_val)
probs <- discretize(pnorm(x, mean_val, sd_val), method = "upper",
                    from = min_val, to = max_val, step = (max_val - min_val)/length(x))
probs <- probs/sum(probs)
plate <- data.frame(increment=x, probs=probs)
tmp <- merge(tmp, plate)
spawner_tada <- rbind(spawner_tada, tmp)

# Yuba Q spawn (Normal distribution)
tmp <- subset(one.way, parameter=="YubaQspawn")
# These come from my sensitivity code
mean_val <- 0.0028
sd_val <- abs(0.0028 * 0.1)
min_val <- qnorm(0.001, mean_val, sd_val)
max_val <- qnorm(0.999, mean_val, sd_val)
probs <- discretize(pnorm(x, mean_val, sd_val), method = "upper",
                    from = min_val, to = max_val, step = (max_val - min_val)/length(x))
probs <- probs/sum(probs)
plate <- data.frame(increment=x, probs=probs)
tmp <- merge(tmp, plate)
spawner_tada <- rbind(spawner_tada, tmp)

#San Joaquin spawn intercept (Normal distribution)
tmp <- subset(one.way, parameter=="SJ.spawnInt")
# from sens code
mean_val <- -7.2096
sd_val <- abs(-7.2096 * 0.2)
min_val <- qnorm(0.001, mean_val, sd_val)
max_val <- qnorm(0.999, mean_val, sd_val)
probs <- discretize(pnorm(x, mean_val, sd_val), method = "upper",
                    from = min_val, to = max_val, step = (max_val - min_val)/length(x))
probs <- probs/sum(probs)
plate <- data.frame(increment=x, probs=probs)
tmp <- merge(tmp, plate)
spawner_tada <- rbind(spawner_tada, tmp)

# San Joaquin Q spawn (Normal distribution)
tmp <- subset(one.way, parameter=="SJQspawn")
# from sens code
mean_val <- 0.0023
sd_val <- abs(0.0023 * 0.1)
min_val <- qnorm(0.001, mean_val, sd_val)
max_val <- qnorm(0.999, mean_val, sd_val)
probs <- discretize(pnorm(x, mean_val, sd_val), method = "upper",
                    from = min_val, to = max_val, step = (max_val - min_val)/length(x))
probs <- probs/sum(probs)
plate <- data.frame(increment=x, probs=probs)
tmp <- merge(tmp, plate)
spawner_tada <- rbind(spawner_tada, tmp)


# Spawn proportion Feather (Beta distribution)
tmp <- subset(one.way, parameter=="SpawnProp.feather")
alpha_val <- 1.61
beta_val <- 17.11
min_val <- 0
max_val <- 1
probs <- discretize(pbeta(x, alpha_val, beta_val), method = "upper",
                    from = min_val, to = max_val, step = max_val/length(x))
probs <- probs/sum(probs)
plate <- data.frame(increment=x, probs=probs)
tmp <- merge(tmp, plate)
spawner_tada <- rbind(spawner_tada, tmp)

# Spawn proportion Yuba (Beta distribution)
tmp <- subset(one.way, parameter=="SpawnProp.yuba")
alpha_val <- 1.37
beta_val <- 15.8
min_val <- 0
max_val <- 1
probs <- discretize(pbeta(x, alpha_val, beta_val), method = "upper",
                    from = min_val, to = max_val, step = max_val/length(x))
probs <- probs/sum(probs)
plate <- data.frame(increment=x, probs=probs)
tmp <- merge(tmp, plate)
spawner_tada <- rbind(spawner_tada, tmp)

# Spawn proportion San Joaquin (Beta distribution)
tmp <- subset(one.way, parameter=="SpawnProp.sj")
alpha_val <- 1.37
beta_val <- 15.8
min_val <- 0
max_val <- 1
probs <- discretize(pbeta(x, alpha_val, beta_val), method = "upper",
                    from = min_val, to = max_val, step = max_val/length(x))
probs <- probs/sum(probs)
plate <- data.frame(increment=x, probs=probs)
tmp <- merge(tmp, plate)
spawner_tada <- rbind(spawner_tada, tmp)

# Calculate EVPI for spawner parameters
spawner_summ <- aggregate(spawner_tada[c(3)], by = spawner_tada[c(1,4,5)], max)
spawner_summ$w.abundance <- spawner_summ$abundance * spawner_summ$probs
spawner_new.summ <- aggregate(spawner_summ["w.abundance"], by = spawner_summ["parameter"], FUN = sum)
spawner_new.summ$best_abundance <- best_abundance
spawner_new.summ$EVPI <- spawner_new.summ$w.abundance - spawner_new.summ$best_abundance

print(spawner_new.summ[order(spawner_new.summ$EVPI, decreasing = TRUE),])

mean(spawner_new.summ$EVPI)

###Do spawner submodel now####
spawning_tada <- data.frame()

# Small female fecundity (Uniform distribution)
tmp <- subset(one.way, parameter=="sm.Fecund")
min_val <- 39600
max_val <- 130278
probs <- discretize(punif(x, min_val, max_val), method = "upper",
                    from = min_val, to = max_val, step = (max_val - min_val)/length(x))
probs <- probs/sum(probs)
plate <- data.frame(increment=x, probs=probs)
tmp <- merge(tmp, plate)
spawning_tada <- rbind(spawning_tada, tmp)

# Large female fecundity (Uniform distribution)
tmp <- subset(one.way, parameter=="lg.Fecund")
min_val <- 130278
max_val <- 242000
probs <- discretize(punif(x, min_val, max_val), method = "upper",
                    from = min_val, to = max_val, step = (max_val - min_val)/length(x))
probs <- probs/sum(probs)
plate <- data.frame(increment=x, probs=probs)
tmp <- merge(tmp, plate)
spawning_tada <- rbind(spawning_tada, tmp)

# Sex ratio (Beta distribution)
tmp <- subset(one.way, parameter=="sex.ratio")
male_ratio <- 1.38 / (1 + 1.38)  # = 0.579
alpha_val <- 100 * (1 - male_ratio)  # = 100 * 0.421 = 42.1
beta_val <- 100 * male_ratio        # = 100 * 0.579 = 57.9
min_val <- 0
max_val <- 1
probs <- discretize(pbeta(x, alpha_val, beta_val), method = "upper",
                    from = min_val, to = max_val, step = max_val/length(x))
probs <- probs/sum(probs)
plate <- data.frame(increment=x, probs=probs)
tmp <- merge(tmp, plate)
spawning_tada <- rbind(spawning_tada, tmp)

# Spawn discharge low threshold (Uniform distribution)
tmp <- subset(one.way, parameter=="SpawnQ.low")
min_val <- 2000
max_val <- 5000
probs <- discretize(punif(x, min_val, max_val), method = "upper",
                    from = min_val, to = max_val, step = (max_val - min_val)/length(x))
probs <- probs/sum(probs)
plate <- data.frame(increment=x, probs=probs)
tmp <- merge(tmp, plate)
spawning_tada <- rbind(spawning_tada, tmp)

# Spawn discharge upper threshold (Uniform distribution)
tmp <- subset(one.way, parameter=="SpawnQ.high")
min_val <- 35000
max_val <- 45000
probs <- discretize(punif(x, min_val, max_val), method = "upper",
                    from = min_val, to = max_val, step = (max_val - min_val)/length(x))
probs <- probs/sum(probs)
plate <- data.frame(increment=x, probs=probs)
tmp <- merge(tmp, plate)
spawning_tada <- rbind(spawning_tada, tmp)

# Spawn temperature low threshold (Uniform distribution)
tmp <- subset(one.way, parameter=="Spawntemp.low")
min_val <- 8.0
max_val <- 11.0
probs <- discretize(punif(x, min_val, max_val), method = "upper",
                    from = min_val, to = max_val, step = (max_val - min_val)/length(x))
probs <- probs/sum(probs)
plate <- data.frame(increment=x, probs=probs)
tmp <- merge(tmp, plate)
spawning_tada <- rbind(spawning_tada, tmp)

# Spawn temperature upper threshold (Uniform distribution)
tmp <- subset(one.way, parameter=="Spawntemp.high")
min_val <- 16.0
max_val <- 19.0
probs <- discretize(punif(x, min_val, max_val), method = "upper",
                    from = min_val, to = max_val, step = (max_val - min_val)/length(x))
probs <- probs/sum(probs)
plate <- data.frame(increment=x, probs=probs)
tmp <- merge(tmp, plate)
spawning_tada <- rbind(spawning_tada, tmp)

# Calculate EVPI for spawning parameters
spawning_summ <- aggregate(spawning_tada[c(3)], by = spawning_tada[c(1,4,5)], max)
spawning_summ$w.abundance <- spawning_summ$abundance * spawning_summ$probs
spawning_new.summ <- aggregate(spawning_summ["w.abundance"], by = spawning_summ["parameter"], FUN = sum)
spawning_new.summ$best_abundance <- best_abundance
spawning_new.summ$EVPI <- spawning_new.summ$w.abundance - spawning_new.summ$best_abundance

print("Spawning Submodel EVPI Results:")
print(spawning_new.summ[order(spawning_new.summ$EVPI, decreasing = TRUE),])
mean(spawning_new.summ$EVPI)

####Egg survival submodel####
egg_survival_tada <- data.frame()

# Egg survival intercept (Normal distribution)
tmp <- subset(one.way, parameter=="eggS.int")
mean_val <- -10.5
sd_val <- abs(-10.5 * 0.2)  # 20% of absolute value
min_val <- qnorm(0.001, mean_val, sd_val)
max_val <- qnorm(0.999, mean_val, sd_val)
probs <- discretize(pnorm(x, mean_val, sd_val), method = "upper",
                    from = min_val, to = max_val, step = (max_val - min_val)/length(x))
probs <- probs/sum(probs)
plate <- data.frame(increment=x, probs=probs)
tmp <- merge(tmp, plate)
egg_survival_tada <- rbind(egg_survival_tada, tmp)

# Egg temperature coefficient (Normal distribution)
tmp <- subset(one.way, parameter=="egg.tmp")
mean_val <- 0.72
sd_val <- abs(0.72 * 0.1)  # 10% of absolute value
min_val <- qnorm(0.001, mean_val, sd_val)
max_val <- qnorm(0.999, mean_val, sd_val)
probs <- discretize(pnorm(x, mean_val, sd_val), method = "upper",
                    from = min_val, to = max_val, step = (max_val - min_val)/length(x))
probs <- probs/sum(probs)
plate <- data.frame(increment=x, probs=probs)
tmp <- merge(tmp, plate)
egg_survival_tada <- rbind(egg_survival_tada, tmp)

# Maximum egg survival (Beta distribution)
tmp <- subset(one.way, parameter=="Egg.S")
alpha_val <- 3.51
beta_val <- 1667.137
min_val <- 0
max_val <- 1
probs <- discretize(pbeta(x, alpha_val, beta_val), method = "upper",
                    from = min_val, to = max_val, step = max_val/length(x))
probs <- probs/sum(probs)
plate <- data.frame(increment=x, probs=probs)
tmp <- merge(tmp, plate)
egg_survival_tada <- rbind(egg_survival_tada, tmp)

# Calculate EVPI for egg survival parameters
egg_survival_summ <- aggregate(egg_survival_tada[c(3)], by = egg_survival_tada[c(1,4,5)], max)
egg_survival_summ$w.abundance <- egg_survival_summ$abundance * egg_survival_summ$probs
egg_survival_new.summ <- aggregate(egg_survival_summ["w.abundance"], by = egg_survival_summ["parameter"], FUN = sum)
egg_survival_new.summ$best_abundance <- best_abundance
egg_survival_new.summ$EVPI <- egg_survival_new.summ$w.abundance - egg_survival_new.summ$best_abundance

print("Egg Survival Submodel EVPI Results:")
print(egg_survival_new.summ[order(egg_survival_new.summ$EVPI, decreasing = TRUE),])
mean(egg_survival_new.summ$EVPI)

####Larvae survival submodel now####

larvae_survival_tada <- data.frame()

# Larvae survival intercept (Normal distribution)
tmp <- subset(one.way, parameter=="larvaeS.int")
mean_val <- -15.7102
sd_val <- 1.26
min_val <- qnorm(0.001, mean_val, sd_val)
max_val <- qnorm(0.999, mean_val, sd_val)
probs <- discretize(pnorm(x, mean_val, sd_val), method = "upper",
                    from = min_val, to = max_val, step = (max_val - min_val)/length(x))
probs <- probs/sum(probs)
plate <- data.frame(increment=x, probs=probs)
tmp <- merge(tmp, plate)
larvae_survival_tada <- rbind(larvae_survival_tada, tmp)

# Larvae temperature coefficient (Normal distribution)
tmp <- subset(one.way, parameter=="larvae.tmp")
mean_val <- 0.6216
sd_val <- 0.05
min_val <- qnorm(0.001, mean_val, sd_val)
max_val <- qnorm(0.999, mean_val, sd_val)
probs <- discretize(pnorm(x, mean_val, sd_val), method = "upper",
                    from = min_val, to = max_val, step = (max_val - min_val)/length(x))
probs <- probs/sum(probs)
plate <- data.frame(increment=x, probs=probs)
tmp <- merge(tmp, plate)
larvae_survival_tada <- rbind(larvae_survival_tada, tmp)

#Larvae baseline mortality (Beta distribution)
tmp <- subset(one.way, parameter=="larvae.Mort")
alpha_val <- 15.7176
beta_val <- 3.8456
min_val <- 0
max_val <- 1
probs <- discretize(pbeta(x, alpha_val, beta_val), method = "upper",
                     from = min_val, to = max_val, step = (max_val-min_val)/length(x))
probs <- probs/sum(probs)
plate <- data.frame(increment=x, probs=probs)
tmp <- merge(tmp, plate)
larvae_survival_tada <- rbind(larvae_survival_tada, tmp)

#Larvae cold mortality (Beta distribution)
tmp <- subset(one.way, parameter=="larvae.cold")
alpha_val <- 15.72
beta_val <- 3.85
min_val <- 0
max_val <- 1
probs <- discretize(pbeta(x, alpha_val, beta_val), method = "upper",
                    from = min_val, to = max_val, step = (max_val-min_val)/length(x))
probs <- probs/sum(probs)
plate <- data.frame(increment=x, probs=probs)
tmp <- merge(tmp, plate)
larvae_survival_tada <- rbind(larvae_survival_tada, tmp)

# High predation effect (Beta distribution)
tmp <- subset(one.way, parameter=="high.pred")
alpha_val <- 4.48
beta_val <- 8.96
min_val <- 0
max_val <- 1
probs <- discretize(pbeta(x, alpha_val, beta_val), method = "upper",
                    from = min_val, to = max_val, step = (max_val-min_val)/length(x))
probs <- probs/sum(probs)
plate <- data.frame(increment=x, probs=probs)
tmp <- merge(tmp, plate)
larvae_survival_tada <- rbind(larvae_survival_tada, tmp)

# Medium predation effect (Beta distribution)
tmp <- subset(one.way, parameter=="med.pred")
alpha_val <- 5.31
beta_val <- 14.02
min_val <- 0
max_val <- 1
probs <- discretize(pbeta(x, alpha_val, beta_val), method = "upper",
                    from = min_val, to = max_val, step = (max_val-min_val)/length(x))
probs <- probs/sum(probs)
plate <- data.frame(increment=x, probs=probs)
tmp <- merge(tmp, plate)
larvae_survival_tada <- rbind(larvae_survival_tada, tmp)

# Low predation effect (Beta distribution)
tmp <- subset(one.way, parameter=="low.pred")
alpha_val <- 1.376
beta_val <- 15.8
min_val <- 0
max_val <- 1
probs <- discretize(pbeta(x, alpha_val, beta_val), method = "upper",
                    from = min_val, to = max_val, step = (max_val-min_val)/length(x))
probs <- probs/sum(probs)
plate <- data.frame(increment=x, probs=probs)
tmp <- merge(tmp, plate)
larvae_survival_tada <- rbind(larvae_survival_tada, tmp)

# Calculate EVPI for egg survival parameters
larvae_survival_summ <- aggregate(larvae_survival_tada[c(3)], by = larvae_survival_tada[c(1,4,5)], max)
larvae_survival_summ$w.abundance <- larvae_survival_summ$abundance * larvae_survival_summ$probs
larvae_survival_new.summ <- aggregate(larvae_survival_summ["w.abundance"], by = larvae_survival_summ["parameter"], FUN = sum)
larvae_survival_new.summ$best_abundance <- best_abundance
larvae_survival_new.summ$EVPI <- larvae_survival_new.summ$w.abundance - larvae_survival_new.summ$best_abundance

print("larvae Survival Submodel EVPI Results:")
print(larvae_survival_new.summ[order(larvae_survival_new.summ$EVPI, decreasing = TRUE),])
mean(larvae_survival_new.summ$EVPI)

####Juvenile migration####
juv_migration_tada <- data.frame()

# Upper Sacramento juvenile migration parameters
# UpSac juvenile migration intercept (Normal distribution)
tmp <- subset(one.way, parameter=="UpSacjuvmig.int")
mean_val <- -5.9741
sd_val <- abs(-5.9741 * 0.2)
min_val <- qnorm(0.001, mean_val, sd_val)
max_val <- qnorm(0.999, mean_val, sd_val)
probs <- discretize(pnorm(x, mean_val, sd_val), method = "upper",
                    from = min_val, to = max_val, step = (max_val - min_val)/length(x))
probs <- probs/sum(probs)
plate <- data.frame(increment=x, probs=probs)
tmp <- merge(tmp, plate)
juv_migration_tada <- rbind(juv_migration_tada, tmp)

# UpSac juvenile migration flow coefficient (Normal distribution)
tmp <- subset(one.way, parameter=="UpSacjuvmigQ")
mean_val <- 0.001
sd_val <- abs(0.001 * 0.1)
min_val <- qnorm(0.001, mean_val, sd_val)
max_val <- qnorm(0.999, mean_val, sd_val)
probs <- discretize(pnorm(x, mean_val, sd_val), method = "upper",
                    from = min_val, to = max_val, step = (max_val - min_val)/length(x))
probs <- probs/sum(probs)
plate <- data.frame(increment=x, probs=probs)
tmp <- merge(tmp, plate)
juv_migration_tada <- rbind(juv_migration_tada, tmp)

# Feather River juvenile migration parameters
# Feather juvenile migration intercept (Normal distribution)
tmp <- subset(one.way, parameter=="Feathjuvmig.int")
mean_val <- -6.5
sd_val <- abs(-6.5 * 0.2)
min_val <- qnorm(0.001, mean_val, sd_val)
max_val <- qnorm(0.999, mean_val, sd_val)
probs <- discretize(pnorm(x, mean_val, sd_val), method = "upper",
                    from = min_val, to = max_val, step = (max_val - min_val)/length(x))
probs <- probs/sum(probs)
plate <- data.frame(increment=x, probs=probs)
tmp <- merge(tmp, plate)
juv_migration_tada <- rbind(juv_migration_tada, tmp)

# Feather juvenile migration flow coefficient (Normal distribution)
tmp <- subset(one.way, parameter=="FeathjuvmigQ")
mean_val <- 0.0015
sd_val <- abs(0.0015 * 0.1)
min_val <- qnorm(0.001, mean_val, sd_val)
max_val <- qnorm(0.999, mean_val, sd_val)
probs <- discretize(pnorm(x, mean_val, sd_val), method = "upper",
                    from = min_val, to = max_val, step = (max_val - min_val)/length(x))
probs <- probs/sum(probs)
plate <- data.frame(increment=x, probs=probs)
tmp <- merge(tmp, plate)
juv_migration_tada <- rbind(juv_migration_tada, tmp)

# Yuba River juvenile migration parameters
#Yuba juvenile migration intercept (Normal distribution)
tmp <- subset(one.way, parameter=="yubajuvmig.int")
mean_val <- -6.2
sd_val <- abs(-6.2 * 0.2)
min_val <- qnorm(0.001, mean_val, sd_val)
max_val <- qnorm(0.999, mean_val, sd_val)
probs <- discretize(pnorm(x, mean_val, sd_val), method = "upper",
                    from = min_val, to = max_val, step = (max_val - min_val)/length(x))
probs <- probs/sum(probs)
plate <- data.frame(increment=x, probs=probs)
tmp <- merge(tmp, plate)
juv_migration_tada <- rbind(juv_migration_tada, tmp)

# Yuba juvenile migration flow coefficient (Normal distribution)
tmp <- subset(one.way, parameter=="yubajuvmigQ")
mean_val <- 0.0012
sd_val <- abs(0.0012 * 0.1)
min_val <- qnorm(0.001, mean_val, sd_val)
max_val <- qnorm(0.999, mean_val, sd_val)
probs <- discretize(pnorm(x, mean_val, sd_val), method = "upper",
                    from = min_val, to = max_val, step = (max_val - min_val)/length(x))
probs <- probs/sum(probs)
plate <- data.frame(increment=x, probs=probs)
tmp <- merge(tmp, plate)
juv_migration_tada <- rbind(juv_migration_tada, tmp)

# San Joaquin River juvenile migration parameters
# SJ juvenile migration intercept (Normal distribution)
tmp <- subset(one.way, parameter=="SJjuvmig.int")
mean_val <- -5.8
sd_val <- abs(-5.8 * 0.2)
min_val <- qnorm(0.001, mean_val, sd_val)
max_val <- qnorm(0.999, mean_val, sd_val)
probs <- discretize(pnorm(x, mean_val, sd_val), method = "upper",
                    from = min_val, to = max_val, step = (max_val - min_val)/length(x))
probs <- probs/sum(probs)
plate <- data.frame(increment=x, probs=probs)
tmp <- merge(tmp, plate)
juv_migration_tada <- rbind(juv_migration_tada, tmp)

# SJ juvenile migration flow coefficient (Normal distribution)
tmp <- subset(one.way, parameter=="SJjuvmigQ")
mean_val <- 0.0008
sd_val <- abs(0.0008 * 0.1)
min_val <- qnorm(0.001, mean_val, sd_val)
max_val <- qnorm(0.999, mean_val, sd_val)
probs <- discretize(pnorm(x, mean_val, sd_val), method = "upper",
                    from = min_val, to = max_val, step = (max_val - min_val)/length(x))
probs <- probs/sum(probs)
plate <- data.frame(increment=x, probs=probs)
tmp <- merge(tmp, plate)
juv_migration_tada <- rbind(juv_migration_tada, tmp)

# Calculate EVPI for juvenile migration parameters
juv_migration_summ <- aggregate(juv_migration_tada[c(3)], by = juv_migration_tada[c(1,4,5)], max)
juv_migration_summ$w.abundance <- juv_migration_summ$abundance * juv_migration_summ$probs
juv_migration_new.summ <- aggregate(juv_migration_summ["w.abundance"], by = juv_migration_summ["parameter"], FUN = sum)
juv_migration_new.summ$best_abundance <- best_abundance
juv_migration_new.summ$EVPI <- juv_migration_new.summ$w.abundance - juv_migration_new.summ$best_abundance

print("Juvenile Migration Submodel EVPI Results:")
print(juv_migration_new.summ[order(juv_migration_new.summ$EVPI, decreasing = TRUE),])
mean(juv_migration_new.summ$EVPI)

####Entrainment####
entrainment_tada <- data.frame()
# Upper Sacramento entrainment vulnerability (Beta distribution)
tmp <- subset(one.way, parameter=="entrain.upsac")
alpha_val <- 22.58
beta_val <- 1662.697
min_val <- 0
max_val <- 1
probs <- discretize(pbeta(x, alpha_val, beta_val), method = "upper",
                    from = min_val, to = max_val, step = max_val/length(x))
probs <- probs/sum(probs)
plate <- data.frame(increment=x, probs=probs)
tmp <- merge(tmp, plate)
entrainment_tada <- rbind(entrainment_tada, tmp)

# Upper-Mid Sacramento entrainment vulnerability (Beta distribution)
tmp <- subset(one.way, parameter=="entrain.upMid")
alpha_val <- 21.68
beta_val <- 1572.45
min_val <- 0
max_val <- 1
probs <- discretize(pbeta(x, alpha_val, beta_val), method = "upper",
                    from = min_val, to = max_val, step = max_val/length(x))
probs <- probs/sum(probs)
plate <- data.frame(increment=x, probs=probs)
tmp <- merge(tmp, plate)
entrainment_tada <- rbind(entrainment_tada, tmp)

# Lower-Mid Sacramento entrainment vulnerability (Beta distribution)
tmp <- subset(one.way, parameter=="entrain.lowMid")
alpha_val <- 17.84
beta_val <- 1179.36
min_val <- 0
max_val <- 1
probs <- discretize(pbeta(x, alpha_val, beta_val), method = "upper",
                    from = min_val, to = max_val, step = max_val/length(x))
probs <- probs/sum(probs)
plate <- data.frame(increment=x, probs=probs)
tmp <- merge(tmp, plate)
entrainment_tada <- rbind(entrainment_tada, tmp)

#Lower Sacramento entrainment vulnerability (Beta distribution)
tmp <- subset(one.way, parameter=="entrain.low")
alpha_val <- 27.87
beta_val <- 2471.29
min_val <- 0
max_val <- 1
probs <- discretize(pbeta(x, alpha_val, beta_val), method = "upper",
                    from = min_val, to = max_val, step = max_val/length(x))
probs <- probs/sum(probs)
plate <- data.frame(increment=x, probs=probs)
tmp <- merge(tmp, plate)
entrainment_tada <- rbind(entrainment_tada, tmp)

#Feather River entrainment vulnerability (Beta distribution)
tmp <- subset(one.way, parameter=="entrain.Feath")
alpha_val <- 18.013
beta_val <- 1060.61
min_val <- 0
max_val <- 1
probs <- discretize(pbeta(x, alpha_val, beta_val), method = "upper",
                    from = min_val, to = max_val, step = max_val/length(x))
probs <- probs/sum(probs)
plate <- data.frame(increment=x, probs=probs)
tmp <- merge(tmp, plate)
entrainment_tada <- rbind(entrainment_tada, tmp)

# Yuba River entrainment vulnerability (Beta distribution)
tmp <- subset(one.way, parameter=="entrain.yuba")
alpha_val <- 73.52
beta_val <- 2410.24
min_val <- 0
max_val <- 1
probs <- discretize(pbeta(x, alpha_val, beta_val), method = "upper",
                    from = min_val, to = max_val, step = max_val/length(x))
probs <- probs/sum(probs)
plate <- data.frame(increment=x, probs=probs)
tmp <- merge(tmp, plate)
entrainment_tada <- rbind(entrainment_tada, tmp)

# San Joaquin River entrainment vulnerability (Beta distribution)
tmp <- subset(one.way, parameter=="entrain.sj")
alpha_val <- 7.51
beta_val <- 444.946
min_val <- 0
max_val <- 1
probs <- discretize(pbeta(x, alpha_val, beta_val), method = "upper",
                    from = min_val, to = max_val, step = max_val/length(x))
probs <- probs/sum(probs)
plate <- data.frame(increment=x, probs=probs)
tmp <- merge(tmp, plate)
entrainment_tada <- rbind(entrainment_tada, tmp)

# Delta entrainment vulnerability (Beta distribution)
tmp <- subset(one.way, parameter=="delta.entrain")
alpha_val <- 2.13
beta_val <- 1994.89
min_val <- 0
max_val <- 1
probs <- discretize(pbeta(x, alpha_val, beta_val), method = "upper",
                    from = min_val, to = max_val, step = max_val/length(x))
probs <- probs/sum(probs)
plate <- data.frame(increment=x, probs=probs)
tmp <- merge(tmp, plate)
entrainment_tada <- rbind(entrainment_tada, tmp)

# Calculate EVPI for entrainment parameters
entrainment_summ <- aggregate(entrainment_tada[c(3)], by = entrainment_tada[c(1,4,5)], max)
entrainment_summ$w.abundance <- entrainment_summ$abundance * entrainment_summ$probs
entrainment_new.summ <- aggregate(entrainment_summ["w.abundance"], by = entrainment_summ["parameter"], FUN = sum)
entrainment_new.summ$best_abundance <- best_abundance
entrainment_new.summ$EVPI <- entrainment_new.summ$w.abundance - entrainment_new.summ$best_abundance

print("Entrainment Submodel EVPI Results:")
print(entrainment_new.summ[order(entrainment_new.summ$EVPI, decreasing = TRUE),])
mean(entrainment_new.summ$EVPI)

####Bycatch####
bycatch_tada <- data.frame()

# risk (beta distribution)
tmp <- subset(one.way, parameter=="bycatch.risk")
# These come from my sensitivity code 51.597,51.75
alpha_val <- 2.14
beta_val <- 19.87
min_val <- 0
max_val <- 1
probs <- discretize(pbeta(x, alpha_val, beta_val), method = "upper",
                    from = min_val, to = max_val, step = max_val/length(x))
probs <- probs/sum(probs)
plate <- data.frame(increment=x, probs=probs)
tmp <- merge(tmp, plate)
bycatch_tada <- rbind(bycatch_tada, tmp)

# survival (beta distribution)
tmp <- subset(one.way, parameter=="bycatch.S")
# These come from my sensitivity code 51.597,51.75
alpha_val <- 36.79
beta_val <- 6.93
min_val <- 0
max_val <- 1
probs <- discretize(pbeta(x, alpha_val, beta_val), method = "upper",
                    from = min_val, to = max_val, step = max_val/length(x))
probs <- probs/sum(probs)
plate <- data.frame(increment=x, probs=probs)
tmp <- merge(tmp, plate)
bycatch_tada <- rbind(bycatch_tada, tmp)

# Calculate EVPI for spawner parameters
bycatch_summ <- aggregate(bycatch_tada[c(3)], by = bycatch_tada[c(1,4,5)], max)
bycatch_summ$w.abundance <- bycatch_summ$abundance * bycatch_summ$probs
bycatch_new.summ <- aggregate(bycatch_summ["w.abundance"], by = bycatch_summ["parameter"], FUN = sum)
bycatch_new.summ$best_abundance <- best_abundance
bycatch_new.summ$EVPI <- bycatch_new.summ$w.abundance - bycatch_new.summ$best_abundance

print(bycatch_new.summ[order(bycatch_new.summ$EVPI, decreasing = TRUE),])

####Carrying Capacity####
capacity_tada <- data.frame()

# juvenile capacity (uniform distribution)
tmp <- subset(one.way, parameter=="juv.cap")
min_val <- 0.5
max_val <- 4.6
probs <- discretize(punif(x, min_val, max_val), method = "upper",
                    from = min_val, to = max_val, step = (max_val - min_val)/length(x))
probs <- probs/sum(probs)
plate <- data.frame(increment=x, probs=probs)
tmp <- merge(tmp, plate)
capacity_tada  <- rbind(capacity_tada , tmp)

# Adult capacity (uniform distribution)
tmp <- subset(one.way, parameter=="spawn.cap")
# These come from my sensitivity code 51.597,51.75
min_val <- 4.6
max_val <- 14
probs <- discretize(punif(x, min_val, max_val), method = "upper",
                    from = min_val, to = max_val, step = (max_val - min_val)/length(x))
probs <- probs/sum(probs)
plate <- data.frame(increment=x, probs=probs)
tmp <- merge(tmp, plate)
capacity_tada  <- rbind(capacity_tada , tmp)

# Calculate EVPI for spawner parameters
capacity_summ <- aggregate(capacity_tada[c(3)], by = capacity_tada[c(1,4,5)], max)
capacity_summ$w.abundance <- capacity_summ$abundance * capacity_summ$probs
capacity_new.summ <- aggregate(capacity_summ["w.abundance"], by = capacity_summ["parameter"], FUN = sum)
capacity_new.summ$best_abundance <- best_abundance
capacity_new.summ$EVPI <- capacity_new.summ$w.abundance - capacity_new.summ$best_abundance

print(capacity_new.summ[order(capacity_new.summ$EVPI, decreasing = TRUE),])

####Last lets do the miscellaneous parms that popped up####
misc_tada <- data.frame()

# Juvenile age transition (beta distribution)
tmp <- subset(one.way, parameter=="Age.tranjuv")
# These come from my sensitivity code 51.597,51.75
alpha_val <- 51.597
beta_val <- 51.75
min_val <- 0
max_val <- 1
probs <- discretize(pbeta(x, alpha_val, beta_val), method = "upper",
                    from = min_val, to = max_val, step = max_val/length(x))
probs <- probs/sum(probs)
plate <- data.frame(increment=x, probs=probs)
tmp <- merge(tmp, plate)
misc_tada <- rbind(misc_tada, tmp)


# natural mortality (beta distribution)
tmp <- subset(one.way, parameter=="natural.S")
# These come from my sensitivity code 36.25,6.62
alpha_val <- 36.25
beta_val <- 6.62
min_val <- 0
max_val <- 1
probs <- discretize(pbeta(x, alpha_val, beta_val), method = "upper",
                    from = min_val, to = max_val, step = max_val/length(x))
probs <- probs/sum(probs)
plate <- data.frame(increment=x, probs=probs)
tmp <- merge(tmp, plate)
misc_tada <- rbind(misc_tada, tmp)

# Calculate EVPI for spawner parameters
misc_summ <- aggregate(misc_tada[c(3)], by = misc_tada[c(1,4,5)], max)
misc_summ$w.abundance <- misc_summ$abundance * misc_summ$probs
misc_new.summ <- aggregate(misc_summ["w.abundance"], by = misc_summ["parameter"], FUN = sum)
misc_new.summ$best_abundance <- best_abundance
misc_new.summ$EVPI <- misc_new.summ$w.abundance - misc_new.summ$best_abundance

print(misc_new.summ[order(misc_new.summ$EVPI, decreasing = TRUE),])
