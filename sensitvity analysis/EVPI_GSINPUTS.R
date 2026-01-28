setwd("C:/Users/gilliger/Box/Erin Dissertation/SIT sturgeon models/Green Sturgeon Model code")
source("strategiestoRun.R")
source("GSmodsens.R")
setwd("C:/Users/gilliger/Box/Erin Dissertation/SIT sturgeon models/Green Sturgeon Model code/SensitivityAnalysis")
#sen.table<-read.csv("GSsensINPUTparms.csv", stringsAsFactors = F)
sen.table<-read.csv("GSsensINPUTparmsDIST.csv", stringsAsFactors = F)

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
#incr<-cbind(seq(0.1,1.9,by=0.1),0.5) # for all scaling parms
incr<- cbind(seq(0.05, 0.95, by=0.01), 0.5)

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

write.csv(one.way_results, "C:/Users/gilliger/Box/Erin Dissertation/SIT sturgeon models/Green Sturgeon Model code/Results/EVPI_InputResDIST.csv", row.names = FALSE)


###ACTUAL CALCULATION####
library(actuar)

one.way_results_scale<-read.csv("C:/Users/gilliger/Box/Erin Dissertation/SIT sturgeon models/Green Sturgeon Model code/Results/EVPI_InputRes.csv")
one.way_results_dist<-read.csv("C:/Users/gilliger/Box/Erin Dissertation/SIT sturgeon models/Green Sturgeon Model code/Results/EVPI_InputResDIST.csv")
one.way_results<-rbind(one.way_results_scale,one.way_results_dist)

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


x <- seq(0.1, 1.9, 0.1) 

##stream Temp
tmp=subset(one.way,parameter=="avgStreamT.vary")
min=0
max=30
probs<-discretize(pnorm(x,15,3), method = "upper",
                  from = min, to = max, step = max/length(x))
# normalize, this is how Netica does it
probs=probs/sum(probs)
## combine probabilities with increments
plate<-data.frame(increment=x, probs = probs)

tada<-merge(tmp,plate)

## Flows
tmp=subset(one.way,parameter=="Qmean.vary")
flow_mean <- 3215.736
flow_sd <- 8901.035
flow_mean_adj <- flow_mean + 1  # Avoid log(0)
meanlog <- log(flow_mean_adj^2 / sqrt(flow_sd^2 + flow_mean_adj^2))
sdlog <- sqrt(log(1 + (flow_sd^2/flow_mean_adj^2)))
min_flow=0
max_flow=50000
probs <- discretize(plnorm(x, meanlog=meanlog, sdlog=sdlog), method = "upper",
                    from = min_flow, to = max_flow, step = max_flow/length(x))
# normalize, this is how Netica does it
probs=probs/sum(probs)
## combine probabilities with increments
plate<-data.frame(increment=x, probs = probs)
tmp<-merge(tmp,plate)

tada<-rbind(tada,tmp)

## River diversions
tmp=subset(one.way,parameter=="river.div.vary")
shape_param <- 4
rate_param <- 4 
min_scale <- 0.4    
max_scale <- 2.0
probs <- discretize(pgamma(x, shape=shape_param, rate=rate_param), 
                    method = "upper",
                    from = min_scale, to = max_scale, 
                    step = (max_scale - min_scale)/length(x))
# normalize, this is how Netica does it
probs=probs/sum(probs)
## combine probabilities with increments
plate<-data.frame(increment=x, probs = probs)
tmp<-merge(tmp,plate)

tada<-rbind(tada,tmp)

## delta diversions
tmp=subset(one.way,parameter=="delta.div.vary")
shape_param <- 4
rate_param <- 4 
min_scale <- 0.4    
max_scale <- 2.0
probs <- discretize(pgamma(x, shape=shape_param, rate=rate_param), 
                    method = "upper",
                    from = min_scale, to = max_scale, 
                    step = (max_scale - min_scale)/length(x))
# normalize, this is how Netica does it
probs=probs/sum(probs)
## combine probabilities with increments
plate<-data.frame(increment=x, probs = probs)
tmp<-merge(tmp,plate)

tada<-rbind(tada,tmp)

##Predation
tmp <- subset(one.way, parameter=="Predation.vary")
alpha_param <- 5  
beta_param <- 5 
min_scale <- 0.2  
max_scale <- 3.0

probs <- discretize(pbeta(x, shape1=alpha_param, shape2=beta_param), 
                    method = "upper",
                    from = min_scale, to = max_scale, 
                    step = (max_scale - min_scale)/length(x))
probs <- probs/sum(probs)
plate <- data.frame(increment=x, probs = probs)
tmp <- merge(tmp, plate)
tada <- rbind(tada, tmp)

# UpSac habitat availability (uniform)
tmp <- subset(one.way, parameter=="habitat.UpSac.vary")
min_val <- 315218.78
max_val <- 1109007.15
probs <- discretize(punif(x, min_val, max_val), method = "upper",
                    from = min_val, to = max_val, step = (max_val - min_val)/length(x))
probs <- probs/sum(probs)
plate <- data.frame(increment=x, probs=probs)
tmp <- merge(tmp, plate)
tada  <- rbind(tada , tmp)

# Feather habitat availability (uniform)
tmp <- subset(one.way, parameter=="habitat.Feather.vary")
min_val <- 12315.5
max_val <- 303304.6117
probs <- discretize(punif(x, min_val, max_val), method = "upper",
                    from = min_val, to = max_val, step = (max_val - min_val)/length(x))
probs <- probs/sum(probs)
plate <- data.frame(increment=x, probs=probs)
tmp <- merge(tmp, plate)
tada  <- rbind(tada , tmp)

# Yuba habitat availability (uniform)
tmp <- subset(one.way, parameter=="habitat.Yuba.vary")
min_val <- 7456.530958
max_val <- 183635.7054
probs <- discretize(punif(x, min_val, max_val), method = "upper",
                    from = min_val, to = max_val, step = (max_val - min_val)/length(x))
probs <- probs/sum(probs)
plate <- data.frame(increment=x, probs=probs)
tmp <- merge(tmp, plate)
tada  <- rbind(tada , tmp)

# SJ habitat availability (uniform)
tmp <- subset(one.way, parameter=="habitat.SJ.vary")
min_val <- 2813.488875
max_val <- 69289.19322
probs <- discretize(punif(x, min_val, max_val), method = "upper",
                    from = min_val, to = max_val, step = (max_val - min_val)/length(x))
probs <- probs/sum(probs)
plate <- data.frame(increment=x, probs=probs)
tmp <- merge(tmp, plate)
tada  <- rbind(tada , tmp)


summ<-aggregate(tada[c(3)], by = tada[c(1,4,5)], max)
## weight by the probability of the value
summ$w.abundance<-summ$abundance*summ$probs
# 
new.summ<-aggregate(summ["w.abundance"], by = summ["parameter"], FUN = sum)

new.summ$best_abundance<-best_abundance
new.summ$EVPI<-new.summ$w.abundance-new.summ$best_abundance

names(new.summ)<-c("Parameter","Abundance.Per.Info","Abundance.Opt.Dec","EVPI")

new.summ

###############################################################
### For distribution parameters
###############################################################
tada<-data.frame()
#starting population (normal distribution)
x<- seq(0.05, 0.95, 0.01) 
tmp <- subset(one.way, parameter=="startpop.n.pop")
min_val=4502
max_val=17775
probs<-discretize(pnorm(x,10000,3342), method = "upper",
                  from = min_val, to = max_val,  step = (max_val - min_val)/length(x))
probs <- probs/sum(probs)
plate <- data.frame(increment=x, probs=probs)
tmp <- merge(tmp, plate)
tada <- rbind(tada, tmp)


#HAB event alpha (Probability intensity) (uniform)
# HAB event alpha (Probability intensity) (uniform)
tmp <- subset(one.way, parameter=="HAB.prob")
alpha_val <- 3
beta_val <- 10
min_val <- 0
max_val <- 1
probs <- discretize(pbeta(x, alpha_val, beta_val), method = "upper",
                    from = min_val, to = max_val, step = (max_val - min_val)/length(x))
probs <- probs/sum(probs)
plate <- data.frame(increment=x, probs=probs)
tmp <- merge(tmp, plate)
tada <- rbind(tada, tmp)


summ<-aggregate(tada[c(3)], by = tada[c(1,4,5)], max)
## weight by the probability of the value
summ$w.abundance<-summ$abundance*summ$probs
# 
new.summ<-aggregate(summ["w.abundance"], by = summ["parameter"], FUN = sum)

new.summ$best_abundance<-best_abundance
new.summ$EVPI<-new.summ$w.abundance-new.summ$best_abundance

names(new.summ)<-c("Parameter","Abundance.Per.Info","Abundance.Opt.Dec","EVPI")

new.summ
