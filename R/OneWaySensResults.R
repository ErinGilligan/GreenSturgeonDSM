parm.stages<-read.csv("C:/Users/gilliger/Box/Erin Dissertation/SIT sturgeon models/Green Sturgeon Model code/SensitivityAnalysis/GSsensParms.csv")

one.way<-read.csv("C:/Users/gilliger/Box/Erin Dissertation/SIT sturgeon models/Green Sturgeon Model code/Results/Sensitivity Results/sensitivity_analysis_resultsFINALIZED2.csv")
names(one.way)[2:15]<-c("lambda","recruit","spawners","large.spawners",
                        "num.holding","spawningUpSacMonths","spawningFeatherMonths","spawningYubaMonths",
                        "spawningSJMonths","num.rivers","rearUpSac","rearFeather","rearYuba","rearSJ")
one.way$avg_spawning <- rowMeans(one.way[, c("spawningUpSacMonths", "spawningFeatherMonths", "spawningYubaMonths", "spawningSJMonths")])
one.way$avg_rearDur <- rowMeans(one.way[, c("rearUpSac","rearFeather","rearYuba","rearSJ")])

######Create a correlation matrix of the attributes####
# Load required libraries
library(corrplot)
library(tidyverse)
data<-data.frame(
  lambda=one.way$lambda,
  recruit=one.way$recruit,
  spawners=one.way$spawners,
  large.spawners=one.way$large.spawners,
  num.holding=one.way$num.holding,
  num.rivers=one.way$num.rivers,
  avg_spawning=one.way$avg_spawning,
  avg_rearDur=one.way$avg_rearDur
)

cor_matrix<-cor(data)
# Create correlation plot
png("C:/Users/gilliger/Box/Erin Dissertation/SIT sturgeon models/Green Sturgeon Model code/Results/Sensitivity Results/correlation_plot.png",
    width = 800, height = 800, res = 100)
corrplot(cor_matrix, 
         method = "color",
         type= "upper",
         order = "original",
         addCoef.col = "black",
         tl.col = "black",
         tl.srt = 45,
         col = colorRampPalette(c("#4575B4", "white", "#D73027"))(100))

# Add title
title("Correlation Plot of Strategy Attributes")

# Close the device
dev.off()

# Print the correlation matrix with rounded values for reference
print(round(cor_matrix, 3))

##############################################
###     Now proportional score outcomes
##############################################
library(dplyr)
library(tidyverse)

param_sensitivities <- list()
parameters <- unique(one.way$parm1)

for(param in parameters) {
  # Get all data for this parameter across all increments
  param_data <- one.way[one.way$parm1 == param, ]
  
  # Check what increments exist for this parameter
  increments <- unique(param_data$incr1)
  
  for(incr in increments) {
    # Filter to just this parameter-increment combination
    subset_data <- param_data[param_data$incr1 == incr, ]
    
    # Now calculate proportional scores for this specific parameter-increment combination
    strategies <- unique(subset_data$Strategy)
    
    # For each strategy, calculate proportional scores and utility
    strategy_utilities <- numeric(length(strategies))
    
    for(i in 1:length(strategies)) {
      strat <- strategies[i]
      strat_data <- subset_data[subset_data$Strategy == strat, ]
      
      # Calculate mean values for this strategy
      lambda_mean <- mean(strat_data$lambda, na.rm = TRUE)
      recruit_mean <- mean(strat_data$recruit, na.rm = TRUE)
      spawners_mean <- mean(strat_data$spawners, na.rm = TRUE)
      lg_spawners_mean <- mean(strat_data$large.spawners, na.rm = TRUE)
      holding_mean <- mean(strat_data$num.holding, na.rm = TRUE)
      spawn_dur_mean <- mean(strat_data$avg_spawning, na.rm = TRUE)
      rivers_mean <- mean(strat_data$num.rivers, na.rm = TRUE)
      rear_dur_mean <- mean(strat_data$avg_rearDur, na.rm = TRUE)
      
      # Get min and max values across all strategies for this parameter-increment
      lambda_min <- min(subset_data$lambda, na.rm = TRUE)
      lambda_max <- max(subset_data$lambda, na.rm = TRUE)
      recruit_min <- min(subset_data$recruit, na.rm = TRUE)
      recruit_max <- max(subset_data$recruit, na.rm = TRUE)
      spawners_min <- min(subset_data$spawners, na.rm = TRUE)
      spawners_max <- max(subset_data$spawners, na.rm = TRUE)
      lg_spawners_min <- min(subset_data$large.spawners, na.rm = TRUE)
      lg_spawners_max <- max(subset_data$large.spawners, na.rm = TRUE)
      holding_min <- min(subset_data$num.holding, na.rm = TRUE)
      holding_max <- max(subset_data$num.holding, na.rm = TRUE)
      spawn_dur_min <- min(subset_data$avg_spawning, na.rm = TRUE)
      spawn_dur_max <- max(subset_data$avg_spawning, na.rm = TRUE)
      rivers_min <- min(subset_data$num.rivers, na.rm = TRUE)
      rivers_max <- max(subset_data$num.rivers, na.rm = TRUE)
      rear_dur_min <- min(subset_data$avg_rearDur, na.rm = TRUE)
      rear_dur_max <- max(subset_data$avg_rearDur, na.rm = TRUE)
      
      # Calculate proportional scores (handling cases where min=max)
      lambda_prop <- if(lambda_max > lambda_min) (lambda_mean - lambda_min)/(lambda_max - lambda_min) else 1
      recruit_prop <- if(recruit_max > recruit_min) (recruit_mean - recruit_min)/(recruit_max - recruit_min) else 1
      spawners_prop <- if(spawners_max > spawners_min) (spawners_mean - spawners_min)/(spawners_max - spawners_min) else 1
      lg_spawners_prop <- if(lg_spawners_max > lg_spawners_min) (lg_spawners_mean - lg_spawners_min)/(lg_spawners_max - lg_spawners_min) else 1
      holding_prop <- if(holding_max > holding_min) (holding_mean - holding_min)/(holding_max - holding_min) else 1
      spawn_dur_prop <- if(spawn_dur_max > spawn_dur_min) (spawn_dur_mean - spawn_dur_min)/(spawn_dur_max - spawn_dur_min) else 1
      rivers_prop <- if(rivers_max > rivers_min) (rivers_mean - rivers_min)/(rivers_max - rivers_min) else 1
      rear_dur_prop <- if(rear_dur_max > rear_dur_min) (rear_dur_mean - rear_dur_min)/(rear_dur_max - rear_dur_min) else 1
      
      # Calculate utility using your AHP weights
      strategy_utilities[i] <- (lambda_prop * 0.24) + 
        (recruit_prop * 0.19) + 
        (spawners_prop * 0.14) + 
        (rivers_prop * 0.19) + 
        (lg_spawners_prop * 0.09) + 
        (spawn_dur_prop * 0.06) + 
        (rear_dur_prop * 0.08) + 
        (holding_prop * 0.05)
    }
    
    # Store results for this parameter-increment combination
    result_df <- data.frame(
      decision = strategies,
      parameter = param,
      increment = incr,
      utility = strategy_utilities
    )
    
    param_sensitivities[[paste(param, incr, sep="_")]] <- result_df
  }
}

# Combine all results
keep.one.way <- do.call(rbind, param_sensitivities)

# Write to CSV
#write.csv(keep.one.way, "Sens.Results", row.names = FALSE)
#sens<-read.csv("Sens.Results")
sens <- keep.one.way

## step 1 find optimal decision (greatest utility) for each combination 
## of parameter and increment
summ<-aggregate(utility ~ parameter + increment, data = sens, FUN=max)

### now find the minimum and maximum utilities of the optimal
## decision for each component these will define the 
## ends of the bars in the tornado diagram
# Find min and max utilities for each parameter
min.ut <- aggregate(utility ~ parameter, data = summ, FUN=min)
max.ut <- aggregate(utility ~ parameter, data = summ, FUN=max)


# Combine min and max
sensit <- merge(min.ut, max.ut, by = "parameter")
names(sensit) <- c("Component", "Min", "Max")


## calculate the difference between the min and max
## this will be the width of the bars in the diagram
# Calculate difference
sensit$Dif <- sensit$Max - sensit$Min

# Sort by sensitivity
sensit <- sensit[order(sensit$Dif), ]

#To add color cordination
param_lifestage_map <- setNames(parm.stages$Lifestage, parm.stages$Parameter)
sensit$Lifestage <- param_lifestage_map[sensit$Component]

## Create tornado diagram for utilties####
library(gplots)
#library(Hmisc)

# Define colors for each lifestage
lifestage_colors <- c("juvenile" = "blue", "yoy" = "red", "adult" = "gold")


# Reformat your data
tornado_data <- cbind(sensit$Min, sensit$Dif)
rownames(tornado_data) <- sensit$Component
influential_params <- tornado_data[tornado_data[,2] > 1e-3, ] 

# Get lifestages for the influential parms
influential_components <- rownames(influential_params)
influential_lifestages <- sensit$Lifestage[match(influential_components, sensit$Component)]

# Create color vector for the bars
bar_colors <- lifestage_colors[influential_lifestages]


png("C:/Users/gilliger/Box/Erin Dissertation/SIT sturgeon models/Green Sturgeon Model code/Results/Sensitivity Results/Color_tornado.png",
    width = 1200, height = 1500, res = 120)
par(oma = c(0, 0, 0, 0), mar = c(5, 10, 2, 2)) 
minz <- min(sensit$Min)
maxz <- max(sensit$Max)

barplot2(t(influential_params), 
         beside = FALSE, 
         horiz = TRUE,
         xlim = c(0.16, 0.46),
         col = c("white", "black"),
         border = c("white", "black"),
         las = 1,
         cex.names = 0.9,
         xlab = "Utility")

bp <- barplot2(t(influential_params), 
               beside = FALSE, 
               horiz = TRUE,
               xlim = c(0.16, 0.46),
               col = c("white", "black"),
               border = c("white", "black"),
               las = 1,
               cex.names = 0.9, 
               plot = FALSE)  # Just get coordinates, don't plot

# Add colored rectangles based on lifestage
n <- length(influential_components)
for(i in 1:n) {
  rect(influential_params[i,1], bp[i] - 0.4, 
       influential_params[i,1] + influential_params[i,2], bp[i] + 0.4,
       col = as.character(bar_colors[i]),  # Convert to character explicitly
       border = "black")
}

# Add a legend
legend("bottomright", 
       legend = c("juvenile", "yoy", "adult"),
       fill = c("blue", "red", "gold"),
       title = "Lifestage",
       cex = 1.2)

box()
dev.off()




## Create Response Profile####
infile<-keep.one.way 
## grab parameters to be evaluated
compnts<-read.csv("C:/Users/gilliger/Box/Erin Dissertation/SIT sturgeon models/Green Sturgeon Model code/SensitivityAnalysis/GSsensParms.csv", stringsAsFactors = F)
names(compnts)[1]<-"parameter"
infile<-merge(infile,compnts)

parms<-unique(infile$parameter)
# parms<-parms[c(20,49,76:78,85,65)]
# 
# #This is from class i dont want to look at all these plots though
#  require(lattice)
# custom_colors <- c("red", "blue", "darkgreen", "purple", "orange", 
#                    "black", "brown", "cyan", "magenta", "gold", 
#                    "darkred", "darkblue", "forestgreen", "violet", 
#                    "darkorange", "gray30", "chocolate")
# #
#  for(i in 1:length(parms)){
#    #i=4
#    working<- subset(infile,parameter == parms[i])
# 
#    #Now plot the expected utility for each decision across
#    # the range of parameter values
#    # note parameter values will be in percentile increments
#    print( xyplot(utility~increment,groups = decision, type = c("a"), data = working,
#                  xlab="Quantile of distribution", ylab="Expected utility",
#                  main=paste("Response profile for ", parms[i], sep =""),
#                  par.settings=list(superpose.line = list(col = custom_colors)),
#                  auto.key = list(space = "right", points = FALSE, lines = TRUE, cex = 0.8)))
#  }
# Calculate how many times top strategies change across parameter increments
strategy_changes <- function(data) {
  # Get list of unique parameters
  parameters <- unique(data$parameter)
  
  # Create empty data frame to store results
  changes_df <- data.frame(
    Parameter = character(),
    times = numeric(),
    stringsAsFactors = FALSE
  )
  
  # Process each parameter
  for(param in parameters) {
    # Get data for this parameter only
    param_data <- subset(data, parameter == param)
    
    # Initialize counter for strategy changes
    changes <- 0
    prev_top3 <- NULL
    
    # For each increment, check if top 3 strategies change
    for(inc in sort(unique(param_data$increment))) {
      # Get data for this increment
      inc_data <- subset(param_data, increment == inc)
      
      # Sort by utility and get top 3 strategies
      inc_data <- inc_data[order(-inc_data$utility), ]
      top3 <- head(inc_data$decision, 3)
      
      # Check if this is different from previous top 3
      if(!is.null(prev_top3)) {
        # Check if top 3 changed (either order or composition)
        if(!identical(top3, prev_top3)) {
          changes <- changes + 1
        }
      }
      
      # Update previous top 3
      prev_top3 <- top3
    }
    
    # Add to results
    changes_df <- rbind(changes_df, data.frame(
      Parameter = param,
      times = changes,
      stringsAsFactors = FALSE
    ))
  }
  
  # Sort by number of changes (descending)
  changes_df <- changes_df[order(-changes_df$times), ]
  
  return(changes_df)
}

# Apply function
top3_changes <- strategy_changes(infile)

# Print results
print(top3_changes)

influential_changes <- top3_changes[top3_changes[,2] > 0, ] 
# Create barplot
# Optional: Create a bar plot of the results
png("C:/Users/gilliger/Box/Erin Dissertation/SIT sturgeon models/Green Sturgeon Model code/Results/Sensitivity Results/ResponseProfile_plot.png",
    width = 1500, height = 1200, res = 150)
par(mar = c(8, 4.1, 4.1, 2.1))

barplot(influential_changes$times, 
        names.arg = influential_changes$Parameter, 
        horiz = FALSE, 
        las = 2, 
        cex.names = 0.7,
        col = "steelblue",
        main = "Response Profile Summary",
        ylab = "Number times top 3 Rankings Changed")
box()
dev.off()


##How about just the top strategy just to check

# Calculate how many times top strategy changes across parameter increments
strategy_changes1 <- function(data) {
  # Get list of unique parameters
  parameters <- unique(data$parameter)
  
  # Create empty data frame to store results
  changes_df <- data.frame(
    Parameter = character(),
    times = numeric(),
    stringsAsFactors = FALSE
  )
  
  # Process each parameter
  for(param in parameters) {
    # Get data for this parameter only
    param_data <- subset(data, parameter == param)
    
    # Initialize counter for strategy changes
    changes <- 0
    prev_top <- NULL
    
    # For each increment, check if top strategy changes
    for(inc in sort(unique(param_data$increment))) {
      # Get data for this increment
      inc_data <- subset(param_data, increment == inc)
      
      # Sort by utility and get top strategy
      inc_data <- inc_data[order(-inc_data$utility), ]
      top <- inc_data$decision[1]
      
      # Check if this is different from previous top
      if(!is.null(prev_top)) {
        # Check if top strategy changed
        if(top != prev_top) {
          changes <- changes + 1
        }
      }
      
      # Update previous top
      prev_top <- top
    }
    
    # Add to results
    changes_df <- rbind(changes_df, data.frame(
      Parameter = param,
      times = changes,
      stringsAsFactors = FALSE
    ))
  }
  
  # Sort by number of changes (descending)
  changes_df <- changes_df[order(-changes_df$times), ]
  
  return(changes_df)
}

# Apply function
top1_changes <- strategy_changes1(infile)

# Print results
print(top1_changes)
#As of now should all be 0!!!



##############################################
###     Now Do straight up lambda
############################################

param_sensitivities <- list()
parameters <- unique(one.way$parm1)

for(param in parameters) {
  # Get all data for this parameter across all increments
  param_data <- one.way[one.way$parm1 == param, ]
  
  # Check what increments exist for this parameter
  increments <- unique(param_data$incr1)
  
  for(incr in increments) {
    # Filter to just this parameter-increment combination
    subset_data <- param_data[param_data$incr1 == incr, ]
    
    # Now calculate lambda values for this specific parameter-increment combination
    strategies <- unique(subset_data$Strategy)
    
    # For each strategy, get the lambda value
    strategy_lambdas <- numeric(length(strategies))
    
    for(i in 1:length(strategies)) {
      strat <- strategies[i]
      strat_data <- subset_data[subset_data$Strategy == strat, ]
      
      # Calculate mean lambda value for this strategy
      lambda_mean <- mean(strat_data$lambda, na.rm = TRUE)
      
      # Store just the lambda value (no utility calculation)
      strategy_lambdas[i] <- lambda_mean
    }
    
    # Store results for this parameter-increment combination
    result_df <- data.frame(
      decision = strategies,
      parameter = param,
      increment = incr,
      lambda = strategy_lambdas
    )
    
    param_sensitivities[[paste(param, incr, sep="_")]] <- result_df
  }
}

# Combine all results
Abund.one.way <- do.call(rbind, param_sensitivities)

# Write to CSV if needed
# write.csv(keep.one.way, "Lambda.Sens.Results", row.names = FALSE)
sens.abund <- Abund.one.way

## Step 1: Find optimal decision (greatest lambda) for each combination
## of parameter and increment
summ.abund <- aggregate(lambda ~ parameter + increment, data = sens.abund, FUN=max)

### Now find the minimum and maximum lambda of the optimal
## decision for each component - these will define the 
## ends of the bars in the tornado diagram
# Find min and max lambda for each parameter
min.lambda <- aggregate(lambda ~ parameter, data = summ.abund, FUN=min)
max.lambda <- aggregate(lambda ~ parameter, data = summ.abund, FUN=max)

# Combine min and max
sensit.abund <- merge(min.lambda, max.lambda, by = "parameter")
names(sensit.abund) <- c("Component", "Min", "Max")

## Calculate the difference between the min and max
## this will be the width of the bars in the diagram
# Calculate difference
sensit.abund$Dif <- sensit.abund$Max - sensit.abund$Min

# Sort by sensitivity
sensit.abund <- sensit.abund[order(sensit.abund$Dif), ]

#for the color
sensit$Lifestage <- param_lifestage_map[sensit.abund$Component]

## Create tornado diagram
# Reformat your data
tornado_abund <- cbind(sensit.abund$Min, sensit.abund$Dif)
rownames(tornado_abund) <- sensit.abund$Component
# Sort by influence (Dif) and select top 25
tornado_sorted <- tornado_abund[order(tornado_abund[,2], decreasing = TRUE), ]
influential_abund <- tornado_sorted[1:25, ]

influential_components <- rownames(influential_abund)
influential_lifestages <- sensit.abund$Lifestage[match(influential_components, sensit.abund$Component)]

# Define colors for each lifestage
lifestage_colors <- c("juvenile" = "blue", "yoy" = "red", "adult" = "gold")

# Create color vector for the bars
bar_colors <- lifestage_colors[influential_lifestages]

png("C:/Users/gilliger/Box/Erin Dissertation/SIT sturgeon models/Green Sturgeon Model code/Results/Sensitivity Results/tornadoColor_lambda.png",
    width = 1200, height = 1500, res = 120)
par(oma = c(0, 0, 0, 0), mar = c(5, 10, 2, 2)) 
minz <- min(sensit.abund$Min)
maxz <- max(sensit.abund$Max)

barplot2(t(influential_abund), 
         beside = FALSE, 
         horiz = TRUE,
         xlim = c(min(sensit.abund$Min)*0.95, max(sensit.abund$Max)*1.05),  # Adjusted to fit lambda values
         col = c("white", "black"),
         border = c("white", "black"),
         las = 1,
         cex.names = 0.9,
         xlab = "Population abundance")

# Plot the bars
barplot2(t(influential_abund), 
         beside = FALSE, 
         horiz = TRUE,
         xlim = c(min(sensit.abund$Min)*0.95, max(sensit.abund$Max)*1.05),
         col = c("white", "black"),
         border = c("white", "black"),
         las = 1,
         cex.names = 0.9,
         xlab = "Population abundance")

# Add colored rectangles based on lifestage
n <- length(influential_components)
for(i in 1:n) {
  rect(influential_abund[i,1], bp[i] - 0.4, 
       influential_abund[i,1] + influential_abund[i,2], bp[i] + 0.4,
       col = as.character(bar_colors[i]),
       border = "black")
}

# Add a legend with increased size
legend("bottomright", 
       legend = c("juvenile", "yoy", "adult"),
       fill = c("blue", "red", "gold"),
       title = "Lifestage",
       cex = 1.2)
box()
dev.off()



##############################################
###     Now Do straight up Recruits
##############################################

param_sensitivities <- list()
parameters <- unique(one.way$parm1)

for(param in parameters) {
  # Get all data for this parameter across all increments
  param_data <- one.way[one.way$parm1 == param, ]
  
  # Check what increments exist for this parameter
  increments <- unique(param_data$incr1)
  
  for(incr in increments) {
    # Filter to just this parameter-increment combination
    subset_data <- param_data[param_data$incr1 == incr, ]
    
    # Now calculate recruitment values for this specific parameter-increment combination
    strategies <- unique(subset_data$Strategy)
    
    # For each strategy, get the lambda value
    strategy_recruits <- numeric(length(strategies))
    
    for(i in 1:length(strategies)) {
      strat <- strategies[i]
      strat_data <- subset_data[subset_data$Strategy == strat, ]
      
      # Calculate mean recruit value for this strategy
      recruit_mean <- mean(strat_data$recruit, na.rm = TRUE)
      
      # Store just the recruit value 
      strategy_recruits[i] <- recruit_mean
    }
    
    # Store results for this parameter-increment combination
    result_df <- data.frame(
      decision = strategies,
      parameter = param,
      increment = incr,
      recruit = strategy_recruits
    )
    
    param_sensitivities[[paste(param, incr, sep="_")]] <- result_df
  }
}

# Combine all results
recruit.one.way <- do.call(rbind, param_sensitivities)

# Write to CSV if needed
# write.csv(keep.one.way, "Lambda.Sens.Results", row.names = FALSE)
sens.recruit <- recruit.one.way

## Step 1: Find optimal decision (greatest lambda) for each combination
## of parameter and increment
summ.recruit <- aggregate(recruit ~ parameter + increment, data = sens.recruit, FUN=max)

### Now find the minimum and maximum lambda of the optimal
## decision for each component - these will define the 
## ends of the bars in the tornado diagram
# Find min and max lambda for each parameter
min.recruit <- aggregate(recruit ~ parameter, data = summ.recruit, FUN=min)
max.recruit <- aggregate(recruit ~ parameter, data = summ.recruit, FUN=max)

# Combine min and max
sensit.recruit <- merge(min.recruit, max.recruit, by = "parameter")
names(sensit.recruit) <- c("Component", "Min", "Max")

## Calculate the difference between the min and max
## this will be the width of the bars in the diagram
# Calculate difference
sensit.recruit$Dif <- sensit.recruit$Max - sensit.recruit$Min

# Sort by sensitivity
sensit.recruit <- sensit.recruit[order(sensit.recruit$Dif), ]

## Create tornado diagram
# Reformat your data
tornado_recruit <- cbind(sensit.recruit$Min, sensit.recruit$Dif)
rownames(tornado_recruit) <- sensit.recruit$Component
influential_recruit <- tornado_recruit[tornado_recruit[,2] > 1e-3, ] 

png("C:/Users/gilliger/Box/Erin Dissertation/SIT sturgeon models/Green Sturgeon Model code/Results/Sensitivity Results/tornado_recuit.png",
    width = 1200, height = 1500, res = 120)
par(oma = c(0, 0, 0, 0), mar = c(5, 10, 2, 2)) 
minz <- min(sensit.recruit$Min)
maxz <- max(sensit.recruit$Max)

barplot2(t(influential_recruit), 
         beside = FALSE, 
         horiz = TRUE,
         xlim = c(min(sensit.recruit$Min)*0.95, max(sensit.recruit$Max)*1.05),  # Adjusted to fit lambda values
         col = c("white", "black"),
         border = c("white", "black"),
         las = 1,
         cex.names = 0.9,
         xlab = "Recruitment abundance")
box()
dev.off()



##############################################
###     Now Do straight up Spawners
##############################################

param_sensitivities <- list()
parameters <- unique(one.way$parm1)

for(param in parameters) {
  # Get all data for this parameter across all increments
  param_data <- one.way[one.way$parm1 == param, ]
  
  # Check what increments exist for this parameter
  increments <- unique(param_data$incr1)
  
  for(incr in increments) {
    # Filter to just this parameter-increment combination
    subset_data <- param_data[param_data$incr1 == incr, ]
    
    # Now calculate recruitment values for this specific parameter-increment combination
    strategies <- unique(subset_data$Strategy)
    
    # For each strategy, get the lambda value
    strategy_spawners<- numeric(length(strategies))
    
    for(i in 1:length(strategies)) {
      strat <- strategies[i]
      strat_data <- subset_data[subset_data$Strategy == strat, ]
      
      # Calculate mean recruit value for this strategy
      spawner_mean <- mean(strat_data$spawners, na.rm = TRUE)
      
      # Store just the recruit value 
      strategy_spawners[i] <- spawner_mean
    }
    
    # Store results for this parameter-increment combination
    result_df <- data.frame(
      decision = strategies,
      parameter = param,
      increment = incr,
      spawner = strategy_spawners
    )
    
    param_sensitivities[[paste(param, incr, sep="_")]] <- result_df
  }
}

# Combine all results
spawner.one.way <- do.call(rbind, param_sensitivities)

# Write to CSV if needed
# write.csv(keep.one.way, "Lambda.Sens.Results", row.names = FALSE)
sens.spawner <- spawner.one.way

## Step 1: Find optimal decision (greatest lambda) for each combination
## of parameter and increment
summ.spawner <- aggregate(spawner ~ parameter + increment, data = sens.spawner, FUN=max)

### Now find the minimum and maximum lambda of the optimal
## decision for each component - these will define the 
## ends of the bars in the tornado diagram
# Find min and max lambda for each parameter
min.spawner <- aggregate(spawner ~ parameter, data = summ.spawner, FUN=min)
max.spawner <- aggregate(spawner ~ parameter, data = summ.spawner, FUN=max)

# Combine min and max
sensit.spawner <- merge(min.spawner, max.spawner, by = "parameter")
names(sensit.spawner) <- c("Component", "Min", "Max")

## Calculate the difference between the min and max
## this will be the width of the bars in the diagram
# Calculate difference
sensit.spawner$Dif <- sensit.spawner$Max - sensit.spawner$Min

# Sort by sensitivity
sensit.spawner <- sensit.spawner[order(sensit.spawner$Dif), ]

## Create tornado diagram
# Reformat your data
tornado_spawner <- cbind(sensit.spawner$Min, sensit.spawner$Dif)
rownames(tornado_spawner) <- sensit.spawner$Component
influential_spawner <- tornado_spawner[tornado_spawner[,2] > 1e-3, ] 

png("C:/Users/gilliger/Box/Erin Dissertation/SIT sturgeon models/Green Sturgeon Model code/Results/Sensitivity Results/tornado_spawner.png",
    width = 1200, height = 1500, res = 120)
par(oma = c(0, 0, 0, 0), mar = c(5, 10, 2, 2)) 
minz <- min(sensit.spawner$Min)
maxz <- max(sensit.spawner$Max)

barplot2(t(influential_spawner), 
         beside = FALSE, 
         horiz = TRUE,
         xlim = c(min(sensit.spawner$Min)*0.95, max(sensit.spawner$Max)*1.05),  # Adjusted to fit lambda values
         col = c("white", "black"),
         border = c("white", "black"),
         las = 1,
         cex.names = 0.9,
         xlab = "Spawner abundance")
box()
dev.off()


##############################################
###     Now Do Number of Rivers
##############################################

param_sensitivities <- list()
parameters <- unique(one.way$parm1)

for(param in parameters) {
  # Get all data for this parameter across all increments
  param_data <- one.way[one.way$parm1 == param, ]
  
  # Check what increments exist for this parameter
  increments <- unique(param_data$incr1)
  
  for(incr in increments) {
    # Filter to just this parameter-increment combination
    subset_data <- param_data[param_data$incr1 == incr, ]
    
    # Now calculate recruitment values for this specific parameter-increment combination
    strategies <- unique(subset_data$Strategy)
    
    # For each strategy, get the lambda value
    strategy_rivers<- numeric(length(strategies))
    
    for(i in 1:length(strategies)) {
      strat <- strategies[i]
      strat_data <- subset_data[subset_data$Strategy == strat, ]
      
      # Calculate mean recruit value for this strategy
      river_mean <- mean(strat_data$num.rivers, na.rm = TRUE)
      
      # Store just the recruit value 
      strategy_rivers[i] <- river_mean
    }
    
    # Store results for this parameter-increment combination
    result_df <- data.frame(
      decision = strategies,
      parameter = param,
      increment = incr,
      river = strategy_rivers
    )
    
    param_sensitivities[[paste(param, incr, sep="_")]] <- result_df
  }
}

# Combine all results
rivers.one.way <- do.call(rbind, param_sensitivities)

# Write to CSV if needed
# write.csv(keep.one.way, "Lambda.Sens.Results", row.names = FALSE)
sens.river <- rivers.one.way

## Step 1: Find optimal decision (greatest lambda) for each combination
## of parameter and increment
summ.river <- aggregate(river ~ parameter + increment, data = sens.river, FUN=max)

### Now find the minimum and maximum lambda of the optimal
## decision for each component - these will define the 
## ends of the bars in the tornado diagram
# Find min and max lambda for each parameter
min.river <- aggregate(river ~ parameter, data = summ.river, FUN=min)
max.river <- aggregate(river ~ parameter, data = summ.river, FUN=max)

# Combine min and max
sensit.river <- merge(min.river, max.river, by = "parameter")
names(sensit.river) <- c("Component", "Min", "Max")

## Calculate the difference between the min and max
## this will be the width of the bars in the diagram
# Calculate difference
sensit.river$Dif <- sensit.river$Max - sensit.river$Min

# Sort by sensitivity
sensit.river <- sensit.river[order(sensit.river$Dif), ]

## Create tornado diagram
# Reformat your data
tornado_river <- cbind(sensit.river$Min, sensit.river$Dif)
rownames(tornado_river) <- sensit.river$Component
influential_river <- tornado_river[tornado_river[,2] > 1e-3, ] 

png("C:/Users/gilliger/Box/Erin Dissertation/SIT sturgeon models/Green Sturgeon Model code/Results/Sensitivity Results/tornado_river.png",
    width = 1200, height = 1500, res = 120)
par(oma = c(0, 0, 0, 0), mar = c(5, 10, 2, 2)) 
minz <- min(sensit.river$Min)
maxz <- max(sensit.river$Max)

barplot2(t(influential_river), 
         beside = FALSE, 
         horiz = TRUE,
         xlim = c(min(sensit.river$Min)*0.95, max(sensit.river$Max)*1.05),  # Adjusted to fit lambda values
         col = c("white", "black"),
         border = c("white", "black"),
         las = 1,
         cex.names = 0.9,
         xlab = "Number of Rivers")
box()
dev.off()



##############################################
###     Prop Scored lambda
############################################
# Store original lambda values first, then normalize at the end
param_sensitivities <- list()
parameters <- unique(one.way$parm1)
for(param in parameters) {
  # Get all data for this parameter across all increments
  param_data <- one.way[one.way$parm1 == param, ]
  
  # Check what increments exist for this parameter
  increments <- unique(param_data$incr1)
  
  for(incr in increments) {
    # Filter to just this parameter-increment combination
    subset_data <- param_data[param_data$incr1 == incr, ]
    
    # Calculate lambda values for this specific parameter-increment combination
    strategies <- unique(subset_data$Strategy)
    strategy_lambdas <- numeric(length(strategies))
    
    for(i in 1:length(strategies)) {
      strat <- strategies[i]
      strat_data <- subset_data[subset_data$Strategy == strat, ]
      
      # Calculate mean lambda value for this strategy
      lambda_mean <- mean(strat_data$lambda, na.rm = TRUE)
      strategy_lambdas[i] <- lambda_mean
    }
    
    # Store results for this parameter-increment combination
    result_df <- data.frame(
      decision = strategies,
      parameter = param,
      increment = incr,
      lambda = strategy_lambdas
    )
    
    param_sensitivities[[paste(param, incr, sep="_")]] <- result_df
  }
}

# Combine all results
Abund.one.way <- do.call(rbind, param_sensitivities)
sens.abund <- Abund.one.way

## Step 1: Find optimal decision (greatest lambda) for each combination
## of parameter and increment
summ.abund <- aggregate(lambda ~ parameter + increment, data = sens.abund, FUN=max)

## Now find the minimum and maximum lambda of the optimal
## decision for each component
min.lambda <- aggregate(lambda ~ parameter, data = summ.abund, FUN=min)
max.lambda <- aggregate(lambda ~ parameter, data = summ.abund, FUN=max)

# Combine min and max
sensit.abund <- merge(min.lambda, max.lambda, by = "parameter")
names(sensit.abund) <- c("Component", "Min", "Max")

# Calculate proportional values after aggregation
# First get global min and max for scaling
global_min <- min(sensit.abund$Min)
global_max <- max(sensit.abund$Max)

# Apply proportional scaling to both Min and Max columns
sensit.abund$Min_prop <- (sensit.abund$Min - global_min) / (global_max - global_min)
sensit.abund$Max_prop <- (sensit.abund$Max - global_min) / (global_max - global_min)

## Calculate the difference between the min and max
sensit.abund$Dif <- sensit.abund$Max_prop - sensit.abund$Min_prop

# Sort by sensitivity
sensit.abund <- sensit.abund[order(sensit.abund$Dif), ]

## Create tornado diagram
# Reformat your data
tornado_abund <- cbind(sensit.abund$Min_prop, sensit.abund$Dif)
rownames(tornado_abund) <- sensit.abund$Component

# Sort by influence (Dif) and select top 25
tornado_sorted <- tornado_abund[order(tornado_abund[,2], decreasing = TRUE), ]
influential_abund <- tornado_sorted[1:25, ]
influential_abund <- influential_abund[nrow(influential_abund):1, ] #reverse the order

# Add lifestage information from parm.stages
# Create a mapping between parameters and lifestages
param_lifestage_map <- setNames(parm.stages$Lifestage, parm.stages$Parameter)

# Add lifestage column to sensit.abund
sensit.abund$Lifestage <- param_lifestage_map[sensit.abund$Component]


# Get lifestages for the influential parameters
influential_components <- rownames(influential_abund)
influential_lifestages <- sensit.abund$Lifestage[match(influential_components, sensit.abund$Component)]


# Define colors for each lifestage
lifestage_colors <- c("juvenile" = "blue", "yoy" = "red", "adult" = "gold")

# Create color vector for the bars
bar_colors <- lifestage_colors[influential_lifestages]

png("C:/Users/gilliger/Box/Erin Dissertation/SIT sturgeon models/Green Sturgeon Model code/Results/Sensitivity Results/tornadoCOLOR_lambdaPropscore.png",
    width = 1200, height = 1500, res = 120)
par(oma = c(0, 0, 0, 0), mar = c(5, 10, 2, 2)) 

# Get bar positions without plotting
bp <- barplot2(t(influential_abund), 
               beside = FALSE, 
               horiz = TRUE,
               xlim = c(-0.01, 1.01),
               col = c("white", "black"),
               border = c("white", "black"),
               las = 1,
               cex.names = 0.9,
               plot = FALSE)

# Plot the bars
barplot2(t(influential_abund), 
         beside = FALSE, 
         horiz = TRUE,
         xlim = c(-0.01, 1.01),
         col = c("white", "black"),
         border = c("white", "black"),
         las = 1,
         cex.names = 0.9,
         xlab = "Proportionally scored abundance")

# Add colored rectangles based on lifestage
n <- length(influential_components)
for(i in 1:n) {
  rect(influential_abund[i,1], bp[i] - 0.4, 
       influential_abund[i,1] + influential_abund[i,2], bp[i] + 0.4,
       col = as.character(bar_colors[i]),
       border = "black")
}

# Add a legend with increased size
legend("bottomright", 
       legend = c("juvenile", "yoy", "adult"),
       fill = c("blue", "red", "gold"),
       title = "Lifestage",
       cex = 1.2)

box()
dev.off()

##############################################
###     Prop Scored Recruit
############################################
# Store original lambda values first, then normalize at the end
param_sensitivities <- list()
parameters <- unique(one.way$parm1)
for(param in parameters) {
  # Get all data for this parameter across all increments
  param_data <- one.way[one.way$parm1 == param, ]
  
  # Check what increments exist for this parameter
  increments <- unique(param_data$incr1)
  
  for(incr in increments) {
    # Filter to just this parameter-increment combination
    subset_data <- param_data[param_data$incr1 == incr, ]
    
    # Calculate lambda values for this specific parameter-increment combination
    strategies <- unique(subset_data$Strategy)
    strategy_recruits <- numeric(length(strategies))
    
    for(i in 1:length(strategies)) {
      strat <- strategies[i]
      strat_data <- subset_data[subset_data$Strategy == strat, ]
      
      # Calculate mean recruit value for this strategy
      recruit_mean <- mean(strat_data$recruit, na.rm = TRUE)
      strategy_recruits[i] <- recruit_mean
    }
    
    # Store results for this parameter-increment combination
    result_df <- data.frame(
      decision = strategies,
      parameter = param,
      increment = incr,
      recruit = strategy_recruits
    )
    
    param_sensitivities[[paste(param, incr, sep="_")]] <- result_df
  }
}

# Combine all results
recruit.one.way <- do.call(rbind, param_sensitivities)
sens.recruit <- recruit.one.way

## Step 1: Find optimal decision (greatest recruit) for each combination
## of parameter and increment
summ.recruit <- aggregate(recruit ~ parameter + increment, data = sens.recruit, FUN=max)

## Now find the minimum and maximum recruit of the optimal
## decision for each component
min.recruit <- aggregate(recruit ~ parameter, data = summ.recruit, FUN=min)
max.recruit <- aggregate(recruit ~ parameter, data = summ.recruit, FUN=max)

# Combine min and max
sensit.recruit <- merge(min.recruit, max.recruit, by = "parameter")
names(sensit.recruit) <- c("Component", "Min", "Max")

# Calculate proportional values after aggregation
# First get global min and max for scaling
global_min <- min(sensit.recruit$Min)
global_max <- max(sensit.recruit$Max)

# Apply proportional scaling to both Min and Max columns
sensit.recruit$Min_prop <- (sensit.recruit$Min - global_min) / (global_max - global_min)
sensit.recruit$Max_prop <- (sensit.recruit$Max - global_min) / (global_max - global_min)

## Calculate the difference between the min and max
sensit.recruit$Dif <- sensit.recruit$Max_prop - sensit.recruit$Min_prop

# Sort by sensitivity
sensit.recruit <- sensit.recruit[order(sensit.recruit$Dif), ]

## Create tornado diagram
# Reformat your data
tornado_recruit <- cbind(sensit.recruit$Min_prop, sensit.recruit$Dif)
rownames(tornado_recruit) <- sensit.recruit$Component

png("C:/Users/gilliger/Box/Erin Dissertation/SIT sturgeon models/Green Sturgeon Model code/Results/Sensitivity Results/tornado_recruitPropscore.png",
    width = 1200, height = 1500, res = 120)
par(oma = c(0, 0, 0, 0), mar = c(5, 10, 2, 2)) 
barplot2(t(tornado_recruit), 
         beside = FALSE, 
         horiz = TRUE,
         xlim = c(min(sensit.recruit$Min_prop)*0.95, max(sensit.recruit$Max_prop)*1.05),
         col = c("white", "black"),
         border = c("white", "black"),
         las = 1,
         cex.names = 0.9,
         xlab = "Proportionally scored recruitment")
box()
dev.off()



##############################################
###     Prop Scored Spawners
############################################
# Store original lambda values first, then normalize at the end
param_sensitivities <- list()
parameters <- unique(one.way$parm1)
for(param in parameters) {
  # Get all data for this parameter across all increments
  param_data <- one.way[one.way$parm1 == param, ]
  
  # Check what increments exist for this parameter
  increments <- unique(param_data$incr1)
  
  for(incr in increments) {
    # Filter to just this parameter-increment combination
    subset_data <- param_data[param_data$incr1 == incr, ]
    
    # Calculate lambda values for this specific parameter-increment combination
    strategies <- unique(subset_data$Strategy)
    strategy_spawners <- numeric(length(strategies))
    
    for(i in 1:length(strategies)) {
      strat <- strategies[i]
      strat_data <- subset_data[subset_data$Strategy == strat, ]
      
      # Calculate mean spawner value for this strategy
      spawner_mean <- mean(strat_data$spawners, na.rm = TRUE)
      strategy_spawners[i] <- spawner_mean
    }
    
    # Store results for this parameter-increment combination
    result_df <- data.frame(
      decision = strategies,
      parameter = param,
      increment = incr,
      spawner = strategy_spawners
    )
    
    param_sensitivities[[paste(param, incr, sep="_")]] <- result_df
  }
}

# Combine all results
spawner.one.way <- do.call(rbind, param_sensitivities)
sens.spawner <- spawner.one.way

## Step 1: Find optimal decision (greatest spawner) for each combination
## of parameter and increment
summ.spawner <- aggregate(spawner ~ parameter + increment, data = sens.spawner, FUN=max)

## Now find the minimum and maximum spawner of the optimal
## decision for each component
min.spawner <- aggregate(spawner ~ parameter, data = summ.spawner, FUN=min)
max.spawner <- aggregate(spawner ~ parameter, data = summ.spawner, FUN=max)

# Combine min and max
sensit.spawner <- merge(min.spawner, max.spawner, by = "parameter")
names(sensit.spawner) <- c("Component", "Min", "Max")

# Calculate proportional values after aggregation
# First get global min and max for scaling
global_min <- min(sensit.spawner$Min)
global_max <- max(sensit.spawner$Max)

# Apply proportional scaling to both Min and Max columns
sensit.spawner$Min_prop <- (sensit.spawner$Min - global_min) / (global_max - global_min)
sensit.spawner$Max_prop <- (sensit.spawner$Max - global_min) / (global_max - global_min)

## Calculate the difference between the min and max
sensit.spawner$Dif <- sensit.spawner$Max_prop - sensit.spawner$Min_prop

# Sort by sensitivity
sensit.spawner <- sensit.spawner[order(sensit.spawner$Dif), ]

## Create tornado diagram
# Reformat your data
tornado_spawner <- cbind(sensit.spawner$Min_prop, sensit.spawner$Dif)
rownames(tornado_spawner) <- sensit.spawner$Component


# Sort by influence (Dif) and select top 25
tornado_sorted <- tornado_spawner[order(tornado_spawner[,2], decreasing = TRUE), ]
influential_spawner <- tornado_sorted[1:25, ]
influential_spawner <- influential_spawner[nrow(influential_spawner):1, ] #reverse the order

# Add lifestage information from parm.stages
# Create a mapping between parameters and lifestages
param_lifestage_map <- setNames(parm.stages$Lifestage, parm.stages$Parameter)

# Add lifestage column to sensit.abund
sensit.spawner$Lifestage <- param_lifestage_map[sensit.spawner$Component]


# Get lifestages for the influential parameters
influential_components <- rownames(influential_spawner)
influential_lifestages <- sensit.spawner$Lifestage[match(influential_components, sensit.spawner$Component)]

# Define colors for each lifestage
lifestage_colors <- c("juvenile" = "blue", "yoy" = "red", "adult" = "gold")

# Create color vector for the bars
bar_colors <- lifestage_colors[influential_lifestages]

png("C:/Users/gilliger/Box/Erin Dissertation/SIT sturgeon models/Green Sturgeon Model code/Results/Sensitivity Results/tornadoCOLOR_spawnerPropscore.png",
    width = 1200, height = 1500, res = 120)
par(oma = c(0, 0, 0, 0), mar = c(5, 10, 2, 2)) 
# Get bar positions without plotting
bp <- barplot2(t(influential_spawner), 
               beside = FALSE, 
               horiz = TRUE,
               xlim = c(-0.01, 1.01),
               col = c("white", "black"),
               border = c("white", "black"),
               las = 1,
               cex.names = 0.9,
               plot = FALSE)

# Plot the bars
barplot2(t(influential_spawner), 
         beside = FALSE, 
         horiz = TRUE,
         xlim = c(-0.01, 1.01),
         col = c("white", "black"),
         border = c("white", "black"),
         las = 1,
         cex.names = 0.9,
         xlab = "Proportionally scored spawner abundance")

# Add colored rectangles based on lifestage
n <- length(influential_components)
for(i in 1:n) {
  rect(influential_spawner[i,1], bp[i] - 0.4, 
       influential_spawner[i,1] + influential_spawner[i,2], bp[i] + 0.4,
       col = as.character(bar_colors[i]),
       border = "black")
}

# Add a legend with increased size
legend("bottomright", 
       legend = c("juvenile", "yoy", "adult"),
       fill = c("blue", "red", "gold"),
       title = "Lifestage",
       cex = 1.2)

box()
dev.off()


##############################################
###     Prop Scored Large Spawners
############################################
# Store original lambda values first, then normalize at the end
param_sensitivities <- list()
parameters <- unique(one.way$parm1)
for(param in parameters) {
  # Get all data for this parameter across all increments
  param_data <- one.way[one.way$parm1 == param, ]
  
  # Check what increments exist for this parameter
  increments <- unique(param_data$incr1)
  
  for(incr in increments) {
    # Filter to just this parameter-increment combination
    subset_data <- param_data[param_data$incr1 == incr, ]
    
    # Calculate lambda values for this specific parameter-increment combination
    strategies <- unique(subset_data$Strategy)
    strategy_lg.spawners <- numeric(length(strategies))
    
    for(i in 1:length(strategies)) {
      strat <- strategies[i]
      strat_data <- subset_data[subset_data$Strategy == strat, ]
      
      # Calculate mean lg.spawner value for this strategy
      lg.spawner_mean <- mean(strat_data$large.spawners, na.rm = TRUE)
      strategy_lg.spawners[i] <- lg.spawner_mean
    }
    
    # Store results for this parameter-increment combination
    result_df <- data.frame(
      decision = strategies,
      parameter = param,
      increment = incr,
      lg.spawner = strategy_lg.spawners
    )
    
    param_sensitivities[[paste(param, incr, sep="_")]] <- result_df
  }
}

# Combine all results
lg.spawner.one.way <- do.call(rbind, param_sensitivities)
sens.lg.spawner <- lg.spawner.one.way

## Step 1: Find optimal decision (greatest lg.spawner) for each combination
## of parameter and increment
summ.lg.spawner <- aggregate(lg.spawner ~ parameter + increment, data = sens.lg.spawner, FUN=max)

## Now find the minimum and maximum lg.spawner of the optimal
## decision for each component
min.lg.spawner <- aggregate(lg.spawner ~ parameter, data = summ.lg.spawner, FUN=min)
max.lg.spawner <- aggregate(lg.spawner ~ parameter, data = summ.lg.spawner, FUN=max)

# Combine min and max
sensit.lg.spawner <- merge(min.lg.spawner, max.lg.spawner, by = "parameter")
names(sensit.lg.spawner) <- c("Component", "Min", "Max")

# Calculate proportional values after aggregation
# First get global min and max for scaling
global_min <- min(sensit.lg.spawner$Min)
global_max <- max(sensit.lg.spawner$Max)

# Apply proportional scaling to both Min and Max columns
sensit.lg.spawner$Min_prop <- (sensit.lg.spawner$Min - global_min) / (global_max - global_min)
sensit.lg.spawner$Max_prop <- (sensit.lg.spawner$Max - global_min) / (global_max - global_min)

## Calculate the difference between the min and max
sensit.lg.spawner$Dif <- sensit.lg.spawner$Max_prop - sensit.lg.spawner$Min_prop

# Sort by sensitivity
sensit.lg.spawner <- sensit.lg.spawner[order(sensit.lg.spawner$Dif), ]

## Create tornado diagram
# Reformat your data
tornado_lg.spawner <- cbind(sensit.lg.spawner$Min_prop, sensit.lg.spawner$Dif)
rownames(tornado_lg.spawner) <- sensit.lg.spawner$Component


# Sort by influence (Dif) and select top 25
tornado_sorted <- tornado_lg.spawner[order(tornado_lg.spawner[,2], decreasing = TRUE), ]
influential_lg.spawner <- tornado_sorted[1:25, ]
influential_lg.spawner <- influential_lg.spawner[nrow(influential_lg.spawner):1, ] #reverse the order

# Add lifestage information from parm.stages
# Create a mapping between parameters and lifestages
param_lifestage_map <- setNames(parm.stages$Lifestage, parm.stages$Parameter)

# Add lifestage column to sensit.abund
sensit.lg.spawner$Lifestage <- param_lifestage_map[sensit.lg.spawner$Component]


# Get lifestages for the influential parameters
influential_components <- rownames(influential_lg.spawner)
influential_lifestages <- sensit.lg.spawner$Lifestage[match(influential_components, sensit.lg.spawner$Component)]

# Define colors for each lifestage
lifestage_colors <- c("juvenile" = "blue", "yoy" = "red", "adult" = "gold")

# Create color vector for the bars
bar_colors <- lifestage_colors[influential_lifestages]

png("C:/Users/gilliger/Box/Erin Dissertation/SIT sturgeon models/Green Sturgeon Model code/Results/Sensitivity Results/tornadoCOLOR_LGspawnerPropscore.png",
    width = 1200, height = 1500, res = 120)
par(oma = c(0, 0, 0, 0), mar = c(5, 10, 2, 2)) 
# Get bar positions without plotting
bp <- barplot2(t(influential_lg.spawner), 
               beside = FALSE, 
               horiz = TRUE,
               xlim = c(-0.01, 1.01),
               col = c("white", "black"),
               border = c("white", "black"),
               las = 1,
               cex.names = 0.9,
               plot = FALSE)

# Plot the bars
barplot2(t(influential_lg.spawner), 
         beside = FALSE, 
         horiz = TRUE,
         xlim = c(-0.01, 1.01),
         col = c("white", "black"),
         border = c("white", "black"),
         las = 1,
         cex.names = 0.9,
         xlab = "Proportionally scored large spawner abundance")

# Add colored rectangles based on lifestage
n <- length(influential_components)
for(i in 1:n) {
  rect(influential_lg.spawner[i,1], bp[i] - 0.4, 
       influential_lg.spawner[i,1] + influential_lg.spawner[i,2], bp[i] + 0.4,
       col = as.character(bar_colors[i]),
       border = "black")
}

# Add a legend with increased size
legend("bottomright", 
       legend = c("juvenile", "yoy", "adult"),
       fill = c("blue", "red", "gold"),
       title = "Lifestage",
       cex = 1.2)

box()
dev.off()


##############################################
###     Prop Scored holders
############################################
# Store original lambda values first, then normalize at the end
param_sensitivities <- list()
parameters <- unique(one.way$parm1)
for(param in parameters) {
  # Get all data for this parameter across all increments
  param_data <- one.way[one.way$parm1 == param, ]
  
  # Check what increments exist for this parameter
  increments <- unique(param_data$incr1)
  
  for(incr in increments) {
    # Filter to just this parameter-increment combination
    subset_data <- param_data[param_data$incr1 == incr, ]
    
    # Calculate lambda values for this specific parameter-increment combination
    strategies <- unique(subset_data$Strategy)
    strategy_holders <- numeric(length(strategies))
    
    for(i in 1:length(strategies)) {
      strat <- strategies[i]
      strat_data <- subset_data[subset_data$Strategy == strat, ]
      
      # Calculate mean holder value for this strategy
      holder_mean <- mean(strat_data$num.holding, na.rm = TRUE)
      strategy_holders[i] <- holder_mean
    }
    
    # Store results for this parameter-increment combination
    result_df <- data.frame(
      decision = strategies,
      parameter = param,
      increment = incr,
      holder = strategy_holders
    )
    
    param_sensitivities[[paste(param, incr, sep="_")]] <- result_df
  }
}

# Combine all results
holder.one.way <- do.call(rbind, param_sensitivities)
sens.holder <- holder.one.way

## Step 1: Find optimal decision (greatest holder) for each combination
## of parameter and increment
summ.holder <- aggregate(holder ~ parameter + increment, data = sens.holder, FUN=max)

## Now find the minimum and maximum holder of the optimal
## decision for each component
min.holder <- aggregate(holder ~ parameter, data = summ.holder, FUN=min)
max.holder <- aggregate(holder ~ parameter, data = summ.holder, FUN=max)

# Combine min and max
sensit.holder <- merge(min.holder, max.holder, by = "parameter")
names(sensit.holder) <- c("Component", "Min", "Max")

# Calculate proportional values after aggregation
# First get global min and max for scaling
global_min <- min(sensit.holder$Min)
global_max <- max(sensit.holder$Max)

# Apply proportional scaling to both Min and Max columns
sensit.holder$Min_prop <- (sensit.holder$Min - global_min) / (global_max - global_min)
sensit.holder$Max_prop <- (sensit.holder$Max - global_min) / (global_max - global_min)

## Calculate the difference between the min and max
sensit.holder$Dif <- sensit.holder$Max_prop - sensit.holder$Min_prop

# Sort by sensitivity
sensit.holder <- sensit.holder[order(sensit.holder$Dif), ]

## Create tornado diagram
# Reformat your data
tornado_holder <- cbind(sensit.holder$Min_prop, sensit.holder$Dif)
rownames(tornado_holder) <- sensit.holder$Component

png("C:/Users/gilliger/Box/Erin Dissertation/SIT sturgeon models/Green Sturgeon Model code/Results/Sensitivity Results/tornado_holderPropscore.png",
    width = 1200, height = 1500, res = 120)
par(oma = c(0, 0, 0, 0), mar = c(5, 10, 2, 2)) 
barplot2(t(tornado_holder), 
         beside = FALSE, 
         horiz = TRUE,
         xlim = c(min(sensit.holder$Min_prop)*0.95, max(sensit.holder$Max_prop)*1.05),
         col = c("white", "black"),
         border = c("white", "black"),
         las = 1,
         cex.names = 0.9,
         xlab = "Proportionally scored Holders")
box()
dev.off()


##############################################
###     Prop Scored Rivers
############################################

param_sensitivities <- list()
parameters <- unique(one.way$parm1)
for(param in parameters) {
  # Get all data for this parameter across all increments
  param_data <- one.way[one.way$parm1 == param, ]
  
  # Check what increments exist for this parameter
  increments <- unique(param_data$incr1)
  
  for(incr in increments) {
    # Filter to just this parameter-increment combination
    subset_data <- param_data[param_data$incr1 == incr, ]
    
    # Calculate lambda values for this specific parameter-increment combination
    strategies <- unique(subset_data$Strategy)
    strategy_rivers <- numeric(length(strategies))
    
    for(i in 1:length(strategies)) {
      strat <- strategies[i]
      strat_data <- subset_data[subset_data$Strategy == strat, ]
      
      # Calculate mean river value for this strategy
      river_mean <- mean(strat_data$num.rivers, na.rm = TRUE)
      strategy_rivers[i] <- river_mean
    }
    
    # Store results for this parameter-increment combination
    result_df <- data.frame(
      decision = strategies,
      parameter = param,
      increment = incr,
      river = strategy_rivers
    )
    
    param_sensitivities[[paste(param, incr, sep="_")]] <- result_df
  }
}

# Combine all results
river.one.way <- do.call(rbind, param_sensitivities)
sens.river <- river.one.way

## Step 1: Find optimal decision (greatest river) for each combination
## of parameter and increment
summ.river <- aggregate(river ~ parameter + increment, data = sens.river, FUN=max)

## Now find the minimum and maximum river of the optimal
## decision for each component
min.river <- aggregate(river ~ parameter, data = summ.river, FUN=min)
max.river <- aggregate(river ~ parameter, data = summ.river, FUN=max)

# Combine min and max
sensit.river <- merge(min.river, max.river, by = "parameter")
names(sensit.river) <- c("Component", "Min", "Max")

# Calculate proportional values after aggregation
# First get global min and max for scaling
global_min <- min(sensit.river$Min)
global_max <- max(sensit.river$Max)

# Apply proportional scaling to both Min and Max columns
sensit.river$Min_prop <- (sensit.river$Min - global_min) / (global_max - global_min)
sensit.river$Max_prop <- (sensit.river$Max - global_min) / (global_max - global_min)

## Calculate the difference between the min and max
sensit.river$Dif <- sensit.river$Max_prop - sensit.river$Min_prop

# Sort by sensitivity
sensit.river <- sensit.river[order(sensit.river$Dif), ]

## Create tornado diagram
# Reformat your data
tornado_river <- cbind(sensit.river$Min_prop, sensit.river$Dif)
rownames(tornado_river) <- sensit.river$Component

# Sort by influence (Dif) and select top 25
tornado_sorted <- tornado_river[order(tornado_river[,2], decreasing = TRUE), ]
influential_river <- tornado_sorted[1:25, ]
influential_river <- influential_river[nrow(influential_river):1, ] #reverse the order

# Add lifestage information from parm.stages
# Create a mapping between parameters and lifestages
param_lifestage_map <- setNames(parm.stages$Lifestage, parm.stages$Parameter)

# Add lifestage column to sensit.river
sensit.river$Lifestage <- param_lifestage_map[sensit.river$Component]


# Get lifestages for the influential parameters
influential_components <- rownames(influential_river)
influential_lifestages <- sensit.river$Lifestage[match(influential_components, sensit.river$Component)]


# Define colors for each lifestage
lifestage_colors <- c("juvenile" = "blue", "yoy" = "red", "adult" = "gold")

# Create color vector for the bars
bar_colors <- lifestage_colors[influential_lifestages]

png("C:/Users/gilliger/Box/Erin Dissertation/SIT sturgeon models/Green Sturgeon Model code/Results/Sensitivity Results/tornadoCOLOR_riverPropscore.png",
    width = 1200, height = 1500, res = 120)
par(oma = c(0, 0, 0, 0), mar = c(5, 10, 2, 2)) 
# Get bar positions without plotting
bp <- barplot2(t(influential_river), 
               beside = FALSE, 
               horiz = TRUE,
               xlim = c(-0.01, 1.01),
               col = c("white", "black"),
               border = c("white", "black"),
               las = 1,
               cex.names = 0.9,
               plot = FALSE)

# Plot the bars
barplot2(t(influential_river), 
         beside = FALSE, 
         horiz = TRUE,
         xlim = c(-0.01, 1.01),
         col = c("white", "black"),
         border = c("white", "black"),
         las = 1,
         cex.names = 0.9,
         xlab = "Proportionally scored # of rivers")

# Add colored rectangles based on lifestage
n <- length(influential_components)
for(i in 1:n) {
  rect(influential_river[i,1], bp[i] - 0.4, 
       influential_river[i,1] + influential_river[i,2], bp[i] + 0.4,
       col = as.character(bar_colors[i]),
       border = "black")
}

# Add a legend with increased size
legend("bottomright", 
       legend = c("juvenile", "yoy", "adult"),
       fill = c("blue", "red", "gold"),
       title = "Lifestage",
       cex = 1.2)

box()
dev.off()


##############################################
###     Prop Scored Rearing Duration
############################################

param_sensitivities <- list()
parameters <- unique(one.way$parm1)
for(param in parameters) {
  # Get all data for this parameter across all increments
  param_data <- one.way[one.way$parm1 == param, ]
  
  # Check what increments exist for this parameter
  increments <- unique(param_data$incr1)
  
  for(incr in increments) {
    # Filter to just this parameter-increment combination
    subset_data <- param_data[param_data$incr1 == incr, ]
    
    # Calculate lambda values for this specific parameter-increment combination
    strategies <- unique(subset_data$Strategy)
    strategy_RearDurs <- numeric(length(strategies))
    
    for(i in 1:length(strategies)) {
      strat <- strategies[i]
      strat_data <- subset_data[subset_data$Strategy == strat, ]
      
      # Calculate mean RearDur value for this strategy
      RearDur_mean <- mean(strat_data$avg_rearDur, na.rm = TRUE)
      strategy_RearDurs[i] <- RearDur_mean
    }
    
    # Store results for this parameter-increment combination
    result_df <- data.frame(
      decision = strategies,
      parameter = param,
      increment = incr,
      RearDur = strategy_RearDurs
    )
    
    param_sensitivities[[paste(param, incr, sep="_")]] <- result_df
  }
}

# Combine all results
RearDur.one.way <- do.call(rbind, param_sensitivities)
sens.RearDur <- RearDur.one.way

## Step 1: Find optimal decision (greatest RearDur) for each combination
## of parameter and increment
summ.RearDur <- aggregate(RearDur ~ parameter + increment, data = sens.RearDur, FUN=max)

## Now find the minimum and maximum RearDur of the optimal
## decision for each component
min.RearDur <- aggregate(RearDur ~ parameter, data = summ.RearDur, FUN=min)
max.RearDur <- aggregate(RearDur ~ parameter, data = summ.RearDur, FUN=max)

# Combine min and max
sensit.RearDur <- merge(min.RearDur, max.RearDur, by = "parameter")
names(sensit.RearDur) <- c("Component", "Min", "Max")

# Calculate proportional values after aggregation
# First get global min and max for scaling
global_min <- min(sensit.RearDur$Min)
global_max <- max(sensit.RearDur$Max)

# Apply proportional scaling to both Min and Max columns
sensit.RearDur$Min_prop <- (sensit.RearDur$Min - global_min) / (global_max - global_min)
sensit.RearDur$Max_prop <- (sensit.RearDur$Max - global_min) / (global_max - global_min)

## Calculate the difference between the min and max
sensit.RearDur$Dif <- sensit.RearDur$Max_prop - sensit.RearDur$Min_prop

# Sort by sensitivity
sensit.RearDur <- sensit.RearDur[order(sensit.RearDur$Dif), ]

## Create tornado diagram
# Reformat your data
tornado_RearDur <- cbind(sensit.RearDur$Min_prop, sensit.RearDur$Dif)
rownames(tornado_RearDur) <- sensit.RearDur$Component

# Sort by influence (Dif) and select top 25
tornado_sorted <- tornado_RearDur[order(tornado_RearDur[,2], decreasing = TRUE), ]
influential_RearDur <- tornado_sorted[1:25, ]
influential_RearDur <- influential_RearDur[nrow(influential_RearDur):1, ] #reverse the order

# Add lifestage information from parm.stages
# Create a mapping between parameters and lifestages
param_lifestage_map <- setNames(parm.stages$Lifestage, parm.stages$Parameter)

# Add lifestage column to sensit.RearDur
sensit.RearDur$Lifestage <- param_lifestage_map[sensit.RearDur$Component]


# Get lifestages for the influential parameters
influential_components <- rownames(influential_RearDur)
influential_lifestages <- sensit.RearDur$Lifestage[match(influential_components, sensit.RearDur$Component)]


# Define colors for each lifestage
lifestage_colors <- c("juvenile" = "blue", "yoy" = "red", "adult" = "gold")

# Create color vector for the bars
bar_colors <- lifestage_colors[influential_lifestages]

png("C:/Users/gilliger/Box/Erin Dissertation/SIT sturgeon models/Green Sturgeon Model code/Results/Sensitivity Results/tornadoCOLOR_RearDurPropscore.png",
    width = 1200, height = 1500, res = 120)
par(oma = c(0, 0, 0, 0), mar = c(5, 10, 2, 2)) 
# Get bar positions without plotting
bp <- barplot2(t(influential_RearDur), 
               beside = FALSE, 
               horiz = TRUE,
               xlim = c(-0.01, 1.01),
               col = c("white", "black"),
               border = c("white", "black"),
               las = 1,
               cex.names = 0.9,
               plot = FALSE)

# Plot the bars
barplot2(t(influential_RearDur), 
         beside = FALSE, 
         horiz = TRUE,
         xlim = c(-0.01, 1.01),
         col = c("white", "black"),
         border = c("white", "black"),
         las = 1,
         cex.names = 0.9,
         xlab = "Proportionally scored Rearing Duration")

# Add colored rectangles based on lifestage
n <- length(influential_components)
for(i in 1:n) {
  rect(influential_RearDur[i,1], bp[i] - 0.4, 
       influential_RearDur[i,1] + influential_RearDur[i,2], bp[i] + 0.4,
       col = as.character(bar_colors[i]),
       border = "black")
}

# Add a legend with increased size
legend("bottomright", 
       legend = c("juvenile", "yoy", "adult"),
       fill = c("blue", "red", "gold"),
       title = "Lifestage",
       cex = 1.2)

box()
dev.off()


##############################################
###     Prop Scored Spawning Duration
############################################

param_sensitivities <- list()
parameters <- unique(one.way$parm1)
for(param in parameters) {
  # Get all data for this parameter across all increments
  param_data <- one.way[one.way$parm1 == param, ]
  
  # Check what increments exist for this parameter
  increments <- unique(param_data$incr1)
  
  for(incr in increments) {
    # Filter to just this parameter-increment combination
    subset_data <- param_data[param_data$incr1 == incr, ]
    
    # Calculate lambda values for this specific parameter-increment combination
    strategies <- unique(subset_data$Strategy)
    strategy_SpawnDurs <- numeric(length(strategies))
    
    for(i in 1:length(strategies)) {
      strat <- strategies[i]
      strat_data <- subset_data[subset_data$Strategy == strat, ]
      
      # Calculate mean SpawnDur value for this strategy
      SpawnDur_mean <- mean(strat_data$avg_spawning, na.rm = TRUE)
      strategy_SpawnDurs[i] <- SpawnDur_mean
    }
    
    # Store results for this parameter-increment combination
    result_df <- data.frame(
      decision = strategies,
      parameter = param,
      increment = incr,
      SpawnDur = strategy_SpawnDurs
    )
    
    param_sensitivities[[paste(param, incr, sep="_")]] <- result_df
  }
}

# Combine all results
SpawnDur.one.way <- do.call(rbind, param_sensitivities)
sens.SpawnDur <- SpawnDur.one.way

## Step 1: Find optimal decision (greatest SpawnDur) for each combination
## of parameter and increment
summ.SpawnDur <- aggregate(SpawnDur ~ parameter + increment, data = sens.SpawnDur, FUN=max)

## Now find the minimum and maximum SpawnDur of the optimal
## decision for each component
min.SpawnDur <- aggregate(SpawnDur ~ parameter, data = summ.SpawnDur, FUN=min)
max.SpawnDur <- aggregate(SpawnDur ~ parameter, data = summ.SpawnDur, FUN=max)

# Combine min and max
sensit.SpawnDur <- merge(min.SpawnDur, max.SpawnDur, by = "parameter")
names(sensit.SpawnDur) <- c("Component", "Min", "Max")

# Calculate proportional values after aggregation
# First get global min and max for scaling
global_min <- min(sensit.SpawnDur$Min)
global_max <- max(sensit.SpawnDur$Max)

# Apply proportional scaling to both Min and Max columns
sensit.SpawnDur$Min_prop <- (sensit.SpawnDur$Min - global_min) / (global_max - global_min)
sensit.SpawnDur$Max_prop <- (sensit.SpawnDur$Max - global_min) / (global_max - global_min)

## Calculate the difference between the min and max
sensit.SpawnDur$Dif <- sensit.SpawnDur$Max_prop - sensit.SpawnDur$Min_prop

# Sort by sensitivity
sensit.SpawnDur <- sensit.SpawnDur[order(sensit.SpawnDur$Dif), ]

## Create tornado diagram
# Reformat your data
tornado_SpawnDur <- cbind(sensit.SpawnDur$Min_prop, sensit.SpawnDur$Dif)
rownames(tornado_SpawnDur) <- sensit.SpawnDur$Component

png("C:/Users/gilliger/Box/Erin Dissertation/SIT sturgeon models/Green Sturgeon Model code/Results/Sensitivity Results/tornado_SpawnDurPropscore.png",
    width = 1200, height = 1500, res = 120)
par(oma = c(0, 0, 0, 0), mar = c(5, 10, 2, 2)) 
barplot2(t(tornado_SpawnDur), 
         beside = FALSE, 
         horiz = TRUE,
         xlim = c(min(sensit.SpawnDur$Min_prop)*0.95, max(sensit.SpawnDur$Max_prop)*1.05),
         col = c("white", "black"),
         border = c("white", "black"),
         las = 1,
         cex.names = 0.9,
         xlab = "Proportionally scored Spawning Duration")
box()
dev.off()


##############################################
###     Response profile for each attribute
########################################
analyze_attribute_response_profile <- function(data, attribute) {
  # Get list of unique parameters
  parameters <- unique(data$parameter)
  
  # Create empty data frame to store results
  changes_df <- data.frame(
    Parameter = character(),
    times = numeric(),
    stringsAsFactors = FALSE
  )
  
  # Process each parameter
  for(param in parameters) {
    # Get data for this parameter only
    param_data <- subset(data, parameter == param)
    
    # Initialize counter for strategy changes
    changes <- 0
    prev_top3 <- NULL
    
    # For each increment, check if top 3 strategies change
    for(inc in sort(unique(param_data$increment))) {
      # Get raw data for this parameter-increment combination
      raw_data <- one.way[one.way$parm1 == param & one.way$incr1 == inc, ]
      
      # Calculate mean attribute value for each strategy
      strategies <- unique(raw_data$Strategy)
      strategy_values <- data.frame(
        Strategy = strategies,
        Value = numeric(length(strategies)),
        stringsAsFactors = FALSE
      )
      
      for(i in 1:length(strategies)) {
        strat <- strategies[i]
        strat_data <- raw_data[raw_data$Strategy == strat, ]
        strategy_values$Value[i] <- mean(strat_data[[attribute]], na.rm = TRUE)
      }
      
      # Sort by attribute value (descending) and get top 3 strategies
      strategy_values <- strategy_values[order(-strategy_values$Value), ]
      top3 <- head(strategy_values$Strategy, 3)
      
      # Check if this is different from previous top 3
      if(!is.null(prev_top3)) {
        # Check if top 3 changed (either order or composition)
        if(!identical(top3, prev_top3)) {
          changes <- changes + 1
        }
      }
      
      # Update previous top 3
      prev_top3 <- top3
    }
    
    # Add to results
    changes_df <- rbind(changes_df, data.frame(
      Parameter = param,
      times = changes,
      stringsAsFactors = FALSE
    ))
  }
  
  # Sort by number of changes (descending)
  changes_df <- changes_df[order(-changes_df$times), ]
  
  return(changes_df)
}

# Define colors for each attribute (using a visually distinct palette)
attribute_colors <- c(
  "lambda" = "darkblue",
  "spawners" = "orange",
  "recruit" = "skyblue",
  "num.holding" = "pink3",
  "num.rivers" = "green2",
  "avg_rearDur" = "gold",
  "utility" = 'purple'
)

# List of attributes to analyze
attributes <- c("lambda", "spawners", "recruit", 
                "num.holding", "avg_rearDur")

# Create a list to store results for each attribute
all_results <- list()

# Process each attribute
for(attr in attributes) {
  # Analyze this attribute
  result <- analyze_attribute_response_profile(infile, attr)
  result$Attribute <- attr
  
  # Store in list
  all_results[[attr]] <- result
}

top3_changes$Attribute <- "utility"
all_results[["utility"]] <- top3_changes

attributes <- c(attributes, "utility")
# Combine all results
combined_results <- do.call(rbind, all_results)
combined_results <- combined_results[combined_results[,2] > 1, ] 

# Get unique parameters across all attributes
unique_parameters <- unique(combined_results$Parameter)

# Create a matrix to hold the plot data
# Rows = parameters, Columns = attributes
plot_matrix <- matrix(0, nrow = length(unique_parameters), ncol = length(attributes))
rownames(plot_matrix) <- unique_parameters
colnames(plot_matrix) <- attributes

# Fill the matrix with values
for(i in 1:nrow(combined_results)) {
  param <- combined_results$Parameter[i]
  attr <- combined_results$Attribute[i]
  changes <- combined_results$times[i]
  
  # Find the right position in the matrix
  row_idx <- which(rownames(plot_matrix) == param)
  col_idx <- which(colnames(plot_matrix) == attr)
  
  plot_matrix[row_idx, col_idx] <- changes
}

# Create the combined plot
png("C:/Users/gilliger/Box/Erin Dissertation/SIT sturgeon models/Green Sturgeon Model code/Results/Sensitivity Results/ResponseProfile_Combined.png",
    width = 1800, height = 1200, res = 150)

# Set up barplot
par(mar = c(10, 5, 4, 10))  # Increase right margin for legend

# Reorder parameters based on total changes across all attributes
param_order <- order(rowSums(plot_matrix > 0), rowSums(plot_matrix), decreasing = TRUE)
plot_matrix <- plot_matrix[param_order, ]

# Create barplot
bp <- barplot(t(plot_matrix), 
              beside = TRUE,
              horiz = FALSE,
              las = 2,
              cex.names = 0.9,
              col = attribute_colors[colnames(plot_matrix)],
              border = NA,
              ylim = c(0, max(plot_matrix) * 1.3),  # Add space for legend
              main = "Input response profile sensitivity analysis summary",
              ylab = "Number of times top 3 ranks changed",
              xlab = "")

# Add legend 
legend("topright", 
       legend = attributes,
       fill = attribute_colors[colnames(plot_matrix)],
       cex = 0.9,
       bty = "n")

box()
dev.off()



##############################################
###Do large spawners and average spawning duration 
###separate because they're too big
##############################################
# List of attributes to analyze
attributes <- c( "avg_spawning") #"large.spawners",

# Create a list to store results for each attribute
all_results <- list()

# Process each attribute
for(attr in attributes) {
  # Analyze this attribute
  result <- analyze_attribute_response_profile(infile, attr)
  result$Attribute <- attr
  
  # Store in list
  all_results[[attr]] <- result
}
# Extract just the large spawners results
large_spawners_results <- all_results[["large.spawners"]]
large_spawners_results <- large_spawners_results[large_spawners_results[,2] > 1, ] 

# Define colors for each lifestage
lifestage_colors <- c("juvenile" = "blue", "yoy" = "red", "adult" = "gold")
#To add color cordination
param_lifestage_map <- setNames(parm.stages$Lifestage, parm.stages$Parameter)
large_spawners_results$Lifestage <- param_lifestage_map[large_spawners_results$Parameter]

bar_colors<-lifestage_colors[large_spawners_results$Lifestage]


# Create the combined plot
png("C:/Users/gilliger/Box/Erin Dissertation/SIT sturgeon models/Green Sturgeon Model code/Results/Sensitivity Results/ResponseProfile_LargeSpawners.png",
    width = 1800, height = 1200, res = 150)

# Set up the plot
par(mar = c(10, 5, 4, 2))  # Adjust margins as needed

# Order parameters by number of changes
large_spawners_results <- large_spawners_results[order(-large_spawners_results$times), ]

# Create the barplot
bp <- barplot(large_spawners_results$times,
              names.arg = large_spawners_results$Parameter,
              col = bar_colors,
              border = NA,
              las = 2,
              cex.names = 0.9,
              ylim = c(0, max(large_spawners_results$times) * 1.2),
              main = "Response Profile Large Spawners",
              ylab = "Number of times top 3 ranks changed",
              xlab = "")


# Add a legend
legend("topright", 
       legend = names(lifestage_colors),
       fill = lifestage_colors,
       title = "Life Stage",
       cex = 0.8)

box()
dev.off()



#Avg spawn dur
# Extract just the large spawners results
SpawnDur_results <- all_results[["avg_spawning"]]
SpawnDur_results <- SpawnDur_results[SpawnDur_results[,2] > 0, ] 

#To add color cordination
SpawnDur_results$Lifestage <- param_lifestage_map[SpawnDur_results$Parameter]

bar_colors<-lifestage_colors[SpawnDur_results$Lifestage]


# Create the combined plot
png("C:/Users/gilliger/Box/Erin Dissertation/SIT sturgeon models/Green Sturgeon Model code/Results/Sensitivity Results/ResponseProfile_SpawnDur.png",
    width = 1800, height = 1200, res = 150)

# Set up the plot
par(mar = c(10, 5, 4, 2))  # Adjust margins as needed

# Order parameters by number of changes
SpawnDur_results <- SpawnDur_results[order(-SpawnDur_results$times), ]

# Create the barplot
bp <- barplot(SpawnDur_results$times,
              names.arg = SpawnDur_results$Parameter,
              col = c("black"),
              border = NA,
              las = 2,
              cex.names = 0.9,
              ylim = c(0, max(SpawnDur_results$times) * 1.2),
              main = "Response Profile Spawning duration",
              ylab = "Number of times top 3 ranks changed",
              xlab = "")



box()
dev.off()


# Extract just the lambda results
lambda_results <- combined_results[combined_results$Attribute == "lambda", ]
lambda_results <- lambda_results[lambda_results[,2] > 0, ] 

# Define colors for each lifestage
lifestage_colors <- c("juvenile" = "blue", "yoy" = "red", "adult" = "gold")
#To add color cordination
param_lifestage_map <- setNames(parm.stages$Lifestage, parm.stages$Parameter)
lambda_results$Lifestage <- param_lifestage_map[lambda_results$Parameter]

bar_colors<-lifestage_colors[lambda_results$Lifestage]


# Create the combined plot
png("C:/Users/gilliger/Box/Erin Dissertation/SIT sturgeon models/Green Sturgeon Model code/Results/Sensitivity Results/ResponseProfile_LambdaCOLOR.png",
    width = 1800, height = 1200, res = 150)

# Set up the plot
par(mar = c(10, 5, 4, 2))  # Adjust margins as needed

# Order parameters by number of changes
lambda <- lambda_results[order(-lambda_results$times), ]

# Create the barplot
bp <- barplot(lambda_results$times,
              names.arg = lambda_results$Parameter,
              col = bar_colors,
              border = NA,
              las = 2,
              cex.names = 0.9,
              ylim = c(0, max(lambda_results$times) * 1.2),
              main = "Response Profile Population Abundance",
              ylab = "Number of times top 3 ranks changed",
              xlab = "")


# Add a legend
legend("topright", 
       legend = names(lifestage_colors),
       fill = lifestage_colors,
       title = "Life Stage",
       cex = 0.8)

box()
dev.off()


# Extract just the large spawners results
river_results <- combined_results[combined_results$Attribute == "num.rivers", ]
river_results <- river_results[river_results[,2] > 0, ] 

# Define colors for each lifestage
lifestage_colors <- c("juvenile" = "blue", "yoy" = "red", "adult" = "gold")
#To add color cordination
param_lifestage_map <- setNames(parm.stages$Lifestage, parm.stages$Parameter)
river_results$Lifestage <- param_lifestage_map[river_results$Parameter]

bar_colors<-lifestage_colors[river_results$Lifestage]


# Create the combined plot
png("C:/Users/gilliger/Box/Erin Dissertation/SIT sturgeon models/Green Sturgeon Model code/Results/Sensitivity Results/ResponseProfile_riverCOLOR.png",
    width = 1800, height = 1200, res = 150)

# Set up the plot
par(mar = c(10, 5, 4, 2))  # Adjust margins as needed

# Order parameters by number of changes
river <- river_results[order(-river_results$times), ]

# Create the barplot
bp <- barplot(river_results$times,
              names.arg = river_results$Parameter,
              col = bar_colors,
              border = NA,
              las = 2,
              cex.names = 0.9,
              ylim = c(0, max(river_results$times) * 1.2),
              main = "Response Profile Number of Rivers",
              ylab = "Number of times top 3 ranks changed",
              xlab = "")


# Add a legend
legend("topright", 
       legend = names(lifestage_colors),
       fill = lifestage_colors,
       title = "Life Stage",
       cex = 0.8)

box()
dev.off()

Utility_results <- influential_changes

# Define colors for each lifestage
lifestage_colors <- c("juvenile" = "blue", "yoy" = "red", "adult" = "gold")
#To add color cordination
param_lifestage_map <- setNames(parm.stages$Lifestage, parm.stages$Parameter)
Utility_results$Lifestage <- param_lifestage_map[Utility_results$Parameter]

bar_colors<-lifestage_colors[Utility_results$Lifestage]


# Create the combined plot
png("C:/Users/gilliger/Box/Erin Dissertation/SIT sturgeon models/Green Sturgeon Model code/Results/Sensitivity Results/ResponseProfile_UtilityCOLOR.png",
    width = 1800, height = 1200, res = 150)

# Set up the plot
par(mar = c(10, 5, 4, 2))  # Adjust margins as needed

# Order parameters by number of changes
Utility <- Utility_results[order(-Utility_results$times), ]

# Create the barplot
bp <- barplot(Utility_results$times,
              names.arg = Utility_results$Parameter,
              col = bar_colors,
              border = NA,
              las = 2,
              cex.names = 0.9,
              ylim = c(0, max(Utility_results$times) * 1.2),
              main = "Response Profile Utility",
              ylab = "Number of times top 3 ranks changed",
              xlab = "")


# Add a legend
legend("topright", 
       legend = names(lifestage_colors),
       fill = lifestage_colors,
       title = "Life Stage",
       cex = 0.8)

box()
dev.off()



# Extract just the recruit results
recruit_results <- combined_results[combined_results$Attribute == "recruit", ]
recruit_results <- recruit_results[recruit_results[,2] > 0, ] 

# Define colors for each lifestage
lifestage_colors <- c("juvenile" = "blue", "yoy" = "red", "adult" = "gold")
#To add color cordination
param_lifestage_map <- setNames(parm.stages$Lifestage, parm.stages$Parameter)
recruit_results$Lifestage <- param_lifestage_map[recruit_results$Parameter]

bar_colors<-lifestage_colors[recruit_results$Lifestage]


# Create the combined plot
png("C:/Users/gilliger/Box/Erin Dissertation/SIT sturgeon models/Green Sturgeon Model code/Results/Sensitivity Results/ResponseProfile_recruitCOLOR.png",
    width = 1800, height = 1200, res = 150)

# Set up the plot
par(mar = c(10, 5, 4, 2))  # Adjust margins as needed

# Order parameters by number of changes
recruit <- recruit_results[order(-recruit_results$times), ]

# Create the barplot
bp <- barplot(recruit_results$times,
              names.arg = recruit_results$Parameter,
              col = c("black"),
              border = NA,
              las = 2,
              cex.names = 0.9,
              ylim = c(0, max(recruit_results$times) * 1.2),
              main = "Response Profile Recruitment",
              ylab = "Number of times top 3 ranks changed",
              xlab = "")



box()
dev.off()



# Extract just the spawner results
spawner_results <- combined_results[combined_results$Attribute == "spawners", ]
spawner_results <- spawner_results[spawner_results[,2] > 0, ] 

# Define colors for each lifestage
lifestage_colors <- c("juvenile" = "blue", "yoy" = "red", "adult" = "gold")
#To add color cordination
param_lifestage_map <- setNames(parm.stages$Lifestage, parm.stages$Parameter)
spawner_results$Lifestage <- param_lifestage_map[spawner_results$Parameter]

bar_colors<-lifestage_colors[spawner_results$Lifestage]


# Create the combined plot
png("C:/Users/gilliger/Box/Erin Dissertation/SIT sturgeon models/Green Sturgeon Model code/Results/Sensitivity Results/ResponseProfile_spawner.png",
    width = 1800, height = 1200, res = 150)

# Set up the plot
par(mar = c(10, 5, 4, 2))  # Adjust margins as needed

# Order parameters by number of changes
spawner <- spawner_results[order(-spawner_results$times), ]

# Create the barplot
bp <- barplot(spawner_results$times,
              names.arg = spawner_results$Parameter,
              col = c("black"),
              border = NA,
              las = 2,
              cex.names = 0.9,
              ylim = c(0, max(spawner_results$times) * 1.2),
              main = "Response Profile Spawners",
              ylab = "Number of times top 3 ranks changed",
              xlab = "")



box()
dev.off()


# Extract just the spawner results
holder_results <- combined_results[combined_results$Attribute == "num.holding", ]
holder_results <- holder_results[holder_results[,2] > 0, ] 

# Define colors for each lifestage
lifestage_colors <- c("juvenile" = "blue", "yoy" = "red", "adult" = "gold")
#To add color cordination
param_lifestage_map <- setNames(parm.stages$Lifestage, parm.stages$Parameter)
holder_results$Lifestage <- param_lifestage_map[holder_results$Parameter]

bar_colors<-lifestage_colors[holder_results$Lifestage]


# Create the combined plot
png("C:/Users/gilliger/Box/Erin Dissertation/SIT sturgeon models/Green Sturgeon Model code/Results/Sensitivity Results/ResponseProfile_holder.png",
    width = 1800, height = 1200, res = 150)

# Set up the plot
par(mar = c(10, 5, 4, 2))  # Adjust margins as needed

# Order parameters by number of changes
holder <- holder_results[order(-holder_results$times), ]

# Create the barplot
bp <- barplot(holder_results$times,
              names.arg = holder_results$Parameter,
              col = c("black"),
              border = NA,
              las = 2,
              cex.names = 0.9,
              ylim = c(0, max(holder_results$times) * 1.2),
              main = "Response Profile Holders",
              ylab = "Number of times top 3 ranks changed",
              xlab = "")



box()
dev.off()


# Extract just the spawner results
rearDur_results <- combined_results[combined_results$Attribute == "avg_rearDur", ]
rearDur_results <- rearDur_results[rearDur_results[,2] > 0, ] 

# Define colors for each lifestage
lifestage_colors <- c("juvenile" = "blue", "yoy" = "red", "adult" = "gold")
#To add color cordination
param_lifestage_map <- setNames(parm.stages$Lifestage, parm.stages$Parameter)
rearDur_results$Lifestage <- param_lifestage_map[rearDur_results$Parameter]

bar_colors<-lifestage_colors[rearDur_results$Lifestage]


# Create the combined plot
png("C:/Users/gilliger/Box/Erin Dissertation/SIT sturgeon models/Green Sturgeon Model code/Results/Sensitivity Results/ResponseProfile_rearDur.png",
    width = 1800, height = 1200, res = 150)

# Set up the plot
par(mar = c(10, 5, 4, 2))  # Adjust margins as needed

# Order parameters by number of changes
rearDur <- rearDur_results[order(-rearDur_results$times), ]

# Create the barplot
bp <- barplot(rearDur_results$times,
              names.arg = rearDur$Parameter,
              col = c("black"),
              border = NA,
              las = 2,
              cex.names = 0.9,
              ylim = c(0, max(rearDur$times) * 1.2),
              main = "Response Profile Rearing Duration",
              ylab = "Number of times top 3 ranks changed",
              xlab = "")



box()
dev.off()
