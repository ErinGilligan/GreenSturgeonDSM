#######################################################
##Green Sturgeon Optimization with $10M Annual Budget
#######################################################
# flow pulses limited to 1 per river per year
# screening limited to 25 diversions per unit of effort
# temp is on a monthly budget so can do multiple
source("GSmod3.R")

library(Rsolnp)
library(parallel)

# Define action costs by water year type (millions)
get_action_costs <- function(water_year_type) {
  
  # Initialize cost vector for all possible actions
  action_costs <- rep(0, 2000)  # Large enough to cover all action numbers
  
  # Flow pulse costs (can only do 1 per river per year)
  if(water_year_type == "W") {
    # Feather flow pulses (1001-1004) - only 1 allowed per year
    action_costs[1001:1004] <- 7.0
    # Yuba flow pulses (1101-1104) - only 1 allowed per year  
    action_costs[1101:1104] <- 3.5
    # San Joaquin flow pulses (1201-1204) - only 1 allowed per year
    action_costs[1201:1204] <- 5.6
    
    # Temperature regulation costs (monthly, can do multiple)
    action_costs[403:406] <- 3.7 #Upsac incubation
    action_costs[503:510] <- 3.7 #UpSac rearing
    action_costs[603:610] <- 3.7  # UpMid Sac rearing
    action_costs[203:206] <- 1.9 #Feather incubation
    action_costs[303:310] <- 1.9 #Feather rearing
    action_costs[703:706] <- 1.2 # Yuba incubation
    action_costs[803:810] <- 1.2 # Yuba rearing
    action_costs[903:910] <- 2.2 #SJ spawn and rear (bring temp down)
    
  } else if(water_year_type == "AN") {
    # Flow pulses
    action_costs[1001:1004] <- 6.3 #Feather
    action_costs[1101:1104] <- 3.15 #Yuba
    action_costs[1201:1204] <- 5.25 #SJ
    
    # Temperature regulation
    action_costs[403:406] <- 6.5 #Upsac incubation
    action_costs[503:510] <- 6.5 #UpSac rearing
    action_costs[603:610] <- 6.5 #UpMid Sac rearing
    action_costs[203:206] <- 2.6 #Feather incubation
    action_costs[303:310] <- 2.6 #Feather rearing
    action_costs[703:706] <- 2.0 # Yuba incubation
    action_costs[803:810] <- 2.0 # Yuba rearing
    action_costs[903:910] <- 3.7 #SJ
    
  } else if(water_year_type == "D") {
    # Flow pulses
    action_costs[1001:1004] <- 6.3 #Feather
    action_costs[1101:1104] <- 3.36 #Yuba
    action_costs[1201:1204] <- 5.04 #SJ
    
    # Temperature regulation (some not feasible)
    # Sacramento - out of budget/not enough water (set high cost)
    action_costs[403:406] <- 16
    action_costs[503:510] <- 16
    action_costs[603:610] <- 16
    # Feather - out of budget
    action_costs[203:206] <- 13
    action_costs[303:310] <- 13
    # Yuba - expensive but feasible
    action_costs[703:706] <- 6.6
    action_costs[803:810] <- 6.6
    # SJ - expensive but feasible
    action_costs[903:910] <- 9.6
    
  } else if(water_year_type == "C") {
    # Flow pulses
    #Kinda cheap because there's no water to use so end up kinda useless
    action_costs[1001:1004] <- 4.2
    action_costs[1101:1104] <- 2.52
    action_costs[1201:1204] <- 3.36
    
    # Temperature regulation (most not feasible)
    #Too expensive plus the water is probably already warm enough
    action_costs[403:406] <- 65
    action_costs[503:510] <- 65
    action_costs[603:610] <- 65
    action_costs[203:206] <- 26
    action_costs[303:310] <- 26
    action_costs[703:706] <- 13.2
    action_costs[803:810] <- 13.2
    action_costs[903:910] <- 38.4
  }
  
  # Fixed costs (same for all water years)
  action_costs[1301:1309] <- 1.6  # Screen 25 diversions (5 per action)
  action_costs[c(17, 28, 29, 30, 33, 34, 35, 36)] <- 1.0  # Habitat (2 acres)
  
  return(action_costs)
}

# Constraint function-basically i'm just checking if a proposed solution violates any 
#constraints: budget,flow pulse
constraint_function <- function(x, water_year_type, action_costs, budget = 10.0) {
  
  constraints <- numeric(0)
  
  # Budget constraint
  total_cost <- sum(x * action_costs)
  constraints <- c(constraints, total_cost - budget)
  
  # Flow pulse constraints: max 1 per river system per year
  # Feather: actions 1001-1004 (sum <= 1)
  feather_flow_sum <- sum(x[1001:1004])
  constraints <- c(constraints, feather_flow_sum - 1)
  
  # Yuba: actions 1101-1104 (sum <= 1)
  yuba_flow_sum <- sum(x[1101:1104])
  constraints <- c(constraints, yuba_flow_sum - 1)
  
  # San Joaquin: actions 1201-1204 (sum <= 1)
  sj_flow_sum <- sum(x[1201:1204])
  constraints <- c(constraints, sj_flow_sum - 1)
  
  # Diversion screening constraint: max 5 actions (= 25 diversions)
  diversion_sum <- sum(x[1301:1309])
  constraints <- c(constraints, diversion_sum - 5)
  
  return(constraints)
}

# Objective function
objective_function <- function(x, water_year_type, years = 20, objective = "population") {
  
  # Convert binary vector to decision list
  selected_actions <- which(x == 1)
  
  # Create decision matrix for all years with this water year type
  decis.X <- vector("list", years)
  for(yr in 1:years) {
    decis.X[[yr]] <- selected_actions
  }
  
  # Run the model
  tryCatch({
    result <- run.scenarios(decis.X = decis.X, runz = 1, years = years, stochastic = 0)
    
    # Extract objective value (negative for minimization)
    if(objective == "population") {
      return(-mean(result[, 1]))
    } else if(objective == "recruitment") {
      return(-mean(result[, 2]))
    } else if(objective == "spawners") {
      return(-mean(result[, 3]))
    } else if(objective == "diversity") {
      return(-mean(result[, 10]))
    }
  }, error = function(e) {
    return(1e6)  # Large penalty for failed runs
  })
}

# Optimization function for a single water year type
optimize_single_water_year <- function(water_year_type, 
                                       objective = "population",
                                       budget = 10.0,
                                       years = 20) {
  
  cat("Optimizing for water year type:", water_year_type, "\n")
  
  # Get costs for this water year type
  action_costs <- get_action_costs(water_year_type)
  
  # Define action space (all possible actions)
  max_action <- 2000  # Match the action_costs vector length
  
  # Initial solution (start with no actions)
  initial_x <- rep(0, max_action)
  
  # Try a few promising starting points
  starting_points <- list(
    rep(0, max_action),  # No actions
    {x <- rep(0, max_action); x[c(1301:1305)] <- 1; x},  # Just diversions
    {x <- rep(0, max_action); x[c(17, 28, 33, 34)] <- 1; x}  # Just habitat
  )
  
  best_result <- NULL
  best_value <- Inf
  
  for(start_idx in 1:length(starting_points)) {
    initial_x <- starting_points[[start_idx]]
    
    # Ensure initial solution meets budget
    while(sum(initial_x * action_costs) > budget) {
      selected <- which(initial_x == 1)
      if(length(selected) == 0) break
      initial_x[sample(selected, 1)] <- 0
    }
    
    # Objective wrapper
    obj_wrapper <- function(x) {
      objective_function(x, water_year_type, years, objective)
    }
    
    # Constraint wrapper
    ineq_wrapper <- function(x) {
      constraint_function(x, water_year_type, action_costs, budget)
    }
    
    # Run optimization
    result <- tryCatch({
      solnp(pars = initial_x,
            fun = obj_wrapper,
            ineqfun = ineq_wrapper,
            ineqLB = rep(-Inf, 5),  # 5 constraints (budget + 4 flow constraints)
            ineqUB = rep(0, 5),     # All must be <= 0
            LB = rep(0, max_action),
            UB = rep(1, max_action),
            control = list(trace = 0, maxit = 500))
    }, error = function(e) {
      cat("Optimization failed for start", start_idx, ":", e$message, "\n")
      return(NULL)
    })
    
    if(!is.null(result) && result$values[length(result$values)] < best_value) {
      best_result <- result
      best_value <- result$values[length(result$values)]
    }
  }
  
  if(!is.null(best_result)) {
    # Round to binary solutions
    optimal_actions_binary <- round(best_result$pars)
    selected_actions <- which(optimal_actions_binary == 1)
    
    # Calculate actual cost
    actual_cost <- sum(optimal_actions_binary * action_costs)
    
    return(list(
      water_year = water_year_type,
      optimal_actions_binary = optimal_actions_binary,
      selected_actions = selected_actions,
      objective_value = -best_result$values[length(best_result$values)],
      convergence = best_result$convergence,
      actual_cost = actual_cost,
      action_costs = action_costs
    ))
  } else {
    cat("All optimization attempts failed for", water_year_type, "\n")
    return(NULL)
  }
}

# Function to optimize all water year types
optimize_all_water_years <- function(objective = "population",
                                     budget = 10.0,
                                     years = 20) {
  
  water_years <- c("W", "AN", "D", "C")
  results <- list()
  
  for(wy in water_years) {
    cat("\n", paste(rep("=", 50), collapse=""), "\n")
    result <- optimize_single_water_year(wy, objective, budget, years)
    results[[wy]] <- result
  }
  
  return(results)
}

# Function to create adaptive strategy based on actual water year sequence
create_adaptive_strategy <- function(optimization_results, water_year_sequence) {
  
  years <- length(water_year_sequence)
  adaptive_strategy <- vector("list", years)
  
  for(yr in 1:years) {
    wy_type <- water_year_sequence[yr]
    
    if(!is.null(optimization_results[[wy_type]])) {
      adaptive_strategy[[yr]] <- optimization_results[[wy_type]]$selected_actions
    } else {
      adaptive_strategy[[yr]] <- numeric(0)  # No actions if optimization failed
    }
  }
  
  return(adaptive_strategy)
}

# Summary function
summarize_optimization_results <- function(results) {
  
  summary_list <- list()
  
  for(wy in names(results)) {
    result <- results[[wy]]
    
    if(!is.null(result)) {
      
      # Decode selected actions
      selected <- result$selected_actions
      
      flow_actions <- selected[selected >= 1001 & selected <= 1204]
      temp_actions <- selected[selected >= 203 & selected <= 910]
      diversion_actions <- selected[selected >= 1301 & selected <= 1309]
      habitat_actions <- selected[selected %in% c(17, 28, 29, 30, 33, 34, 35, 36)]
      
      summary_list[[wy]] <- list(
        water_year = wy,
        objective_value = result$objective_value,
        total_cost = result$actual_cost,
        convergence = result$convergence,
        num_flow_actions = length(flow_actions),
        num_temp_actions = length(temp_actions),
        num_diversion_actions = length(diversion_actions),
        num_habitat_actions = length(habitat_actions),
        flow_actions = flow_actions,
        temp_actions = temp_actions,
        diversion_actions = diversion_actions,
        habitat_actions = habitat_actions
      )
    }
  }
  
  return(summary_list)
}

# Example usage
cat("Green Sturgeon $10M Budget Optimization Ready!\n")
cat("================================================\n\n")
cat("Key functions:\n")
cat("1. optimize_single_water_year(water_year_type, objective, budget)\n")
cat("2. optimize_all_water_years(objective, budget)\n")
cat("3. create_adaptive_strategy(results, water_year_sequence)\n")
cat("4. summarize_optimization_results(results)\n\n")
cat("Example usage:\n")
cat('results <- optimize_all_water_years("population", 10.0)\n')
cat("summary <- summarize_optimization_results(results)\n")
cat("print(summary)\n\n")

# Optimize for all water year types
results <- optimize_all_water_years("population", 10.0)

# See what actions are selected for each water year
summary <- summarize_optimization_results(results)
print(summary)

# Create strategy for actual 20-year sequence
water_years <-  WY["Sacramento_WYT", 1:20]  # Your actual data
adaptive_strategy <- create_adaptive_strategy(results, water_years)

saveRDS(summary,file="OptimizeResults.rds")
trying<-readRDS("OptimizeResults.rds")




##########
# Test all water year types with the fixed flow code
cat("=== COMPREHENSIVE OPTIMIZATION RESULTS ===\n\n")

wet_result <- optimize_manual("W", 10.0, 5)
cat("WET YEAR:\n")
cat("  Best actions:", paste(wet_result$best_actions, collapse=", "), "\n")
cat("  Population:", round(wet_result$best_value), "\n")
cat("  Cost: $", wet_result$best_cost, "M\n\n")

an_result <- optimize_manual("AN", 10.0, 5) 
cat("ABOVE NORMAL YEAR:\n")
cat("  Best actions:", paste(an_result$best_actions, collapse=", "), "\n")
cat("  Population:", round(an_result$best_value), "\n")
cat("  Cost: $", an_result$best_cost, "M\n\n")

dry_result <- optimize_manual("D", 10.0, 5)
cat("DRY YEAR:\n")
cat("  Best actions:", paste(dry_result$best_actions, collapse=", "), "\n")
cat("  Population:", round(dry_result$best_value), "\n")
cat("  Cost: $", dry_result$best_cost, "M\n\n")

crit_result <- optimize_manual("C", 10.0, 5)
cat("CRITICAL YEAR:\n")
cat("  Best actions:", paste(crit_result$best_actions, collapse=", "), "\n")
cat("  Population:", round(crit_result$best_value), "\n")
cat("  Cost: $", crit_result$best_cost, "M\n\n")




# Debug the temperature costs
action_costs_W <- get_action_costs_simple("W")

cat("=== TEMPERATURE COST ANALYSIS (WET YEAR) ===\n")
cat("Feather spawning temp (Mar-Jun):", sum(action_costs_W[203:206]), "M\n")
cat("Feather rearing temp (Mar-Oct):", sum(action_costs_W[303:310]), "M\n") 
cat("Yuba spawning temp (Mar-Jun):", sum(action_costs_W[703:706]), "M\n")
cat("Yuba rearing temp (Mar-Oct):", sum(action_costs_W[803:810]), "M\n")
cat("SJ temp (Mar-Oct):", sum(action_costs_W[903:910]), "M\n")
cat("Budget available:", 10.0, "M\n\n")

# Test cheaper temperature combinations
cat("=== TESTING AFFORDABLE TEMPERATURE STRATEGIES ===\n")

# Candidate: Feather temp for just 2 months + diversions
if(sum(action_costs_W[c(203, 204, 1301, 1302, 1303, 1304, 1305)]) <= 10.0) {
  test_actions <- c(203, 204, 1301, 1302, 1303, 1304, 1305)
  test_cost <- sum(test_actions * (action_costs_W * (1:2000 %in% test_actions)))
  test_value <- objective_function_simple(test_actions, "W", 5)
  cat("Feather 2-month + diversions: cost =", test_cost, ", value =", round(test_value), "\n")
}

# Candidate: Yuba temp for 2 months + diversions  
if(sum(action_costs_W[c(703, 704, 1301, 1302, 1303, 1304, 1305)]) <= 10.0) {
  test_actions <- c(703, 704, 1301, 1302, 1303, 1304, 1305)
  test_cost <- sum(test_actions * (action_costs_W * (1:2000 %in% test_actions)))
  test_value <- objective_function_simple(test_actions, "W", 5)
  cat("Yuba 2-month + diversions: cost =", test_cost, ", value =", round(test_value), "\n")
}

# Candidate: Just Feather temp for 4 months (no diversions)
test_actions2 <- c(203, 204, 205, 206)
test_cost2 <- sum(action_costs_W[test_actions2])
if(test_cost2 <= 10.0) {
  test_value2 <- objective_function_simple(test_actions2, "W", 5)
  cat("Feather 4-month temp only: cost =", test_cost2, ", value =", round(test_value2), "\n")
}

# Candidate: Just Yuba temp for 4 months (no diversions)
test_actions3 <- c(703, 704, 705, 706) 
test_cost3 <- sum(action_costs_W[test_actions3])
if(test_cost3 <= 10.0) {
  test_value3 <- objective_function_simple(test_actions3, "W", 5)
  cat("Yuba 4-month temp only: cost =", test_cost3, ", value =", round(test_value3), "\n")
}

# Test hybrid strategies that might beat diversions
cat("=== TESTING HYBRID STRATEGIES ===\n")

# Test: Feather temp + reduced diversions
test_actions4 <- c(203, 204, 205, 206, 1301, 1302)  # Feather 4-month + 2 diversions
test_cost4 <- sum(action_costs_W[test_actions4])
if(test_cost4 <= 10.0) {
  test_value4 <- objective_function_simple(test_actions4, "W", 5)
  cat("Feather 4-month + 2 diversions: cost =", test_cost4, ", value =", round(test_value4), "\n")
}

# Test: Yuba temp + more diversions
test_actions5 <- c(703, 704, 705, 706, 1301, 1302, 1303)  # Yuba 4-month + 3 diversions  
test_cost5 <- sum(action_costs_W[test_actions5])
if(test_cost5 <= 10.0) {
  test_value5 <- objective_function_simple(test_actions5, "W", 5)
  cat("Yuba 4-month + 3 diversions: cost =", test_cost5, ", value =", round(test_value5), "\n")
}

# Test: Both Feather + Yuba temp (no diversions)
test_actions6 <- c(203, 204, 205, 206, 703, 704, 705, 706)
test_cost6 <- sum(action_costs_W[test_actions6])
if(test_cost6 <= 10.0) {
  test_value6 <- objective_function_simple(test_actions6, "W", 5)
  cat("Feather + Yuba temp (no diversions): cost =", test_cost6, ", value =", round(test_value6), "\n")
}

# Test: Feather temp + habitat
test_actions7 <- c(203, 204, 205, 206, 17, 28)
test_cost7 <- sum(action_costs_W[test_actions7])
if(test_cost7 <= 10.0) {
  test_value7 <- objective_function_simple(test_actions7, "W", 5)
  cat("Feather 4-month + habitat: cost =", test_cost7, ", value =", round(test_value7), "\n")
}
