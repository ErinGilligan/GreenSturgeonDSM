entrain.sub <- function(fish, prob.entrain, n.div, stochastic) {
  # Pre-allocate vectors for vulnerability values
  vul_values <- c(0.0134, 0.0136, 0.0149, 0.01115, 0.1249, 0.0167, 0.0296, 0.0166)
  vul_sd <- c(0.0028, 0.0029, 0.0035, 0.0021, 0.0318, 0.0039, 0.0034, 0.006)
  
  if (stochastic) {
    vul_values <- rnorm(8, vul_values, vul_sd)
  }
  
  # Use vectorized operations for encounter calculations
  encounter_probs <- vul_values
  fish_counts <- c(fish[1,1], fish[6,1], fish[7,1], fish[10,1],
                   fish[4,1], fish[2,1], fish[3,1], fish[5,1])
  n_div_values <- c(n.div[1], n.div[16], n.div[21], n.div[24],
                    n.div[18], n.div[19], n.div[20], n.div[31])
  
  # Calculate encounters for each fish group separately
  encounters <- vector("list", 8)
  for (i in 1:8) {
    if (!is.na(fish_counts[i]) && fish_counts[i] > 0) {
      encounters[[i]] <- pmin(rbinom(fish_counts[i], size = n_div_values[i], prob = encounter_probs[i]), 20)
    } else {
      encounters[[i]] <- numeric(0)  # Empty numeric vector for zero or NA fish counts
    }
  }
  
  # Additional encounters for lowSac
  lowSacAge1 <- if (!is.na(fish[10,2]) && fish[10,2] > 0) pmin(rbinom(fish[10,2], size = n.div[24], prob = vul_values[4]), 20) else numeric(0)
  lowSacAge2 <- if (!is.na(fish[10,3]) && fish[10,3] > 0) pmin(rbinom(fish[10,3], size = n.div[24], prob = vul_values[4]), 20) else numeric(0)
  
  # Combine all encounters
  all_encounters <- c(encounters, list(lowSacAge1), list(lowSacAge2))
  
  # Create encounter data frames
  encounter_dfs <- lapply(all_encounters, function(x) {
    if (length(x) == 0) {
      df <- data.frame(Entrainment.encounters = 0, Freq = 0)
    } else {
      df <- as.data.frame(table(x))
      colnames(df) <- c("Entrainment.encounters", "Freq")
    }
    df$Entrainment.encounters <- as.numeric(as.character(df$Entrainment.encounters))
    merged_df <- merge(df, prob.entrain, by = "Entrainment.encounters", all.x = TRUE)
    merged_df$Diverting.velocity.0.28.m3.s[is.na(merged_df$Diverting.velocity.0.28.m3.s)] <- 0  # Handle NA probabilities
    merged_df
  })
  
  # Calculate entrainment
  entrainment <- lapply(encounter_dfs, function(df) {
    if (nrow(df) == 0) {
      return(0)
    }
    if (stochastic) {
      sum(rbinom(nrow(df), df$Freq, df$Diverting.velocity.0.28.m3.s))
    } else {
      sum(round(df$Freq * df$Diverting.velocity.0.28.m3.s))
    }
  })
  
  # Combine results
  age0entrained <- unlist(entrainment[1:8])
  age0entrained <- c(age0entrained[1:7], 0, 0, age0entrained[8])
  
  return(list(age0entrained, entrainment[[9]], entrainment[[10]]))
}


#entrain.sub(fish=st.fish[,1:3],prob.entrain=entrain[1:2],n.div=diversions,stochastic=0)
#######################################################
#####     Delta entrainment submodel for green sturgeon
######################################################
####Delta####
delta.entrain <- function(fish, prob.entrain, n.div, stochastic) {
  entrain.vul <- 0.001
  
  # Prepare prob.entrain for faster lookups
  prob.entrain_vector <- prob.entrain[[2]]  # Extract the probability column
  names(prob.entrain_vector) <- prob.entrain[[1]]  # Name it with encounter numbers
  
  # Vectorize rbinom calls and cap at 20
  encounters <- lapply(fish, function(n) pmin(rbinom(n = n, size = n.div, prob = entrain.vul), 20))
  
  # Function to process encounters and calculate entrainment
  process_and_calculate <- function(enc) {
    if (length(enc) == 0) return(0)
    
    enc_table <- table(enc)
    probs <- prob.entrain_vector[names(enc_table)]
    
    if (stochastic) {
      sum(mapply(rbinom, enc_table, 1, probs))
    } else {
      sum(round(as.numeric(enc_table) * probs))
    }
  }
  
  # Apply process_and_calculate to all age groups
  Delta.entrained <- vapply(encounters, process_and_calculate, numeric(1))
  
  return(Delta.entrained)
}

#delta.entrain(fish=st.fish[12,1:3],prob.entrain=entrain[1:2],n.div=dltadivs[2],stochastic=0)
