#######################################################
#####     entrainment submodel for green sturgeon
######################################################
#Mussen et al. 2014 online figure reader extraction prob of entrainment based on number
#of pipe encounters and the diverting velocity in m3/s with a sweeping
#velocity of 0.15 m/s (no stat diff b/ween sweeping velocities but increased
#entrainment at low sweep velocity and high diversion rates)


entrain.deterministic <- function(fish, prob.entrain, n.div, parms,incr) {
  fish[fish < 0] <- 0  # Ensure no negative fish counts
  
  
  upsacvul<-0.0134; if(sum(parms == "entrain.upsac")) upsacvul<-qbeta(incr [parms=="entrain.upsac"], 22.58,1662.697)
  upMidvul<-0.0136; if(sum(parms == "entrain.upMid")) upMidvul<- qbeta(incr [parms=="entrain.upMid"], 21.68, 1572.45)
  lowMidvul<-0.0149; if(sum(parms == "entrain.lowMid")) lowMidvul<- qbeta(incr [parms=="entrain.lowMid"], 17.84, 1179.36)
  lowSacvul<-0.01115; if(sum(parms == "entrain.low")) lowSacvul<- qbeta(incr [parms=="entrain.low"], 27.87, 2471.29)
  Bearvul<-0.1249
  Feathervul<-0.0167; if(sum(parms == "entrain.Feath")) Feathervul<- qbeta(incr [parms=="entrain.Feath"], 18.013, 1060.61)
  Yubavul<-0.0296; if(sum(parms == "entrain.yuba")) Yubavul<- qbeta(incr [parms=="entrain.yuba"], 73.52, 2410.24)
  SJvul<-0.0166; if(sum(parms == "entrain.sj")) SJvul<- qbeta(incr [parms=="entrain.sj"], 7.51, 444.946)
  
  
  # Calculate per-fish encounter probabilities for each diversion pass
  # This is the probability of a single fish encountering a single diversion
  calc_encounter_distribution <- function(fish_count, div_count, vulnerability) {
    if (is.na(fish_count) || fish_count <= 0) {
      return(rep(0, 21))  # Return 0-20 encounters with zero frequency
    }
    
    # Probability of a fish encountering exactly k diversions out of n.div
    # Binomial probability distribution
    encounter_probs <- numeric(21)  # For 0 to 20 encounters
    
    for (k in 0:20) {
      # Cap at div_count (can't encounter more diversions than exist)
      if (k <= div_count) {
        # Calculate binomial probability: fish has exactly k encounters
        encounter_probs[k+1] <- choose(div_count, k) * vulnerability^k * (1-vulnerability)^(div_count-k)
      } else {
        encounter_probs[k+1] <- 0
      }
    }
    
    # Convert probabilities to expected number of fish with each encounter count
    encounter_counts <- fish_count * encounter_probs
    
    return(encounter_counts)
  }
  
  # Calculate encounter distributions for each segment
  upsac_dist <- calc_encounter_distribution(fish[1,1], n.div[1], upsacvul)
  upmid_dist <- calc_encounter_distribution(fish[6,1], n.div[16], upMidvul)
  lowmid_dist <- calc_encounter_distribution(fish[7,1], n.div[21], lowMidvul)
  lowsac_dist <- calc_encounter_distribution(fish[10,1], n.div[24], lowSacvul)
  bear_dist <- calc_encounter_distribution(fish[4,1], n.div[18], Bearvul)
  feather_dist <- calc_encounter_distribution(fish[2,1], n.div[19], Feathervul)
  yuba_dist <- calc_encounter_distribution(fish[3,1], n.div[20], Yubavul)
  sj_dist <- calc_encounter_distribution(fish[5,1], n.div[31], SJvul)
  lowsac_age1_dist <- calc_encounter_distribution(fish[10,2], n.div[24], lowSacvul)
  lowsac_age2_dist <- calc_encounter_distribution(fish[10,3], n.div[24], lowSacvul)
  
  # Calculate entrainment for each encounter count
  calc_entrainment <- function(encounter_dist) {
    total_entrained <- 0
    
    # For each possible number of encounters (0-20)
    for (i in 1:21) {
      encounters <- i - 1  # 0 to 20 encounters
      prob_index <- which(prob.entrain$Entrainment.encounters == encounters)
      
      if (length(prob_index) > 0) {
        entrain_prob <- prob.entrain$Diverting.velocity.0.28.m3.s[prob_index]
        # Expected number of fish with this encounter count * probability of entrainment
        total_entrained <- total_entrained + encounter_dist[i] * entrain_prob
      }
    }
    
    return(total_entrained)
  }
  
  # Calculate entrainment for each segment
  upsac_entrain <- calc_entrainment(upsac_dist)
  upmid_entrain <- calc_entrainment(upmid_dist) 
  lowmid_entrain <- calc_entrainment(lowmid_dist)
  lowsac_entrain <- calc_entrainment(lowsac_dist)
  bear_entrain <- calc_entrainment(bear_dist)
  feather_entrain <- calc_entrainment(feather_dist)
  yuba_entrain <- calc_entrainment(yuba_dist)
  sj_entrain <- calc_entrainment(sj_dist)
  age1_entrain <- calc_entrainment(lowsac_age1_dist)
  age2_entrain <- calc_entrainment(lowsac_age2_dist)
  
  # Format results
  age0_entrained <- c(
    upsac_entrain, feather_entrain, yuba_entrain,
    bear_entrain, sj_entrain, upmid_entrain,
    lowmid_entrain, 0, 0, lowsac_entrain
  )
  
  # as.integer values
  age0_entrained <- floor(age0_entrained)
  age1_entrain <- floor(age1_entrain)
  age2_entrain <- floor(age2_entrain)
  
  return(list(age0_entrained, age1_entrain, age2_entrain))
}

#entrain.deterministic(fish=st.fish[,1:3],prob.entrain=entrain[1:2],n.div=diversions)

######################################################
#####     Delta entrainment submodel for green sturgeon
######################################################

delta.entrain.deterministic <- function(fish, prob.entrain, n.div, parms, incr) {
  # Ensure no negative fish counts
  fish[fish < 0] <- 0
  
  entrain.vul<-0.001; if(sum(parms == "delta.entrain")) entrain.vul<-qbeta(incr [parms=="delta.entrain"], 2.13, 1994.89)
  
  
  # Calculate encounter distribution (deterministic version)
  calc_encounter_distribution <- function(fish_count, div_count, vulnerability) {
    if (is.na(fish_count) || fish_count <= 0) {
      return(rep(0, 21))  # Return 0-20 encounters with zero frequency
    }
    
    # Probability of a fish encountering exactly k diversions out of n.div
    # Binomial probability distribution
    encounter_probs <- numeric(21)  # For 0 to 20 encounters
    
    for (k in 0:20) {
      # Cap at div_count (can't encounter more diversions than exist)
      if (k <= div_count) {
        # Calculate binomial probability: fish has exactly k encounters
        encounter_probs[k+1] <- choose(div_count, k) * vulnerability^k * (1-vulnerability)^(div_count-k)
      } else {
        encounter_probs[k+1] <- 0
      }
    }
    
    # Convert probabilities to expected number of fish with each encounter count
    encounter_counts <- fish_count * encounter_probs
    
    return(encounter_counts)
  }
  
  # Calculate encounter distributions for each age class
  age0_dist <- calc_encounter_distribution(fish[1], n.div, entrain.vul)
  age1_dist <- calc_encounter_distribution(fish[2], n.div, entrain.vul)
  age2_dist <- calc_encounter_distribution(fish[3], n.div, entrain.vul)
  
  # Calculate entrainment for each encounter count
  calc_entrainment <- function(encounter_dist) {
    total_entrained <- 0
    
    # For each possible number of encounters (0-20)
    for (i in 1:21) {
      encounters <- i - 1  # 0 to 20 encounters
      prob_index <- which(prob.entrain$Entrainment.encounters == encounters)
      
      if (length(prob_index) > 0) {
        entrain_prob <- prob.entrain$Diverting.velocity.0.28.m3.s[prob_index]
        # Expected number of fish with this encounter count * probability of entrainment
        total_entrained <- total_entrained + encounter_dist[i] * entrain_prob
      }
    }
    
    return(total_entrained)
  }
  
  # Calculate entrainment for each age class
  age0_entrain <- calc_entrainment(age0_dist)
  age1_entrain <- calc_entrainment(age1_dist)
  age2_entrain <- calc_entrainment(age2_dist)
  
  # Round values
  age0_entrain <- floor(age0_entrain)
  age1_entrain <- floor(age1_entrain)
  age2_entrain <- floor(age2_entrain)
  
  # Return results as a vector
  Delta_entrained <- c(age0_entrain, age1_entrain, age2_entrain)
  
  return(Delta_entrained)
}

#delta.entrain.deterministic(fish=st.fish[12,1:3],prob.entrain=entrain[1:2],n.div=dltadivs[2])
