#######################################################
#####     entrainment submodel for green sturgeon
######################################################
#Mussen et al. 2014 online figure reader extraction prob of entrainment based on number
#of pipe encounters and the diverting velocity in m3/s with a sweeping
#velocity of 0.15 m/s (no stat diff b/ween sweeping velocities but increased
#entrainment at low sweep velocity and high diversion rates)


entrain.deterministic <- function(fish, prob.entrain, n.div) {
  fish[fish < 0] <- 0  # Ensure no negative fish counts
  
  # Fixed vulnerability parameters
  upsacvul <- 0.0134
  upMidvul <- 0.0136
  lowMidvul <- 0.0149
  lowSacvul <- 0.01115
  Bearvul <- 0.1249
  Feathervul <- 0.0167
  Yubavul <- 0.0296
  SJvul <- 0.0166
  
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

entrain.stochastic<-function(fish,prob.entrain,n.div){
  fish[fish < 0] <- 0  # Ensure no negative fish counts

    upsacvul<-rnorm(1,0.0134,0.0028)
    upMidvul<-rnorm(1,0.0136,0.0029) 
    lowMidvul<-rnorm(1,0.0149,0.0035)
    lowSacvul<-rnorm(1,0.01115,0.0021)
    Bearvul<-rnorm(1,0.1249,0.0318)
    Feathervul<-rnorm(1,0.0167, 0.0039)
    Yubavul<-rnorm(1,0.0296, 0.0034)
    SJvul<-rnorm(1,0.0166, 0.006)
 
  Upsac.encounter <- if(is.na(fish[1,1]) || fish[1,1] < 0) {
    0
  } else {
    pmin(rbinom(n=fish[1,1], size=n.div[1], prob=upsacvul), 20)
  }
  
  UpMid.encounter <- if(is.na(fish[6,1]) || fish[6,1] < 0) {
    0
  } else {
    pmin(rbinom(n=fish[6,1], size=n.div[16], prob=upMidvul), 20)
  }
  
  lowMid.encounter <- if(is.na(fish[7,1]) || fish[7,1] < 0) {
    0
  } else {
    pmin(rbinom(n=fish[7,1], size=n.div[21], prob=lowMidvul), 20)
  }
  
  lowSac.encounter <- if(is.na(fish[10,1]) || fish[10,1] < 0) {
    0
  } else {
    pmin(rbinom(n=fish[10,1], size=n.div[24], prob=lowSacvul), 20)
  }
  
  Bear.encounter <- if(is.na(fish[4,1]) || fish[4,1] < 0) {
    0
  } else {
    pmin(rbinom(n=fish[4,1], size=n.div[18], prob=Bearvul), 20)
  }
  
  Feather.encounter <- if(is.na(fish[2,1]) || fish[2,1] < 0) {
    0
  } else {
    pmin(rbinom(n=fish[2,1], size=n.div[19], prob=Feathervul), 20)
  }
  
  Yuba.encounter <- if(is.na(fish[3,1]) || fish[3,1] < 0) {
    0
  } else {
    pmin(rbinom(n=fish[3,1], size=n.div[20], prob=Yubavul), 20)
  }
  
  SJ.encounter <- if(is.na(fish[5,1]) || fish[5,1] < 0) {
    0
  } else {
    pmin(rbinom(n=fish[5,1], size=n.div[31], prob=SJvul), 20)
  }
  
  lowSacAge1 <- if(is.na(fish[10,2]) || fish[10,2] < 0) {
    0
  } else {
    pmin(rbinom(n=fish[10,2], size=n.div[24], prob=lowSacvul), 20)
  }
  
  lowSacAge2 <- if(is.na(fish[10,3]) || fish[10,3] < 0) {
    0
  } else {
    pmin(rbinom(n=fish[10,3], size=n.div[24], prob=lowSacvul), 20)
  }
  #upsac
  if (length(Upsac.encounter)==0){
    Upsac.encounter<-data.frame(Entrainment.encounters=c(0),Freq=c(0))}else{
      Upsac.encounter<-data.frame(table(Upsac.encounter))
      colnames(Upsac.encounter)<-c("Entrainment.encounters","Freq")}
  Upsac.encounter<-merge(Upsac.encounter,prob.entrain, by= "Entrainment.encounters")
  
  #upmid
  if (length(UpMid.encounter)==0){
    UpMid.encounter<-data.frame(Entrainment.encounters=c(0),Freq=c(0))}else{
      UpMid.encounter<-data.frame(table(UpMid.encounter))
      colnames(UpMid.encounter)<-c("Entrainment.encounters","Freq")}
  UpMid.encounter<-merge(UpMid.encounter,prob.entrain, by= "Entrainment.encounters")
  
  #lowmid
  if (length(lowMid.encounter)==0){
    lowMid.encounter<-data.frame(Entrainment.encounters=c(0),Freq=c(0))}else{
      lowMid.encounter<-data.frame(table(lowMid.encounter))
      colnames(lowMid.encounter)<-c("Entrainment.encounters","Freq")}
  lowMid.encounter<-merge(lowMid.encounter,prob.entrain, by= "Entrainment.encounters")
  
  #lowSac
  if (length(lowSac.encounter)==0){
    lowSac.encounter<-data.frame(Entrainment.encounters=c(0),Freq=c(0))}else{
      lowSac.encounter<-data.frame(table(lowSac.encounter))
      colnames(lowSac.encounter)<-c("Entrainment.encounters","Freq")}
  lowSac.encounter<-merge(lowSac.encounter,prob.entrain, by= "Entrainment.encounters")
  
  #Bear
  if (length(Bear.encounter)==0){
    Bear.encounter<-data.frame(Entrainment.encounters=c(0),Freq=c(0))}else{
      Bear.encounter<-data.frame(table(Bear.encounter))
      colnames(Bear.encounter)<-c("Entrainment.encounters","Freq")}
  Bear.encounter<-merge(Bear.encounter,prob.entrain, by= "Entrainment.encounters")
  
  #Feather
  if (length(Feather.encounter)==0){
    Feather.encounter<-data.frame(Entrainment.encounters=c(0),Freq=c(0))}else{
      Feather.encounter<-data.frame(table(Feather.encounter))
      colnames(Feather.encounter)<-c("Entrainment.encounters","Freq")}
  Feather.encounter<-merge(Feather.encounter,prob.entrain, by= "Entrainment.encounters")
  
  #Yuba
  if (length(Yuba.encounter)==0){
    Yuba.encounter<-data.frame(Entrainment.encounters=c(0),Freq=c(0))}else{
      Yuba.encounter<-data.frame(table(Yuba.encounter))
      colnames(Yuba.encounter)<-c("Entrainment.encounters","Freq")}
  Yuba.encounter<-merge(Yuba.encounter,prob.entrain, by= "Entrainment.encounters")
  
  #SJ
  if (length(SJ.encounter)==0){
    SJ.encounter<-data.frame(Entrainment.encounters=c(0),Freq=c(0))}else{
      SJ.encounter<-data.frame(table(SJ.encounter))
      colnames(SJ.encounter)<-c("Entrainment.encounters","Freq")}
  SJ.encounter<-merge(SJ.encounter,prob.entrain, by= "Entrainment.encounters")
  
  #lowSac Age 1
  if (length(lowSacAge1)==0){
    lowSacAge1<-data.frame(Entrainment.encounters=c(0),Freq=c(0))}else{
      lowSacAge1<-data.frame(table(lowSacAge1))
      colnames(lowSacAge1)<-c("Entrainment.encounters","Freq")}
  lowSacAge1<-merge(lowSacAge1,prob.entrain, by= "Entrainment.encounters")
  
  #lowSac Age 2
  if (length(lowSacAge2)==0){
    lowSacAge2<-data.frame(Entrainment.encounters=c(0),Freq=c(0))}else{
      lowSacAge2<-data.frame(table(lowSacAge2))
      colnames(lowSacAge2)<-c("Entrainment.encounters","Freq")}
  lowSacAge2<-merge(lowSacAge2,prob.entrain, by= "Entrainment.encounters")
  
  UpSac.entrain<-c()
  for(i in 1:nrow(Upsac.encounter)){
    UpSac.entrain[i]<-rbinom(1,Upsac.encounter[i,2],Upsac.encounter[i,3])}
  
  Feather.entrain<-c()
  for(i in 1:nrow(Feather.encounter)){
    Feather.entrain[i]<-rbinom(1,Feather.encounter[i,2],Feather.encounter[i,3])}
  
  Yuba.entrain<-c()
  for(i in 1:nrow(Yuba.encounter)){
    Yuba.entrain[i]<-rbinom(1,Yuba.encounter[i,2],Yuba.encounter[i,3])}
  
  Bear.entrain<-c()
  for(i in 1:nrow(Bear.encounter)){
    Bear.entrain[i]<-rbinom(1,Bear.encounter[i,2],Bear.encounter[i,3])}
  
  SJ.entrain<-c()
  for(i in 1:nrow(SJ.encounter)){
    SJ.entrain[i]<-rbinom(1,SJ.encounter[i,2],SJ.encounter[i,3])}
  
  UpMid.entrain<-c()
  for(i in 1:nrow(UpMid.encounter)){
    UpMid.entrain[i]<-rbinom(1,UpMid.encounter[i,2],UpMid.encounter[i,3])}
  
  lowMid.entrain<-c()
  for(i in 1:nrow(lowMid.encounter)){
    lowMid.entrain[i]<-rbinom(1,lowMid.encounter[i,2],lowMid.encounter[i,3])}
  
  lowSac.entrain<-c()
  for(i in 1:nrow(lowSac.encounter)){
    lowSac.entrain[i]<-rbinom(1,lowSac.encounter[i,2],lowSac.encounter[i,3])}
  
  #now do age 1 and 2 fish in the lower sac
  age1entrain<-c()
  for(i in 1:nrow(lowSacAge1)){
    age1entrain[i]<-rbinom(1,lowSacAge1[i,2],lowSacAge1[i,3])}
  
  age2entrain<-c()
  for(i in 1:nrow(lowSacAge2)){
    age2entrain[i]<-rbinom(1,lowSacAge2[i,2],lowSacAge2[i,3])}
  
  
  #zeros are the bypasses to make easy addition to base mod 
  age0entrained<-c(sum(UpSac.entrain),sum(Feather.entrain),sum(Yuba.entrain),
                   sum(Bear.entrain),sum(SJ.entrain),sum(UpMid.entrain),
                   sum(lowMid.entrain),0,0,sum(lowSac.entrain)) 
  
  
  return(list(age0entrained,sum(age1entrain),sum(age2entrain)))
  
}


#entrain.stochastic(fish=st.fish[,1:3],prob.entrain=entrain[1:2],n.div=diversions)


#######################################################
#####     Delta entrainment submodel for green sturgeon
######################################################

delta.entrain.deterministic <- function(fish, prob.entrain, n.div) {
  # Ensure no negative fish counts
  fish[fish < 0] <- 0
  
  # Fixed vulnerability parameter (same as in original delta.entrain)
  entrain.vul <- 0.001
  
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

#Stochastic version
delta.entrain.stoch<-function(fish,prob.entrain,n.div){
  
  entrain.vul<-0.001
  
  encounterage0 <- if(is.na(fish[1]) || fish[1] < 0) {
    0
  } else {
    pmin(rbinom(n=fish[1], size=n.div, prob=entrain.vul), 20)
  }
  
  encounterage1 <- if(is.na(fish[2]) || fish[2] < 0) {
    0
  } else {
    pmin(rbinom(n=fish[2], size=n.div, prob=entrain.vul), 20)
  }
  
  encounterage2 <- if(is.na(fish[3]) || fish[3] < 0) {
    0
  } else {
    pmin(rbinom(n=fish[3], size=n.div, prob=entrain.vul), 20)
  }
  
  if (length(encounterage0)==0){
    encounterage0<-data.frame(Entrainment.encounters=c(0),Freq=c(0))}else{
      encounterage0<-data.frame(table(encounterage0))
      colnames(encounterage0)<-c("Entrainment.encounters","Freq")}
  encounterage0<-merge(encounterage0,prob.entrain, by= "Entrainment.encounters")
  
  if (length(encounterage1)==0){
    encounterage1<-data.frame(Entrainment.encounters=c(0),Freq=c(0))}else{
      encounterage1<-data.frame(table(encounterage1))
      colnames(encounterage1)<-c("Entrainment.encounters","Freq")}
  encounterage1<-merge(encounterage1,prob.entrain, by= "Entrainment.encounters")
  
  if (length(encounterage2)==0){
    encounterage2<-data.frame(Entrainment.encounters=c(0),Freq=c(0))}else{
      encounterage2<-data.frame(table(encounterage2))
      colnames(encounterage2)<-c("Entrainment.encounters","Freq")}
  encounterage2<-merge(encounterage2,prob.entrain, by= "Entrainment.encounters")
  
  age0.entrain<-c()
  for(i in 1:nrow(encounterage0)){
    age0.entrain[i]<-rbinom(1,encounterage0[i,2],encounterage0[i,3])}
  
  age1.entrain<-c()
  for(i in 1:nrow(encounterage1)){
    age1.entrain[i]<-rbinom(1,encounterage1[i,2],encounterage1[i,3])}
  
  age2.entrain<-c()
  for(i in 1:nrow(encounterage2)){
    age2.entrain[i]<-rbinom(1,encounterage2[i,2],encounterage2[i,3])}
  
  Delta.entrained<-c(sum(age0.entrain),sum(age1.entrain),sum(age2.entrain))
  
  return(Delta.entrained)
  
}
#delta.entrain.stoch(fish=st.fish[12,1:3],prob.entrain=entrain[1:2],n.div=dltadivs[2])
