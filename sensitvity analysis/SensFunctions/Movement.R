#######################################################
#####     Juvenile movement
######################################################
#Proportion of fish for each age class that outmigrate due to a pulse in flow
#proportion_pulse= the proportion of flow that is a pulse, estimates by the standard
#deviation of flow divided by the median flow for each month

# m.flow<-Qmean[16,month,yr]
# pulse_proportion<-prop_pulse[1,month]
# pulse_flow <- m.flow * pulse_proportion
# total_flow <- m.flow + pulse_flow
# outmigration_prob<-min(1,pulse_flow/6000)
#(Q_pulse - Q_base)/Q_base
#((mean(Qmean[18,month,yr]))-(mean(Qmean[18,,yr])))/(mean(Qmean[18,,yr]))

#NOT SURE HOW TO PROPERLY DO PULSE FLOW-DOING THIS FOR NOW....

juv.migrate <- function(flow, trib, parms, incr) {
  params <- list(
    UpSac = list(B0 = -5.9741, B1 = 0.001),    # UpSac parms from Poytress et al. 2024
    Feather = list(B0 = -6.5, B1 = 0.0015),    # Hypothetical Feather River (I want them to leave at lower flows)
    Yuba = list(B0 = -6.2, B1 = 0.0012),       # same for Yuba River
    Bear = list(B0 = -6.8, B1 = 0.002),        # Bear River-Not really important here just in case
    SJ = list(B0 = -5.8, B1 = 0.0008)  # San Joaquin River (higher flows than upsac)
  )
  
  # Update parameters if they are in parms
  if (trib == "UpSac") {
    if ("UpSacjuvmig.int" %in% parms) params$UpSac$B0 <- qnorm(incr[parms == "UpSacjuvmig.int"], params$UpSac$B0, abs(params$UpSac$B0*0.2))
    if ("UpSacjuvmigQ" %in% parms) params$UpSac$B1 <- qnorm(incr[parms == "UpSacjuvmigQ"], params$UpSac$B1, abs(params$UpSac$B1*0.1))
  } else if (trib == "Feather") {
    if ("Feathjuvmig.int" %in% parms) params$Feather$B0 <- qnorm(incr[parms == "Feathjuvmig.int"], params$Feather$B0, abs(params$Feather$B0*0.2))
    if ("FeathjuvmigQ" %in% parms) params$Feather$B1 <- qnorm(incr[parms == "FeathjuvmigQ"], params$Feather$B1, abs(params$Feather$B1*0.1))
  } else if (trib == "Yuba") {
    if ("yubajuvmig.int" %in% parms) params$Yuba$B0 <- qnorm(incr[parms == "yubajuvmig.int"], params$Yuba$B0, abs(params$Yuba$B0*0.2))
    if ("yubajuvmigQ" %in% parms) params$Yuba$B1 <- qnorm(incr[parms == "yubajuvmigQ"], params$Yuba$B1, abs(params$Yuba$B1*0.1))
  } else if (trib == "SJ") {
    if ("SJjuvmig.int" %in% parms) params$SJ$B0 <- qnorm(incr[parms == "SJjuvmig.int"], params$SJ$B0, abs(params$SJ$B0*0.2))
    if ("SJjuvmigQ" %in% parms) params$SJ$B1 <- qnorm(incr[parms == "SJjuvmigQ"], params$SJ$B1, abs(params$SJ$B1*0.1))
  }
  
  # Select the right parms
  B0 <- params[[trib]]$B0
  B1 <- params[[trib]]$B1
  
  # Calculate probability of outmigration
  monthly_prob <- 1 / (1 + exp(-(B0 + B1 * flow)))
  
  # Calculate cumulative probability
  cumulative_prob <- 1 - cumprod(1 - monthly_prob)
  
  # Ensure the last month is exactly 1
  cumulative_prob[length(cumulative_prob)] <- 1
  
  return(cumulative_prob)
}



#juv.migrate(flow=c(100, 200, 300, 400),trib='UpSac',parms=c("UpSacjuvmig.int", "UpSacjuvmigQ"), incr=c(1.1,1.2))


# Main function to handle outmigration for all tributaries
dist.migrators <- function(Qmean, yr, parms, incr) {
  # Define tributaries and their corresponding indices
  tributaries <- list(
    UpSac = list(from = 6, to = c(7, 10), flow_index = 16),
    Feather = list(from = 2, to = c(7, 10), flow_index = 19),
    Yuba = list(from = 3, to = c(7, 10), flow_index = 20),
    Bear = list(from = 4, to = c(7, 10), flow_index = 18),
    SJ = list(from = 5, to = 12, flow_index = 31)
  )
  
  # Initialize a list to store cumulative probabilities for each tributary
  cum_probs <- list()
  
  for (trib_name in names(tributaries)) {
    trib <- tributaries[[trib_name]]
    
    # Extract flows for the entire period
    flows <- c(
      Qmean[trib$flow_index, 10:12, yr],
      Qmean[trib$flow_index, 1:3, yr + 1]
    )
    
    # Calculate cumulative probability for the entire period
    cum_probs[[trib_name]] <- juv.migrate(flows, trib_name, parms=parms, incr=incr)
  }
  
  return(cum_probs)
}

#cumdistrib<-dist.migrators(Qmean, yr)

migrate_and_check_capacity <- function(fish_count, probability, capacity, parms, incr) {
  migrants <- floor(probability * fish_count)
  excess <- max(0, fish_count - capacity)
  forced_migrants <- max(migrants, excess)
  
  remaining <- fish_count - forced_migrants
  
  # Distribute migrants
  cont.mig.prop<-0.62; if(sum(parms == "cont.mig.prop")) cont.mig.prop<-qunif(incr [parms=="cont.mig.prop"], 0.5,0.75) 
  cont_mig <- floor(forced_migrants * cont.mig.prop)
  stepped_mig <- forced_migrants - cont_mig
  
  list(
    remaining = remaining,
    stepped = stepped_mig,
    cont = cont_mig
  )
}

#migrate_and_check_capacity(fish_count=40000,probability=0,capacity=carrying_capacity["Upper_Sac"])

# #######################################################
# #####     Adult outmigration movement
# ######################################################
# # meanQ mean discharge in cubic meters per second for the month
Adult.Early<-function(flow, trib, stochastic, parms, incr){
  params <- list(
    UpSac = list(B0 = -1.62, B1 = 0.0001),  # Upper Sacramento parameters from your original function
    Feather = list(B0 = -1.8, B1 = 0.00012),  # Hypothetical Feather River
    Yuba = list(B0 = -1.7, B1 = 0.00011),     # Hypothetical Yuba River
    Bear = list(B0 = -1.9, B1 = 0.00013),     # Hypothetical Bear River
    SJ = list(B0 = -1.5, B1 = 0.00009)        # Hypothetical San Joaquin River
  )
  
  # Update parameters if they are in parms
  if (trib == "UpSac") {
    if ("UpSacAdultmig.int" %in% parms) params$UpSac$B0 <- qnorm(incr[parms == "UpSacAdultmig.int"], params$UpSac$B0, abs(params$UpSac$B0*0.2))
    if ("UpSacAdultmigQ" %in% parms) params$UpSac$B1 <- qnorm(incr[parms == "UpSacAdultmigQ"], params$UpSac$B1, abs(params$UpSac$B1*0.1))
  } else if (trib == "Feather") {
    if ("FeathAdultmig.int" %in% parms) params$Feather$B0 <- qnorm(incr[parms == "FeathAdultmig.int"], params$Feather$B0, abs(params$Feather$B0*0.2))
    if ("FeathAdultmigQ" %in% parms) params$Feather$B1 <- qnorm(incr[parms == "FeathAdultmigQ"], params$Feather$B1, abs(params$Feather$B1*0.1))
  } else if (trib == "Yuba") {
    if ("yubaAdultmig.int" %in% parms) params$Yuba$B0 <- qnorm(incr[parms == "yubaAdultmig.int"], params$Yuba$B0, abs(params$Yuba$B0*0.2))
    if ("yubaAdultmigQ" %in% parms) params$Yuba$B1 <- qnorm(incr[parms == "yubaAdultmigQ"], params$Yuba$B1, abs(params$Yuba$B1*0.1))
  } else if (trib == "SJ") {
    if ("SJAdultmig.int" %in% parms) params$SJ$B0 <- qnorm(incr[parms == "SJAdultmig.int"], params$SJ$B0, abs(params$SJ$B0*0.2))
    if ("SJAdultmigQ" %in% parms) params$SJ$B1 <- qnorm(incr[parms == "SJAdultmigQ"], params$SJ$B1, abs(params$SJ$B1*0.1))
  }
  
  # Select the right parameters
  B0 <- params[[trib]]$B0
  B1 <- params[[trib]]$B1
  
  # Calculate prop of early mig
  prop.early <- 1 / (1 + exp(-(B0 + B1 * flow)))
  
  if (stochastic) {
    # For stochastic version, return 1 (early) or 0 (late) based on probability
    return(rbinom(1, 1, prop.early))
  } else {
    # For deterministic version, return the probability
    return(prop.early)
  }
}

#Adult.Early(mean(Qmean[1,5:7,1]),trib=c("UpSac"),parms=c("UpSacAdultmig.int", "UpSacAdultmigQ"), incr=c(1.1,1.2))


