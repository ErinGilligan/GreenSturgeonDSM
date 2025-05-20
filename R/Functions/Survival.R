#Survival functions used in the Green sturgeon model

#######################################################
#####     Adult Survival
######################################################
#I developed this curve from the CT max of 33.7 (0.09) degree c in 
#Sardella et al. 2008 where we expect 50% mort at 33.7C

Adult.S<- function(sm.spawn,lg.spawn,avgT,stochastic){
  B0 <- -374.407
  B1 <- 11.11
  
  
  Temp.Mort<-1/(1+exp(-(B0 + B1*avgT)))
  temp.S<-1-Temp.Mort
  
  sm.spawners<- if (stochastic){sum(rbinom(sm.spawn,1,temp.S))} else{round(sm.spawn*temp.S)}
  lg.spawners<- if (stochastic){sum(rbinom(lg.spawn,1,temp.S))} else{round(lg.spawn*temp.S)}
  
  
  return(c(sm.spawners,lg.spawners))
}

# Create a function for adult survival
apply_adult_survival <- function(index, temp,curr_month=NA, stochastic = FALSE) {
  st.fish[index, 6:7] <<- Adult.S(st.fish[index, 6], st.fish[index, 7], temp, stochastic)
}

#######################################################
#####     Egg to Larvae Survival
######################################################
#Excluded parms=DO,scour,fertilization,turbid, pred, contaminants
#habit.avail (i would prefer to add this to the spawning function instead of Survival)
#avgT is average monthly temperature
#taken based on figure in Rodgers et al. 2019 explaining optimal,suboptimal temps

Egg.Surv<- function(avgT){ 
  B0 <- -15  # Intercept parameter
  B1 <- 0.65   # Slope parameter
  max_survival <- 0.001  # Maximum survival rate (5%)
  
  # Base survival using logistic function
  Temp.Mort <- 1/(1+exp(-(B0 + B1*avgT)))
  
  # Add additional decline at cold temperatures (below ~10°C)
  cold_dip <- 0.20 * exp(-0.01 * (avgT - 0)^2)
  Temp.Mort <- Temp.Mort + cold_dip
  # Calculate final survival rate
  # #mortality stays between 0 and 1
  Temp.Mort <- pmin(1, pmax(0, Temp.Mort))
  
  temp.S <- max_survival * (1 - Temp.Mort)
  # 
  # #survival stays between 0 and 1
  EggS <- pmin(1, pmax(0, temp.S))
  
  return(EggS)
}

#Egg.Surv(avgT=c(11,5,10,8,26)) 


#######################################################
#####   Larvae to age 0 Survival
######################################################
#Excluded parms= pred, contaminants,habitat complexity,food.avail
#avgT is average monthly temperature
#pred= predation for lengths less than 220 mm (Baird et al. 2019) >220 pred risk=0
#diversions its own submodel
#temp surv taken from Linares-Casenave 2013 (I assumed abnormalities meant mortality
#since predation and other factors would likely kill them)

Larvae.Surv <- function(larvae,avgT,pred,stochastic){
   B0 <- -15.7102
   B1 <- 0.6216
   baseline.M <- 0.8
  
  larvS <- c()
  for (i in 1:5) {
    # Calculate base mortality
    Temp.Mort <- 1 / (1 + exp(-(B0 + B1 * avgT[i])))
    
    # Add a Gaussian dip to mortality at cold temperatures
    cold_dip <- 0.8 * exp(-0.01 * (avgT[i] - 0)^2)
    Temp.Mort <- Temp.Mort + cold_dip
    
    #mortality stays between 0 and 1
    Temp.Mort <- pmin(1, pmax(0, Temp.Mort))
    
    temp.S <- (1 - Temp.Mort) * (1 - baseline.M)
    
    #survival stays between 0 and 1
    larvS[i]  <- pmin(1, pmax(0, temp.S))
    
    # Create a scaler for temperature
    temp_factor <- min(1, max(0.5, avgT[i] / 20))  # Normalize temperature effect
    
    # Adjust survival rate based on predation category and temperature
    if (pred[i] == "high") {
      pred_effect <- 1 - (0.5 * temp_factor)  # Max 50% reduction at high temps
    } else if (pred[i] == "medium") {
      pred_effect <- 1 - (0.25 * temp_factor)  # Max 25% reduction at high temps
    } else if (pred[i] == "low") {
      pred_effect <- 1 - (0.1 * temp_factor)  # Max 10% reduction at high temps
    }
    
    larvS[i] <- (larvS[i] * pred_effect)
  }
  
  age0<-c()
  for(i in 1:5){
    age0[i]<- if (stochastic){rbinom(1,size=larvae[i],larvS[i])} else{round(larvae[i]*larvS[i])} 
  }
  
  
  return(age0)
}
  # Larvae.Surv(larvae=c(101,101,101,101,101),avgT=c(15.2,15.5,10.5,10.6,23.5),pred=c("medium","high","low","medium","high"),
  #            stochastic=0)

#######################################################
#####   Juvenile trib and river survival
######################################################
#How I developed is in calc_parms
juvTrib_survival <- function(temp) {
  B0 <- -23.5570
  B1 <- 0.8714
  baseline.S <- 0.86
  
  #Calculate mortality
  Temp.Mort <- 1/(1+exp(-(B0 + B1*temp)))
  
  # Add dip to mortality at cold temperatures
  cold_dip <- 0.10 * exp(-0.01 * (temp - 0)^2)
  Temp.Mort <- Temp.Mort + cold_dip
  
  #mortality stays between 0 and 1
  Temp.Mort <- pmin(1, pmax(0, Temp.Mort))
  
  temp.S <- baseline.S * (1 - Temp.Mort)
  
  #survival stays between 0 and 1
  temp.S <- pmin(1, pmax(0, temp.S))
  

return(temp.S)
}


#juvTrib_survival(temp=15)



#######################################################
#####   Juvenile delta survival
######################################################
#I want to add predation yet-entrainment is a separate submodel
#I'm leaving this as the old temperature relationship with no dip
Delta.Surv <- function(temp) {
  
  B0 <- -23.5570
  B1 <- 0.8714
  baseline.S= 0.86
  
  Temp.Mort <- 1/(1+exp(-(B0 + B1*temp)))
  
  # # Calculate survival rates
  temp.S <- baseline.S * (1 - Temp.Mort)
  
  #survival stays between 0 and 1
  temp.S <- pmin(1, pmax(0, temp.S))
  
  return(temp.S)
  
}

#Delta.Surv(temp=10)

# Create a function for repeated operations
# Create a function for repeated juvenile operations
# Create a function for repeated juvenile operations
apply_juv_survival <- function(index, temp, delta = FALSE,curr_month= NA) {
  surv <- if (delta) Delta.Surv(temp) else juvTrib_survival(temp)
  cols <- if (index %in% c(10, 11, 12)) 1:4 else 1:2
  
  st.fish[index, cols] <<- round(st.fish[index, cols] * surv)

  surv
}


#######################################################
##### Temp plot for SIT to review 
######################################################
# ##juvTemp plot
# # # Create a sequence of temperatures
# temp <- seq(0, 35, by = 0.1)
# B0 <- -23.5570
# B1 <- 0.8714
# baseline.S <- 0.86
# 
# #Calculate mortality
# Temp.Mort <- 1/(1+exp(-(B0 + B1*temp)))
# 
# # Add dip to mortality at cold temperatures
# cold_dip <- 0.10 * exp(-0.01 * (temp - 0)^2)
# Temp.Mort <- Temp.Mort + cold_dip
# 
# #mortality stays between 0 and 1
# Temp.Mort <- pmin(1, pmax(0, Temp.Mort))
# 
# temp.S <- baseline.S * (1 - Temp.Mort)
# 
# #survival stays between 0 and 1
# temp.S <- pmin(1, pmax(0, temp.S))
# 
# 
# # Create the plot
# plot(temp,temp.S, type = "l", col = "black", lwd = 2,
#      xlab = "Temperature (°C)", ylab = "Survival Probability",
#      main = "Juvenile Green sturgeon temperature Survival")
# # # Add a grid for better readability
#  grid()

# ##Egg Survival Plot
# avgT <- seq(0, 40, by = 0.1)
# B0 <- -15  # Intercept parameter
# B1 <- 0.65   # Slope parameter
# max_survival <- 0.001  # Maximum survival rate (5%)
# 
# # Base survival using logistic function
# Temp.Mort <- 1/(1+exp(-(B0 + B1*avgT)))
# 
# # Add additional decline at cold temperatures (below ~10°C)
# cold_dip <- 0.20 * exp(-0.01 * (avgT - 0)^2)
# Temp.Mort <- Temp.Mort + cold_dip
# # Calculate final survival rate
# # #mortality stays between 0 and 1
#  Temp.Mort <- pmin(1, pmax(0, Temp.Mort))
#  
#  temp.S <- max_survival * (1 - Temp.Mort)
# # 
# # #survival stays between 0 and 1
#  EggS <- pmin(1, pmax(0, temp.S))
# 
# 
# 
# # # Create the plot
# plot(avgT,EggS, type = "l", col = "black", lwd = 2,
#      xlab = "Temperature (°C)", ylab = "Survival Probability",
#      main = "Green sturgeon Egg temperature Survival")
# # # Add a grid for better readability
#  grid()
 
#  #delta plot->NO DIP at low temps
#  # Define parameters
# B0 <- -26.5570
# B1 <- 0.8714
# baseline.M = 0.05 
# 
#  #
#  # # Create a sequence of temperatures
#  temp <- seq(20, 40, by = 0.1)
#  
#  Temp.Mort <- 1/(1+exp(-(B0 + B1*temp)))
#  #
#  # # Calculate survival rates
#  temp.S <- (1 - Temp.Mort) * (1 - baseline.M)
#  #survival stays between 0 and 1
#  temp.S <- pmin(1, pmax(0, temp.S))
#  
#  #
#  # # Create the plot
#  plot(temp, temp.S, type = "l", col = "black", lwd = 2,
#       xlab = "Temperature (°C)", ylab = "Survival Probability",
#       main = "Delta Juvenile Green sturgeon temperature Survival")
#  #
#  # # Add a grid for better readability
#  grid()


# ##larvae Temp plot
# # Define parameters
 # B0 <- -15.7102
 # B1 <- 0.6216
 # 
 # # Create a sequence of temperatures
 # temp <- seq(9, 40, by = 0.1)
 # 
 # # Calculate base mortality
 # Temp.Mort <- 1/(1+exp(-(B0 + B1*temp)))
 # 
 # # Add a Gaussian dip to mortality at cold temperatures
 # cold_dip <- 0.8 * exp(-0.01 * (temp - 0)^2)
 # Temp.Mort <- Temp.Mort + cold_dip
 # 
 # # Calculate survival
 # temp.S <- 1 - pmin(1, pmax(0, Temp.Mort))
 # 
 # # Create the plot
 # plot(temp, temp.S, type = "l", col = "black", lwd = 2,
 #      xlab = "Temperature (°C)", ylab = "Survival Probability",
 #      main = "Larvae Green sturgeon temperature Survival")
 # # # Add a grid for better readability
 # grid()

##Predation Plot
# Define parameters
# B0 <- -15.7102
# B1 <- 0.6216
# baseline_survival <- 0.85
# 
# # Create temperature sequence
# temps <- seq(5, 25, by = 0.1)
# 
# # Calculate survival rates for different predation levels
# calculateSurvival <- function(temps, predLevel) {
#   survival <- numeric(length(temps))
#   
#   for (i in 1:length(temps)) {
#     # Base temperature mortality
#     Temp.Mort <- 1 / (1 + exp(-(B0 + B1 * temps[i])))
#     
#     # Cold temperature effect
#     cold_dip <- 0.05 * exp(-0.1 * (temps[i] - 5)^2)
#     Temp.Mort <- Temp.Mort + cold_dip
#     
#     # Ensure mortality stays between 0 and 1
#     Temp.Mort <- pmin(1, pmax(0, Temp.Mort))
#     
#     # Base survival rate
#     base_surv <- (1 - Temp.Mort) * baseline_survival
#     
#     # Temperature factor that increases with temperature
#     temp_factor <- pmin(1, pmax(0.5, temps[i] / 20))
#     
#     # Apply predation effect based on level
#     if (predLevel == "none") {
#       # No predation - slight reduction at high temperatures
#       pred_effect <- 1 - (0.02 * (temp_factor - 0.5)) * (temps[i] > 15)
#     } else if (predLevel == "low") {
#       # Low predation
#       pred_effect <- 1 - (0.12 * temp_factor)
#     } else if (predLevel == "medium") {
#       # Medium predation
#       pred_effect <- 1 - (0.22 * temp_factor)
#     } else if (predLevel == "high") {
#       # High predation
#       pred_effect <- 1 - (0.4 * temp_factor)
#     }
#     
#     # Final survival rate
#     survival[i] <- base_surv * pred_effect
#     
#     # Ensure survival stays between 0 and 1
#     survival[i] <- pmin(1, pmax(0, survival[i]))
#   }
#   
#   return(survival)
# }
# 
# # Calculate survival rates for each predation level
# surv_none <- calculateSurvival(temps, "none")
# surv_low <- calculateSurvival(temps, "low")
# surv_medium <- calculateSurvival(temps, "medium")
# surv_high <- calculateSurvival(temps, "high")
# 
# # Create the plot
# plot(temps, surv_none, type = "l", col = "black", lwd = 2,
#      xlab = "Temperature (°C)", ylab = "Larval Survival Rate",
#      ylim = c(0, 1), xlim = c(5, 25))
# 
# # Add lines for other predation levels
# lines(temps, surv_low, col = "green", lwd = 2)
# lines(temps, surv_medium, col = "blue", lwd = 2)
# lines(temps, surv_high, col = "red", lwd = 2)
# 
# # Add grid
# grid()
# 
# # Add legend
# legend("bottomright", 
#        legend = c("No Predation", "Low Predation", "Medium Predation", "High Predation"),
#        col = c("black", "green", "blue", "red"), 
#        lwd = 2, 
#        bg = "white")


# ##delta juvTemp plot
# # # Create a sequence of temperatures
# temp <- seq(0, 35, by = 0.1)
# B0 <- -23.5570
# B1 <- 0.8714
# baseline.S= 0.86
# 
# Temp.Mort <- 1/(1+exp(-(B0 + B1*temp)))
# 
# # # Calculate survival rates
# temp.S <- baseline.S * (1 - Temp.Mort)
# 
# #survival stays between 0 and 1
# temp.S <- pmin(1, pmax(0, temp.S))
# 
# # # Create the plot
# plot(temp,temp.S, type = "l", col = "black", lwd = 2,
#      xlab = "Temperature (°C)", ylab = "Survival Probability",
#      main = "Juvenile Green sturgeon temperature Survival")
# # # Add a grid for better readability
#  grid()