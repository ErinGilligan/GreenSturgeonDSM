#Survival functions used in the Green sturgeon model

#######################################################
#####     Adult Prespawn Survival
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
apply_adult_survival <- function(index, temp, stochastic = FALSE) {
  st.fish[index, 6:7] <<- Adult.S(st.fish[index, 6], st.fish[index, 7], temp, stochastic)
}


#######################################################
#####     Egg to Larvae Survival
######################################################
#Excluded parms=DO,scour,fertilization,turbid, pred, contaminants
#habit.avail (i would prefer to add this to the spawning function instead of Survival)
#avgT is average monthly temperature
#I calculated temp survival from fig 1 of Van Eenennaam et al. 2005 (glm is in calc_parms file)

Egg.Surv<- function(avgT, mode="sim",int=NULL){
  
  if (mode=="cal"){ B0=int} else {B0 = 0.1}
  
  EggS<-c()
  for(i in 1:5){
    #Temperature effect on survival
    EggS[i]<- max(0,-0.004 * (avgT[i] - 15)^2 + B0)
  }  
  
  return(EggS)
}

#Egg.Surv(avgT=stream_temp[tribs2use,month,yr],mode="sim") 
#Egg.Surv(avgT=runif(10,2,15),mode="sim")
#Egg.Surv(avgT=runif(10,2,15),mode="cal",int=0)

#######################################################
#####   Larvae to age 0 Survival
######################################################
#Excluded parms= pred, contaminants,habitat complexity,food.avail
#avgT is average monthly temperature
#pred= predation for lengths less than 220 mm (Baird et al. 2019) >220 pred risk=0
#diversions its own submodel
#temp surv taken from Linares-Casenave 2013 (I assumed abnormalities meant mortality
#since predation and other factors would likely kill them)

Larvae.Surv <- function(larvae,avgT,pred,mode="sim",int=NULL,stochastic){
  
  B0= -15.7102
  B1 = 0.6216
  
  if (mode=="cal") {baseline.M=int} else {baseline.M=0.13}

  larvS<-c()
  for(i in 1:5){
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
    
    larvS[i] <- larvS[i] * pred_effect
  }
  
age0<-c()
for(i in 1:5){
  age0[i]<- if (stochastic){rbinom(1,size=larvae[i],larvS[i])} else{round(larvae[i]*larvS[i])} 
}

 
  return(age0)
}
#Larvae.Surv(larvae=rep(1000,5),stream_temp[tribs2use,month,yr],pred=c("medium","high","low","medium","high"),decis,stochastic=0,mode="sim")
#Larvae.Surv(larvae=rep(1000,5),stream_temp[tribs2use,month,yr],stochastic=0,mode="cal",int=0)
#######################################################
#####   Juvenile trib and river survival
######################################################

#Function to simulate juvenile survival
juvTrib_survival <- function(temp,mode="sim",int=NULL) {
  
  B0 <- -26.5570
  B1 <- 0.8714
  
  # calibrate baseline
  if (mode=="cal") {
    baseline.M = int
  } else {
    baseline.M = 0.1  
  }
  
  #Calculate mortality
  Temp.Mort <- 1/(1+exp(-(B0 + B1*temp)))
  
  # Add dip to mortality at cold temperatures
  cold_dip <- 0.10 * exp(-0.01 * (temp - 0)^2)
  Temp.Mort <- Temp.Mort + cold_dip
  
  #mortality stays between 0 and 1
  Temp.Mort <- pmin(1, pmax(0, Temp.Mort))
  
  temp.S <- (1 - Temp.Mort) * (1 - baseline.M)
  
  #survival stays between 0 and 1
  temp.S <- pmin(1, pmax(0, temp.S))
  
}


#######################################################
#####   Juvenile delta survival
######################################################
#I want to add predation yet-entrainment is a separate submodel
Delta.Surv <- function(temp,mode="sim",int=NULL) {
  # Base parameters
  B0 <- -26.5570
  B1 <- 0.8714
  
  #Baseline mortality
  if (mode=="cal") {
    baseline.M = int
  } else {
    baseline.M = 0.05 
  }
  
  Temp.Mort <- 1/(1+exp(-(B0 + B1*temp)))
  
  # # Calculate survival rates
  temp.S <- (1 - Temp.Mort) * (1 - baseline.M)
  
  #survival stays between 0 and 1
  temp.S <- pmin(1, pmax(0, temp.S))
  
  return(temp.S)
}

#Delta.Surv(temp=delta_temperature[month,yr,1])
#Delta.Surv(temp=17,mode="sim")
#Delta.Surv(temp=17,mode="cal",int=-0.2)


apply_juv_survival <- function(index, temp, delta = FALSE,mode,int) {
  surv <- if (delta) Delta.Surv(temp,mode=mode,int=int) else juvTrib_survival(temp,mode=mode,int=int)
  cols <- if (index %in% c(10, 11, 12)) 1:4 else 1:2
  st.fish[index, cols] <<- round(st.fish[index, cols] * surv)
  surv
}

#######################################################
##### Temp plot for SIT to review 
######################################################
# # Define the sturgeon survival function
# sturgeon_survival <- function(temp) {
#   -0.002 * (temp - 17)^2 + 1
# }
# 
# # Generate temperature range
# temps <- seq(0, 35, length.out = 300)
# 
# # Calculate survival rates
# survival_rates <- sturgeon_survival(temps)
# 
# # Create the plot
# plot(temps, survival_rates, type = "l",
#      main = "Green Sturgeon Temperature Survival Function",
#      xlab = "Temperature (°C)", ylab = "Survival Rate",
#      ylim = c(0, 1))
# 
# # Add grid
# grid()
# 
# # Add vertical lines for optimal range
# abline(v = c(15, 19), col = "red", lty = 2)
# 
# # Add legend
# #legend("topright", legend = "Optimal Range", col = "red", lty = 2)
# 
# # Print some key values
# cat("Survival rate at 17°C:", sprintf("%.2f", sturgeon_survival(17)), "\n")
# cat("Survival rate at 15°C:", sprintf("%.2f", sturgeon_survival(15)), "\n")
# cat("Survival rate at 19°C:", sprintf("%.2f", sturgeon_survival(19)), "\n")
# cat("Survival rate at 10°C:", sprintf("%.2f", sturgeon_survival(10)), "\n")
# cat("Survival rate at 25°C:", sprintf("%.2f", sturgeon_survival(25)), "\n")

# 
# #######################################################
# ##### Plot the relationship developed for Temp to food for SIT to review
# ######################################################
# # Define the function
# temp_food <- function(temp, optimal_temp = 17) {
#   temp_food<- exp(-((temp - optimal_temp)^2) / (2 * 5^2))
# }
# 
# # Create a sequence of flow values
# tempfood_seq <- seq(0, 30, by = 0.1)
# 
# # Calculate the effect for each flow value
# effect_values <- sapply(tempfood_seq, temp_food)
# 
# # Create the plot
# plot(tempfood_seq, effect_values, type = "l",
#      main = "Temp Effect on Food Availability",
#      xlab = "Temperature (°C)",
#      ylab = "Proportion of max food available",
#      ylim = c(0, 1))




###OLD####in case i need#####
#Function to simulate juvenile survival
# juvTrib_survival <- function(flow, temp, optimal_flow, optimal_temp=17,max_food=100) {
#   
#   # First:Temperature effect on survival
#   temp_effect<- -0.002 * (temp - 17)^2 + 1
#   
#   #Develop a relationship for food availability in relation to temp and flow
#   # Flow effect (bell curve)
#   flow_food <- exp(-((flow - optimal_flow)^2) / (2 * optimal_flow^2))
#   # Temperature effect (bell curve)
#   temp_food<- exp(-((temp - optimal_temp)^2) / (2 * 5^2))
#   
#   # Combine effects
#   food_availability <- max_food * flow_food * temp_food
#   
#   # Add some random variation
#   food_availability <- food_availability + rnorm(length(flow), mean = 0, sd = 5)
#   food_availability<-pmax(food_availability, 0) #non-negative 
#   
#   # Params
#   food_effect <- food_availability / 100
#   
#   # Combine effects (I'm giving them equal weights right now because im not sure)
#   survival_prob <- 0.5 * temp_effect + 0.5 * food_effect
#   
#   # Ensure probability is between 0 and 1
#   return(pmax(pmin(survival_prob, 1), 0))
# }
# 
# juvTrib_survival(flow=3000, temp=10,optimal_flow=5000,max_food=100)