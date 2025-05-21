#Survival functions used in the Green sturgeon model

#######################################################
#####     Adult Survival
######################################################
#I developed this curve from the CT max of 33.7 (0.09) degree c in 
#Sardella et al. 2008 where we expect 50% mort at 33.7C

  
 Adult.S<- function(sm.spawn,lg.spawn,avgT,stochastic,parms,incr){
   B0 <- -374.407;if(sum(parms == "AdultS.int")) B0<- qnorm(incr[parms == "AdultS.int"], B0, abs(B0*0.2))
   B1 <- 11.11;if(sum(parms == "Adult.tmp")) B1<- qnorm(incr[parms == "Adult.tmp"], B1, abs(B1*0.1))
   
   
   Temp.Mort<-1/(1+exp(-(B0 + B1*avgT)))
   temp.S<-1-Temp.Mort
   
   sm.spawners<- if (stochastic){sum(rbinom(sm.spawn,1,temp.S))} else{round(sm.spawn*temp.S)}
   lg.spawners<- if (stochastic){sum(rbinom(lg.spawn,1,temp.S))} else{round(lg.spawn*temp.S)}
   
   
   return(c(sm.spawners,lg.spawners))
 }
 
 # Create a function for adult survival
 apply_adult_survival <- function(index, temp, stochastic=FALSE,curr_month=NA,parms,incr) {
   st.fish[index, 6:7] <<- Adult.S(st.fish[index, 6], st.fish[index, 7], temp,
                                   stochastic, parms=parms, incr=incr)
 }
 


#######################################################
#####     Egg to Larvae Survival
######################################################
#Excluded parms=DO,scour,fertilization,turbid, pred, contaminants
#habit.avail (i would prefer to add this to the spawning function instead of Survival)
#avgT is average monthly temperature
#I calculated temp survival from fig 1 of Van Eenennaam et al. 2005 (glm is in calc_parms file)

Egg.Surv<- function(avgT,parms,incr){ 
    
  B0= -10.5; if(sum(parms == "eggS.int")) B0<- qnorm(incr[parms == "eggS.int"], B0, abs(B0*0.2))
  B1= 0.72; if(sum(parms == "egg.tmp")) B1<- qnorm(incr[parms == "egg.tmp"], B1, abs(B1*0.1))
  
  max_survival <- 0.005;if(sum(parms == "Egg.S")) max_survival<- qbeta(incr[parms == "Egg.S"], 2.39, 320.01)
  
  
  # Base survival using logistic function
  base_surv <- max_survival * 1/(1+exp(-(B0 + B1*avgT)))
  
  # Add additional decline at cold temperatures (below ~10°C)
  cold_penalty <- ifelse(avgT < 9, 
                         0.05 * (1 - avgT/9), 
                         0)
  
  # Calculate final survival rate
  EggS <- base_surv - cold_penalty
  
  # Ensure survival stays between 0 and 1
  EggS <- pmin(1, pmax(0, EggS))
  
  
  return(EggS)
}


# Egg.Surv(avgT=stream_temp[tribs2use,month,yr],parms=c("eggS.int","egg.tmp"),
# incr=c(0.275,0.975)) 


#######################################################
#####   Larvae to age 0 Survival
######################################################
#Excluded parms= pred, contaminants,habitat complexity,food.avail
#avgT is average monthly temperature
#pred= predation for lengths less than 220 mm (Baird et al. 2019) >220 pred risk=0
#diversions its own submodel
#temp surv taken from Linares-Casenave 2013 (I assumed abnormalities meant mortality
#since predation and other factors would likely kill them)

Larvae.Surv <- function(larvae,avgT,predation,stochastic,parms, incr){
  B0 = -15.7102; if(sum(parms == "larvaeS.int")) B0<- qnorm(incr[parms == "larvaeS.int"], B0, 1.26)
  B1 = 0.6216; if(sum(parms == "larvae.tmp")) B1<- qnorm(incr[parms == "larvae.tmp"], B1, 0.05)

  baseline.M=0.8;if(sum(parms == "larvae.Mort")) baseline.M<- qbeta(incr[parms == "larvae.Mort"], 15.7176, 3.8456)
 

  larvS<-c()
  for(i in 1:5){
    # Calculate base mortality
    Temp.Mort <- 1 / (1 + exp(-(B0 + B1 * avgT[i])))
    
    # Add a Gaussian dip to mortality at cold temperatures
    C1<-0.8; if(sum(parms == "larvae.cold")) B0<- qbeta(incr[parms == "larvae.cold"], 15.72,3.85)
    cold_dip <- C1 * exp(-0.01 * (avgT[i] - 0)^2)
    Temp.Mort <- Temp.Mort + cold_dip
    
    #mortality stays between 0 and 1
    Temp.Mort <- pmin(1, pmax(0, Temp.Mort))
    
    temp.S <- (1 - Temp.Mort) * (1 - baseline.M)
    
    #survival stays between 0 and 1
    larvS[i]  <- pmin(1, pmax(0, temp.S))
    
    # Create a scaler for temperature
    temp_factor <- min(1, max(0.5, avgT[i] / 20))  # Normalize temperature effect
    
    # Adjust survival rate based on predation category and temperature
    if (predation[i] == "high") {
      X1<-0.5;if(sum(parms == "high.pred")) X1<- qbeta(incr[parms == "high.pred"], 4.48, 8.96)
      pred_effect <- 1 - (X1 * temp_factor)  # Max 50% reduction at high temps
      
    } else if (predation[i] == "medium") {
        X1<-0.25;if(sum(parms == "med.pred")) X1<- qbeta(incr[parms == "med.pred"], 5.31, 14.02)
        pred_effect <- 1 - (X1 * temp_factor)  # Max 25% reduction at high temps
      
    } else if (predation[i] == "low") {
      X1<-0.1;if(sum(parms == "low.pred")) X1<- qbeta(incr[parms == "low.pred"], 1.376, 15.8)
      pred_effect <- 1 - (X1 * temp_factor)  # Max 10% reduction at high temps
    }
    
    larvS[i] <- larvS[i] * pred_effect
  }
  
  age0<-c()
  for(i in 1:5){
    age0[i]<- if (stochastic){rbinom(1,size=larvae[i],larvS[i])} else{round(larvae[i]*larvS[i])} 
  }
  
  
  return(age0)
}
#Larvae.Surv(larvae=rep(1000,5),avgT=c(7,10,15,5,12),stochastic=0, predation=pred_cats,
#            parms=c("larvaeS.int","larvae.tmp"), incr=c(1,0.5))
#######################################################
#####   Juvenile trib and river survival
######################################################

#Function to simulate juvenile survival
juvTrib_survival <- function(temp,parms,incr) {
  
  B0 <- -26.5570; if(sum(parms == "juv.TribS.int")) B0<- qnorm(incr[parms == "juv.TribS.int"], B0, abs(B0*0.2))
  B1 <- 0.8714; if(sum(parms == "juvTrib.tmp")) B1<- qnorm(incr[parms == "juvTrib.tmp"], B1, abs(B1*0.1))
  baseline.S <- 0.86;if(sum(parms == "juvTrib.S")) baseline.S<- qbeta(incr[parms == "juvTrib.S"], 39.22, 5.89)
  
  #Calculate mortality
  Temp.Mort <- 1/(1+exp(-(B0 + B1*temp)))
  
  # Add dip to mortality at cold temperatures
  C1<-0.10; if(sum(parms == "juv.cold")) C1<- qbeta(incr[parms == "juv.cold"], 1.48, 13.18)
  cold_dip <- C1 * exp(-0.01 * (temp - 0)^2)
  Temp.Mort <- Temp.Mort + cold_dip
  
  #mortality stays between 0 and 1
  Temp.Mort <- pmin(1, pmax(0, Temp.Mort))
  
  temp.S <- baseline.S * (1 - Temp.Mort)
  
  #survival stays between 0 and 1
  temp.S <- pmin(1, pmax(0, temp.S))
  
  
  return(temp.S)
}
#juvTrib_survival(temp=6,parms=c("juv.TribS.int","juvTrib.tmp"),incr=c(1,0.5))

#######################################################
#####   Juvenile delta survival
######################################################
#I want to add predation yet-entrainment is a separate submodel
Delta.Surv <- function(temp,parms,incr) {
  
  B0 <- -26.5570; if(sum(parms == "juv.DeltaS.int")) B0<- qnorm(incr[parms == "juv.DeltaS.int"], B0, abs(B0*0.2))
  B1 <- 0.8714; if(sum(parms == "juvDelt.tmp")) B1<- qnorm(incr[parms == "juvDelt.tmp"], B1, abs(B1*0.1))
  baseline.S=0.86;if(sum(parms == "juvDelt.S")) baseline.S<- qbeta(incr[parms == "juvDelt.S"], 39.22, 5.89)
  
  Temp.Mort <- 1/(1+exp(-(B0 + B1*temp)))
  
  # # Calculate survival rates
  temp.S <- baseline.S * (1 - Temp.Mort)
  
  #survival stays between 0 and 1
  temp.S <- pmin(1, pmax(0, temp.S))
  
  return(temp.S)
  
}

#Delta.Surv(temp=6,parms=c("juv.DeltaS.int","juvDelt.tmp"),incr=c(1,0.5))
 
 
 apply_juv_survival <- function(index, temp, delta = FALSE,curr_month=NA, parms,incr) {
   surv <- if (delta) Delta.Surv(temp,parms=parms,incr=incr) else juvTrib_survival(temp,parms=parms,incr=incr)
   cols <- if (index %in% c(10, 11, 12)) 1:4 else 1:2
   st.fish[index, cols] <<- round(st.fish[index, cols] * surv)
   surv
 }
 
 #apply_juv_survival(params[[1]], params[[2]])