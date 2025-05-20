set.array <- function(N0) {
  Tot.stages <- 7 #age 0, age 1, age 2, juv (3-4), sub, small adult, lg adult
  Tot.loc <- 14 #up sac, upper mid sac, feather, yuba, bear, lower mid sac,
  #yolo bypass, Sutter bypass, lower sac, N delta, San Joaquin,
  #S. delta, Bays, Ocean
  
  #List names so I don't have to keep looking back
  column.names <- c("Age0","Age1","Age2","Juv","subAd",'sm.Adult',"lg.Adult")
  row.names <- c("Upper Sac","Feather","Yuba", "Bear","San Joaquin","Upper mid Sac","lower mid sac","yolo bypass","Sutter bypass","lower Sac",
                 "N delta","S delta", "bays","Ocean")
  
  # Create popn state matrices 5 tributaries, 8 stages/ages, 8 movement locations
  # stage groups are larvae, age 0, age 1, age 2, Juve (3-4), subadult, small adult, lg adults
  st.fish <- array(0, dim=c(Tot.loc,Tot.stages), dimnames = list(row.names,column.names))
  
  #Proportion Age 1 throughout
  #S delta
  st.fish[12,2] <- floor(c(N0[2,1]) * 0.25)
  #N delta
  st.fish[11,2] <- floor(c(N0[2,1]) * 0.50)
  #lower Sac
  st.fish[10,2] <- floor(c(N0[2,1]) * 0.25)
  
  #Proportion and distribute Age2 
  ## move 25% into all possible areas-> ocean
  st.fish[14,3] <- floor(c(N0[3,1]) * 0.25)
  #bays
  st.fish[13,3] <- floor(c(N0[3,1]) * 0.25)
  #S delta
  st.fish[12,3] <- floor(c(N0[3,1]) * 0.25)
  #N delta
  st.fish[11,3] <- floor(c(N0[3,1]) * 0.25)
  
  #Proportion and distribute juveniles
  ## move 25% into all possible areas-> ocean
  st.fish[14,4] <-floor(c(N0[4,1]) * 0.25)
  #bays
  st.fish[13,4] <- floor(c(N0[4,1]) * 0.25)
  #S delta
  st.fish[12,4] <- floor(c(N0[4,1]) * 0.25)
  #N delta
  st.fish[11,4] <- floor(c(N0[4,1]) * 0.25)
  
  #Proportion and distribute Subadults
  ## move 100% into ocean
  st.fish[14,5] <- floor(c(N0[5,1]))

  
  #Set up ocean small adults 
  st.fish[14,6] <- floor(c(N0[6,1]))
  
  #Set up ocean large adults
  st.fish[14,7] <- floor(c(N0[7,1]))
  
  return(st.fish)
}