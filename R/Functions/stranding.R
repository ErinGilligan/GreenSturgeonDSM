#######################################################
#####     stranding submodel for green sturgeon
######################################################

strand.sub<-function(spawn.rt,overtopYolo,overtopSutter,stochastic){
  
  yolo.risk.mean <- 0.17
  yolo.risk.sd <- 0.048
  sutter.risk.mean <- 0.17
  sutter.risk.sd <- 0.048
  
  # Calculate risk based on overtopping conditions
  if (overtopYolo == TRUE) {
    yolo.risk = if(stochastic) rnorm(1, yolo.risk.mean, yolo.risk.sd) else yolo.risk.mean
  } else {
    yolo.risk = 0
  } 
  
  if (overtopSutter == TRUE) {
    sutter.risk = if(stochastic) rnorm(1, sutter.risk.mean, sutter.risk.sd) else sutter.risk.mean
  } else {
    sutter.risk = 0
  }
  
  # Calculate strand for Yolo
  strand.yolo <- if (stochastic) {
    strand.y <- rbinom(length(spawn.rt), spawn.rt, yolo.risk)
    matrix(strand.y, nrow = 4, ncol = 2)
  } else {
    matrix(round(spawn.rt * yolo.risk), nrow = 4, ncol = 2)
  }
  
  spawn.rt <- spawn.rt - strand.yolo
  strand.yolo <- colSums(strand.yolo)
  
  # Calculate strand for Sutter
  strand.sutter <- if (stochastic) {
    strand.s <- rbinom(length(spawn.rt), spawn.rt, sutter.risk)
    matrix(strand.s, nrow = 4, ncol = 2)
  } else {
    matrix(round(spawn.rt * sutter.risk), nrow = 4, ncol = 2)
  }
  
  spawn.rt <- spawn.rt - strand.sutter
  strand.sutter <- colSums(strand.sutter)
  
  strand <- rbind(strand.yolo, strand.sutter)
  return(list(
    spawn.rt,
    strand
  ))  
}

#strand.sub(spawn.rt=spawnroute[1:4,],
#          overtopYolo=CalSim3_metrics$gates_overtopped[month,yr,2],
#          overtopSutter=CalSim3_metrics$gates_overtopped[month,yr,1],
#         stochastic=1)



#######################################################
#####     stranding Survival submodel for green sturgeon
######################################################
strand.S<-function(stranded,decis,stochastic){

StrandS<- if (stochastic){rbeta(1,213.58,9.67)} else {0.965}
RescueS<- if (stochastic){rbeta(1,2669.30,10.995)} else {0.997}
  
strand.S.Yol<-if (15 %in% decis) {RescueS} else {StrandS}
strand.S.Sut<-if (16 %in% decis){RescueS} else {StrandS}

extract.Strand<-if(stochastic){
  surv.strand.Y<-rbinom(2,stranded[1,],strand.S.Yol)
  surv.strand.S<-rbinom(2,stranded[2,],strand.S.Sut)
  surv.strand<-rbind(surv.strand.Y,surv.strand.S)
  matrix(surv.strand,nrow=2,ncol=2)}else{
  surv.strand.Y<-round(stranded[1,]*strand.S.Yol)
  surv.strand.S<-round(stranded[2,]*strand.S.Sut)
  surv.strand<-rbind(surv.strand.Y,surv.strand.S)
  matrix(surv.strand,nrow=2,ncol=2)}

return(extract.Strand)
}

#strand.S(stranded=st.fish[8:9,6:7],decis=1,stochastic=1)

#######################################################
#####     stranding abandonment
######################################################
abandonment<-function(stranded,stochastic){
  yolo.abandon<-0.33
  sutter.abandon<-0.19
  
  #send the stranded out from previous month up first
  abandon.Yolo<- if(stochastic){rbinom(length(st.fish[8,6:7]),st.fish[8,6:7],yolo.abandon)}else{
    round(st.fish[8,6:7]*yolo.abandon)}   #yolo abandonment
  abandon.Sutter<- if(stochastic){rbinom(length(st.fish[9,6:7]),st.fish[9,6:7],sutter.abandon)}else{
    round(st.fish[9,6:7]*sutter.abandon)} 
  abandon<-rbind(abandon.Yolo,abandon.Sutter)
  return(abandon)
}

#abandonment(stranded=st.fish[8:9,6:7],stochastic=1)


##Plot for SIT
# #Generate x values
# x <- seq(0.8, 1, length.out = 1000)
# 
# # Calculate y values for both distributions
# y_strand <- dbeta(x, shape1 = 213.58, shape2 = 9.67)
# y_rescue <- dbeta(x, shape1 = 2669.30, shape2 = 10.995)
# 
# # Set up the plot
# plot(x, y_strand, type = "l", col = "blue", lwd = 2,
#      xlab = "Value", ylab = "Density", ylim=c(0,max(y_rescue)),
#      main = "Beta Distributions: StrandS vs RescueS")
# 
# # Add the second line
# lines(x, y_rescue, col = "red", lwd = 2)
# 
# # Add a legend
# legend("topright", legend = c("Stranding Survival", "Rescue Survival"),
#        col = c("blue", "red"), lwd = 2)
