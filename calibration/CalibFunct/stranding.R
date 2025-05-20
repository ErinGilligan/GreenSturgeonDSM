#######################################################
#####     stranding submodel for green sturgeon
######################################################

strand.sub<-function(spawn.rt,overtopYolo,overtopSutter,stochastic){
  
  if (overtopYolo==TRUE) {
    yolo.risk=rnorm(1,0.17,0.048)} else {yolo.risk=0} 
  if (overtopSutter==TRUE) {
    sutter.risk=rnorm(1,0.17,0.048)} else {sutter.risk=0}
  
  strand.yolo<-if (stochastic){
    strand.y<-rbinom(length(spawn.rt),spawn.rt,yolo.risk)
    matrix(strand.y,nrow=4,ncol=2)}else{round(spawn.rt*yolo.risk)}
  spawn.rt<-spawn.rt-strand.yolo
  strand.yolo<-colSums(strand.yolo)
  
  strand.sutter<-if (stochastic){
    strand.s<-rbinom(length(spawn.rt),spawn.rt,sutter.risk)
    matrix(strand.s,nrow=4,ncol=2)}else{round(spawn.rt*sutter.risk)}
  spawn.rt<-spawn.rt-strand.sutter
  strand.sutter<-colSums(strand.sutter)
  
  strand<-rbind(strand.yolo,strand.sutter)
  return(list(
    spawn.rt,
    strand))  
  
  
}


 # strand.sub(spawn.rt=spawnroute[1:4,],
 #            overtopYolo=gates_overtopped$biop_itp_2018_2019[month,yr,2],
 #            overtopSutter=gates_overtopped$biop_itp_2018_2019[month,yr,1],
 #            stochastic=0)



#######################################################
#####     stranding Survival submodel for green sturgeon
######################################################
strand.S<-function(stranded,decis,stochastic){
  
  StrandS<- if (stochastic){rbeta(1,213.58,9.67)} else {0.965}
  RescueS<- if (stochastic){rbeta(1,2669.30,10.995)} else {0.997}
  
  strand.S.Yol<-if (decis %in% c(15)) {RescueS} else {StrandS}
  strand.S.Sut<-if (decis %in% c(16)) {RescueS} else {StrandS}
  
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


#strand.S(stranded=st.fish[8:9,6:7],decis=17,stochastic=0)

#######################################################
#####     stranding abandonment
######################################################
abandonment<-function(stranded,stochastic){
  yolo.abandon<-0.33
  sutter.abandon<-0.13
  
  #send the stranded out from previous month up first
  abandon.Yolo<- if(stochastic){rbinom(length(st.fish[8,6:7]),st.fish[8,6:7],yolo.abandon)}else{
    round(st.fish[8,6:7]*yolo.abandon)}   #yolo abandonment
  abandon.Sutter<- if(stochastic){rbinom(length(st.fish[9,6:7]),st.fish[9,6:7],sutter.abandon)}else{
    round(st.fish[9,6:7]*sutter.abandon)} 
  abandon<-rbind(abandon.Yolo,abandon.Sutter)
  return(abandon)
}


#abandonment(stranded=st.fish[8:9,6:7],stochastic)
