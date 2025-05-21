#######################################################
#####     stranding submodel for green sturgeon
######################################################

strand.sub<-function(spawn.rt,overtopYolo,overtopSutter,stochastic,parms,incr){

X.Yolo<-0.17; if(sum(parms == "yolo.Strand")) X.Yolo<- qbeta(incr [parms=="yolo.Strand"], 8.929,46.88)
X.Sut<-0.17; if(sum(parms == "Sutter.Strand")) X.Sut<- qbeta(incr [parms=="Sutter.Strand"], 8.929,46.88)
  
  if (overtopYolo==TRUE) {
    yolo.risk=X.Yolo} else {yolo.risk=0} 
  if (overtopSutter==TRUE) {
    sutter.risk=X.Sut} else {sutter.risk=0}
  
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
 #            stochastic=0,parms=c("yolo.Strand","Sutter.Strand"),incr=c(0.1,0.1))



#######################################################
#####     stranding Survival submodel for green sturgeon
######################################################
strand.S<-function(stranded,stochastic,decis,parms,incr){

X.strandS<-0.965; if(sum(parms == "strand.S")) X.strandS<- qbeta(incr [parms=="strand.S"], 213.58, 9.67)
X.rescue<-0.997; if(sum(parms == "Rescue.S")) X.rescue<- qbeta(incr [parms=="Rescue.S"], 2669.3, 10.99)


strand.S.Yol<-if (15 %in% decis) {X.rescue} else {X.strandS}
strand.S.Sut<-if (16 %in% decis) {X.rescue} else {X.strandS}

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

#strand.S(stranded=st.fish[8:9,6:7],decis=17,stochastic=0,
#         parms=c("strand.S","Rescue.S"), incr=c(0.5,0.5))

#######################################################
#####     stranding abandonment
######################################################
abandonment<-function(stranded,stochastic,parms,incr){
  yolo.abandon<-0.33; if(sum(parms == "Abandon.yolo")) yolo.abandon<- qbeta(incr [parms=="Abandon.yolo"], 5, 9)
  sutter.abandon<-0.19; if(sum(parms == "Abandon.sutt")) sutter.abandon<- qbeta(incr [parms=="Abandon.sutt"], 3, 10)
  
  #send the stranded out from previous month up first
  abandon.Yolo<- if(stochastic){rbinom(length(st.fish[8,6:7]),st.fish[8,6:7],yolo.abandon)}else{
    round(st.fish[8,6:7]*yolo.abandon)}   #yolo abandonment
  abandon.Sutter<- if(stochastic){rbinom(length(st.fish[9,6:7]),st.fish[9,6:7],sutter.abandon)}else{
    round(st.fish[9,6:7]*sutter.abandon)} 
  abandon<-rbind(abandon.Yolo,abandon.Sutter)
  return(abandon)
}

#abandonment(stranded=st.fish[8:9,6:7],stochastic,parms=c("Abandon.yolo","Abandon.sutt"), incr=c(0.4,0.4))
