#Create a starting population from a negative binomial distribution based on 
#Dudley et al. 2024 paper. Note: n.pop over estimates population that way we produce 
#approximately 6500 fish as a starting pop. The function returns a list of estimates
#for each age class needed in the DSM. Calibrate the model to these proportions
#WE WILL PROBABLY NEEED TO TEST THE SENSITIVTY OF STARTING POP

StartPop<-function(n.pop,maxAge,Natural.S){
  n.init<-MASS::rnegbin(n.pop,maxAge,Natural.S)
  n.init <- n.init[n.init <= maxAge]
  age.table<-table(n.init)
  n.sm.ad<-sum(age.table[17:26])  #small adults
  n.lg.ad<-sum(age.table[27:35])  #large adults
  n.sub<-sum(age.table[12:16])  #subadults
  n.marineJ<-sum(age.table[6:11])   #marine juveniles
  n.age0<-sum(age.table[1]) #age 0
  n.age1<-sum(age.table[2]) #age 1
  n.age2<-sum(age.table[3]) #age 2
  n.juvs<-sum(age.table[4:5])#juveniles (3-4)
  
  return(list(
    n.age0=n.age0,
    n.age1=n.age2,
    n.age2=n.age2,
    n.juvs=n.juvs,
    n.marineJ=n.marineJ,
    n.sub=n.sub,
    n.sm.ad=n.sm.ad,
    n.lg.ad=n.lg.ad
  ))
  
}

#n.init<-StartPop(n.pop=10000,maxAge=34,Natural.S=0.85)

#sum(n.init$n.age0+n.init$n.age1+n.init$n.age2+n.init$n.juvs+n.init$n.marineJ+ 
#      n.init$n.sub + n.init$n.sm.ad + n.init$n.lg.ad)
