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


#Make a plot for SIT to review
# Set seed for reproducibility
# set.seed(20)
# 
# # Parameters
# n.pop <- 10000
# maxAge <- 34
# Natural.S <- 0.85
# 
# # Generate initial population
# n.init <- MASS::rnegbin(n.pop, maxAge, Natural.S)
# n.init <- n.init[n.init <= maxAge]
# 
# # Create age table
# age.table <- table(n.init)
# 
# # Calculate counts for each age group
# n.age0 <- sum(age.table[1])  # age 0
# n.age1 <- sum(age.table[2])  # age 1
# n.age2 <- sum(age.table[3])  # age 2
# n.juvs <- sum(age.table[4:5])  # juveniles (3-4)
# n.marineJ <- sum(age.table[6:11])  # marine juveniles
# n.sub <- sum(age.table[12:16])  # subadults
# n.sm.ad <- sum(age.table[17:26])  # small adults
# n.lg.ad <- sum(age.table[27:35])  # large adults
# 
# # Create a data frame for plotting
# #
# #
# plot_data <- data.frame(
#   AgeGroup = c("Age 0","Age 1", "Age 2", "Juveniles",
#                "Subadults","Small Adults", "Large Adults"),
#   Count = c(round(n.age0*4.2),round(n.age1*4.2), round(n.age2*4.2),
#             round(n.juvs*4.2), n.marineJ+n.sub, n.sm.ad, n.lg.ad)
# )
# 
# # Create the bar plot
# barplot(plot_data$Count,
#         names.arg = plot_data$AgeGroup,
#         main = "Age Distribution in Population",
#         xlab = "Age Group",
#         ylab = "Count",
#         col = "gray",
#         ylim=c(0,2800),
#         axis.lty=1,
#         las = 1,  # Rotate x-axis labels
#         cex.names = 0.7)  # Adjust text size for x-axis labels
# 
# # Add text labels on top of each bar
# text(x = seq(0.7, by = 1.2, length.out = 8),  # Adjust x positions
#      y = plot_data$Count,
#      labels = plot_data$Count,
#      pos = 3,
#      cex = 0.8)
# box()
