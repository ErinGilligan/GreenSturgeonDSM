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
juv.migrate<-function(flow,trib){
  params <- list(
    UpSac = list(B0 = -5.9741, B1 = 0.001),    # UpSac parms from Poytress et al. 2024
    Feather = list(B0 = -6.5, B1 = 0.0015),    # Hypothetical Feather River (I want them to leave at lower flows)
    Yuba = list(B0 = -6.2, B1 = 0.0012),       # same for Yuba River
    Bear = list(B0 = -6.8, B1 = 0.002),        # Bear River-Not really important here just in case
    SJ = list(B0 = -5.8, B1 = 0.0008)  # San Joaquin River (higher flows than upsac)
  )
  
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

#juv.migrate(flow=15000,trib='UpSac')

# Main function to handle outmigration for all tributaries
dist.migrators <- function(Qmean, yr) {
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
    cum_probs[[trib_name]] <- juv.migrate(flows, trib_name)
  }
  
  return(cum_probs)
}

#cumdistrib<-dist.migrators(Qmean, yr)

migrate_and_check_capacity <- function(fish_count, probability, capacity) {
  migrants <- floor(probability * fish_count)
  excess <- max(0, fish_count - capacity)
  forced_migrants <- max(migrants, excess)
  
  remaining <- fish_count - forced_migrants
  
  # Distribute migrants
  cont_mig <- floor(forced_migrants * 0.62)
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
 Adult.Early<-function(flow,trib,stochastic){
   params <- list(
     UpSac = list(B0 = -1.62, B1 = 0.0001),  # Upper Sacramento parameters from original 
     Feather = list(B0 = -1.8, B1 = 0.00012),  # my best guess relating to Feather River
     Yuba = list(B0 = -1.7, B1 = 0.00011),     # my best guess relating to Yuba River
     Bear = list(B0 = -1.9, B1 = 0.00013),     # my best guess relating to Bear River
     SJ = list(B0 = -1.5, B1 = 0.00009)        # my best guess relating to San Joaquin River
   )
   
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

#Adult.Early(mean(Qmean[1,5:7,1]),trib=c("UpSac"),stochastic=0)


# #######################################################
# #####  Plotting code to show SIT
# ######################################################
# Define parms
# params <- list(
#   UpSac = list(B0 = -5.9741, B1 = 0.001),    # UpSac parms from Poytress et al. 2024
#   Feather = list(B0 = -6.5, B1 = 0.0015),    # Hypothetical Feather River (I want them to leave at lower flows)
#   Yuba = list(B0 = -6.2, B1 = 0.0012),       # same for Yuba River
#   Bear = list(B0 = -6.8, B1 = 0.002),        # Bear River-Not really important here just in case
#   SJ = list(B0 = -5.8, B1 = 0.0008)  # Hypothetical parameters for San Joaquin River (higher flows than upsac)
# )
# 
# # Function to plot outmigration probability vs flow for all rivers
# plot_outmigration_curves <- function() {
#   flows <- seq(0, 15000, by = 100)
# 
#   # Calculate probabilities for each river
#   probs <- lapply(params, function(p) 1 / (1 + exp(-(p$B0 + p$B1 * flows))))
# 
#   # Plot
#   colors <- c("blue", "red", "green", "purple", "orange")
#   plot(flows, probs[[1]], type = "l", col = colors[1],
#        xlab = "Flow (cfs)", ylab = "Probability of Outmigration",
#        main = "Outmigration Probability vs Flow", ylim = c(0, 1))
# 
#   for (i in 2:length(probs)) {
#     lines(flows, probs[[i]], col = colors[i])
#   }
# 
#   legend("bottomright", legend = names(params),
#          col = colors, lty = 1, cex = 0.8)
# }
# 
# # Plot the outmigration curves
# plot_outmigration_curves()
# Function to plot cumulative probability
# Example usage
# st.fish2 <- matrix(c(1000, 1000, 1000, 1000, 1000, 1000, 0, 0, 0, 0, 0, 0), nrow = 6, ncol = 2)
# Qmean <- array(runif(6*12*2, 1000, 5000), dim = c(31, 12, 2))
# yr <- 1
# 
# result <- handle_outmigration(st.fish, Qmean, yr)
# 
# handle_outmigration(st.fish, Qmean, yr)

# # Plotting function for SIT
# plot_cumulative_probability <- function(monthly_flows, tributaries) {
#   cum_probs <- lapply(tributaries, function(trib) {
#     juv.migrate(monthly_flows, trib)
#   })
#   
#   plot(1:6, cum_probs[[1]], type = "n", xlim = c(1, 6), ylim = c(0, 1),
#        xlab = "Month", ylab = "Cumulative Probability",
#        main = "Cumulative Probability of Juvenile Outmigration",
#        xaxt = "n")
#   
#   axis(1, at = 1:6, labels = c("Oct", "Nov", "Dec", "Jan", "Feb", "Mar"))
#   
#   colors <- c("blue", "red", "green", "purple", "orange")
#   
#   for (i in seq_along(tributaries)) {
#     lines(1:6, cum_probs[[i]], col = colors[i], lwd = 2)
#   }
#   
#   legend("bottomright", legend = tributaries, col = colors, 
#          lty = 1, lwd = 2, bty = "n")
#   
#   grid()
# }
# 
# # Example plot
# example_flows <- c(3000, 3500, 4000, 4500, 5000, 4500)
# tributaries <- c("UpSac", "Feather", "Yuba", "Bear", "SJ")
# plot_cumulative_probability(example_flows, tributaries)
# flows <- c(
#   Qmean[31, 10:12, 2],
#   Qmean[31, 1:3, 2 + 1]
# )
# plot_cumulative_probability(flows,"SJ")



# #Plots for adult early migration
# library(ggplot2)
# params <- list(
#   UpSac = list(B0 = -1.62, B1 = 0.0001),  # Upper Sacramento parameters from original
#   Feather = list(B0 = -1.8, B1 = 0.00012),  # my best guess relating to Feather River
#   Yuba = list(B0 = -1.7, B1 = 0.00011),     # my best guess relating to Yuba River
#   SJ = list(B0 = -1.5, B1 = 0.00009)        # my best guess relating to San Joaquin River
# )
# # Create a sequence of flow values
# flow_values <- seq(0, 20000, by = 100)
# 
# # Function to calculate proportion of early migration for a given flow and tributary
# calculate_prop_early <- function(flow, trib) {
#   B0 <- params[[trib]]$B0
#   B1 <- params[[trib]]$B1
#   prop.early <- 1 / (1 + exp(-(B0 + B1 * flow)))
#   return(prop.early)
# }
# 
# # Create a data frame with results for all tributaries
# results_df <- expand.grid(River = names(params), Flow = flow_values)
# results_df$Prop_Early <- mapply(function(river, flow) {
#   calculate_prop_early(flow, river)
# }, results_df$River, results_df$Flow)
# 
# # Find the flow value where prop.early = 0.5 for each river
# threshold_flows <- sapply(names(params), function(trib) {
#   B0 <- params[[trib]]$B0
#   B1 <- params[[trib]]$B1
#   # Solving for flow when prop.early = 0.5:
#   # 0.5 = 1 / (1 + exp(-(B0 + B1 * flow)))
#   # 1 = 2 / (1 + exp(-(B0 + B1 * flow)))
#   # 1 + exp(-(B0 + B1 * flow)) = 2
#   # exp(-(B0 + B1 * flow)) = 1
#   # -(B0 + B1 * flow) = 0
#   # B1 * flow = -B0
#   # flow = -B0 / B1
#   flow_at_half <- -B0 / B1
#   return(flow_at_half)
# })
# 
# threshold_df <- data.frame(
#   River = names(params),
#   Threshold_Flow = threshold_flows
# )
# 
# # Create logistic curves plot
#  ggplot(results_df, aes(x = Flow, y = Prop_Early, color = River)) +
#   geom_line(size = 1) +
#   labs(title = "Early outmigration by tributary",
#        x = "Flow",
#        y = "Proportion of Early Migrators") +
#   scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1)) +
#   theme_minimal() +
#   theme(legend.position = "bottom")
