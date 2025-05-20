
all.decis<-read.csv("C:/Users/gilliger/Box/Erin Dissertation/SIT sturgeon models/Green Sturgeon Model code/Results/SturgeonResultsFinalized.csv")
all.decis <- all.decis[, c("strategy", names(all.decis)[!names(all.decis) %in% c("X", "strategy")])]
names(all.decis)[2:15]<-c("lambda","recruit","spawners","large.spawners",
                          "num.holding","spawningUpSacMonths","spawningFeatherMonths","spawningYubaMonths",
                          "spawningSJMonths","num.rivers","rearUpSac","rearFeather","rearYuba","rearSJ")
all.decis$avg_spawning <- rowMeans(all.decis[, c("spawningUpSacMonths", "spawningFeatherMonths", "spawningYubaMonths", "spawningSJMonths")])
all.decis$avg_rearDur <- rowMeans(all.decis[, c("rearUpSac","rearFeather","rearYuba","rearSJ")])


######Create a correlation matrix of the attributes####
# Load required libraries
library(corrplot)
library(tidyverse)
data<-data.frame(
  lambda=all.decis$lambda,
  recruit=all.decis$recruit,
  spawners=all.decis$spawners,
  large.spawners=all.decis$large.spawners,
  num.holding=all.decis$num.holding,
  num.rivers=all.decis$num.rivers,
  avg_spawning=all.decis$avg_spawning,
  avg_rearDur=all.decis$avg_rearDur
)

cor_matrix<-cor(data)
# Create correlation plot
png("C:/Users/gilliger/Box/Erin Dissertation/SIT sturgeon models/Green Sturgeon Model code/Results/correlation_plot.png",
    width = 800, height = 800, res = 100)
corrplot(cor_matrix, 
         method = "color",
         type= "upper",
         order = "original",
         addCoef.col = "black",
         tl.col = "black",
         tl.srt = 45,
         col = colorRampPalette(c("#4575B4", "white", "#D73027"))(100))

# Add title
title("Correlation Plot of Strategy Attributes")

# Close the device
dev.off()

# Print the correlation matrix with rounded values for reference
print(round(cor_matrix, 3))

##############################################
###     Now proportional score outcomes
##############################################
library(dplyr)

#First get the mean value for each strategy
mean_lambda <- all.decis %>%
  select(strategy, lambda, V15, iteration) %>%
  group_by(strategy) %>%
  summarize(meanpop = mean(lambda, na.rm = TRUE))
#Prop score
mean_lambda$prop.score<-(mean_lambda$meanpop - min(mean_lambda$meanpop))/
  (max(mean_lambda$meanpop)-min(mean_lambda$meanpop))

#Mean recruit
mean_recruit <- all.decis %>%
  select(strategy, recruit, V15, iteration) %>%
  group_by(strategy) %>%
  summarize(meanrecruit = mean(recruit, na.rm = TRUE))
#Prop score
mean_recruit$prop.score<-(mean_recruit$meanrecruit - min(mean_recruit$meanrecruit))/
  (max(mean_recruit$meanrecruit)-min(mean_recruit$meanrecruit))

#Mean spawners
mean_spawners <- all.decis %>%
  select(strategy, spawners, V15, iteration) %>%
  group_by(strategy) %>%
  summarize(meanspawners = mean(spawners, na.rm = TRUE))
#Prop score
mean_spawners$prop.score<-(mean_spawners$meanspawners - min(mean_spawners$meanspawners))/
  (max(mean_spawners$meanspawners)-min(mean_spawners$meanspawners))

#Mean large spawners
mean_large.spawners <- all.decis %>%
  select(strategy, large.spawners, V15, iteration) %>%
  group_by(strategy) %>%
  summarize(meanlgspawners = mean(large.spawners, na.rm = TRUE))
#Prop score
mean_large.spawners$prop.score<-(mean_large.spawners$meanlgspawners - min(mean_large.spawners$meanlgspawners))/
  (max(mean_large.spawners$meanlgspawners)-min(mean_large.spawners$meanlgspawners))

#Mean holding
mean_holding <- all.decis %>%
  select(strategy, num.holding, V15, iteration) %>%
  group_by(strategy) %>%
  summarize(meanholding = mean(num.holding, na.rm = TRUE))
#Prop score
mean_holding$prop.score<-(mean_holding$meanholding - min(mean_holding$meanholding))/
  (max(mean_holding$meanholding)-min(mean_holding$meanholding))

#Mean spawn duration
mean_avgSpawn <- all.decis %>%
  select(strategy, avg_spawning, V15, iteration) %>%
  group_by(strategy) %>%
  summarize(meanspawnDur = mean(avg_spawning, na.rm = TRUE))
#Prop score
mean_avgSpawn$prop.score<-(mean_avgSpawn$meanspawnDur - min(mean_avgSpawn$meanspawnDur))/
  (max(mean_avgSpawn$meanspawnDur)-min(mean_avgSpawn$meanspawnDur))

#Mean num rivers
mean_rivers <- all.decis %>%
  select(strategy, num.rivers, V15, iteration) %>%
  group_by(strategy) %>%
  summarize(meanrivers = mean(num.rivers, na.rm = TRUE))
#Prop score
mean_rivers$prop.score<-(mean_rivers$meanrivers - min(mean_rivers$meanrivers))/
  (max(mean_rivers$meanrivers)-min(mean_rivers$meanrivers))

#Mean rearing duration
mean_rearDur <- all.decis %>%
  select(strategy, avg_rearDur, V15, iteration) %>%
  group_by(strategy) %>%
  summarize(meanrearDur = mean(avg_rearDur, na.rm = TRUE))
#Prop score
mean_rearDur$prop.score<-(mean_rearDur$meanrearDur - min(mean_rearDur$meanrearDur))/
  (max(mean_rearDur$meanrearDur)-min(mean_rearDur$meanrearDur))


##Create combined utility from AHP##
Utility <- (mean_lambda$prop.score*0.24) + (mean_recruit$prop.score*0.19) +  (mean_spawners$prop.score*0.14) +
  (mean_rivers$prop.score*0.19) + (mean_large.spawners$prop.score*0.09) + (mean_avgSpawn$prop.score*0.06) + 
  (mean_rearDur$prop.score*0.08) + (mean_holding$prop.score*0.05)


MultUtility <- (mean_lambda$prop.score*0.24) * (mean_recruit$prop.score*0.19) *  (mean_spawners$prop.score*0.14) *
  (mean_rivers$prop.score*0.19) * (mean_large.spawners$prop.score*0.09) * (mean_avgSpawn$prop.score*0.06) * 
  (mean_rearDur$prop.score*0.08) * (mean_holding$prop.score*0.05)

strats<-mean_lambda$strategy

Utilities<-data.frame(strats,Utility,MultUtility)
names(Utilities)<-c("Strategy","Additive_Util","MultiplicativeUtil")

#Incase I need to save it out
#write.csv(all.decis,file="Utilities.csv")


#########################
##Combined utility graphs 
#########################
library(ggplot2)

#Additive first
# Order strategies from highest to lowest utilty
order_idx <- order(Utilities$Additive_Util, decreasing = FALSE)
utility <- Utilities$Additive_Util[order_idx]
strategies <- Utilities$Strategy[order_idx]


#dataframe for ggplot
data <- data.frame(
  strategy = strategies,
  utility = utility
)

ggplot(data, aes(x = reorder(strategy, utility), y = utility)) +
  geom_bar(
    stat = "identity",
    fill = "steelblue"
  ) +
  coord_flip() +
  labs(
    x = "Strategy",
    y = "Utility"
  ) +
  ggtitle("Additive Utility") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )



##Multplicative Utility
# Order strategies from highest to lowest utilty

order_idx <- order(Utilities$MultiplicativeUtil, decreasing = FALSE)
utility <- Utilities$MultiplicativeUtil[order_idx]
strategies <- Utilities$Strategy[order_idx]


#dataframe for ggplot
data <- data.frame(
  strategy = strategies,
  utility = utility
)

ggplot(data, aes(x = reorder(strategy, utility), y = utility)) +
  geom_bar(
    stat = "identity",
    fill = "steelblue"
  ) +
  coord_flip() +
  labs(
    x = "Strategy",
    y = "Utility"
  ) +
  ggtitle("Multiplicative Utility") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )



##############################################
###     Graphs to review
##############################################

#########################
## Abundance graph
#########################

# Calculate means for lambda and recruits
lambda_means <- tapply(all.decis$lambda, all.decis$strategy, function(x) mean(x))
lambda_se <- tapply(all.decis$lambda, all.decis$strategy, function(x) sd(x)/sqrt(length(x)))
recruit_means <- tapply(all.decis$recruit, all.decis$strategy, function(x) mean(x))
recruit_se <- tapply(all.decis$recruit, all.decis$strategy, function(x) sd(x)/sqrt(length(x)))

# Order the strategies from highest to lowest lambda
order_idx <- order(lambda_means, decreasing = FALSE)
lambda_means <- lambda_means[order_idx]
recruit_means <- recruit_means[order_idx]
strategies <- names(lambda_means)

data <- data.frame(
  strategy = strategies,
  lambda = lambda_means,
  recruit = recruit_means
)
library(ggplot2)
ggplot(data, aes(x = reorder(strategy, lambda), y = value, fill = metric)) +
  geom_bar(
    aes(y = lambda, fill = "Total Population"),
    stat = "identity",
    position = "dodge",
    width = 0.4
  ) +
  geom_bar(
    aes(y = recruit, fill = "Recruits"),
    stat = "identity",
    position = "dodge",
    width = 0.3
  ) +
  scale_fill_manual(values = c("Total Population" = "steelblue", "Recruits" = "orange")) +
  coord_flip() +
  labs(
    x = "Strategy",
    y = "Abundance",
    fill = "Attribute"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top"
  )

#DO it for spawners too
spawner_means <- tapply(all.decis$spawners, all.decis$strategy, function(x) mean(x))
lg.spawner_means <- tapply(all.decis$large.spawners, all.decis$strategy, function(x) mean(x))


# Order the strategies from highest to lowest lambda
order_idx <- order(spawner_means, decreasing = FALSE)
spawner_means <- spawner_means[order_idx]
lg.spawner_means <- lg.spawner_means[order_idx]
strategies <- names(spawner_means)

data <- data.frame(
  strategy = strategies,
  spawners = spawner_means,
  lg.spawners = lg.spawner_means 
)


library(ggplot2)
ggplot(data, aes(x = reorder(strategy, spawners), y = value, fill = metric)) +
  geom_bar(
    aes(y = spawners, fill = "Spawners"),
    stat = "identity",
    position = "dodge",
    width = 0.4
  ) +
  geom_bar(
    aes(y = lg.spawners, fill = "Large Spawners"),
    stat = "identity",
    position = "dodge",
    width = 0.3
  ) +
  scale_fill_manual(values = c("Spawners" = "steelblue", "Large Spawners" = "orange")) +
  coord_flip() +
  labs(
    x = "Strategy",
    y = "Abundance",
    fill = "Attribute"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top"
  )

#########################
##Prop scored pop growth graph
#########################

#Proportionally scored figure
lambda_means <- tapply(all.decis$lambda, all.decis$strategy, function(x) mean(x))

# Order strategies from highest to lowest lambda
order_idx <- order(lambda_means, decreasing = FALSE)
lambda_means <- lambda_means[order_idx]
strategies <- names(lambda_means)

# Proportionally scale lambda values from 0 to 1
lambda_scaled <- (lambda_means - min(lambda_means))/
  (max(lambda_means)-min(lambda_means))

#dataframe for ggplot
data <- data.frame(
  strategy = strategies,
  lambda = lambda_scaled
)

ggplot(data, aes(x = reorder(strategy, lambda), y = lambda)) +
  geom_bar(
    stat = "identity",
    fill = "steelblue"
  ) +
  coord_flip() +
  labs(
    x = "Strategy",
    y = "Proportional score"
  ) +
  ggtitle("Population Abundance") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )

#Now do # of spawners
#Proportionally scored figure
spawner_means <- tapply(all.decis$spawners, all.decis$strategy, function(x) mean(x))

# Order strategies from highest to lowest lambda
order_idx <- order(spawner_means, decreasing = FALSE)
spawner_means <- spawner_means[order_idx]
strategies <- names(spawner_means)

# Proportionally scale lambda values from 0 to 1
spawner_scaled <- (spawner_means - min(spawner_means))/
  (max(spawner_means)-min(spawner_means))

#dataframe for ggplot
data <- data.frame(
  strategy = strategies,
  Spawners = spawner_scaled
)

ggplot(data, aes(x = reorder(strategy, Spawners), y = Spawners)) +
  geom_bar(
    stat = "identity",
    fill = "steelblue"
  ) +
  coord_flip() +
  labs(
    x = "Strategy",
    y = "Proportional score"
  ) +
  ggtitle("Number of Spawners") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )

#Now do # of large spawners
#Proportionally scored figure
lg.spawner_means <- tapply(all.decis$large.spawners, all.decis$strategy, function(x) mean(x))

# Order strategies from highest to lowest lambda
order_idx <- order(lg.spawner_means, decreasing = FALSE)
lg.spawner_means <- lg.spawner_means[order_idx]
strategies <- names(lg.spawner_means)

# Proportionally scale lambda values from 0 to 1
lg.spawner_scaled <- (lg.spawner_means - min(lg.spawner_means))/
  (max(lg.spawner_means)-min(lg.spawner_means))

#dataframe for ggplot
data <- data.frame(
  strategy = strategies,
  lg.Spawners = lg.spawner_scaled
)

ggplot(data, aes(x = reorder(strategy, lg.Spawners), y = lg.Spawners)) +
  geom_bar(
    stat = "identity",
    fill = "steelblue"
  ) +
  coord_flip() +
  labs(
    x = "Strategy",
    y = "Proportional score"
  ) +
  ggtitle("Number of Large Spawners") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )


#Now do # of holding
#Proportionally scored figure
holding_means <- tapply(all.decis$num.holding, all.decis$strategy, function(x) mean(x))

# Order strategies from highest to lowest lambda
order_idx <- order(holding_means, decreasing = FALSE)
holding_means <- holding_means[order_idx]
strategies <- names(holding_means)

# Proportionally scale lambda values from 0 to 1
holding_scaled <- (holding_means- min(holding_means))/
  (max(holding_means)-min(holding_means))

#dataframe for ggplot
data <- data.frame(
  strategy = strategies,
  holders = holding_scaled 
)

ggplot(data, aes(x = reorder(strategy, holders), y = holders)) +
  geom_bar(
    stat = "identity",
    fill = "steelblue"
  ) +
  coord_flip() +
  labs(
    x = "Strategy",
    y = "Proportional score"
  ) +
  ggtitle("Holding Adults") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )

#Now do # of rivers
#Proportionally scored figure
rivs_means <- tapply(all.decis$num.rivers, all.decis$strategy, function(x) mean(x))

# Order strategies from highest to lowest lambda
order_idx <- order(rivs_means, decreasing = FALSE)
rivs_means <- rivs_means[order_idx]
strategies <- names(rivs_means)

# Proportionally scale lambda values from 0 to 1
rivs_scaled <- (rivs_means- min(rivs_means))/
  (max(rivs_means)-min(rivs_means))

#dataframe for ggplot
data <- data.frame(
  strategy = strategies,
  rivs = rivs_scaled 
)

ggplot(data, aes(x = reorder(strategy, rivs), y = rivs)) +
  geom_bar(
    stat = "identity",
    fill = "steelblue"
  ) +
  coord_flip() +
  labs(
    x = "Strategy",
    y = "Proportional score"
  ) +
  ggtitle("Number of Rivers") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )

#Now do Rearing duration
#Proportionally scored figure
RearDur_means <- tapply(all.decis$avg_rearDur, all.decis$strategy, function(x) mean(x))

# Order strategies from highest to lowest lambda
order_idx <- order(RearDur_means, decreasing = FALSE)
RearDur_means <- RearDur_means[order_idx]
strategies <- names(RearDur_means)

# Proportionally scale lambda values from 0 to 1
RearDur_scaled <- (RearDur_means- min(RearDur_means))/
  (max(RearDur_means)-min(RearDur_means))

#dataframe for ggplot
data <- data.frame(
  strategy = strategies,
  RearDur = RearDur_scaled
)

ggplot(data, aes(x = reorder(strategy, RearDur), y = RearDur)) +
  geom_bar(
    stat = "identity",
    fill = "steelblue"
  ) +
  coord_flip() +
  labs(
    x = "Strategy",
    y = "Proportional score"
  ) +
  ggtitle("Average Rearing Duration") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )

#Now do spawn duration
#Proportionally scored figure
SpawnDur_means <- tapply(all.decis$avg_spawning, all.decis$strategy, function(x) mean(x))

# Order strategies from highest to lowest lambda
order_idx <- order(SpawnDur_means, decreasing = FALSE)
SpawnDur_means <- SpawnDur_means[order_idx]
strategies <- names(SpawnDur_means)

# Proportionally scale lambda values from 0 to 1
SpawnDur_scaled <- (SpawnDur_means- min(SpawnDur_means))/
  (max(SpawnDur_means)-min(SpawnDur_means))

#dataframe for ggplot
data <- data.frame(
  strategy = strategies,
  SpawnDur = SpawnDur_scaled
)

ggplot(data, aes(x = reorder(strategy, SpawnDur), y = SpawnDur)) +
  geom_bar(
    stat = "identity",
    fill = "steelblue"
  ) +
  coord_flip() +
  labs(
    x = "Strategy",
    y = "Proportional score"
  ) +
  ggtitle("Average Spawning Duration") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )

#Now do spawn duration
#Proportionally scored figure
recruit_means <- tapply(all.decis$recruit, all.decis$strategy, function(x) mean(x))

# Order strategies from highest to lowest lambda
order_idx <- order(recruit_means, decreasing = FALSE)
recruit_means <- recruit_means[order_idx]
strategies <- names(recruit_means)

# Proportionally scale lambda values from 0 to 1
recruit_scaled <- (recruit_means- min(recruit_means))/
  (max(recruit_means)-min(recruit_means))

#dataframe for ggplot
data <- data.frame(
  strategy = strategies,
  recruit= recruit_scaled
)

ggplot(data, aes(x = reorder(strategy, recruit), y = recruit)) +
  geom_bar(
    stat = "identity",
    fill = "steelblue"
  ) +
  coord_flip() +
  labs(
    x = "Strategy",
    y = "Proportional score"
  ) +
  ggtitle("Average Recruitment") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )
###############################################################
#####  For Indifference Curves
##### Create combined utility using weighting from AHP
##### i would only do this for the sturgeon strategies I think
###############################################################

require(lattice)
#Reload data in for calcs

Utilities
#First get the mean value for each strategy
mean_lambda 

#Mean recruit
mean_recruit 

#Mean spawners
mean_spawners 

#Mean large spawners
mean_large.spawners 

#Mean holding
mean_holding 

#Mean spawn duration
mean_avgSpawn 

#Mean num rivers
mean_rivers 

#Mean rearing duration
mean_rearDur



#For lambda
IC<-NULL
for(i in seq(0,1,by=0.05)){
  #i=0.5
  new.wt<-c(i,(1-i)*0.1428571,(1-i)*0.14285714,(1-i)*0.1428571,(1-i)*0.1428571,(1-i)*0.1428571,(1-i)*0.1428571,(1-i)*0.1428571)
  Utility<-mean_lambda$prop.score*new.wt[1]+ 
    mean_recruit$prop.score*new.wt[2]+
    mean_spawners$prop.score*new.wt[3]+
    mean_rivers$prop.score*new.wt[4]+
    mean_large.spawners$prop.score*new.wt[5]+
    mean_avgSpawn$prop.score*new.wt[6]+
    mean_rearDur$prop.score*new.wt[7]+
    mean_holding$prop.score *new.wt[8]
  
  IC<-rbind(IC,data.frame(Strategy=mean_lambda$strategy,Utility=Utility,inc=i))
}

# Create a custom color palette with distinct colors
distinct_colors <- c("blue3", "azure3", "cadetblue", "deeppink", 
                     "gold2", "darkorange", "firebrick4", "red2",
                     "magenta4", "forestgreen", "mediumorchid", "cyan3",
                     "seagreen2","olivedrab",'lightpink','burlywood3','gray3')
trellis.par.set(superpose.line = list(col = distinct_colors),
                superpose.symbol = list(col = distinct_colors))
#Now plot the expected utility for each decision across
# the range of weights
# note parameter values will be in percentile increments
xyplot(Utility~inc,groups = Strategy, type = c("a"), data = IC,
       xlab="Utility weight", ylab="Expected utility",
       main="Indifference curve for Population Growth",ylim=c(-0.1,1.1),
       auto.key=list(space="right", points=FALSE, lines=TRUE))

#For recruit
IC<-NULL
for(i in seq(0,1,by=0.05)){
  #i=0.5
  new.wt<-c(i,(1-i)*0.1428571,(1-i)*0.14285714,(1-i)*0.1428571,(1-i)*0.1428571,(1-i)*0.1428571,(1-i)*0.1428571,(1-i)*0.1428571)
  Utility<-mean_recruit$prop.score*new.wt[1]+ 
    mean_lambda$prop.score*new.wt[2]+
    mean_spawners$prop.score*new.wt[3]+
    mean_rivers$prop.score*new.wt[4]+
    mean_large.spawners$prop.score*new.wt[5]+
    mean_avgSpawn$prop.score*new.wt[6]+
    mean_rearDur$prop.score*new.wt[7]+
    mean_holding$prop.score *new.wt[8]
  
  IC<-rbind(IC,data.frame(Strategy=mean_recruit$strategy,Utility=Utility,inc=i))
}

# Create a custom color palette with distinct colors
distinct_colors <- c("blue3", "azure3", "cadetblue", "deeppink", 
                     "gold2", "darkorange", "firebrick4", "red2",
                     "magenta4", "forestgreen", "mediumorchid", "cyan3",
                     "seagreen2","olivedrab",'lightpink','burlywood3','gray3')
trellis.par.set(superpose.line = list(col = distinct_colors),
                superpose.symbol = list(col = distinct_colors))
#Now plot the expected utility for each decision across
# the range of weights
# note parameter values will be in percentile increments
xyplot(Utility~inc,groups = Strategy, type = c("a"), data = IC,
       xlab="Utility weight", ylab="Expected utility",
       main="Indifference curve for Recruitment",ylim=c(-0.1,1.1),
       auto.key=list(space="right", points=FALSE, lines=TRUE))

#For spawners
IC<-NULL
for(i in seq(0,1,by=0.05)){
  #i=0.5
  new.wt<-c(i,(1-i)*0.1428571,(1-i)*0.14285714,(1-i)*0.1428571,(1-i)*0.1428571,(1-i)*0.1428571,(1-i)*0.1428571,(1-i)*0.1428571)
  Utility<-mean_spawners$prop.score*new.wt[1]+ 
    mean_lambda$prop.score*new.wt[2]+
    mean_recruit$prop.score*new.wt[3]+
    mean_rivers$prop.score*new.wt[4]+
    mean_large.spawners$prop.score*new.wt[5]+
    mean_avgSpawn$prop.score*new.wt[6]+
    mean_rearDur$prop.score*new.wt[7]+
    mean_holding$prop.score *new.wt[8]
  
  IC<-rbind(IC,data.frame(Strategy=mean_spawners$strategy,Utility=Utility,inc=i))
}

# Create a custom color palette with distinct colors
distinct_colors <- c("blue3", "azure3", "cadetblue", "deeppink", 
                     "gold2", "darkorange", "firebrick4", "red2",
                     "magenta4", "forestgreen", "mediumorchid", "cyan3",
                     "seagreen2","olivedrab",'lightpink','burlywood3','gray3')
trellis.par.set(superpose.line = list(col = distinct_colors),
                superpose.symbol = list(col = distinct_colors))
#Now plot the expected utility for each decision across
# the range of weights
# note parameter values will be in percentile increments
xyplot(Utility~inc,groups = Strategy, type = c("a"), data = IC,
       xlab="Utility weight", ylab="Expected utility",
       main="Indifference curve for Spawners",ylim=c(-0.1,1.1),
       auto.key=list(space="right", points=FALSE, lines=TRUE))

#For num.rivers
IC<-NULL
for(i in seq(0,1,by=0.05)){
  #i=0.5
  new.wt<-c(i,(1-i)*0.1428571,(1-i)*0.14285714,(1-i)*0.1428571,(1-i)*0.1428571,(1-i)*0.1428571,(1-i)*0.1428571,(1-i)*0.1428571)
  Utility<-mean_rivers$prop.score*new.wt[1]+ 
    mean_lambda$prop.score*new.wt[2]+
    mean_recruit$prop.score*new.wt[3]+
    mean_spawners$prop.score*new.wt[4]+
    mean_large.spawners$prop.score*new.wt[5]+
    mean_avgSpawn$prop.score*new.wt[6]+
    mean_rearDur$prop.score*new.wt[7]+
    mean_holding$prop.score *new.wt[8]
  
  IC<-rbind(IC,data.frame(Strategy=mean_rivers$strategy,Utility=Utility,inc=i))
}

# Create a custom color palette with distinct colors
distinct_colors <- c("blue3", "azure3", "cadetblue", "deeppink", 
                     "gold2", "darkorange", "firebrick4", "red2",
                     "magenta4", "forestgreen", "mediumorchid", "cyan3",
                     "seagreen2","olivedrab",'lightpink','burlywood3','gray3')
trellis.par.set(superpose.line = list(col = distinct_colors),
                superpose.symbol = list(col = distinct_colors))
#Now plot the expected utility for each decision across
# the range of weights
# note parameter values will be in percentile increments
xyplot(Utility~inc,groups = Strategy, type = c("a"), data = IC,
       xlab="Utility weight", ylab="Expected utility",
       main="Indifference curve for number of Rivers",ylim=c(-0.1,1.1),
       auto.key=list(space="right", points=FALSE, lines=TRUE))


#For large spawners
IC<-NULL
for(i in seq(0,1,by=0.05)){
  #i=0.5
  new.wt<-c(i,(1-i)*0.1428571,(1-i)*0.14285714,(1-i)*0.1428571,(1-i)*0.1428571,(1-i)*0.1428571,(1-i)*0.1428571,(1-i)*0.1428571)
  Utility<-mean_large.spawners$prop.score*new.wt[1]+ 
    mean_lambda$prop.score*new.wt[2]+
    mean_recruit$prop.score*new.wt[3]+
    mean_spawners$prop.score*new.wt[4]+
    mean_rivers$prop.score*new.wt[5]+
    mean_avgSpawn$prop.score*new.wt[6]+
    mean_rearDur$prop.score*new.wt[7]+
    mean_holding$prop.score *new.wt[8]
  
  IC<-rbind(IC,data.frame(Strategy=mean_large.spawners$strategy,Utility=Utility,inc=i))
}

# Create a custom color palette with distinct colors
distinct_colors <- c("blue3", "azure3", "cadetblue", "deeppink", 
                     "gold2", "darkorange", "firebrick4", "red2",
                     "magenta4", "forestgreen", "mediumorchid", "cyan3",
                     "seagreen2","olivedrab",'lightpink','burlywood3','gray3')
trellis.par.set(superpose.line = list(col = distinct_colors),
                superpose.symbol = list(col = distinct_colors))
#Now plot the expected utility for each decision across
# the range of weights
# note parameter values will be in percentile increments
xyplot(Utility~inc,groups = Strategy, type = c("a"), data = IC,
       xlab="Utility weight", ylab="Expected utility",
       main="Indifference curve for Large Spawners",ylim=c(-0.1,1.1),
       auto.key=list(space="right", points=FALSE, lines=TRUE))

#For spawning window
IC<-NULL
for(i in seq(0,1,by=0.05)){
  #i=0.5
  new.wt<-c(i,(1-i)*0.1428571,(1-i)*0.14285714,(1-i)*0.1428571,(1-i)*0.1428571,(1-i)*0.1428571,(1-i)*0.1428571,(1-i)*0.1428571)
  Utility<-all.decis$avg_spawning*new.wt[1]+ all.decis$llambda*new.wt[2]+
    all.decis$recruit*new.wt[3]+all.decis$spawners*new.wt[4]+all.decis$num.rivers*new.wt[5]+
    all.decis$large.spawners*new.wt[6]+all.decis$avg_rearDur*new.wt[7]+all.decis$num.holding*new.wt[8]
  
  IC<-rbind(IC,data.frame(Strategy=all.decis$strategy,Utility=Utility,inc=i))
}

#Now plot the expected utility for each decision across
# the range of weights
# note parameter values will be in percentile increments
xyplot(Utility~inc,groups = Strategy, type = c("a"), data = IC,
       xlab="Utility weight", ylab="Expected utility",
       main="Indifference curve for spawning window", ylim=c(0.3,0.45))

#For rearing window avg_rearDur
IC<-NULL
for(i in seq(0,1,by=0.05)){
  #i=0.5
  new.wt<-c(i,(1-i)*0.1428571,(1-i)*0.14285714,(1-i)*0.1428571,(1-i)*0.1428571,(1-i)*0.1428571,(1-i)*0.1428571,(1-i)*0.1428571)
  Utility<-all.decis$avg_rearDur*new.wt[1]+ all.decis$llambda*new.wt[2]+
    all.decis$recruit*new.wt[3]+all.decis$spawners*new.wt[4]+all.decis$num.rivers*new.wt[5]+
    all.decis$large.spawners*new.wt[6]+all.decis$avg_spawning*new.wt[7]+all.decis$num.holding*new.wt[8]
  
  IC<-rbind(IC,data.frame(Strategy=all.decis$strategy,Utility=Utility,inc=i))
}

#Now plot the expected utility for each decision across
# the range of weights
# note parameter values will be in percentile increments
xyplot(Utility~inc,groups = Strategy, type = c("a"), data = IC,
       xlab="Utility weight", ylab="Expected utility",
       main="Indifference curve for rearing window", ylim=c(0.3,0.45))


#For number holding adults
IC<-NULL
for(i in seq(0,1,by=0.05)){
  #i=0.5
  new.wt<-c(i,(1-i)*0.1428571,(1-i)*0.14285714,(1-i)*0.1428571,(1-i)*0.1428571,(1-i)*0.1428571,(1-i)*0.1428571,(1-i)*0.1428571)
  Utility<-all.decis$num.holding*new.wt[1]+ all.decis$llambda*new.wt[2]+
    all.decis$recruit*new.wt[3]+all.decis$spawners*new.wt[4]+all.decis$num.rivers*new.wt[5]+
    all.decis$large.spawners*new.wt[6]+all.decis$avg_spawning*new.wt[7]+all.decis$avg_rearDur*new.wt[8]
  
  IC<-rbind(IC,data.frame(Strategy=all.decis$strategy,Utility=Utility,inc=i))
}

#Now plot the expected utility for each decision across
# the range of weights
# note parameter values will be in percentile increments
xyplot(Utility~inc,groups = Strategy, type = c("a"), data = IC,
       xlab="Utility weight", ylab="Expected utility",
       main="Indifference curve for # holders", ylim=c(0.3,0.75),
       auto.key=list(space="right", points=FALSE, lines=TRUE))


