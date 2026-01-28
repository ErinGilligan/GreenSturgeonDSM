
all.decis<-read.csv("C:/Users/gilliger/Box/Erin Dissertation/SIT sturgeon models/Green Sturgeon Model code/Results/AllTaxaStrategyResultsFinalized.csv")
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
png("C:/Users/gilliger/Box/Erin Dissertation/SIT sturgeon models/Green Sturgeon Model code/Results/correlation_plotALLTAXA.png", 
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
#write.csv(Utilities,file="AllTAxaUtilitiesFINALIZED.csv")




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

#Now do # of large spawners
#Proportionally scored figure
lg.spawner_means <- tapply(all.decis$large.spawners, all.decis$strategy, function(x) mean(x))

# Order strategies from highest to lowest lambda
order_idx <- order(lg.spawner_means, decreasing = FALSE)
lg.spawner_means <- lg.spawner_means[order_idx]
strategies <- names(lg.spawner_means)

# Proportionally scale lambda values from 0 to 1
lg.spawners_scaled <- (lg.spawner_means- min(lg.spawner_means))/
  (max(lg.spawner_means)-min(lg.spawner_means))

#dataframe for ggplot
data <- data.frame(
  strategy = strategies,
  lg.spawners = lg.spawners_scaled
)

ggplot(data, aes(x = reorder(strategy, lg.spawners), y = lg.spawners)) +
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

#Now do spawning duration
#Proportionally scored figure
avg_spawning_means <- tapply(all.decis$avg_spawning, all.decis$strategy, function(x) mean(x))

# Order strategies from highest to lowest lambda
order_idx <- order(avg_spawning_means, decreasing = FALSE)
avg_spawning_means <- avg_spawning_means[order_idx]
strategies <- names(avg_spawning_means)

# Proportionally scale lambda values from 0 to 1
avg_spawning_scaled <- (avg_spawning_means- min(avg_spawning_means))/
  (max(avg_spawning_means)-min(avg_spawning_means))

#dataframe for ggplot
data <- data.frame(
  strategy = strategies,
  avg_spawning = avg_spawning_scaled
)

ggplot(data, aes(x = reorder(strategy, avg_spawning), y = avg_spawning)) +
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


#Now do rearing duration
#Proportionally scored figure
avg_rearDur_means <- tapply(all.decis$avg_rearDur, all.decis$strategy, function(x) mean(x))

# Order strategies from highest to lowest lambda
order_idx <- order(avg_rearDur_means, decreasing = FALSE)
avg_rearDur_means <- avg_rearDur_means[order_idx]
strategies <- names(avg_rearDur_means)

# Proportionally scale lambda values from 0 to 1
avg_rearDur_scaled <- (avg_rearDur_means- min(avg_rearDur_means))/
  (max(avg_rearDur_means)-min(avg_rearDur_means))

#dataframe for ggplot
data <- data.frame(
  strategy = strategies,
  avg_rearDur = avg_rearDur_scaled
)

ggplot(data, aes(x = reorder(strategy, avg_rearDur), y = avg_rearDur)) +
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
