library(ggplot2)
library(scales)

# Put in the data
observed <- c(1730, 1449, 1298, 2432, 2403, 1471, 960)  # Target values from Dudley et al. 2024
predicted <- c(2033, 1622, 1315, 2278, 2133, 1217, 910)  # My best calibration results
life_stages <- c("Age0", "Age1", "Age2", "Juvenile", "Subadult", "SmallAdult", "LargeAdult")

# Put everything into a dataframe
cal_data <- data.frame(
  LifeStage = factor(life_stages, levels = life_stages),
  Observed = observed,
  Predicted = predicted
)

# Calc R-squared and RMSE
correlation <- cor(observed, predicted)
r_squared <- correlation^2
rmse <- sqrt(mean((observed - predicted)^2))

# color palette for plotting
grayscale_colors <- c("Observed" = "gray30",      
                      "Predicted" = "gray70")     

# Figure 2 for Manuscript
p1 <- ggplot(cal_data, aes(x = LifeStage)) +
  geom_col(aes(y = Observed, fill = "Observed"), 
           position = position_dodge(width = 0.8), 
           width = 0.4, alpha = 0.8,
           color = "black", size = 0.3) +  # Add black outline
  geom_col(aes(y = Predicted, fill = "Predicted"), 
           position = position_dodge(width = 0.8), 
           width = 0.4, alpha = 0.8,
           color = "black", size = 0.3) +  # Add black outline
  scale_fill_manual(values = grayscale_colors) +
  labs(
    title = "Green Sturgeon Population Model Calibration Results",
    subtitle = paste0("R² = ", round(r_squared, 3), " | RMSE = ", round(rmse, 0)),
    x = "Life Stage",
    y = "Number of Fish",
    fill = "Data Type"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.text.y = element_text(size = 10),
    axis.title = element_text(size = 12, face = "bold"),
    legend.position = "top",
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 11, face = "bold"),
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12),
    panel.grid.major = element_line(color = "gray90", size = 0.3),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 0.5)
  ) +
  scale_y_continuous(labels = comma_format(), expand = expansion(mult = c(0, 0.05)))

print(p1)

# Observed vs predicted scatter plot (Not sure if I'll use this one yet)
p2 <- ggplot(cal_data, aes(x = Observed, y = Predicted)) +
  geom_point(size = 3, color = "gray30", alpha = 0.8, 
             stroke = 0.5, fill = "gray70", shape = 21) +  # Use filled circles
  geom_smooth(method = "lm", se = TRUE, color = "gray10", 
              fill = "gray80", linetype = "dashed", size = 0.8) +
  geom_abline(slope = 1, intercept = 0, color = "black", 
              linetype = "solid", size = 0.8) +
  annotate("text", x = max(observed) * 0.95, y = max(predicted) * 0.3, 
           label = paste0("R² = ", round(r_squared, 3)), 
           size = 5, fontface = "bold") +
  labs(
    title = "Observed vs Predicted Values",
    x = "Observed (Target Values)",
    y = "Predicted (Model Output)",
    caption = "Dashed line = fitted relationship\nSolid line = perfect 1:1 relationship"
  ) +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12, face = "bold"),
    plot.title = element_text(size = 14, face = "bold"),
    plot.caption = element_text(size = 9, hjust = 0),
    panel.grid.major = element_line(color = "gray90", size = 0.3),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 0.5)
  ) +
  scale_x_continuous(labels = comma_format(), expand = expansion(mult = c(0.05, 0.08))) +
  scale_y_continuous(labels = comma_format())


print(p2)

# Print summary statistics
cat("=== CALIBRATION SUMMARY ===\n")
cat("R-squared:", round(r_squared, 3), "\n")
cat("Correlation:", round(correlation, 3), "\n")
cat("RMSE:", round(rmse, 0), "\n")
cat("Mean Absolute Error:", round(mean(abs(observed - predicted)), 0), "\n")
cat("Mean Percent Error:", round(mean(abs((predicted - observed)/observed) * 100), 1), "%\n")

# Create summary table for Supplement
cal_data$Difference <- predicted - observed
cal_data$Percent_Error <- round((cal_data$Difference / cal_data$Observed) * 100, 1)
print(cal_data)
