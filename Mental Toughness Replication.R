Replication of Main Findings from Mental Toughness Study

#Step One replicate meta-analysis from 16 stidues

library(metafor)

# Create a data frame with your data
df <- data.frame(
  Study = c("Slimani et al., 2016", "Jones & Parker, 2019", "Cowden, 2016", "Meggs et al., 2019", "Morais & Gomes, 2019", "Brace et al., 2020", "Mahoney et al., 2014", "Newland et al., 2013", "Christensen et al., 2018", "Mendez-Alonso et al., 2021", "Hagag & Ali, 2014", "Sheard, 2009", "Beattie et al. 2017", "Madrigal et al., 2013", "Cowden et al, 2014", "Zaric et al., 2021"),
  Correlation = c(0.85, 0.439, 0.225, 0.62, 0.474, 0.255, 0.21, 0.12, 0.3, 0.22, 0.52, 0.402, 0.067, -0.08, 0.79, 0.37),
  Lower_Limit = c(0.712, 0.345, -0.081, 0.492, -0.176, -0.008, 0.080, -0.02, 0.142, 0.12, 0.145, 0.157, -0.148, -0.241, 0.484, 0.057),
  Upper_Limit = c(0.925, 0.524, 0.492, 0.722, 0.836, 0.485, 0.333, 0.256, 0.443, 0.315, 0.764, 0.601, 0.276, 0.085, 0.924, 0.617)
)

# Perform the meta-analysis using the method of moments
meta_analysis <- rma.uni(yi = df$Correlation, sei = ((df$Upper_Limit - df$Lower_Limit) / (2 * 1.96)), method="ML")

# View summary results
summary(meta_analysis)

# Create a forest plot
forest(meta_analysis)

# Create a funnel plot
funnel(meta_analysis)

#Example Code to replicate data for variables such as age, sport category etc.

# Assuming you have collected and merged the variable data into your df data frame lets use "age" as an example

# Perform the meta-analysis with "age" as moderators
meta_analysis <- rma.uni(yi = df$Correlation, sei = ((df$Upper_Limit - df$Lower_Limit) / (2 * 1.96)), method = "ML", mods = ~ age, data = df)

# View summary results
summary(meta_analysis)

#How to preform a sensitivity analysis

# Perform the sensitivity analysis
sensitivity_results <- list()

for (i in 1:nrow(df)) {
  # Exclude the i-th study from the analysis
  df_subset <- df[-i, ]
  
  # Perform the meta-analysis
  meta_analysis <- rma.uni(yi = df_subset$Correlation, sei = ((df_subset$Upper_Limit - df_subset$Lower_Limit) / (2 * 1.96)), method = "ML")
  
  # Store the results in the sensitivity_results list
  sensitivity_results[[i]] <- summary(meta_analysis)
}

# View the sensitivity analysis results
for (i in 1:length(sensitivity_results)) {
  cat("Excluding study", df$Study[i], "results in:\n")
  print(sensitivity_results[[i]])
  cat("\n")
}
#This code performs a sensitivity analysis by iteratively excluding one study at a time and then displaying the results for each scenario. You can examine the changes in the effect size and other summary statistics to assess the impact of each study's removal on the overall meta-analysis results.

#Test for publication bias using Egger test

# Create a funnel plot
funnel(meta_analysis)

# Perform Egger's test for publication bias
egger_test <- regtest(meta_analysis)
cat("Egger's test for publication bias:\n")
print(egger_test)

# Calculate the Fail-Safe Number and Z-value
fail_safe_number <- function(es, se, alpha = 0.05) {
  z_alpha <- qnorm(1 - alpha / 2)
  fail_safe <- ((z_alpha * se)^2 - es^2) / (2 * es^2)
  z_value <- sqrt(fail_safe / (nrow(df) - 3))  # Degrees of freedom: k-3 (k is the number of studies)
  return(list(Fail_Safe = fail_safe, Z_Value = z_value))
}

# Calculate the Fail-Safe Number and Z-value for your meta-analysis
es <- 0.3585
se <- 0.0647
fail_safe_result <- fail_safe_number(es, se)

# Calculate the number of missing studies to change the p-value
alpha <- 0.05
critical_z <- qnorm(1 - alpha / 2)
critical_z_sq <- critical_z^2
missing_studies <- ((critical_z_sq * (nrow(df) - 3)) + es^2) / (critical_z_sq * (se^2)) - nrow(df)

# Print the results
cat("Fail-Safe Number:", round(fail_safe_result$Fail_Safe, 2), "\n")
cat("Z-Value:", round(fail_safe_result$Z_Value, 2), "\n")
cat("The number of missing studies that would bring the p-value to >", alpha, "to overturn the conclusion of the positive effect of the study:", round(missing_studies), "\n")

#Extensions

# Sensitivity analysis with different correlation coefficient thresholds
thresholds <- c(0.3, 0.4, 0.5) # Replace with desired thresholds

for (threshold in thresholds) {
  subset_data <- df[df$Correlation >= threshold, ]
  meta_result <- rma(yi = df$Correlation, sei = ((df$Upper_Limit - df$Lower_Limit) / (2 * 1.96)), method = "ML")
  print(paste(0.5, threshold))
  print(summary(meta_result))
  forest(meta_result)
}



# Threshold-free publication bias analysis
library(metafor)
meta_result <- rma(yi = df$Correlation, sei = ((df$Upper_Limit - df$Lower_Limit) / (2 * 1.96)), method = "ML")
trim_and_fill_result <- trimfill(meta_result)
summary(trim_and_fill_result)
forest(trim_and_fill_result)




