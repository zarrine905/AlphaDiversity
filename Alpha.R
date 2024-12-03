# Load necessary libraries
library(vegan)
library(ggplot2)

# Read abundance data (replace 'data.csv' with the actual file path)
# Ensure the first row contains subject names and the first column contains organism names
abundance_data <- read.csv("data.csv", row.names = 1, check.names = FALSE)

# Read organism grouping information (replace 'groupings.csv' with actual file path)
groupings <- read.csv("groupings.csv")  # Replace with your file containing Baseline/Treated groups
colnames(groupings) <- c("Organism", "SampleType")  # Ensure column names are correct

# Split data by group (Baseline and Treated)
baseline_organisms <- groupings[groupings$SampleType == "Baseline", "Organism"]
treated_organisms <- groupings[groupings$SampleType == "Treated", "Organism"]

baseline_data <- abundance_data[rownames(abundance_data) %in% baseline_organisms, ]
treated_data <- abundance_data[rownames(abundance_data) %in% treated_organisms, ]

# Calculate Shannon diversity for each subject
shannon_baseline <- diversity(t(baseline_data), index = "shannon")
shannon_treated <- diversity(t(treated_data), index = "shannon")

# Combine results into a single data frame
results <- data.frame(
  Subject = colnames(abundance_data),
  Baseline_Shannon = shannon_baseline,
  Treated_Shannon = shannon_treated
)

# Convert results to long format for plotting
library(reshape2)
results_long <- melt(results, id.vars = "Subject", 
                     variable.name = "Group", 
                     value.name = "Shannon")

# Create the boxplot
ggplot(results_long, aes(x = Group, y = Shannon, fill = Group)) +
  geom_boxplot() +
  labs(title = "Shannon Diversity Index by Group (Baseline vs Treated)",
       x = "Group", y = "Shannon Diversity Index") +
  theme_minimal() +
  scale_fill_manual(values = c("Baseline_Shannon" = "skyblue", "Treated_Shannon" = "salmon")) +
  theme(legend.position = "none")
