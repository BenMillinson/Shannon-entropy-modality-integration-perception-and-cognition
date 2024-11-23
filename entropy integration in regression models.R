# This R script aims to provide a basis of entropy integration as a predictor for,
# a cognitive function
# entropy integration formula are provided in the "bi-modal entropy integration" script
# Functions are provided here

# Load necessary libraries
library(dplyr)
library(tidyr)
library(ggplot2)

set.seed(123)  # For reproducibility

# Number of trials/participants
N <- 100

# Simulate entropy levels for audio and visual stimuli
audio_entropy <- rnorm(N, mean = 1, sd = 0.5)  # Entropy values for audio stimuli
visual_entropy <- rnorm(N, mean = 1, sd = 0.5) # Entropy values for visual stimuli

# Simulate recall accuracy (higher entropy might lead to lower accuracy)
recall_accuracy <- 0.8 - 0.2 * audio_entropy - 0.2 * visual_entropy + rnorm(N, mean = 0, sd = 0.05)

# Cap recall accuracy between 0 and 1
recall_accuracy <- pmax(0, pmin(1, recall_accuracy))

# Create a data frame
data <- data.frame(
  audio_entropy = audio_entropy,
  visual_entropy = visual_entropy,
  recall_accuracy = recall_accuracy
)

# Shannon's entropy function
shannon_entropy <- function(p) {
  -sum(p * log2(p + 1e-10))  # Add a small number to avoid log(0)
}

# Function to calculate the combined AV entropy using a square root formula
combined_av_entropy <- function(audio, visual) {
  sqrt((-audio * log2(audio + 1e-10)) + (-visual * log2(visual + 1e-10)))
}

# Function to calculate the additive AV entropy
additive_av_entropy <- function(audio, visual) {
  audio + visual
}

# Function to calculate joint entropy assuming independence
joint_av_entropy <- function(audio, visual) {
  -log2(2^-audio * 2^-visual)
}

# Function to calculate mutual information entropy
mutual_info_av_entropy <- function(audio, visual, actual_av) {
  audio + visual - 0.5 * actual_av
}

# Calculate actual AV entropy (for the purpose of simulation)
actual_av_entropy <- 0.5 * audio_entropy + 0.5 * visual_entropy + rnorm(N, mean = 0, sd = 0.05)

# Compute different entropy values
data <- data %>%
  mutate(
    combined_av_entropy = combined_av_entropy(audio_entropy, visual_entropy),
    additive_av_entropy = additive_av_entropy(audio_entropy, visual_entropy),
    joint_av_entropy = joint_av_entropy(audio_entropy, visual_entropy),
    mutual_info_av_entropy = mutual_info_av_entropy(audio_entropy, visual_entropy, actual_av_entropy),
    actual_av_entropy = actual_av_entropy  # Storing for plotting purposes
  )

# View the data
head(data)

# Regression models for each entropy approach
summary(lm(recall_accuracy ~ audio_entropy + visual_entropy, data = data))  # Basic model
summary(lm(recall_accuracy ~ combined_av_entropy, data = data))             # Combined AV entropy
summary(lm(recall_accuracy ~ additive_av_entropy, data = data))             # Additive AV entropy
summary(lm(recall_accuracy ~ joint_av_entropy, data = data))                # Joint AV entropy
summary(lm(recall_accuracy ~ mutual_info_av_entropy, data = data))          # Mutual Information entropy

# Reshape the data to long format for easier plotting with ggplot2
data_long <- tidyr::pivot_longer(data, cols = c(combined_av_entropy, additive_av_entropy, joint_av_entropy, mutual_info_av_entropy, actual_av_entropy),
                                 names_to = "entropy_type", values_to = "entropy_value")

# Plot the comparison of different entropy calculations
ggplot(data_long, aes(x = entropy_value, y = recall_accuracy, color = entropy_type)) +
  geom_point(size = 2, alpha = 0.7) +  # Scatter plot
  geom_smooth(method = "lm", se = FALSE) +  # Regression lines for each type
  labs(title = "Recall Accuracy vs Various AV Entropy Measures",
       x = "Entropy",
       y = "Recall Accuracy",
       color = "Entropy Type") +
  scale_color_manual(values = c(
    "combined_av_entropy" = "green",
    "additive_av_entropy" = "blue",
    "joint_av_entropy" = "red",
    "mutual_info_av_entropy" = "purple",
    "actual_av_entropy" = "black"
  )) +  # Color mapping for different entropy measures
  theme_minimal()


# redone with non-autocorrelative data ------------------------------------

# This R script aims to provide a basis of entropy integration as a predictor for,
# a cognitive function. The entropy integration formula is provided in the "bi-modal entropy integration" script
# Functions are provided here

# Load necessary libraries
library(dplyr)
library(tidyr)
library(ggplot2)

set.seed(123)  # For reproducibility

# Number of trials/participants
N <- 100

# Simulate entropy levels for audio and visual stimuli
audio_entropy <- rnorm(N, mean = 1, sd = 0.5)  # Entropy values for audio stimuli
visual_entropy <- rnorm(N, mean = 1, sd = 0.5) # Entropy values for visual stimuli

# Simulate recall accuracy without using entropy values directly
# Here, recall accuracy is randomly generated within a specified range
recall_accuracy <- rnorm(N, mean = 0.75, sd = 0.05)  # Random values between 0 and 1
recall_accuracy <- pmax(0, pmin(1, recall_accuracy))  # Cap recall accuracy between 0 and 1

# Create a data frame
data <- data.frame(
  audio_entropy = audio_entropy,
  visual_entropy = visual_entropy,
  recall_accuracy = recall_accuracy
)

# Shannon's entropy function
shannon_entropy <- function(p) {
  -sum(p * log2(p + 1e-10))  # Add a small number to avoid log(0)
}

# Function to calculate the combined AV entropy using a square root formula
combined_av_entropy <- function(audio, visual) {
  sqrt((-audio * log2(audio + 1e-10)) + (-visual * log2(visual + 1e-10)))
}

# Function to calculate the additive AV entropy
additive_av_entropy <- function(audio, visual) {
  audio + visual
}

# Function to calculate joint entropy assuming independence
joint_av_entropy <- function(audio, visual) {
  -log2(2^-audio * 2^-visual)
}

# Function to calculate mutual information entropy
mutual_info_av_entropy <- function(audio, visual) {
  audio + visual - 0.5 * (audio + visual)  # Using a different logic to avoid autocorrelation
}

# Modify how actual AV entropy is calculated to avoid direct dependence on entropy variables
# We can generate it randomly or use a different function
actual_av_entropy <- rnorm(N, mean = 1, sd = 0.5)  # Randomized actual AV entropy (no dependency on audio or visual entropy)

# Compute different entropy values
data <- data %>%
  mutate(
    combined_av_entropy = combined_av_entropy(audio_entropy, visual_entropy),
    additive_av_entropy = additive_av_entropy(audio_entropy, visual_entropy),
    joint_av_entropy = joint_av_entropy(audio_entropy, visual_entropy),
    mutual_info_av_entropy = mutual_info_av_entropy(audio_entropy, visual_entropy),
    actual_av_entropy = actual_av_entropy  # Storing for plotting purposes
  )

# View the data
head(data)

# Regression models for each entropy approach
summary(lm(recall_accuracy ~ audio_entropy + visual_entropy, data = data))  # Basic model
summary(lm(recall_accuracy ~ combined_av_entropy, data = data))             # Combined AV entropy
summary(lm(recall_accuracy ~ additive_av_entropy, data = data))             # Additive AV entropy
summary(lm(recall_accuracy ~ joint_av_entropy, data = data))                # Joint AV entropy
summary(lm(recall_accuracy ~ mutual_info_av_entropy, data = data))          # Mutual Information entropy

# Reshape the data to long format for easier plotting with ggplot2
data_long <- tidyr::pivot_longer(data, cols = c(combined_av_entropy, additive_av_entropy, joint_av_entropy, mutual_info_av_entropy, actual_av_entropy),
                                 names_to = "entropy_type", values_to = "entropy_value")

# Plot the comparison of different entropy calculations
ggplot(data_long, aes(x = entropy_value, y = recall_accuracy, color = entropy_type)) +
  geom_point(size = 2, alpha = 0.7) +  # Scatter plot
  geom_smooth(method = "lm", se = FALSE) +  # Regression lines for each type
  labs(title = "Recall Accuracy vs Various AV Entropy Measures",
       x = "Entropy",
       y = "Recall Accuracy",
       color = "Entropy Type") +
  scale_color_manual(values = c(
    "combined_av_entropy" = "green",
    "additive_av_entropy" = "blue",
    "joint_av_entropy" = "red",
    "mutual_info_av_entropy" = "purple",
    "actual_av_entropy" = "black"
  )) +  # Color mapping for different entropy measures
  theme_minimal()

