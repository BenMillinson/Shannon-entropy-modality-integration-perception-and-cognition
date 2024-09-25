# This script shows the effectiveness of modality integration equations,
# to predict integration of modalities from individual modalities.
# The example given here is audio-visual entropy from auditory and visual modalities respectively.
# Data has been simulated and these methods (to my knowledge) have not been tested prior,
# except if they have been referenced.
# Therefore it is wise to take this code with a pinch of salt.
# Besson, et al., 2010
# https://doi.org/10.1007/s00422-010-0392-8


# Load necessary libraries
library(dplyr)
library(ggplot2)
library(tidyr)

# Set seed for reproducibility
set.seed(123)

# Number of trials/participants
N <- 100

# Simulate entropy levels for audio and visual stimuli
audio_entropy <- rnorm(N, mean = 1, sd = 0.5)  # Entropy values for audio stimuli
visual_entropy <- rnorm(N, mean = 1, sd = 0.5)  # Entropy values for visual stimuli

# Calculate actual AV entropy (for the purpose of simulation)
actual_av_entropy <- sqrt(-audio_entropy * log2(audio_entropy + 1e-10) - visual_entropy * log2(visual_entropy + 1e-10)) + 
  rnorm(N, mean = 0, sd = 0.05)  # Adding small noise

# Combine into a data frame
data <- data.frame(audio_entropy, visual_entropy, actual_av_entropy)

# Shannon's entropy function
shannon_entropy <- function(p) {
  p <- p[p > 0]  # Remove zero probabilities to avoid log(0)
  -sum(p * log2(p + 1e-10))  # Add a small number to avoid log(0)
}


# Additive Approach
data <- data %>%
  mutate(additive_av_entropy = audio_entropy + visual_entropy)


# Joint Entropy Approach (assuming independence for this example)
data <- data %>%
  mutate(joint_av_entropy = -log2(2^-audio_entropy * 2^-visual_entropy))


# Combined AV Entropy
data <- data %>%
  mutate(combined_av_entropy = sqrt(-audio_entropy * log2(audio_entropy + 1e-10) - visual_entropy * log2(visual_entropy + 1e-10)))


# Calculate Mutual Information I(X; Y)
#(Besson, et al., 2010)
H_x = shannon_entropy(audio_entropy)
H_y = shannon_entropy(visual_entropy)
H_xy = shannon_entropy(cbind(audio_entropy, visual_entropy))
mutual_info_av_entropy = H_x + H_y - H_xy


# Conditional entropy (Besson, et al., 2010)
data <- data %>%
  mutate(
    joint_av_entropy = -log2(2^-audio_entropy * 2^-visual_entropy),  # Joint entropy calculation
    given_entropy = shannon_entropy(visual_entropy),                  # Given entropy calculation
    conditional_entropy_av = joint_av_entropy - given_entropy         # Conditional entropy calculation
  )


# Calculate Conditional Mutual Information I(X; Y | Z)
# Here, we will treat the actual AV entropy as a third variable Z
#(Besson, et al., 2010)
H_X_given_Z = shannon_entropy(audio_entropy) - 0.1  # Placeholder adjustments
H_Y_given_Z = shannon_entropy(visual_entropy) - 0.1  # Placeholder adjustments
H_XY_given_Z = shannon_entropy(cbind(audio_entropy, visual_entropy)) - 0.1  # Placeholder adjustments
conditional_mutual_info = H_X_given_Z + H_Y_given_Z - H_XY_given_Z


# Add results to data frame
data <- data %>%
  mutate(mutual_info_av_entropy = mutual_info_av_entropy, 
         conditional_mutual_info = conditional_mutual_info,
         normalized_mutual_info = normalized_mutual_info)


# View the data
head(data)


# Prepare data for plotting
data_long <- data %>%
  gather(key = "approach", value = "predicted_av_entropy", 
         additive_av_entropy, joint_av_entropy, 
         mutual_info_av_entropy, combined_av_entropy,
         conditional_entropy_av)


# Plotting actual vs predicted AV entropy for all approaches
ggplot(data_long, aes(x = actual_av_entropy, y = predicted_av_entropy, color = approach)) +
  geom_point(alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  facet_wrap(~ approach) +
  labs(title = "Comparison of Actual AV Entropy vs Predicted by Different Models",
       x = "Actual AV Entropy",
       y = "Predicted AV Entropy",
       color = "Approach") +
  theme_minimal()

