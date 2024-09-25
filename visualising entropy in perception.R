# This R script aims to show how perceptual entropy of modalities changes as,
# the amount of time exposed to a stimulus.
# For the sake of explanation, auditory and visual modalities are chosen,
# yet these can be adapted for other sensory modalities


# Load necessary library for plotting
library(ggplot2)

# Shannon's entropy function
shannon_entropy <- function(p) {
  -sum(p * log2(p + 1e-10) )  # Add a small number to avoid log(0)
}

# Generating 75 time points using seq()
time_points <- seq(1, 75, by = 0.25)

# Creating probability distributions that change gradually
p_audio_list <- lapply(time_points, function(t) {
  c(0.2 + 0.01 * (t - 1), 0.5 - 0.01 * (t - 1), 0.3)
})

p_visual_list <- lapply(time_points, function(t) {
  c(0.4 - 0.01 * (t - 1), 0.4 + 0.01 * (t - 1), 0.2)
})

# Calculate entropy for each time point
entropy_audio <- sapply(p_audio_list, shannon_entropy)
entropy_visual <- sapply(p_visual_list, shannon_entropy)

# Combine entropy by averaging audio and visual entropy
combined_entropy <- (entropy_audio + entropy_visual) / 2  # You can also use weighted averages if needed

# Create a data frame for plotting
entropy_df <- data.frame(
  Time = time_points,
  Entropy_Audio = entropy_audio,
  Entropy_Visual = entropy_visual,
  Combined_Entropy = combined_entropy
)

# Reshape the data for ggplot
library(reshape2)
entropy_long <- melt(entropy_df, id.vars = "Time", variable.name = "Type", value.name = "Entropy")

# Plotting the entropy lines
ggplot(entropy_long, aes(x = Time, y = Entropy, color = Type)) +
  geom_line(size = 1) +  # Lines for each entropy type
  geom_point(size = 2) +  # Points for each observation
  labs(title = "Shannon Entropy Over Time",
       x = "Time",
       y = "Entropy (bits)",
       color = "Entropy Type") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "green", "red"))  # Custom colors for different entropy types

