
# notes -------------------------------------------------------------------

# 1. Simulating a Dataset
#Purpose: Create a synthetic dataset to mimic a perceptual experiment with binary outcomes for audio and visual trials.
#Steps:
#Generate n_trials = 100 trials where each trial has:
#audio_outcome: Correct (1) or incorrect (0), with a 60% probability of being correct.
#visual_outcome: Correct (1) or incorrect (0), with a 90% probability of being correct.
#Combine into a dataframe, df, where each row represents a trial.
  
# 2. Calculating Joint and Marginal Probabilities
#Purpose: Compute joint probabilities (Audio, Visual combinations) and marginal probabilities for Audio and Visual individually.
#Steps:
#Joint Counts: Count occurrences of each combination of (audio, visual):
#(0, 0): Both incorrect.
#(0, 1): Audio incorrect, visual correct.
#(1, 0): Audio correct, visual incorrect.
#(1, 1): Both correct.
#Joint Probabilities: Normalize the counts by dividing by the total number of trials.
#Probability Matrix: Construct a 2×22×2 matrix (joint_prob) representing the joint distribution of Audio and Visual.
  
# 3. Joint Entropy: H(X,Y)H(X,Y)
# Purpose: Calculate the total uncertainty in the combined Audio and Visual outcomes.
#Formula:
#H(X,Y)=−∑x,yP(x,y)⋅log⁡2(P(x,y))H(X,Y)=−x,y∑P(x,y)⋅log2(P(x,y))
#Steps:
#Remove zero probabilities from the joint matrix (joint_prob) to avoid errors in logarithmic calculations.
#Sum over all combinations of Audio and Visual.
  
# 4. Marginal Entropies H(X)H(X) and H(Y)H(Y)
#Purpose: Compute the uncertainty in Audio (H(X)H(X)) and Visual (H(Y)H(Y)) outcomes separately.
#Formula:
#H(X)=−∑xP(x)⋅log⁡2(P(x))H(Y)=−∑yP(y)⋅log⁡2(P(y))H(X)=−x∑P(x)⋅log2(P(x))H(Y)=−y∑P(y)⋅log2(P(y))
#Steps:
#Marginal probabilities are calculated by summing the joint probabilities across rows (for Audio) and columns (for Visual).
#Apply the entropy formula to these marginal probabilities.
  
#5. Conditional Entropies H(X∣Y)H(X∣Y) and H(Y∣X)H(Y∣X)
#Purpose: Measure the uncertainty in Audio given Visual (H(X∣Y)H(X∣Y)) and Visual given Audio (H(Y∣X)H(Y∣X)).
#Formula:
#H(X∣Y)=H(X,Y)−H(Y)H(Y∣X)=H(X,Y)−H(X)H(X∣Y)=H(X,Y)−H(Y)H(Y∣X)=H(X,Y)−H(X)
#Interpretation:
#H(X∣Y)H(X∣Y): Residual noise in the Audio channel once Visual information is known.
#H(Y∣X)H(Y∣X): Residual noise in the Visual channel once Audio information is known.
  
#6. Noise Ratio
#Purpose: Compare the relative contributions of Audio and Visual to overall noise.
#Formula:
#Noise Ratio=H(X∣Y)H(X∣Y)+H(Y∣X)Noise Ratio=H(X∣Y)+H(Y∣X)H(X∣Y)
#Interpretation:
#Noise Ratio = 0: Noise is entirely in the Audio channel.
#Noise Ratio = 1: Noise is entirely in the Visual channel.
#Values between 0 and 1 indicate the relative contribution of the two modalities.
  
#Results
#After running the code, the following metrics are computed:
#Joint Entropy H(X,Y)H(X,Y):
#Total uncertainty in the combined Audio-Visual outcomes.
#Marginal Entropies H(X)H(X) and H(Y)H(Y):
#Individual uncertainties in Audio and Visual outcomes.
#Conditional Entropies H(X∣Y)H(X∣Y) and H(Y∣X)H(Y∣X):
#Residual uncertainties in one modality given the other.
#Noise Ratio:
#A single value representing the balance of noise between Audio and Visual conditions.
#For example:
#Noise Ratio = 0.75: The majority of noise is attributed to the Visual condition relative to the Audio condition.
  
  

# -------------------------------------------------------------------------



# Load necessary libraries
library(dplyr)

# Step 1: Simulate a raw dataset of perceptual trials
set.seed(123)  # For reproducibility

# Assume 100 trials with binary outcomes for both Audio (A) and Visual (V)
# 1 represents "correct", 0 represents "incorrect"
n_trials <- 100
audio_outcome <- sample(c(0, 1), size = n_trials, replace = TRUE, prob = c(0.4, 0.6))  # 60% correct in audio
visual_outcome <- sample(c(0, 1), size = n_trials, replace = TRUE, prob = c(0.9, 0.1))  # 50% correct in visual

# Combine into a dataframe
df <- data.frame(
  trial = 1:n_trials,
  audio = audio_outcome,
  visual = visual_outcome
)

# Step 2: Calculate joint and marginal probabilities from raw data

# Calculate joint counts (how many trials have each (audio, visual) combination)
joint_counts <- df %>%
  group_by(audio, visual) %>%
  summarise(count = n(), .groups = 'drop')

# Convert counts to joint probabilities
joint_counts <- joint_counts %>%
  mutate(probability = count / n_trials)

# Create a probability matrix (joint distribution of audio and visual)
joint_prob <- matrix(0, nrow = 2, ncol = 2)
for (i in 1:nrow(joint_counts)) {
  joint_prob[joint_counts$audio[i] + 1, joint_counts$visual[i] + 1] <- joint_counts$probability[i]
}

# Step 3: Calculate the joint entropy H(X, Y)
joint_entropy <- function(joint_prob) {
  joint_prob <- joint_prob[joint_prob > 0]  # Remove zero probabilities
  -sum(joint_prob * log2(joint_prob))
}

H_XY <- joint_entropy(joint_prob)
H_XY  # Joint entropy of audio and visual outcomes

# Step 4: Calculate marginal probabilities and marginal entropies

# Marginal probabilities for Audio (sum across Visual outcomes)
audio_prob <- rowSums(joint_prob)

# Marginal probabilities for Visual (sum across Audio outcomes)
visual_prob <- colSums(joint_prob)

# Entropy function for marginal distributions
marginal_entropy <- function(prob) {
  prob <- prob[prob > 0]  # Remove zero probabilities
  -sum(prob * log2(prob))
}

# Calculate marginal entropies
H_X <- marginal_entropy(audio_prob)
H_Y <- marginal_entropy(visual_prob)

H_X  # Entropy of Audio outcomes
H_Y  # Entropy of Visual outcomes

# Step 5: Calculate conditional entropies H(X|Y) and H(Y|X)
H_X_given_Y <- H_XY - H_Y  # Conditional entropy of Audio given Visual
H_Y_given_X <- H_XY - H_X  # Conditional entropy of Visual given Audio

H_X_given_Y  # Noise from Audio given Visual
H_Y_given_X  # Noise from Visual given Audio

# Step 6: Calculate Noise Ratio
# Noise Ratio = H(X|Y) / (H(X|Y) + H(Y|X))
# where 0 is audio only and 1 is visual only
noise_ratio <- H_X_given_Y / (H_X_given_Y + H_Y_given_X)
noise_ratio  # Noise ratio between Audio and Visual conditions
