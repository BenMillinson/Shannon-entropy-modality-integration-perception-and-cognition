# notes -------------------------------------------------------------------

#Steps for Noise Ratio Calculation

# 1. Input Recall Data:
#Collect percentage correct data (pEpE and pLpL) from recall tasks.
#If joint recall performance (pE,LpE,L) is unavailable, it can be approximated under the assumption of independent recall probabilities:pE,L≈pE⋅pLpE,L≈pE⋅pL

# 2. Calculate Entropies:
#Marginal entropies for early and late stages:HE=H(pE),HL=H(pL)HE=H(pE),HL=H(pL)
#Joint entropy (if pE,LpE,L is known or approximated):H(E,L)=H(pE,L)H(E,L)=H(pE,L)

# 3. Compute Conditional Entropies:
#Conditional entropy of late stage given early stage:H(L∣E)=H(E,L)−HEH(L∣E)=H(E,L)−HE
#Conditional entropy of early stage given late stage:H(E∣L)=H(E,L)−HLH(E∣L)=H(E,L)−HL

# 4.Noise Ratio:
#Noise ratio for late-stage vs. early-stage noise:Noise Ratio=H(L∣E)H(L∣E)+H(E∣L)Noise Ratio=H(L∣E)+H(E∣L)H(L∣E)
#A high ratio suggests noise is more attributable to late-stage perceptual processes.
#A low ratio suggests noise is more attributable to early-stage sensory processes.

# Interpretation of Results:

#HE: Uncertainty/noise in early-stage recall.
#HL: Uncertainty/noise in late-stage recall.
#H(E,L)H(E,L): Total uncertainty considering both stages together.
#H(L∣E)H(L∣E): Noise in late-stage recall given early-stage performance.
#H(E∣L)H(E∣L): Noise in early-stage recall given late-stage performance.

#Noise Ratio:
#Closer to 0: Noise is predominantly from early-stage sensory processes.
#Closer to 1: Noise is predominantly from late-stage perceptual processes.



# -------------------------------------------------------------------------


# Percentage correct recall (as decimals, e.g., 0.8 = 80% correct)
p_E <- 0.8  # Early-stage percentage correct
p_L <- 0.6  # Late-stage percentage correct

# Function to calculate binary entropy
binary_entropy <- function(p) {
  if (p == 0 || p == 1) return(0)  # Avoid log of zero
  -p * log2(p) - (1 - p) * log2(1 - p)
}

# Step 1: Calculate marginal entropies
H_E <- binary_entropy(p_E)
H_L <- binary_entropy(p_L)

# Step 2: Estimate joint entropy
# Assuming independence: p_E_L = p_E * p_L
p_E_L <- p_E * p_L  # Approximation for joint probability
H_E_L <- binary_entropy(p_E_L)

# Step 3: Calculate conditional entropies
H_L_given_E <- H_E_L - H_E  # Late given Early
H_E_given_L <- H_E_L - H_L  # Early given Late

# Step 4: Calculate noise ratio
noise_ratio <- H_L_given_E / (H_L_given_E + H_E_given_L)

# Output results
cat("H(E): ", H_E, "\n")
cat("H(L): ", H_L, "\n")
cat("H(E, L): ", H_E_L, "\n")
cat("H(L|E): ", H_L_given_E, "\n")
cat("H(E|L): ", H_E_given_L, "\n")
cat("Noise Ratio (Late vs. Early): ", noise_ratio, "\n")
#where 1 is late noise and 0 is early
