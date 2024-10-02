# Continuous outcomes

# Generate data.
data <- familiar:::test_create_synthetic_series_data(
  outcome_type = "survival",
  n_batch = 3L,
  n_samples = 50L,
  n_series = 1L,
  n_rep = 1L
)

# Test results
.check_batch_normalisation_assumptions(
    data = data,
    normalisation_method = "combat"
)
