# # jonashaslbeck@protonmail.com; Feb 21, 2024

# --------------------------------------------
# --------- What is happening here? ----------
# --------------------------------------------

# If we have correlated correlations; how correlated are the partial correlations?

# --------------------------------------------
# --------- Let's have a Look ----------------
# --------------------------------------------

generate_positive_definite_matrix <- function(n, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)  # Set seed for reproducibility if provided

  # Generate a random matrix
  A <- matrix(rnorm(n * n), n, n)

  # Create a symmetric positive definite matrix
  pd_matrix <- t(A) %*% A

  return(pd_matrix)
}

# Example usage
n <- 10  # Define the size of the matrix
m1 <- generate_positive_definite_matrix(n)
m2 <- m1 + generate_positive_definite_matrix(n)


library(matrixcalc)
is.positive.definite(m1)
is.positive.definite(m2)

cor(as.numeric(m1), as.numeric(m2))

# Inverse
m1i <- solve(m1)
m2i <- solve(m2)
cor(as.numeric(m1i), as.numeric(m2i))

