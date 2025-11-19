library(irw)
getwd()
library("redivis")
library(jsonlite)


# What we do here:
# - Load the dataset with cosine similarities calculated per scale
# - Extract empirical data from IRW for each of the scales in the df above
# - Calculate empirical correlations for each of the extracted dataset
# - Store the correlation matrix in a new column in the df
# - Store final data frame

#------------------------ functions --------------------#
#------------------------ functions --------------------#
#------------------------ functions --------------------#


vector_to_square_matrix <- function(x) {
  # Handle NA or empty cells
  if (is.na(x) || x == "") {
    return(NA)
  }
  
  # Parse JSON string → numeric vector
  v <- fromJSON(x)
  
  if (!is.numeric(v)) {
    stop("Parsed object is not numeric; got class: ", paste(class(v), collapse = ", "))
  }
  
  L <- length(v)
  n <- sqrt(L)
  
  # Check it's really a perfect square
  if (abs(n - round(n)) > 1e-8) {
    stop("Vector length (", L, ") is not a perfect square.")
  }
  n <- as.integer(round(n))
  
  # IMPORTANT: use byrow = TRUE if Python flattened row-wise
  m <- matrix(v, nrow = n, ncol = n, byrow = TRUE)
  return(m)
}

#----------------------------------------------------------#
#----------------------------------------------------------#

#----------------------------------------------------------#


df_cosines <- read.csv(
  "./Network-Boundary-Detection/cosine_scales.csv",
  stringsAsFactors = FALSE
)

# Reconstruct matrices for one model, e.g. gpt3.large
gpt3_large_mats <- lapply(df_cosines$gpt3.large, vector_to_square_matrix)

# Inspect one
gpt3_large_mats[[length(gpt3_large_mats)]]
dim(gpt3_large_mats[[length(gpt3_large_mats)]])  # should be n x n




# 
dataset <- redivis::user("datapages")$dataset("item_response_warehouse") # connect to IRW
df <- dataset$table("4thgrade_math_sirt")$to_tibble() # download data

df