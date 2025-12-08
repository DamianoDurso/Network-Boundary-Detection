# jonashaslbeck@protonmail.com; Dec 2025

# --------------------------------------------
# -------- What is happening here? -----------
# --------------------------------------------

# Have a look at the correlations from emp vs. emb by Damiano

# --------------------------------------------
# -------- Load Packages ---------------------
# --------------------------------------------


# --------------------------------------------
# -------- Load Data -------------------------
# --------------------------------------------

data <- readRDS("cosine_and_corr.rds")
dim(data)

colnames(data)

data[1,2]

class(data)


# View(data)





# --------------------------------------------
# -------- Processing Damiano ----------------
# --------------------------------------------

# Processing logic
# Check if there is both empirical corr matrix and gpt.large matrix
# - To extract gpt.large matrix you can do data$gpt3.large[row][[1]]
# - To extract empirical corr we need to do extra processing steps
#   - Check if empirical corr for that row is a list as length(data$empirical_corr[[row]])
#     - If yes, then compute corr for all elements in the list with gpt.large embeddings by doing cor(as.numeric(data$gpt3.large[row][[1]]), as.numeric(data$empirical_corr[[row]]$`element_of_the_list`))
#     - If no, compute corr for the only element in the list
#     - Add correlation coefficient (or average in case of multiple elements in the list) to the data in the corr_coeff column

## Assumes `data` has:
## - data$gpt3.large : list-column where each element is a numeric vector
## - data$empirical_corr : list-column where each element is either
##     * a single numeric vector, or
##     * a list of numeric vectors
## We create/overwrite data$corr_coeff

# Note that the code below aggregates for multiple-waves datasets 
# but we could consider calculating separately for each wave and report them

# Helper: recursively extract all numeric vectors from any structure
extract_numeric_vectors <- function(x) {
  if (is.null(x) || length(x) == 0) {
    return(list())
  }
  
  # If x is atomic and numeric/coercible, treat it as a single vector
  if (is.atomic(x) && !is.list(x)) {
    return(list(as.numeric(x)))
  }
  
  # If x is a list, flatten recursively
  if (is.list(x)) {
    out <- lapply(x, extract_numeric_vectors)
    # flatten one level
    return(unlist(out, recursive = FALSE))
  }
  
  # Fallback: not usable → skip
  list()
}

# Compute correlation for a single row
compute_corr_for_row <- function(empirical_entry, gpt_vec) {

  # Normalize GPT vector
  gpt_vec <- as.numeric(gpt_vec)

  # Extract all numeric vectors from empirical entry
  vec_list <- extract_numeric_vectors(empirical_entry)

  # If nothing extracted → NA
  if (length(vec_list) == 0) {
    return(NA_real_)
  }

  # Compute correlations safely
  corrs <- vapply(
    vec_list,
    function(v) {
      v <- as.numeric(v)
      # Must match length and have some variance
      if (length(v) != length(gpt_vec) || length(unique(v)) < 2) {
        return(NA_real_)
      }
      suppressWarnings(cor(gpt_vec, v))
    },
    numeric(1)
  )

  # If all NA → NA
  if (all(is.na(corrs))) {
    return(NA_real_)
  }

  # Average non-NA correlations
  mean(corrs, na.rm = TRUE)
}

# Apply across rows
data$corr_coeff <- vapply(
  seq_len(nrow(data)),
  function(i) {
    gpt_vec <- data$gpt3.large[[i]]
    empirical_entry <- data$empirical_corr[[i]]
    compute_corr_for_row(empirical_entry, gpt_vec)
  },
  numeric(1)
)

mean(data$corr_coeff, na.rm =TRUE)

# --------------------------------------------
# -------- Processing Jonas ----------------
# --------------------------------------------


# ---- Subset: Actually having Emp Cors ---
data[1, ]$empirical_corr # Missing with NA
data[8, ]$empirical_corr # Missing with NULL

cor(as.numeric(data$gpt3.large[2][[1]]), as.numeric(data$empirical_corr[[2]]$`1`))

N <- nrow(data)
ind_OK <- rep(NA, N)
for(i in 1:N) {
  ind_OK[i] <- !(is.na(data[i, ]$empirical_corr) | is.null(data[i, ]$empirical_corr[[1]]))
}
# Subset
data_noNA <- data[which(ind_OK), ]
nrow(data_noNA)

# ---- How many scales per dataset? ----
Ns <- nrow(data_noNA)
n_scales <- rep(NA, Ns)
for(i in 1:Ns) {
  # In case there are several scales; my god, full disaster mode to reconstruct this^^
  if(!is.null(dim(data_noNA[i, ]$empirical_corr[[1]][[1]]))) {
    n_scales[i] <- length(data_noNA[i, ]$empirical_corr[[1]])
  } else {
    n_scales[i] <- 1
  }
}

# --------------------------------------------------
# ----- Trying to Make Sense of Data Structure -----
# --------------------------------------------------

i <- 1 # Dataset 1
j <- 1 # Scale 1
emp_ij <- data_noNA[i, ]$empirical_corr[[1]][[j]] # OK
emb_ij1 <- data_noNA[i, 2] # Where are the six scales for "distilroberta"?



# --------------------------------------------------
# ----- Get to DF of Main Results ------------------
# --------------------------------------------------

# We want DF:
# Rows: dataset x scales combinations
# Columns: Correlation between emp and the different ebs

tot_sc <- sum(n_scales)
df <- data.frame(matrix(NA, tot_sc, ncol = 7)) # For seven embeddings
counter <- 1
for(i in 1:Ns) {
  for(j in 1:n_scales[i]) {
    emp_ij <- data_noNA[i, ]$empirical_corr[[1]][[j]]
    v_emp_ij <- emp_ij[upper.tri(emp_ij)]
    # Loop through embedding & calculate cor
    for(e in 1:7) {
      emb_ije <- data_noNA[i, e+1][[1]]

      # STOP WORKING HERE: Dec 8th, 10am; No idea where the other six scales are

    }

  }
  emp_i <- data_noNA[i, ]$empirical_corr[[1]]



  # Iterate
  counter <- counter + 1
}





i <- 1 # Dataset
j <- 1 # Scale (I presume)

# ---- Make one plot
cors_1_emp <- (data_noNA[1, ]$empirical_corr)[[1]][[j]] # Why this double nesting here?
cors_1_emb1 <- data_noNA[1, ]$distilroberta[[j]]
# Get vectors
v_cors_1_emp <- cors_1_emp[upper.tri(cors_1_emp)]
v_cors_1_emb1 <- cors_1_emb1[upper.tri(cors_1_emb1)]

## PLot
plot(v_cors_1_emb1, v_cors_1_emp)
cor(v_cors_1_emb1, v_cors_1_emp)






# --------------------------------------------
# -------- Processing ------------------------
# --------------------------------------------

















