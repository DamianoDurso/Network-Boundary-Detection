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
# -------- Processing ------------------------
# --------------------------------------------

# ---- Subset: Actually having Emp Cors ---
data[1, ]$empirical_corr # Missing with NA
data[8, ]$empirical_corr # Missing with NULL

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
  # In case there are several scales; my god, fucking horrible to reconstruct this^^
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

















