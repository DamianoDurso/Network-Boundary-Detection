# jonashaslbeck@protonmail.com; Sept 30, 2024

# --------------------------------------------
# --------- What is happening here? ----------
# --------------------------------------------

# We check out some candidate open datasets

# --------------------------------------------
# --------- Functions ------------------------
# --------------------------------------------
#In the function below we conduct the following operations
# 1- copy upper triangle of the matrix to lower triangle
# 2- align rownames and colnames
# 3- reorder matrix by the order specified in var
symm <- function(m, var) {
  m[upper.tri(m)] <- t(m)[upper.tri(m)]
  rownames(m) <- colnames(m)
  orig_order <- colnames(m)
  ord_ind <- match(var, orig_order)
  reordered_matrix <- m[ord_ind, ord_ind]
  return(reordered_matrix)
}

# --------------------------------------------
# --------- Load Packages --------------------
# --------------------------------------------

library(corpcor)
library(qgraph)

# --------------------------------------------
# --------- Van den Berg et al. 2020 ---------
# --------------------------------------------

# ----- Data Description -----
# https://link.springer.com/article/10.1007/s10608-020-10153-w
# https://openpsychometrics.org/_rawdata/DASS_data_21.02.19.zip

# Info on DASS scale: https://www2.psy.unsw.edu.au/dass/over.htm

# DASS-21 (Anxiety / Depression / Stress)
# N= ~11k
# The data includes the DASS-42, but we want to DASS-21
# Mapping: https://chatgpt.com/share/66fa7c2a-101c-800b-88a5-7334934a995d
big5_items <- list(
  Ext = c(paste('EXT', 1:10, sep = "")),
  Est = c(paste('EST', 1:10, sep = "")),
  Agr = c(paste('AGR', 1:10, sep = "")),
  Csn = c(paste('CSN', 1:10, sep = "")),
  Opn = c(paste('OPN', 1:10, sep = ""))
)

# Define one-word descriptions for each item in the Big Five
big5_descriptions <- list(
  Extraversion = c("LifeOfParty", "Quiet", "Comfortable", "Background", "Conversations", 
                   "Silent", "Sociable", "Reserved", "Attention", "Strangers"),
  Emotional_Stability = c("Stressed", "Relaxed", "Worry", "Happy", "Disturbed", 
                          "Upset", "MoodChange", "MoodSwings", "Irritable", "Sad"),
  Agreeableness = c("Unconcerned", "Interested", "Insulting", "Empathetic", "Uninvolved", 
                    "SoftHearted", "Apathetic", "Helpful", "Empathy", "Comforting"),
  Conscientiousness = c("Prepared", "Messy", "DetailOriented", "Untidy", "Efficient", 
                        "Forgetful", "Organized", "Neglectful", "Scheduled", "Exact"),
  Openness = c("Vocabulary", "Concrete", "Imaginative", "Uninterested", "Innovative", 
               "Unimaginative", "QuickLearner", "Verbose", "Reflective", "Ideaful")
)

# View the lists
big5_items
big5_descriptions

# Only problem: I think this is a convenience sample (but then, where do we find a random sample?)

# ----- Load Data -----
# data set should not be saved on the folder synced to github, because it's too big and you won't be able to push/pull. Ensure that you get the data from a local folder
data <- read.csv("/Users/damianodurso/Downloads/data_subsetted.csv")
head(data)

# ----- Subset DASS-21 -----
# ----- Subset Big Five Data -----
# Generate column names based on item indices
ind_ss <- unlist(big5_items)
data_ss <- data[, ind_ss]
dim(data_ss)

# ----- Make Network -----
library(qgraph)
library(RColorBrewer)
# ----- Ensure Columns are Numeric -----
# Convert all columns in data_ss to numeric
data_ss[] <- lapply(data_ss, function(x) as.numeric(as.character(x)))

# Calculate partial correlations for the network
network_emp <- cor2pcor(cor(data_ss, use = 'complete.obs'))

# Create labels from descriptions for use in the network
labels <- unlist(big5_descriptions)

# Define colors (using Set1 for variety) for the five traits
cols <- brewer.pal(5, "Set1")

# Plot network
qgraph(network_emp, labels = labels, color = cols, layout = "spring")
# --------------------------------------------
# --------- Load Synthethic Networks ---------
# --------------------------------------------

# ----- Models Synth -----
models = c('distilroberta', 'miniLM', 'mpnet',
           'e5', 'labse', 'wulff', 'psych')

# ----- Load Synth -----
# create empty lists which we will populate with the different cosine matrices
item_embed = list()

# ----- Adjust variable names synth -----
names_synth <- c(unlist(big5_descriptions, use.names = FALSE))

# Load the matries and align variable names
for (i in 1:length(models)){
  temp = as.matrix(read.csv(paste0("Personality_psy/Data/IPIP-FFM-data-Nov/cos_matrices/matrix_concatenated_item_", models[i], ".csv")))
  colnames(temp) = names_synth
  item_embed[[i]] = symm(temp, names_synth)
}

# Add average across transformers
item_embed[[length(models)+1]] = apply(simplify2array(item_embed)[,,c(5,6)], 1:2, mean)
models[length(models)+1] = 'Average [Damiano]'

# Add OpenAI model "text-embedding-3-small"
#item_embed[length(models)+1] <- read.csv("Data/cos_matrices/text-embedding-3-small.csv")
#models[length(models)+1] <- "text-embedding-3-small"

# Add OpenAI model "text-embedding-3-small"
#item_embed[[length(models)+1]] <- read.csv("Data/cos_matrices/text-embedding-3-large.csv")
#models[length(models)+1] <- "text-embedding-3-large"


# ----- Make Networks Synth-----
network_synth = list()

N_embed <- length(item_embed)

for (synth in 1:N_embed){
  network_synth[[synth]] = cor2pcor(item_embed[[synth]])
}

# Plot network
qgraph(network_synth[[6]], labels = labels, color = cols, layout = "spring")


# --------------------------------------------
# --------- Compare: Emp vs. Synthetic -------
# --------------------------------------------

sc <- 0.75
pdf("Figures/Comparison_big5_7Embeddings.pdf", width = 12*sc, height = 12*sc)

# Layout
par(mfrow=c(3,3))

# Show data
for(synth in 1:N_embed) {

  # Canvas
  plot.new()
  plot.window(xlim = c(-0.2, 1), ylim = c(-0.2, 1))
  axis(1)
  axis(2, las = 2)
  abline(0, 1)
  title(xlab = "InvCosSim [Synth]")
  title(ylab = "ParCor [Emp]")
  mtext(paste0("EmbedMod ='", models[synth], "'"), side = 3, line = 1)
  # Data
  v_emp <- network_emp[lower.tri(network_emp)]
  v_syn <- network_synth[[synth]][lower.tri(network_synth[[synth]])]
  points(v_emp, v_syn)
  cor_s <- round(cor(v_emp, v_syn), 2)
  text(0.3, 0.8, paste0("Cor = ", cor_s), col="steelblue", cex=1.5)

}

dev.off()

# --------------------------------------------
# --------- DUMP -----------------------------
# --------------------------------------------


# # OLD PLOTTING
# # Create the PDF file
# pdf("Figures/EmpNetwork_vandenBerg2020_n.pdf", width = 12, height = 4 * length(network_synth))
#
# # Layout: rows = length(network_synth), columns = 3
# par(mfrow = c(length(network_synth), 1))
#
# # Loop through each network
# for (net in 1:length(network_synth)) {
#
#   # Empirical Network plot
#   #  qgraph(network_emp, layout = "spring", labels = labels,
#   #         groups = list("Depression" = 1:7, "Anxiety" = 8:14, "Stress" = 15:21),
#   #         mar = rep(7, 4), color = cols, legend = FALSE)
#   #  mtext("Empirical Network", side = 3, line = 1)
#
#   # Embedding Network plot
#   #  qgraph(network_synth[[net]], layout = "spring", labels = labels,
#   #         groups = list("Depression" = 1:7, "Anxiety" = 8:14, "Stress" = 15:21),
#   #         mar = rep(7, 4), color = cols, legend = FALSE)
#   #  mtext(paste0("Embedding Network", " ", models[net]), side = 3, line = 1)
#
#   # Scatter plot for comparing edges
#   plot(network_emp, network_synth[[net]])
#   plot.window(xlim = c(-0.2, 1), ylim = c(-0.2, 1))
#   axis(1)
#   axis(2, las = 2)
#   abline(0, 1)
#   title(xlab = "Inverse Cosine Similarity [Embedding Network]")
#   title(ylab = "Partial Correlation [Empirical Network]")
#   mtext(paste0("Comparing in empirical and ", models[net], " network"), side = 3, line = 1)
# }
#
# # Close the PDF file
# dev.off()
#
# network_synth[[1]] == 1
#
#




