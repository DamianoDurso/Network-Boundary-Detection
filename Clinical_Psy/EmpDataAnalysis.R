# jonashaslbeck@protonmail.com; Feb 21st, 2025

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
dass_21_items <- list(
  Depression = c(3, 5, 10, 13, 16, 17, 21),
  Anxiety = c(2, 4, 7, 9, 15, 19, 20),
  Stress = c(1, 6, 8, 11, 12, 14, 18)
)
# DASS-21 items described in one word
dass_21_descriptions <- list(
  Depression = c("Positive", "Initiative", "Future", "Downhearted", "Enthusiasm", "Worth", "Meaningless"),
  Anxiety = c("Dryness", "Breathing", "Trembling", "Panic", "ClosePanic", "Heart", "Scared"),
  Stress = c("WindDown", "OverReact", "Energy", "Agitated", "Relax", "Intolerant", "Touchy")
)
# View the list
dass_21_descriptions

# Only problem: I think this is a convenience sample (but then, where do we find a random sample?)

# ----- Load Data -----
data <- read.csv("Clinical_Psy/Data/DASS_data_21.02.19/data.csv", sep="\t")
head(data)

# ----- Subset DASS-21 -----
ind_ss <- unlist(lapply(dass_21_items, function(x) paste0("Q", x, "A")))
data_ss <- data[, ind_ss]
dim(data_ss)

# ----- Make Network -----
# network_emp <- cor2pcor(cor(data_ss))
network_emp <- cor(data_ss)
labels <- unlist(dass_21_descriptions)
library(RColorBrewer)
cols <- brewer.pal(3, "Set1")

# --------------------------------------------
# --------- Load Synthethic Networks ---------
# --------------------------------------------

# ----- Models Synth -----
models = c('miniLM', 'mpnet',
           'e5')

# ----- Load Synth -----
# create empty lists which we will populate with the different cosine matrices
item_embed = list()

# ----- Adjust variable names synth -----
names_synth <- c(unlist(dass_21_descriptions, use.names = FALSE))

# Load the matries and align variable names
for (i in 1:length(models)){
  temp = as.matrix(read.csv(paste0("Clinical_Psy/Data/cos_matrices/matrix_concatenated_item_", models[i], ".csv")))
  colnames(temp) = names_synth
  item_embed[[i]] = symm(temp, names_synth)
}

# # Add OpenAI model "text-embedding-3-small"
item_embed[[length(models)+1]] <- as.matrix(read.csv("Clinical_Psy/Data/cos_matrices/text-embedding-3-large.csv"),21,21)
colnames(item_embed[[length((models))+1]]) <- rownames(item_embed[[length((models))+1]]) <- names_synth
models[length(models)+1] <- "text-embedding-3-small"
##
## # Add OpenAI model "text-embedding-3-small"
item_embed[[length(models)+1]] <- as.matrix(read.csv("Clinical_Psy/Data/cos_matrices/text-embedding-3-small.csv"),21,21)
colnames(item_embed[[length((models))+1]]) <- rownames(item_embed[[length((models))+1]]) <- names_synth
models[length(models)+1] <- "text-embedding-3-large"


# # Add OpenAI model "text-embedding-3-small"
#item_embed[length(models)+1] <- read.csv("Files/text-embedding-3-small.csv")
#models[length(models)+1] <- "text-embedding-3-small"
##
## # Add OpenAI model "text-embedding-3-small"
#item_embed[[length(models)+1]] <- read.csv("Files/text-embedding-3-large.csv")
#models[length(models)+1] <- "text-embedding-3-large"

item_embed

# Add average across transformers
item_embed[[length(models)+1]] = apply(simplify2array(item_embed)[,,c(1:5)], 1:2, mean)
models[length(models)+1] = 'Average [All]'



# ----- Make Networks Synth-----
network_synth = list()

N_embed <- length(item_embed)

for (synth in 1:N_embed){
  # network_synth[[synth]] = cor2pcor(item_embed[[synth]])
  network_synth[[synth]] = item_embed[[synth]]
}


# --------------------------------------------
# --------- Compare: Emp vs. Synthetic -------
# --------------------------------------------

sc <- 0.75
pdf("Figures/Comparison_DASS21_7Embeddings_COR.pdf", width = 12*sc, height = 12*sc)

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
# --------- Subscale Analysis ----------------
# --------------------------------------------

# ----- Define Subscale Items -----
dass_21_subscale_items <- list(
  Depression = paste0("Q", dass_21_items$Depression, "A"),
  Anxiety = paste0("Q", dass_21_items$Anxiety, "A"),
  Stress = paste0("Q", dass_21_items$Stress, "A")
)

# ----- Empirical Networks for Subscales -----
network_emp_subscales <- list()

for (scale in names(dass_21_subscale_items)) {
  subscale_data <- data_ss[, dass_21_subscale_items[[scale]]]
  network_emp_subscales[[scale]] <- cor(subscale_data)
}

# ----- Synthetic Networks for Subscales -----
network_synth_subscales <- list()

for (scale in names(dass_21_subscale_items)) {
  network_synth_subscales[[scale]] <- list()
  subscale_items <- dass_21_descriptions[[scale]]

  for (synth in 1:N_embed) {
    subscale_matrix <- item_embed[[synth]][subscale_items, subscale_items]
    network_synth_subscales[[scale]][[synth]] <- cor(subscale_matrix)
  }
}

# --------------------------------------------
# --------- Compare Subscales ----------------
# --------------------------------------------

pdf("Figures/Comparison_Subscales_DASS21_7Embeddings.pdf", width = 12, height = 12)

# Layout
par(mfrow = c(3, 3))

# Compare empirical and synthetic networks for each subscale
for (scale in names(dass_21_subscale_items)) {
  for (synth in 1:N_embed) {

    # Canvas
    plot.new()
    plot.window(xlim = c(-0.2, 1), ylim = c(-0.2, 1))
    axis(1)
    axis(2, las = 2)
    abline(0, 1)
    title(xlab = "InvCosSim [Synth]")
    title(ylab = "ParCor [Emp]")
    mtext(paste0(scale, " - EmbedMod = '", models[synth], "'"), side = 3, line = 1)

    # Data
    v_emp <- network_emp_subscales[[scale]][lower.tri(network_emp_subscales[[scale]])]
    v_syn <- network_synth_subscales[[scale]][[synth]][lower.tri(network_synth_subscales[[scale]][[synth]])]
    points(v_emp, v_syn)
    cor_s <- round(cor(v_emp, v_syn), 2)
    text(0.3, 0.8, paste0("Cor = ", cor_s), col = "steelblue", cex = 1.5)

  }
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
