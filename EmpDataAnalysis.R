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
data <- read.csv("Data/DASS_data_21.02.19/data.csv", sep="\t")
head(data)

# ----- Subset DASS-21 -----
ind_ss <- unlist(lapply(dass_21_items, function(x) paste0("Q", x, "A")))
data_ss <- data[, ind_ss]
dim(data_ss)

# ----- Make Network -----
network_emp <- cor2pcor(cor(data_ss))
labels <- unlist(dass_21_descriptions)
library(RColorBrewer)
cols <- brewer.pal(3, "Set1")

# --------------------------------------------
# --------- Synthethic Networks --------------
# --------------------------------------------

# ----- Models Synth -----
models = c('distilroberta', 'miniLM', 'mpnet',
           'e5', 'labse', 'wulff')

# ----- Load Synth -----
# create empty lists which we will populate with the different cosine matrices
item_embed = list()

# ----- Adjust variable names synth -----
names_synth <- c(unlist(dass_21_descriptions, use.names = FALSE))

# Load the matries and align variable names
for (i in 1:length(models)){
    temp = as.matrix(read.csv(paste0("Data/cos_matrices/matrix_concatenated_item_", models[i], ".csv")))
    colnames(temp) = names_synth
    item_embed[[i]] = symm(temp, names_synth)
}

# Add average across transformers
item_embed[[i+1]] = apply(simplify2array(item_embed)[,,c(1:length(models))], 1:2, mean)
models[length(models)+1] = 'Average'

# ----- Make Networks Synth-----
network_synth = list()

for (synth in 1:length(item_embed)){
  network_synth[[synth]] = cor2pcor(item_embed[[synth]])
}

# Create the PDF file
pdf("Figures/EmpNetwork_vandenBerg2020_n.pdf", width = 12, height = 4 * length(network_synth))

# Layout: rows = length(network_synth), columns = 3
par(mfrow = c(length(network_synth), 1))

# Loop through each network
for (net in 1:length(network_synth)) {
  
  # Empirical Network plot
#  qgraph(network_emp, layout = "spring", labels = labels, 
#         groups = list("Depression" = 1:7, "Anxiety" = 8:14, "Stress" = 15:21),
#         mar = rep(7, 4), color = cols, legend = FALSE)
#  mtext("Empirical Network", side = 3, line = 1)
  
  # Embedding Network plot
#  qgraph(network_synth[[net]], layout = "spring", labels = labels, 
#         groups = list("Depression" = 1:7, "Anxiety" = 8:14, "Stress" = 15:21),
#         mar = rep(7, 4), color = cols, legend = FALSE)
#  mtext(paste0("Embedding Network", " ", models[net]), side = 3, line = 1)
  
  # Scatter plot for comparing edges
  plot(network_emp, network_synth[[net]])
#  plot.window(xlim = c(-0.2, 1), ylim = c(-0.2, 1))
#  axis(1)
#  axis(2, las = 2)
  abline(0, 1)
#  title(xlab = "Inverse Cosine Similarity [Embedding Network]")
#  title(ylab = "Partial Correlation [Empirical Network]")
  mtext(paste0("Comparing in empirical and ", models[net], " network"), side = 3, line = 1)
}

# Close the PDF file
dev.off()

network_synth[[1]] == 1






