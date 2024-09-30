# jonashaslbeck@protonmail.com; Sept 30, 2024

# --------------------------------------------
# --------- What is happening here? ----------
# --------------------------------------------

# We check out some candidate open datasets


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

pdf("Figures/EmpNetwork_vandenBerg2020.pdf", width=12, height = 4)

# Layout
par(mfrow=c(1,3))

# Empirical Network
qgraph(network_emp, layout="spring", labels=labels, groups=list("Depression"=1:7,
                                                                "Anxiety"=8:14,
                                                                "Stress"=15:21),
       mar=rep(7,4),
       color = cols, legend=FALSE)
mtext("Empirical Network")

# Embedding Network
plot.new()
mtext("Embedding Network")

# Relationship Edges
plot.new()
plot.window(xlim=c(-.2, 1), ylim=c(-.2, 1))
axis(1)
axis(2, las=2)
abline(0,1)
title(xlab="Inverse Cosine Similarity [Embedding Network]")
title(ylab="Partial Correlation [Empirical Network]")
mtext("Comparing Edges in both Networks")


dev.off()















