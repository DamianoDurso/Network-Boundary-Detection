# jonashaslbeck@protonmail.com; July 1st, 2025

# --------------------------------------------
# --------- What is happening here? ----------
# --------------------------------------------

# Process the DASS-21 data from Open Psychometric Database:
# Source: https://openpsychometrics.org/_rawdata/DASS_data_21.02.19.zip
# This is a convenience sample

# --------------------------------------------
# --------- Loading Packages -----------------
# --------------------------------------------



# --------------------------------------------
# --------- Loading Data ---------------------
# --------------------------------------------

data <- read.csv("Empirical_data/Raw_data/1_DASS_OpenPsychometricDataBase/data.csv", sep="\t")
head(data)
dim(data)


# --------------------------------------------
# --------- Subsetting DASS-21 ---------------
# --------------------------------------------

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

# ----- Load Data -----

# ----- Subset DASS-21 -----
ind_ss <- unlist(lapply(dass_21_items, function(x) paste0("Q", x, "A")))
data_ss <- data[, ind_ss]

labels <- c(paste0("D.",dass_21_descriptions[[1]]),
            paste0("A.",dass_21_descriptions[[2]]),
            paste0("S.",dass_21_descriptions[[3]]))

colnames(data_ss) <- labels


# --------------------------------------------
# --------- Delete Missing Data --------------
# --------------------------------------------

dim(data_ss)
data_ss_noNA <- na.omit(data_ss)
dim(data_ss_noNA) # No missing data


# --------------------------------------------
# --------- Save -----------------------------
# --------------------------------------------

write.table(data_ss_noNA, file="Empirical_data/Processed_data/1_DASS-21.csv", sep = ";")


