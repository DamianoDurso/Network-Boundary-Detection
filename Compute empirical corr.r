library(irw)
getwd()
library("redivis")
library(jsonlite)
library(dplyr)
library(tidyr)
library(psych)

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

# 1 Load Data
df_cosines <- read.csv(
  "./Network-Boundary-Detection/cosine_scales.csv",
  stringsAsFactors = FALSE
)

colnames(df_cosines)


model_cols <- c("distilroberta", "mpnet", "miniLM", "e5",
                "labse", "gpt3.large", "gpt3.small")

for (col in model_cols) {
  df_cosines[[col]] <- lapply(df_cosines[[col]], vector_to_square_matrix)
}
#check one
dim(gpt3_large_mats[[length(gpt3_large_mats)]])  # should be n x n

# Extract empirical, and compute correlations and store in empirical col cell
# 
dataset <- redivis::user("datapages")$dataset("item_response_warehouse") # connect to IRW


# list-column to hold empirical correlation matrices
df_cosines$empirical_corr <- vector("list", nrow(df_cosines))

# Helper: safe polychoric with better defaults and fallback
safe_polychoric <- function(wide) {
  # Ensure ordered factors
  wide_pf <- data.frame(lapply(wide, function(x) {
    if (is.numeric(x) || is.integer(x)) {
      ordered(x)
    } else {
      ordered(as.character(x))
    }
  }))
  
  # Try polychoric with global = FALSE and correct = 0 (to avoid the "try setting correct=0" issue)
  pc <- tryCatch(
    suppressWarnings(
      psych::polychoric(
        wide_pf,
        global  = FALSE,  # allow unequal response alternatives
        correct = 0,      # avoid polycor continuity corrections that sometimes break
        smooth  = TRUE
      )
    ),
    error = function(e) e
  )
  
  if (inherits(pc, "error")) {
    stop(pc)
  }
  
  pc$rho
}

# Helper: compute correlation matrix for a long-format data frame
compute_corr_from_long <- function(d, corr_type) {
  # 1) Identify key columns
  id_col <- "id"
  if (!id_col %in% names(d)) {
    stop("Could not find an 'id' column in the dataset.")
  }
  
  # Try common item column names
  item_col <- dplyr::case_when(
    "item_id" %in% names(d) ~ "item_id",
    "item"    %in% names(d) ~ "item",
    TRUE ~ NA_character_
  )
  if (is.na(item_col)) {
    stop("Could not find an item column (item_id / item) in the dataset.")
  }
  
  # 2) Long -> wide: rows = id, columns = items
  wide <- d %>%
    dplyr::select(all_of(c(id_col, item_col, "resp"))) %>%
    tidyr::pivot_wider(
      id_cols    = all_of(id_col),
      names_from = all_of(item_col),
      values_from = "resp"
    )
  
  # Drop id, keep only item responses
  wide <- wide %>% dplyr::select(-all_of(id_col))
  
  # Need at least 2 items
  if (ncol(wide) < 2L) {
    stop("Not enough items (columns) after pivot to wide format.")
  }
  
  # 3) Correlation
  if (corr_type == "polychoric") {
    # Try polychoric; if it fails, fall back to Pearson on numeric coding
    rho <- tryCatch(
      safe_polychoric(wide),
      error = function(e) e
    )
    if (inherits(rho, "error")) {
      warning("Polychoric failed (", conditionMessage(rho), 
              "); falling back to Pearson for this subset.")
      wide_num <- data.frame(lapply(wide, function(x) as.numeric(as.factor(x))))
      return(psych::cor.wt(wide_num, cor = TRUE, method = "pearson")$cor)
    }
    return(rho)
    
  } else {
    # Pearson correlations
    # If resp are factors/characters, coerce to numeric codes
    wide_num <- data.frame(lapply(wide, function(x) {
      if (is.numeric(x) || is.integer(x)) x else as.numeric(as.factor(x))
    }))
    psych::cor.wt(wide_num, cor = TRUE, method = "pearson")$cor
  }
}

# ------------------ MAIN LOOP OVER SCALES ------------------ #

for (scale in seq_len(nrow(df_cosines))) {
  scale_id <- df_cosines$scale_id[scale]
  message("Processing scale: ", scale_id, " (row ", scale, ")")
  
  #--- 1. Try to load dataset for this scale -----------------------#
  df_long <- try(
    dataset$table(scale_id)$to_tibble(),
    silent = TRUE
  )
  
  if (inherits(df_long, "try-error")) {
    warning("Could not load dataset for scale_id = ", scale_id, 
            "; setting empirical_corr to NA")
    df_cosines$empirical_corr[[scale]] <- NA
    next
  }
  
  # Need 'resp' and 'id' at minimum
  if (!all(c("id", "resp") %in% names(df_long))) {
    warning("Dataset for scale_id = ", scale_id, 
            " does not contain required columns 'id' and 'resp'; setting empirical_corr to NA")
    df_cosines$empirical_corr[[scale]] <- NA
    next
  }
  
  # Remove rows with missing responses
  df_long <- df_long[!is.na(df_long$resp), ]
  if (nrow(df_long) == 0) {
    warning("No non-missing responses for scale_id = ", scale_id,
            "; setting empirical_corr to NA")
    df_cosines$empirical_corr[[scale]] <- NA
    next
  }
  
  #--- 2. Decide correlation type based on number of categories ----#
  resp_vals <- unique(df_long$resp)
  n_cat <- length(resp_vals)
  
  corr_type <- dplyr::case_when(
    n_cat == 2 ~ "pearson",
    n_cat > 2 & n_cat < 7 ~ "polychoric",
    n_cat >= 7 ~ "pearson",
    TRUE ~ "pearson"
  )
  message("  Using ", corr_type, " correlations with ", n_cat, " response categories.")
  
  has_wave <- "wave" %in% names(df_long)
  cor_mats <- list()
  
  #--- 3. With or without wave ------------------------------------#
  if (has_wave) {
    waves <- sort(unique(df_long$wave))
    message("  Found wave column with ", length(waves), " waves: ",
            paste(waves, collapse = ", "))
    
    for (w in waves) {
      df_w <- df_long[df_long$wave == w & !is.na(df_long$wave), ]
      if (nrow(df_w) == 0) next
      
      cm <- try(compute_corr_from_long(df_w, corr_type), silent = TRUE)
      if (inherits(cm, "try-error")) {
        warning("    Correlation failed for wave = ", w, " in scale_id = ", scale_id)
        next
      }
      cor_mats[[as.character(w)]] <- cm
    }
    
    if (length(cor_mats) == 0) {
      warning("No valid correlation matrices for any wave in scale_id = ", scale_id)
      df_cosines$empirical_corr[[scale]] <- NA
      next
    }
    
    # Align matrices on common items (columns/rows) and average
    common_items <- Reduce(intersect, lapply(cor_mats, colnames))
    if (length(common_items) < 2) {
      warning("No or too few common items across waves for scale_id = ", scale_id)
      df_cosines$empirical_corr[[scale]] <- NA
      next
    }
    
    cor_mats_aligned <- lapply(cor_mats, function(m) m[common_items, common_items, drop = FALSE])
    empirical_mat <- Reduce("+", cor_mats_aligned) / length(cor_mats_aligned)
    
  } else {
    message("  No wave column; computing single correlation matrix.")
    empirical_mat <- try(compute_corr_from_long(df_long, corr_type), silent = TRUE)
    
    if (inherits(empirical_mat, "try-error")) {
      warning("Correlation computation failed for scale_id = ", scale_id)
      df_cosines$empirical_corr[[scale]] <- NA
      next
    }
  }
  
  #--- 4. Store empirical correlation matrix in df_cosines --------#
  df_cosines$empirical_corr[[scale]] <- empirical_mat
}


#logic above
    #try to load dataset using df <- dataset$table(df_cosines$scale_id[7])$to_tibble()
    # If fails, print to the corresponding row NA
    # If succeed:
        #- first check if df has the column "wave"
        #- second, check the amount of unique categories in the "resp" column (i.e., response categories)
        #- second, pivot datasets. All of them are in long format, meaning that, for each item, there are as many rows as respondents. 
        #   we want the dataset in wide format, where id (i.e., respondents) are rows and iem are columns
        # third, compute correlations, specifically:
            # if dataset has wave, correlations are computed separate per wave and then an average corr matrix is calculated from the wave-specific correlations
            # if dataset does not have wave, correlations are computed normally
            # if length of unique resp (excluding NA) is lower than 7 but higher than 2, use polychoric correlations
            # if length of unique resp (excluding NA) is == 2, use pearson correlations
            # if length of unique resp (excluding NA) is higher than 7  use standard correlations
        # insert empirical correlation matrix at the corresponding cell in the empirical_corr column

is.na(df_cosines$empirical_corr)
df_cosines$empirical_corr[1]


warnings()
empirical_mat
#Warning messages:
#1: Could not load dataset for scale_id = 360emergencymed_azami_2024; setting empirical_corr to NA
#2: No or too few common items across waves for scale_id = coach_chen_2022_hdrs
#3: No or too few common items across waves for scale_id = coach_chen_2022_phq9
#4: Could not load dataset for scale_id = fcv19s_hossain_2022_anxiety; setting empirical_corr to NA
#5: Could not load dataset for scale_id = fcv19s_hossain_2022_depression; setting empirical_corr to NA
#6: Correlation computation failed for scale_id = gilbert_meta_1
#7: Correlation computation failed for scale_id = gilbert_meta_100
#8: Correlation computation failed for scale_id = gilbert_meta_11
#9: Correlation computation failed for scale_id = gilbert_meta_12
#10: No or too few common items across waves for scale_id = gilbert_meta_34
#11:     Correlation failed for wave = 0 in scale_id = gilbert_meta_49
#12:     Correlation failed for wave = 1 in scale_id = gilbert_meta_49
#13:     Correlation failed for wave = 2 in scale_id = gilbert_meta_49
#14: No valid correlation matrices for any wave in scale_id = gilbert_meta_49
#15:     Correlation failed for wave = 0 in scale_id = gilbert_meta_53
#16:     Correlation failed for wave = 1 in scale_id = gilbert_meta_53
#17: No valid correlation matrices for any wave in scale_id = gilbert_meta_53
#18: No or too few common items across waves for scale_id = gilbert_meta_54
#19:     Correlation failed for wave = 0 in scale_id = gilbert_meta_56
#20:     Correlation failed for wave = 1 in scale_id = gilbert_meta_56
#21: No valid correlation matrices for any wave in scale_id = gilbert_meta_56
#22: No or too few common items across waves for scale_id = gilbert_meta_57
#23: No or too few common items across waves for scale_id = gilbert_meta_59
#24: No or too few common items across waves for scale_id = gilbert_meta_62
#25: Correlation computation failed for scale_id = gilbert_meta_7
#26: Correlation computation failed for scale_id = gilbert_meta_8
#27:     Correlation failed for wave = 0 in scale_id = gilbert_meta_88
#28:     Correlation failed for wave = 1 in scale_id = gilbert_meta_88
#29: No valid correlation matrices for any wave in scale_id = gilbert_meta_88
#30:     Correlation failed for wave = 0 in scale_id = gilbert_meta_89
#31:     Correlation failed for wave = 1 in scale_id = gilbert_meta_89
#32: No valid correlation matrices for any wave in scale_id = gilbert_meta_89
#33:     Correlation failed for wave = 1 in scale_id = gilbert_meta_90
#34: No valid correlation matrices for any wave in scale_id = gilbert_meta_90
#35: No or too few common items across waves for scale_id = gilbert_meta_91
#36: No or too few common items across waves for scale_id = gilbert_meta_92
#37: No or too few common items across waves for scale_id = oxfordcovid_xue_2024_gad
#38:     Correlation failed for wave = 0 in scale_id = oxfordcovid_xue_2024_mh
#39:     Correlation failed for wave = 17 in scale_id = oxfordcovid_xue_2024_mh
#40:     Correlation failed for wave = 23 in scale_id = oxfordcovid_xue_2024_mh
#41: No valid correlation matrices for any wave in scale_id = oxfordcovid_xue_2024_mh
#42: No or too few common items across waves for scale_id = oxfordcovid_xue_2024_phq
#43: Values from `resp` are not uniquely identified; output will contain list-cols.
#• Use `values_fn = list` to suppress this warning.
#• Use `values_fn = {summary_fun}` to summarise duplicates.
#• Use the following dplyr code to identify duplicates.
#  {data} |>
#  dplyr::summarise(n = dplyr::n(), .by = c(id, item)) |>
#  dplyr::filter(n > 1L)
#44: Correlation computation failed for scale_id = political_psychology
#45: Correlation computation failed for scale_id = spanishmegastudy