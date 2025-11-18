# ------------------------------------------------------------
# Build retrieval pipeline for embeddings vs empirical correlation analysis
# ------------------------------------------------------------

# What we do here:
# - Pull item data
# - Pull Scale metaninformation
# - Merge above data sets
# - Store dataframe


library(irw)
library(redivis)
library(dplyr)
library(stringr)
library(tidyr)
library(purrr)
library(digest)
library(RefManageR)


all_item_tables <- irw_list_itemtext_tables()

all_items <- dplyr::tibble()     # start with empty tibble instead of NA
failed_tables <- character()     # to log which tables we skipped


for (tbl in all_item_tables) {
  message("Processing table: ", tbl)

  # Try to pull this table safely
  df_tbl <- tryCatch(
    {
      irw::irw_itemtext(tbl)
    },
    error = function(e) {
      message("❌ Failed to retrieve table: ", tbl, " | Error: ", e$message)
      failed_tables <<- c(failed_tables, tbl)
      return(NULL)
    }
  )

  # If it failed, skip
  if (is.null(df_tbl)) next

  # --- Harmonize types we know cause trouble ---

  # 1. item column (ID / number of the item in the scale)
  if ("item" %in% names(df_tbl)) {
    df_tbl$item <- as.character(df_tbl$item)
  }

  # 2. item_text should always be character (defensive)
  if ("item_text" %in% names(df_tbl)) {
    df_tbl$item_text <- as.character(df_tbl$item_text)
  }

  # 3. scale_id and scale_name often come through as factors or mixed;
  #    we'll coerce them to character if they exist
  if ("scale_id" %in% names(df_tbl)) {
    df_tbl$scale_id <- as.character(df_tbl$scale_id)
  } else {
    df_tbl$scale_id <- as.character(tbl)
  }

  if ("scale_name" %in% names(df_tbl)) {
    df_tbl$scale_name <- as.character(df_tbl$scale_name)
  } else if ("measure_name" %in% names(df_tbl)) {
    df_tbl$scale_name <- as.character(df_tbl$measure_name)
  } else if ("instrument_name" %in% names(df_tbl)) {
    df_tbl$scale_name <- as.character(df_tbl$instrument_name)
  } else if ("resp" %in% names(df_tbl)) {
    df_tbl$resp <- as.character(df_tbl$resp)
  } else {
    df_tbl$scale_name <- as.character(tbl)
  }

  # Add traceability
  df_tbl$source_table <- tbl

  # Deduplicate rows within this table based on item_text (if present),
  # otherwise just on all columns
  if ("item_text" %in% names(df_tbl)) {
    df_tbl_unique <- df_tbl[!duplicated(df_tbl$item_text), ]
  } else {
    df_tbl_unique <- df_tbl[!duplicated(df_tbl), ]
  }

  # Accumulate
  all_items <- dplyr::bind_rows(all_items, df_tbl_unique)
}

# Optional: inspect failures
if (length(failed_tables) > 0) {
  message("⚠️ Some tables failed:")
  print(failed_tables)
  writeLines(failed_tables, "failed_tables.txt")
} else {
  message("✅ All tables processed successfully.")
}

items_raw <- all_items

colnames(items_raw)

# 2. pull construct / tagging metadata
tags_df <- irw_tags()
# We assume tags_df includes columns like:
# - scale_id
# - construct_label
# - construct_definition
# - construct_type
# (If it's keyed differently, adapt the join criteria below.)


#----------------- Bib, for later maybe --------------------#
# 3. link bibliographic info to each scale
# Save bibtex for *all* item tables
bib_file <- "refs.bib"
irw_save_bibtex(all_item_tables, output_file = bib_file)

# Parse BibTeX using RefManageR
bib_entries <- RefManageR::ReadBib(bib_file)

# Convert to a data frame
bib_df <- as.data.frame(bib_entries, stringsAsFactors = FALSE)
bib_df$key <- rownames(bib_df) #create key for later retrieval

# At this point bib_df will typically have columns like:
#   bibtype, key, title, author, year, doi, journal, ...
# NOTE: names can differ slightly depending on the entries.
# We’ll standardize a few columns we care about.

# Normalize / coerce critical fields:
# - 'key' will become our 'citation_key'
# - We'll try to create 'scale_id' out of 'key' the same way we did before.
#   (If you have a proper scale_id mapping from IRW directly, use that instead.)
bib_df <- bib_df %>%
  mutate(
    citation_key = ifelse(!is.na(key), key, ""),
    doi          = ifelse(!is.na(doi), doi, ""),
    year         = ifelse(!is.na(year), year, ""),
    title        = ifelse(!is.na(title), title, ""),
    author       = ifelse(!is.na(author), author, "")
  )

# We still need some way to link bib info to the items for enrichment.
# In the earlier draft, I assumed we could join on scale_id.
# If IRW uses table names or scale identifiers in the bibtex key, we do:
#   scale_id <- citation_key
# Otherwise, if you *do* have a real scale_id column somewhere else
# (e.g. tags_df also has a DOI and scale_id you can match), prefer that.
bib_df <- bib_df %>%
  mutate(
    scale_id = citation_key  # <-- fallback assumption
  )


# Now pick/rename only the columns we care about for retrieval
bib_df_clean <- bib_df %>%
  select(
    scale_id,
    citation_key,
    doi,
    title,
    year,
    author
  ) %>%
  distinct()



# 4. join items_raw with tags and bib info (scale-level metadata)
items_enriched <- items_raw %>%
  left_join(tags_df, by = "table")  %>%
  left_join(bib_df_clean, by = "scale_id")


# After these joins, items_enriched should ideally have:
#   item_text
#   scale_id
#   scale_name
#   construct_label
#   construct_definition
#   construct_type
#   doi
#   citation_key
#   source_table
# You can inspect names(items_enriched) and rename as needed.

# 5. Build normalized tables -------------------------

# constructs table
constructs_tbl <- items_enriched %>%
  select(construct_type,
         construct_name) %>%
  distinct() %>%
  mutate(construct_id = row_number()) %>%
  relocate(construct_id)

# scales table
# We'll assign each unique scale_id / scale_name etc a row
scales_tbl <- items_enriched %>%
  select(scale_id,
         scale_name,
         doi,
         citation_key,
         title,
         year,
         author,
         construct_type) %>%
  distinct() %>%
  mutate(scale_id = as.character(scale_id))

# items table
items_tbl <- items_enriched %>%
  select(scale_id,
         item_text,
         source_table) %>%
  distinct() %>%
  mutate(item_id = row_number()) %>%
  relocate(item_id)

# 6. Build retrieval corpus (denormalized, 1 row per item) ----
# This is what we will embed and index.
# We also generate a stable doc_id to let us trace back.
# doc_id can be hash(scale_id + item_text)

retrieval_corpus <- items_enriched %>%
  mutate(
    scale_id = as.character(scale_id),
    construct_definition = coalesce(construct_name, ""),
    construct_type = coalesce(construct_type, ""),
    doi = coalesce(doi, ""),
    scale_name = coalesce(scale_name, ""),
    # stable ID:
    doc_id = paste0(scale_id, "::", digest(item_text, algo = "md5"))
  ) %>%
  select(
    doc_id,
    item_text,
    scale_id,
    scale_name,
    construct_definition,
    construct_type,
    doi,
    citation_key,
    source_table
  ) %>%
  distinct(item_text, .keep_all = TRUE)

# 7. Save everything ----------------------------------

scales_tbl$author = as.character(scales_tbl$author)
write.csv(items_enriched, "complete_df.csv", row.names = FALSE)
write.csv(constructs_tbl, "constructs.csv", row.names = FALSE)
write.csv(scales_tbl,     "scales.csv",     row.names = FALSE)
write.csv(items_tbl,      "items.csv",      row.names = FALSE)

# This is the file you will embed + turn into FAISS later
write.csv(retrieval_corpus, "retrieval_corpus.csv", row.names = FALSE)

