# ============================================================
# Bibliometrix import script for WoS, Scopus, and Lens
# ============================================================

# Load required package
library(bibliometrix)

# Utility function
keep_with_id <- function(df) {
  has_doi <- !is.na(df$DI) & trimws(df$DI) != ""
  df[has_doi, , drop = FALSE]
}

fix_borger_name <- function(M) {
  stopifnot(is.data.frame(M), "AU" %in% names(M))
  
  # Work on a copy
  AU <- M$AU
  
  # Skip missing entries
  idx <- !is.na(AU) & trimws(AU) != ""
  AU_sub <- AU[idx]
  
  # Normalize diacritics temporarily for matching
  AU_norm <- iconv(AU_sub, from = "", to = "ASCII//TRANSLIT")
  
  # Split authors
  authors_list <- strsplit(AU_norm, ";", fixed = TRUE)
  
  # Process each author string
  authors_fixed <- lapply(authors_list, function(auths) {
    auths <- trimws(auths)
    
    auths <- sapply(auths, function(a) {
      # Match Borger variants (surname-first format)
      if (grepl("^borger,|^boerger,", tolower(a))) {
        # Recover initials if present
        initials <- sub("^[^,]+,", "", a)
        initials <- trimws(initials)
        if (initials != "") {
          return(paste0("Börger, ", initials))
        } else {
          return("Börger")
        }
      }
      a
    }, USE.NAMES = FALSE)
    
    paste(auths, collapse = "; ")
  })
  
  # Put back into AU field
  AU[idx] <- authors_fixed
  M$AU <- AU
  
  return(M)
}

# Guardiamo solo "Authors Keywords" e "Titolo", e devono avere il DOI. Quindi facciamo il merge.
# Se hanno pagina iniziale e finale non è un book, e lo cambiamo automaticamente in Book Chapter

# ------------------------------------------------------------
# File paths
# ------------------------------------------------------------
lens_file   <- "../query/lens/lens-export-final.csv"
scopus_file <- "../query/scopus/scopus-export-final.csv"
wos_file    <- "../query/wok/wos-export-final-sanitized.txt"

# ------------------------------------------------------------
# Import datasets
# ------------------------------------------------------------

# Lens
lens_data <- convert2df(
  file = lens_file,
  dbsource = "lens",
  format = "csv"
)
lens_data <- keep_with_id(lens_data)
lens_data <- fix_borger_name(lens_data)

# Scopus
scopus_data <- convert2df(
  file = scopus_file,
  dbsource = "scopus",
  format = "csv"
)
scopus_data <- keep_with_id(scopus_data)
scopus_data <- fix_borger_name(scopus_data)

# Web of Science (WoS)
wos_data <- convert2df(
  file = wos_file,
  dbsource = "wos",
  format = "plaintext"
)
wos_data <- keep_with_id(wos_data)
wos_data <- fix_borger_name(wos_data)

# ------------------------------------------------------------
# Basic checks
# ------------------------------------------------------------
cat("Lens records:   ", nrow(lens_data), "\n")
cat("Scopus records: ", nrow(scopus_data), "\n")
cat("WoS records:    ", nrow(wos_data), "\n")

# ------------------------------------------------------------
# Merge datasets
# ------------------------------------------------------------
merged_data <- mergeDbSources(
  list(lens_data, scopus_data, wos_data),
  #list(scopus_data, wos_data),
  #list(scopus_data),
  remove.duplicated = TRUE,
  verbose = TRUE
)

merged_data <- merged_data[
  !is.na(merged_data$AU) & trimws(merged_data$AU) != "",
]

cat("Merged records after removing empty authors:", nrow(merged_data), "\n")
saveRDS(merged_data, file = "merged_data_bibliometrix.rds")

# ------------------------------------------------------------
# Number of papers per publication type
# ------------------------------------------------------------
# Replace NA or empty DT with 'Unknown'
merged_data$DT_clean <- merged_data$DT
merged_data$DT_clean[is.na(merged_data$DT_clean) | trimws(merged_data$DT_clean) == ""] <- "Unknown"

# Count again
pubtype_counts <- table(merged_data$DT_clean)
pubtype_counts_df <- as.data.frame(pubtype_counts)
colnames(pubtype_counts_df) <- c("DocumentType", "Count")

print(pubtype_counts_df)

# ------------------------------------------------------------
# Annual scientific production
# ------------------------------------------------------------
# Papers per year (PY)
years <- merged_data$PY
years <- years[!is.na(years)]

year_counts <- as.data.frame(table(years))
colnames(year_counts) <- c("Year", "Papers")
year_counts$Year <- as.integer(as.character(year_counts$Year))

# Sort by year
year_counts <- year_counts[order(year_counts$Year), ]

# Plot
plot(
  year_counts$Year, year_counts$Papers,
  type = "b",
  xlab = "Year",
  ylab = "Number of papers"
)

# ------------------------------------------------------------
# Top 10 most cited papers
# ------------------------------------------------------------
# Keep only papers with citation data
cit_data <- merged_data[!is.na(merged_data$TC), ]

# Order by total citations (descending)
top10_cited <- cit_data[order(-cit_data$TC), ][1:10, ]

# Create readable labels (shortened titles)
labels <- substr(top10_cited$TI, 1, 60)
labels <- paste0(labels, ifelse(nchar(top10_cited$TI) > 60, "...", ""))

# Plot histogram (bar chart)
barplot(
  top10_cited$TC,
  names.arg = labels,
  las = 2,
  ylab = "Total citations",
  main = "Top 10 Most Cited ASM Papers"
)

# ------------------------------------------------------------
# Top-5 authors -- No. Documents
# ------------------------------------------------------------

# 1) Get all author strings, drop missing
au <- merged_data$AU
au <- au[!is.na(au) & trimws(au) != ""]

# 2) Split authors (bibliometrix uses ';' as separator)
authors <- unlist(strsplit(au, ";", fixed = TRUE))
authors <- trimws(authors)
authors <- authors[authors != ""]

authors_norm <- iconv(authors, from = "", to = "ASCII//TRANSLIT")
author_counts <- sort(table(authors_norm), decreasing = TRUE)
top5 <- head(author_counts, 5)

barplot(as.numeric(top5), names.arg = names(top5), las = 2,
        ylab = "Number of contributions",
        main = "Top 5 Authors by Number of Contributions")

# ------------------------------------------------------------
# Top-5 authors -- Citations
# ------------------------------------------------------------

top_cited_authors <- function(M, k = 5, sep = ";") {
  stopifnot(is.data.frame(M), "AU" %in% names(M), "TC" %in% names(M))
  
  # Keep only rows with authors + citation counts
  M <- M[!is.na(M$AU) & trimws(M$AU) != "" & !is.na(M$TC), , drop = FALSE]
  
  # Split authors for each paper
  AU_list <- strsplit(M$AU, split = sep, fixed = TRUE)
  
  # Build an expanded table: one row per (paper, author)
  author <- trimws(unlist(AU_list))
  tc     <- rep(M$TC, times = lengths(AU_list))
  
  # Drop empty author tokens if any
  keep <- author != "" & !is.na(author)
  author <- author[keep]
  tc <- tc[keep]
  
  # Aggregate: total citations + number of papers per author
  df <- data.frame(Author = author, TC = tc, stringsAsFactors = FALSE)
  
  total_cit <- aggregate(TC ~ Author, data = df, sum)
  n_papers  <- aggregate(TC ~ Author, data = df, length)
  names(n_papers)[2] <- "Papers"
  
  out <- merge(total_cit, n_papers, by = "Author")
  out <- out[order(-out$TC, -out$Papers, out$Author), , drop = FALSE]
  
  head(out, k)
}

# Usage
top5_authors <- top_cited_authors(merged_data, k = 5)
print(top5_authors)

# ------------------------------------------------------------
# Top-5 authors -- Production over time
# ------------------------------------------------------------

authorProdOverTime(
  M = merged_data,
  k = 5,
  graph = TRUE
)

# ------------------------------------------------------------
# Top-10 keywords + keyword usage over time
# ------------------------------------------------------------
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)

# --- quick sanity checks ---
cat("Rows merged_data:", nrow(merged_data), "\n")
cat("Has PY:", "PY" %in% names(merged_data), "\n")
cat("Has DE:", "DE" %in% names(merged_data), "\n")
cat("Non-empty DE rows:", sum(!is.na(merged_data$DE) & str_trim(merged_data$DE) != ""), "\n")

# If DE is missing or empty, try ID (Keywords Plus)
kw_col <- if ("DE" %in% names(merged_data) && sum(!is.na(merged_data$DE) & str_trim(merged_data$DE) != "") > 0) {
  "DE"
} else if ("ID" %in% names(merged_data) && sum(!is.na(merged_data$ID) & str_trim(merged_data$ID) != "") > 0) {
  cat("Using ID instead of DE (DE missing/empty).\n")
  "ID"
} else {
  stop("No usable keyword column found: both DE and ID are missing or empty.")
}

# --- extract keywords ---
kw_df <- merged_data %>%
  select(PY, DE) %>%
  filter(!is.na(PY), !is.na(DE), str_trim(DE) != "") %>%
  mutate(Keyword = strsplit(DE, ";", fixed = TRUE)) %>%
  unnest(Keyword) %>%
  mutate(
    Keyword = str_trim(Keyword),
    Keyword = tolower(Keyword),
    # normalize punctuation so "asm.", "asm," etc get caught
    Keyword_norm = str_replace_all(Keyword, "[^a-z0-9\\s]+", ""),
    Keyword_norm = str_squish(Keyword_norm)
  ) %>%
  filter(Keyword_norm != "") %>%
  # EXCLUDE these keywords
  filter(!Keyword_norm %in% c(
    "abstract state machines",
    "abstract state machine",
    "asm"
  ))

cat("Total keyword tokens:", nrow(kw_df), "\n")

# 2) Thesaurus: group keywords (edit/extend these rules)
kw_df <- kw_df %>%
  mutate(
    Keyword_group = case_when(
      # verification grouping (your example)
      Keyword_norm %in% c("formal verification", "verification") ~ "verification",
      Keyword_norm %in% c("operational semantics", "semantics", "formal semantics") ~ "semantics",
      Keyword_norm %in% c("theory", "behavioural theory") ~ "theory",
      
      TRUE ~ Keyword_norm
    )
  )

# 3) Top 10 overall (after grouping)
top10_kw <- kw_df %>%
  count(Keyword_group, sort = TRUE) %>%
  slice_head(n = 10)

# 4) Usage over time
kw_time <- kw_df %>%
  semi_join(top10_kw, by = "Keyword_group") %>%
  count(PY, Keyword_group, name = "n") %>%
  complete(PY, Keyword_group, fill = list(n = 0)) %>%
  mutate(
    PY = as.integer(PY),
    Keyword_group = factor(Keyword_group, levels = rev(top10_kw$Keyword_group))
  )

# 5) Plot (darker = higher usage)
p <- ggplot(kw_time, aes(x = PY, y = Keyword_group, fill = n)) +
  geom_tile(color = "white", linewidth = 0.3) +
  scale_x_continuous(breaks = sort(unique(kw_time$PY))) +
  scale_fill_gradient(low = "white", high = "navy", name = "Occurrences") +
  labs(
    title = "Top 10 grouped keywords: usage over time",
    x = "Year",
    y = "Keyword (grouped)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p)