# ============================================================
# Bibliometrix import script for WoS, Scopus, and Lens
# ============================================================

# Load required package
library(bibliometrix)

# Set working directory to the Query folder
# setwd("path/to/Query")  # <-- uncomment and adapt if needed

# ------------------------------------------------------------
# File paths
# ------------------------------------------------------------
lens_file   <- "./query/lens/lens-export2_filtered.csv"
scopus_file <- "./query/scopus/scopus_export_filtered.csv"
wos_file    <- "./query/wok/savedrecs.txt"

# ------------------------------------------------------------
# Import datasets
# ------------------------------------------------------------

# Lens
lens_data <- convert2df(
  file = lens_file,
  dbsource = "lens",
  format = "csv"
)
lens_data <- lens_data[
  !is.na(lens_data$DI) & trimws(lens_data$DI) != "",
]

# Scopus
scopus_data <- convert2df(
  file = scopus_file,
  dbsource = "scopus",
  format = "csv"
)
scopus_data <- scopus_data[
  !is.na(scopus_data$DI) & trimws(scopus_data$DI) != "",
]

# Web of Science (WoS)
wos_data <- convert2df(
  file = wos_file,
  dbsource = "wos",
  format = "plaintext"
)
wos_data <- wos_data[
  !is.na(wos_data$DI) & trimws(wos_data$DI) != "",
]


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
  remove.duplicated = TRUE
)

cat("Merged records:", nrow(merged_data), "\n")

merged_data <- merged_data[
  !is.na(merged_data$AU) & trimws(merged_data$AU) != "",
]

cat("Merged records after removing empty authors:", nrow(merged_data), "\n")

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


