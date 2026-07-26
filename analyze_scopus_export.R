## ---------------------------------------------------------------------
## Solar Geoengineering Bibliometric Analysis (R version)
## ---------------------------------------------------------------------
## Reads a Scopus CSV export and produces:
##   1. Publication counts by country (whole counting AND fractional counting)
##   2. India's rank vs. high-income countries
##   3. Publication trend over time (India vs. top high-income countries)
##   4. Table of leading Indian institutions
##   5. Per-capita normalized comparison (addresses Lucy's "is a raw
##      comparison fair?" question)
##
## HOW TO GET THE INPUT FILE
## ---------------------------------------------------------------------
## 1. In Scopus, run your search (see suggested string in the notes below).
## 2. Select all results -> Export -> CSV.
## 3. In the export options, make sure these fields are checked:
##       Author(s), Author affiliations (or "Affiliations"), Year, Title,
##       Cited by, DOI
##    The "Affiliations" column is the important one -- it's what we
##    parse for country AND institution name. (Not "Authors with
##    affiliations" -- that puts the author's name first, which throws
##    off institution parsing.)
## 4. Save the file as scopus_export.csv in your working directory, or
##    change INPUT_FILE below / pass a path when sourcing.
##
## COUNTING METHOD (addresses Lucy's UK+India co-authorship question)
## ---------------------------------------------------------------------
## Whole counting (default, matches your stated methodology): if a paper
## has at least one author affiliated with a country, that country gets
## +1 for that paper -- regardless of how many total authors/countries
## are on it. A paper with a UK and an Indian author adds 1 to both UK's
## total and India's total.
## Fractional counting (also computed, for comparison): a paper with N
## distinct countries contributes 1/N to each. Shown as a secondary
## column so you can discuss why you chose whole counting.
##
## REQUIRED PACKAGES
## ---------------------------------------------------------------------
## The block below checks for each required package and installs it
## automatically if missing -- so this script runs on a fresh computer
## with zero manual setup beyond having R itself installed. This may
## take a minute or two the first time it's run on a new machine.

required_packages <- c("dplyr", "stringr", "readr", "ggplot2", "tidyr")
missing_packages <- required_packages[!(required_packages %in% installed.packages()[, "Package"])]
if (length(missing_packages) > 0) {
  cat("Installing missing packages (first run only, this may take a minute):",
      paste(missing_packages, collapse = ", "), "\n")
  install.packages(missing_packages, repos = "https://cloud.r-project.org")
}

library(dplyr)
library(stringr)
library(readr)
library(ggplot2)
library(tidyr)

# ---------------------------------------------------------------------
# 1. CONFIG -- edit these if your search / comparison set changes
# ---------------------------------------------------------------------

INPUT_FILE <- commandArgs(trailingOnly = TRUE)[1]

if (is.na(INPUT_FILE)) {
  # No filename given -- auto-detect: use scopus_export.csv if it exists,
  # otherwise fall back to the first .csv file found in this folder. This
  # means the script works out of the box on any computer without anyone
  # having to edit a filename by hand, as long as exactly one relevant
  # CSV is sitting in the same folder as this script.
  if (file.exists("scopus_export.csv")) {
    INPUT_FILE <- "scopus_export.csv"
  } else {
    OWN_OUTPUT_FILES <- c(
      "country_publication_counts.csv",
      "country_percapita_comparison.csv",
      "india_top_institutions.csv"
    )
    csv_files <- list.files(pattern = "\\.csv$", ignore.case = TRUE)
    csv_files <- setdiff(csv_files, OWN_OUTPUT_FILES)  # ignore our own prior outputs
    if (length(csv_files) == 1) {
      INPUT_FILE <- csv_files[1]
      cat(sprintf("No filename specified -- auto-detected and using '%s'\n", INPUT_FILE))
    } else if (length(csv_files) > 1) {
      stop(paste0(
        "Multiple CSV files found in this folder and no filename was specified:\n  ",
        paste(csv_files, collapse = "\n  "),
        "\nEither rename your Scopus export to 'scopus_export.csv', or run with:\n",
        "  Rscript analyze_scopus_export.R your_filename.csv"
      ))
    } else {
      INPUT_FILE <- "scopus_export.csv"  # will trigger the friendly not-found message in main()
    }
  }
}

# High-income comparator countries you're currently expecting to matter
# most. The script does NOT limit analysis to only these -- it ranks ALL
# countries first, then flags which of the top countries fall into this
# set, exactly as your methodology describes. Edit/expand if the real
# ranking surprises you.
HIGH_INCOME_WATCHLIST <- c(
  "United States", "United Kingdom", "Germany", "Australia", "Canada",
  "France", "Japan", "Netherlands", "Switzerland", "Sweden", "Norway",
  "South Korea", "Italy", "Spain", "Denmark", "Finland", "Singapore"
)

# Population (millions), covering the same top-15 countries shown in the
# ranking chart, so the per-capita comparison uses an identical, explainable
# set rather than an arbitrary subset. Figures are current estimates (2025)
# for illustration, not precision inputs to your paper -- update as needed.
POPULATION_MILLIONS <- c(
  "India" = 1440, "United States" = 335, "United Kingdom" = 68,
  "Germany" = 84, "Australia" = 26, "Canada" = 39, "China" = 1410,
  "France" = 65, "Japan" = 124, "Netherlands" = 18,
  "Finland" = 5.6, "Norway" = 5.6, "Denmark" = 6.0,
  "Switzerland" = 9.0, "South Africa" = 64.7
)

# Normalizes messy Scopus country strings to a consistent label.
COUNTRY_NORMALIZE <- c(
  "usa" = "United States", "united states of america" = "United States",
  "u.s.a." = "United States", "u.s." = "United States",
  "uk" = "United Kingdom", "u.k." = "United Kingdom",
  "england" = "United Kingdom", "scotland" = "United Kingdom",
  "wales" = "United Kingdom", "northern ireland" = "United Kingdom",
  "south korea" = "South Korea", "republic of korea" = "South Korea",
  "korea, republic of" = "South Korea",
  "russia" = "Russia", "russian federation" = "Russia",
  "viet nam" = "Vietnam",
  "peoples r china" = "China", "china (mainland)" = "China"
)

normalize_institution <- function(raw) {
  raw <- str_trim(raw)
  # Only split on 'and' when immediately followed by another Center/Centre
  # name (avoids wrongly splitting names that contain 'and' internally,
  # like "Centre for Atmospheric and Oceanic Sciences").
  parts <- str_split(raw, "(?<=\\S)\\s+and\\s+(?=Cent(er|re)\\b)")[[1]]
  parts <- str_trim(parts)
  parts <- parts[parts != ""]
  str_replace_all(parts, "\\bCenter\\b", "Centre")
}

normalize_country <- function(raw) {
  raw <- str_trim(raw)
  raw <- str_remove(raw, "\\.$")
  key <- tolower(raw)
  if (key %in% names(COUNTRY_NORMALIZE)) {
    return(unname(COUNTRY_NORMALIZE[key]))
  }
  # Title-case if the raw string was ALL CAPS, otherwise leave as-is
  if (raw == toupper(raw)) return(str_to_title(raw))
  raw
}

# ---------------------------------------------------------------------
# 2. PARSE AFFILIATIONS -> COUNTRIES PER PAPER
# ---------------------------------------------------------------------
# Scopus affiliation strings look like:
# "Univ Delhi, Dept Phys, Delhi, India; MIT, Cambridge, MA, United States"
# Each affiliation block is separated by ';'. The country is (almost
# always) the last comma-separated segment of each block.

# Some institution names contain a comma inside their own name (e.g. "Centre
# for Ocean, River, Atmosphere and Land Sciences (CORAL)"), which breaks our
# comma-based block splitting and truncates them (e.g. down to just "Centre
# for Ocean"). We protect known cases here by temporarily swapping their
# internal commas for a placeholder before splitting, then restoring them.
# Add more entries here as you discover them during screening.
KNOWN_MULTI_COMMA_INSTITUTIONS <- c(
  "Centre for Ocean, River, Atmosphere and Land Sciences" =
    "Centre for Ocean\u00b6River\u00b6Atmosphere and Land Sciences"
)

protect_known_institutions <- function(block) {
  for (full_name in names(KNOWN_MULTI_COMMA_INSTITUTIONS)) {
    block <- str_replace(block, fixed(full_name), KNOWN_MULTI_COMMA_INSTITUTIONS[[full_name]])
  }
  block
}

restore_known_institutions <- function(text) {
  str_replace_all(text, "\u00b6", ", ")
}

extract_countries_and_institutions <- function(affil_string) {
  empty <- list(countries = character(0),
                institutions = data.frame(country = character(0), inst = character(0)))
  if (is.na(affil_string) || str_trim(affil_string) == "") return(empty)

  blocks <- str_split(affil_string, ";")[[1]]
  blocks <- str_trim(blocks)
  blocks <- blocks[blocks != ""]
  if (length(blocks) == 0) return(empty)

  countries <- character(0)
  inst_country <- character(0)
  inst_name <- character(0)

  for (block in blocks) {
    block <- protect_known_institutions(block)
    parts <- str_trim(str_split(block, ",")[[1]])
    parts <- parts[parts != ""]
    if (length(parts) == 0) next
    country <- normalize_country(restore_known_institutions(parts[length(parts)]))
    countries <- c(countries, country)
    for (nm in normalize_institution(restore_known_institutions(parts[1]))) {
      inst_country <- c(inst_country, country)
      inst_name <- c(inst_name, nm)
    }
  }

  list(
    countries = unique(countries),
    institutions = data.frame(country = inst_country, inst = inst_name, stringsAsFactors = FALSE)
  )
}

find_column <- function(df, candidates) {
  hit <- candidates[candidates %in% names(df)]
  if (length(hit) == 0) {
    stop(paste0(
      "Couldn't find a matching column. Looked for: ", paste(candidates, collapse = ", "),
      "\nColumns found: ", paste(names(df), collapse = ", ")
    ))
  }
  hit[1]
}

# ---------------------------------------------------------------------
# 3. MAIN ANALYSIS
# ---------------------------------------------------------------------

main <- function() {
  if (!file.exists(INPUT_FILE)) {
    cat(sprintf(
      "Could not find '%s'. Export your Scopus results as CSV and place\nthem there, or pass the path as an argument: Rscript analyze_scopus_export.R your_file.csv\n",
      INPUT_FILE
    ))
    return(invisible(NULL))
  }

  df <- read_csv(INPUT_FILE, show_col_types = FALSE)
  aff_col <- find_column(df, c("Affiliations", "Author affiliations", "Authors with affiliations"))
  year_col <- find_column(df, c("Year", "Publication Year", "Year Published"))

  whole_counts <- list()
  fractional_counts <- list()
  cy_country <- character(0)   # growing list of (country, year) pairs -- one
  cy_year <- character(0)      # entry per country per paper, aggregated after
  india_institutions <- character(0)
  n_papers_with_affil <- 0

  for (i in seq_len(nrow(df))) {
    parsed <- extract_countries_and_institutions(df[[aff_col]][i])
    countries <- parsed$countries
    if (length(countries) == 0) next
    n_papers_with_affil <- n_papers_with_affil + 1
    year <- as.character(df[[year_col]][i])

    for (c in countries) {
      whole_counts[[c]] <- (whole_counts[[c]] %||% 0) + 1
      fractional_counts[[c]] <- (fractional_counts[[c]] %||% 0) + 1 / length(countries)
      cy_country <- c(cy_country, c)
      cy_year <- c(cy_year, year)
    }

    india_rows <- parsed$institutions[parsed$institutions$country == "India", "inst"]
    if (length(india_rows) > 0) india_institutions <- c(india_institutions, india_rows)
  }

  # Aggregate (country, year) pairs into counts -- built as a plain table,
  # not a manual running total, so there's no risk of the lookup-miss bug
  # that silently dropped repeat years in earlier versions of this script.
  cy_df <- data.frame(Country = cy_country, Year = cy_year, stringsAsFactors = FALSE)
  cy_counts <- cy_df %>% count(Country, Year, name = "n")

  cat(sprintf("\nTotal rows in export: %d\n", nrow(df)))
  cat(sprintf("Rows with parseable affiliation/country data: %d\n", n_papers_with_affil))

  ## ---- Country ranking table ----
  countries_all <- names(whole_counts)
  ranking <- data.frame(
    Country = countries_all,
    Publications_whole = unlist(whole_counts[countries_all]),
    Publications_fractional = round(unlist(fractional_counts[countries_all]), 1),
    stringsAsFactors = FALSE
  ) %>% arrange(desc(Publications_whole))
  rownames(ranking) <- NULL

  write_csv(ranking, "country_publication_counts.csv")
  cat("\nTop 15 countries by publication count:\n")
  print(head(ranking, 15))

  india_row <- which(ranking$Country == "India")
  if (length(india_row) > 0) {
    cat(sprintf("\nIndia's rank: %d out of %d countries\n", india_row[1], nrow(ranking)))
    cat(sprintf("India's publication count: %d\n", ranking$Publications_whole[india_row[1]]))
  } else {
    cat("\nNo 'India' entries found -- check country normalization/spelling in your data.\n")
  }

  top10 <- head(ranking, 10)
  hi_in_top10 <- top10 %>% filter(Country %in% HIGH_INCOME_WATCHLIST)
  cat("\nHigh-income countries in the top 10:\n")
  print(hi_in_top10)

  ## ---- Bar chart: top countries, India highlighted ----
  top15 <- head(ranking, 15) %>%
    mutate(Country = factor(Country, levels = rev(Country)),
           is_india = Country == "India")

  p1 <- ggplot(top15, aes(x = Country, y = Publications_whole, fill = is_india)) +
    geom_col() +
    coord_flip() +
    scale_fill_manual(values = c("FALSE" = "#4c72b0", "TRUE" = "#d62728"), guide = "none") +
    labs(x = NULL, y = "Number of publications",
         title = "Solar Geoengineering Publications by Country (Top 15)") +
    theme_minimal(base_size = 12)
  ggsave("chart_country_counts.png", p1, width = 9, height = 6, dpi = 150)
  cat("\nSaved: chart_country_counts.png\n")

  ## ---- Trend over time: India vs top 5 high-income comparators ----
  comparators <- top10 %>% filter(Country %in% HIGH_INCOME_WATCHLIST) %>% pull(Country)
  comparators <- head(comparators, 5)
  if (!"India" %in% comparators) comparators <- c("India", comparators)

  trend_df <- cy_counts %>%
    filter(Country %in% comparators) %>%
    mutate(Year = as.numeric(Year), Publications = n) %>%
    select(Country, Year, Publications) %>%
    arrange(Year)
  write_csv(trend_df, "trend_data_for_datawrapper.csv")
  p2 <- ggplot(trend_df, aes(x = Year, y = Publications, color = Country,
                              linewidth = Country == "India")) +
    geom_line() +
    geom_point(size = 1.5) +
    scale_linewidth_manual(values = c("FALSE" = 0.7, "TRUE" = 1.6), guide = "none") +
    labs(title = "Publication Trend Over Time: India vs. Leading High-Income Countries") +
    theme_minimal(base_size = 12)
  ggsave("chart_trend_over_time.png", p2, width = 9, height = 6, dpi = 150)
  cat("Saved: chart_trend_over_time.png\n")

  ## ---- Per-capita normalized comparison ----
  percap <- data.frame(
    Country = names(POPULATION_MILLIONS),
    Population_millions = as.numeric(POPULATION_MILLIONS),
    stringsAsFactors = FALSE
  ) %>%
    mutate(Raw_count = sapply(Country, function(c) whole_counts[[c]] %||% NA)) %>%
    filter(!is.na(Raw_count)) %>%
    mutate(Publications_per_million = round(Raw_count / Population_millions, 3)) %>%
    arrange(desc(Publications_per_million))

  write_csv(percap, "country_percapita_comparison.csv")
  cat("\nPer-capita comparison (population-normalized):\n")
  print(percap)

  ## ---- Indian institutions table ----
  inst_tab <- as.data.frame(table(india_institutions), stringsAsFactors = FALSE) %>%
    rename(Institution = india_institutions, Publications = Freq) %>%
    arrange(desc(Publications)) %>%
    head(15)
  write_csv(inst_tab, "india_top_institutions.csv")
  cat("\nTop Indian institutions:\n")
  print(inst_tab)

  cat("\nAll output files written to the current folder:\n")
  cat(" - country_publication_counts.csv\n")
  cat(" - country_percapita_comparison.csv\n")
  cat(" - india_top_institutions.csv\n")
  cat(" - chart_country_counts.png\n")
  cat(" - chart_trend_over_time.png\n")
}

## small null-coalescing helper, since base R doesn't have one
`%||%` <- function(a, b) if (is.null(a)) b else a

main()
