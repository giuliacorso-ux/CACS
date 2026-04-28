#!/usr/bin/env Rscript
#
# Climate Adaptation Article Screening Script (R version)
# ========================================================
#
# Screens Chinese-language articles on climate adaptation from CNKI database
# using inclusion and exclusion criteria (PRISMA 2020 compliant).
#
# Input:  CNKI CSV export (semicolon-separated)
# Output: Screened articles, statistics, and visualizations
#
# Author: Claude (Anthropic)
# Date: April 2026
#

library(dplyr)
library(stringr)
library(tidyr)


# ═══════════════════════════════════════════════════════════════════════════
# CONFIGURATION
# ═══════════════════════════════════════════════════════════════════════════

# Define screening criteria (as regex patterns)
inclusion_terms <- "社会|发展|公共|参与|人民|正义"
exclusion_terms <- "全球|金融|翻译|海洋|双碳|碳汇|碳中和|碳市场|碳交易|低碳经济"

# Thematic cluster reference
themes <- list(
  policy = list(
    name_en = "Policy & governance",
    name_zh = "政策与治理",
    keywords = c("适应战略", "立法", "政策", "环境影响评价", "国家安全")
  ),
  agri = list(
    name_en = "Agriculture & food security",
    name_zh = "农业与粮食安全",
    keywords = c("农业", "节水", "粮食", "土地", "水资源")
  ),
  urban = list(
    name_en = "Urban resilience & planning",
    name_zh = "城市韧性与规划",
    keywords = c("城市", "空间规划", "生态", "韧性", "灾害", "公共卫生")
  ),
  finance = list(
    name_en = "Financing mechanisms",
    name_zh = "资金机制",
    keywords = c("资金", "财政", "投资", "融资", "机制")
  ),
  tech = list(
    name_en = "Science & technology support",
    name_zh = "科技支撑",
    keywords = c("科技", "技术", "监测", "系统", "转移")
  ),
  equity = list(
    name_en = "Social equity & participation",
    name_zh = "社会公平与参与",
    keywords = c("社会", "参与", "脆弱", "权利", "移民", "社区")
  )
)


# ═══════════════════════════════════════════════════════════════════════════
# CORE SCREENING FUNCTIONS
# ═══════════════════════════════════════════════════════════════════════════

read_cnki_export <- function(filepath) {
  """
  Read CNKI CSV export file (semicolon-delimited, UTF-8 encoded).
  """
  df <- read.csv(
    filepath,
    sep = ";",
    encoding = "UTF-8",
    stringsAsFactors = FALSE,
    na.strings = c("", "N/A", "nan")
  )
  cat(sprintf("✓ Loaded %d records from %s\n", nrow(df), basename(filepath)))
  cat(sprintf("  Columns: %s\n\n", paste(colnames(df), collapse = ", ")))
  return(df)
}


combine_searchable_text <- function(title, keywords, abstract) {
  """
  Combine title, keywords, and abstract into single searchable field.
  """
  parts <- c(
    as.character(title %||% ""),
    as.character(keywords %||% ""),
    as.character(abstract %||% "")
  )
  # Remove NAs and empty strings, then collapse
  parts <- parts[!is.na(parts) & parts != ""]
  return(paste(parts, collapse = " "))
}


screen_articles <- function(df, inclusion_pattern, exclusion_pattern) {
  """
  Apply inclusion and exclusion filters to article metadata.
  
  Returns: List with screened_df and stats
  """
  
  # Create combined searchable text
  df <- df %>%
    mutate(
      combined_text = mapply(
        combine_searchable_text,
        `Title-题名`,
        `Keyword-关键词`,
        `Summary-摘要`,
        SIMPLIFY = FALSE
      ) %>% unlist()
    )
  
  # Step 1: Inclusion filter
  df <- df %>%
    mutate(
      matches_inclusion = str_detect(combined_text, inclusion_pattern)
    )
  
  n_matched_inclusion <- sum(df$matches_inclusion, na.rm = TRUE)
  df_included <- filter(df, matches_inclusion == TRUE)
  
  # Step 2: Exclusion filter
  df <- df %>%
    mutate(
      matches_exclusion = str_detect(combined_text, exclusion_pattern)
    )
  
  df_excluded <- df_included %>%
    filter(matches_exclusion == TRUE)
  
  n_excluded = nrow(df_excluded)
  
  # Final screened set
  df_screened <- df_included %>%
    filter(matches_exclusion == FALSE)
  
  n_screened <- nrow(df_screened)
  
  # Compile statistics
  stats <- list(
    total_input = nrow(df),
    matched_inclusion = n_matched_inclusion,
    excluded_by_exclusion = n_excluded,
    final_retained = n_screened,
    retention_rate = round(100 * n_screened / nrow(df), 1),
    inclusion_rate = round(100 * n_matched_inclusion / nrow(df), 1),
    excluded_inclusion = nrow(df) - n_matched_inclusion
  )
  
  return(list(
    screened = df_screened,
    excluded = df_excluded,
    stats = stats
  ))
}


analyze_keywords <- function(df) {
  """
  Extract and count keywords from retained articles.
  """
  keywords_list <- df$`Keyword-关键词` %>%
    str_split(pattern = "[;;，,]") %>%
    unlist() %>%
    str_trim() %>%
    keep(~. != "" & . != "nan" & . != "N/A" & . != "无")
  
  keyword_freq <- table(keywords_list) %>%
    sort(decreasing = TRUE) %>%
    as.data.frame() %>%
    setNames(c("keyword", "count"))
  
  return(keyword_freq)
}


analyze_temporal <- function(df) {
  """
  Analyze publication year distribution.
  """
  year_dist <- df %>%
    count(`Year-年`, name = "count") %>%
    rename(year = `Year-年`) %>%
    arrange(year)
  
  return(year_dist)
}


analyze_journals <- function(df, top_n = 15) {
  """
  Analyze source (journal) distribution.
  """
  journal_dist <- df %>%
    count(`Source-文献来源`, name = "count") %>%
    rename(journal = `Source-文献来源`) %>%
    arrange(desc(count)) %>%
    head(top_n)
  
  return(journal_dist)
}


# ═══════════════════════════════════════════════════════════════════════════
# OUTPUT GENERATION
# ═══════════════════════════════════════════════════════════════════════════

print_screening_summary <- function(stats) {
  """
  Print summary statistics.
  """
  cat("\n")
  cat(strrep("=", 70), "\n")
  cat("SCREENING SUMMARY\n")
  cat(strrep("=", 70), "\n")
  cat(sprintf("Total records in input:        %d\n", stats$total_input))
  cat(sprintf("Matched inclusion criteria:    %d (%.1f%%)\n", 
              stats$matched_inclusion, stats$inclusion_rate))
  cat(sprintf("Excluded by exclusion terms:   %d\n", stats$excluded_by_exclusion))
  cat(sprintf("Final retained for analysis:   %d\n", stats$final_retained))
  cat(sprintf("Overall retention rate:        %.1f%%\n", stats$retention_rate))
  cat(strrep("=", 70), "\n\n")
}


print_screened_articles <- function(df, max_abstract_chars = 250) {
  """
  Print full details of screened articles.
  """
  cat("\n")
  cat(strrep("=", 70), "\n")
  cat(sprintf("SCREENED ARTICLES (%d total)\n", nrow(df)))
  cat(strrep("=", 70), "\n\n")
  
  for (i in 1:nrow(df)) {
    row <- df[i, ]
    title <- row$`Title-题名` %||% "N/A"
    year <- row$`Year-年` %||% "N/A"
    journal <- row$`Source-文献来源` %||% "N/A"
    keywords <- row$`Keyword-关键词` %||% "N/A"
    abstract <- substring(as.character(row$`Summary-摘要` %||% "N/A"), 1, max_abstract_chars)
    
    cat(sprintf("\n[%d] %s\n", i, title))
    cat(sprintf("     Year: %s | Journal: %s\n", year, journal))
    cat(sprintf("     Keywords: %s\n", keywords))
    cat(sprintf("     Abstract: %s...\n", abstract))
    cat(sprintf("     %s\n", strrep("-", 66)))
  }
}


print_keyword_analysis <- function(keyword_df, top_n = 30) {
  """
  Print top keywords.
  """
  cat("\n")
  cat(strrep("=", 70), "\n")
  cat(sprintf("TOP %d KEYWORDS\n", min(top_n, nrow(keyword_df))))
  cat(strrep("=", 70), "\n\n")
  
  for (i in 1:min(top_n, nrow(keyword_df))) {
    kw <- keyword_df[i, "keyword"]
    count <- keyword_df[i, "count"]
    cat(sprintf("  %-20s : %3d\n", kw, count))
  }
  cat("\n")
}


print_temporal_analysis <- function(year_df) {
  """
  Print articles by publication year.
  """
  cat("\n")
  cat(strrep("=", 70), "\n")
  cat("PUBLICATION TIMELINE\n")
  cat(strrep("=", 70), "\n\n")
  
  for (i in 1:nrow(year_df)) {
    year <- year_df[i, "year"]
    count <- year_df[i, "count"]
    bar <- strrep("█", count)
    cat(sprintf("  %d  %s  (%d)\n", year, bar, count))
  }
  cat("\n")
}


print_journal_analysis <- function(journal_df) {
  """
  Print articles by journal.
  """
  cat("\n")
  cat(strrep("=", 70), "\n")
  cat("TOP JOURNALS\n")
  cat(strrep("=", 70), "\n\n")
  
  for (i in 1:nrow(journal_df)) {
    journal <- journal_df[i, "journal"]
    count <- journal_df[i, "count"]
    cat(sprintf("  %-35s : %2d\n", journal, count))
  }
  cat("\n")
}


export_screened_csv <- function(df, output_path, inclusion_pattern, exclusion_pattern) {
  """
  Export screened articles to CSV for manual review.
  """
  
  export_cols <- c(
    "Title-题名", "Year-年", "Source-文献来源", "Author-作者",
    "Keyword-关键词", "Summary-摘要"
  )
  
  export_df <- df %>%
    select(all_of(intersect(export_cols, colnames(df)))) %>%
    mutate(
      Screening_Inclusion_Terms = inclusion_pattern,
      Screening_Exclusion_Terms = exclusion_pattern,
      Manual_Theme_Code = NA_character_,
      Retrievable = NA_character_,
      Notes = NA_character_
    )
  
  write.csv(
    export_df,
    file = output_path,
    row.names = FALSE,
    fileEncoding = "UTF-8"
  )
  
  cat(sprintf("\n✓ Exported %d screened articles to: %s\n", nrow(export_df), output_path))
}


# ═══════════════════════════════════════════════════════════════════════════
# MAIN PIPELINE
# ═══════════════════════════════════════════════════════════════════════════

main <- function(input_csv_path, output_csv_path = NULL) {
  """
  Main screening pipeline.
  """
  
  cat("\n")
  cat(strrep("=", 70), "\n")
  cat("🔍 CLIMATE ADAPTATION ARTICLE SCREENING\n")
  cat("CNKI Systematic Review Pipeline (PRISMA 2020)\n")
  cat(strrep("=", 70), "\n")
  
  # Load data
  df <- read_cnki_export(input_csv_path)
  
  # Screen articles
  screening_result <- screen_articles(df, inclusion_terms, exclusion_terms)
  screened <- screening_result$screened
  excluded <- screening_result$excluded
  stats <- screening_result$stats
  
  # Print results
  print_screening_summary(stats)
  
  # Analyze screened set
  keywords <- analyze_keywords(screened)
  years <- analyze_temporal(screened)
  journals <- analyze_journals(screened)
  
  print_keyword_analysis(keywords, top_n = 30)
  print_temporal_analysis(years)
  print_journal_analysis(journals)
  
  print_screened_articles(screened)
  
  # Export for manual review
  if (is.null(output_csv_path)) {
    output_csv_path <- sub(".csv$", "_SCREENED.csv", input_csv_path)
  }
  
  export_screened_csv(
    screened,
    output_csv_path,
    inclusion_terms,
    exclusion_terms
  )
  
  # Print next steps
  cat("\n")
  cat(strrep("=", 70), "\n")
  cat("Next steps:\n")
  cat("  1. Check full-text retrievability for all retained articles\n")
  cat("  2. Manually code each article with thematic cluster\n")
  cat("  3. Review and validate thematic assignments with co-coder\n")
  cat(strrep("=", 70), "\n\n")
  
  # Return invisibly for further processing if needed
  invisible(list(
    screened = screened,
    excluded = excluded,
    stats = stats
  ))
}


# ═══════════════════════════════════════════════════════════════════════════
# EXECUTION
# ═══════════════════════════════════════════════════════════════════════════

if (!interactive()) {
  # Run from command line
  input_file <- "/mnt/user-data/uploads/CNKI-20260428103549247_last.csv"
  output_file <- "/mnt/user-data/outputs/screened_articles.csv"
  
  result <- main(input_file, output_file)
}
