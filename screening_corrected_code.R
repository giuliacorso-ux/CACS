# Climate Adaptation Article Screening - local version (R)
# ========================================================
# Rule-based keyword (lexicon) screening of CNKI records on title, author
# keywords and abstract. Initial screening is automated; thematic coding and
# full-text checks remain manual (see "Next steps" printed at the end).
#
# HOW TO RUN (RStudio):
#   1. Put this script and the CNKI csv in the same folder.
#   2. Session > Set Working Directory > To Source File Location.
#   3. Run the whole script (Source), or run main() at the bottom.
#
# Outputs (written to the working directory):
#   screened_articles.csv   retained articles + empty columns for manual coding
#   screening_log.csv       ALL records with decision and reason (for PRISMA)

library(readr)
library(dplyr)
library(stringr)
library(purrr)


# ═══════════════════════════════════════════════════════════════════════════
# CONFIGURATION  (edit only this block)
# ═══════════════════════════════════════════════════════════════════════════

# Files: relative to the working directory. Use a full path if you prefer,
# e.g. "C:/Users/giulia/Documents/review/CNKI-20260428103549247_last.csv"
input_file    <- "CNKI-20260428103549247_last.csv"
screened_file <- "screened_articles.csv"
log_file      <- "screening_log.csv"

# Screening criteria (regex alternatives separated by |)
inclusion_terms <- "社会|发展|公共|参与|人民|正义"
exclusion_terms <- "全球|金融|翻译|海洋|双碳|碳汇|碳中和|碳市场|碳交易|低碳经济"

# Thematic cluster reference (for MANUAL coding only; not used in screening)
themes <- list(
  policy  = list(name_en = "Policy & governance",
                 name_zh = "政策与治理",
                 keywords = c("适应战略", "立法", "政策", "环境影响评价", "国家安全")),
  agri    = list(name_en = "Agriculture & food security",
                 name_zh = "农业与粮食安全",
                 keywords = c("农业", "节水", "粮食", "土地", "水资源")),
  urban   = list(name_en = "Urban resilience & planning",
                 name_zh = "城市韧性与规划",
                 keywords = c("城市", "空间规划", "生态", "韧性", "灾害", "公共卫生")),
  finance = list(name_en = "Financing mechanisms",
                 name_zh = "资金机制",
                 keywords = c("资金", "财政", "投资", "融资", "机制")),
  tech    = list(name_en = "Science & technology support",
                 name_zh = "科技支撑",
                 keywords = c("科技", "技术", "监测", "系统", "转移")),
  equity  = list(name_en = "Social equity & participation",
                 name_zh = "社会公平与参与",
                 keywords = c("社会", "参与", "脆弱", "权利", "移民", "社区"))
)


# ═══════════════════════════════════════════════════════════════════════════
# HELPERS
# ═══════════════════════════════════════════════════════════════════════════

# Return `default` when x is NULL or NA
or_default <- function(x, default = "N/A") {
  if (is.null(x) || length(x) == 0 || is.na(x)) default else x
}

# Which terms of a "a|b|c" pattern occur in each text? Returns one string per text.
terms_hit <- function(texts, pattern) {
  terms <- str_split(pattern, "\\|")[[1]]
  map_chr(texts, function(t) paste(terms[str_detect(t, fixed(terms))], collapse = "; "))
}


# ═══════════════════════════════════════════════════════════════════════════
# CORE FUNCTIONS
# ═══════════════════════════════════════════════════════════════════════════

# Read the CNKI export (semicolon-delimited, UTF-8)
read_cnki_export <- function(filepath) {
  if (!file.exists(filepath)) {
    stop("File not found: ", filepath, "\nCurrent working directory: ", getwd(),
         "\nSet the working directory or edit `input_file` in the CONFIGURATION block.")
  }
  df <- read_delim(
    filepath,
    delim = ";",
    escape_double = FALSE,
    trim_ws = TRUE,
    locale = locale(encoding = "UTF-8"),
    show_col_types = FALSE
  )
  cat(sprintf("Loaded %d records from %s\n", nrow(df), basename(filepath)))
  cat(sprintf("Columns: %s\n\n", paste(colnames(df), collapse = ", ")))
  df
}


# Screen records. Both filters are computed in ONE mutate() on the full table
# (avoids the "matches_exclusion not found" error), and every record gets a
# decision plus the terms that triggered it.
screen_articles <- function(df, inclusion_pattern, exclusion_pattern) {
  
  df <- df %>%
    mutate(
      combined_text = str_squish(paste(
        coalesce(as.character(`Title-题名`), ""),
        coalesce(as.character(`Keyword-关键词`), ""),
        coalesce(as.character(`Summary-摘要`), "")
      )),
      matches_inclusion = str_detect(combined_text, inclusion_pattern),
      matches_exclusion = str_detect(combined_text, exclusion_pattern),
      inclusion_terms_hit = terms_hit(combined_text, inclusion_pattern),
      exclusion_terms_hit = terms_hit(combined_text, exclusion_pattern),
      decision = case_when(
        !matches_inclusion ~ "excluded: no inclusion term",
        matches_exclusion  ~ "excluded: exclusion term",
        TRUE               ~ "retained"
      ),
      reason = case_when(
        decision == "excluded: no inclusion term" ~ "no inclusion term matched",
        decision == "excluded: exclusion term"    ~ paste("exclusion term(s):", exclusion_terms_hit),
        TRUE                                      ~ ""
      )
    )
  
  df_screened <- filter(df, decision == "retained")
  df_excluded <- filter(df, matches_inclusion, matches_exclusion)
  
  n_total <- nrow(df)
  n_incl  <- sum(df$matches_inclusion)
  
  stats <- list(
    total_input           = n_total,
    matched_inclusion     = n_incl,
    excluded_by_exclusion = nrow(df_excluded),
    final_retained        = nrow(df_screened),
    retention_rate        = round(100 * nrow(df_screened) / n_total, 1),
    inclusion_rate        = round(100 * n_incl / n_total, 1),
    excluded_inclusion    = n_total - n_incl
  )
  
  list(all = df, screened = df_screened, excluded = df_excluded, stats = stats)
}


analyze_keywords <- function(df) {
  kw <- df$`Keyword-关键词` %>%
    as.character() %>%
    str_split(pattern = "[;；，,]") %>%   # ASCII ; , and Chinese ； ，
    unlist() %>%
    str_trim()
  kw <- kw[!is.na(kw) & !(kw %in% c("", "nan", "N/A", "无"))]
  
  tibble(keyword = kw) %>%
    count(keyword, name = "count", sort = TRUE)
}


analyze_temporal <- function(df) {
  df %>%
    count(`Year-年`, name = "count") %>%
    rename(year = `Year-年`) %>%
    arrange(year)
}


analyze_journals <- function(df, top_n = 15) {
  df %>%
    count(`Source-文献来源`, name = "count") %>%
    rename(journal = `Source-文献来源`) %>%
    arrange(desc(count)) %>%
    head(top_n)
}


# ═══════════════════════════════════════════════════════════════════════════
# OUTPUT
# ═══════════════════════════════════════════════════════════════════════════

print_screening_summary <- function(stats) {
  cat("\n", strrep("=", 70), "\n", sep = "")
  cat("SCREENING SUMMARY\n")
  cat(strrep("=", 70), "\n", sep = "")
  cat(sprintf("Total records in input:        %d\n", stats$total_input))
  cat(sprintf("Matched inclusion criteria:    %d (%.1f%%)\n",
              stats$matched_inclusion, stats$inclusion_rate))
  cat(sprintf("No inclusion term matched:     %d\n", stats$excluded_inclusion))
  cat(sprintf("Excluded by exclusion terms:   %d\n", stats$excluded_by_exclusion))
  cat(sprintf("Final retained for analysis:   %d\n", stats$final_retained))
  cat(sprintf("Overall retention rate:        %.1f%%\n", stats$retention_rate))
  cat(strrep("=", 70), "\n\n", sep = "")
}


print_exclusion_breakdown <- function(df_excluded) {
  # How many records each exclusion term removed (a record can hit several)
  if (nrow(df_excluded) == 0) return(invisible(NULL))
  cat(strrep("=", 70), "\n", sep = "")
  cat("EXCLUSION TERMS: records removed per term\n")
  cat(strrep("=", 70), "\n", sep = "")
  counts <- df_excluded$exclusion_terms_hit %>%
    str_split("; ") %>% unlist() %>% table() %>% sort(decreasing = TRUE)
  for (term in names(counts)) cat(sprintf("  %-12s : %3d\n", term, counts[[term]]))
  cat("\n")
}


print_screened_articles <- function(df, max_abstract_chars = 250) {
  cat(strrep("=", 70), "\n", sep = "")
  cat(sprintf("SCREENED ARTICLES (%d total)\n", nrow(df)))
  cat(strrep("=", 70), "\n\n", sep = "")
  
  for (i in seq_len(nrow(df))) {
    row      <- df[i, ]
    title    <- or_default(row$`Title-题名`)
    year     <- or_default(row$`Year-年`)
    journal  <- or_default(row$`Source-文献来源`)
    keywords <- or_default(row$`Keyword-关键词`)
    abstract <- substring(as.character(or_default(row$`Summary-摘要`)), 1, max_abstract_chars)
    
    cat(sprintf("\n[%d] %s\n", i, title))
    cat(sprintf("     Year: %s | Journal: %s\n", year, journal))
    cat(sprintf("     Keywords: %s\n", keywords))
    cat(sprintf("     Abstract: %s...\n", abstract))
    cat(sprintf("     %s\n", strrep("-", 66)))
  }
}


print_keyword_analysis <- function(keyword_df, top_n = 30) {
  n <- min(top_n, nrow(keyword_df))
  cat("\n", strrep("=", 70), "\n", sep = "")
  cat(sprintf("TOP %d KEYWORDS\n", n))
  cat(strrep("=", 70), "\n\n", sep = "")
  for (i in seq_len(n)) {
    cat(sprintf("  %-20s : %3d\n", keyword_df$keyword[i], keyword_df$count[i]))
  }
  cat("\n")
}


print_temporal_analysis <- function(year_df) {
  cat(strrep("=", 70), "\n", sep = "")
  cat("PUBLICATION TIMELINE\n")
  cat(strrep("=", 70), "\n\n", sep = "")
  for (i in seq_len(nrow(year_df))) {
    cat(sprintf("  %s  %s  (%d)\n", year_df$year[i],
                strrep("█", year_df$count[i]), year_df$count[i]))
  }
  cat("\n")
}


print_journal_analysis <- function(journal_df) {
  cat(strrep("=", 70), "\n", sep = "")
  cat("TOP JOURNALS\n")
  cat(strrep("=", 70), "\n\n", sep = "")
  for (i in seq_len(nrow(journal_df))) {
    cat(sprintf("  %-35s : %2d\n", journal_df$journal[i], journal_df$count[i]))
  }
  cat("\n")
}


# Retained articles + empty columns for manual coding.
# write_excel_csv() writes UTF-8 with BOM, so Excel shows Chinese correctly.
export_screened_csv <- function(df, output_path, inclusion_pattern, exclusion_pattern) {
  export_cols <- c("Title-题名", "Year-年", "Source-文献来源", "Author-作者",
                   "Keyword-关键词", "Summary-摘要")
  export_df <- df %>%
    select(all_of(intersect(export_cols, colnames(df)))) %>%
    mutate(
      Screening_Inclusion_Terms = inclusion_pattern,
      Screening_Exclusion_Terms = exclusion_pattern,
      Manual_Theme_Code = NA_character_,
      Retrievable       = NA_character_,
      Notes             = NA_character_
    )
  write_excel_csv(export_df, output_path)
  cat(sprintf("Exported %d screened articles to: %s\n", nrow(export_df),
              normalizePath(output_path, mustWork = FALSE)))
}


# Full screening log (every record, decision and reason) for the PRISMA flow
export_screening_log <- function(df_all, output_path) {
  log_cols <- c("Title-题名", "Year-年", "Source-文献来源", "Author-作者",
                "Keyword-关键词", "decision", "reason",
                "inclusion_terms_hit", "exclusion_terms_hit")
  write_excel_csv(select(df_all, all_of(intersect(log_cols, colnames(df_all)))), output_path)
  cat(sprintf("Exported screening log (%d records) to: %s\n", nrow(df_all),
              normalizePath(output_path, mustWork = FALSE)))
}


# ═══════════════════════════════════════════════════════════════════════════
# MAIN PIPELINE
# ═══════════════════════════════════════════════════════════════════════════

main <- function(input_csv_path = input_file,
                 screened_csv_path = screened_file,
                 log_csv_path = log_file) {
  
  cat("\n", strrep("=", 70), "\n", sep = "")
  cat("CLIMATE ADAPTATION ARTICLE SCREENING\n")
  cat("CNKI Systematic Review Pipeline (PRISMA 2020)\n")
  cat(strrep("=", 70), "\n", sep = "")
  
  df <- read_cnki_export(input_csv_path)
  
  res      <- screen_articles(df, inclusion_terms, exclusion_terms)
  screened <- res$screened
  excluded <- res$excluded
  stats    <- res$stats
  
  print_screening_summary(stats)
  print_exclusion_breakdown(excluded)
  
  keywords <- analyze_keywords(screened)
  years    <- analyze_temporal(screened)
  journals <- analyze_journals(screened)
  
  print_keyword_analysis(keywords, top_n = 30)
  print_temporal_analysis(years)
  print_journal_analysis(journals)
  print_screened_articles(screened)
  
  export_screened_csv(screened, screened_csv_path, inclusion_terms, exclusion_terms)
  export_screening_log(res$all, log_csv_path)
  
  cat("\n", strrep("=", 70), "\n", sep = "")
  cat("Next steps:\n")
  cat("  1. Check full-text retrievability for all retained articles\n")
  cat("  2. Manually code each article with thematic cluster\n")
  cat("  3. Review and validate thematic assignments with co-coder\n")
  cat("  4. Skim screening_log.csv to check what the exclusion terms removed\n")
  cat(strrep("=", 70), "\n\n", sep = "")
  
  invisible(res)
}


# ═══════════════════════════════════════════════════════════════════════════
# RUN
# ═══════════════════════════════════════════════════════════════════════════
result <- main()
