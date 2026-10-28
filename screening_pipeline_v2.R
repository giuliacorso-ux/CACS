# Climate Adaptation Article Screening - v2 (R)
# ==============================================
# Works on the NEW CNKI export (professional search, Chinese interface).
# Search string (CNKI Professional Search, Academic Journals, Cross-Language OFF):
#   TKA=('适应气候变化'+'气候变化适应')*('公平'+'正义'+'社会影响'+'公众参与'+
#        '弱势群体'+'脆弱群体'+'贫困'+'性别'+'治理')
#
# Steps
#   1. Read the CNKI export (.xls that is really an HTML table)
#   2. Year filter: OFF by default (all years kept, incl. pre-2008, 2026 and online first)
#   3. Remove duplicate titles
#   4. Automatic exclusion: Table 4 terms (mitigation focus) on title+keywords+abstract
#   5. Thematic helper: count hits of each cluster's keywords (suggestion only)
#   6. Write an Excel file for MANUAL title/abstract screening + a full log for PRISMA
#
# The inclusion terms are now inside the search string itself, so there is
# no separate inclusion filter (set use_inclusion_filter <- TRUE to re-enable it).
#
# HOW TO RUN (RStudio):
#   install.packages(c("rvest", "dplyr", "stringr", "purrr", "writexl"))   # once
#   Session > Set Working Directory > To Source File Location, then Source.

library(rvest)
library(dplyr)
library(stringr)
library(purrr)
library(writexl)


# ═══════════════════════════════════════════════════════════════════════════
# CONFIGURATION  (edit only this block)
# ═══════════════════════════════════════════════════════════════════════════

input_file  <- "CNKI-20261028231243282.xls"
output_file <- "screening_v2.xlsx"

# Year filter: NA = no limit. Example to restrict: year_min <- 2008; year_max <- 2025
year_min <- NA
year_max <- NA
# Records without a year are usually 'online first' articles
keep_no_year <- TRUE

# Exclusion terms = Table 4 of the manuscript (mitigation focus)
exclusion_terms <- "金融|双碳|碳汇|碳中和|碳市场|碳交易|低碳经济"

# Optional old inclusion filter (redundant with the new search string)
use_inclusion_filter <- FALSE
inclusion_terms <- "社会|发展|公共|参与|人民|正义"

# Thematic clusters (helper for MANUAL coding; does not decide anything)
themes <- list(
  "Policy & governance"           = c("适应战略", "立法", "政策", "环境影响评价", "国家安全", "治理"),
  "Agriculture & food security"   = c("农业", "节水", "粮食", "土地", "水资源"),
  "Urban resilience & planning"   = c("城市", "空间规划", "韧性", "灾害", "公共卫生"),
  "Financing mechanisms"          = c("资金", "财政", "投资", "融资"),
  "Science & technology support"  = c("科技", "技术", "监测", "转移"),
  "Social equity & participation" = c("公平", "正义", "参与", "脆弱", "弱势", "贫困", "性别",
                                      "权利", "移民", "社区")
)


# ═══════════════════════════════════════════════════════════════════════════
# FUNCTIONS
# ═══════════════════════════════════════════════════════════════════════════

# CNKI ".xls" exports are HTML tables in UTF-8
read_cnki_xls <- function(path) {
  if (!file.exists(path)) stop("File not found: ", path, "\nWorking directory: ", getwd())
  tab <- read_html(path, encoding = "UTF-8") %>% html_element("table") %>% html_table()
  # header can come as first row instead of column names
  if (!"Title-题名" %in% names(tab)) {
    names(tab) <- as.character(unlist(tab[1, ]))
    tab <- tab[-1, ]
  }
  tab <- tab %>% filter(`Title-题名` != "Title-题名")
  cat(sprintf("Loaded %d records from %s\n", nrow(tab), basename(path)))
  tab
}

terms_hit <- function(texts, pattern) {
  terms <- str_split(pattern, "\\|")[[1]]
  map_chr(texts, ~ paste(terms[str_detect(.x, fixed(terms))], collapse = "; "))
}

theme_helper <- function(texts) {
  scores <- map_dfc(themes, function(kw) map_int(texts, ~ sum(str_detect(.x, fixed(kw)))))
  best <- apply(scores, 1, function(s) if (max(s) == 0) "" else
    paste(names(themes)[s == max(s)], collapse = " / "))
  list(scores = scores, best = best)
}


# ═══════════════════════════════════════════════════════════════════════════
# MAIN
# ═══════════════════════════════════════════════════════════════════════════

main <- function() {
  df <- read_cnki_xls(input_file)
  n_identified <- nrow(df)

  df <- df %>%
    mutate(
      Year = suppressWarnings(as.integer(str_sub(as.character(`Year-年`), 1, 4))),
      title_key = str_remove_all(`Title-题名`, "[[:punct:][:space:]“”‘’「」《》（）—：，。、？！]"),
      combined_text = str_squish(paste(
        coalesce(as.character(`Title-题名`), ""),
        coalesce(as.character(`Keyword-关键词`), ""),
        coalesce(as.character(`Summary-摘要`), "")
      ))
    )

  # --- step 2-3: years and duplicates -------------------------------------
  df <- df %>%
    mutate(decision = case_when(
      is.na(Year) & !keep_no_year                     ~ "removed: no publication year (online first)",
      !is.na(Year) & !is.na(year_min) & Year < year_min ~ "removed: outside year range",
      !is.na(Year) & !is.na(year_max) & Year > year_max ~ "removed: outside year range",
      TRUE                                  ~ NA_character_
    ))
  in_range <- is.na(df$decision)
  dup <- in_range & duplicated(ifelse(in_range, df$title_key, paste0("x", seq_len(nrow(df)))))
  df$decision[dup] <- "removed: duplicate"

  # --- step 4: automatic exclusion ----------------------------------------
  todo <- is.na(df$decision)
  df$exclusion_terms_hit <- ""
  df$exclusion_terms_hit[todo] <- terms_hit(df$combined_text[todo], exclusion_terms)
  if (use_inclusion_filter) {
    no_inc <- todo & !str_detect(df$combined_text, inclusion_terms)
    df$decision[no_inc] <- "excluded: no inclusion term"
    todo <- is.na(df$decision)
  }
  df$decision[todo & df$exclusion_terms_hit != ""] <- "excluded: exclusion term (Table 4)"
  df$decision[is.na(df$decision)] <- "retained for manual screening"

  # --- step 5: thematic helper --------------------------------------------
  th <- theme_helper(df$combined_text)
  df$Suggested_cluster <- th$best

  # --- step 6: outputs ----------------------------------------------------
  retained <- df %>%
    filter(decision == "retained for manual screening") %>%
    mutate(ID = row_number()) %>%
    transmute(
      ID,
      Title = `Title-题名`, Authors = `Author-作者`, Journal = `Source-文献来源`, Year,
      Keywords = `Keyword-关键词`, Abstract = `Summary-摘要`,
      Suggested_cluster,
      `Decision (Include/Exclude)` = "",
      `Exclusion reason` = "",
      `Full text retrieved? (Y/N)` = "",
      `Thematic cluster (manual)` = "",
      Notes = "",
      URL = `URL-网址`, DOI = `DOI-DOI`
    )

  log <- df %>% transmute(Title = `Title-题名`, Journal = `Source-文献来源`, Year,
                          decision, exclusion_terms_hit, Suggested_cluster)

  counts <- tibble(
    Step = c("Identified (CNKI, academic journals)",
             "Removed: outside year range",
             "Removed: no publication year (online first)",
             "Removed: duplicates",
             "Screened automatically",
             "Excluded: exclusion term (Table 4)",
             "Excluded: no inclusion term",
             "Retained for manual title/abstract screening"),
    n = c(n_identified,
          sum(df$decision == "removed: outside year range"),
          sum(df$decision == "removed: no publication year (online first)"),
          sum(df$decision == "removed: duplicate"),
          sum(!str_starts(df$decision, "removed")),
          sum(df$decision == "excluded: exclusion term (Table 4)"),
          sum(df$decision == "excluded: no inclusion term"),
          nrow(retained))
  )

  write_xlsx(list("Manual screening" = retained, "Log (all records)" = log,
                  "PRISMA counts" = counts), output_file)
  print(counts, n = Inf)
  cat("\nWritten:", output_file, "\n")
  cat("Next: fill 'Decision' for each row; download full texts only for 'Include'.\n")
}

main()
