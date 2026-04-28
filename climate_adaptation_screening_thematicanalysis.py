#!/usr/bin/env python3
"""
Climate Adaptation Article Screening Script
============================================

This script performs systematic screening of Chinese-language articles on climate adaptation
from CNKI database according to PRISMA 2020 guidelines.

Input: CSV file exported from CNKI (semicolon-separated)
Output: Screened articles with thematic coding, exclusion reasons, and summary statistics

Author: Claude (Anthropic)
Date: April 2026
"""

import pandas as pd
import re
from collections import Counter
from typing import Tuple, List, Dict


# ═══════════════════════════════════════════════════════════════════════════
# CONFIGURATION
# ═══════════════════════════════════════════════════════════════════════════

# Define screening criteria
INCLUSION_TERMS = "社会|发展|公共|参与|人民|正义"
EXCLUSION_TERMS = "全球|金融|翻译|海洋|双碳|碳汇|碳中和|碳市场|碳交易|低碳经济"

# Thematic clusters (for manual coding reference)
THEMES = {
    'policy': {
        'name_en': 'Policy & governance',
        'name_zh': '政策与治理',
        'keywords': ['适应战略', '立法', '政策', '环境影响评价', '国家安全'],
    },
    'agri': {
        'name_en': 'Agriculture & food security',
        'name_zh': '农业与粮食安全',
        'keywords': ['农业', '节水', '粮食', '土地', '水资源'],
    },
    'urban': {
        'name_en': 'Urban resilience & planning',
        'name_zh': '城市韧性与规划',
        'keywords': ['城市', '空间规划', '生态', '韧性', '灾害', '公共卫生'],
    },
    'finance': {
        'name_en': 'Financing mechanisms',
        'name_zh': '资金机制',
        'keywords': ['资金', '财政', '投资', '融资', '机制'],
    },
    'tech': {
        'name_en': 'Science & technology support',
        'name_zh': '科技支撑',
        'keywords': ['科技', '技术', '监测', '系统', '转移'],
    },
    'equity': {
        'name_en': 'Social equity & participation',
        'name_zh': '社会公平与参与',
        'keywords': ['社会', '参与', '脆弱', '权利', '移民', '社区'],
    },
}


# ═══════════════════════════════════════════════════════════════════════════
# CORE SCREENING FUNCTIONS
# ═══════════════════════════════════════════════════════════════════════════

def read_cnki_export(filepath: str) -> pd.DataFrame:
    """
    Read CNKI export CSV file.
    
    CNKI exports are semicolon-delimited UTF-8 with Chinese column headers.
    Expected columns:
      - Title-题名
      - Keyword-关键词
      - Summary-摘要
      - Year-年
      - Source-文献来源
      - Author-作者
    
    Args:
        filepath: Path to CNKI CSV export
        
    Returns:
        DataFrame with article metadata
    """
    df = pd.read_csv(filepath, sep=';', encoding='utf-8-sig')
    print(f"✓ Loaded {len(df)} records from {filepath}")
    print(f"  Columns: {df.columns.tolist()}\n")
    return df


def combine_searchable_text(row: pd.Series) -> str:
    """
    Combine title, keywords, and abstract into single searchable text field.
    
    This is the text against which inclusion/exclusion terms are matched.
    Missing fields treated as empty strings.
    
    Args:
        row: DataFrame row
        
    Returns:
        Combined text string
    """
    parts = [
        str(row.get('Title-题名', '')),
        str(row.get('Keyword-关键词', '')),
        str(row.get('Summary-摘要', '')),
    ]
    return ' '.join([p for p in parts if p and p != 'nan'])


def check_inclusion(text: str, pattern: str) -> bool:
    """Check if text matches any inclusion term."""
    return bool(re.search(pattern, text, re.IGNORECASE))


def check_exclusion(text: str, pattern: str) -> bool:
    """Check if text matches any exclusion term."""
    return bool(re.search(pattern, text, re.IGNORECASE))


def get_exclusion_terms_hit(text: str, exclusion_pattern: str) -> List[str]:
    """
    Identify which specific exclusion terms matched.
    
    Returns list of matching terms for documentation.
    """
    terms = exclusion_pattern.split('|')
    hits = [term for term in terms if re.search(term, text, re.IGNORECASE)]
    return hits


def screen_articles(
    df: pd.DataFrame,
    inclusion_pattern: str,
    exclusion_pattern: str
) -> Tuple[pd.DataFrame, Dict]:
    """
    Apply inclusion and exclusion filters to article metadata.
    
    Args:
        df: Input DataFrame from CNKI
        inclusion_pattern: Regex pattern for inclusion terms
        exclusion_pattern: Regex pattern for exclusion terms
        
    Returns:
        Tuple of (screened_df, screening_stats_dict)
    """
    
    # Create combined text field
    df['_combined_text'] = df.apply(combine_searchable_text, axis=1)
    
    # Step 1: Apply inclusion filter
    df['_matches_inclusion'] = df['_combined_text'].apply(
        lambda x: check_inclusion(x, inclusion_pattern)
    )
    included_df = df[df['_matches_inclusion']].copy()
    
    # Step 2: Apply exclusion filter
    df['_matches_exclusion'] = df['_combined_text'].apply(
        lambda x: check_exclusion(x, exclusion_pattern)
    )
    excluded_df = included_df[included_df['_matches_exclusion']].copy()
    
    # Track which exclusion terms hit
    excluded_df['_exclusion_terms_hit'] = excluded_df['_combined_text'].apply(
        lambda x: get_exclusion_terms_hit(x, exclusion_pattern)
    )
    
    # Final screened set: inclusion match but no exclusion match
    screened_df = included_df[~included_df['_matches_exclusion']].copy()
    
    # Compile statistics
    stats = {
        'total_input': len(df),
        'matched_inclusion': len(included_df),
        'excluded_by_exclusion': len(excluded_df),
        'final_retained': len(screened_df),
        'retention_rate': round(100 * len(screened_df) / len(df), 1),
        'inclusion_match_rate': round(100 * len(included_df) / len(df), 1),
        'excluded_inclusion_match': len(df) - len(included_df),
    }
    
    return screened_df, stats


def analyze_keywords(df: pd.DataFrame, top_n: int = 30) -> Counter:
    """
    Extract and count all keywords from retained articles.
    
    Keywords split by semicolon, comma, or Chinese punctuation (；，).
    Common empty/invalid tokens filtered out.
    
    Args:
        df: Screened DataFrame
        top_n: Return top N keywords
        
    Returns:
        Counter object with keyword frequencies
    """
    all_keywords = []
    
    for kw_str in df['Keyword-关键词'].dropna():
        # Split by various delimiters
        keywords = re.split(r'[;;，,]', str(kw_str))
        for k in keywords:
            k = k.strip()
            # Filter empty or invalid entries
            if k and k not in ['nan', '', 'N/A', '无']:
                all_keywords.append(k)
    
    return Counter(all_keywords)


def analyze_temporal_distribution(df: pd.DataFrame) -> Dict:
    """
    Analyze publication year distribution.
    
    Returns dict with year as key, count as value.
    """
    return df['Year-年'].value_counts().sort_index().to_dict()


def analyze_sources(df: pd.DataFrame, top_n: int = 15) -> Dict:
    """
    Analyze publication source (journal) distribution.
    
    Returns dict with journal name as key, count as value (top N).
    """
    return df['Source-文献来源'].value_counts().head(top_n).to_dict()


# ═══════════════════════════════════════════════════════════════════════════
# OUTPUT GENERATION
# ═══════════════════════════════════════════════════════════════════════════

def print_screening_summary(stats: Dict) -> None:
    """Print summary statistics of screening process."""
    print("\n" + "="*70)
    print("SCREENING SUMMARY")
    print("="*70)
    print(f"Total records in input:        {stats['total_input']}")
    print(f"Matched inclusion criteria:    {stats['matched_inclusion']} ({stats['inclusion_match_rate']}%)")
    print(f"Excluded by exclusion terms:   {stats['excluded_by_exclusion']}")
    print(f"Final retained for analysis:   {stats['final_retained']}")
    print(f"Overall retention rate:        {stats['retention_rate']}%")
    print("="*70 + "\n")


def print_screened_articles(df: pd.DataFrame) -> None:
    """Print full details of screened articles."""
    print("\n" + "="*70)
    print(f"SCREENED ARTICLES ({len(df)} total)")
    print("="*70 + "\n")
    
    for idx, (i, row) in enumerate(df.iterrows(), 1):
        title = row.get('Title-题名', 'N/A')
        year = row.get('Year-年', 'N/A')
        journal = row.get('Source-文献来源', 'N/A')
        keywords = row.get('Keyword-关键词', 'N/A')
        abstract = str(row.get('Summary-摘要', 'N/A'))[:250]
        
        print(f"\n[{idx}] {title}")
        print(f"     Year: {year} | Journal: {journal}")
        print(f"     Keywords: {keywords}")
        print(f"     Abstract: {abstract}...")
        print("     " + "-"*66)


def print_keyword_analysis(keyword_counter: Counter, top_n: int = 30) -> None:
    """Print top keywords from screened articles."""
    print("\n" + "="*70)
    print(f"TOP {top_n} KEYWORDS")
    print("="*70 + "\n")
    
    for kw, count in keyword_counter.most_common(top_n):
        print(f"  {kw:20s} : {count:3d}")
    print()


def print_temporal_analysis(year_dist: Dict) -> None:
    """Print articles by publication year."""
    print("\n" + "="*70)
    print("PUBLICATION TIMELINE")
    print("="*70 + "\n")
    
    for year in sorted(year_dist.keys()):
        count = year_dist[year]
        bar = "█" * count
        print(f"  {year}  {bar}  ({count})")
    print()


def print_journal_analysis(source_dist: Dict) -> None:
    """Print articles by journal/source."""
    print("\n" + "="*70)
    print("TOP JOURNALS")
    print("="*70 + "\n")
    
    for journal, count in source_dist.items():
        print(f"  {journal:35s} : {count:2d}")
    print()


def export_screened_csv(
    df: pd.DataFrame,
    output_path: str,
    inclusion_pattern: str,
    exclusion_pattern: str
) -> None:
    """
    Export screened articles to CSV for further manual review or thematic coding.
    
    Args:
        df: Screened DataFrame
        output_path: Path to output CSV file
        inclusion_pattern: Inclusion terms (for documentation)
        exclusion_pattern: Exclusion terms (for documentation)
    """
    
    # Select relevant columns and reorder
    export_cols = [
        'Title-题名',
        'Year-年',
        'Source-文献来源',
        'Author-作者',
        'Keyword-关键词',
        'Summary-摘要',
    ]
    
    export_df = df[[col for col in export_cols if col in df.columns]].copy()
    
    # Add screening metadata
    export_df['Screening_Inclusion_Terms'] = inclusion_pattern
    export_df['Screening_Exclusion_Terms'] = exclusion_pattern
    export_df['Manual_Theme_Code'] = ''  # Column for human coder to fill in
    export_df['Retrievable'] = ''        # Column to track full-text access
    export_df['Notes'] = ''              # Column for coder notes
    
    export_df.to_csv(output_path, sep=';', encoding='utf-8-sig', index=False)
    print(f"\n✓ Exported {len(export_df)} screened articles to: {output_path}")


# ═══════════════════════════════════════════════════════════════════════════
# MAIN EXECUTION
# ═══════════════════════════════════════════════════════════════════════════

def main(input_csv_path: str, output_csv_path: str = None):
    """
    Main screening pipeline.
    
    Args:
        input_csv_path: Path to CNKI export CSV
        output_csv_path: Path for screened articles CSV (optional)
    """
    
    print("\n" + "🔍 CLIMATE ADAPTATION ARTICLE SCREENING ".center(70, "="))
    print("CNKI Systematic Review Pipeline (PRISMA 2020)\n")
    
    # Load data
    df = read_cnki_export(input_csv_path)
    
    # Screen articles
    screened, stats = screen_articles(df, INCLUSION_TERMS, EXCLUSION_TERMS)
    
    # Print results
    print_screening_summary(stats)
    
    # Analyze screened set
    keywords = analyze_keywords(screened)
    years = analyze_temporal_distribution(screened)
    journals = analyze_sources(screened)
    
    print_keyword_analysis(keywords, top_n=30)
    print_temporal_analysis(years)
    print_journal_analysis(journals)
    
    print_screened_articles(screened)
    
    # Export for manual review
    if output_csv_path is None:
        output_csv_path = input_csv_path.replace('.csv', '_SCREENED.csv')
    
    export_screened_csv(
        screened,
        output_csv_path,
        INCLUSION_TERMS,
        EXCLUSION_TERMS
    )
    
    print("\n" + "="*70)
    print("Next steps:")
    print("  1. Check full-text retrievability for all retained articles")
    print("  2. Manually code each article with thematic cluster")
    print("  3. Review and validate thematic assignments with co-coder")
    print("="*70 + "\n")
    
    return screened


if __name__ == "__main__":
    # Example usage:
    input_file = "/mnt/user-data/uploads/CNKI-20260428103549247_last.csv"
    output_file = "/mnt/user-data/outputs/screened_articles.csv"
    
    screened_df = main(input_file, output_file)
