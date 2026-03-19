library(tidyverse)
library(readxl)
options(scipen=999)
rm(list=ls())
library(WDI)

# pop_data= WDI(indicator =  c( "SP.POP.TOTL","SP.POP.TOTL.MA.ZS","SP.POP.0014.TO.ZS"),start=2024,end=2024) 
# pop_data=pop_data %>% select(Area=country,Area.ID=iso3c,population=SP.POP.TOTL,prop_under14=SP.POP.0014.TO.ZS)
# pop_data$adult_pop=pop_data$population*(1-pop_data$prop_under14/100)
# pop_data=pop_data %>% select(Area,Area.ID,population,adult_pop)

dir="/Users/adenooy/Library/CloudStorage/OneDrive-Personal/AMC/HIV Prioritization tool/HIV_prioritization_tool/data/who_hts_dashboard/"
files=list.files(path = dir)

all_data=data.frame()
length(files)
for(i in 1: length(files)){
data=read.csv(paste(dir,files[i],sep=""))
cntry=files[i]
cntry=gsub("WHO HTS Data - ","",cntry)
cntry=gsub(".csv","",cntry)
data$Area.ID=cntry

all_data=rbind(all_data,data)

}


all_data=all_data %>% filter(year>=2023) %>%  group_by(Area.ID, chart,indicator,sex,age) %>% mutate(max_year=max(year)) %>% filter(year==max_year)
all_data$label=paste(all_data$chart,all_data$indicator,sep="&")

write.csv(all_data,"/Users/adenooy/Downloads/hts_data.csv")

# =============================================================================
# WHO HTS Dashboard – Modality Enrichment Multiplier Analysis
# =============================================================================
# Reads the raw CSV exported from the WHO HTS dashboard and produces an Excel
# workbook with three sheets:
#   1. Modality Multipliers  – one row per country, multiplier per modality
#   2. Positivity + Multiplier – paired raw positivity and multiplier columns
#   3. Modality Summary      – median / mean / min / max multiplier per modality
#
# Definition: multiplier = modality_positivity / total_positivity
# A value >1 means that modality finds HIV-positive individuals at a higher
# rate than the country average; <1 means lower than average.
#
# Requirements: install.packages(c("dplyr", "tidyr", "openxlsx"))
# =============================================================================

library(dplyr)
library(tidyr)
library(openxlsx)

# =============================================================================
# 0.  Configuration
# =============================================================================

INPUT_CSV  <- "/Users/adenooy/Downloads/hts_data.csv"   # path to your downloaded CSV
OUTPUT_XLS <- "/Users/adenooy/Library/CloudStorage/OneDrive-Personal/AMC/HIV Prioritization tool/HIV_prioritization_tool/data/WHO_HTS_Multipliers.xlsx"


# =============================================================================
# 1. Modality mapping  (name, volume indicators, positivity indicators)
#    First matching indicator with non-NA data in the most recent year is used
# =============================================================================

MODALITIES <- list(
  list("Total",            c("Den Age-All"),
       c("Per Age-All")),
  list("Total Female",     c("Den Age-Female Gte 15"),
       c("Per Age-Female Gte 15")),
  list("Total Male",       c("Den Age-Male Gte 15"),
       c("Per Age-Male Gte 15")),
  list("Community (All)",  c("Den Community-Community All", "Total Community Tests"),
       c("Per Community-Community All", "Positivity - Community Modalities Total")),
  list("Community Mobile", c("Den Community-Community Mobile", "Mobile testing - Number of tests - Community"),
       c("Per Community-Community Mobile", "Positivity - Community Mobile Testing")),
  list("Community VCT",    c("Den Community-Community Vct", "VCT - Number of tests - Community"),
       c("Per Community-Community Vct", "Positivity - Community VCT Testing")),
  list("Community Other",  c("Den Community-Community Other", "Other - Number of tests - Community"),
       c("Per Community-Community Other", "Positivity - Community Other Testing")),
  list("Facility (All)",   c("Den Facility-Facility All"),
       c("Per Facility-Facility All")),
  list("Facility PITC",    c("Den Facility-Facility Provider Init", "PITC - Number of tests - Facility"),
       c("Per Facility-Facility Provider Init", "Positivity - Facility PITC Testing")),
  list("Facility ANC",     c("Den Facility-Facility Anc", "ANC - Number of tests - Facility"),
       c("Per Facility-Facility Anc", "Positivity - Facility ANC Testing")),
  list("Facility VCT",     c("Den Facility-Facility Vct", "VCT - Number of tests - Facility"),
       c("Per Facility-Facility Vct", "Positivity - Facility VCT Testing")),
  list("Facility FP Clinic",c("Den Facility-Facility Fp Clinic"),
       c("Per Facility-Facility Fp Clinic")),
  list("Index (Total)",    c("Total Index tests"),
       c("Positivity - Index Testing Total")),
  list("Index Community",  c("Index - Number of tests - Community"),
       c("Positivity - Community Index testing")),
  list("Index Facility",   c("Index - Number of tests - Facility"),
       c("Positivity - Facility Index Testing")),
  list("Self-Test",        c("Self Test Distributed-Data Value"),
       c())
)

# =============================================================================
# 2. Load data
# =============================================================================

raw <- read.csv(INPUT_CSV, stringsAsFactors = FALSE, check.names = FALSE)
raw <- raw[, !(is.na(names(raw)) | names(raw) == "")]   # drop unnamed index col

# =============================================================================
# 3. Helper: best (most recent non-NA) value for a set of indicator names
# =============================================================================

best_value <- function(cdf, indicator_names) {
  rows <- cdf |>
    filter(indicator %in% indicator_names, !is.na(value)) |>
    arrange(desc(year)) |>
    slice(1)
  if (nrow(rows) == 0) return(list(value = NA_real_, year = NA_integer_))
  list(value = rows$value[1], year = as.integer(rows$year[1]))
}

# =============================================================================
# 4. Build wide table
# =============================================================================

countries <- sort(unique(raw$Area.ID))

wide <- bind_rows(lapply(countries, function(iso) {
  cdf <- filter(raw, Area.ID == iso)
  row <- list(Country = iso)
  
  for (mod in MODALITIES) {
    nm       <- mod[[1]]
    vol_inds <- mod[[2]]
    pos_inds <- mod[[3]]
    
    vol <- best_value(cdf, vol_inds)
    row[[paste0(nm, "_volume")]] <- vol$value
    row[[paste0(nm, "_vol_yr")]] <- vol$year
    
    if (length(pos_inds) > 0) {
      pos <- best_value(cdf, pos_inds)
      row[[paste0(nm, "_positivity")]] <- pos$value
      row[[paste0(nm, "_pos_yr")]]     <- pos$year
    }
  }
  as.data.frame(row, stringsAsFactors = FALSE, check.names = FALSE)
}))

# =============================================================================
# 5. Compute multipliers  (modality positivity / total positivity)
# =============================================================================

for (mod in MODALITIES) {
  nm <- mod[[1]]
  if (nm == "Total" || length(mod[[3]]) == 0) next
  pos_col  <- paste0(nm, "_positivity")
  mult_col <- paste0(nm, "_multiplier")
  if (pos_col %in% names(wide)) {
    wide[[mult_col]] <- round(wide[[pos_col]] / wide[["Total_positivity"]], 2)
  }
}

# =============================================================================
# 6. Re-order columns: Country, then for each modality: vol, pos, multiplier
# =============================================================================

ordered_cols <- "Country"
for (mod in MODALITIES) {
  nm <- mod[[1]]
  ordered_cols <- c(ordered_cols, paste0(nm, "_volume"), paste0(nm, "_vol_yr"))
  if (length(mod[[3]]) > 0) {
    ordered_cols <- c(ordered_cols, paste0(nm, "_positivity"), paste0(nm, "_pos_yr"))
    if (nm != "Total") ordered_cols <- c(ordered_cols, paste0(nm, "_multiplier"))
  }
}
ordered_cols <- ordered_cols[ordered_cols %in% names(wide)]
wide <- wide[, ordered_cols]

# Sort by total positivity descending
wide <- arrange(wide, desc(Total_positivity))

# =============================================================================
# 7. Write Excel – single sheet, minimal styling
# =============================================================================

wb <- createWorkbook()
addWorksheet(wb, "HTS Multipliers")

# Two header rows:
#   Row 1: modality group label (merged across its sub-columns)
#   Row 2: sub-column names (Volume, Yr, Positivity%, Yr, Multiplier)

hdr1 <- "Country"
hdr2 <- "Country"

for (mod in MODALITIES) {
  nm      <- mod[[1]]
  has_pos <- length(mod[[3]]) > 0
  is_ref  <- nm == "Total"
  
  sub <- c("Volume", "Yr")
  if (has_pos) {
    sub <- c(sub, "Positivity%", "Yr")
    if (!is_ref) sub <- c(sub, "Multiplier")
  }
  hdr1 <- c(hdr1, nm, rep("", length(sub) - 1))
  hdr2 <- c(hdr2, sub)
}

writeData(wb, "HTS Multipliers", as.data.frame(t(hdr1)),
          startRow = 1, startCol = 1, colNames = FALSE)
writeData(wb, "HTS Multipliers", as.data.frame(t(hdr2)),
          startRow = 2, startCol = 1, colNames = FALSE)

# Merge modality group header cells across their sub-columns
col_ptr <- 2
for (mod in MODALITIES) {
  nm      <- mod[[1]]
  has_pos <- length(mod[[3]]) > 0
  is_ref  <- nm == "Total"
  n_sub   <- 2 + (if (has_pos) 2 else 0) + (if (has_pos && !is_ref) 1 else 0)
  if (n_sub > 1) mergeCells(wb, "HTS Multipliers", cols = col_ptr:(col_ptr + n_sub - 1), rows = 1)
  col_ptr <- col_ptr + n_sub
}

# Header styles
hdr_style <- createStyle(fontName = "Arial", fontSize = 9, textDecoration = "bold",
                         fgFill = "#2E5DA6", fontColour = "#FFFFFF",
                         halign = "center", valign = "center", wrapText = TRUE)
addStyle(wb, "HTS Multipliers", hdr_style,
         rows = 1:2, cols = 1:length(hdr2), gridExpand = TRUE)

# Write data
writeData(wb, "HTS Multipliers", wide, startRow = 3, startCol = 1, colNames = FALSE)

# Alternating row shading
for (i in seq_len(nrow(wide))) {
  if (i %% 2 == 0) {
    addStyle(wb, "HTS Multipliers", createStyle(fgFill = "#F2F7FF"),
             rows = i + 2, cols = 1:ncol(wide), gridExpand = TRUE)
  }
}

# Number formats
data_rows <- 3:(nrow(wide) + 2)
col_ptr   <- 2
for (mod in MODALITIES) {
  nm      <- mod[[1]]
  has_pos <- length(mod[[3]]) > 0
  is_ref  <- nm == "Total"
  
  addStyle(wb, "HTS Multipliers", createStyle(numFmt = "#,##0", halign = "right"),
           rows = data_rows, cols = col_ptr, gridExpand = TRUE, stack = TRUE)
  col_ptr <- col_ptr + 2   # skip Yr column
  
  if (has_pos) {
    addStyle(wb, "HTS Multipliers", createStyle(numFmt = "0.0", halign = "right"),
             rows = data_rows, cols = col_ptr, gridExpand = TRUE, stack = TRUE)
    col_ptr <- col_ptr + 2   # skip Yr column
    if (!is_ref) {
      addStyle(wb, "HTS Multipliers", createStyle(numFmt = '0.0"x"', halign = "center"),
               rows = data_rows, cols = col_ptr, gridExpand = TRUE, stack = TRUE)
      col_ptr <- col_ptr + 1
    }
  }
}

# Column widths and freeze
setColWidths(wb, "HTS Multipliers", cols = 1,            widths = 8)
setColWidths(wb, "HTS Multipliers", cols = 2:length(hdr2), widths = 9)
setRowHeights(wb, "HTS Multipliers", rows = 1:2, heights = 28)
freezePane(wb, "HTS Multipliers", firstActiveRow = 3, firstActiveCol = 2)

saveWorkbook(wb, OUTPUT_XLS, overwrite = TRUE)
message("Saved: ", OUTPUT_XLS)
