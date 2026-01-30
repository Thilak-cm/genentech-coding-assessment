# ------------------------------------------------------------------------------
# Program: 01_create_ae_summary_table.R
# Purpose: Create TEAE summary table (FDA Table 10 style)
# Input: pharmaverseadam::adae, pharmaverseadam::adsl
# Output: ae_summary_table.html
# ------------------------------------------------------------------------------

library(dplyr)
library(gtsummary)
library(gt)
library(pharmaverseadam)


data("adae", package = "pharmaverseadam")
data("adsl", package = "pharmaverseadam")

# Use ACTARM from ADSL for consistent treatment labels/denominators.
# Drop non-treatment arm(s) like Screen Failure from the table columns.
adsl_trt <- adsl %>%
  select(USUBJID, ACTARM) %>%
  filter(!is.na(ACTARM)) %>%
  filter(toupper(ACTARM) != "SCREEN FAILURE")

adae_te <- adae %>%
  filter(TRTEMFL == "Y") %>%
  select(-ACTARM) %>%
  left_join(adsl_trt, by = "USUBJID")

# Denominators: all subjects in ADSL by treatment arm (ACTARM)
denom_by_arm <- adsl_trt %>%
  distinct(USUBJID, ACTARM) %>%
  count(ACTARM, name = "N")

# Build distinct subject-level counts for TEAE any / SOC / PT
teae_any <- adae_te %>%
  distinct(USUBJID, ACTARM) %>%
  mutate(
    AESOC = "Treatment Emergent AEs",
    AETERM = NA_character_
  )

soc_rows <- adae_te %>%
  distinct(USUBJID, ACTARM, AESOC) %>%
  mutate(AETERM = NA_character_)

pt_rows <- adae_te %>%
  distinct(USUBJID, ACTARM, AESOC, AETERM)

counts_long <- bind_rows(teae_any, soc_rows, pt_rows) %>%
  mutate(
    row_type = case_when(
      AESOC == "Treatment Emergent AEs" ~ "ANY",
      !is.na(AESOC) & is.na(AETERM) ~ "SOC",
      TRUE ~ "PT"
    )
  ) %>%
  group_by(row_type, AESOC, AETERM, ACTARM) %>%
  summarise(n = n_distinct(USUBJID), .groups = "drop")

# Overall counts (all arms) for sorting (not displayed as a column)
counts_overall <- counts_long %>%
  group_by(row_type, AESOC, AETERM) %>%
  summarise(n = sum(n), .groups = "drop") %>%
  mutate(ACTARM = "Overall")

# Helper to format "n (p%)" with ADSL denominators
fmt_npct <- function(n, denom) {
  ifelse(
    is.na(n) | is.na(denom) | denom == 0,
    "0 (0%)",
    sprintf("%d (%.0f%%)", n, 100 * n / denom)
  )
}

# Order SOCs by overall frequency, and PTs within SOC by overall frequency
soc_order <- counts_overall %>%
  filter(row_type == "SOC") %>%
  arrange(desc(n), AESOC) %>%
  distinct(AESOC) %>%
  mutate(soc_ord = row_number())

pt_order <- counts_overall %>%
  filter(row_type == "PT") %>%
  arrange(AESOC, desc(n), AETERM) %>%
  group_by(AESOC) %>%
  mutate(pt_ord = row_number()) %>%
  ungroup() %>%
  select(AESOC, AETERM, pt_ord)

table_rows <- tibble::tibble(
  row_type = c("ANY"),
  AESOC = c("Treatment Emergent AEs"),
  AETERM = c(NA_character_)
) %>%
  bind_rows(
    soc_order %>%
      transmute(row_type = "SOC", AESOC, AETERM = NA_character_)
  ) %>%
  bind_rows(
    soc_order %>%
      left_join(pt_order, by = "AESOC") %>%
      arrange(soc_ord, pt_ord) %>%
      transmute(row_type = "PT", AESOC, AETERM)
  ) %>%
  distinct()

# Assemble final display labels with indentation for PT
table_rows <- table_rows %>%
  mutate(
    label = dplyr::case_when(
      row_type == "ANY" ~ "Treatment Emergent AEs",
      row_type == "SOC" ~ AESOC,
      TRUE ~ paste0("  ", AETERM)
    )
  )

# Pivot counts wide and format per-arm columns
arms <- denom_by_arm %>% arrange(ACTARM) %>% pull(ACTARM)

wide_counts <- counts_long %>%
  select(row_type, AESOC, AETERM, ACTARM, n) %>%
  tidyr::pivot_wider(names_from = ACTARM, values_from = n)

table_df <- table_rows %>%
  left_join(wide_counts, by = c("row_type", "AESOC", "AETERM"))

for (i in seq_along(arms)) {
  arm <- arms[[i]]
  denom <- denom_by_arm$N[match(arm, denom_by_arm$ACTARM)]
  col_nm <- paste0("stat_", i)
  
  # If an arm has zero TEAEs, it may not appear as a column after pivoting.
  n_vec <- if (arm %in% names(table_df)) table_df[[arm]] else rep(0L, nrow(table_df))
  table_df[[col_nm]] <- fmt_npct(n_vec, denom)
}

table_body <- table_df %>%
  transmute(
    variable = "AESOC_AETERM",
    row_type = "label",
    label = .data$label,
    dplyr::across(dplyr::starts_with("stat_"))
  )

# Create a gtsummary object from the pre-computed table body
ae_table <- as_gtsummary(table_body) %>%
  modify_header(label = "**Primary System Organ Class / Preferred Term**") %>%
  bold_labels()

# Add column headers with denominators
header_list <- c()
for (i in seq_along(arms)) {
  arm <- arms[[i]]
  n_arm <- denom_by_arm$N[match(arm, denom_by_arm$ACTARM)]
  header_list[[paste0("stat_", i)]] <- paste0("**", arm, "**<br>N = ", n_arm)
}

ae_table <- ae_table %>%
  modify_header(!!!header_list)

# Hide internal columns if they appear in output
ae_table <- ae_table %>%
  modify_column_hide(columns = c(variable, row_type))

ae_table %>%
  as_gt() %>%
  gtsave("ae_summary_table.html")
