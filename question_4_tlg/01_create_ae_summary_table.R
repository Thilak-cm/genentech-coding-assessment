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

adae_te <- adae %>%
  filter(TRTEMFL == "Y")

# Overall TEAE row (subjects with ≥1 TEAE)
teae_any <- adae_te %>%
  distinct(USUBJID, ACTARM) %>%
  mutate(row_label = "Treatment Emergent AEs")

# SOC rows
soc_rows <- adae_te %>%
  distinct(USUBJID, ACTARM, AESOC) %>%
  mutate(row_label = AESOC)

# Preferred Term rows (indented)
pt_rows <- adae_te %>%
  distinct(USUBJID, ACTARM, AESOC, AETERM) %>%
  mutate(row_label = paste0("  ", AETERM))

# Combine all rows
adae_for_table <- bind_rows(
  teae_any,
  soc_rows,
  pt_rows
)


adsl_trt <- adsl %>%
  select(USUBJID, ACTARM)

ae_table <- adae_for_table %>%
  tbl_summary(
    by = ACTARM,
    include = row_label,
    statistic = all_categorical() ~ "{n} ({p}%)",
    percent = "column",
    missing = "no"
  ) %>%
  add_overall(last = TRUE) %>%
  modify_header(label = "**Primary System Organ Class / Preferred Term**") %>%
  bold_labels()

teae_any <- adae_te %>%
  distinct(USUBJID, ACTARM) %>%
  mutate(AESOC = "Treatment Emergent AEs")

adae_for_table <- bind_rows(
  teae_any,
  adae_te %>% select(USUBJID, ACTARM, AESOC)
)

ae_table <- ae_table %>%
  modify_table_body(
    ~ .x %>%
      arrange(desc(as.numeric(gsub(" .*", "", stat_0))))
  )


ae_table %>%
  as_gt() %>%
  gtsave("ae_summary_table.html")
