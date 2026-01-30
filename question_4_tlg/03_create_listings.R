# ------------------------------------------------------------------------------
# Program: 03_create_listings.R
# Purpose: Create treatment-emergent AE listings
# Input: pharmaverseadam::adae
# Output: ae_listings.html
# ------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(gtsummary)
  library(gt)
  library(crane)
  library(pharmaverseadam)
})

data("adae", package = "pharmaverseadam")

# Create AE listing data
ae_listing <- adae %>%
  filter(TRTEMFL == "Y") %>%
  select(
    USUBJID,
    ACTARM,
    AETERM,
    AESEV,
    AEREL,
    AESTDTC,
    AEENDTC
  ) %>%
  # Match sample output note: exclude Screen Failure patients
  filter(!is.na(ACTARM), toupper(ACTARM) != "SCREEN FAILURE") %>%
  arrange(USUBJID, AESTDTC)

# Add labels so listing headers match the sample output
attr(ae_listing$USUBJID, "label") <- "Unique Subject Identifier"
attr(ae_listing$ACTARM, "label") <- "Description of Actual Arm"
attr(ae_listing$AETERM, "label") <- "Reported Term for the Adverse Event"
attr(ae_listing$AESEV, "label") <- "Severity/Intensity"
attr(ae_listing$AEREL, "label") <- "Causality"
attr(ae_listing$AESTDTC, "label") <- "Start Date/Time of Adverse Event"
attr(ae_listing$AEENDTC, "label") <- "End Date/Time of Adverse Event"

# Create listing using {crane} (supplements {gtsummary}) and format by subject:
# - add blank row between subjects
# - suppress repeated keys (subject and arm)
listing_tbl <- crane::tbl_listing(
  ae_listing,
  add_blank_rows = list(variable_level = "USUBJID")
) %>%
  crane::remove_duplicate_keys(keys = c("USUBJID", "ACTARM"))

listing_gt <- as_gt(listing_tbl) %>%
  gt::tab_header(
    title = "Listing of Treatment-Emergent Adverse Events by Subject",
    subtitle = "Excluding Screen Failure Patients"
  ) %>%
  gt::tab_options(
    table.font.size = gt::px(12)
  )

gtsave(listing_gt, "ae_listings.html")
