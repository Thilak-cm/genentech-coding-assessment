# ------------------------------------------------------------------------------
# Program: 03_create_listings.R
# Purpose: Create treatment-emergent AE listings
# Input: pharmaverseadam::adae
# Output: ae_listings.html
# ------------------------------------------------------------------------------

library(dplyr)
library(gtsummary)
library(gt)

library(pharmaverseadam)

data("adae", package = "pharmaverseadam")

# Filter to treatment-emergent AEs and select required variables
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
  arrange(USUBJID, AESTDTC)

# Create listing table
ae_listing_tbl <- ae_listing %>%
  tbl_summary(
    by = USUBJID,
    statistic = all_categorical() ~ "{level}",
    missing = "no"
  ) %>%
  modify_header(label = "**Adverse Event Listing**") %>%
  bold_labels()

# Export to HTML
ae_listing_tbl %>%
  as_gt() %>%
  gtsave("ae_listings.html")
