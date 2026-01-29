# ------------------------------------------------------------------------------
# Program: 03_create_listings.R
# Purpose: Create treatment-emergent AE listings
# Input: pharmaverseadam::adae
# Output: ae_listings.html
# ------------------------------------------------------------------------------

library(dplyr)
library(gt)
library(pharmaverseadam)

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
  arrange(USUBJID, AESTDTC)

# Create listing table
ae_listing_gt <- ae_listing %>%
  gt(groupname_col = "USUBJID") %>%
  tab_header(
    title = "Listing of Treatment-Emergent Adverse Events",
    subtitle = "Sorted by Subject and Event Start Date"
  ) %>%
  cols_label(
    USUBJID = "Subject ID",
    ACTARM  = "Treatment",
    AETERM  = "Adverse Event",
    AESEV   = "Severity",
    AEREL   = "Relationship",
    AESTDTC = "Start Date",
    AEENDTC = "End Date"
  ) %>%
  cols_hide(USUBJID) %>%   # hide repeated subject column
  opt_all_caps() %>%
  tab_options(
    row_group.font.weight = "bold",
    table.font.size = px(12)
  )


# Export to HTML
gtsave(ae_listing_gt, "ae_listings.html")
