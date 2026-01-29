# ------------------------------------------------------------------------------
# Program: export_adae_csv.R
# Purpose: Export ADAE dataset for Python FastAPI use
# Output: adae.csv
# ------------------------------------------------------------------------------

library(pharmaverseadam)
library(readr)

data("adae", package = "pharmaverseadam")

# Optional: ensure character columns are not factors
adae_export <- adae

# Write CSV
write_csv(
  adae_export,
  "data/adae.csv"
)

message("adae.csv successfully written")
