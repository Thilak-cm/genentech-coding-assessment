# ------------------------------------------------------------------------------
# Program: 02_create_ds_domain.R
# Purpose: Create SDTM DS domain using {sdtm.oak}
# Input: pharmaverseraw::ds_raw
# Output: DS domain
# ------------------------------------------------------------------------------

library(dplyr)
library(sdtm.oak)
library(pharmaverseraw)

data("ds_raw", package = "pharmaverseraw")

study_ct <- data.frame(
  stringsAsFactors = FALSE,
  codelist_code = rep("C66727", 10),
  term_code = c(
    "C41331","C25250","C28554","C48226","C48227",
    "C48250","C142185","C49628","C49632","C49634"
  ),
  term_value = c(
    "ADVERSE EVENT","COMPLETED","DEATH","LACK OF EFFICACY",
    "LOST TO FOLLOW-UP","PHYSICIAN DECISION",
    "PROTOCOL VIOLATION","SCREEN FAILURE",
    "STUDY TERMINATED BY SPONSOR","WITHDRAWAL BY SUBJECT"
  ),
  collected_value = c(
    "Adverse Event","Complete","Dead","Lack of Efficacy",
    "Lost To Follow-Up","Physician Decision",
    "Protocol Violation","Trial Screen Failure",
    "Study Terminated By Sponsor","Withdrawal by Subject"
  ),
  term_preferred_term = c(
    "AE","Completed","Died",NA,NA,NA,
    "Violation","Failure to Meet Inclusion/Exclusion Criteria",NA,"Dropout"
  ),
  term_synonyms = c(
    "ADVERSE EVENT","COMPLETE","Death",NA,NA,NA,NA,NA,NA,
    "Discontinued Participation"
  )
)

ds <- ds_raw %>%
  mutate(
    STUDYID = STUDY,
    DOMAIN  = "DS",
    USUBJID = paste(STUDY, PATNUM, sep = "-")
  )

ds <- ds %>%
  mutate(
    DSTERM = ifelse(
      !is.na(OTHERSP),
      OTHERSP,
      IT.DSDECOD
    ),
    DSDECOD = IT.DSDECOD,
    DSCAT   = "DISPOSITION EVENT"
  )

# Apply study-specific controlled terminology using {sdtm.oak} structure
ds <- ds %>%
  left_join(
    study_ct,
    by = c("DSTERM" = "collected_value")
  ) %>%
  mutate(
    DSDECOD = coalesce(term_preferred_term, DSDECOD)
  )


ds <- ds %>%
  mutate(
    DS_DATE = as.Date(IT.DSSTDAT, format = "%d-%m-%Y")
  )

ds <- ds %>%
  mutate(
    DSDTC = ifelse(
      is.na(DSTMCOL),
      format(DS_DATE, "%Y-%m-%d"),
      paste0(format(DS_DATE, "%Y-%m-%d"), "T", DSTMCOL)
    )
  )

ds <- ds %>%
  mutate(
    DSSTDTC = DSDTC
  )

ref_dates <- ds %>%
  group_by(USUBJID) %>%
  summarise(
    REF_DATE = if (all(is.na(DS_DATE))) NA else min(DS_DATE, na.rm = TRUE),
    .groups = "drop"
  )

ds <- ds %>%
  left_join(ref_dates, by = "USUBJID") %>%
  mutate(
    DSSTDY = ifelse(
      is.na(REF_DATE),
      NA_integer_,
      as.integer(DS_DATE - REF_DATE + 1)
    )
  )

ds <- ds %>%
  arrange(USUBJID, DSDTC) %>%
  group_by(USUBJID) %>%
  mutate(DSSEQ = row_number()) %>%
  ungroup()

ds_final <- ds %>%
  select(
    STUDYID,
    DOMAIN,
    USUBJID,
    DSSEQ,
    DSTERM,
    DSDECOD,
    DSCAT,
    VISITNUM = INSTANCE,
    VISIT    = INSTANCE,
    DSDTC,
    DSSTDTC,
    DSSTDY
  )

ds_final

# print(ds[1:3,8:16])

