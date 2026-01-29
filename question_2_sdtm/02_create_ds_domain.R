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

study_ct <- rbind(
  study_ct,
  data.frame(
    stringsAsFactors = FALSE,
    codelist_code = "C66727",
    term_code = NA_character_,
    term_value = c(
      "SCREEN FAILURE",
      "STUDY TERMINATED BY SPONSOR",
      "LOST TO FOLLOW-UP",
      "RANDOMIZED"
    ),
    collected_value = c(
      "Screen Failure",
      "Study Terminated by Sponsor",
      "Lost to Follow-Up",
      "Randomized"
    ),
    term_preferred_term = NA_character_,
    term_synonyms = NA_character_
  )
)

ds <- ds_raw %>%
  mutate(
    STUDYID = STUDY,
    DOMAIN = "DS",
    USUBJID = paste(STUDY, PATNUM, sep = "-"),
    DSCAT = "DISPOSITION EVENT",
    VISITNUM = INSTANCE,
    VISIT = as.character(INSTANCE),
    # Topic: use "Other, specify" when present, else the collected disposition value
    DSTERM = ifelse(!is.na(OTHERSP), OTHERSP, IT.DSDECOD),
    # ISO8601 datetime (date + optional time)
    DSDTC = sdtm.oak::create_iso8601(
      IT.DSSTDAT,
      DSTMCOL,
      .format = list(
        c("d-m-y"),
        c("H:M", "H:M:S", "HH:MM", "HH:MM:SS")
      ),
      .na = c("UN", "UNK"),
      .warn = TRUE
    ),
    DSSTDTC = DSDTC,
    # Parsed Date for study day derivation
    DS_DATE = as.Date(IT.DSSTDAT, format = "%d-%m-%Y"),
    # Controlled terminology mapping for DSDECOD to standard term_value
    DSDECOD = sdtm.oak::ct_map(
      x = IT.DSDECOD,
      ct_spec = study_ct,
      ct_clst = "C66727",
      from = c("collected_value", "term_synonyms", "term_preferred_term"),
      to = "term_value"
    )
  )

ref_dates <- ds %>%
  group_by(USUBJID) %>%
  summarise(
    REF_DATE = if (all(is.na(DS_DATE))) as.Date(NA) else min(DS_DATE, na.rm = TRUE),
    .groups = "drop"
  )

ds <- ds %>%
  left_join(ref_dates, by = "USUBJID") %>%
  mutate(
    DSSTDY = ifelse(is.na(REF_DATE), NA_integer_, as.integer(DS_DATE - REF_DATE + 1L))
  ) %>%
  arrange(STUDYID, USUBJID, DSDTC, DSTERM) %>%
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
    VISITNUM,
    VISIT,
    DSDTC,
    DSSTDTC,
    DSSTDY
  ) %>%
  arrange(STUDYID, USUBJID, DSSEQ)

ds_final

stopifnot(all(c("STUDYID","DOMAIN","USUBJID","DSSEQ","DSTERM","DSDECOD","DSCAT",
                "VISITNUM","VISIT","DSDTC","DSSTDTC","DSSTDY") %in% names(ds_final)))