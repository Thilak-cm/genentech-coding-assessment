# ------------------------------------------------------------------------------
# Program: create_adsl.R
# Purpose: Create ADaM ADSL dataset using {admiral}
# Input: SDTM domains (DM, VS, EX, DS, AE)
# Output: ADSL
# ------------------------------------------------------------------------------

library(dplyr)
library(lubridate)

library(admiral)
library(pharmaversesdtm)

data("dm", package = "pharmaversesdtm")
data("vs", package = "pharmaversesdtm")
data("ex", package = "pharmaversesdtm")
data("ds", package = "pharmaversesdtm")
data("ae", package = "pharmaversesdtm")

adsl <- dm %>%
  mutate(
    DOMAIN = "ADSL"
  )

adsl <- adsl %>%
  mutate(
    AGEGR9 = case_when(
      AGE < 18 ~ "<18",
      AGE >= 18 & AGE <= 50 ~ "18 - 50",
      AGE > 50 ~ ">50",
      TRUE ~ NA_character_
    ),
    AGEGR9N = case_when(
      AGE < 18 ~ 1,
      AGE >= 18 & AGE <= 50 ~ 2,
      AGE > 50 ~ 3,
      TRUE ~ NA_integer_
    )
  )

adsl <- adsl %>%
  mutate(
    ITTFL = ifelse(!is.na(ARM), "Y", "N")
  )


# Helper: identify records with a complete ISO date part (YYYY-MM-DD)
has_complete_datepart <- function(x) {
  !is.na(x) & grepl("^\\d{4}-\\d{2}-\\d{2}", x)
}

ex_valid <- ex %>%
  filter(
    EXDOSE > 0 |
      (EXDOSE == 0 & grepl("PLACEBO", toupper(EXTRT)))
  ) %>%
  # Per spec: only consider exposures where the date part is complete
  filter(has_complete_datepart(EXSTDTC))

# Derive EXSTDTM with time imputation and an imputation flag.
# Requirement: impute missing hours/minutes (and seconds to 00), but do NOT flag if
# only seconds were imputed. `ignore_seconds_flag = TRUE` matches that requirement.
ex_valid_dtm <- derive_vars_dtm(
  dataset = ex_valid,
  new_vars_prefix = "EXST",
  dtc = EXSTDTC,
  highest_imputation = "s",
  time_imputation = "00:00:00",
  ignore_seconds_flag = TRUE
)

ex_trt_summ <- ex_valid_dtm %>%
  arrange(USUBJID, EXSTDTM, EXSTDTC) %>%
  group_by(USUBJID) %>%
  summarise(
    # TRTSDTM: first valid-dose exposure datetime (with time imputation)
    TRTSDTM = first(EXSTDTM),
    TRTSTMF = first(EXSTTMF),
    # TRTEDTM: last valid-dose exposure datetime (used for LSTALVDT candidate 4)
    TRTEDTM = last(EXSTDTM),
    .groups = "drop"
  )

adsl <- adsl %>%
  left_join(ex_trt_summ, by = "USUBJID")

vs_abn_sbp <- vs %>%
  filter(
    VSTESTCD == "SYSBP",
    VSSTRESU == "mmHg",
    !is.na(VSSTRESN),
    VSSTRESN < 100 | VSSTRESN >= 140
  ) %>%
  distinct(USUBJID) %>%
  mutate(ABNSBPFL = "Y")

adsl <- adsl %>%
  left_join(vs_abn_sbp, by = "USUBJID") %>%
  mutate(
    ABNSBPFL = ifelse(is.na(ABNSBPFL), "N", ABNSBPFL)
  )


ae_cardiac <- ae %>%
  filter(!is.na(AESOC), toupper(AESOC) == "CARDIAC DISORDERS") %>%
  distinct(USUBJID) %>%
  mutate(CARPOPFL = "Y")

adsl <- adsl %>%
  left_join(ae_cardiac, by = "USUBJID")

vs_alive <- vs %>%
  filter(
    has_complete_datepart(VSDTC),
    !(is.na(VSSTRESN) & is.na(VSSTRESC))
  ) %>%
  mutate(VS_DATE = as.Date(substr(VSDTC, 1, 10))) %>%
  group_by(USUBJID) %>%
  summarise(
    VS_LSTDT = if (all(is.na(VS_DATE))) as.Date(NA) else max(VS_DATE, na.rm = TRUE),
    .groups = "drop"
  )

ae_alive <- ae %>%
  filter(has_complete_datepart(AESTDTC)) %>%
  mutate(AE_DATE = as.Date(substr(AESTDTC, 1, 10))) %>%
  group_by(USUBJID) %>%
  summarise(
    AE_LSTDT = if (all(is.na(AE_DATE))) as.Date(NA) else max(AE_DATE, na.rm = TRUE),
    .groups = "drop"
  )

ds_alive <- ds %>%
  filter(has_complete_datepart(DSSTDTC)) %>%
  mutate(DS_DATE = as.Date(substr(DSSTDTC, 1, 10))) %>%
  group_by(USUBJID) %>%
  summarise(
    DS_LSTDT = if (all(is.na(DS_DATE))) as.Date(NA) else max(DS_DATE, na.rm = TRUE),
    .groups = "drop"
  )

ex_alive <- adsl %>%
  transmute(
    USUBJID,
    # Per spec: use datepart of TRTEDTM (Datetime of Last Exposure to Treatment)
    EX_LSTDT = as.Date(TRTEDTM)
  )

adsl <- adsl %>%
  left_join(vs_alive, by = "USUBJID") %>%
  left_join(ae_alive, by = "USUBJID") %>%
  left_join(ds_alive, by = "USUBJID") %>%
  left_join(ex_alive, by = "USUBJID")

adsl <- adsl %>%
  mutate(
    # pmax() on Date columns can drop class; do pmax on numeric dates then convert back.
    LSTALVDT_NUM = pmax(
      as.numeric(VS_LSTDT),
      as.numeric(AE_LSTDT),
      as.numeric(DS_LSTDT),
      as.numeric(EX_LSTDT),
      na.rm = TRUE
    ),
    LSTALVDT_NUM = if_else(is.infinite(LSTALVDT_NUM), NA_real_, LSTALVDT_NUM),
    LSTALVDT = as.Date(LSTALVDT_NUM, origin = "1970-01-01")
  )

adsl_final <- adsl %>%
  select(
    STUDYID,
    DOMAIN,
    USUBJID,
    SUBJID,
    SITEID,
    AGE,
    AGEU,
    AGEGR9,
    AGEGR9N,
    SEX,
    RACE,
    ETHNIC,
    ARM,
    ARMCD,
    ACTARM,
    ACTARMCD,
    ITTFL,
    TRTSDTM,
    TRTSTMF,
    ABNSBPFL,
    CARPOPFL,
    LSTALVDT
  )

adsl_final
