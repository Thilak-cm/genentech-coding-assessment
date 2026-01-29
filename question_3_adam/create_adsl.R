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

adsl <- dm

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
      TRUE ~ NA_real_
    )
  )

adsl <- adsl %>%
  mutate(
    ITTFL = ifelse(!is.na(ARM), "Y", "N")
  )


ex_valid <- ex %>%
  filter(
    EXDOSE > 0 |
      (EXDOSE == 0 & grepl("PLACEBO", toupper(EXTRT)))
  )

ex_first <- ex_valid %>%
  filter(!is.na(EXSTDTC)) %>%
  arrange(USUBJID, EXSTDTC) %>%
  group_by(USUBJID) %>%
  slice(1) %>%
  ungroup() %>%
  select(USUBJID, EXSTDTC)

adsl <- adsl %>%
  left_join(ex_first, by = "USUBJID")

adsl <- derive_vars_dtm(
  dataset = adsl,
  new_vars_prefix = "TRTS",
  dtc = EXSTDTC
)

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
    !is.na(VSDTC),
    !(is.na(VSSTRESN) & is.na(VSSTRESC))
  ) %>%
  mutate(VS_DATE = as.Date(substr(VSDTC, 1, 10))) %>%
  group_by(USUBJID) %>%
  summarise(VS_LSTDT = max(VS_DATE, na.rm = TRUE), .groups = "drop")

ae_alive <- ae %>%
  filter(!is.na(AESTDTC)) %>%
  mutate(AE_DATE = as.Date(substr(AESTDTC, 1, 10))) %>%
  group_by(USUBJID) %>%
  summarise(
    AE_LSTDT = if (all(is.na(AE_DATE))) NA else max(AE_DATE, na.rm = TRUE),
    .groups = "drop"
  )

ds_alive <- ds %>%
  filter(!is.na(DSSTDTC)) %>%
  mutate(DS_DATE = as.Date(substr(DSSTDTC, 1, 10))) %>%
  group_by(USUBJID) %>%
  summarise(DS_LSTDT = max(DS_DATE, na.rm = TRUE), .groups = "drop")

ex_alive <- ex_valid %>%
  filter(!is.na(EXSTDTC)) %>%
  mutate(EX_DATE = as.Date(substr(EXSTDTC, 1, 10))) %>%
  group_by(USUBJID) %>%
  summarise(EX_LSTDT = max(EX_DATE, na.rm = TRUE), .groups = "drop")

adsl <- adsl %>%
  left_join(vs_alive, by = "USUBJID") %>%
  left_join(ae_alive, by = "USUBJID") %>%
  left_join(ds_alive, by = "USUBJID") %>%
  left_join(ex_alive, by = "USUBJID")

adsl <- adsl %>%
  mutate(
    LSTALVDT = pmax(
      VS_LSTDT,
      AE_LSTDT,
      DS_LSTDT,
      EX_LSTDT,
      na.rm = TRUE
    )
  )

adsl <- adsl %>%
  mutate(
    LSTALVDT = as.Date(LSTALVDT, origin = "1970-01-01")
  )

adsl <- adsl %>%
  mutate(
    LSTALVDT = if_else(
      is.infinite(as.numeric(LSTALVDT)),
      as.Date(NA),
      LSTALVDT
    )
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
    ABNSBPFL,
    CARPOPFL,
    LSTALVDT
  )

adsl_final
