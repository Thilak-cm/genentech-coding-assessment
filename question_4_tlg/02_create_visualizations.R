# ------------------------------------------------------------------------------
# Program: 02_create_visualizations.R
# Purpose: Create AE visualizations (severity + top 10 AEs)
# Input: pharmaverseadam::adae, pharmaverseadam::adsl
# Output: PNG files
# ------------------------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(binom)

library(pharmaverseadam)

data("adae", package = "pharmaverseadam")
data("adsl", package = "pharmaverseadam")

adae_te <- adae %>%
  filter(TRTEMFL == "Y")

p_sev <- adae_te %>%
  filter(!is.na(AESEV), !is.na(ACTARM)) %>%
  ggplot(aes(x = ACTARM, fill = AESEV)) +
  geom_bar(position = "stack") +
  labs(
    y = "Count of AEs"
  )
  labs(
    title = "Distribution of AE Severity by Treatment",
    x = "Treatment",
    y = "Percentage of TEAEs",
    fill = "AE Severity"
  ) +
  theme_minimal()

ggsave(
  filename = "ae_severity_by_treatment.png",
  plot = p_sev,
  width = 8,
  height = 5,
  dpi = 300
)

denom <- adae_te %>% distinct(USUBJID) %>% nrow()

top10_ae <- adae_te %>%
  distinct(USUBJID, AETERM) %>%
  count(AETERM, name = "n") %>%
  arrange(desc(n)) %>%
  slice_head(n = 10) %>%
  mutate(
    ci = binom.confint(n, denom, methods = "exact"),
    pct = 100 * n / denom,
    lower = 100 * ci$lower,
    upper = 100 * ci$upper
  )

p_top10 <- top10_ae %>%
  ggplot(aes(x = pct, y = reorder(AETERM, pct))) +
  geom_point(size = 2) +
  geom_errorbarh(
    aes(xmin = lower, xmax = upper),
    height = 0.2
  ) +
  labs(
    title = "Top 10 Most Frequent Adverse Events",
    subtitle = paste0(
      "n = ", denom,
      " subjects; 95% Clopper-Pearson CIs"
    ),
    x = "Percentage of Patients (%)",
    y = "Adverse Event"
  ) +
  theme_minimal()


ggsave(
  filename = "top10_ae_incidence.png",
  plot = p_top10,
  width = 8,
  height = 5,
  dpi = 300
)
