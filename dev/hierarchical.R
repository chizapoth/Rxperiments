# hierarchical tables
# both gtsummary and crane

library(syntheticadam)
library(tidyverse)

adsl <- syntheticadam::adsl

adae_subset <- syntheticadam::adae |>
  dplyr::filter(
    AESOC %in%
      c(
        "Cardiac disorders",
        "Gastrointestinal disorders",
        "Nervous system disorders",
        "Psychiatric disorders"
      ),
    AETERM %in%
      c("ATRIAL FIBRILLATION", "DIARRHOEA", "DIZZINESS", "AMNESIA", "INSOMNIA")
  )


# basic eda

adae_subset |> count(AESOC, AETERM)


# gtsummary hierarchical table
# without stratification

tbl_hierarchical(
  data = adae_subset,
  variables = c(AESOC, AETERM),
  # by = TRT01A,
  denominator = adsl,
  id = USUBJID
)

# now stratify by treatment arm
tbl_hierarchical(
  data = adae_subset,
  variables = c(AESOC, AETERM),
  by = TRT01A,
  denominator = adsl,
  id = USUBJID
)

# overall row
tbl_hierarchical(
  data = adae_subset,
  variables = c(AESOC, AETERM),
  by = TRT01A,
  denominator = adsl,
  id = USUBJID,
  include = 'AETERM',
  overall_row = TRUE
)

# counts only
tbl_hierarchical_count(
  data = adae_subset,
  variables = c(AESOC, AETERM),
  by = TRT01A,
  overall_row = TRUE,
  # denominator = adsl,
  include = 'AETERM'
)

# customization -----
# add overall

tbl_hierarchical_count(
  data = adae_subset,
  variables = c(AESOC, AETERM),
  by = TRT01A,
  overall_row = TRUE,
  # denominator = adsl,
  include = 'AETERM'
) |>
  add_overall()

# sort
tbl_hierarchical_count(
  data = adae_subset,
  variables = c(AESOC, AETERM),
  by = TRT01A,
  include = 'AETERM'
) |>
  sort_hierarchical(
    sort = c(AESOC ~ "descending")
  )

# filtering
tbl_hierarchical_count(
  data = adae_subset,
  variables = c(AESOC, AETERM),
  by = TRT01A,
  include = 'AETERM'
) |>
  add_overall() |>
  filter_hierarchical(n_overall > 10)


# rate and count (crane) -----
library(crane)

# adds one row for total number of events  for each  AESOC

tbl_hierarchical_rate_and_count(
  data = adae_subset,
  variables = c(AESOC, AETERM),
  by = TRT01A,
  denominator = adsl,
  id = USUBJID
)


# rate by grade ------

# has one more variable, AETOXGR, ae toxicity grade

tbl_hierarchical_rate_by_grade(
  data = adae_subset,
  variables = c(AESOC, AETERM, AETOXGR),
  by = TRT01A,
  denominator = adsl,
  id = USUBJID
)
