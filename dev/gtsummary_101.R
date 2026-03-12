# test gtsummary functionalities

library(gtsummary)
library(tidyverse)

# data prep
adsl <- syntheticadam::adsl |>
  select(USUBJID, ARM, TRT01A, AGE, ETHNIC, SEX, TRTDURD) |>
  mutate(FEMALE = SEX == "F") |>
  labelled::set_variable_labels(FEMALE = "Female")

adae <- pharmaverseadam::adae |>
  filter(
    USUBJID %in% adsl$USUBJID,
    AESOC %in% c("CARDIAC DISORDERS", "EYE DISORDERS"),
    AEDECOD %in%
      c(
        "ATRIAL FLUTTER",
        "MYOCARDIAL INFARCTION",
        "EYE ALLERGY",
        "EYE SWELLING"
      )
  ) |>
  select(USUBJID, TRT01A, AEDECOD, AESOC, AEHLT, AESEV, AESER, AEREL)


adsl
adae

adsl |>
  tbl_summary(
    include = c(AGE, ETHNIC, FEMALE)
  )
