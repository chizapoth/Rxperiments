# test out main crane tables
library(crane)


# tbl_baseline_chg -----

df <- cards::ADLB |>
  dplyr::mutate(AVISIT = trimws(AVISIT)) |>
  dplyr::filter(
    AVISIT != "End of Treatment",
    PARAMCD %in% c("SODIUM", "K")
  )

tbl_baseline_chg(
  data = df |> dplyr::filter(PARAMCD == "SODIUM"),
  baseline_level = "Baseline",
  by = "TRTA",
  denominator = cards::ADSL
)

tbl_baseline_chg(
  data = df |> dplyr::filter(PARAMCD == "K"),
  baseline_level = "Baseline",
  by = "TRTA",
  denominator = cards::ADSL
)

# Split by PARAM
# tbl_strata(
#   data = df,
#   strata = PARAMCD,
#   .tbl_fun = ~ tbl_baseline_chg(
#     data = .x,
#     baseline_level = "Baseline",
#     by = "TRTA",
#     denominator = cards::ADSL
#   ),
#   .combine_with = "tbl_stack",
#   .combine_args = list(group_header = NULL, quiet = TRUE)
# ) |>
#   tbl_split_by_rows(variable_level = ends_with("lbl"))

# tbl_shift ------

adlb <- pharmaverseadam::adlb |>
  dplyr::select(
    "USUBJID",
    "TRT01A",
    "PARAM",
    "PARAMCD",
    "ATOXGRH",
    "BTOXGRH",
    "VISITNUM"
  ) |>
  mutate(TRT01A = factor(TRT01A)) |>
  dplyr::filter(PARAMCD %in% c("CHOLES", "GLUC")) |>
  slice_max(
    by = c(USUBJID, PARAMCD),
    order_by = ATOXGRH,
    n = 1L,
    with_ties = FALSE
  ) |>
  labelled::set_variable_labels(
    BTOXGRH = "Baseline  \nNCI-CTCAE Grade",
    ATOXGRH = "Post-baseline  \nNCI-CTCAE Grade"
  )
adsl <- pharmaverseadam::adsl[c("USUBJID", "TRT01A")] |>
  dplyr::filter(TRT01A != "Screen Failure")


# tabulate baseline grade by worst grade
# adlb here is one row per person

d_lab <- dplyr::filter(adlb, PARAMCD %in% "CHOLES")
d_lab$ATOXGRH |> table() # 0,1,2: 234, 8, 12
d_lab$BTOXGRH |> table() # 0,1,2: 245, 1, 8

# the help file says variable is typically the post-baseline grade
# strata is the baseline grade
# strata would be the first column, variable would be the second column

filter(d_lab, BTOXGRH == 0) |> select(ATOXGRH) |> table()


tbl_shift(
  data = d_lab,
  strata = BTOXGRH, # baseline
  variable = ATOXGRH, # post baseline
  #by = TRT01A,
  data_header = adsl
) |>
  add_overall()


# subgroup analysis -----

# prepare sample data
df_adtte <- data.frame(
  time = rexp(100, rate = 0.1),
  status = sample(c(0, 1), 100, replace = TRUE),
  arm = sample(c("Arm A", "Arm B"), 100, replace = TRUE),
  grade = sample(c("I", "II"), 100, replace = TRUE),
  strata = sample(c("1", "2"), 100, replace = TRUE)
) |>
  mutate(arm = relevel(factor(arm), ref = "Arm A")) # Set Reference

head(df_adtte)

df_adtte |>
  tbl_roche_subgroups(
    rsp = "status",
    by = "arm",
    subgroups = c("grade"),
    .tbl_fun = ~ glm(status ~ arm, data = .x) |>
      tbl_regression(
        show_single_row = arm,
        exponentiate = TRUE # , tidy_fun = broom.helpers::tidy_parameters
      )
  ) |>
  modify_header(starts_with("estimate") ~ "**Odds Ratio**")


# tbl_listing -----
trial_data <- trial |>
  dplyr::select(trt, age, marker, stage) |>
  dplyr::filter(stage %in% c("T2", "T3")) |>
  dplyr::slice_head(n = 2, by = c(trt, stage)) |> # downsampling
  dplyr::arrange(trt, stage) |> # key columns should be sorted
  dplyr::relocate(trt, stage) # key columns should be first

# Example 1 --------------------------------
out <- tbl_listing(trial_data)
out
out |> remove_duplicate_keys(keys = "trt")


show_header_names(lst)
grps <- list(c("trt", "stage", "age"), c("trt", "stage", "marker"))
list_lst <- tbl_listing(trial_data, split_by_columns = list(groups = grps))
list_lst[[2]]
