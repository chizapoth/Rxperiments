# exploring tbl_summary() with wide format data

library(gtsummary)
library(tidyr)
library(dplyr)

# create example wide format data ------
# simulating repeated measures across timepoints
set.seed(123)
n <- 100

wide_data <- tibble(
  id = 1:n,
  group = sample(c("Treatment", "Control"), n, replace = TRUE),
  age = rnorm(n, mean = 50, sd = 10),
  # blood pressure at 3 timepoints (wide format)
  bp_baseline = rnorm(n, mean = 130, sd = 15),
  bp_week4 = rnorm(n, mean = 125, sd = 15),
  bp_week8 = rnorm(n, mean = 120, sd = 15),
  # response at multiple visits (wide format)
  response_visit1 = sample(
    c("Yes", "No"),
    n,
    replace = TRUE,
    prob = c(0.3, 0.7)
  ),
  response_visit2 = sample(
    c("Yes", "No"),
    n,
    replace = TRUE,
    prob = c(0.5, 0.5)
  ),
  response_visit3 = sample(
    c("Yes", "No"),
    n,
    replace = TRUE,
    prob = c(0.7, 0.3)
  )
)

head(wide_data)


# basic summary of wide data ------
# tbl_summary() will treat each column as a separate variable
tbl_summary(wide_data, include = -id)

# stratified by group
tbl_summary(wide_data, include = -id, by = group)


# the issue: columns are summarized independently ------
# this works but doesn't capture the longitudinal structure
# each bp_* column is treated as a separate variable
tbl_summary(
  wide_data,
  include = c(bp_baseline, bp_week4, bp_week8),
  by = group,
  label = list(
    bp_baseline ~ "Baseline",
    bp_week4 ~ "Week 4",
    bp_week8 ~ "Week 8"
  ),
  statistic = all_continuous() ~ "{mean} ({sd})"
) |>
  modify_header(label = "**Blood Pressure**")


# converting to long format ------
# for proper longitudinal analysis, pivot to long format
long_bp <- wide_data |>
  select(id, group, starts_with("bp_")) |>
  pivot_longer(
    cols = starts_with("bp_"),
    names_to = "timepoint",
    values_to = "blood_pressure",
    names_prefix = "bp_"
  )

head(long_bp)

# summary by timepoint (long format)
tbl_summary(
  long_bp,
  include = blood_pressure,
  by = timepoint,
  statistic = all_continuous() ~ "{mean} ({sd})"
) |>
  add_p()


# stratified summary with long format ------
# can now properly stratify by both group and timepoint
tbl_strata(
  data = long_bp,
  strata = group,
  .tbl_fun = ~ tbl_summary(
    .x,
    include = blood_pressure,
    by = timepoint,
    statistic = all_continuous() ~ "{mean} ({sd})"
  )
)


# comparing change from baseline ------
# calculate change while data is still wide
wide_data <- wide_data |>
  mutate(
    bp_change_week4 = bp_week4 - bp_baseline,
    bp_change_week8 = bp_week8 - bp_baseline
  )

tbl_summary(
  wide_data,
  include = c(bp_change_week4, bp_change_week8),
  by = group,
  label = list(
    bp_change_week4 ~ "Change at Week 4",
    bp_change_week8 ~ "Change at Week 8"
  ),
  statistic = all_continuous() ~ "{mean} ({sd})"
) |>
  add_p()


# categorical wide data (response over visits) ------
# summarize each visit separately
tbl_summary(
  wide_data,
  include = c(response_visit1, response_visit2, response_visit3),
  by = group,
  label = list(
    response_visit1 ~ "Visit 1",
    response_visit2 ~ "Visit 2",
    response_visit3 ~ "Visit 3"
  )
) |>
  add_p()

# pivot categorical data to long
long_response <- wide_data |>
  select(id, group, starts_with("response_")) |>
  pivot_longer(
    cols = starts_with("response_"),
    names_to = "visit",
    values_to = "response",
    names_prefix = "response_"
  )

# summary by visit
tbl_summary(
  long_response,
  include = response,
  by = visit
) |>
  add_p()


# using tbl_wide_summary() for compact display ------
# this function is designed for wide summary tables
tbl_wide_summary(
  wide_data,
  include = c(bp_baseline, bp_week4, bp_week8),
  statistic = c("{mean}", "{sd}")
)

# by group
tbl_wide_summary(
  wide_data,
  include = c(bp_baseline, bp_week4, bp_week8),
  statistic = c("{mean}", "{sd}"),
  by = group
)
