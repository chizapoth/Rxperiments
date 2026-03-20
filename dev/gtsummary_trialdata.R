# this is the trial data set used in the gtsummary package

library(gtsummary)
trial

head(trial)
summary(trial)

trial$trt |> table()
trial$age |> summary()

trial$ttdeath |> hist()

# basic summary table
tbl_summary(trial, include = everything())

tbl_summary(data = trial, include = c(age, marker, grade))

# stratify by treatment arm
tbl_summary(data = trial, include = c(age, marker, grade), by = trt)

# I don't want missing values
# this ignores the missing
tbl_summary(
  data = trial,
  include = c(age, marker, grade),
  by = trt,
  missing = "no"
)


# trial data wide ------

# this is the long format
trial_mini <- select(trial, trt, age, stage) |> mutate(id = 1:nrow(trial))


head(trial_mini)

tbl_summary(trial_mini, by = trt, include = c(age, stage))
tbl_summary(trial_mini, by = trt, include = c(age, stage), missing = "no")

# wide format
trial_mini_wide <- trial_mini |>
  pivot_wider(names_from = stage, values_from = age)

trial_mini_wide

tbl_summary(
  trial_mini_wide,
  include = c('T1', 'T2', 'T3', 'T4'),
  missing = "no"
)

tbl_summary(
  trial_mini_wide,
  by = trt,
  include = c('T1', 'T2', 'T3', 'T4'),
  missing = 'no',
  label = list(
    'T1' = "Stage T1",
    'T2' = "Stage T2",
    'T3' = "Stage T3",
    'T4' = "Stage T4"
  )
)

tbl_summary(
  trial_mini_wide,
  by = trt,
  include = c('T1', 'T2', 'T3', 'T4'),
  missing = 'no',
  label = list(
    'T1' = "Stage T1",
    'T2' = "Stage T2",
    'T3' = "Stage T3",
    'T4' = "Stage T4"
  ),
  statistic = list(all_continuous() ~ "{mean} ({sd})")
)
