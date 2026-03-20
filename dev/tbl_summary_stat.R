# compare groups

# compare groups
# I have two groups, so appropriate tests are selected:
# wilcoxon for continuous, chi-squared for categorical
tbl_summary(
  data = trial,
  include = c(age, marker, grade),
  by = trt,
  missing = "no"
) |>
  add_p()

# regression ------
library(survival)

mod1 <- glm(response ~ trt + age + grade, data = trial, family = binomial)
summary(mod1)

# now put it in the model
# with logistic reg (or anything that involves logs), can use exponentiat
tbl_regression(mod1)
tbl_regression(mod1, exponentiate = TRUE)

# second model: cox model
mod2 <- coxph(Surv(ttdeath, death) ~ trt + age + grade, data = trial)
summary(mod2)
# use the same function to wrap mod2
tbl_regression(mod2, exponentiate = TRUE)


# can use a summary to confirm it
tbl_summary(data = trial, include = c(death, ttdeath, grade), by = grade) |>
  add_p()


# merge tables ------
# needs a list of tables
t1 <- tbl_regression(mod1, exponentiate = TRUE)
t2 <- tbl_regression(mod2, exponentiate = TRUE)

# these two tables have the same covariates
tbl_merge(
  tbls = list(
    t1,
    t2
  )
)

# want to change the name
tbl_merge(
  tbls = list(
    t1,
    t2
  ),
  tab_spanner = c("**Tumor Response**", "**Time to Death**")
)
