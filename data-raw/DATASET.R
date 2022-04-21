library(tidyverse)
library(readxl)
library(janitor)
library(faux)
library(mice)



# Placebo Amplification ---------------------------------------------------
# Dataset from Claus et al., (2020)
# https://osf.io/rc754/
claus_import <- read_excel("data-raw/claus-2020.xlsx") %>%
  select(id, age, sex, treatment, matches("(BDI|HAMD|SHAPS|WHO).*total$")) %>%
  clean_names()

claus_2020 <- claus_import %>%
  pivot_longer(
    cols = bdi1_total:hamd4_total,
    names_to = c(".value", "time"),
    names_pattern = "(^.*)(\\d{1})"
  ) %>%
  mutate(
    sex = factor(sex, levels = 0:1, labels = c("Female", "Male")),
    treatment = factor(treatment, levels = 0:1, labels = c("TAU", "PA")),
    time = as.numeric(time)
  ) %>%
  as_tibble()

use_data(claus_2020, overwrite = TRUE)



# Marital Therapy ---------------------------------------------------------
# Dataset from Jacobson et al., 1989
jacobson_import <- read_excel("data-raw/jacobson-1989.xlsx")

jacobson_1989 <- jacobson_import %>%
  arrange(subject)

use_data(jacobson_1989, overwrite = TRUE)




# Anxiety Dataset ---------------------------------------------------------
# Fictional data set to demonstrate HLM method.
# Intercept
gamma_00 <- 35
u_0j <- 6

# Time
gamma_10 <- -1
u_1j <- 2.5

# Treatment
gamma_20 <- -2
u_2j <- 1

# Interaction
gamma_30 <- 2

set.seed(20220419)
anxiety_complete <- add_random(subject = 116) %>%
  add_between(treatment = c("Placebo", "Intervention"), .shuffle = TRUE) %>%
  add_within(measurement = 1:5) %>%
  add_recode("measurement", "code_time", "1" = 0, "2" = 1, "3" = 2, "4" = 3, "5" = 4) %>%
  add_recode("treatment", "code_treatment", Placebo = 0, Intervention = gamma_20) %>%
  add_ranef("subject", u_0j = u_0j) %>%
  add_ranef("subject", u_1j = u_1j) %>%
  add_ranef("subject", u_2j = u_2j) %>%
  add_ranef(sigma = 3.5) %>%
  mutate(
    outcome = gamma_00 + u_0j + (gamma_10 + u_1j) * code_time + (gamma_20 + u_2j) * code_treatment + gamma_30 * code_time * code_treatment + sigma,
    outcome = pmax(outcome, 0),
    subject = str_to_title(subject)
  ) %>%
  select(subject, treatment, measurement = code_time, anxiety = outcome)

use_data(anxiety_complete, overwrite = TRUE)


# Anxiety dataset with missings
missing_pattern <- rbind(
  c(1, 1, 1, 1, 1, 1, 0),
  c(1, 1, 1, 1, 1, 0, 0),
  c(1, 1, 1, 1, 0, 0, 0),
  c(1, 1, 1, 0, 0, 0, 0)
)

set.seed(20220420)
anxiety <- anxiety %>%
  pivot_wider(
    names_from = measurement,
    names_prefix = "t_",
    values_from = anxiety
  ) %>%
  ampute(prop = 0.25, type = "MID", mech = "MCAR", patterns = missing_pattern, freq = c(0.7, 0.2, 0.05, 0.05)) %>%
  pluck("amp") %>%
  as_tibble() %>%
  pivot_longer(
    cols = t_0:t_4,
    names_to = "measurement",
    values_to = "anxiety"
  ) %>%
  mutate(measurement = parse_number(measurement))


use_data(anxiety, overwrite = TRUE)




# Visual Checks -----------------------------------------------------------
anxiety %>%
  ggplot(aes(measurement, anxiety, color = treatment)) +
  geom_line(aes(group = subject)) +
  facet_wrap(~ treatment)


anxiety_missings %>%
  ggplot(aes(measurement, anxiety, color = treatment)) +
  geom_line(aes(group = subject)) +
  facet_wrap(~ treatment)


test_object <- anxiety_missings %>%
  clinical_significance(subject, measurement, anxiety, group = treatment, method = "HLM")



# Plot as Usual -----------------------------------------------------------
cutoff_val <- get_cutoff(test_object)[["value"]]

get_augmented_data(test_object) %>%
  ggplot(aes(pre, post)) +
  geom_point(aes(color = category)) +
  geom_abline() +
  geom_vline(xintercept = cutoff_val, lty = 2) +
  geom_hline(yintercept = cutoff_val, lty = 2) +
  expand_limits(x = c(0, 60), y = c(0, 60)) +
  theme_light()


# Empirical Plot ----------------------------------------------------------
empirical_data <- get_augmented_data(test_object) %>%
  select(-group)

test_object$datasets$model_data %>%
  left_join(empirical_data, by = "id") %>%
  ggplot(aes(time, outcome)) +
  geom_line(aes(group = id, color = category), na.rm = TRUE) +
  facet_grid(~ group) +
  theme_light()




# Fitted Plot -------------------------------------------------------------
.calc_slope_data <- function(intercept, slope, min, max) {
  tibble(
    time = seq(min, max),
    fitted = intercept + time * slope
  )
}

clin_groups <- get_augmented_data(test_object) %>%
  select(-n, -rci)

test_object$rci$coefficients %>%
  left_join(clin_groups, by = "id") %>%
  mutate(
    data = map2(intercept, eb_slope, ~ .calc_slope_data(.x, .y, 0, 4))
  ) %>%
  unnest(data) %>%
  ggplot(aes(time, fitted)) +
  geom_line(aes(group = id, color = category)) +
  facet_wrap(~ group) +
  theme_light()


get_augmented_data(test_object)

test_cat <- test_object$categories

test_coef <- test_object$rci$coefficients %>%
  select(id, intercept, slope, eb_slope)


test_cat %>%
  left_join(test_coef, by = "id") %>%
  relocate(intercept:eb_slope, .after = post)
