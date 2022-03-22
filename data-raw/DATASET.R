library(tidyverse)
library(readxl)
library(janitor)



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
