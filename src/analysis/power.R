library(dplyr)
library(mixedpower)
library(lme4)
library(stringr)

setwd("D:/OneDrive/projects/RM_action/src/analysis/")

# data
data <- readr::read_csv("data.csv", show_col_types = FALSE) %>%
  mutate(
    participant_label = participant,
    participant = case_when(
      participant_label == "pilot_march20" ~ 2L,
      participant_label == "PILOT868APRIL24" ~ 6L,
      TRUE ~ as.integer(str_match(
        participant_label,
        regex("^pilot_?(\\d+)", ignore_case = TRUE)
      )[, 2])
    )
  ) %>%
  filter(type != "overreport")

data_power <- data %>%
  mutate(
    participant_id = factor(participant),
    set_size = factor(set_size),
    type = relevel(factor(type), ref = "noRM"),
    dot_overshot = dot_deviaiton > 0
  )

# model
m_type_overshot <- lme4::glmer(
  dot_overshot ~ type * set_size + (1 | participant_id),
  data = data_power,
  family = binomial(link = "logit")
)

# power
power <- mixedpower(
  model = m_type_overshot,
  data = data_power,
  fixed_effects = c("type", "set_size"),
  simvar = "participant",
  steps = c(5, 10, 15, 20, 30),
  critical_value = 2,
  n_sim = 100
)


