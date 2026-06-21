library(dplyr)
library(mixedpower)
library(stringr)


# ===========exp2==========

setwd("D:/OneDrive/projects/RM_action/data_ens/data_exp2/")

data2 <- readr::read_csv("data_RMaction_exp2.csv", show_col_types = FALSE) %>%
  filter(type != "overreport")

data_power <- data2 %>%
  mutate(
    participant_id = factor(participant),
    participant_num = as.numeric(factor(participant)),
    set_size = factor(setsize),
    type = relevel(factor(type), ref = "noRM")
  )

model <- lme4::lmer(
  error_x_deg ~ type * task + (1 | participant_id),
  data = data_power
)

summary(model)


# power
power_exp2 <- mixedpower(
  model = model,
  data = data_power,
  fixed_effects = c("type", "task"),
  simvar = "participant_num",
  steps = c(15, 20, 25, 30),
  critical_value = 2,
  n_sim = 1000
)


# save result
saveRDS(power_exp2, "power_exp2_result.rds")


capture.output(
  print(power_exp2),
  file = "power_exp2_result.txt"
)


