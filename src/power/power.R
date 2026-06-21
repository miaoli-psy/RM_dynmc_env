library(dplyr)
library(mixedpower)
library(stringr)

# ===========exp1==========

setwd("D:/OneDrive/projects/RM_action/data_ens/data_exp1/")

# data
data <- readr::read_csv("data_RMaction_exp1.csv", show_col_types = FALSE) %>%
  filter(type != "overreport")

data_power <- data %>%
  mutate(
    participant_id = factor(participant),
    participant_num = as.numeric(factor(participant)),
    set_size = factor(set_size),
    type = relevel(factor(type), ref = "noRM"),
    dot_overshot = dot_deviation > 0 
  )

# model
m_type_overshot <- lme4::glmer(
  dot_overshot ~ type * set_size + (1 | participant_id),
  data = data_power,
  family = binomial(link = "logit")
)

# power
power_exp1 <- mixedpower(
  model = m_type_overshot,
  data = data_power,
  fixed_effects = c("type", "set_size"),
  simvar = "participant_num",
  steps = c(5, 10, 15, 20, 30),
  critical_value = 2,
  n_sim = 1000
)

# save result
saveRDS(power_exp1, "power_exp1_result.rds")

capture.output(
  print(power_exp1),
  file = "power_exp1_result.txt"
)

# 
# # read the results
# 
# setwd("D:/OneDrive/projects/RM_action/data_ens/data_exp1/")
# 
# power_exp1 <- readRDS("power_exp1_result.rds")
# 
# power_exp1
