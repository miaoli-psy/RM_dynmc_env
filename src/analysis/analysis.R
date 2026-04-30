library(dplyr)
library(ggplot2)

# # set working dir
# setwd("D:/OneDrive/projects/RM_action/src/analysis/")


# read data
data <- readr::read_csv("data.csv")

data$set_size <- factor(data$set_size)

# check age. sex

df_check_age <- data %>%
  group_by(participant, age, sex) %>% 
  tally() 

mean(df_check_age$age)

# RM in number task

data_by_participant <- data %>%
  group_by(participant, set_size) %>% 
  summarise(
    mean_num_dv = mean(number_deviation, drop.na = TRUE),
    n = n(),
    .groups = "drop"
  )


data_across_participant <- data_by_participant %>% 
  group_by(set_size) %>% 
  summarise(
    avg_num_dv = mean(mean_num_dv),
    sd_num_dv = sd(mean_num_dv),
    n = n() 
  ) %>% 
  mutate(
    sem_num_dv = sd_num_dv/sqrt(n),
    ci_num_dv =qt(0.975,(n-1)) * sem_num_dv
  )


str(data_across_participant)

my_plot <- ggplot() +
  
  geom_point(
    data = data_across_participant,
    aes(
      x = set_size,
      y = avg_num_dv,
      color = set_size
    ),
    size = 3
  )+
  
  geom_point(
    data = data_by_participant,
    aes(
      x = as.factor(set_size),
      y = mean_num_dv,
      color = set_size,
      alpha =0.2
        ),
    size =1
  )+
  geom_errorbar(
    data = data_across_participant,
    aes(
      x = set_size,
      y = avg_num_dv,
      ymin= avg_num_dv - sem_num_dv,
      ymax = avg_num_dv + sem_num_dv
    ),
    color = "black",
    width = 0.2
  )+
  
  scale_y_continuous(
    limits = c(-1, 1),
    breaks = seq(-1, 1, by = 0.2)
  ) +

  theme_minimal()

my_plot

# percentage RM trials per condition

data_by_participant <- data %>%
  group_by(participant, set_size) %>% 
  summarise(
    percentage_RM = mean(number_deviation < 0) * 100,
    n = n(),
    .groups = "drop"
  )

data_across_participant <- data_by_participant %>% 
  group_by(set_size) %>% 
  summarise(
    ...,
    ...,
    ...
  ) %>% 
  mutate(
    sem..,
    ci..
  )

# group by RM and check dot position



data_by_participant <- data %>%
  group_by(participant, set_size, type) %>% 
  summarise(
    avg_dot_deivaiton = mean(dot_deviaiton),
    n = n(),
    .groups = "drop"
  )


data_across_participant <- data_by_participant %>% 
  group_by( set_size, type) %>% 
  summarise(
    mean_dot_dv = mean(avg_dot_deivaiton),
    sd_dot_dv = sd(avg_dot_deivaiton),
    n = n(),
    .groups = "drop"
  ) %>% 
  mutate(
    sem_dot_dv = sd_dot_dv/sqrt(n),
    ci_dot_dv =qt(0.975,(n-1)) * sem_dot_dv
  )




my_plot_dotdv <- ggplot() +
  
  geom_point(
    data = data_across_participant,
    aes(
      x = set_size,
      y = mean_dot_dv,
      color = type,
      group = type,
    ),
    position = position_dodge(width = 0.4),
    size = 3
  )+
  
  
  geom_errorbar(
    data = data_across_participant,
    aes(
      x = set_size,
      y = mean_dot_dv,
      ymin= mean_dot_dv - sem_dot_dv,
      ymax = mean_dot_dv + sem_dot_dv,
      group = type,
      color = type
    ),
    position = position_dodge(width = 0.4),
    width = 0.2
  )+

  scale_y_continuous(
    limits = c(-1, 1),
    breaks = seq(-1, 1, by = 0.2)
  ) +

  theme_minimal() 
  
  # facet_wrap(~visual_field)
  
  

my_plot_dotdv



m_full <- lme4::lmer(
  dot_deviaiton ~ set_size * type + (1 |participant), data = data
)

m_reduce <- lme4::lmer(
  dot_deviaiton ~ set_size + type + (1 |participant), data = data
)

anova(m_full, m_reduce)

summary(m_full)

sjPlot::tab_model(
  m_full,
  p.style = 'scientific_stars',
  show.se = T,
  show.stat = T,
  digits = 3)

emm  <- emmeans::emmeans(
  m_full,
  ~ type | set_size)


contrasts  <- emmeans::contrast(
  emm,
  method = "pairwise",
  adjust = "holm"  
)

contrasts 
