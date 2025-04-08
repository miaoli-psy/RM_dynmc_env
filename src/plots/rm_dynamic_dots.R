# libraires ---------------------------------------------------------------
library(tidyverse)
library(ggplot2)
library(ggthemes)
library(svglite)
library(ggpubr)
library(reshape2)
# install.packages('circular')

setwd("d:/OneDrive/projects/rm_dynamic_env/src/plots/")

# df_main_full.csv
data <- read_csv(file.choose())

# ---------------data------------------------

col_names <- names(data)
col_names

data <- data %>%
  dplyr::select(nblines, position_x2, participant_id, condition,correct_value, dot_deviation,
                perceived_lines, line_deviation, excentricity)

data <- data %>% 
  rename("eccentricity" = "excentricity")

data$eccentricity <-factor(data$eccentricity)


data_by_subject <- data %>% 
  group_by(nblines,
           participant_id,
           eccentricity) %>% 
  summarise(
    num_deviation_mean = mean(line_deviation),
    num_deviation_std = sd(line_deviation),
    n = n()
  ) %>% 
  mutate(
    num_deviation_SEM = num_deviation_std / sqrt(n),
    num_deviation_CI = num_deviation_SEM * qt((1 - 0.05) / 2 + .5, n - 1)
  )

data_by_subject2 <- data %>% 
  group_by(nblines,
           participant_id) %>% 
  summarise(
    num_deviation_mean = mean(line_deviation),
    num_deviation_std = sd(line_deviation),
    n = n()
  ) %>% 
  mutate(
    num_deviation_SEM = num_deviation_std / sqrt(n),
    num_deviation_CI = num_deviation_SEM * qt((1 - 0.05) / 2 + .5, n - 1)
  )


plt_num_ds <- ggplot() +
  
  geom_point(
    data = data_by_subject,
    aes(
      x = nblines,
      y = num_deviation_mean,
      group = eccentricity,
      color = eccentricity
    ),
    position = position_dodge(0.8), stat = "identity", alpha = 0.8,
    size = 3
    ) +
  
  geom_point(
    data = data_by_subject2,
    aes(
      x = nblines,
      y = num_deviation_mean
    ),
    position = position_dodge(0.8), stat = "identity", alpha = 0.5,
    shape = 21,
    size = 5,
    stroke =2
  ) +
  
  
  geom_hline(yintercept = 0, linetype = "dashed") +
  
  geom_errorbar(data = data_by_subject, aes(x = nblines,
                                            y = num_deviation_mean,
                                            ymin = num_deviation_mean - num_deviation_CI,
                                            ymax = num_deviation_mean + num_deviation_CI,
                                            group = eccentricity,
                                            color = eccentricity
                                            ),
                
                size  = 0.8,
                width = .00,
                position = position_dodge(0.8)) +
  
  geom_errorbar(data = data_by_subject2, aes(x = nblines,
                                            y = num_deviation_mean,
                                            ymin = num_deviation_mean - num_deviation_CI,
                                            ymax = num_deviation_mean + num_deviation_CI
  ),
  
  size  = 0.8,
  width = .00,
  position = position_dodge(0.8),
  alpha = 0.5) +
  
  labs(y = "Deviation Score (DS)", x = "Set Size") +
  
  scale_x_continuous(breaks = c(3, 4),
                     labels = c("3", "4"),
                     expand = c(0.1, 0.5)) +

  theme(axis.title.x = element_text(color="black", size=14, face="bold"),
        axis.title.y = element_text(color="black", size=14, face="bold"),
        panel.border = element_blank(),  
        # remove panel grid lines
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        # remove panel background
        panel.background = element_blank(),
        # add axis line
        axis.line = element_line(colour = "grey"),
        # x,y axis tick labels
        axis.text.x = element_text(size = 12, face = "bold"),
        axis.text.y = element_text(size = 12, face = "bold"),
        # legend size
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 10),
        # facet wrap title
        strip.text.x = element_text(size = 12, face = "bold"),
        panel.spacing = unit(1.0, "lines")) +
  
  
  facet_wrap( ~participant_id)

plt_num_ds


data_across_subject <- data_by_subject %>% 
  group_by(nblines,
           eccentricity) %>% 
  summarise(
    num_deviaion = mean(num_deviation_mean),
    num_deviaiton_std = sd(num_deviation_mean),
    n = n()
  ) %>% 
  mutate(
    num_deviation_SEM = num_deviaiton_std / sqrt(n),
    num_deviation_CI = num_deviation_SEM * qt((1 - 0.05) / 2 + .5, n - 1)
  )

data_across_subject2 <- data_by_subject %>% 
  group_by(nblines) %>% 
  summarise(
    num_deviaion = mean(num_deviation_mean),
    num_deviaiton_std = sd(num_deviation_mean),
    n = n()
  ) %>% 
  mutate(
    num_deviation_SEM = num_deviaiton_std / sqrt(n),
    num_deviation_CI = num_deviation_SEM * qt((1 - 0.05) / 2 + .5, n - 1)
  )

plt_num_ds2 <- ggplot() +
  
  geom_point(
    data = data_across_subject,
    aes(
      x = nblines,
      y = num_deviaion,
      group = eccentricity,
      color = eccentricity
    ),
    position = position_dodge(0.8), stat = "identity", alpha = 0.8,
    size = 3
  ) +
  
  geom_point(
    data = data_across_subject2,
    aes(
      x = nblines,
      y = num_deviaion
    ),
    position = position_dodge(0.8), stat = "identity", alpha = 0.5,
    shape = 21,
    size = 5,
    stroke =2
  ) +
  
  
  geom_hline(yintercept = 0, linetype = "dashed") +
  
  geom_errorbar(data = data_across_subject, aes(x = nblines,
                                            y = num_deviaion,
                                            ymin = num_deviaion - num_deviation_SEM,
                                            ymax = num_deviaion + num_deviation_SEM,
                                            group = eccentricity,
                                            color = eccentricity
  ),
  
  size  = 0.8,
  width = .00,
  position = position_dodge(0.8)) +
  
  geom_errorbar(data = data_across_subject2, aes(x = nblines,
                                             y = num_deviaion,
                                             ymin = num_deviaion - num_deviation_SEM,
                                             ymax = num_deviaion + num_deviation_SEM
  ),
  
  size  = 0.8,
  width = .00,
  position = position_dodge(0.8),
  alpha = 0.5) +
  
  labs(y = "Deviation Score (DS)", x = "Set Size") +
  
  scale_x_continuous(breaks = c(3, 4),
                     labels = c("3", "4"),
                     expand = c(0.1, 0.5)) +
  
  theme(axis.title.x = element_text(color="black", size=14, face="bold"),
        axis.title.y = element_text(color="black", size=14, face="bold"),
        panel.border = element_blank(),  
        # remove panel grid lines
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        # remove panel background
        panel.background = element_blank(),
        # add axis line
        axis.line = element_line(colour = "grey"),
        # x,y axis tick labels
        axis.text.x = element_text(size = 12, face = "bold"),
        axis.text.y = element_text(size = 12, face = "bold"),
        # legend size
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 10),
        # facet wrap title
        strip.text.x = element_text(size = 12, face = "bold"),
        panel.spacing = unit(1.0, "lines"))+
  
  annotate(geom="text", x=4.5, y=-1, label="error bars are SEM",
           color="black")

plt_num_ds2


# check dot response distribution by number deviation


target_conditions <- c("condition 1")

data1_a <- data %>%
  filter(condition %in% target_conditions)
  
data1_a <- data1_a %>%
  
  mutate(
    line_deviation = factor(line_deviation, levels = c("-2", "-1", "0", "1", "2")),
    
    dot_deviation_label = case_when(
      all(target_conditions %in% c("condition 1", "condition 4")) ~ recode(
        dot_deviation,
        "0" = "correct (first gap)",
        "0.6" = "2nd line",
        "1.2" = "2nd gap",
        .default = as.character(dot_deviation)
      ),
      all(target_conditions %in% c("condition 2", "condition 5")) ~ recode(
        dot_deviation,
        "-0.6" = "1st line",
        "0" = "correct (first gap)",
        "0.6" = "2nd line",
        .default = as.character(dot_deviation)
      ),
      all(target_conditions %in% c("condition 3", "condition 6")) ~ recode(
        dot_deviation,
        "-1.2" = "under shoot",
        "-0.6" = "1st line",
        "0" = "correct (first gap)",
        .default = as.character(dot_deviation)
      ),
      TRUE ~ as.character(dot_deviation)
    )
    )

data1_a$dot_deviation <- factor(data1_a$dot_deviation_label)

summary_data1 <- data1_a %>%
  group_by(line_deviation, dot_deviation) %>%
  summarise(count = n(), .groups = 'drop') %>%
  group_by(line_deviation) %>%
  mutate(percent = count / sum(count) * 100)

line_dev_labels <- data1_a %>%
  count(line_deviation) %>%
  mutate(percent = round(n / sum(n) * 100, 1),
         label = paste0(line_deviation, "\n(", percent, "%)")) %>%
  arrange(as.numeric(as.character(line_deviation))) # ensure the same order


data1_a <- data1_a %>%
  left_join(line_dev_labels %>% select(line_deviation, label), by = "line_deviation") %>%
  mutate(line_deviation_label = factor(label, levels =line_dev_labels$label))

summary_keypress <- data1_a %>%
  group_by(line_deviation_label, dot_deviation) %>%
  summarise(count = n(), .groups = 'drop') %>%
  group_by(line_deviation_label) %>%
  mutate(percent = round(count / sum(count) * 100, 2))



summary_keypress$dot_deviation <- factor(summary_keypress$dot_deviation)



dot_colors <- c(
  "correct (first gap)" = "#1b9e77",  
  "on the line"         = "#d95f02",  
  "2nd gap"             = "#7570b3",  
  "2nd line"            = "#729ece",  
  "1st line"            = "#e78ac3",  
  "under shoot"         = "#ffd92f"   
)

# Keep only colors for labels that actually appear in this plot
used_colors <- dot_colors[names(dot_colors) %in% as.character(summary_keypress$dot_deviation)]


plt_condi <- ggplot() +
  geom_bar(
    data = summary_keypress,
    aes(x = line_deviation_label, y = percent, fill = dot_deviation),
    position = position_dodge(0.8), stat = "identity", alpha = 0.5,
  
  )+
  geom_text(
    data = summary_keypress,
    aes(x = line_deviation_label, y = percent, group = dot_deviation, label = percent),
    position = position_dodge(0.8), stat = "identity", alpha = 0.8,
    size = 3) +
    
  
  labs(title = paste0("Response Distribution by Number Deviation (", 
       paste(target_conditions, collapse = " & "), ")"),
       x = "Num Deviation (percentage trials)",
       y = "Percentage",
       fill = "Dot location") +
  
  scale_y_continuous(limits = c(0, 105)) +
  
  scale_fill_manual(values = used_colors)+

  
  theme(axis.title.x = element_text(color="black", size=14, face="bold"),
        axis.title.y = element_text(color="black", size=14, face="bold"),
        panel.border = element_blank(),  
        # remove panel grid lines
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        # remove panel background
        panel.background = element_blank(),
        # add axis line
        axis.line = element_line(colour = "grey"),
        # x,y axis tick labels
        axis.text.x = element_text(size = 12, face = "bold"),
        axis.text.y = element_text(size = 12, face = "bold"),
        # legend size
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 10),
        # facet wrap title
        strip.text.x = element_text(size = 12, face = "bold"),
        panel.spacing = unit(1.0, "lines"))
  
  
plt_condi


# sep for rm and non-rm, cal mean dot deviation




