# libraires ---------------------------------------------------------------
library(tidyverse)
library(ggplot2)
library(ggthemes)
library(svglite)
library(ggpubr)
library(reshape2)
# install.packages('circular')

setwd("d:/OneDrive/projects/multi_gabor_discr/src/plot/")

# gabor_adjst_ori_alldata2.csv
alldata_exp5 <- read_csv(file.choose())

# ---------------data------------------------
selected_data <- alldata_exp5 %>%
  dplyr::select(label, ori, abs_ori, ref_ori, participant, inner_resp, midd_resp, outer_resp)

selected_setsize3 <- selected_data %>% 
  filter(label %in% c("setsize3_r_ladder", "setsize3_r_snake"))

selected_setsize3 <- selected_setsize3 %>% 
  rowwise() %>%
  mutate(
    variance = var(c(inner_resp, midd_resp, outer_resp))
  )

# long format
alldata_long_format <-
  melt(
    selected_data,
    id.vars = c("label", "abs_ori", "ori", "ref_ori", "participant"),
    variable.name = "gabor_location"
  )

alldata_long_format <- alldata_long_format %>% 
  dplyr::rename(resp_ori = value)

# cal adj_error

# cal the shortest angular distance between two angles
angle_dist <- function(a, b) {
  # diff between angles a and b
  c <- a - b
  
  # normalize the diffcto be within the range of -90 to 90 degrees
  # (c + 90) % 180 shifts the range of c to [0, 180) by adding 90, 
  # then taking modulo 180 subtracting 90 shifts the range back to [-90, 90)
  normalized_angle <- (c + 90) %% 180 - 90
  
  return(normalized_angle)
}

#  apply angle_dist for every row
alldata_long_format <- alldata_long_format %>%
  rowwise() %>%
  mutate(adj_error = angle_dist(resp_ori, ori))

# adj_error correct for sign
alldata_long_format <- alldata_long_format %>% 
  mutate(adj_error = if_else(ori < 0, -1 * adj_error, adj_error))


setsize3_long <- alldata_long_format %>% 
  filter(label %in% c("setsize3_r_ladder", "setsize3_r_snake"))

setsize1_long <- alldata_long_format %>% 
  filter(label %in% c("setsize1_v", "setsize1_h") & gabor_location == "inner_resp")

#---------------within-trial variance---------------------
selected_setsize3_by_subject <- selected_setsize3 %>% 
  group_by(label, abs_ori, participant) %>%
  summarise(
    var_mean = mean(variance),
    var_sd = sd(variance),
    n = n()
  ) %>% 
  mutate(
    var_SEM = var_sd / sqrt(n)
  )

selected_setsize3_across_subject <- selected_setsize3_by_subject %>% 
  group_by(label, abs_ori) %>% 
  summarise(
    variation_mean = mean(var_mean),
    variation_sd = sd(var_mean),
    n = n()
  ) %>% 
  mutate(
    variation_SEM = variation_sd / sqrt(n)
  )

plot_var <- ggplot() +
  geom_point(
    data = selected_setsize3_across_subject,
    aes(
      x = abs_ori,
      y = variation_mean,
      group = label,
      color = label,
      size = 0.4
    ),
    position = position_dodge(0.8),
    stat = "identity",
    alpha = 0.8,
    show.legend = FALSE) +
  
  geom_errorbar(
    data = selected_setsize3_across_subject,
    aes(
      x = abs_ori,
      y = variation_mean,
      ymin = variation_mean - variation_SEM,
      ymax = variation_mean + variation_SEM,
      group = label,
      color = label
    ),
    
    size  = 0.8,
    width = .00,
    alpha = 0.5,
    position = position_dodge(0.8)
  ) +
  
  
  labs(y = "Variance", x = "Orientation(°)") +
  
  scale_y_continuous(limits = c(0, 95)) +
  
  scale_x_continuous(breaks = c(2, 4, 10),
                     labels = c("2", "4", "10"), limits = c(1.5, 10.5))+
  
  
  
  scale_color_manual(
    labels = c("setsize3_r_ladder", "setsize3_r_snake"),
    values = c("#BB5566", "#674EA7"),
    name = "Gabor type"
  ) +
  
  theme(
    axis.title.x = element_text(
      color = "black",
      size = 14,
      face = "bold"
    ),
    axis.title.y = element_text(
      color = "black",
      size = 14,
      face = "bold"
    ),
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
    panel.spacing = unit(1.0, "lines")
  ) 

plot_var
#ggsave(file = "exp5_variance.svg", plot = plot_var, width = 5, height = 3.7,  units = "in")


# ---------------circular mean and group variability-----------
selected_setsize3_by_subject2 =  setsize3_long %>%
  dplyr::group_by(label, abs_ori, gabor_location,participant) %>%
  dplyr::summarise(
    meanAdj = circular::mean.circular(circular::circular(adj_error / 180 * pi)) / pi * 180,
    sdAdj = circular::sd.circular(circular::circular(adj_error / 180 * pi)) / pi * 180,
    n = n()
  )

selected_setsize3_acorss_subject2 =  selected_setsize3_by_subject2 %>%
  dplyr::group_by(label, abs_ori, gabor_location) %>%
  dplyr::summarise(
    adj_error_mean = mean(meanAdj),
    adj_error_sd = sd(meanAdj),
    group_variability = mean(sdAdj),
    group_variability_sd = sd(sdAdj),
    n = n()
  ) %>% 
  mutate(
    adj_error_sem = adj_error_sd/sqrt(n),
    group_variability_sem = group_variability_sd/sqrt(n)
  )

selected_setsize1_by_subject2 =  setsize1_long %>%
  dplyr::group_by(label, abs_ori, gabor_location,participant) %>%
  dplyr::summarise(
    meanAdj = circular::mean.circular(circular::circular(adj_error / 180 * pi)) / pi * 180,
    sdAdj = circular::sd.circular(circular::circular(adj_error / 180 * pi)) / pi * 180,
    n = n()
  )

selected_setsize1_acorss_subject2 =  selected_setsize1_by_subject2 %>%
  dplyr::group_by(label, abs_ori, gabor_location) %>%
  dplyr::summarise(
    adj_error_mean = mean(meanAdj),
    adj_error_sd = sd(meanAdj),
    group_variability = mean(sdAdj),
    group_variability_sd = sd(sdAdj),
    n = n()
  ) %>% 
  mutate(
    adj_error_sem = adj_error_sd/sqrt(n),
    group_variability_sem = group_variability_sd/sqrt(n)
  )


plot_circular_mean_adj_error <- ggplot() +
  geom_point(
    data = selected_setsize3_acorss_subject2,
    aes(
      x = gabor_location,
      y = adj_error_mean,
      group = label,
      color = label,
      size = 0.4
    ),
    position = position_dodge(0.8),
    stat = "identity",
    alpha = 0.8,
    show.legend = FALSE) +
  
  geom_errorbar(
    data = selected_setsize3_acorss_subject2,
    aes(
      x = gabor_location,
      y = adj_error_mean,
      ymin = adj_error_mean - adj_error_sem,
      ymax = adj_error_mean + adj_error_sem,
      group = label,
      color = label
    ),
    
    size  = 0.8,
    width = .00,
    alpha = 0.5,
    position = position_dodge(0.8)
  ) +
  
  
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  
  labs(y = "Circular mean of adjustment error(°)", x = "Gabor Location") +
  
  scale_y_continuous(limits = c(-4, 4)) +
  # 
  # scale_x_continuous(breaks = c(2, 4, 10),
  #                    labels = c("2", "4", "10"), limits = c(1.5, 10.5))+
  # 
  # 
  scale_x_discrete(
    "Gabor Location",
    labels = c(
      "inner_resp" = "I",
      "midd_resp" = "M",
      "outer_resp" = "O"
    )) +
  
  scale_color_manual(
    labels = c("setsize3_r_ladder", "setsize3_r_snake"),
    values = c("#BB5566", "#674EA7"),
    name = "Gabor type"
  ) +
  
  theme(
    axis.title.x = element_text(
      color = "black",
      size = 14,
      face = "bold"
    ),
    axis.title.y = element_text(
      color = "black",
      size = 14,
      face = "bold"
    ),
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
    panel.spacing = unit(1.0, "lines")
  ) +
  facet_wrap(~ abs_ori, nrow = 1, labeller = labeller(
    abs_ori =
      c("2" = "2°",
        "4" = "4°",
        "10" = "10°"
      )
  ))

plot_circular_mean_adj_error1 <- ggplot() +
  geom_point(
    data = selected_setsize1_acorss_subject2,
    aes(
      x = gabor_location,
      y = adj_error_mean,
      group = label,
      color = label,
      size = 0.4
    ),
    position = position_dodge(0.8),
    stat = "identity",
    alpha = 0.8,
    show.legend = FALSE) +
  
  geom_errorbar(
    data = selected_setsize1_acorss_subject2,
    aes(
      x = gabor_location,
      y = adj_error_mean,
      ymin = adj_error_mean - adj_error_sem,
      ymax = adj_error_mean + adj_error_sem,
      group = label,
      color = label
    ),
    
    size  = 0.8,
    width = .00,
    alpha = 0.5,
    position = position_dodge(0.8)
  ) +
  
  
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  
  labs(y = "Circular mean of adjustment error(°)", x = "Gabor Location") +
  
  scale_y_continuous(limits = c(-4, 4)) +
  
  # scale_x_continuous(breaks = c(2, 4, 10),
  #                    labels = c("2", "4", "10"), limits = c(1.5, 10.5))+
  # 
  # 
  scale_x_discrete(
    "Gabor Location",
    labels = c(
      "inner_resp" = "I",
      "midd_resp" = "M",
      "outer_resp" = "O"
    )) +
  
  scale_color_manual(
    labels = c("setsize1_h", "setsize1_v"),
    values = c("black", "grey"),
    name = "Gabor type"
  ) +
  
  
  theme(
    axis.title.x = element_text(
      color = "black",
      size = 14,
      face = "bold"
    ),
    axis.title.y = element_text(
      color = "black",
      size = 14,
      face = "bold"
    ),
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
    panel.spacing = unit(1.0, "lines")
  ) +
  facet_wrap(~ abs_ori, nrow = 1, labeller = labeller(
    abs_ori =
      c("2" = "2°",
        "4" = "4°",
        "10" = "10°"
      )
  ))
plot_circular_mean_adj_error
plot_circular_mean_adj_error1

ggsave(file = "expcirmean.svg", plot = plot_circular_mean_adj_error,  width = 7.23, height = 3.5, units = "in")
ggsave(file = "expcirmean1.svg", plot = plot_circular_mean_adj_error1,  width = 7.23, height = 3.5, units = "in")

plot_group_variability <- ggplot() +
  geom_point(
    data = selected_setsize3_acorss_subject2,
    aes(
      x = gabor_location,
      y = group_variability,
      group = label,
      color = label,
      size = 0.4
    ),
    position = position_dodge(0.8),
    stat = "identity",
    alpha = 0.8,
    show.legend = FALSE) +
  
  geom_errorbar(
    data = selected_setsize3_acorss_subject2,
    aes(
      x = gabor_location,
      y = group_variability,
      ymin = group_variability - group_variability_sem,
      ymax = group_variability + group_variability_sem,
      group = label,
      color = label
    ),
    
    size  = 0.8,
    width = .00,
    alpha = 0.5,
    position = position_dodge(0.8)
  ) +
  
  
  labs(y = "Group Variability", x = "Gabor Location") +
  
  scale_y_continuous(limits = c(2, 12)) +
  
  # scale_x_continuous(breaks = c(2, 4, 10),
  #                    labels = c("2", "4", "10"), limits = c(1.5, 10.5))+
  # 
  # 
  scale_x_discrete(
    "Gabor Location",
    labels = c(
      "inner_resp" = "I",
      "midd_resp" = "M",
      "outer_resp" = "O"
    )) +
  
  scale_color_manual(
    labels = c("setsize3_r_ladder", "setsize3_r_snake"),
    values = c("#BB5566", "#674EA7"),
    name = "Gabor type"
  ) +
  
  theme(
    axis.title.x = element_text(
      color = "black",
      size = 14,
      face = "bold"
    ),
    axis.title.y = element_text(
      color = "black",
      size = 14,
      face = "bold"
    ),
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
    panel.spacing = unit(1.0, "lines")
  ) +
  facet_wrap(~ abs_ori, nrow = 1, labeller = labeller(
    abs_ori =
      c("2" = "2°",
        "4" = "4°",
        "10" = "10°"
      )
  ))
plot_group_variability1 <- ggplot() +
  geom_point(
    data = selected_setsize1_acorss_subject2,
    aes(
      x = gabor_location,
      y = group_variability,
      group = label,
      color = label,
      size = 0.4
    ),
    position = position_dodge(0.8),
    stat = "identity",
    alpha = 0.8,
    show.legend = FALSE) +
  
  geom_errorbar(
    data = selected_setsize1_acorss_subject2,
    aes(
      x = gabor_location,
      y = group_variability,
      ymin = group_variability - group_variability_sem,
      ymax = group_variability + group_variability_sem,
      group = label,
      color = label
    ),
    
    size  = 0.8,
    width = .00,
    alpha = 0.5,
    position = position_dodge(0.8)
  ) +
  
  
  labs(y = "Variability", x = "Gabor Location") +
  
  scale_y_continuous(limits = c(2, 12)) +
  
  # scale_x_continuous(breaks = c(2, 4, 10),
  #                    labels = c("2", "4", "10"), limits = c(1.5, 10.5))+
  # 
  # 
  scale_x_discrete(
    "Gabor Location",
    labels = c(
      "inner_resp" = "I",
      "midd_resp" = "M",
      "outer_resp" = "O"
    )) +
  
  scale_color_manual(
    labels = c("setsize1_h", "setsize1_v"),
    values = c("black", "grey"),
    name = "Gabor type"
  ) +
  
  
  theme(
    axis.title.x = element_text(
      color = "black",
      size = 14,
      face = "bold"
    ),
    axis.title.y = element_text(
      color = "black",
      size = 14,
      face = "bold"
    ),
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
    panel.spacing = unit(1.0, "lines")
  ) +
  facet_wrap(~ abs_ori, nrow = 1, labeller = labeller(
    abs_ori =
      c("2" = "2°",
        "4" = "4°",
        "10" = "10°"
      )
  ))

plot_group_variability
plot_group_variability1
# ggsave(file = "group_var.svg", plot = plot_group_variability,  width = 7.23, height = 3.5, units = "in")
# ggsave(file = "group_var1.svg", plot = plot_group_variability1,  width = 7.23, height = 3.5, units = "in")
