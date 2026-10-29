# ------------library-----------------
library(dplyr)
library(ggplot2)


# ------------set project path---------
setwd("D:/OneDrive/projects/RM_action")


# ------------read data---------------
my_data_raw <- read.csv(
  "data_ens/data_RMaction_exp1.csv",
  colClasses = c(participant = "character"),
  stringsAsFactors = FALSE
)


# ------------remove incomplete participants---------------
# Participant 002 completed 192 trials and participant 012 completed 576
# trials. Exclude both participants before correcting number responses.
participants_to_exclude <- c(
  "002",
  "012"
)

data_incomplete_participants <- my_data_raw %>%
  mutate(
    SubID = participant,
    correct_num = set_size
  ) %>%
  filter(SubID %in% participants_to_exclude) %>%
  arrange(
    SubID,
    correct_num
  )

data_participant_exclusion_summary <-
  data_incomplete_participants %>%
  count(
    SubID,
    correct_num,
    name = "n_trials"
  ) %>%
  arrange(
    SubID,
    correct_num
  )


data_participant_exclusion_summary


n_trials_incomplete_participants <- nrow(
  data_incomplete_participants
)

my_data <- my_data_raw %>%
  mutate(
    SubID = participant,
    correct_num = set_size
  ) %>%
  filter(!SubID %in% participants_to_exclude)

n_participants_after_exclusion <- n_distinct(
  my_data$SubID
)

n_trials_after_participant_exclusion <- nrow(my_data)

data_participant_cleaning_summary <- data.frame(
  n_participants_excluded = length(participants_to_exclude),
  n_trials_excluded = n_trials_incomplete_participants,
  n_participants_after = n_participants_after_exclusion,
  n_trials_after = n_trials_after_participant_exclusion
)


data_participant_cleaning_summary


# ------------correct number responses---------------
# Keep the original response, then correct the known keypad error:
# for ENS participants only, response 7 means that the participant intended
# to report 4. Lille responses of 7 are not changed.
ens_participants <- sprintf(
  "%03d",
  1:24
)

my_data <- my_data %>%
  mutate(
    num_resp_raw = num_resp,
    num_resp = if_else(
      SubID %in% ens_participants & num_resp == 7,
      4,
      num_resp
    ),
    number_deviation = num_resp - correct_num
  )

n_number_responses_corrected <- sum(
  my_data$SubID %in% ens_participants &
    my_data$num_resp_raw == 7,
  na.rm = TRUE
)

data_number_response_correction_summary <- data.frame(
  n_responses_corrected_7_to_4 =
    n_number_responses_corrected,
  n_trials_after_correction = nrow(my_data)
)


data_number_response_correction_summary


# # check participants info
# df_check_participants <- my_data %>%
#   group_by(SubID, age, sex) %>%
#   tally()
# 
# # mean age
# mean(df_check_participants$age)


# ------------plot theme--------------
my_plot_theme <- theme(
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
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.background = element_blank(),
  axis.line = element_line(colour = "grey"),
  axis.text.x = element_text(size = 12, face = "bold"),
  axis.text.y = element_text(size = 12, face = "bold"),
  legend.title = element_text(size = 12, face = "bold"),
  legend.text = element_text(size = 10),
  strip.text.x = element_text(size = 12, face = "bold")
)


# ------------plot 1: number deviation by set size---------------
# Arrange the number-task data first.
data_number_deviation <- my_data %>%
  filter(
    !is.na(SubID),
    !is.na(correct_num),
    !is.na(number_deviation)
  ) %>%
  arrange(
    SubID,
    correct_num
  )


# Calculate each participant's mean deviation for each set size.
data_number_deviation_by_pp <- data_number_deviation %>%
  group_by(
    SubID,
    correct_num
  ) %>%
  summarise(
    mean_number_deviation = mean(
      number_deviation,
      na.rm = TRUE
    ),
    .groups = "drop"
  )


# Calculate the average and 95% CI across participants.
data_number_deviation_across_pp <-
  data_number_deviation_by_pp %>%
  group_by(correct_num) %>%
  summarise(
    n = sum(!is.na(mean_number_deviation)),
    number_deviation_mean = mean(
      mean_number_deviation,
      na.rm = TRUE
    ),
    number_deviation_sd = sd(
      mean_number_deviation,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  mutate(
    number_deviation_SEM =
      number_deviation_sd / sqrt(n),
    number_deviation_CI =
      number_deviation_SEM * qt(0.975, n - 1)
  )


# Calculate trial numbers before plotting.
n_trials_plot_1_by_pp <- data_number_deviation %>%
  count(
    SubID,
    correct_num,
    name = "n_trials"
  ) %>%
  arrange(
    SubID,
    correct_num
  )


n_trials_plot_1 <- data_number_deviation %>%
  count(
    correct_num,
    name = "n_trials"
  ) %>%
  left_join(
    data_number_deviation_by_pp %>%
      count(
        correct_num,
        name = "n_participants"
      ),
    by = "correct_num"
  ) %>%
  arrange(correct_num)


n_trials_plot_1


my_plot_number_deviation <- ggplot() +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "grey"
  ) +
  geom_point(
    data = data_number_deviation_by_pp,
    aes(
      x = factor(correct_num),
      y = mean_number_deviation
    ),
    position = position_jitter(
      width = 0.08,
      height = 0,
      seed = 20261029
    ),
    size = 2,
    alpha = 0.5,
    color = "#A0A0A0"
  ) +
  geom_point(
    data = data_number_deviation_across_pp,
    aes(
      x = factor(correct_num),
      y = number_deviation_mean
    ),
    size = 3,
    color = "black"
  ) +
  geom_errorbar(
    data = data_number_deviation_across_pp,
    aes(
      x = factor(correct_num),
      y = number_deviation_mean,
      ymin = number_deviation_mean - number_deviation_CI,
      ymax = number_deviation_mean + number_deviation_CI
    ),
    linewidth = 0.5,
    width = 0.0,
    color = "black"
  ) +
  scale_y_continuous(
    limits = c(-2, 1),
    breaks = seq(-2, 1, by = 0.5),
    expand = expansion(mult = c(0.05, 0.05))
  ) +
  labs(
    x = "Set Size",
    y = "Number Deviation"
  ) +
  my_plot_theme

my_plot_number_deviation


# ------------plot 2: percentage of trials by number deviation---------------
# Arrange the number-deviation data first.
data_number_percentage <- my_data %>%
  filter(
    !is.na(SubID),
    !is.na(correct_num),
    !is.na(number_deviation)
  ) %>%
  arrange(
    SubID,
    correct_num,
    number_deviation
  )

number_deviation_levels <- seq(
  min(data_number_percentage$number_deviation),
  max(data_number_percentage$number_deviation)
)


# Calculate percentages within each participant and set size first.
data_number_percentage_by_pp <- data_number_percentage %>%
  count(
    SubID,
    correct_num,
    number_deviation,
    name = "n_trials"
  ) %>%
  group_by(
    SubID,
    correct_num
  ) %>%
  tidyr::complete(
    number_deviation = number_deviation_levels,
    fill = list(n_trials = 0)
  ) %>%
  mutate(
    percentage_trials = n_trials / sum(n_trials) * 100
  ) %>%
  ungroup() %>%
  arrange(
    SubID,
    correct_num,
    number_deviation
  )


# Calculate the average and 95% CI across participants.
data_number_percentage_across_pp <-
  data_number_percentage_by_pp %>%
  group_by(
    correct_num,
    number_deviation
  ) %>%
  summarise(
    n = n(),
    percentage_trials_mean = mean(percentage_trials),
    percentage_trials_sd = sd(percentage_trials),
    .groups = "drop"
  ) %>%
  mutate(
    percentage_trials_SEM =
      percentage_trials_sd / sqrt(n),
    percentage_trials_CI =
      percentage_trials_SEM * qt(0.975, n - 1)
  ) %>%
  arrange(
    correct_num,
    number_deviation
  )


# Calculate trial numbers before plotting.
n_trials_plot_2_by_pp <- data_number_percentage %>%
  count(
    SubID,
    correct_num,
    number_deviation,
    name = "n_trials"
  ) %>%
  arrange(
    SubID,
    correct_num,
    number_deviation
  )


n_trials_plot_2 <- data_number_percentage %>%
  count(
    correct_num,
    number_deviation,
    name = "n_trials"
  ) %>%
  group_by(correct_num) %>%
  tidyr::complete(
    number_deviation = number_deviation_levels,
    fill = list(n_trials = 0)
  ) %>%
  ungroup() %>%
  arrange(
    correct_num,
    number_deviation
  )


n_trials_plot_2


data_number_percentage_plot <-
  data_number_percentage_across_pp %>%
  left_join(
    n_trials_plot_2,
    by = c(
      "correct_num",
      "number_deviation"
    )
  ) %>%
  arrange(
    correct_num,
    number_deviation
  )


my_plot_number_percentage <- ggplot(
  data_number_percentage_plot,
  aes(
    x = factor(number_deviation),
    y = percentage_trials_mean,
    fill = factor(correct_num)
  )
) +
  geom_col(
    position = position_dodge(0.7),
    width = 0.6,
    alpha = 0.5
  ) +
  geom_errorbar(
    aes(
      ymin = percentage_trials_mean - percentage_trials_CI,
      ymax = percentage_trials_mean + percentage_trials_CI
    ),
    position = position_dodge(0.7),
    width = 0.0,
    color = "black"
  ) +
  geom_text(
    aes(
      y = percentage_trials_mean + percentage_trials_CI + 1,
      label = paste0("n = ", n_trials)
    ),
    position = position_dodge(0.7),
    size = 3,
    color = "black"
  ) +
  labs(
    x = "Number Deviation",
    y = "Percentage of Trials",
    fill = "Set Size"
  ) +
  scale_fill_manual(
    values = c(
      "3" = "#0072B2",
      "4" = "#D55E00"
    )
  ) +
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, by = 20),
    expand = expansion(mult = c(0, 0.05))
  ) +
  my_plot_theme


my_plot_number_percentage


# ------------data for other analyses---------------
# Keep only RM trials and correct-number trials for subsequent analyses.
n_trials_before_other_analysis_filter <- nrow(my_data)

my_data_other_analysis <- my_data %>%
  filter(number_deviation %in% c(-1, 0)) %>%
  mutate(
    trial_type = factor(
      number_deviation,
      levels = c(-1, 0),
      labels = c("RM", "Correct")
    )
  ) %>%
  arrange(
    SubID,
    correct_num,
    trial_type
  )

n_trials_after_other_analysis_filter <- nrow(
  my_data_other_analysis
)

n_trials_removed_other_analysis <-
  n_trials_before_other_analysis_filter -
  n_trials_after_other_analysis_filter

# 94.5% trials deviation = -1 and 0 
percentage_trials_kept_other_analysis <-
  n_trials_after_other_analysis_filter /
  n_trials_before_other_analysis_filter * 100

n_trials_other_analysis <- my_data_other_analysis %>%
  count(
    correct_num,
    trial_type,
    name = "n_trials"
  ) %>%
  arrange(
    correct_num,
    trial_type
  )

n_trials_other_analysis


# ------------plot 4: dot deviation by set size and eccentricity---------------
# Arrange the folded dot-deviation data separately for each set size and
# innermost-line eccentricity.
data_dot_deviation_all <- my_data_other_analysis %>%
  mutate(
    innermost_line_eccentricity = abs(first),
    target_position_folded = abs(target_position),
    dot_position_folded = abs(dot_position),
    dot_deviation_folded =
      dot_position_folded - target_position_folded
  )


# A zero dot position means that the participant did not move the dot from
# fixation. Remove these invalid action responses before plotting or modeling.
data_dot_no_movement <- data_dot_deviation_all %>%
  filter(dot_position_folded == 0) %>%
  arrange(
    SubID,
    correct_num,
    innermost_line_eccentricity,
    trial_type
  )

n_trials_dot_no_movement <- nrow(data_dot_no_movement)

percentage_trials_dot_no_movement <-
  n_trials_dot_no_movement /
  nrow(data_dot_deviation_all) * 100

data_dot_cleaning_summary <- data.frame(
  n_trials_before = nrow(data_dot_deviation_all),
  n_no_movement_trials_removed = n_trials_dot_no_movement,
  percentage_removed = percentage_trials_dot_no_movement,
  n_trials_after =
    nrow(data_dot_deviation_all) - n_trials_dot_no_movement
)


data_dot_cleaning_summary


data_dot_deviation <- data_dot_deviation_all %>%
  filter(
    !is.na(SubID),
    !is.na(correct_num),
    !is.na(trial_type),
    !is.na(dot_deviation_folded),
    dot_position_folded > 0
  ) %>%
  arrange(
    SubID,
    correct_num,
    trial_type
  )


# Calculate each participant's mean dot deviation for each condition.
data_dot_deviation_by_pp <- data_dot_deviation %>%
  group_by(
    SubID,
    correct_num,
    trial_type
  ) %>%
  summarise(
    mean_dot_deviation = mean(
      dot_deviation_folded,
      na.rm = TRUE
    ),
    .groups = "drop"
  )


# Calculate the average and 95% CI across participants. Each participant has
# equal weight even though ENS participants completed more repetitions.
data_dot_deviation_across_pp <-
  data_dot_deviation_by_pp %>%
  group_by(
    correct_num,
    trial_type
  ) %>%
  summarise(
    n = sum(!is.na(mean_dot_deviation)),
    dot_deviation_mean = mean(
      mean_dot_deviation,
      na.rm = TRUE
    ),
    dot_deviation_sd = sd(
      mean_dot_deviation,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  mutate(
    dot_deviation_SEM =
      dot_deviation_sd / sqrt(n),
    dot_deviation_CI =
      dot_deviation_SEM * qt(0.975, n - 1)
  ) %>%
  arrange(
    correct_num,
    trial_type
  )

data_dot_deviation_by_eccentricity <- data_dot_deviation %>%
  filter(
    !is.na(innermost_line_eccentricity)
  ) %>%
  arrange(
    SubID,
    correct_num,
    innermost_line_eccentricity,
    trial_type
  )


# Calculate each participant's mean dot deviation for each condition.
data_dot_deviation_eccentricity_by_pp <-
  data_dot_deviation_by_eccentricity %>%
  group_by(
    SubID,
    correct_num,
    innermost_line_eccentricity,
    trial_type
  ) %>%
  summarise(
    mean_dot_deviation = mean(
      dot_deviation_folded,
      na.rm = TRUE
    ),
    .groups = "drop"
  )


# Calculate the average and 95% CI across participants.
data_dot_deviation_eccentricity_across_pp <-
  data_dot_deviation_eccentricity_by_pp %>%
  group_by(
    correct_num,
    innermost_line_eccentricity,
    trial_type
  ) %>%
  summarise(
    n = sum(!is.na(mean_dot_deviation)),
    dot_deviation_mean = mean(
      mean_dot_deviation,
      na.rm = TRUE
    ),
    dot_deviation_sd = sd(
      mean_dot_deviation,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  mutate(
    dot_deviation_SEM =
      dot_deviation_sd / sqrt(n),
    dot_deviation_CI =
      dot_deviation_SEM * qt(0.975, n - 1)
  ) %>%
  arrange(
    correct_num,
    innermost_line_eccentricity,
    trial_type
  )


# ------------linear mixed model: dot deviation---------------
# Set size and eccentricity are factors because the response bounds differ
# across innermost-line locations. ENS and Lille are pooled as participants.
data_dot_deviation_model <-
  data_dot_deviation_by_eccentricity %>%
  mutate(
    trial_type = relevel(
      factor(trial_type),
      ref = "Correct"
    ),
    set_size = factor(
      correct_num,
      levels = c(3, 4)
    ),
    innermost_eccentricity = factor(
      innermost_line_eccentricity,
      levels = c(6.8, 7.4, 8.0)
    )
  )


model_dot_deviation <- lmerTest::lmer(
  dot_deviation_folded ~
    trial_type *
      set_size *
      innermost_eccentricity +
    (1 + trial_type || SubID),
  data = data_dot_deviation_model,
  REML = TRUE,
  control = lme4::lmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 100000)
  )
)

sjPlot::tab_model(
  model_dot_deviation,
  p.style = 'scientific_stars',
  show.se = T,
  show.stat = T,
  digits = 3)

# result_dot_deviation_anova <- anova(
#   model_dot_deviation,
#   type = 3,
#   ddf = "Satterthwaite"
# )
# 
# 
# result_dot_deviation_anova


# Compare RM with correct trials inside every plotted panel. Holm correction
# is applied jointly across the six comparisons.
emm_dot_deviation <- emmeans::emmeans(
  model_dot_deviation,
  ~ trial_type |
    set_size * innermost_eccentricity,
  lmer.df = "satterthwaite"
)

result_dot_RM_vs_correct <- emmeans::contrast(
  emm_dot_deviation,
  method = list(
    "RM - Correct" = c(-1, 1)
  ),
  adjust = "none"
) %>%
  as.data.frame() %>%
  mutate(
    p_value_holm = p.adjust(
      p.value,
      method = "holm"
    ),
    significance = case_when(
      p_value_holm < 0.001 ~ "***",
      p_value_holm < 0.01 ~ "**",
      p_value_holm < 0.05 ~ "*",
      TRUE ~ "n.s"
    ),
    correct_num = as.numeric(
      as.character(set_size)
    ),
    innermost_line_eccentricity = as.numeric(
      as.character(innermost_eccentricity)
    )
  ) %>%
  arrange(
    correct_num,
    innermost_line_eccentricity
  )


# Cohen's dz uses each participant's paired RM-minus-Correct difference.
data_dot_RM_vs_correct_by_pp <-
  data_dot_deviation_eccentricity_by_pp %>%
  select(
    SubID,
    correct_num,
    innermost_line_eccentricity,
    trial_type,
    mean_dot_deviation
  ) %>%
  tidyr::pivot_wider(
    names_from = trial_type,
    values_from = mean_dot_deviation
  ) %>%
  filter(
    !is.na(RM),
    !is.na(Correct)
  ) %>%
  mutate(
    difference_RM_minus_correct = RM - Correct
  )


data_dot_effect_size <-
  data_dot_RM_vs_correct_by_pp %>%
  group_by(
    correct_num,
    innermost_line_eccentricity
  ) %>%
  summarise(
    n_paired = n(),
    mean_difference = mean(
      difference_RM_minus_correct
    ),
    sd_difference = sd(
      difference_RM_minus_correct
    ),
    cohen_dz = mean_difference / sd_difference,
    .groups = "drop"
  )


result_dot_RM_vs_correct <- result_dot_RM_vs_correct %>%
  left_join(
    data_dot_effect_size,
    by = c(
      "correct_num",
      "innermost_line_eccentricity"
    )
  )


result_dot_RM_vs_correct


#
# # At 7.4 degrees, participants can move both inward and outward. This model is
# # the cleanest direct test of whether RM responses overshoot zero.
# data_dot_deviation_7_4 <- data_dot_deviation_model %>%
#   filter(innermost_line_eccentricity == 7.4)
# 
# model_dot_deviation_7_4 <- lmerTest::lmer(
#   dot_deviation_folded ~
#     trial_type * set_size +
#     (1 + trial_type || SubID),
#   data = data_dot_deviation_7_4,
#   REML = TRUE,
#   control = lme4::lmerControl(
#     optimizer = "bobyqa",
#     optCtrl = list(maxfun = 100000)
#   )
# )
# 
# 
# summary(model_dot_deviation_7_4)
# 
# emm_dot_deviation_7_4 <- emmeans::emmeans(
#   model_dot_deviation_7_4,
#   ~ trial_type | set_size,
#   lmer.df = "satterthwaite"
# )
# 
# result_RM_overshoot_7_4 <- emmeans::test(
#   emm_dot_deviation_7_4,
#   null = 0,
#   side = "right",
#   adjust = "none"
# ) %>%
#   as.data.frame() %>%
#   filter(trial_type == "RM") %>%
#   mutate(
#     p_value_holm = p.adjust(
#       p.value,
#       method = "holm"
#     )
#   )
# 
# 
# result_RM_overshoot_7_4


# ------------plot 5: half-violin dot-deviation plot---------------
# Arrange the participant-level data first.
data_dot_deviation_half_violin <-
  data_dot_deviation_eccentricity_by_pp %>%
  arrange(
    SubID,
    correct_num,
    innermost_line_eccentricity,
    trial_type
  )


# Calculate trial numbers before plotting.
n_trials_plot_5 <- data_dot_deviation_by_eccentricity %>%
  count(
    correct_num,
    innermost_line_eccentricity,
    trial_type,
    name = "n_trials"
  ) %>%
  arrange(
    correct_num,
    innermost_line_eccentricity,
    trial_type
  )


n_trials_plot_5


my_plot_dot_deviation_half_violin <- ggplot(
  data_dot_deviation_half_violin,
  aes(
    x = trial_type,
    y = mean_dot_deviation,
    fill = trial_type,
    color = trial_type
  )
) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "grey"
  ) +
  gghalves::geom_half_violin(
    side = "l",
    trim = FALSE,
    width = 0.8,
    alpha = 0.4,
    color = NA
  ) +
  geom_jitter(
    width = 0.05,
    height = 0,
    size = 1.8,
    alpha = 0.4
  ) +
  geom_point(
    data = data_dot_deviation_eccentricity_across_pp,
    aes(
      x = trial_type,
      y = dot_deviation_mean
    ),
    inherit.aes = FALSE,
    size = 3,
    color = "black"
  ) +
  geom_errorbar(
    data = data_dot_deviation_eccentricity_across_pp,
    aes(
      x = trial_type,
      y = dot_deviation_mean,
      ymin = dot_deviation_mean - dot_deviation_CI,
      ymax = dot_deviation_mean + dot_deviation_CI
    ),
    inherit.aes = FALSE,
    width = 0.0,
    linewidth = 0.5,
    color = "black"
  ) +
  facet_grid(
    rows = vars(correct_num),
    cols = vars(innermost_line_eccentricity),
    labeller = labeller(
      correct_num = c(
        "3" = "Set Size 3",
        "4" = "Set Size 4"
      ),
      innermost_line_eccentricity = c(
        "6.8" = "Innermost 6.8°",
        "7.4" = "Innermost 7.4°",
        "8" = "Innermost 8.0°"
      )
    )
  ) +
  scale_fill_manual(
    labels = c("RM ", "correct"),
    values = c("#800074", "#298C8C"),
    name = ""
  ) +
  scale_color_manual(
    labels = c("RM ", "correct"),
    values = c("#800074", "#298C8C"),
    name = ""
  ) +
  guides(
    fill = "none",
    color = "none"
  ) +
  scale_y_continuous(
    limits = c(-2, 2),
    breaks = seq(-2, 2, by = 1),
    expand = expansion(mult = c(0.05, 0.10))
  ) +
  labs(
    x = "Trial Type",
    y = "Dot Deviation (°)"
  ) +
  my_plot_theme +
  theme(
    strip.text.y = element_text(
      size = 12,
      face = "bold",
      angle = 270
    ),
    panel.spacing.y = grid::unit(1.5, "lines")
  )


my_plot_dot_deviation_half_violin




