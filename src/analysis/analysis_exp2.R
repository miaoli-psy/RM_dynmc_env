# ------------library-----------------
library(dplyr)
library(ggplot2)


# ------------set project path---------
setwd("D:/OneDrive/projects/RM_action")


# ------------read data---------------
my_data <- read.csv(
  "data_ens/data_RMaction_exp2.csv",
  colClasses = c(participant = "character"),
  stringsAsFactors = FALSE
) %>%
  mutate(
    SubID = participant,
    correct_num = setsize,
    reported_num = number_response,
    number_deviation = reported_num - correct_num
  )

# 
# # check participants info
# df_check_participants <- my_data %>%
#   group_by(SubID, age, sex) %>%
#   tally()
# 
# # mean age
# mean(df_check_participants$age)

# # Keep the known data-quality notes visible without applying corrections whose
# # trial mappings are not unambiguous in the exported data.
# data_quality_notes <- my_data %>%
#   filter(
#     !is.na(data_quality_note),
#     data_quality_note != ""
#   ) %>%
#   distinct(
#     SubID,
#     data_quality_note
#   ) %>%
#   arrange(SubID)
# 
# 
# data_quality_notes

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
  ) %>%
  arrange(correct_num)


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


# ------------plot 3: aligned touch density---------------
# # Check which parts of the stimulus geometry jittered.
# data_exp2_geometry_check <- my_data %>%
#   mutate(
#     center_spacing_deg =
#       edge_spacing_deg + line_width_deg,
#     hemifield_direction = case_when(
#       tolower(hemifield) == "left" ~ -1,
#       tolower(hemifield) == "right" ~ 1,
#       TRUE ~ NA_real_
#     ),
#     target_x_folded_deg =
#       hemifield_direction * target_x_deg,
#     target_x_aligned =
#       (target_x_folded_deg - innermost_ecc_deg) /
#       center_spacing_deg,
#     target_x_expected =
#       target_index_from_innermost - 1
#   ) %>%
#   summarise(
#     innermost_eccentricity_min = min(
#       innermost_ecc_deg,
#       na.rm = TRUE
#     ),
#     innermost_eccentricity_max = max(
#       innermost_ecc_deg,
#       na.rm = TRUE
#     ),
#     edge_spacing_min = min(
#       edge_spacing_deg,
#       na.rm = TRUE
#     ),
#     edge_spacing_max = max(
#       edge_spacing_deg,
#       na.rm = TRUE
#     ),
#     line_width_min = min(
#       line_width_deg,
#       na.rm = TRUE
#     ),
#     line_width_max = max(
#       line_width_deg,
#       na.rm = TRUE
#     ),
#     center_spacing_min = min(
#       center_spacing_deg,
#       na.rm = TRUE
#     ),
#     center_spacing_max = max(
#       center_spacing_deg,
#       na.rm = TRUE
#     ),
#     maximum_target_alignment_error = max(
#       abs(target_x_aligned - target_x_expected),
#       na.rm = TRUE
#     )
#   )
# 
# 
# data_exp2_geometry_check


# Arrange and align each touch first. Left-field trials are reflected so that
# zero is always the innermost line and positive values point outward. One
# horizontal unit is the center-to-center line interval on that trial.
data_exp2_aligned_touch <- my_data %>%
  mutate(
    center_spacing_deg =
      edge_spacing_deg + line_width_deg,
    relative_line_width =
      line_width_deg / center_spacing_deg,
    hemifield_direction = case_when(
      tolower(hemifield) == "left" ~ -1,
      tolower(hemifield) == "right" ~ 1,
      TRUE ~ NA_real_
    ),
    touch_x_folded_deg =
      hemifield_direction * touch_x_deg,
    touch_x_aligned =
      (touch_x_folded_deg - innermost_ecc_deg) /
      center_spacing_deg,
    trial_type = recode(
      type,
      "correct" = "Correct",
      "RM" = "RM"
    ),
    trial_type = factor(
      trial_type,
      levels = c("Correct", "RM")
    ),
    task_label = recode(
      task,
      "plus_proche" = "Innermost",
      "deuxieme_plus_proche" = "Second innermost",
      "deuxieme_plus_eloignee" = "Second outermost",
      "plus_eloignee" = "Outermost"
    ),
    task_label = factor(
      task_label,
      levels = c(
        "Innermost",
        "Second innermost",
        "Second outermost",
        "Outermost"
      )
    ),
    panel_column = paste0(
      "Set Size ",
      correct_num,
      " | ",
      trial_type
    ),
    panel_column = factor(
      panel_column,
      levels = c(
        "Set Size 3 | Correct",
        "Set Size 3 | RM",
        "Set Size 4 | Correct",
        "Set Size 4 | RM"
      )
    )
  ) %>%
  filter(
    trial_type %in% c("Correct", "RM"),
    !is.na(SubID),
    !is.na(task_label),
    !is.na(panel_column),
    !is.na(touch_x_aligned),
    !is.na(touch_y_deg),
    !is.na(relative_line_width)
  ) %>%
  arrange(
    task_label,
    correct_num,
    trial_type,
    SubID
  )


# Calculate trial and participant numbers before plotting.
n_trials_exp2_aligned_touch <- data_exp2_aligned_touch %>%
  group_by(
    task_label,
    panel_column
  ) %>%
  summarise(
    n_trials = n(),
    n_participants = n_distinct(SubID),
    n_touches_outside_vertical_view = sum(
      touch_y_deg < -1.5 | touch_y_deg > 1.5
    ),
    .groups = "drop"
  ) %>%
  mutate(
    panel_label = paste0(
      format(n_trials, big.mark = ","),
      " trials; ",
      n_participants,
      " participants"
    ),
    outside_label = paste0(
      n_touches_outside_vertical_view,
      " touches beyond y limits"
    )
  ) %>%
  arrange(
    task_label,
    panel_column
  )


n_trials_exp2_aligned_touch


# Give every participant equal total weight within each plot panel.
# Touch-location densities were calculated separately for each task, 
# set size, and number-response category (RM, correct). 
# To prevent participants with more trials in a given condition 
# from disproportionately influencing the density, trials were 
# weighted within each panel so that every contributing participant 
# had equal total weight.

# For participant i, with n_i trials in a panel containing P participants, 
# each trial received weight 1/P * n_i.

data_exp2_aligned_touch <- data_exp2_aligned_touch %>%
  group_by(
    task_label,
    panel_column,
    SubID
  ) %>%
  mutate(
    n_trials_by_pp = n()
  ) %>%
  group_by(
    task_label,
    panel_column
  ) %>%
  mutate(
    n_participants = n_distinct(SubID),
    density_weight =
      1 / (n_participants * n_trials_by_pp)
  ) %>%
  ungroup() %>%
  arrange(
    task_label,
    panel_column,
    SubID
  )


# Calculate a weighted two-dimensional Gaussian density for every panel.
# All panels use the same bandwidths and plotting grid.

# Participant-balanced two-dimensional kernel density estimates 
# were calculated separately for each task, set size, 
# and number-response category. Gaussian kernels were used 
# with fixed bandwidths of 0.18 center-to-center line intervals horizontally 
# and 0.20° vertically. The same bandwidths, coordinate grid, 
# and density scale were used across panels. Trial weights were 
# chosen so that every contributing participant had equal total weight within each panel.
calculate_exp2_touch_density <- function(data) {
  x_grid <- seq(
    -2.5,
    5.5,
    length.out = 161
  )
  y_grid <- seq(
    -2,
    2,
    length.out = 121
  )
  x_bandwidth <- 0.18
  y_bandwidth <- 0.20

  kernel_x <- outer(
    x_grid,
    data$touch_x_aligned,
    function(grid_value, observed_value) {
      dnorm(
        (grid_value - observed_value) / x_bandwidth
      ) / x_bandwidth
    }
  )
  kernel_y <- outer(
    y_grid,
    data$touch_y_deg,
    function(grid_value, observed_value) {
      dnorm(
        (grid_value - observed_value) / y_bandwidth
      ) / y_bandwidth
    }
  )

  density_matrix <-
    sweep(
      kernel_x,
      2,
      data$density_weight,
      "*"
    ) %*% t(kernel_y)

  expand.grid(
    touch_x_aligned = x_grid,
    touch_y_deg = y_grid
  ) %>%
    mutate(
      density = as.vector(density_matrix),
      task_label = first(data$task_label),
      panel_column = first(data$panel_column)
    )
}


data_exp2_touch_density <- data_exp2_aligned_touch %>%
  group_split(
    task_label,
    panel_column,
    .keep = TRUE
  ) %>%
  lapply(calculate_exp2_touch_density) %>%
  bind_rows() %>%
  mutate(
    task_label = factor(
      task_label,
      levels = levels(data_exp2_aligned_touch$task_label)
    ),
    panel_column = factor(
      panel_column,
      levels = levels(data_exp2_aligned_touch$panel_column)
    ),
    density_scaled = density / max(
      density,
      na.rm = TRUE
    )
  ) %>%
  arrange(
    task_label,
    panel_column,
    touch_y_deg,
    touch_x_aligned
  )


# Summarise the variable line width after center alignment. The dark line is
# the median relative width; the pale band extends to the 95th percentile.
data_exp2_stimulus_reference <-
  data_exp2_aligned_touch %>%
  group_by(
    task_label,
    panel_column,
    correct_num
  ) %>%
  summarise(
    target_index_from_innermost = first(
      target_index_from_innermost
    ),
    relative_width_median = median(
      relative_line_width,
      na.rm = TRUE
    ),
    relative_width_95 = quantile(
      relative_line_width,
      0.95,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  tidyr::uncount(
    weights = correct_num,
    .id = "line_index",
    .remove = FALSE
  ) %>%
  mutate(
    line_position = line_index - 1,
    line_role = if_else(
      line_index == target_index_from_innermost,
      "Target",
      "Other"
    ),
    x_min_95 =
      line_position - relative_width_95 / 2,
    x_max_95 =
      line_position + relative_width_95 / 2,
    x_min_median =
      line_position - relative_width_median / 2,
    x_max_median =
      line_position + relative_width_median / 2
  ) %>%
  arrange(
    task_label,
    panel_column,
    line_index
  )


# Use set-size-specific names for the line positions. These labels are placed
# below the bottom row because a single numeric x scale is shared by all rows.
data_exp2_line_position_labels <-
  data_exp2_stimulus_reference %>%
  filter(task_label == "Outermost") %>%
  distinct(
    task_label,
    panel_column,
    correct_num,
    line_position
  ) %>%
  mutate(
    line_position_label = case_when(
      correct_num == 3 & line_position == 0 ~
        "Innermost",
      correct_num == 3 & line_position == 1 ~
        "Middle",
      correct_num == 3 & line_position == 2 ~
        "Outermost",
      correct_num == 4 & line_position == 0 ~
        "Innermost",
      correct_num == 4 & line_position == 1 ~
        "Second innermost",
      correct_num == 4 & line_position == 2 ~
        "Second outermost",
      correct_num == 4 & line_position == 3 ~
        "Outermost"
    )
  ) %>%
  arrange(
    panel_column,
    line_position
  )


my_plot_exp2_aligned_touch_density <- ggplot() +
  geom_raster(
    data = data_exp2_touch_density,
    aes(
      x = touch_x_aligned,
      y = touch_y_deg,
      fill = density_scaled
    ),
    interpolate = TRUE
  ) +
  geom_rect(
    data = data_exp2_stimulus_reference,
    aes(
      xmin = x_min_95,
      xmax = x_max_95,
      ymin = -0.6,
      ymax = 0.6
    ),
    fill = "#334155",
    alpha = 0.12,
    inherit.aes = FALSE
  ) +
  geom_rect(
    data = data_exp2_stimulus_reference,
    aes(
      xmin = x_min_median,
      xmax = x_max_median,
      ymin = -0.6,
      ymax = 0.6
    ),
    fill = "#334155",
    alpha = 0.85,
    inherit.aes = FALSE
  ) +
  geom_rect(
    data = data_exp2_stimulus_reference %>%
      filter(line_role == "Target"),
    aes(
      xmin = x_min_95,
      xmax = x_max_95,
      ymin = -0.68,
      ymax = 0.68
    ),
    fill = NA,
    color = "#D55E00",
    linewidth = 0.7,
    inherit.aes = FALSE
  ) +
  geom_text(
    data = n_trials_exp2_aligned_touch,
    aes(
      x = -1.85,
      y = 1.30,
      label = panel_label
    ),
    hjust = 0,
    vjust = 0,
    size = 3,
    color = "#475569",
    inherit.aes = FALSE
  ) +
  geom_text(
    data = data_exp2_line_position_labels,
    aes(
      x = line_position,
      y = -1.66,
      label = line_position_label
    ),
    angle = 45,
    hjust = 1,
    vjust = 1,
    lineheight = 0.9,
    size = 4.2,
    fontface = "bold",
    color = "black",
    inherit.aes = FALSE
  ) +
  geom_segment(
    data = data_exp2_line_position_labels,
    aes(
      x = line_position,
      xend = line_position,
      y = -1.5,
      yend = -1.58
    ),
    linewidth = 0.4,
    color = "black",
    inherit.aes = FALSE
  ) +
  # geom_text(
  #   data = n_trials_exp2_aligned_touch,
  #   aes(
  #     x = -2.3,
  #     y = -1.88,
  #     label = outside_label
  #   ),
  #   hjust = 0,
  #   vjust = 0,
  #   size = 2.7,
  #   color = "#64748B",
  #   inherit.aes = FALSE
  # ) +
  facet_grid(
    rows = vars(task_label),
    cols = vars(panel_column),
    switch = "y"
  ) +
  scale_fill_gradientn(
    colors = c(
      "#FFFFFF",
      "#E3F0F7",
      "#9ECAE1",
      "#4292C6",
      "#08519C",
      "#08306B"
    ),
    limits = c(0, 1),
    breaks = seq(0, 1, by = 0.25),
    labels = c(
      "0",
      "0.25",
      "0.50",
      "0.75",
      "1"
    ),
    name = "Density",
    guide = guide_colorbar(
      title.position = "top",
      title.hjust = 0.5,
      barwidth = grid::unit(0.45, "cm"),
      barheight = grid::unit(5.5, "cm"),
      frame.colour = "#64748B",
      frame.linewidth = 0.4,
      ticks.colour = "#334155"
    )
  ) +
  scale_x_continuous(
    breaks = NULL,
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    breaks = seq(-1.5, 1.5, by = 0.5),
    expand = c(0, 0)
  ) +
  coord_cartesian(
    xlim = c(-2, 5),
    ylim = c(-1.5, 1.5),
    expand = FALSE,
    clip = "off"
  ) +
  labs(
    # title = "Experiment 2 touch locations aligned to each trial's line array",
    # subtitle = paste0(
    #   "Left trials are reflected horizontally. ",
    #   "Density gives each participant equal weight within a panel."),
    x = paste0(
      "Line Position"
    ),
    y = "Task"
  ) +
  my_plot_theme +
  theme(
    # strip.background = element_blank(),
    panel.border = element_rect(
      color = "black",
      fill = NA,
      linewidth = 0.5
    ),
    panel.spacing.y = grid::unit(1.4, "lines"),
    strip.text.y = element_text(
      size = 12,
      face = "bold",
      color = "black"
    ),
    strip.placement = "outside",
    axis.title.x = element_text(
      margin = margin(t = 88)
    ),
    legend.position = "right",
    legend.title.position = "top",
    plot.margin = margin(12, 18, 32, 12)
  )


my_plot_exp2_aligned_touch_density


# dir.create(
#   "outputs",
#   showWarnings = FALSE,
#   recursive = TRUE
# )
# 
# 
# ggsave(
#   filename = "outputs/exp2_aligned_touch_density.png",
#   plot = my_plot_exp2_aligned_touch_density,
#   width = 16,
#   height = 12,
#   units = "in",
#   dpi = 300,
#   bg = "white"
# )


# ------------plot 4: horizontal dot deviation by task and set size---------------
# Arrange the horizontal-error data first. The preprocessing has already
# folded the two visual fields: positive values indicate outward overshoot and
# negative values indicate inward undershoot.
data_dot_deviation_exp2 <- my_data %>%
  mutate(
    trial_type = case_when(
      type == "RM" ~ "RM",
      type == "correct" ~ "Correct",
      TRUE ~ NA_character_
    ),
    trial_type = factor(
      trial_type,
      levels = c("RM", "Correct")
    ),
    task_label = recode(
      task,
      "plus_proche" = "Innermost",
      "deuxieme_plus_proche" = "Second innermost",
      "deuxieme_plus_eloignee" = "Second outermost",
      "plus_eloignee" = "Outermost"
    ),
    task_label = factor(
      task_label,
      levels = c(
        "Innermost",
        "Second innermost",
        "Second outermost",
        "Outermost"
      )
    )
  ) %>%
  filter(
    !is.na(SubID),
    !is.na(correct_num),
    !is.na(task_label),
    !is.na(trial_type),
    !is.na(error_x_deg)
  ) %>%
  arrange(
    SubID,
    correct_num,
    task_label,
    trial_type
  )


# Calculate trial numbers before plotting.
n_trials_plot_4 <- data_dot_deviation_exp2 %>%
  count(
    correct_num,
    task_label,
    trial_type,
    name = "n_trials"
  ) %>%
  arrange(
    correct_num,
    task_label,
    trial_type
  )


n_trials_plot_4


# Calculate each participant's mean horizontal error for every condition.
data_dot_deviation_exp2_by_pp <-
  data_dot_deviation_exp2 %>%
  group_by(
    SubID,
    correct_num,
    task_label,
    trial_type
  ) %>%
  summarise(
    mean_horizontal_error = mean(
      error_x_deg,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  arrange(
    SubID,
    correct_num,
    task_label,
    trial_type
  )


# Calculate the average and 95% CI across participants. Participants, rather
# than trials, are the independent units in this descriptive summary.
data_dot_deviation_exp2_across_pp <-
  data_dot_deviation_exp2_by_pp %>%
  group_by(
    correct_num,
    task_label,
    trial_type
  ) %>%
  summarise(
    n = sum(!is.na(mean_horizontal_error)),
    horizontal_error_mean = mean(
      mean_horizontal_error,
      na.rm = TRUE
    ),
    horizontal_error_sd = sd(
      mean_horizontal_error,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  mutate(
    horizontal_error_SEM =
      horizontal_error_sd / sqrt(n),
    horizontal_error_CI =
      horizontal_error_SEM * qt(0.975, n - 1)
  ) %>%
  arrange(
    correct_num,
    task_label,
    trial_type
  )


# ------------linear mixed model: horizontal dot deviation---------------
# This is a trial-level model. Set size and task are factors, and Correct is
# the reference level of trial type. Positive errors indicate outward
# overshoot; negative errors indicate inward undershoot.
data_dot_deviation_exp2_lmm <-
  data_dot_deviation_exp2 %>%
  mutate(
    trial_type = relevel(
      factor(trial_type),
      ref = "Correct"
    ),
    set_size = factor(
      correct_num,
      levels = c(3, 4)
    ),
    task = factor(
      task_label,
      levels = c(
        "Innermost",
        "Second innermost",
        "Second outermost",
        "Outermost"
      )
    )
  ) %>%
  arrange(
    SubID,
    set_size,
    task,
    trial_type
  )


model_dot_deviation_exp2 <- lmerTest::lmer(
  error_x_deg ~
    trial_type * set_size * task +
    (1 + trial_type || SubID),
  data = data_dot_deviation_exp2_lmm,
  REML = TRUE,
  control = lme4::lmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 100000)
  )
)


summary(model_dot_deviation_exp2)


sjPlot::tab_model(
  model_dot_deviation_exp2,
  p.style = "scientific_stars",
  show.se = TRUE,
  show.stat = TRUE,
  digits = 3
)


# Compare RM with Correct trials separately for every task and set size. Holm
# correction is applied jointly across the eight planned comparisons.
emm_dot_deviation_exp2 <- emmeans::emmeans(
  model_dot_deviation_exp2,
  ~ trial_type |
    set_size * task,
  lmer.df = "satterthwaite"
)


result_dot_RM_vs_correct_exp2 <- emmeans::contrast(
  emm_dot_deviation_exp2,
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
    task_label = factor(
      as.character(task),
      levels = c(
        "Innermost",
        "Second innermost",
        "Second outermost",
        "Outermost"
      )
    )
  ) %>%
  arrange(
    correct_num,
    task_label
  )


# Cohen's dz uses each participant's paired RM-minus-Correct difference.
# Cells without both response types do not contribute to dz, but their trials
# remain in the mixed model above.
data_dot_RM_vs_correct_exp2_by_pp <-
  data_dot_deviation_exp2_by_pp %>%
  select(
    SubID,
    correct_num,
    task_label,
    trial_type,
    mean_horizontal_error
  ) %>%
  tidyr::pivot_wider(
    names_from = trial_type,
    values_from = mean_horizontal_error
  ) %>%
  filter(
    !is.na(RM),
    !is.na(Correct)
  ) %>%
  mutate(
    difference_RM_minus_correct = RM - Correct
  ) %>%
  arrange(
    correct_num,
    task_label,
    SubID
  )


data_dot_effect_size_exp2 <-
  data_dot_RM_vs_correct_exp2_by_pp %>%
  group_by(
    correct_num,
    task_label
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
  ) %>%
  arrange(
    correct_num,
    task_label
  )


result_dot_RM_vs_correct_exp2 <-
  result_dot_RM_vs_correct_exp2 %>%
  left_join(
    data_dot_effect_size_exp2,
    by = c(
      "correct_num",
      "task_label"
    )
  ) %>%
  arrange(
    correct_num,
    task_label
  )


result_dot_RM_vs_correct_exp2


my_plot_dot_deviation_exp2 <- ggplot(
  data_dot_deviation_exp2_by_pp,
  aes(
    x = trial_type,
    y = mean_horizontal_error,
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
    trim = TRUE,
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
    data = data_dot_deviation_exp2_across_pp,
    aes(
      x = trial_type,
      y = horizontal_error_mean
    ),
    inherit.aes = FALSE,
    size = 3,
    color = "black"
  ) +
  geom_errorbar(
    data = data_dot_deviation_exp2_across_pp,
    aes(
      x = trial_type,
      y = horizontal_error_mean,
      ymin = horizontal_error_mean - horizontal_error_CI,
      ymax = horizontal_error_mean + horizontal_error_CI
    ),
    inherit.aes = FALSE,
    width = 0,
    linewidth = 0.5,
    color = "black"
  ) +
  facet_grid(
    rows = vars(correct_num),
    cols = vars(task_label),
    labeller = labeller(
      correct_num = c(
        "3" = "Set Size 3",
        "4" = "Set Size 4"
      )
    )
  ) +
  scale_fill_manual(
    values = c(
      "RM" = "#800074",
      "Correct" = "#298C8C"
    ),
    name = ""
  ) +
  scale_color_manual(
    values = c(
      "RM" = "#800074",
      "Correct" = "#298C8C"
    ),
    name = ""
  ) +
  guides(
    fill = "none",
    color = "none"
  ) +
  scale_y_continuous(
    limits = c(-4, 4),
    breaks = seq(-4, 4, by = 1),
    expand = expansion(mult = c(0.05, 0.10))
  ) +
  labs(
    x = "Trial Type",
    y = "Horizontal Dot Deviation (°)"
  ) +
  my_plot_theme +
  theme(
    strip.text.y = element_text(
      size = 12,
      face = "bold",
      angle = 270
    ),
    panel.spacing.x = grid::unit(0.8, "lines"),
    panel.spacing.y = grid::unit(1.5, "lines")
  )


my_plot_dot_deviation_exp2







