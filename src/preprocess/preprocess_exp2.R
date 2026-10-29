library(dplyr)

# set working dir
setwd("D:/OneDrive/projects/RM_action/data_ens/EXP_2/")

# get all csv files
file_list <- list.files(pattern = "\\.csv$", full.names = TRUE)

# read and combine
data <- file_list %>%
  setNames(basename(.)) %>%
  lapply(
    readr::read_csv,
    col_types = readr::cols(.default = readr::col_character()),
    show_col_types = FALSE
  ) %>% 
  bind_rows(.id = "source_file")

parse_num <- function(x) {
  suppressWarnings(readr::parse_number(as.character(x)))
}

parse_logical <- function(x) {
  case_when(
    tolower(trimws(as.character(x))) == "true" ~ TRUE,
    tolower(trimws(as.character(x))) == "false" ~ FALSE,
    TRUE ~ NA
  )
}

# remove unnamed columns 
data <- data %>%
  select(-matches("^\\.\\.\\.[0-9]+$"))


# remove cols that all are NA
data <- data %>%
  select(where(~ !all(is.na(.x))))


# Check that all raw columns needed for preprocessing are available.
required_raw_cols <- c(
  "phase",
  "block_index",
  "task",
  "trial_index",
  "fixation_duration_sec",
  "hemifield",
  "setsize",
  "edge_spacing_deg",
  "line_width_deg",
  "line_length_deg",
  "innermost_ecc_deg",
  "target_index_from_innermost",
  "target_x_deg",
  "target_y_deg",
  "touch_x_deg",
  "touch_y_deg",
  "touch_rt_sec",
  "error_y_deg",
  "error_euclidean_deg",
  "post_stimulus_mask_gap_sec",
  "touch_accepted",
  "number_response",
  "number_rt_sec",
  "number_touch_x_deg",
  "number_touch_y_deg",
  "number_accepted",
  "participant",
  "age",
  "gender",
  "date"
)

missing_raw_cols <- setdiff(
  required_raw_cols,
  names(data)
)

if (length(missing_raw_cols) > 0) {
  stop(
    paste(
      "Missing required EXP_2 columns:",
      paste(missing_raw_cols, collapse = ", ")
    )
  )
}


# Use the same demographic column name as Experiment 1.
data <- data %>%
  rename(sex = gender)

# remove practice rows

data <- data %>%
  filter(phase == "main")


cols_to_keep <- c(
  "source_file",
  "block_index",
  "task",
  "trial_index",
  "fixation_duration_sec",
  "hemifield",
  "setsize",
  "edge_spacing_deg",
  "line_width_deg",
  "line_length_deg",
  "target_index_from_innermost",
  "target_x_deg",
  "target_y_deg",
  "touch_x_deg",
  "touch_y_deg",
  "touch_rt_sec",
  "touch_accepted",
  "error_y_deg",
  "error_euclidean_deg",
  "post_stimulus_mask_gap_sec",
  "number_response",
  "number_rt_sec",
  "number_touch_x_deg",
  "number_touch_y_deg",
  "number_accepted",
  "innermost_ecc_deg",
  "participant",
  "participant_raw",
  "age",
  "sex",
  "date",
  "data_quality_note"
)


# rename participant IDs
data <- data %>%
  mutate(
    participant_raw = participant,
    participant = sprintf(
      "%03d",
      as.integer(parse_num(source_file))
    ),
    data_quality_note = case_when(
      participant == "010" ~ paste(
        "Self-reported pressing 2 when seeing 4 lines",
        "in blocks 1-9."
      ),
      participant == "012" ~ "Reported completing only 24 of 40 blocks.",
      participant == "013" ~ "Self-reported pressing 7 instead of 4.",
      TRUE ~ NA_character_
    )
  )




# columns that should be numeric
num_cols <- c(
  "block_index",
  "trial_index",
  "fixation_duration_sec",
  "setsize",
  "edge_spacing_deg",
  "line_width_deg",
  "line_length_deg",
  "target_index_from_innermost",
  "target_x_deg",
  "target_y_deg",
  "touch_x_deg",
  "touch_y_deg",
  "touch_rt_sec",
  "error_y_deg",
  "error_euclidean_deg",
  "post_stimulus_mask_gap_sec",
  "number_response",
  "number_rt_sec",
  "number_touch_x_deg",
  "number_touch_y_deg",
  "innermost_ecc_deg",
  "age",
  "touch_rt_sec"
)

logical_cols <- c(
  "touch_accepted",
  "number_accepted"
)

# adjust sign:
# after adjustment, + --> overshot, - --> undershot
df_clean <- data %>%
  select(any_of(cols_to_keep)) %>%
  mutate(
    across(any_of(num_cols), parse_num),
    across(any_of(logical_cols), parse_logical),

    sex = tolower(trimws(sex)),
    
    edge_spacing_deg = round(edge_spacing_deg, 2),
    
    error_x_raw_deg = round(touch_x_deg - target_x_deg, 2),
    
    hemifield = tolower(trimws(hemifield)), #remove extra spaces before and after texts; all lowercase
    
    number_deviation = number_response - setsize,
    
    error_x_deg = case_when(
      hemifield == "right" ~ error_x_raw_deg,
      hemifield == "left"  ~ -error_x_raw_deg,
      TRUE ~ NA_real_
    )
  )

# RM
df_clean <- df_clean %>%
  mutate(
    type = case_when(
      number_deviation == 0 ~ "correct",
      number_deviation < 0  ~ "RM",
      number_deviation > 0  ~ "overreport"
    )
  ) 

# check the output before saving
expected_participants <- sprintf(
  "%03d",
  as.integer(parse_num(basename(file_list)))
)

analysis_cols <- c(
  "participant",
  "age",
  "sex",
  "task",
  "hemifield",
  "setsize",
  "number_response",
  "target_x_deg",
  "touch_x_deg",
  "number_deviation",
  "error_x_deg",
  "type"
)

# if (!setequal(unique(df_clean$participant), expected_participants)) {
#   stop("Participant numbers do not match the EXP_2 filenames.")
# }
# 
# if (any(!complete.cases(df_clean[analysis_cols]))) {
#   stop("The EXP_2 output has missing values in analysis columns.")
# }
# 
# if (anyDuplicated(df_clean[c("source_file", "block_index", "trial_index")])) {
#   stop("The EXP_2 output has duplicated trials.")
# }

# data_demographic_check <- df_clean %>%
#   distinct(
#     participant,
#     age,
#     sex
#   ) %>%
#   arrange(participant)

# if (anyDuplicated(data_demographic_check$participant)) {
#   stop("At least one EXP_2 participant has inconsistent age or sex values.")
# }
# 
# if (!all(data_demographic_check$sex %in% c("female", "male"))) {
#   stop("Unexpected values were found in the EXP_2 sex column.")
# }
# 
# data_trial_count_check <- df_clean %>%
#   count(
#     participant,
#     name = "n_trials"
#   ) %>%
#   arrange(participant)
# 
# 
# data_demographic_check
# data_trial_count_check

readr::write_csv(df_clean, "../data_RMaction_exp2.csv")
