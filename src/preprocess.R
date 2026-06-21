library(dplyr)

# set working dir
setwd("D:/OneDrive/projects/RM_action/data_ens/data_exp1/")

# get all csv files
file_list <- list.files(pattern = "\\.csv$", full.names = TRUE)
file_list <- file_list[basename(file_list) != "data.csv"]

# read and combine
data <- file_list %>%
  lapply(
    readr::read_csv,
    col_types = readr::cols(.default = readr::col_character()),
    show_col_types = FALSE
  ) %>% 
  bind_rows()


first_non_na <- function(x) {
  x2 <- x[!is.na(x) & x != ""]
  if (length(x2) == 0) NA else x2[1]
}

parse_num <- function(x) {
  suppressWarnings(readr::parse_number(as.character(x)))
}


df_trial_rows <- data %>%
  filter(
    !(
      is.na(first) &
        is.na(second) &
        is.na(third) &
        is.na(fourth) &
        is.na(position) &
        is.na(nblines)
    )
    |
      !is.na(`key_resp_2.keys`) |
      !is.na(`key_resp_2.rt`)
  )

# check: should usually be 2 rows per trial

# create pair index: every 2 consecutive rows = 1 trial
df_trial_rows <- df_trial_rows %>%
  mutate(trial_pair = rep(1:(n()/2), each = 2, length.out = n()))

# merge the two rows of each trial
df_clean <- df_trial_rows %>%
  group_by(trial_pair) %>%
  summarise(
    thisN = first_non_na(thisN),
    thisTrialN = first_non_na(thisTrialN),
    thisRepN = first_non_na(thisRepN),
    
    first = first_non_na(first),
    second = first_non_na(second),
    # third = first_non_na(third),
    # fourth = first_non_na(fourth),
    
    spacing = first_non_na(spacing),
    eccentricity = first_non_na(excentricity),# wrong spelling in psychopy
    visual_field = first_non_na(position),
    set_size = first_non_na(nblines),
    
    key_resp_2.keys = first_non_na(`key_resp_2.keys`),
    num_rt   = first_non_na(`key_resp_2.rt`),
    
    outerloop.thisRepN = first_non_na(`outerloop.thisRepN`),
    dot_position = first_non_na(position_x2),
    
    participant = first_non_na(participant),
    sex = first_non_na(sexe),
    age = first_non_na(`âge`),
    .groups = "drop"
  )


# rename number resp
df_clean <- df_clean %>%
  mutate(
    num_resp = stringr::str_extract(`key_resp_2.keys`, "\\d+"),
    num_resp = as.integer(num_resp)
  ) %>%
  select(-`key_resp_2.keys`, -trial_pair)


# get first and second line positions
get_x <- function(x) {
  as.numeric(stringr::str_match(x, "^\\(([-0-9.]+),")[, 2])
}

df_clean <- df_clean %>%
  mutate(
    first  = get_x(first),
    second = get_x(second)
  )

# get the useful cols
df_clean <- df_clean %>% 
  mutate(
    spacing = parse_num(spacing),
    eccentricity = parse_num(eccentricity),
    set_size = parse_num(set_size),
    num_rt = parse_num(num_rt),
    outerloop.thisRepN = parse_num(outerloop.thisRepN),
    first = abs(first),
    second = abs(second),
    target_position = (first + second)/2,
    dot_position = abs(parse_num(dot_position)),
    number_deviation = num_resp - set_size,
    dot_deviation = dot_position - target_position
  )

# RM
df_clean <- df_clean %>%
  mutate(
    type = case_when(
      number_deviation == 0 ~ "noRM",
      number_deviation < 0  ~ "RM",
      number_deviation > 0  ~ "overreport"
    )
  ) 

# # only keep RM and noRM?
# df_clean <- df_clean %>%
#   filter(type != "overreport")
# 

# rename participant IDs
df_clean <- df_clean %>%
  mutate(
    participant_raw = participant,
    
    participant_num = ifelse(
      grepl("[0-9]+", participant_raw),
      sub(".*?([0-9]+).*", "\\1", participant_raw, perl = TRUE),
      NA_character_
    ),
    
    participant = sprintf("%03d", as.integer(participant_num))
  ) %>%
  select(-participant_num)


readr::write_csv(df_clean, "data_RMaction_exp1.csv")

