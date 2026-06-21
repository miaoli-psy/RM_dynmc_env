library(dplyr)

# set working dir
setwd("D:/OneDrive/projects/RM_action/data_ens/data_exp2/")

# get all csv files
file_list <- list.files(pattern = "\\.csv$", full.names = TRUE)

# read and combine
data <- file_list %>%
  lapply(
    readr::read_csv,
    col_types = readr::cols(.default = readr::col_character()),
    show_col_types = FALSE
  ) %>% 
  bind_rows()

# remove unnamed columns 
data <- data %>%
  select(-matches("^\\.\\.\\.[0-9]+$"))


# remove cols that all are NA
data <- data %>%
  select(where(~ !all(is.na(.x))))

# remove practice rows

data <- data %>%
  filter(phase == "main")


cols_to_keep <- c(
  "block_index",
  "task",
  "trial_index",
  "hemifield",
  "setsize",
  "edge_spacing_deg",
  "line_width_deg",
  "target_index_from_innermost",
  "target_x_deg",
  "target_y_deg",
  "touch_x_deg",
  "touch_y_deg",
  "number_response",
  "innermost_ecc_deg",
  "number_rt_sec",
  "age",
  "gender",
  "touch_rt_sec",
  "participant"
)


# rename participant IDs
data <- data %>%
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




# columns that should be numeric
num_cols <- c(
  "block_index",
  "trial_index",
  "setsize",
  "edge_spacing_deg",
  "line_width_deg",
  "target_index_from_innermost",
  "target_x_deg",
  "target_y_deg",
  "touch_x_deg",
  "touch_y_deg",
  "number_response",
  "innermost_ecc_deg",
  "number_rt_sec",
  "age",
  "touch_rt_sec"
)

parse_num <- function(x) {
  suppressWarnings(readr::parse_number(as.character(x)))
}

# adjust sign:
# after adjustment, + --> overshot, - --> undershot
df_clean <- data %>%
  select(any_of(cols_to_keep)) %>%
  mutate(
    across(any_of(num_cols), parse_num),
    
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
      number_deviation == 0 ~ "noRM",
      number_deviation < 0  ~ "RM",
      number_deviation > 0  ~ "overreport"
    )
  ) 

# readr::write_csv(df_clean, "data_RMaction_exp2.csv")
