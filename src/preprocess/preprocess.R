library(dplyr)

# set working dir
setwd("D:/OneDrive/projects/RM_action/data_ens/EXP_1/Main/")


# include Lille data?
# FALSE = ENS data only, participants 001-024
# TRUE = ENS + all files in data_lille; Lille IDs continue from 025
include_lille_data <- TRUE

n_ens_participants <- 24


first_non_na <- function(x) {
  x2 <- x[!is.na(x) & x != ""]
  if (length(x2) == 0) NA_character_ else x2[1]
}

parse_num <- function(x) {
  suppressWarnings(readr::parse_number(as.character(x)))
}

read_data_files <- function(file_list, data_site) {
  file_list %>%
    setNames(basename(.)) %>%
    lapply(
      readr::read_csv,
      col_types = readr::cols(.default = readr::col_character()),
      show_col_types = FALSE
    ) %>%
    bind_rows(.id = "source_file") %>%
    mutate(data_site = data_site)
}

standardize_lille_cols <- function(data) {
  data %>%
    mutate(
      thisN = coalesce(
        `bloc1.thisN`,
        `bloc2.thisN`
      ),
      thisTrialN = coalesce(
        `bloc1.thisTrialN`,
        `bloc2.thisTrialN`
      ),
      thisRepN = coalesce(
        `bloc1.thisRepN`,
        `bloc2.thisRepN`
      ),
      outerloop.thisN = case_when(
        !is.na(`bloc1.thisN`) ~ "0",
        !is.na(`bloc2.thisN`) ~ "1"
      ),
      outerloop.thisRepN = outerloop.thisN
    ) %>%
    select(
      -matches("^bloc[12]\\.")
    )
}


# get ENS csv files
ens_file_list <- list.files(
  pattern = "\\.csv$",
  full.names = TRUE
)

ens_participant_numbers <- as.integer(
  parse_num(basename(ens_file_list))
)

if (
  length(ens_file_list) != n_ens_participants ||
    !setequal(ens_participant_numbers, 1:n_ens_participants)
) {
  stop("Expected ENS participant files 1-24.")
}


# get Lille csv files only when requested
lille_file_list <- character(0)
lille_participant_numbers <- integer(0)
n_lille_participants <- 0

if (include_lille_data) {
  lille_file_list <- list.files(
    path = "../../../data_lille",
    pattern = "\\.csv$",
    full.names = TRUE
  )

  lille_participant_numbers <- as.integer(
    parse_num(basename(lille_file_list))
  )

  if (length(lille_file_list) == 0) {
    stop("No Lille csv files were found in data_lille.")
  }

  lille_file_order <- order(
    lille_participant_numbers,
    basename(lille_file_list)
  )

  lille_file_list <- lille_file_list[lille_file_order]
  lille_participant_numbers <-
    lille_participant_numbers[lille_file_order]
  n_lille_participants <- length(lille_file_list)
}


# read and combine
data <- read_data_files(
  ens_file_list,
  data_site = "ens"
) %>%
  mutate(
    participant_output_num = as.integer(
      parse_num(source_file)
    )
  )

if (include_lille_data) {
  data_lille <- read_data_files(
    lille_file_list,
    data_site = "lille"
  ) %>%
    standardize_lille_cols() %>%
    mutate(
      participant_output_num =
        n_ens_participants +
        match(
          source_file,
          basename(lille_file_list)
        )
    )

  data <- bind_rows(
    data,
    data_lille
  )
}


data <- data %>%
  rename(
    eccentricity = excentricity
  ) %>%
  mutate(
    block_index = parse_num(`outerloop.thisN`),
    trial_index = parse_num(thisN)
  )


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


# merge the two rows of each trial
df_clean <- df_trial_rows %>%
  group_by(
    data_site,
    source_file,
    block_index,
    trial_index
  ) %>%
  summarise(
    thisN = first_non_na(thisN),
    thisTrialN = first_non_na(thisTrialN),
    thisRepN = first_non_na(thisRepN),

    first = first_non_na(first),
    second = first_non_na(second),
    # third = first_non_na(third),
    # fourth = first_non_na(fourth),

    spacing = first_non_na(spacing),
    eccentricity = first_non_na(eccentricity),
    visual_field = first_non_na(position),
    set_size = first_non_na(nblines),

    key_resp_2.keys = first_non_na(`key_resp_2.keys`),
    num_rt = first_non_na(`key_resp_2.rt`),

    outerloop.thisRepN = first_non_na(
      `outerloop.thisRepN`
    ),
    dot_position = first_non_na(position_x2),

    participant_raw = first_non_na(participant),
    participant = sprintf(
      "%03d",
      as.integer(first(participant_output_num))
    ),
    sex = first_non_na(sexe),
    age = first_non_na(`âge`),
    .groups = "drop"
  ) %>%
  select(-data_site)


# rename number resp
df_clean <- df_clean %>%
  mutate(
    num_resp = stringr::str_extract(
      `key_resp_2.keys`,
      "\\d+"
    ),
    num_resp = as.integer(num_resp)
  ) %>%
  filter(!is.na(num_resp)) %>% # remove unfinished trials
  select(-`key_resp_2.keys`)


# get first and second line positions
get_x <- function(x) {
  as.numeric(
    stringr::str_match(
      x,
      "^\\(([-0-9.]+),"
    )[, 2]
  )
}

df_clean <- df_clean %>%
  mutate(
    first = get_x(first),
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
    target_position = (first + second) / 2,
    dot_position = abs(parse_num(dot_position)),
    number_deviation = num_resp - set_size,
    dot_deviation = dot_position - target_position
  )


# RM
df_clean <- df_clean %>%
  mutate(
    type = case_when(
      number_deviation == 0 ~ "correct",
      number_deviation < 0 ~ "RM",
      number_deviation > 0 ~ "overreport"
    )
  )


# check the output before saving
expected_participants <- sprintf(
  "%03d",
  ens_participant_numbers
)

expected_n_participants <- n_ens_participants

if (include_lille_data) {
  expected_participants <- c(
    expected_participants,
    sprintf(
      "%03d",
      n_ens_participants +
        seq_len(n_lille_participants)
    )
  )

  expected_n_participants <-
    n_ens_participants + n_lille_participants
}

analysis_cols <- c(
  "participant",
  "set_size",
  "num_resp",
  "target_position",
  "dot_position",
  "number_deviation",
  "dot_deviation",
  "type"
)

# if (
#   n_distinct(df_clean$participant) != expected_n_participants ||
#     !setequal(unique(df_clean$participant), expected_participants)
# ) {
#   stop("Participant numbers do not match the selected data files.")
# }
#
# if (any(!complete.cases(df_clean[analysis_cols]))) {
#   stop("The EXP_1 output has missing values in analysis columns.")
# }
#
# if (
#   anyDuplicated(
#     df_clean[c("source_file", "block_index", "trial_index")]
#   )
# ) {
#   stop("The EXP_1 output has duplicated trials.")
# }


readr::write_csv(
  df_clean,
  "../../data_RMaction_exp1.csv"
)
