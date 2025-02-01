pacman::p_load(lubridate, dplyr, stringr, tidyr, readr, purrr)

to_formatted_ts <- function(ts) {
  iso_week <- isoweek(ts)
  
  if (iso_week == 1 && month(ts) == 12) {
    iso_year <- year(ts) + 1
  } else {
    iso_year <- year(ts)
  }
  
  day_of_week <- wday(ts, week_start = 1) - 1 
  week_of_month <- sprintf("%01d", (day(ts) - 1) %/% 7 + 1)
  hour_of_week <- day_of_week * 24 + hour(ts) + 1 
  day_name <- str_to_upper(wday(ts, label = TRUE, abbr = TRUE))
  two_digit_hour <- sprintf("%02d", hour(ts)) 
  dst <- if (format(ts, "%Z") == "CEST") "S" else "W"
  
  # Format the timestamp
  sprintf("C%s%sH%s-%02dW%02dH%03d%s", 
          week_of_month, 
          day_name, 
          two_digit_hour, 
          iso_year %% 100, 
          iso_week, 
          hour_of_week, 
          dst)
}

rwd2ts <- function(ts, bc_type, rwd_formula) {
  result <- NA_POSIXct_
  
  if (str_detect(rwd_formula, "^\\d\\d")) {
    days_offset <- as.integer(str_sub(rwd_formula, 1, 2))
    result_a <- ts + days(days_offset)
    result <- update(result_a, hour = as.integer(str_sub(rwd_formula, 5, 6)))
  } else if (rwd_formula == "tw" & bc_type != "h") {
    result <- ts + days(7)
  } 
  
  return(result)
}

get_cycle <- function(a_date) {
  ref_date_B_cycle <- ymd_hms("2019-10-17 13:00:00", tz = "Europe/Amsterdam")
  i_diff <- int_length(interval(ref_date_B_cycle, a_date)) / 3600L / 24L
  if (i_diff %% 2L == 0) "B" else "A"
}

source("R/cleanup-moro.R", encoding = "UTF-8")

# Create a sequence of hourly timestamps for the given year in the Europe/Amsterdam timezone
const_tz <- "Europe/Amsterdam"
from <- ymd_hms("2024-12-26 13:00:00", tz = const_tz, quiet = T)
to <- from + weeks(53) - hours(1)
timestamps <- seq(from, to, by = "hour")

# Create a tibble with regular and formatted timestamps
yearly_timestamps <- tibble(regular_timestamp = timestamps,
                            formatted_timestamp = sapply(timestamps, to_formatted_ts))

yearly_timestamps_a <- yearly_timestamps |> 
  separate_wider_delim(cols = formatted_timestamp, delim = "-", names = c("bc_slot", "bc_id"))

cz_week_starts_a <- yearly_timestamps_a |> filter(str_detect(bc_slot, "THUH13"))
cz_ab_week_X <- get_cycle(cz_week_starts_a$regular_timestamp[1])

if (cz_ab_week_X == "A") {
  cz_ab_week_Y <- "B"
} else {
  cz_ab_week_Y <- "A"
}

cz_week_starts_b <- cz_week_starts_a |> mutate(bc_week = row_number() - 1L,
                                           idx = row_number() %% 2L,
                                           bc_ab_week = if_else(idx == 1, cz_ab_week_X, cz_ab_week_Y)) |> 
  select(bc_id, bc_week, bc_ab_week)

yearly_timestamps_b <- yearly_timestamps_a |> left_join(cz_week_starts_b, by = join_by(bc_id)) |> 
  fill(bc_week, bc_ab_week, .direction = "down") |> 
  mutate(bc_slot_tw = paste0("C", bc_ab_week, str_sub(bc_slot, 3))) |> 
  pivot_longer(cols = starts_with("bc_slot"), names_to = "mk_name", values_to = "mk_value") |> 
  select(regular_timestamp:bc_ab_week, bc_slot = mk_value)

cz_schedule <- yearly_timestamps_b |> inner_join(moro_std_2, by = join_by(bc_slot))

cz_schedule_w_rewinds <- cz_schedule |> 
  mutate(bc_rewind_ts = mapply(rwd2ts, regular_timestamp, bc_type, bc_rewind, SIMPLIFY = F),
         bc_rewind_ts = map_dbl(bc_rewind_ts, ~ as.numeric(.x)) |> as_datetime(tz = "Europe/Amsterdam"))

wrk_rwd <- cz_schedule_w_rewinds |> select(bc_id, bc_rewind_ts) |> filter(!is.na(bc_rewind_ts)) |> 
  mutate(bc_rewind_slot = lapply(bc_rewind_ts, to_formatted_ts)) |> 
  separate(col = bc_rewind_slot, into = c("bc_slot_rwd", "bc_id_rwd")) |> select(-bc_rewind_ts)

cz_schedule_w_rewinds_b <- cz_schedule_w_rewinds |> left_join(wrk_rwd, by = join_by(bc_id)) |> 
  filter(!(bc_type == "h" & bc_rewind == "tw")) |> select(-bc_rewind)

cz_schedule_rwd_only <- cz_schedule_w_rewinds_b |> filter(!is.na(bc_rewind_ts)) |> 
  select(regular_timestamp = bc_rewind_ts,
         bc_rewind_of_id = bc_id,
         bc_id = bc_id_rwd,
         bc_slot = bc_slot_rwd,
         bc_len:bc_type) |> 
  mutate(bc_ab_week = NA_character_, bc_week = NA_integer_) |> 
  select(regular_timestamp, bc_id, bc_week, bc_ab_week, bc_slot:bc_type, bc_rewind_of_id)

cz_schedule_orig_only <- cz_schedule_w_rewinds_b |> select(-c(bc_rewind_ts:bc_id_rwd))

cz_schedule_final <- bind_rows(cz_schedule_orig_only, cz_schedule_rwd_only) |> arrange(regular_timestamp) |> 
  mutate(bc_ab_week = if_else(str_detect(bc_slot, "THUH13") & is.na(bc_ab_week), "A", bc_ab_week),
         bc_week = if_else(bc_ab_week == "A" & is.na(bc_week), lead(bc_week), bc_week)) |> 
  fill(bc_week, bc_ab_week, .direction = "down")
