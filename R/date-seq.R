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

count_thursdays <- function(year) {
  dates <- seq(ymd(paste0(year, "-01-01")), ymd(paste0(year, "-12-31")), by = "day")
  sum(wday(dates, label = TRUE) == "Thu")
}

source("R/cleanup-moro.R", encoding = "UTF-8")

# Create a sequence of hourly timestamps for the given year in the Europe/Amsterdam timezone
const_tz <- "Europe/Amsterdam"
from <- ymd_hms("2024-12-26 13:00:00", tz = const_tz, quiet = T)
to <- from + weeks(53) - hours(1)
timestamps <- seq(from, to, by = "hour")
n_thurdays <- count_thursdays(1 + year(from))

# Create a tibble with regular and formatted timestamps
yearly_timestamps <- tibble(regular_timestamp = timestamps,
                            formatted_timestamp = sapply(timestamps, to_formatted_ts))

yearly_timestamps_a <- yearly_timestamps |> 
  separate_wider_delim(cols = formatted_timestamp, delim = "-", names = c("bc_slot", "bc_id"))

cz_week_starts_a <- yearly_timestamps_a |> filter(str_detect(bc_slot, "THUH13"))
cz_ab_week_X <- get_cycle(cz_week_starts_a$regular_timestamp[[1]])

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
  filter(!(bc_type == "h" & bc_rewind == "tw")) # |> select(-bc_rewind)

cz_schedule_rwd_only <- cz_schedule_w_rewinds_b |> filter(!is.na(bc_rewind_ts)) |> 
  select(regular_timestamp = bc_rewind_ts,
         bc_rewind_of_id = bc_id,
         bc_id = bc_id_rwd,
         bc_slot = bc_slot_rwd,
         bc_len:bc_type) |> 
  mutate(bc_ab_week = NA_character_, bc_week = NA_integer_) |> 
  select(regular_timestamp, bc_id, bc_week, bc_ab_week, bc_slot:bc_type, bc_rewind_of_id)

cz_schedule_orig_only <- cz_schedule_w_rewinds_b |> select(-c(bc_rewind_ts:bc_id_rwd))

cz_schedule_bind_a <- bind_rows(cz_schedule_orig_only, cz_schedule_rwd_only) |> arrange(regular_timestamp) 

wrk_wk1 <- cz_schedule_bind_a |> filter(bc_week == 1) |> head(1)
wrk_wk1_ts <- wrk_wk1$regular_timestamp[[1]]
cz_schedule_bind_b <- cz_schedule_bind_a |> 
  mutate(wrk_bc_week_diff = int_length(int = interval(start = wrk_wk1_ts, end = regular_timestamp)),
         bc_week = 1 + wrk_bc_week_diff %/% 604800) |> 
  mutate(wrk_week_mod_nthu = bc_week %% n_thurdays, 
         bc_week = case_when(bc_week == 0 ~ 0,
                             wrk_week_mod_nthu == 0 ~ n_thurdays, 
                             T ~ wrk_week_mod_nthu))

wrk_abweek_even <- cz_schedule_bind_b$bc_ab_week[[1]]
wrk_abweek_odd <- if (wrk_abweek_even == "A") "B" else "A"

cz_schedule_final <- cz_schedule_bind_b |> 
  mutate(bc_ab_week = if_else(bc_week %% 2 == 0, wrk_abweek_even, wrk_abweek_odd),
         bc_slot = if_else(bc_rewind == "tw", paste0("C", bc_ab_week, str_sub(bc_slot, 3)), bc_slot)) |> 
  rename(bc_ts = regular_timestamp) |> select(-starts_with("wrk"))

bcid2bcts <- cz_schedule_final |> select(bc_rewind_of_id = bc_id, bc_rewind_of_ts = bc_ts) |> arrange(bc_rewind_of_id)

pgms_A <- c("Sonoor", "In de Schijnwerper", "Noorderlicht", "Strijkkwartet", "Sanssouci", "De eigenzinnige Prokofjev")
pgms_B <- c("CZ-Live klassiek", "CZ-Live hedendaags", "CZ-Live oud", "CZ-Archief", "CZ-Live wereld")
dutch_weekdays <- c("zo", "ma", "di", "wo", "do", "vr", "za")

bc_ts_fmt <- function(a_date) {
  paste0(format(a_date, "%Y-%m-%d_"), 
         dutch_weekdays[as.integer(format(a_date, "%w")) + 1], 
         format(a_date, "%H"),
         "u")
}

cz_schedule_view_1 <- cz_schedule_final |> 
  left_join(bcid2bcts, by = join_by(bc_rewind_of_id)) |> 
  filter(bc_title %in% pgms_B & bc_ts > "2025-02-05" & bc_ts < "2025-03-21") |> 
  mutate(bc_start = if_else(is.na(bc_ts), NA, bc_ts_fmt(bc_ts)),
         bc_start_replay_of = if_else(is.na(bc_rewind_of_ts), NA, bc_ts_fmt(bc_rewind_of_ts))) |> 
  select(bc_start, bc_title, bc_start_replay_of)

write_tsv(cz_schedule_view_1, "/mnt/muw/cz_gids_pgms_B_SOLL.tsv", append = F, na = "")

