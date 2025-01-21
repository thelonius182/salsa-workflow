pacman::p_load(lubridate, dplyr, stringr, tidyr, readr)

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
  timezone <- case_when(ts %in% dst_ts ~ "X",
                        format(ts, "%Z") == "CEST" ~ "S",
                        T ~ "W")
  
  # Format the timestamp
  sprintf("C%s%sH%s-%02dW%02dH%03d%s", 
          week_of_month, 
          day_name, 
          two_digit_hour, 
          iso_year %% 100, 
          iso_week, 
          hour_of_week, 
          timezone)
}

get_cycle <- function(cz_week_start) {
  ref_date_B_cycle <- ymd_hms("2019-10-17 13:00:00", tz = "Europe/Amsterdam")
  i_diff <- int_length(interval(ref_date_B_cycle, cz_week_start)) / 3600L / 24L
  if_else(i_diff %% 2L == 0, "B", "A")
}

source("R/cleanup-moro.R", encoding = "UTF-8")

# Create a sequence of hourly timestamps for the given year in the Europe/Amsterdam timezone
const_tz <- "Europe/Amsterdam"
timestamps <- seq(
  from = ymd_hms("2024-12-26 13:00:00", tz = const_tz),
  to   = ymd_hms("2026-01-01 13:00:00", tz = const_tz),
  by = "hour"
)

# Extract DST-timestamps
dst_flags <- dst(timestamps)
transition_indices <- which(diff(dst_flags) != 0)
dst_ts <- timestamps[transition_indices]

# Create a tibble with regular and formatted timestamps
yearly_timestamps <- tibble(regular_timestamp = timestamps,
                            formatted_timestamp = sapply(timestamps, to_formatted_ts))

yearly_timestamps_a <- yearly_timestamps |> 
  separate_wider_delim(cols = formatted_timestamp, delim = "-", names = c("moro_key", "bc_id"))

cz_week_starts <- yearly_timestamps_a |> filter(str_detect(moro_key, "THUH13"))
cz_ab_week_X <- get_cycle(cz_week_starts$regular_timestamp[1])

if (cz_ab_week_X == "A") {
  cz_ab_week_Y <- "B"
} else {
  cz_ab_week_Y <- "A"
}

cz_week_starts <- cz_week_starts |> mutate(idx = row_number() %% 2L,
                                           bc_ab_week = if_else(idx == 1, cz_ab_week_X, cz_ab_week_Y)) |> 
  select(bc_id, bc_ab_week)

yearly_timestamps_b <- yearly_timestamps_a |> left_join(cz_week_starts, by = join_by(bc_id)) |> 
  fill(bc_ab_week, .direction = "down") |> 
  mutate(moro_key_ab = paste0("C", bc_ab_week, str_sub(moro_key, 3))) |> 
  pivot_longer(cols = matches("moro_key"), names_to = "m_label", values_to = "m_value") |> 
  select(regular_timestamp, bc_id, moro_key = m_value)

cz_schedule <- yearly_timestamps_b |> inner_join(moro_std_2, by = join_by(moro_key))

cz_schedule_w_rewinds <- cz_schedule |> 
  mutate(bc_rewind_offset_chr = if_else(str_detect(bc_rewind, "^\\d\\d"),
                                str_sub(bc_rewind, 1, 2),
                                "0"),
         bc_rewind_offset = as.integer(bc_rewind_offset_chr),
         bc_rewind_ts = case_when(bc_rewind_offset > 0 ~ regular_timestamp + days(bc_rewind_offset), 
                                  bc_rewind_offset == 0 & bc_rewind == "tw" & bc_type != "h" ~ regular_timestamp + days(7),
                                  T ~ NA_POSIXct_),
         bc_rewind_ts = if_else(bc_rewind_offset > 0, 
                                update(bc_rewind_ts, hour = as.integer(str_sub(bc_rewind, 5, 6))),
                                bc_rewind_ts),
         ) |> 
  select(-bc_rewind_offset_chr, -bc_rewind_offset)

wrk_rwd <- cz_schedule_w_rewinds |> select(bc_id, bc_rewind_ts) |> filter(!is.na(bc_rewind_ts)) |> rowwise() |> 
  mutate(bc_rewind_key = to_formatted_ts(bc_rewind_ts)) |> 
  separate(col = bc_rewind_key, into = c("moro_key_rwd", "bc_id_rwd")) |> select(-bc_rewind_ts)

cz_schedule_w_rewinds_b <- cz_schedule_w_rewinds |> left_join(wrk_rwd, by = join_by(bc_id)) |> 
  filter(!(bc_type == "h" & bc_rewind == "tw")) |> select(-bc_rewind)

cz_schedule_rwd_only <- cz_schedule_w_rewinds_b |> select(-regular_timestamp) |> rename(bc_id_original = bc_id) |> 
  filter(!is.na(bc_rewind_ts)) |> 
  select(regular_timestamp = bc_rewind_ts,
         bc_id = bc_id_rwd,
         moro_key = moro_key_rwd,
         bc_len:bc_type,
         bc_rewind_of_id = bc_id_original)

cz_schedule_orig_only <- cz_schedule_w_rewinds_b |> select(-c(bc_rewind_ts:bc_id_rwd)) |> 
  mutate(bc_rewind_of_id = NA_character_)

cz_schedule_final <- bind_rows(cz_schedule_orig_only, cz_schedule_rwd_only) |> arrange(regular_timestamp)
