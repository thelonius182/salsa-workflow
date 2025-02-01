pacman::p_load(lubridate, stringr, tidyr, purrr, dplyr)

# Function to get the nth occurrence of a weekday in a given month
get_nth_weekday <- function(year, month, weekday, n, hour = 0) {
  first_day <- ymd(paste(year, month, "01", sep = "-"), tz = "Europe/Amsterdam")
  weekday_number <- match(toupper(weekday), c("SUN", "MON", "TUE", "WED", "THU", "FRI", "SAT"))
  
  # Find the first occurrence of the specified weekday
  first_weekday <- first_day + days((weekday_number - wday(first_day)) %% 7)
  
  # Get the nth occurrence
  target_date <- first_weekday + weeks(n - 1)
  
  # Ensure proper POSIXct format
  return(ymd_hm(paste(target_date, sprintf("%02d:00", hour)), tz = "Europe/Amsterdam"))
}

# Function to parse label and extract components
parse_label <- function(label) {
  match <- str_match(label, "C(\\d)([A-Z]{3})(\\d{2})")
  if (any(is.na(match))) stop("Invalid label format")
  
  list(
    cycle = as.integer(match[2]),   # nth occurrence of weekday
    weekday = match[3],             # 3-letter weekday code
    hour = as.integer(match[4])     # Hour in 24-hour format
  )
}

# Function to generate timestamps from a label
generate_timestamps_from_label <- function(label, months_ahead = 12) {
  parsed <- parse_label(label)
  
  current_date <- today(tzone = "Europe/Amsterdam")
  current_year <- year(current_date)
  current_month <- month(current_date)
  
  timestamps <- tibble(label,
                       month_index = 0:(months_ahead - 1),
                       date = map(month_index, 
                                  ~ get_nth_weekday(current_year + ((current_month + .x - 1) %/% 12),
                                                    (current_month + .x - 1) %% 12 + 1,
                                                    parsed$weekday,
                                                    parsed$cycle,
                                                    parsed$hour)
                       )
                ) |> unnest(date) |>   # Convert list column to a flat tibble
    mutate(date = stamp("do 30 jan 2025, 17:00", quiet = T)(date))
    

  # Convert list to POSIXct vector for proper printing
  # timestamps <- do.call(c, timestamps)
  return(timestamps)
}

# Example Usage
label <- "C5MON10"
timestamps <- generate_timestamps_from_label(label, 3)
print(timestamps)
