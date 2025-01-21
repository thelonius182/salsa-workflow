moro_raw <- read_tsv(file = "/mnt/muw/moro - modelrooster-20250102.tsv", na = NA_character_, lazy = F,
                     col_types = paste(rep("c", 37), collapse = "")) |> 
  mutate(week_1 = paste(`week 1`, t1, r1, sep = "@"),
         week_2 = paste(`week 2`, t2, r2, sep = "@"),
         week_3 = paste(`week 3`, t3, r3, sep = "@"),
         week_4 = paste(`week 4`, t4, r4, sep = "@"),
         week_5 = paste(`week 5`, t5, r5, sep = "@"),
         week_all = paste(`elke week`, te, re, sep = "@"),
         week_ab = paste(`twee-wekelijks`, A, B, rt, sep = "@")) |> 
  select(dag, bc_start = start, matches("week_|hh"))

moro_pvt <- moro_raw |> pivot_longer(cols = matches("week_"), names_to = "m_label", values_to = "m_value") |> 
  filter(str_detect(m_value,"^@@@?$", negate = T))

english_days = c("MON", "TUE", "WED", "THU", "THU", "FRI", "SAT", "SUN")
dutch_days = c("ma1", "di1", "wo1", "do1", "do2", "vr1", "za1", "zo1")
dd2ed <- tibble(dag = dutch_days, bc_day_of_week = english_days)

moro_std_1 <- moro_pvt |> mutate(cycle = str_replace(m_label, "week_", "C"),
                                 bc_desk = str_extract(m_value, ".*@(.)", group = 1),
                                 m_value = str_extract(m_value, "(.*)@.", group = 1),
                                 bc_type_b = str_extract(m_value, ".*@(.*)$", group = 1),
                                 bc_title_a = str_extract(m_value, "^(.*)@.*", group = 1),
                                 bc_type_a = str_extract(bc_title_a, ".*@(.*)$", group = 1),
                                 bc_title_b = str_extract(bc_title_a, "^(.*)@.*", group = 1),
                                 bc_title_b = coalesce(bc_title_b, bc_title_a),
                                 bc_title_c = str_replace_all(bc_title_b, " S[0-9]", ""),
                                 bc_title_d = str_replace_all(bc_title_c, " [A-G]_..", ""),
                                 bc_title_d = coalesce(bc_title_d, bc_title_c),
                                 bc_title_d = str_replace_all(bc_title_d, " (L|U)M", ""),
                                 bc_len = str_sub(bc_start, 4, 6)) |> 
  left_join(dd2ed, by = join_by(dag))

moro_std_2 <- moro_std_1 |> select(bc_cycle_raw = cycle, bc_day_of_week, bc_start, bc_len, bc_rewind = `hhOffset-dag.uur`,
                                   bc_type_a, bc_type_b, bc_title = bc_title_d, bc_desk) |> 
  mutate(bc_key_a = if_else(bc_cycle_raw == "Cab", "CA", NA_character_),
         bc_key_b = if_else(bc_cycle_raw == "Cab", "CB", NA_character_),
         bc_key_1 = if_else(bc_cycle_raw %in% c("Call", "C1"), "C1", NA_character_),
         bc_key_2 = if_else(bc_cycle_raw %in% c("Call", "C2"), "C2", NA_character_),
         bc_key_3 = if_else(bc_cycle_raw %in% c("Call", "C3"), "C3", NA_character_),
         bc_key_4 = if_else(bc_cycle_raw %in% c("Call", "C4"), "C4", NA_character_),
         bc_key_5 = if_else(bc_cycle_raw %in% c("Call", "C5"), "C5", NA_character_),
         bc_start = str_sub(bc_start, 1, 2)) |> 
  pivot_longer(cols = matches("bc_key_"), names_to = "m_label", values_to = "m_value") |> filter(!is.na(m_value)) |> 
  select(bc_cycle = m_value, bc_day_of_week:bc_desk) |> 
  pivot_longer(cols = matches("bc_type_"), names_to = "m_label", values_to = "m_value") |> filter(!is.na(m_value)) |> 
  mutate(wrk_sel = case_when(str_detect(bc_cycle, "C\\d") ~ T,
                             str_sub(bc_cycle, -1) == str_to_upper(str_sub(m_label, -1)) ~ T,
                             T ~ F),
         moro_key = paste0(bc_cycle, bc_day_of_week, "H", bc_start)) |> filter(wrk_sel) |> 
  select(moro_key, bc_len:bc_desk, bc_type = m_value)
