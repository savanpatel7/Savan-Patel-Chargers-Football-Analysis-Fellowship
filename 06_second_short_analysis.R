# 2nd & Short analysis - EPA by situation, field zone, distance, game phase
# Filters to 2nd down plays with 3 or fewer yards to go



second_short <- pbp_final %>%
  filter(
    down == 2,
    ydstogo <= 3,
    play_type %in% c("pass", "run"),
    !is.na(epa)
  )

cat("2nd & Short plays:", nrow(second_short),
    "| Pass rate:", round(mean(second_short$pass), 3), "\n\n")

# Overall
second_short %>%
  group_by(play_type) %>%
  summarise(
    n               = n(),
    epa_per_play    = round(mean(epa), 4),
    success_rate    = round(mean(success,     na.rm = TRUE), 3),
    first_down_rate = round(mean(first_down,  na.rm = TRUE), 3),
    yards_gained    = round(mean(yards_gained, na.rm = TRUE), 2),
    .groups = "drop"
  ) %>%
  arrange(desc(epa_per_play)) %>%
  print()

# By distance
second_short %>%
  group_by(ydstogo, play_type) %>%
  summarise(
    n               = n(),
    epa_per_play    = round(mean(epa), 4),
    success_rate    = round(mean(success,    na.rm = TRUE), 3),
    first_down_rate = round(mean(first_down, na.rm = TRUE), 3),
    .groups = "drop"
  ) %>%
  arrange(ydstogo, desc(epa_per_play)) %>%
  print()

# By field zone
second_short %>%
  group_by(field_zone, play_type) %>%
  summarise(
    n               = n(),
    epa_per_play    = round(mean(epa), 4),
    success_rate    = round(mean(success,     na.rm = TRUE), 3),
    first_down_rate = round(mean(first_down,  na.rm = TRUE), 3),
    yards_gained    = round(mean(yards_gained, na.rm = TRUE), 2),
    .groups = "drop"
  ) %>%
  arrange(field_zone, desc(epa_per_play)) %>%
  print()

# By game situation
second_short %>%
  group_by(game_situation, play_type) %>%
  summarise(
    n               = n(),
    epa_per_play    = round(mean(epa), 4),
    success_rate    = round(mean(success,    na.rm = TRUE), 3),
    first_down_rate = round(mean(first_down, na.rm = TRUE), 3),
    .groups = "drop"
  ) %>%
  arrange(game_situation, desc(epa_per_play)) %>%
  print()

# By distance x field zone
second_short %>%
  group_by(ydstogo, field_zone, play_type) %>%
  summarise(
    n            = n(),
    epa_per_play = round(mean(epa), 4),
    .groups      = "drop"
  ) %>%
  arrange(ydstogo, field_zone, desc(epa_per_play)) %>%
  print()

# By game phase
second_short %>%
  mutate(phase = game_phase(qtr,
                            half_seconds_remaining)) %>%
  group_by(phase, play_type) %>%
  summarise(
    n            = n(),
    epa_per_play = round(mean(epa), 4),
    .groups      = "drop"
  ) %>%
  arrange(phase, desc(epa_per_play)) %>%
  print()

# By score state
second_short %>%
  mutate(score_state = score_bucket(
    score_differential)) %>%
  group_by(score_state, play_type) %>%
  summarise(
    n            = n(),
    epa_per_play = round(mean(epa), 4),
    success_rate = round(mean(success,    na.rm = TRUE), 3),
    fd_rate      = round(mean(first_down, na.rm = TRUE), 3),
    .groups      = "drop"
  ) %>%
  arrange(score_state, desc(epa_per_play)) %>%
  print()
