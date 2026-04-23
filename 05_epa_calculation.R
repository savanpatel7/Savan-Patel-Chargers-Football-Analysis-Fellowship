# Join EP to play-by-play data and compute EPA
# EPA handles all scoring event types explicitly

source("04_markov_second_pass.R")

pbp_final <- pbp_drive %>%
  filter(
    season_type == "REG",
    play_type %in% c("pass", "run"),
    !is.na(down), !is.na(ydstogo),
    !is.na(yardline_100),
    !is.na(score_differential),
    !is.na(spread_line),
    !is.na(half_seconds_remaining),
    qb_kneel == 0, qb_spike == 0,
    penalty  == 0,
    !is.na(drive_score)
  ) %>%
  mutate(
    state = build_state(
      down, ydstogo, yardline_100,
      qtr, half_seconds_remaining,
      replace_na(goal_to_go, 0),
      score_differential
    ),
    score_differential = as.numeric(score_differential),
    spread_adj_diff    = score_differential - spread_line,
    field_zone = case_when(
      yardline_100 <= 20 ~ "Red Zone",
      yardline_100 <= 40 ~ "Scoring Range (21-40)",
      yardline_100 <= 60 ~ "Midfield (41-60)",
      TRUE               ~ "Own Territory (60+)"
    ),
    game_situation = case_when(
      score_differential >=  7 ~ "Leading 7+",
      score_differential >=  1 ~ "Leading 1-6",
      score_differential ==  0 ~ "Tied",
      score_differential >= -6 ~ "Trailing 1-6",
      TRUE                     ~ "Trailing 7+"
    )
  ) %>%
  left_join(EP_markov_df, by = "state") %>%
  arrange(game_id, play_id) %>%
  group_by(game_id) %>%
  mutate(
    ep_next      = lead(ep_markov),
    next_posteam = lead(posteam),
    next_drive   = lead(fixed_drive),
    epa = case_when(
      # Defensive TD — must check before offensive TD
      touchdown == 1 &
        (interception == 1 |
           fumble_lost == 1)                      ~ -7 - ep_markov,
      # Offensive TD
      touchdown == 1 &
        (rush_touchdown == 1 |
           pass_touchdown == 1)                   ~  7 - ep_markov,
      # Safety
      safety == 1                                 ~ -2 - ep_markov,
      # Turnover without score
      interception == 1                           ~ -ep_next - ep_markov,
      fumble_lost == 1                            ~ -ep_next - ep_markov,
      # Same drive same possession
      !is.na(ep_next) &
        next_posteam == posteam &
        next_drive == fixed_drive                 ~  ep_next - ep_markov,
      TRUE                                        ~ NA_real_
    )
  ) %>%
  ungroup()

rm(pbp_drive)
gc()

cat("EP range:",
    round(min(pbp_final$ep_markov, na.rm = TRUE), 3),
    "to",
    round(max(pbp_final$ep_markov, na.rm = TRUE), 3), "\n")

cat("Mean EPA by play type:\n")
pbp_final %>%
  filter(!is.na(epa)) %>%
  group_by(play_type) %>%
  summarise(n        = n(),
            mean_epa = round(mean(epa), 4),
            .groups  = "drop") %>%
  print()
