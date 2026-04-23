# Compute field-position adjusted terminal values for punts and turnovers
# Uses first-pass EP to estimate opponent EP after each possession change
# Both adjustments follow the same pattern — find the opponent's first play
# after the event and look up its EP from the first-pass solve


# Punt adjustment
punt_tv <- pbp_drive %>%
  arrange(game_id, play_id) %>%
  group_by(game_id) %>%
  mutate(
    prev_type     = lag(play_type),
    prev_down     = lag(down),
    prev_ydstogo  = lag(ydstogo),
    prev_yl100    = lag(yardline_100),
    prev_qtr      = lag(qtr),
    prev_half_sec = lag(half_seconds_remaining),
    prev_gtg      = lag(replace_na(goal_to_go, 0)),
    prev_sd       = lag(score_differential),
    punt_state = case_when(
      prev_type == "punt" & !is.na(prev_down) &
        !is.na(prev_sd) ~
        build_state(prev_down, prev_ydstogo,
                    prev_yl100, prev_qtr,
                    prev_half_sec, prev_gtg,
                    prev_sd),
      TRUE ~ NA_character_
    ),
    # Flip score differential — opponent is now the offense
    opp_state = case_when(
      prev_type == "punt" &
        play_type %in% c("pass", "run") &
        !is.na(down) & !is.na(ydstogo) &
        !is.na(yardline_100) &
        !is.na(half_seconds_remaining) &
        !is.na(score_differential) ~
        build_state(down, ydstogo, yardline_100,
                    qtr, half_seconds_remaining,
                    replace_na(goal_to_go, 0),
                    -score_differential),
      TRUE ~ NA_character_
    )
  ) %>%
  ungroup() %>%
  filter(!is.na(punt_state), !is.na(opp_state)) %>%
  left_join(EP_v1_df %>% rename(opp_ep = ep_v1),
            by = c("opp_state" = "state")) %>%
  filter(!is.na(opp_ep)) %>%
  group_by(punt_state) %>%
  summarise(punt_tv = -mean(opp_ep), n = n(),
            .groups = "drop")

punt_tv_lookup <- setNames(punt_tv$punt_tv,
                            punt_tv$punt_state)

cat("Global punt TV:",
    round(mean(punt_tv_lookup), 3), "\n")
cat("Punt states covered:",
    length(punt_tv_lookup), "\n")

# Turnover adjustment — covers interceptions, fumbles,
# failed 4th downs, and defensive TDs
turnover_tv <- pbp_drive %>%
  arrange(game_id, play_id) %>%
  group_by(game_id) %>%
  mutate(
    prev_type     = lag(play_type),
    prev_down     = lag(down),
    prev_ydstogo  = lag(ydstogo),
    prev_yl100    = lag(yardline_100),
    prev_qtr      = lag(qtr),
    prev_half_sec = lag(half_seconds_remaining),
    prev_gtg      = lag(replace_na(goal_to_go, 0)),
    prev_sd       = lag(score_differential),
    prev_int      = lag(interception),
    prev_fum      = lag(fumble_lost),
    prev_failed4  = lag(as.integer(
      down == 4 & !is.na(yards_gained) &
        yards_gained < ydstogo &
        play_type %in% c("pass", "run")
    )),
    prev_def_td   = lag(as.integer(
      touchdown == 1 &
        (interception == 1 | fumble_lost == 1)
    )),
    is_turnover_play = case_when(
      replace_na(prev_int,     0) == 1 ~ TRUE,
      replace_na(prev_fum,     0) == 1 ~ TRUE,
      replace_na(prev_failed4, 0) == 1 ~ TRUE,
      replace_na(prev_def_td,  0) == 1 ~ TRUE,
      TRUE                             ~ FALSE
    ),
    turnover_state = case_when(
      is_turnover_play & !is.na(prev_down) &
        !is.na(prev_sd) ~
        build_state(prev_down, prev_ydstogo,
                    prev_yl100, prev_qtr,
                    prev_half_sec, prev_gtg,
                    prev_sd),
      TRUE ~ NA_character_
    ),
    opp_state_to = case_when(
      is_turnover_play &
        play_type %in% c("pass", "run") &
        !is.na(down) & !is.na(ydstogo) &
        !is.na(yardline_100) &
        !is.na(half_seconds_remaining) &
        !is.na(score_differential) ~
        build_state(down, ydstogo, yardline_100,
                    qtr, half_seconds_remaining,
                    replace_na(goal_to_go, 0),
                    -score_differential),
      TRUE ~ NA_character_
    )
  ) %>%
  ungroup() %>%
  filter(!is.na(turnover_state),
         !is.na(opp_state_to)) %>%
  left_join(EP_v1_df %>% rename(opp_ep = ep_v1),
            by = c("opp_state_to" = "state")) %>%
  filter(!is.na(opp_ep)) %>%
  group_by(turnover_state) %>%
  summarise(
    turnover_tv = -mean(opp_ep),
    n           = n(),
    .groups     = "drop"
  )

turnover_tv_lookup <- setNames(
  turnover_tv$turnover_tv,
  turnover_tv$turnover_state
)

cat("Global turnover TV:",
    round(mean(turnover_tv_lookup), 3), "\n")
cat("Turnover states covered:",
    length(turnover_tv_lookup), "\n")

rm(punt_tv, turnover_tv, EP_v1_df)
gc()
