# Build transition matrix and solve first-pass Markov EP
# Punt and turnover terminal values set to 0 here —
# adjusted values computed in 03_terminal_adjustments.R

source("01_helpers.R")

terminal_ep <- c(TD = 7, FG = 3, SAFETY = -2,
                 TURNOVER = 0, PUNT = 0)

transition_counts <- pbp_drive %>%
  arrange(game_id, play_id) %>%
  mutate(
    state = case_when(
      !is.na(down) & !is.na(ydstogo) &
        !is.na(yardline_100) &
        !is.na(half_seconds_remaining) &
        !is.na(score_differential) ~
        build_state(
          down, ydstogo, yardline_100,
          qtr, half_seconds_remaining,
          replace_na(goal_to_go, 0),
          score_differential
        ),
      TRUE ~ NA_character_
    ),
    terminal_state = case_when(
      # Defensive TD must come before offensive TD check
      touchdown == 1 &
        (interception == 1 |
           fumble_lost == 1)                        ~ "TURNOVER",
      touchdown == 1 &
        (rush_touchdown == 1 |
           pass_touchdown == 1)                     ~ "TD",
      field_goal_result == "made"                   ~ "FG",
      safety == 1                                   ~ "SAFETY",
      interception == 1                             ~ "TURNOVER",
      fumble_lost == 1                              ~ "TURNOVER",
      down == 4 & !is.na(yards_gained) &
        yards_gained < ydstogo &
        play_type %in% c("pass", "run")             ~ "TURNOVER",
      play_type == "punt"                           ~ "PUNT",
      TRUE                                          ~ NA_character_
    )
  ) %>%
  filter(
    !is.na(state),
    play_type %in% c("pass", "run", "punt", "field_goal")
  ) %>%
  arrange(game_id, play_id) %>%
  group_by(game_id) %>%
  mutate(next_state = case_when(
    !is.na(terminal_state) ~ terminal_state,
    TRUE                   ~ lead(state)
  )) %>%
  ungroup() %>%
  filter(!is.na(next_state)) %>%
  count(state, next_state) %>%
  group_by(state) %>%
  mutate(prob = n / sum(n)) %>%
  ungroup()

all_states <- sort(setdiff(
  unique(transition_counts$state),
  names(terminal_ep)
))

cat("States:", length(all_states), "\n")
cat("Median plays per state:",
    round(median(
      transition_counts %>%
        group_by(state) %>%
        summarise(n = sum(n)) %>%
        pull(n)
    )), "\n")

EP_v1    <- build_ep_sparse(transition_counts,
                             all_states, terminal_ep)
EP_v1_df <- tibble(state = names(EP_v1), ep_v1 = EP_v1)
rm(EP_v1)
gc()
