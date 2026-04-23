# Helper functions — state space definitions and Markov solver

library(Matrix)
library(tidyverse)

bucket_dist <- function(y) case_when(
  y == 1  ~ "01",    y == 2  ~ "02",    y == 3  ~ "03",
  y == 4  ~ "04",    y == 5  ~ "05",    y == 6  ~ "06",
  y <= 8  ~ "07-08", y <= 10 ~ "09-10", y <= 15 ~ "11-15",
  y <= 20 ~ "16-20", y <= 25 ~ "21-25", TRUE    ~ "26+"
)

bucket_field <- function(y) case_when(
  y <= 10 ~ "01-10", y <= 20 ~ "11-20", y <= 30 ~ "21-30",
  y <= 40 ~ "31-40", y <= 50 ~ "41-50", y <= 60 ~ "51-60",
  y <= 70 ~ "61-70", y <= 80 ~ "71-80", y <= 90 ~ "81-90",
  TRUE    ~ "91-100"
)

# Four game phases — end of half behavior is structurally different
game_phase <- function(qtr, half_seconds) case_when(
  qtr <= 2 & half_seconds >  120 ~ "H1_normal",
  qtr <= 2 & half_seconds <= 120 ~ "H1_2min",
  qtr >  2 & half_seconds >  120 ~ "H2_normal",
  TRUE                            ~ "H2_2min"
)

score_bucket <- function(sd) case_when(
  sd > 0 ~ "leading",
  TRUE   ~ "not_leading"
)

build_state <- function(down, ydstogo, yardline_100,
                        qtr, half_seconds,
                        goal_to_go, score_diff) {
  paste(
    down,
    bucket_dist(ydstogo),
    bucket_field(yardline_100),
    game_phase(qtr, half_seconds),
    if_else(goal_to_go == 1, "GTG", "no"),
    score_bucket(score_diff),
    sep = "_"
  )
}

# Sparse Markov solver — accepts optional field-position adjusted
# terminal values for punts and turnovers
build_ep_sparse <- function(transition_counts, all_states,
                            terminal_ep,
                            punt_tv_lookup     = NULL,
                            turnover_tv_lookup = NULL) {
  n               <- length(all_states)
  idx             <- setNames(seq_len(n), all_states)
  global_punt     <- if (!is.null(punt_tv_lookup))
    mean(punt_tv_lookup, na.rm = TRUE) else 0
  global_turnover <- if (!is.null(turnover_tv_lookup))
    mean(turnover_tv_lookup, na.rm = TRUE) else 0

  tc    <- transition_counts %>% filter(state %in% all_states)
  tc_tt <- tc %>%
    filter(next_state %in% all_states) %>%
    mutate(i = idx[state], j = idx[next_state])

  Q <- sparseMatrix(i = tc_tt$i, j = tc_tt$j,
                    x = tc_tt$prob, dims = c(n, n))
  rm(tc_tt); gc()

  R       <- numeric(n)
  tc_term <- tc %>%
    filter(!next_state %in% all_states) %>%
    mutate(si = idx[state])

  td_rows <- tc_term %>% filter(next_state == "TD")
  for (k in seq_len(nrow(td_rows)))
    R[td_rows$si[k]] <- R[td_rows$si[k]] +
      td_rows$prob[k] * 7

  fg_rows <- tc_term %>% filter(next_state == "FG")
  for (k in seq_len(nrow(fg_rows)))
    R[fg_rows$si[k]] <- R[fg_rows$si[k]] +
      fg_rows$prob[k] * 3

  sf_rows <- tc_term %>% filter(next_state == "SAFETY")
  for (k in seq_len(nrow(sf_rows)))
    R[sf_rows$si[k]] <- R[sf_rows$si[k]] +
      sf_rows$prob[k] * (-2)

  if (!is.null(punt_tv_lookup)) {
    pt_rows <- tc_term %>%
      filter(next_state == "PUNT") %>%
      mutate(pv = case_when(
        state %in% names(punt_tv_lookup) ~
          punt_tv_lookup[state],
        TRUE ~ global_punt
      ))
    for (k in seq_len(nrow(pt_rows)))
      R[pt_rows$si[k]] <- R[pt_rows$si[k]] +
        pt_rows$prob[k] * pt_rows$pv[k]
  }

  if (!is.null(turnover_tv_lookup)) {
    to_rows <- tc_term %>%
      filter(next_state == "TURNOVER") %>%
      mutate(pv = case_when(
        state %in% names(turnover_tv_lookup) ~
          turnover_tv_lookup[state],
        TRUE ~ global_turnover
      ))
    for (k in seq_len(nrow(to_rows)))
      R[to_rows$si[k]] <- R[to_rows$si[k]] +
        to_rows$prob[k] * to_rows$pv[k]
  }

  rm(tc, tc_term); gc()

  ep <- as.numeric(solve(Diagonal(n) - Q, R))
  setNames(ep, all_states)
}
