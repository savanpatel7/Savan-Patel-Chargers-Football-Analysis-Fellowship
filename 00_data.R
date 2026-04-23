# Load and cache nflfastR play-by-play data
# Seasons 2018-2024, regular season only

library(tidyverse)
library(nflreadr)

seasons <- 2018:2024

pbp_raw <- load_pbp(seasons)

keep_cols <- c(
  "game_id", "play_id", "season", "week",
  "season_type", "qtr", "down", "ydstogo",
  "yardline_100", "posteam", "defteam",
  "fixed_drive", "play_type",
  "rush_touchdown", "pass_touchdown", "touchdown",
  "field_goal_result", "safety",
  "interception", "fumble_lost",
  "return_touchdown",
  "yards_gained", "first_down", "goal_to_go",
  "half_seconds_remaining",
  "pass", "rush", "success",
  "score_differential", "spread_line",
  "qb_kneel", "qb_spike", "penalty",
  "ep"
)

keep_cols <- keep_cols[keep_cols %in% names(pbp_raw)]
pbp <- pbp_raw %>% select(all_of(keep_cols))
rm(pbp_raw)

# Drive score — what did this possession actually produce
pbp_drive <- pbp %>%
  filter(season_type == "REG") %>%
  arrange(game_id, fixed_drive) %>%
  group_by(game_id, fixed_drive) %>%
  mutate(
    drive_score = case_when(
      any(touchdown == 1 &
            (rush_touchdown == 1 | pass_touchdown == 1),
          na.rm = TRUE)              ~ 7,
      any(field_goal_result == "made",
          na.rm = TRUE)              ~ 3,
      any(safety == 1, na.rm = TRUE) ~ -2,
      TRUE                           ~ 0
    )
  ) %>%
  ungroup()

rm(pbp)
gc()

cat("Plays:", nrow(pbp_drive), "\n")
cat("Games:", length(unique(pbp_drive$game_id)), "\n")
