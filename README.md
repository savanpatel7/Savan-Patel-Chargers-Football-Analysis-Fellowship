# Savan-Patel-Chargers-Football-Analysis-Fellowship
# NFL Drive Expected Points Model

An Expected Points Added (EPA) model for NFL play-by-play data, built on an 
analytically solved Markov chain. Developed as part of a Football Analysis
Fellowship application.

## Overview

This model computes Expected Points (EP) and Expected Points Added (EPA) for 
every NFL play using a Markov chain framework. Unlike regression-based public 
models, EP is derived analytically from empirical transition probabilities 
across 340,587 regular season plays from 2018–2024.

The model is then used to analyze 2nd & Short play selection, providing 
situational recommendations for offensive play calling.

## Model Architecture

### State Space
Every play is defined as a state across six dimensions:

| Dimension | Buckets | Detail |
|-----------|---------|--------|
| Down | 4 | 1st through 4th |
| Distance to go | 12 | 1, 2, 3, 4, 5, 6, 7-8, 9-10, 11-15, 16-20, 21-25, 26+ |
| Field position | 10 | 10-yard buckets from own end zone to opponent end zone |
| Game phase | 4 | H1 normal, H1 2-minute, H2 normal, H2 2-minute |
| Goal to go | 2 | Binary flag |
| Score state | 2 | Leading vs not leading |

Total observed states: **3,260** across **340,587** plays

### Terminal Values
| Outcome | Value |
|---------|-------|
| Touchdown | 7 |
| Field Goal | 3 |
| Safety | -2 |
| Punt | -mean(opponent EP from receiving position) |
| Turnover | -mean(opponent EP from receiving position) |

Punt and turnover terminal values are computed via a two-stage solve:
1. First pass: all terminal values set to their fixed values (TD=7, FG=3, 
   Safety=-2, Punt=0, Turnover=0)
2. First-pass EP values used to compute mean opponent EP after each punt and 
   turnover by state
3. Second pass: field-position adjusted terminal values substituted and system 
   resolved

### Solution
EP is solved analytically:

**EP = (I − Q)⁻¹ R**

Where:
- Q = matrix of transition probabilities between non-terminal states
- R = vector of expected terminal rewards
- Solved using sparse matrix algebra via the `Matrix` package

### EPA Computation
EPA is computed play-by-play as:

| Play type | EPA formula |
|-----------|-------------|
| Normal play | EP(next state) − EP(current state) |
| Offensive TD | 7 − EP(current state) |
| Defensive TD (pick-six, fumble return) | −7 − EP(current state) |
| Safety | −2 − EP(current state) |
| Turnover (no score) | −EP(opponent next state) − EP(current state) |

## Key Innovations Over Public Models

1. **Field-position adjusted turnover values** — interceptions, fumbles, and 
   failed 4th down conversions are penalized based on where the opponent 
   receives the ball, not assigned a fixed value of zero
2. **Field-position adjusted punt values** — the terminal value of a punt is 
   the negative of the opponent's expected points from their starting position
3. **Defensive TD handling** — pick-sixes and fumble return touchdowns are 
   correctly valued at −7 − EP(current), not treated as zero-value turnovers
4. **Score state in the Markov chain** — leading and not-leading teams have 
   structurally different transition probabilities estimated separately
5. **Game phase** — four phases capture end-of-half behavioral shifts
6. **Goal-to-go flag** — red zone short-yardage situations modeled separately
7. **Finer distance buckets** — 12 buckets vs broader public model groupings

## Data

- **Source:** nflfastR play-by-play via `nflreadr`
- **Seasons:** 2018–2024 NFL Regular Season
- **Plays:** 340,587
- **Games:** 1,942

## Requirements

```r
install.packages(c(
  "tidyverse",
  "nflreadr", 
  "Matrix",
  "ggplot2",
  "ggtext",
  "patchwork",
  "gt",
  "gtExtras",
  "scales"
))
```

## Files

| File | Description |
|------|-------------|
| `00_data.R` | Load nflfastR play-by-play data and compute drive scores |
| `01_helpers.R` | State space bucketing functions and sparse Markov solver |
| `02_markov_first_pass.R` | Build transition matrix and solve first-pass EP (punt/turnover = 0) |
| `03_terminal_adjustments.R` | Compute field-position adjusted terminal values for punts and turnovers |
| `04_markov_second_pass.R` | Second-pass Markov solve with adjusted terminal values |
| `05_epa_computation.R` | Join EP to play-by-play data and compute EPA |
| `06_second_short_analysis.R` | 2nd & Short EPA analysis by situation, field zone, distance, and game phase |
| `07_visualizations.R` | All plots and tables for Director and Coach decks |
| `run_all.R` | Runs the full pipeline in order |

## Usage

Clone the repository and run the full pipeline from a fresh R session:

    source("run_all.R")

Or run individual scripts in order:

    source("00_data.R")
    source("01_helpers.R")
    source("02_markov_first_pass.R")
    source("03_terminal_adjustments.R")
    source("04_markov_second_pass.R")
    source("05_epa_computation.R")
    source("06_second_short_analysis.R")
    source("07_visualizations.R")

Note: Set `R_MAX_VSIZE = "32Gb"` before running on machines with limited memory.
Output plots are saved to the `plots/` directory.

## Key Findings — 2nd & Short Play Selection

Based on analysis of 12,075 2nd & Short plays (2018–2024):

- Passing outperforms running in most 2nd & Short situations when accounting 
  for field position, game phase, and score state
- **Run** in the red zone (inside the 20) and when the game is tied
- **Pass** when trailing by one score, leading by any amount, or in normal 
  1st half game flow
- **Run** in both 2-minute drills: clock management value is real
- Teams currently pass only 27–46% of the time on 2nd & Short across all 
  situations, suggesting systematic under-utilization of the passing game in 
  moderate score situations

## Limitations

- **Data constraints limit state space expansion** — the model currently has 
  3,260 states with a median of 19 plays per state across 7 seasons of data. 
  Every additional state dimension multiplies the state count and reduces 
  observations per state. Adding finer score buckets, opponent quality, 
  personnel groupings, or weather would push many states below the threshold 
  for reliable transition probability estimation. Expanding the state space 
  meaningfully would require 10+ seasons of data.
- **Binary score state** — leading vs not leading loses nuance between small 
  and large leads. Three score buckets were tested but dropped median plays 
  per state to ~11, producing unreliable estimates in sparse states.
- **Drive EP vs net EP** — this model measures offensive possession value, not 
  net next-score by either team. This is the right framework for play-calling 
  decisions but produces different EP values at deep field positions than 
  net-EP models.
- **Scoring play kickoff value not adjusted** — TD and FG terminal values do 
  not subtract the opponent's expected kickoff EP. A fully consistent model 
  would set TD ≈ 6.25 and FG ≈ 2.25 after accounting for a typical touchback.
- **Memoryless Markov assumption** — the model treats each state as independent 
  of prior plays. Sequential play-calling tendencies and in-drive momentum are 
  not captured.

