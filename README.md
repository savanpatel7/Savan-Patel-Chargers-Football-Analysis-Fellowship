# Savan-Patel-Chargers-Football-Analysis-Fellowship
# NFL Drive Expected Points Model

A novel Expected Points (EP) model for NFL play-by-play data, built on an 
analytically solved Markov chain. Developed as part of a Football Analytics 
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
