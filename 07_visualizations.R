dir.create("plots", showWarnings = FALSE)

# All visualizations — Director and Coach decks
# Requires pbp_final, second_short, EP_markov_df from upstream scripts


library(ggplot2)
library(ggtext)
library(patchwork)
library(gt)
library(gtExtras)
library(scales)

# ── Palette ───────────────────────────────────────────────────────────────────
ch_navy  <- "#002A5E"
ch_gold  <- "#FFC20E"
ch_blue  <- "#0080C6"
bg       <- "white"
col_pass <- ch_blue
col_run  <- ch_gold

theme_chargers <- function(base_size = 12, ...) {
  theme_minimal(base_size = base_size, ...) +
    theme(
      plot.background  = element_rect(fill = bg, color = NA),
      panel.background = element_rect(fill = bg, color = NA),
      panel.grid.major = element_line(color = "#E0E0E0",
                                      linewidth = 0.4),
      panel.grid.minor = element_blank(),
      plot.title       = element_markdown(
        size = 15, face = "bold", color = ch_navy,
        hjust = 0.5, margin = margin(b = 4)
      ),
      plot.subtitle    = element_text(
        size = 10, color = "#555555",
        margin = margin(b = 12)
      ),
      plot.caption     = element_text(
        size = 8, color = "#888888",
        hjust = 1, margin = margin(t = 8)
      ),
      legend.position  = "bottom",
      legend.title     = element_blank(),
      legend.text      = element_text(size = 11),
      legend.key       = element_rect(fill = NA, color = NA),
      axis.text        = element_text(size = 10,
                                      color = "#444444"),
      axis.title       = element_text(size = 11,
                                      color = "#333333"),
      plot.margin      = margin(16, 16, 12, 16),
      strip.text       = element_text(size = 12,
                                      color = ch_navy,
                                      face = "bold")
    )
}

title_theme <- function() {
  theme(
    plot.title          = element_markdown(
      size = 15, face = "bold", color = ch_navy,
      hjust = 0.5, margin = margin(b = 12)
    ),
    plot.title.position  = "plot",
    plot.background      = element_rect(fill = "white",
                                        color = NA),
    panel.background     = element_rect(fill = "white",
                                        color = NA)
  )
}

# =============================================================================
# DIRECTOR PLOTS
# =============================================================================

# ── D1: EP curve vs nflfastR ──────────────────────────────────────────────────
ep_curve <- pbp_final %>%
  filter(down == 1, ydstogo == 10,
         half_seconds_remaining > 120,
         score_differential <= 0) %>%
  mutate(fz = round(yardline_100 / 10) * 10) %>%
  group_by(fz) %>%
  summarise(
    our_ep      = mean(ep_markov, na.rm = TRUE),
    nflfastr_ep = mean(ep,        na.rm = TRUE),
    .groups     = "drop"
  ) %>%
  pivot_longer(c(our_ep, nflfastr_ep),
               names_to = "model", values_to = "ep") %>%
  mutate(model = factor(
    if_else(model == "our_ep",
            "Our Drive EP Model", "nflfastR"),
    levels = c("Our Drive EP Model", "nflfastR")
  ))

d1 <- ggplot(ep_curve,
             aes(x = fz, y = ep,
                 color = model, linetype = model)) +
  annotate("rect", xmin = 0, xmax = 20,
           ymin = -Inf, ymax = Inf,
           fill = ch_blue, alpha = 0.06) +
  annotate("rect", xmin = 80, xmax = 100,
           ymin = -Inf, ymax = Inf,
           fill = ch_navy, alpha = 0.06) +
  annotate("text", x = 10, y = 5.3,
           label = "Red Zone", size = 3.5,
           color = ch_blue, fontface = "bold") +
  annotate("text", x = 90, y = 5.3,
           label = "Own Territory", size = 3.5,
           color = ch_navy, fontface = "bold") +
  geom_hline(yintercept = 0, color = "#AAAAAA",
             linewidth = 0.6, linetype = "dashed") +
  geom_line(linewidth = 1.3) +
  geom_point(size = 3) +
  scale_color_manual(values = c(
    "Our Drive EP Model" = ch_navy,
    "nflfastR"           = ch_gold
  )) +
  scale_linetype_manual(values = c(
    "Our Drive EP Model" = "solid",
    "nflfastR"           = "longdash"
  )) +
  scale_x_reverse(
    breaks = seq(10, 100, 10),
    labels = function(x) paste0(x, " yds")
  ) +
  scale_y_continuous(breaks = seq(-2, 7, 1)) +
  labs(
    title   = "Expected Points by Field Position",
    caption = "Drive EP Markov Chain Model · 2018–2024 NFL Regular Season",
    x       = "Yards from Own End Zone",
    y       = "Expected Points (EP)"
  ) +
  theme_chargers() +
  title_theme()

ggsave("plots/d1_ep_curve.png", d1,
       width = 10, height = 6, dpi = 150,
       bg = "white")

# ── D2: EP by Down ────────────────────────────────────────────────────────────
ep_down <- pbp_final %>%
  filter(half_seconds_remaining > 120,
         !is.na(ep_markov)) %>%
  mutate(fz = round(yardline_100 / 5) * 5) %>%
  group_by(fz, down) %>%
  summarise(ep = mean(ep_markov, na.rm = TRUE),
            n  = n(), .groups = "drop") %>%
  filter(n >= 20)

d2 <- ggplot(ep_down,
             aes(x = fz, y = ep,
                 color = factor(down),
                 group = factor(down))) +
  geom_hline(yintercept = 0, color = "#AAAAAA",
             linewidth = 0.6, linetype = "dashed") +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2) +
  scale_color_manual(
    values = c(
      "1" = "#002A5E", "2" = "#0080C6",
      "3" = "#6AAFE6", "4" = "#FFC20E"
    ),
    labels = c("1st Down", "2nd Down",
               "3rd Down", "4th Down")
  ) +
  scale_x_reverse(
    breaks = seq(5, 100, 10),
    labels = function(x) paste0(x, " yds")
  ) +
  labs(
    title   = "Expected Points by Down and Field Position",
    caption = "Drive EP Markov Chain Model · 2018–2024 NFL Regular Season",
    x       = "Yards from Own End Zone",
    y       = "Expected Points (EP)"
  ) +
  theme_chargers() +
  title_theme()

ggsave("plots/d2_ep_by_down.png", d2,
       width = 10, height = 6, dpi = 150,
       bg = "white")

# ── D3: EP by Game Phase ──────────────────────────────────────────────────────
ep_phase <- pbp_final %>%
  filter(down == 1, ydstogo == 10,
         !is.na(ep_markov)) %>%
  mutate(
    fz          = round(yardline_100 / 10) * 10,
    phase       = game_phase(qtr, half_seconds_remaining),
    phase_label = case_when(
      phase == "H1_normal" ~ "1st Half, Normal",
      phase == "H1_2min"   ~ "1st Half, 2-Minute",
      phase == "H2_normal" ~ "2nd Half, Normal",
      phase == "H2_2min"   ~ "2nd Half, 2-Minute"
    )
  ) %>%
  group_by(fz, phase_label) %>%
  summarise(ep = mean(ep_markov, na.rm = TRUE),
            n  = n(), .groups = "drop") %>%
  filter(n >= 20) %>%
  mutate(phase_label = factor(phase_label, levels = c(
    "1st Half, Normal", "1st Half, 2-Minute",
    "2nd Half, Normal", "2nd Half, 2-Minute"
  )))

d3 <- ggplot(ep_phase,
             aes(x = fz, y = ep,
                 color = phase_label,
                 linetype = phase_label)) +
  geom_hline(yintercept = 0, color = "#AAAAAA",
             linewidth = 0.6, linetype = "dashed") +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.5) +
  scale_color_manual(values = c(
    "1st Half, Normal"    = "#002A5E",
    "1st Half, 2-Minute"  = "#0080C6",
    "2nd Half, Normal"    = "#6AAFE6",
    "2nd Half, 2-Minute"  = "#FFC20E"
  )) +
  scale_linetype_manual(values = c(
    "1st Half, Normal"    = "solid",
    "1st Half, 2-Minute"  = "dashed",
    "2nd Half, Normal"    = "solid",
    "2nd Half, 2-Minute"  = "dashed"
  )) +
  scale_x_reverse(
    breaks = seq(10, 100, 10),
    labels = function(x) paste0(x, " yds")
  ) +
  labs(
    title   = "Expected Points by Field Position and Game Phase",
    caption = "Drive EP Markov Chain Model · 2018–2024 NFL Regular Season",
    x       = "Yards from Own End Zone",
    y       = "Expected Points (EP)"
  ) +
  theme_chargers() +
  title_theme()

ggsave("plots/d3_ep_by_phase.png", d3,
       width = 10, height = 6, dpi = 150,
       bg = "white")

# ── D4: EP Heatmap — 3rd down ────────────────────────────────────────────────
heatmap_ep <- pbp_final %>%
  filter(down == 3,
         half_seconds_remaining > 120,
         !is.na(ep_markov)) %>%
  mutate(
    fz       = round(yardline_100 / 10) * 10,
    dist_grp = bucket_dist(ydstogo)
  ) %>%
  group_by(fz, dist_grp) %>%
  summarise(ep = mean(ep_markov, na.rm = TRUE),
            n  = n(), .groups = "drop") %>%
  filter(n >= 20) %>%
  mutate(dist_grp = factor(dist_grp, levels = c(
    "01", "02", "03", "04", "05", "06",
    "07-08", "09-10", "11-15", "16-20",
    "21-25", "26+"
  )))

d4 <- ggplot(heatmap_ep,
             aes(x = factor(fz),
                 y = dist_grp,
                 fill = ep)) +
  geom_tile(color = "white", linewidth = 0.8) +
  scale_fill_gradient2(
    low      = ch_gold,
    mid      = "white",
    high     = ch_navy,
    midpoint = 1.5,
    name     = "Expected Points",
    labels   = function(x) sprintf("%.1f", x)
  ) +
  scale_x_discrete(
    limits = as.character(seq(10, 100, 10)),
    labels = function(x) paste0(x, " yds")
  ) +
  scale_y_discrete(
    labels = c(
      "01" = "1", "02" = "2", "03" = "3",
      "04" = "4", "05" = "5", "06" = "6",
      "07-08" = "7-8", "09-10" = "9-10",
      "11-15" = "11-15", "16-20" = "16-20",
      "21-25" = "21-25", "26+" = "26+"
    )
  ) +
  labs(
    title   = "Expected Points on 3rd Down by Field Position and Distance",
    caption = "Drive EP Markov Chain Model · 2018–2024 NFL Regular Season",
    x       = "Yards from Own End Zone",
    y       = "Distance to Go"
  ) +
  theme_chargers() +
  title_theme() +
  theme(
    panel.grid      = element_blank(),
    legend.position = "right",
    axis.text.x     = element_text(angle = 45, hjust = 1)
  )

ggsave("plots/d4_ep_heatmap.png", d4,
       width = 12, height = 7, dpi = 150,
       bg = "white")

# ── D5: Validation scatter ────────────────────────────────────────────────────
d5_data <- pbp_final %>%
  filter(!is.na(ep_markov), !is.na(drive_score)) %>%
  mutate(ep_decile = ntile(ep_markov, 10)) %>%
  group_by(ep_decile) %>%
  summarise(
    mean_ep          = mean(ep_markov,   na.rm = TRUE),
    mean_drive_score = mean(drive_score, na.rm = TRUE),
    n                = n(),
    .groups          = "drop"
  )

d5 <- ggplot(d5_data,
             aes(x = mean_ep,
                 y = mean_drive_score)) +
  geom_abline(slope = 1, intercept = 0,
              color = "#AAAAAA", linetype = "dashed",
              linewidth = 0.8) +
  geom_smooth(method = "lm", se = TRUE,
              color = ch_blue, fill = ch_blue,
              alpha = 0.1, linewidth = 0.8) +
  geom_point(aes(size = n), color = ch_navy,
             alpha = 0.85) +
  geom_text(aes(label = paste0("D", ep_decile)),
            nudge_y = 0.12, size = 3.5,
            fontface = "bold", color = ch_navy) +
  scale_size_continuous(range = c(3, 8),
                        guide = "none") +
  labs(
    title   = "Predicted EP vs Actual Drive Outcome by Decile",
    caption = "Drive EP Markov Chain Model · 2018–2024 NFL Regular Season",
    x       = "Mean Predicted EP",
    y       = "Mean Actual Drive Score"
  ) +
  theme_chargers() +
  title_theme()

ggsave("plots/d5_validation.png", d5,
       width = 10, height = 6, dpi = 150,
       bg = "white")

# ── D6: Drive outcome rates ───────────────────────────────────────────────────
calibration_data <- pbp_final %>%
  filter(!is.na(ep_markov), !is.na(drive_score)) %>%
  mutate(
    ep_bin           = ntile(ep_markov, 20),
    actual_td        = as.integer(drive_score == 7),
    actual_fg        = as.integer(drive_score == 3),
    actual_scoreless = as.integer(drive_score == 0)
  ) %>%
  group_by(ep_bin) %>%
  summarise(
    mean_ep       = mean(ep_markov,        na.rm = TRUE),
    pct_td        = mean(actual_td,        na.rm = TRUE),
    pct_fg        = mean(actual_fg,        na.rm = TRUE),
    pct_scoreless = mean(actual_scoreless, na.rm = TRUE),
    n             = n(),
    .groups       = "drop"
  )

crossover <- calibration_data %>%
  mutate(diff  = pct_td - pct_scoreless,
         cross = diff > 0 & lag(diff) <= 0) %>%
  filter(cross == TRUE) %>%
  slice(1)

outcome_long <- calibration_data %>%
  select(mean_ep, pct_td, pct_fg, pct_scoreless) %>%
  pivot_longer(c(pct_td, pct_fg, pct_scoreless),
               names_to = "outcome", values_to = "pct") %>%
  mutate(outcome = case_when(
    outcome == "pct_td"        ~ "Touchdown",
    outcome == "pct_fg"        ~ "Field Goal",
    outcome == "pct_scoreless" ~ "No Score"
  ),
  outcome = factor(outcome,
                   levels = c("Touchdown",
                              "Field Goal",
                              "No Score")))

d6 <- ggplot(outcome_long,
             aes(x = mean_ep, y = pct,
                 color = outcome)) +
  geom_vline(xintercept = crossover$mean_ep,
             color = "#AAAAAA", linewidth = 0.6,
             linetype = "dotted") +
  annotate("text",
           x = crossover$mean_ep + 0.15,
           y = 0.28,
           label = paste0("Crossover\n~EP ",
                          round(crossover$mean_ep, 1)),
           size = 3, color = "#555555",
           hjust = 0, fontface = "italic") +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.5) +
  scale_color_manual(values = c(
    "Touchdown"  = ch_navy,
    "Field Goal" = ch_blue,
    "No Score"   = ch_gold
  )) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1)
  ) +
  labs(
    title   = "Drive Outcome Rates by Predicted EP",
    caption = "Drive EP Markov Chain Model · 2018–2024 NFL Regular Season",
    x       = "Mean Predicted EP",
    y       = "Actual Outcome Rate"
  ) +
  theme_chargers() +
  title_theme()

ggsave("plots/d6_outcome_rates.png", d6,
       width = 10, height = 6, dpi = 150,
       bg = "white")

# ── D7: Sparse state coverage ─────────────────────────────────────────────────
sparse_data <- pbp_final %>%
  mutate(
    dist_bucket  = bucket_dist(ydstogo),
    field_bucket = bucket_field(yardline_100)
  ) %>%
  filter(!is.na(down)) %>%
  group_by(down, dist_bucket, field_bucket) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(
    sparsity = case_when(
      n <   5 ~ "< 5 plays",
      n <  10 ~ "5–9 plays",
      n <  20 ~ "10–19 plays",
      n <  50 ~ "20–49 plays",
      TRUE    ~ "50+ plays"
    ),
    sparsity = factor(sparsity, levels = c(
      "< 5 plays", "5–9 plays", "10–19 plays",
      "20–49 plays", "50+ plays"
    )),
    dist_bucket = factor(dist_bucket, levels = c(
      "01", "02", "03", "04", "05", "06",
      "07-08", "09-10", "11-15", "16-20",
      "21-25", "26+"
    )),
    field_bucket = factor(field_bucket, levels = c(
      "01-10", "11-20", "21-30", "31-40",
      "41-50", "51-60", "61-70", "71-80",
      "81-90", "91-100"
    )),
    down_label = paste0(down, c(
      "1" = "st", "2" = "nd",
      "3" = "rd", "4" = "th"
    )[as.character(down)], " Down")
  )

d7 <- ggplot(sparse_data,
             aes(x = field_bucket,
                 y = dist_bucket,
                 fill = sparsity)) +
  geom_tile(color = "white", linewidth = 0.6) +
  scale_fill_manual(
    values = c(
      "< 5 plays"   = "#C0392B",
      "5–9 plays"   = "#E07B00",
      "10–19 plays" = "#F4D03F",
      "20–49 plays" = "#82E0AA",
      "50+ plays"   = "#1E8449"
    ),
    name = "Observations per State"
  ) +
  scale_x_discrete(
    labels = c(
      "01-10" = "1-10", "11-20" = "11-20",
      "21-30" = "21-30", "31-40" = "31-40",
      "41-50" = "41-50", "51-60" = "51-60",
      "61-70" = "61-70", "71-80" = "71-80",
      "81-90" = "81-90", "91-100" = "91-100"
    )
  ) +
  scale_y_discrete(
    labels = c(
      "01" = "1", "02" = "2", "03" = "3",
      "04" = "4", "05" = "5", "06" = "6",
      "07-08" = "7-8", "09-10" = "9-10",
      "11-15" = "11-15", "16-20" = "16-20",
      "21-25" = "21-25", "26+" = "26+"
    )
  ) +
  facet_wrap(~down_label, ncol = 2) +
  labs(
    title   = "State Coverage — Observations per State",
    caption = "Drive EP Markov Chain Model · 2018–2024 NFL Regular Season",
    x       = "Yards from Own End Zone",
    y       = "Distance to Go"
  ) +
  theme_chargers() +
  title_theme() +
  theme(
    panel.grid      = element_blank(),
    axis.text.x     = element_text(angle = 45,
                                   hjust = 1, size = 8),
    axis.text.y     = element_text(size = 8),
    legend.position = "bottom"
  )

ggsave("plots/d7_state_coverage.png", d7,
       width = 12, height = 10, dpi = 150,
       bg = "white")

# =============================================================================
# COACH PLOTS
# =============================================================================

# ── C1: 2nd & Short heat map — field position x distance ─────────────────────
heat_data_fine <- second_short %>%
  filter(yardline_100 >= 5) %>%
  mutate(
    fz         = round(yardline_100 / 10) * 10,
    fz_label   = paste0(fz, " yds"),
    dist_label = paste0("2nd & ", ydstogo)
  ) %>%
  group_by(dist_label, fz, fz_label, play_type) %>%
  summarise(epa = mean(epa, na.rm = TRUE),
            n   = n(), .groups = "drop") %>%
  filter(n >= 15) %>%
  pivot_wider(names_from  = play_type,
              values_from = c(epa, n)) %>%
  mutate(
    epa_diff = epa_pass - epa_run,
    rec_play = if_else(epa_pass >= epa_run,
                       "Pass", "Run")
  ) %>%
  filter(!is.na(epa_diff))

c1 <- ggplot(heat_data_fine,
             aes(x = dist_label,
                 y = reorder(fz_label, -fz),
                 fill = epa_diff)) +
  geom_tile(color = "white", linewidth = 1.2) +
  scale_fill_gradient2(
    low      = ch_gold,
    mid      = "white",
    high     = ch_blue,
    midpoint = 0,
    limits   = c(-0.1, 0.1),
    oob      = scales::squish,
    name     = "Pass EPA − Run EPA",
    labels   = function(x) sprintf("%+.3f", x)
  ) +
  labs(
    title   = "2nd & Short: Pass vs Run EPA by Field Position and Distance",
    caption = "Drive EP Markov Chain Model · 2018–2024 NFL Regular Season",
    x       = NULL,
    y       = "Yards from Own End Zone"
  ) +
  theme_chargers() +
  title_theme() +
  theme(
    panel.grid       = element_blank(),
    legend.position  = "bottom",
    legend.key.width = unit(2, "cm")
  )

ggsave("plots/c1_heatmap.png", c1,
       width = 8, height = 10, dpi = 150,
       bg = "white")

# ── C2: EPA by game situation ─────────────────────────────────────────────────
c2_data <- second_short %>%
  mutate(game_situation = factor(game_situation,
                                 levels = c(
    "Trailing 7+", "Trailing 1-6", "Tied",
    "Leading 1-6", "Leading 7+"
  ))) %>%
  group_by(game_situation, play_type) %>%
  summarise(
    epa = mean(epa, na.rm = TRUE),
    se  = sd(epa, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

c2 <- ggplot(c2_data,
             aes(x = game_situation, y = epa,
                 fill = play_type)) +
  geom_col(position = position_dodge(0.72),
           width = 0.65, alpha = 0.92) +
  geom_errorbar(
    aes(ymin = epa - 1.96 * se,
        ymax = epa + 1.96 * se),
    position = position_dodge(0.72),
    width = 0.22, color = "#333333",
    linewidth = 0.5
  ) +
  geom_hline(yintercept = 0, color = "#666666",
             linewidth = 0.5, linetype = "dashed") +
  scale_fill_manual(
    values = c("pass" = col_pass, "run" = col_run),
    labels = c("Pass", "Run")
  ) +
  scale_y_continuous(
    labels = function(x) sprintf("%+.3f", x)
  ) +
  labs(
    title   = "2nd & Short: EPA by Game Situation",
    caption = "Drive EP Markov Chain Model · 2018–2024 · Error bars = 95% CI",
    x = NULL, y = "EPA per Play"
  ) +
  theme_chargers() +
  title_theme() +
  theme(axis.text.x = element_text(angle = 12,
                                   hjust = 1))

ggsave("plots/c2_game_situation.png", c2,
       width = 10, height = 6, dpi = 150,
       bg = "white")

# ── C3: Actual pass rate by situation ─────────────────────────────────────────
dot_data <- second_short %>%
  mutate(game_situation = factor(game_situation,
                                 levels = c(
    "Trailing 7+", "Trailing 1-6", "Tied",
    "Leading 1-6", "Leading 7+"
  ))) %>%
  group_by(game_situation, play_type) %>%
  summarise(
    epa = mean(epa, na.rm = TRUE),
    n   = n(),
    .groups = "drop"
  ) %>%
  pivot_wider(names_from  = play_type,
              values_from = c(epa, n)) %>%
  mutate(
    actual_pass_rate = n_pass / (n_pass + n_run),
    model_says_pass  = epa_pass >= epa_run
  )

c3 <- ggplot(dot_data,
             aes(x = game_situation,
                 y = actual_pass_rate,
                 fill = model_says_pass)) +
  geom_col(width = 0.6, alpha = 0.9) +
  geom_hline(yintercept = 0.5,
             color = "#888888",
             linewidth = 0.5,
             linetype = "dashed") +
  geom_text(
    aes(label = percent(actual_pass_rate,
                        accuracy = 1)),
    vjust = -0.5, fontface = "bold",
    color = ch_navy, size = 4.5
  ) +
  annotate("text", x = 5.4, y = 0.52,
           label = "50%", size = 3,
           color = "#888888", hjust = 0) +
  scale_fill_manual(
    values = c("TRUE"  = ch_blue,
               "FALSE" = ch_gold),
    labels = c("TRUE"  = "Data suggests: Pass",
               "FALSE" = "Data suggests: Run")
  ) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(0, 0.65)
  ) +
  labs(
    title   = "2nd & Short: Actual Pass Rate by Game Situation",
    caption = "Drive EP Markov Chain Model · 2018–2024 NFL Regular Season",
    x       = NULL,
    y       = "Actual Pass Rate",
    fill    = NULL
  ) +
  theme_chargers() +
  title_theme() +
  theme(
    legend.position = "bottom",
    axis.text.x     = element_text(size = 11,
                                   color = ch_navy)
  )

ggsave("plots/c3_pass_rate.png", c3,
       width = 10, height = 6, dpi = 150,
       bg = "white")

# =============================================================================
# GT TABLES
# =============================================================================

make_gt_table <- function(data, rowname_col,
                           title, subtitle) {
  data %>%
    gt(rowname_col = rowname_col) %>%
    tab_header(title = md(title),
               subtitle = md(subtitle)) %>%
    tab_spanner(label = md("**Pass**"),
                columns = ends_with("_pass")) %>%
    tab_spanner(label = md("**Run**"),
                columns = ends_with("_run")) %>%
    cols_label(
      n_pass               = "n",
      epa_pass             = "EPA",
      success_rate_pass    = "Succ%",
      first_down_rate_pass = "FD%",
      yards_gained_pass    = "Yds",
      n_run                = "n",
      epa_run              = "EPA",
      success_rate_run     = "Succ%",
      first_down_rate_run  = "FD%",
      yards_gained_run     = "Yds",
      rec_play             = "Call"
    ) %>%
    fmt_number(columns = c(epa_pass, epa_run),
               decimals = 3) %>%
    fmt_percent(
      columns  = c(success_rate_pass,
                   first_down_rate_pass,
                   success_rate_run,
                   first_down_rate_run),
      decimals = 1
    ) %>%
    data_color(
      columns = epa_pass,
      method  = "numeric",
      palette = c(ch_gold, "white", ch_blue),
      domain  = c(-0.05, 0.15)
    ) %>%
    data_color(
      columns = epa_run,
      method  = "numeric",
      palette = c(ch_gold, "white", ch_blue),
      domain  = c(-0.05, 0.15)
    ) %>%
    tab_style(
      style     = list(
        cell_fill(color = "#E8F4FF"),
        cell_text(weight = "bold", color = ch_navy)
      ),
      locations = cells_body(columns = rec_play,
                             rows = rec_play == "Pass ▶")
    ) %>%
    tab_style(
      style     = list(
        cell_fill(color = "#FFF8DC"),
        cell_text(weight = "bold", color = "#7A5C00")
      ),
      locations = cells_body(columns = rec_play,
                             rows = rec_play == "Run ▶")
    ) %>%
    tab_style(
      style     = cell_text(weight = "bold"),
      locations = cells_stub()
    ) %>%
    tab_style(
      style     = cell_text(color = "white",
                            weight = "bold"),
      locations = cells_column_labels()
    ) %>%
    tab_style(
      style     = cell_text(color = "white",
                            weight = "bold"),
      locations = cells_column_spanners()
    ) %>%
    tab_options(
      table.font.size                 = px(13),
      heading.title.font.size         = px(16),
      heading.subtitle.font.size      = px(11),
      column_labels.font.weight       = "bold",
      column_labels.background.color  = ch_navy,
      stub.background.color           = "#F0F4F8",
      row.striping.include_table_body = TRUE,
      row.striping.background_color   = "#F7F9FB",
      table.border.top.color          = ch_navy,
      table.border.top.width          = px(3),
      table.border.bottom.color       = ch_navy,
      table.border.bottom.width       = px(2)
    )
}

gt_sit_data <- second_short %>%
  mutate(game_situation = factor(game_situation,
                                 levels = c(
    "Trailing 7+", "Trailing 1-6", "Tied",
    "Leading 1-6", "Leading 7+"
  ))) %>%
  group_by(game_situation, play_type) %>%
  summarise(
    n               = n(),
    epa             = round(mean(epa,          na.rm = TRUE), 3),
    success_rate    = round(mean(success,       na.rm = TRUE), 3),
    first_down_rate = round(mean(first_down,    na.rm = TRUE), 3),
    yards_gained    = round(mean(yards_gained,  na.rm = TRUE), 1),
    .groups         = "drop"
  ) %>%
  pivot_wider(
    names_from  = play_type,
    values_from = c(n, epa, success_rate,
                    first_down_rate, yards_gained)
  ) %>%
  mutate(rec_play = if_else(epa_pass >= epa_run,
                            "Pass ▶", "Run ▶"))

gt_tbl1 <- make_gt_table(
  gt_sit_data,
  rowname_col = "game_situation",
  title       = "**2nd & Short: Play Selection by Game Situation**",
  subtitle    = "EPA, Success Rate, First Down Rate · 2018–2024"
)

gt_fz_data <- second_short %>%
  mutate(field_zone = factor(field_zone, levels = c(
    "Own Territory (60+)", "Midfield (41-60)",
    "Scoring Range (21-40)", "Red Zone"
  ))) %>%
  group_by(field_zone, play_type) %>%
  summarise(
    n               = n(),
    epa             = round(mean(epa,          na.rm = TRUE), 3),
    success_rate    = round(mean(success,       na.rm = TRUE), 3),
    first_down_rate = round(mean(first_down,    na.rm = TRUE), 3),
    yards_gained    = round(mean(yards_gained,  na.rm = TRUE), 1),
    .groups         = "drop"
  ) %>%
  pivot_wider(
    names_from  = play_type,
    values_from = c(n, epa, success_rate,
                    first_down_rate, yards_gained)
  ) %>%
  mutate(rec_play = if_else(epa_pass >= epa_run,
                            "Pass ▶", "Run ▶"))

gt_tbl2 <- make_gt_table(
  gt_fz_data,
  rowname_col = "field_zone",
  title       = "**2nd & Short: Play Selection by Field Zone**",
  subtitle    = "EPA, Success Rate, First Down Rate · 2018–2024"
)

print(gt_tbl1)
print(gt_tbl2)

cat("\nAll plots saved to plots/ directory\n")
