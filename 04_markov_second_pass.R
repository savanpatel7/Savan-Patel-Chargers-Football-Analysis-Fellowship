# Second-pass Markov solve with field-position adjusted terminal values
# Produces the final EP lookup table used for all downstream analysis



EP_v2 <- build_ep_sparse(
  transition_counts, all_states, terminal_ep,
  punt_tv_lookup     = punt_tv_lookup,
  turnover_tv_lookup = turnover_tv_lookup
)

EP_markov_df <- tibble(
  state     = names(EP_v2),
  ep_markov = EP_v2
)

rm(EP_v2, transition_counts)
gc()

# Sanity check — 1st & 10 by field position, H1 normal not leading
EP_markov_df %>%
  filter(grepl("^1_09-10_.*_H1_normal_no_not_leading$",
               state)) %>%
  arrange(state) %>%
  print()

# Score state comparison — own 1, H2 normal
# Leading team should have lower EP than not leading
EP_markov_df %>%
  filter(grepl("^1_09-10_91-100_H2_normal_no",
               state)) %>%
  arrange(state) %>%
  print()
