# =====================================================================
# data_prep.R  —  Unmasking the Leak (VAST Challenge 2026 MC1)
# ---------------------------------------------------------------------
# Reads the raw MC1 JSON and writes analysis-ready .rds files that the
# Shiny app loads at startup. Run this ONCE whenever the data changes.
#
# Location:  shiny_app/my_shiny_app/data_prep.R
# Run from the app folder:   source("data_prep.R")
# Outputs go to:             shiny_app/my_shiny_app/data/
# =====================================================================

library(jsonlite)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(lubridate)
library(tibble)

# tidytext is only needed for the optional TF-IDF table; handled below.

# ---- null-coalescing helper (robust across package versions) --------
`%or%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# ---- paths ----------------------------------------------------------
# JSON sits in the repo's data/ folder, two levels up from the app dir.
json_path <- "../../data/MC1_final_00.json"
if (!file.exists(json_path)) {
  # fallback: allow running from repo root
  alt <- "data/MC1_final_00.json"
  if (file.exists(alt)) json_path <- alt else
    stop("Cannot find MC1_final_00.json. Edit json_path at the top of this script.")
}
out_dir <- "data"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

message("Reading: ", json_path)
raw    <- fromJSON(json_path, simplifyVector = FALSE)
rounds <- raw$rounds
message("Rounds found: ", length(rounds))

# ---- channel classification ----------------------------------------
public_channels <- c("anonymous_post", "official_post", "personal_post")
classify_channel <- function(ch) if_else(ch %in% public_channels, "Public", "Internal")

# =====================================================================
# 1. FLAT COMMUNICATIONS TABLE  ->  comms.rds
# =====================================================================
comms <- map(seq_along(rounds), function(ri) {
  rd   <- rounds[[ri]]
  hour <- rd$hour %or% NA_character_
  map(rd$communications, function(m) {
    ist <- m$internal_state %or% list()
    tibble(
      message_id    = m$message_id    %or% NA_character_,
      round_index   = ri - 1L,
      hour          = hour,
      timestamp     = m$timestamp     %or% NA_character_,
      agent_id      = m$agent_id      %or% NA_character_,
      agent_role    = m$agent_role    %or% NA_character_,
      agent_label   = m$agent_label   %or% NA_character_,
      channel       = m$channel       %or% NA_character_,
      message_type  = m$message_type  %or% NA_character_,
      responding_to = m$responding_to %or% NA_character_,
      recipients    = paste(unlist(m$recipients %or% list()), collapse = "; "),
      content       = m$content       %or% NA_character_,
      reacting      = ist$reacting     %or% NA_character_,
      deliberating  = ist$deliberating %or% NA_character_,
      rationalizing = ist$rationalizing%or% NA_character_
    )
  }) %>% bind_rows()
}) %>% bind_rows()

comms <- comms %>%
  mutate(
    ts           = ymd_hms(timestamp, quiet = TRUE),
    date         = as_date(ts),
    channel_type = classify_channel(channel),
    has_internal = !(is.na(reacting) & is.na(deliberating) & is.na(rationalizing))
  )

# =====================================================================
# 2. AGENT PROFILES  ->  agents.rds
# =====================================================================
agents <- comms %>%
  filter(!is.na(agent_id)) %>%
  count(agent_id, agent_role, agent_label, name = "total_msgs") %>%
  group_by(agent_id) %>%
  slice_max(total_msgs, n = 1, with_ties = FALSE) %>%   # stable label per agent
  ungroup() %>%
  arrange(desc(total_msgs))

# =====================================================================
# 3. ROUND SUMMARY (drives the Module 1 scrubber)  ->  round_summary.rds
#    Stock price is clean only in the baseline; crisis-day values are
#    unreliable (nulls + contextually inconsistent readings). Rather than
#    impose an analyst-selected valid range, we parse the raw value, flag
#    the single inconsistent reading of 180, and exclude the field from
#    substantive analysis. Sentiment severity and volume are the signals.
# =====================================================================
parse_price <- function(x) {
  suppressWarnings(as.numeric(str_remove_all(x %or% "", "[$,]")))
}

# severity scale (higher = worse market sentiment); documented mapping
severity_of <- function(s) {
  recode(tolower(s %or% ""),
         "neutral" = 0, "cautious" = 1, "negative" = 2, "critical" = 3,
         "low" = 3, "recovering" = 1.5, .default = NA_real_)
}

round_summary <- map(seq_along(rounds), function(ri) {
  rd <- rounds[[ri]]
  ms <- rd$environment_context$market_snapshot %or% list()
  cm <- comms %>% filter(round_index == ri - 1L)
  tibble(
    round_index     = ri - 1L,
    hour            = rd$hour %or% NA_character_,
    n_msgs          = nrow(cm),
    n_internal      = sum(cm$channel_type == "Internal"),
    n_public        = sum(cm$channel_type == "Public"),
    n_anon          = sum(cm$channel == "anonymous_post"),
    sentiment_state = ms$sentiment       %or% NA_character_,
    stock_price     = parse_price(ms$stock_price),
    pct_change      = ms$percent_change  %or% NA_character_
  )
}) %>% bind_rows() %>%
  mutate(
    ts        = ymd_hms(hour, quiet = TRUE),
    date      = as_date(ts),
    is_crisis = round_index >= 13L,
    severity  = severity_of(sentiment_state),
    # flag the single contextually inconsistent reading of 180 as NA;
    # the field is not used in the verdict regardless.
    stock_price = if_else(!is.na(stock_price) & stock_price == 180,
                          NA_real_, stock_price)
  )

# =====================================================================
# 4. BASELINE vs CRISIS PER-AGENT STATS (Module 2 cards)
#    ->  baseline_stats.rds
#    Per-round averages: baseline rounds are daily, crisis rounds hourly,
#    so these are read as direction and magnitude, not time-based rates.
# =====================================================================
baseline_stats <- comms %>%
  mutate(phase = if_else(round_index < 13L, "baseline", "crisis")) %>%
  group_by(agent_id, phase) %>%
  summarise(
    n_msgs       = n(),
    public_share = mean(channel_type == "Public"),
    n_anon       = sum(channel == "anonymous_post"),
    .groups = "drop"
  ) %>%
  mutate(
    phase_rounds   = if_else(phase == "baseline", 13L, 10L),
    msgs_per_round = n_msgs / phase_rounds
  )

# =====================================================================
# 5. PER-AGENT PER-ROUND VOLUME + Z-SCORES (Module 2 anomaly)
#    ->  zscores.rds   (z computed against each agent's baseline)
#    z = baseline standard deviations above/below that agent's baseline mean
# =====================================================================
agent_round <- comms %>%
  filter(!is.na(agent_id)) %>%
  count(agent_id, round_index, name = "n_msgs") %>%
  complete(agent_id, round_index = 0:22, fill = list(n_msgs = 0))

baseline_mu_sd <- agent_round %>%
  filter(round_index < 13L) %>%
  group_by(agent_id) %>%
  summarise(mu = mean(n_msgs), sd = sd(n_msgs), .groups = "drop")

zscores <- agent_round %>%
  left_join(baseline_mu_sd, by = "agent_id") %>%
  mutate(
    z         = if_else(!is.na(sd) & sd > 0, (n_msgs - mu) / sd, 0),
    is_crisis = round_index >= 13L
  )

# =====================================================================
# 6. RESPONSE NETWORK EDGES (Module 2 network toggle)
#    responding_to references another message_id; resolve to its author.
#    ->  edges.rds (overall) and edges_phase.rds (baseline/crisis split)
# =====================================================================
id_to_agent <- comms %>%
  filter(!is.na(message_id)) %>%
  distinct(message_id, .keep_all = TRUE) %>%
  select(message_id, agent_id) %>%
  deframe()

edges_raw <- comms %>%
  filter(!is.na(responding_to), responding_to %in% names(id_to_agent)) %>%
  transmute(
    from         = unname(id_to_agent[responding_to]),
    to           = agent_id,
    round_index,
    channel_type
  ) %>%
  filter(!is.na(from), !is.na(to), from != to)

edges <- edges_raw %>% count(from, to, name = "weight")

edges_phase <- edges_raw %>%
  mutate(phase = if_else(round_index < 13L, "baseline", "crisis")) %>%
  count(from, to, phase, name = "weight")

# =====================================================================
# 7. INTENT CHAIN (Module 3, stack 1) — six verified messages, ordered
#    Selected by message_id and arranged in a fixed sequence (match()
#    against intent_ids), so the chain is exact and stable in order even
#    if the source file is reordered.
#    ->  intent_chain.rds
# =====================================================================
intent_ids <- c(
  "20460604_12_012",  # Jun 4  outside counsel already briefed
  "20460605_15_022",  # 11:21  2:15 PM last possible moment
  "20460605_15_037",  # 11:36  changed circumstances under 4.3
  "20460605_19_022",  # 15:21  legal cover to move
  "20460605_21_022",  # 17:21  CONSENT IS IN
  "20460605_21_024"   # 17:23  GO. GO. GO.
)
intent_chain <- comms %>%
  filter(message_id %in% intent_ids) %>%
  mutate(step = match(message_id, intent_ids)) %>%
  arrange(step) %>%
  select(step, message_id, ts, timestamp, agent_id, agent_label,
         channel, channel_type, content,
         reacting, deliberating, rationalizing)

if (nrow(intent_chain) != length(intent_ids))
  warning("Intent chain: expected ", length(intent_ids),
          " messages but matched ", nrow(intent_chain),
          ". Check message_ids against the data file.")

# =====================================================================
# 8. ANONYMOUS POSTS (Module 3, stack 2) — all 12, true author hidden
#    until the user clicks Reveal. true_author kept server-side.
#    ->  anon_posts.rds
# =====================================================================
anon_posts <- comms %>%
  filter(channel == "anonymous_post") %>%
  arrange(ts) %>%
  transmute(
    message_id, ts, timestamp, round_index,
    content,
    true_author       = agent_id,
    true_author_label = agent_label
  )

# =====================================================================
# 9. EVENT PINS (Module 1 scrubber markers) — anchored to real hours
#    ->  event_pins.rds
# =====================================================================
event_pins <- tribble(
  ~ts_chr,                 ~label,                                ~type,
  "2046-06-05T11:00:00",   "OperatorInsider names two sources",   "leak",
  "2046-06-05T12:00:00",   "Employee posts go viral",             "leak",
  "2046-06-05T14:00:00",   "Elena 'exciting times' post",         "leak",
  "2046-06-05T15:00:00",   "Judge last seen",                     "system",
  "2046-06-05T17:00:00",   "SaltWind publishes merger",           "external",
  "2046-06-05T18:00:00",   "Embargo formally lifts",              "system"
) %>%
  mutate(ts = ymd_hms(ts_chr)) %>%
  select(ts, label, type)

# =====================================================================
# 10. TF-IDF TERMS (Module 2 optional vocabulary view) -> tfidf_terms.rds
#     Skipped gracefully if tidytext is not installed.
# =====================================================================
tfidf_terms <- tryCatch({
  library(tidytext)
  data("stop_words", package = "tidytext")
  comms %>%
    filter(!is.na(content), nchar(content) > 0) %>%
    mutate(phase = if_else(round_index < 13L, "baseline", "crisis")) %>%
    unnest_tokens(word, content) %>%
    anti_join(stop_words, by = "word") %>%
    filter(str_detect(word, "^[a-z]+$")) %>%
    count(agent_id, phase, word, name = "n") %>%
    bind_tf_idf(word, agent_id, n) %>%
    group_by(agent_id, phase) %>%
    slice_max(tf_idf, n = 10, with_ties = FALSE) %>%
    ungroup()
}, error = function(e) {
  message("TF-IDF skipped (", conditionMessage(e),
          "). Install 'tidytext' to enable it.")
  tibble()
})

# =====================================================================
# SAVE EVERYTHING
# =====================================================================
saveRDS(comms,          file.path(out_dir, "comms.rds"))
saveRDS(agents,         file.path(out_dir, "agents.rds"))
saveRDS(round_summary,  file.path(out_dir, "round_summary.rds"))
saveRDS(baseline_stats, file.path(out_dir, "baseline_stats.rds"))
saveRDS(zscores,        file.path(out_dir, "zscores.rds"))
saveRDS(edges,          file.path(out_dir, "edges.rds"))
saveRDS(edges_phase,    file.path(out_dir, "edges_phase.rds"))
saveRDS(intent_chain,   file.path(out_dir, "intent_chain.rds"))
saveRDS(anon_posts,     file.path(out_dir, "anon_posts.rds"))
saveRDS(event_pins,     file.path(out_dir, "event_pins.rds"))
if (nrow(tfidf_terms) > 0)
  saveRDS(tfidf_terms,  file.path(out_dir, "tfidf_terms.rds"))

# =====================================================================
# VERIFICATION PRINTOUT (sanity-check the build)
# =====================================================================
cat("\n=====================================================\n")
cat("BUILD COMPLETE — verification\n")
cat("=====================================================\n")
cat(sprintf("  Messages (comms)......... %d   (expect 912)\n", nrow(comms)))
cat(sprintf("  Agents................... %d\n", nrow(agents)))
cat(sprintf("  Rounds (round_summary)... %d   (expect 23)\n", nrow(round_summary)))
cat(sprintf("  Internal-state messages.. %d   (expect 86)\n", sum(comms$has_internal)))
cat(sprintf("  Public messages.......... %d\n", sum(comms$channel_type == "Public")))
cat(sprintf("  Anonymous posts.......... %d   (expect 12)\n", nrow(anon_posts)))
cat(sprintf("  Intent-chain messages.... %d   (expect 6)\n", nrow(intent_chain)))
cat(sprintf("  Network edges (unique)... %d\n", nrow(edges)))
cat(sprintf("  Anon authors............. %s\n",
            paste(unique(anon_posts$true_author), collapse = ", ")))
cat(sprintf("  Files written to '%s/': %d\n",
            out_dir, length(list.files(out_dir, pattern = "rds$"))))
cat("=====================================================\n")
