# =====================================================================
# app.R   Unmasking the Leak (VAST Challenge 2026 MC1)
#   Tab 1  The Day     (Module 1): crisis scrubber, timeline, feed, metrics
#   Tab 2  The Norm    (Module 2): baseline vs crisis, z-score anomaly
#   Tab 3  The Intent  (Module 3): intent chain and anonymous reveal
#   Tab 4  The Verdict (Module 4): evidence-layer toggles and readouts
# Data: .rds files produced by data_prep.R (in ./data/)
# =====================================================================

library(shiny)
library(dplyr)
library(ggplot2)
library(scales)
library(visNetwork)

`%||%` <- function(a, b) if (is.null(a)) b else a

# ---- load prepared data ---------------------------------------------
data_dir       <- "data"
agents         <- readRDS(file.path(data_dir, "agents.rds"))
baseline_stats <- readRDS(file.path(data_dir, "baseline_stats.rds"))
zscores        <- readRDS(file.path(data_dir, "zscores.rds"))
intent_chain   <- readRDS(file.path(data_dir, "intent_chain.rds"))
anon_posts     <- readRDS(file.path(data_dir, "anon_posts.rds"))
comms          <- readRDS(file.path(data_dir, "comms.rds"))
round_summary  <- readRDS(file.path(data_dir, "round_summary.rds"))
event_pins     <- readRDS(file.path(data_dir, "event_pins.rds"))

agent_choices <- setNames(agents$agent_id, agents$agent_label)

# fixed node table for the Day-tab network (positions stay put; only edges change)
net_nodes <- data.frame(
  id    = agents$agent_id,
  label = agents$agent_label,
  stringsAsFactors = FALSE
)

# round_index to human label for the slider
round_labels <- setNames(
  format(round_summary$ts, "%b %d, %H:%M"),
  as.character(round_summary$round_index)
)

chain_headline <- c(
  "20460604_12_012" = "Outside counsel was already briefed for this scenario.",
  "20460605_15_022" = "2:15 PM is the last possible moment for unilateral announcement.",
  "20460605_15_037" = "The Slack leak changes everything for the outside counsel argument.",
  "20460605_19_022" = "10b-5 liability for continued silence. This is our cover.",
  "20460605_21_022" = "CONSENT IS IN. CivicLoom verbal confirmed, written following.",
  "20460605_21_024" = "GO. GO. GO. CivicLoom bilateral consent confirmed."
)
chain_tag <- c(
  "20460604_12_012" = "Jun 4. The day before the crisis.",
  "20460605_15_022" = "11:21 AM. Planning the deadline.",
  "20460605_15_037" = "11:36 AM. Building the legal argument.",
  "20460605_19_022" = "3:21 PM. The written opinion arrives.",
  "20460605_21_022" = "5:21 PM. After SaltWind published at 5:00, after consent.",
  "20460605_21_024" = "5:23 PM. After SaltWind published at 5:00, after consent."
)

theme_house <- function() {
  theme_minimal(base_size = 13) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank(),
          plot.title = element_text(face = "bold", size = 14),
          plot.subtitle = element_text(color = "grey40"),
          axis.title = element_text(color = "grey30"))
}

app_css <- HTML("
  body { background:#fafafa; }
  .app-sub { color:#777; margin-bottom:18px; }
  .metric-card { background:#fff; border:1px solid #e6e6e6; border-radius:10px;
                 padding:16px 18px; text-align:center; height:100%; }
  .metric-label { font-size:12px; color:#888; margin:0; }
  .metric-val   { font-size:24px; font-weight:700; margin:4px 0 0; color:#2d3e50; }
  .metric-shift { font-size:12px; margin-top:4px; }
  .up { color:#c0392b; } .down { color:#2471a3; } .flat { color:#888; }

  /* Module 1 */
  .now-pill { display:inline-block; background:#2d3e50; color:#fff; border-radius:20px;
              padding:4px 14px; font-size:13px; font-weight:600; }
  .pin-row { display:flex; flex-wrap:wrap; gap:8px; margin:14px 0; }
  .pin { font-size:12px; padding:5px 11px; border-radius:18px; border:1px solid #e0e0e0;
         background:#fff; color:#aaa; }
  .pin.past { background:#eef2f6; color:#555; border-color:#d6e0ea; }
  .pin.now  { background:#c0392b; color:#fff; border-color:#c0392b; font-weight:700; }
  .pin.leak.past { background:#fde7e2; color:#a8452a; border-color:#f1c9bb; }
  .feed-msg { background:#fff; border:1px solid #ececec; border-radius:7px;
              padding:9px 12px; margin-bottom:7px; }
  .feed-who { font-size:11px; color:#2d3e50; font-weight:600; }
  .feed-ch  { font-size:10px; color:#aaa; }
  .feed-txt { font-size:12.5px; color:#444; margin-top:3px; line-height:1.4; }
  .feed-txt.pub { border-left:3px solid #c0392b; padding-left:8px; }

  /* Module 3 */
  .chain-card { background:#fff; border:1px solid #e6e6e6; border-left:4px solid #b8c4d0;
                border-radius:8px; padding:14px 18px; margin-bottom:12px; }
  .chain-card.late { border-left-color:#c0392b; }
  .chain-step { font-size:12px; color:#999; font-weight:700; letter-spacing:.05em; }
  .chain-headline { font-size:16px; font-weight:700; color:#2d3e50; margin:4px 0 6px; }
  .chain-tag { font-size:12px; color:#666; margin-bottom:8px; }
  .chain-tag.late { color:#c0392b; font-weight:600; }
  .chain-meta { font-size:11px; color:#aaa; margin-bottom:8px; }
  .chain-body { font-size:13px; color:#444; white-space:pre-wrap; line-height:1.5; }
  .anon-card { background:#fff; border:1px solid #e6e6e6; border-radius:8px;
               padding:12px 14px; margin-bottom:10px; }
  .anon-body { font-size:12.5px; color:#444; line-height:1.45; }
  .anon-auth { font-size:11px; color:#999; margin-top:8px; font-style:italic; }
  .anon-auth.revealed { color:#c0392b; font-weight:700; font-style:normal; }

  /* Module 4 */
  .layer-box { background:#fff; border:1px solid #e6e6e6; border-radius:10px;
               padding:16px 18px; margin-bottom:16px; }
  .verdict-card { background:#fff; border:1px solid #e6e6e6; border-radius:10px;
                  padding:18px 20px; margin-bottom:14px; }
  .verdict-q { font-size:13px; color:#888; margin:0 0 6px; }
  .verdict-a { font-size:19px; font-weight:700; color:#2d3e50; margin:0; line-height:1.3; }
  .verdict-a.bad { color:#c0392b; }
  .verdict-note { font-size:12px; color:#777; margin-top:6px; }
  .report-tag { display:inline-block; font-size:11px; padding:3px 9px; border-radius:20px;
                background:#eef2f6; color:#555; margin-top:8px; }
  .report-tag.synth { background:#d5e8df; color:#1e5a3f; font-weight:600; }
")

# ---- UI -------------------------------------------------------------
ui <- navbarPage(
  title = "Unmasking the Leak",
  header = tags$head(tags$style(app_css)),
  
  # ===== TAB 1: THE DAY (scrubber) =====
  tabPanel(
    "The Day",
    div(class = "app-sub",
        "Module 1. Drag through the timeline and watch the leak unfold."),
    sliderInput("rnd", NULL, min = 0, max = 22, value = 15, step = 1,
                width = "100%", ticks = FALSE,
                animate = animationOptions(interval = 900)),
    fluidRow(
      column(8,
             uiOutput("day_now"),
             uiOutput("day_pins"),
             plotOutput("day_plot", height = "230px"),
             div(style = "margin-top:8px;",
                 p(style = "font-size:12px; color:#888; margin-bottom:2px;",
                   "Who replied to whom this hour. Node size shows messages sent; Legal stays central."),
                 visNetworkOutput("day_network", height = "230px"))),

      column(4,
             uiOutput("day_feed_head"),
             div(style = "max-height:520px; overflow-y:auto;", uiOutput("day_feed")))
    )
  ),
  
  # ===== TAB 2: THE NORM =====
  tabPanel(
    "The Norm",
    div(class = "app-sub", "Module 2. Baseline behaviour against the crisis day."),
    sidebarLayout(
      sidebarPanel(
        width = 3,
        selectInput("agent", "Select agent",
                    choices = agent_choices, selected = "legal_agent"),
        helpText("Baseline is rounds 1 to 13 (May 17 to Jun 4, daily).",
                 "Crisis is rounds 14 to 23 (Jun 5, hourly)."),
        hr(),
        helpText("Cards compare the selected agent's baseline against the",
                 "crisis day. The chart shows each round's deviation from",
                 "that agent's own baseline.")
      ),
      mainPanel(
        width = 9,
        fluidRow(
          column(3, div(class = "metric-card",
                        p(class = "metric-label", "Messages / round"),
                        p(class = "metric-val", textOutput("m_rate", inline = TRUE)),
                        p(class = "metric-shift", uiOutput("m_rate_shift")))),
          column(3, div(class = "metric-card",
                        p(class = "metric-label", "Public share"),
                        p(class = "metric-val", textOutput("m_pub", inline = TRUE)),
                        p(class = "metric-shift", uiOutput("m_pub_shift")))),
          column(3, div(class = "metric-card",
                        p(class = "metric-label", "Anonymous posts"),
                        p(class = "metric-val", textOutput("m_anon", inline = TRUE)),
                        p(class = "metric-shift", uiOutput("m_anon_shift")))),
          column(3, div(class = "metric-card",
                        p(class = "metric-label", "Peak crisis z-score"),
                        p(class = "metric-val", textOutput("m_z", inline = TRUE)),
                        p(class = "metric-shift", span(class = "flat", "vs own baseline"))))
        ),
        br(),
        plotOutput("zplot", height = "380px")
      )
    )
  ),
  
  # ===== TAB 3: THE INTENT =====
  tabPanel(
    "The Intent",
    div(class = "app-sub", "Module 3. The planned response, and who was really posting."),
    fluidRow(
      column(6,
             h4("The intent chain"),
             p(style = "color:#777; font-size:13px;",
               "Six messages, in order, from Legal's own communications.",
               "Read top to bottom. The response was planned in advance and",
               "executed only after the merger was already public."),
             uiOutput("chain_cards")),
      column(6,
             h4("The anonymous channel"),
             p(style = "color:#777; font-size:13px;",
               "Twelve posts that appeared to the public as neutral, third party",
               "commentary during the embargo. Who actually wrote them?"),
             div(style = "margin-bottom:12px;",
                 actionButton("reveal_btn", "Reveal authors",
                              class = "btn-danger", icon = icon("eye")),
                 actionButton("reset_btn", "Reset", icon = icon("rotate-left"))),
             uiOutput("anon_cards"))
    )
  ),
  
  # ===== TAB 4: THE VERDICT =====
  tabPanel(
    "The Verdict",
    div(class = "app-sub",
        "Module 4. Admit each layer of evidence and watch the verdict change."),
    sidebarLayout(
      sidebarPanel(
        width = 4,
        div(class = "layer-box",
            h5("Admit evidence layers"),
            p(style = "font-size:12px; color:#888;",
              "Tick layers to admit them into the analysis. The verdict",
              "updates as the evidence base grows."),
            checkboxInput("L1", "1 · Public timeline", value = FALSE),
            checkboxInput("L2", "2 · Behavioural anomalies", value = FALSE),
            checkboxInput("L3", "3 · Intent chain", value = FALSE),
            checkboxInput("L4", "4 · Anonymous authorship", value = FALSE),
            hr(),
            actionButton("admit_all", "Admit all four", class = "btn-primary btn-sm"),
            actionButton("clear_all", "Clear", class = "btn-sm")
        )
      ),
      mainPanel(
        width = 8,
        div(class = "verdict-card",
            p(class = "verdict-q", "Who put the merger into the public domain?"),
            uiOutput("v_origin")),
        div(class = "verdict-card",
            p(class = "verdict-q", "Was there deliberate misconduct by the agents?"),
            uiOutput("v_misconduct")),
        div(class = "layer-box", uiOutput("v_interpretation"))
      )
    )
  )
)

# ---- server ---------------------------------------------------------
server <- function(input, output, session) {
  
  # ============ MODULE 1: THE DAY ============
  # debounce the slider so dragging doesn't thrash the renders
  cur_round <- reactive(input$rnd) %>% debounce(250)
  
  output$day_now <- renderUI({
    rs <- round_summary %>% filter(round_index == cur_round())
    div(span(class = "now-pill", format(rs$ts, "%A %b %d, %H:%M")),
        span(style = "margin-left:12px; color:#888; font-size:13px;",
             paste0("Round ", rs$round_index, " of 22  ·  market: ",
                    rs$sentiment_state %||% "n/a")))
  })
  
  output$day_pins <- renderUI({
    now_ts <- (round_summary %>% filter(round_index == cur_round()))$ts
    pins <- lapply(seq_len(nrow(event_pins)), function(i) {
      e <- event_pins[i, ]
      # status: past (already happened), now (this exact hour), future
      same_hour <- abs(as.numeric(difftime(e$ts, now_ts, units = "mins"))) < 30
      is_past <- e$ts <= now_ts
      cls <- if (same_hour) "pin now"
      else if (is_past) paste("pin past", if (e$type == "leak") "leak" else "")
      else "pin"
      span(class = cls, paste0(format(e$ts, "%H:%M"), " · ", e$label))
    })
    div(class = "pin-row", tagList(pins))
  })
  
  output$day_plot <- renderPlot({
    cr <- cur_round()
    rs <- round_summary %>%
      mutate(state = if_else(round_index <= cr, "elapsed", "ahead"))
    ggplot(rs, aes(round_index, n_msgs, fill = state)) +
      geom_col(width = 0.8) +
      geom_vline(xintercept = cr, color = "#2d3e50", linewidth = 1) +
      geom_vline(xintercept = 12.5, linetype = "dashed", color = "grey60") +
      annotate("text", x = 12.5, y = max(rs$n_msgs),
               label = "crisis day", hjust = -0.05, vjust = 1,
               size = 3.3, color = "grey45") +
      scale_fill_manual(values = c("elapsed" = "#2d3e50", "ahead" = "#dfe4e9"),
                        guide = "none") +
      scale_x_continuous(breaks = seq(0, 22, 2)) +
      labs(title = "Message volume across the timeline",
           subtitle = "Dark is elapsed up to the slider. The line is the current position.",
           x = "Round", y = "Messages") +
      theme_house()
  })
  
  output$day_feed_head <- renderUI({
    cm <- comms %>% filter(round_index == cur_round())
    rs <- round_summary %>% filter(round_index == cur_round())
    tagList(
      h4("Message feed"),
      p(style = "color:#888; font-size:12px; margin-bottom:10px;",
        sprintf("%s  ·  showing %d of %d messages this round",
                format(rs$ts, "%H:%M"), min(10, nrow(cm)), nrow(cm)))
    )
  })
  # ----- per-round communication network (safe redraw) -----
  output$day_network <- renderVisNetwork({
    cr <- cur_round()
    # resolve this round's replies into edges: responding_to -> author
    id_author <- setNames(comms$agent_id, comms$message_id)
    cm <- comms %>% filter(round_index == cr, !is.na(responding_to))
    e <- cm %>%
      mutate(from = unname(id_author[responding_to]), to = agent_id) %>%
      filter(!is.na(from), !is.na(to), from != to) %>%
      count(from, to, name = "value")
    
    # node size = messages this agent sent this round
    sent <- comms %>% filter(round_index == cr) %>% count(agent_id, name = "n")
    nodes <- net_nodes %>%
      left_join(sent, by = c("id" = "agent_id")) %>%
      mutate(value = ifelse(is.na(n), 1, n + 2),
             color = ifelse(id == "legal_agent", "#c0392b", "#9aa7b4"))
    
    edges <- if (nrow(e) == 0) {
      data.frame(from = character(0), to = character(0), value = numeric(0))
    } else e
    
    visNetwork(nodes, edges) %>%
      visNodes(font = list(size = 16), borderWidth = 1) %>%
      visEdges(arrows = "to", color = list(color = "#b8c4d0", opacity = 0.7),
               smooth = FALSE) %>%
      visIgraphLayout(layout = "layout_in_circle", randomSeed = 42) %>%
      visOptions(highlightNearest = TRUE) %>%
      visInteraction(dragNodes = FALSE, dragView = FALSE, zoomView = FALSE)
  })
  output$day_feed <- renderUI({
    cm <- comms %>% filter(round_index == cur_round()) %>% head(10)
    if (nrow(cm) == 0) return(p("No messages this round."))
    msgs <- lapply(seq_len(nrow(cm)), function(i) {
      m <- cm[i, ]
      is_pub <- m$channel_type == "Public"
      txt <- m$content %||% ""
      if (nchar(txt) > 240) txt <- paste0(substr(txt, 1, 240), "\u2026")
      div(class = "feed-msg",
          span(class = "feed-who", m$agent_label %||% m$agent_id),
          span(class = "feed-ch", paste0("  ·  ", m$channel)),
          div(class = paste("feed-txt", if (is_pub) "pub" else ""), txt))
    })
    tagList(msgs)
  })
  
  # ============ MODULE 2 ============
  ag_stats <- reactive({
    bs <- baseline_stats %>% filter(agent_id == input$agent)
    list(base = bs %>% filter(phase == "baseline"),
         crisis = bs %>% filter(phase == "crisis"))
  })
  getv <- function(df, col) if (nrow(df) == 0) 0 else df[[col]][1]
  
  output$m_rate <- renderText({ s <- ag_stats()
  sprintf("%.1f \u2192 %.1f", getv(s$base,"msgs_per_round"), getv(s$crisis,"msgs_per_round")) })
  output$m_pub <- renderText({ s <- ag_stats()
  sprintf("%.0f%% \u2192 %.0f%%", 100*getv(s$base,"public_share"), 100*getv(s$crisis,"public_share")) })
  output$m_anon <- renderText({ s <- ag_stats()
  sprintf("%d \u2192 %d", as.integer(getv(s$base,"n_anon")), as.integer(getv(s$crisis,"n_anon"))) })
  output$m_z <- renderText({ z <- zscores %>% filter(agent_id == input$agent, is_crisis)
  if (nrow(z) == 0) "n/a" else sprintf("+%.1f \u03c3", max(z$z, na.rm = TRUE)) })
  
  shift_ui <- function(from, to, fmt = "%.1f", invert = FALSE) {
    d <- to - from
    cls <- if (abs(d) < 1e-9) "flat" else if ((d > 0) != invert) "up" else "down"
    arrow <- if (abs(d) < 1e-9) "no change" else if (d > 0) "\u25b2" else "\u25bc"
    span(class = cls, sprintf("%s %s", arrow, sprintf(fmt, abs(d))))
  }
  output$m_rate_shift <- renderUI({ s <- ag_stats()
  shift_ui(getv(s$base,"msgs_per_round"), getv(s$crisis,"msgs_per_round")) })
  output$m_pub_shift <- renderUI({ s <- ag_stats()
  shift_ui(100*getv(s$base,"public_share"), 100*getv(s$crisis,"public_share"), "%.0f pts") })
  output$m_anon_shift <- renderUI({ s <- ag_stats()
  shift_ui(getv(s$base,"n_anon"), getv(s$crisis,"n_anon"), "%.0f") })
  
  output$zplot <- renderPlot({
    z <- zscores %>% filter(agent_id == input$agent)
    lbl <- agents$agent_label[agents$agent_id == input$agent]
    ggplot(z, aes(x = round_index, y = z, fill = is_crisis)) +
      geom_col(width = 0.7) +
      geom_hline(yintercept = 0, color = "grey60") +
      geom_vline(xintercept = 12.5, linetype = "dashed", color = "grey50") +
      annotate("text", x = 12.5, y = max(z$z, na.rm = TRUE),
               label = "crisis begins", hjust = 1.05, vjust = 1, size = 3.4, color = "grey40") +
      scale_fill_manual(values = c("FALSE"="#b8c4d0","TRUE"="#c0392b"), guide = "none") +
      scale_x_continuous(breaks = seq(0, 22, 2)) +
      labs(title = paste0(lbl, ": message volume anomaly by round"),
           subtitle = "Standard deviations from this agent's own baseline mean",
           x = "Round (0 to 12 baseline, 13 to 22 crisis day)", y = "Z-score") +
      theme_house()
  })
  
  # ============ MODULE 3 ============
  output$chain_cards <- renderUI({
    cards <- lapply(seq_len(nrow(intent_chain)), function(i) {
      row <- intent_chain[i, ]; mid <- row$message_id
      late <- mid %in% c("20460605_21_022", "20460605_21_024")
      div(class = paste("chain-card", if (late) "late" else ""),
          div(class = "chain-step", paste0("STEP ", row$step, " OF 6")),
          div(class = "chain-headline", chain_headline[[mid]] %||% ""),
          div(class = paste("chain-tag", if (late) "late" else ""), chain_tag[[mid]] %||% ""),
          div(class = "chain-meta",
              paste0(row$agent_label, "  ·  ", row$channel, "  ·  ", row$timestamp)),
          div(class = "chain-body", row$content))
    })
    tagList(cards)
  })
  
  revealed <- reactiveVal(FALSE)
  observeEvent(input$reveal_btn, revealed(TRUE))
  observeEvent(input$reset_btn,  revealed(FALSE))
  output$anon_cards <- renderUI({
    is_rev <- revealed()
    cards <- lapply(seq_len(nrow(anon_posts)), function(i) {
      row <- anon_posts[i, ]
      auth_txt <- if (is_rev) paste0(row$true_author_label) else "Anonymous"
      div(class = "anon-card",
          div(class = "anon-body", row$content),
          div(class = paste("anon-auth", if (is_rev) "revealed" else ""),
              paste0(auth_txt, "  ·  ", format(row$ts, "%H:%M"))))
    })
    tagList(cards)
  })
  
  # ============ MODULE 4 ============
  observeEvent(input$admit_all, {
    for (id in c("L1","L2","L3","L4")) updateCheckboxInput(session, id, value = TRUE)
  })
  observeEvent(input$clear_all, {
    for (id in c("L1","L2","L3","L4")) updateCheckboxInput(session, id, value = FALSE)
  })
  layers <- reactive(c(L1 = isTRUE(input$L1), L2 = isTRUE(input$L2),
                       L3 = isTRUE(input$L3), L4 = isTRUE(input$L4)))
  
  output$v_origin <- renderUI({
    L <- layers()
    if (!L["L1"] && !L["L2"]) {
      tagList(p(class = "verdict-a", "Unresolved"),
              p(class = "verdict-note", "Admit the public timeline to begin."))
    } else if (L["L1"] && !L["L2"]) {
      tagList(p(class = "verdict-a", "Appears to originate outside the agent system"),
              p(class = "verdict-note",
                "SaltWind published at 5:00 PM. The agents' public posts follow the scoop, they do not precede it."))
    } else {
      tagList(p(class = "verdict-a", "Diffuse internal leak. Not the comms agents."),
              p(class = "verdict-note",
                "Employee Slack, departing staff and the counterparty CEO, aggregated by a reporter with an inside source since May 31. The seven agents did not originate it."))
    }
  })
  
  output$v_misconduct <- renderUI({
    L <- layers()
    if (!L["L3"] && !L["L4"]) {
      tagList(p(class = "verdict-a", "Not yet established"),
              p(class = "verdict-note", "Admit the intent chain and anonymous authorship to assess."))
    } else if (L["L3"] && !L["L4"]) {
      tagList(p(class = "verdict-a", "Planned response, but lawful on its face"),
              p(class = "verdict-note",
                "The intent chain shows a pre-planned, consent-gated announcement. It fires only after SaltWind and after CivicLoom consent. A deliberate response, not a leak."))
    } else if (!L["L3"] && L["L4"]) {
      tagList(p(class = "verdict-a bad", "Yes. A concealed influence operation."),
              p(class = "verdict-note",
                "All 12 anonymous posts were authored by Legal-Agent, during the embargo, on a channel the compliance Judge never observed."))
    } else {
      tagList(p(class = "verdict-a bad", "Yes. Deliberate, on two fronts."),
              p(class = "verdict-note",
                "A pre-planned consent-gated announcement, plus a concealed anonymous influence operation run by Legal throughout the day."))
    }
  })
  
  output$v_interpretation <- renderUI({
    L <- layers(); n <- sum(L)
    if (n == 0) {
      tagList(h5("Interpretation"),
              p(style="color:#777; font-size:13px;",
                "No evidence admitted. Each combination of layers reproduces a different analyst's conclusion. Admit some to see how."))
    } else if (L["L1"] && L["L2"] && !L["L3"] && !L["L4"]) {
      tagList(h5("Interpretation"),
              p(style="font-size:13px;",
                "On the timeline and behavioural evidence alone, this looks like a system failure with the leak originating outside the agents. A coherent reading, but an incomplete one."),
              span(class="report-tag", "Reproduces Report A"))
    } else if (!L["L1"] && !L["L2"] && L["L3"] && !L["L4"]) {
      tagList(h5("Interpretation"),
              p(style="font-size:13px;",
                "On the intent chain alone, this looks like a deliberate, pre-planned leak. A real evidential chain, but attached to the wrong act."),
              span(class="report-tag", "Reproduces Report B"))
    } else if (all(L)) {
      tagList(h5("Interpretation: the synthesis"),
              p(style="font-size:13px;",
                "With all four layers admitted: an external scoop triggered the execution of a pre-planned, consent-gated response, while a concealed anonymous influence operation ran underneath all day. Neither single perspective captures this. Only the full evidence base does."),
              span(class="report-tag synth", "The complete picture"))
    } else {
      tagList(h5("Interpretation"),
              p(style="font-size:13px;",
                sprintf("%d of 4 layers admitted. Partial evidence yields a partial verdict. Keep admitting layers and the picture resolves.", n)))
    }
  })
}

shinyApp(ui = ui, server = server)