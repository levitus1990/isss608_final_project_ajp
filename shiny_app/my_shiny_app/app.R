# =====================================================================
# app.R   Unmasking the Leak (VAST Challenge 2026 MC1)
#   Tab 1  The Day     (Module 1): crisis scrubber, timeline, feed, metrics
#   Tab 2  The Norm    (Module 2): baseline vs crisis, z-score anomaly
#   Tab 3  The Intent  (Module 3): intent chain and anonymous reveal
#   Tab 4  The Verdict (Module 4): evidence-layer toggles and readouts
# Data: .rds files produced by data_prep.R (in ./data/)
#
# COLOUR DISCIPLINE (Tier 1 pass):
#   navy   #243447 / #2d3e50  structure, navigation, "current time"
#   neutral #b8c4d0 / #9aa7b4  baseline, ordinary agents
#   amber  #d47a22            suspicion / activity / crisis / person-of-interest
#   green  #2e7d6e            cleared / post-consent / resolved evidence
#   blue   #3678a8            reduced activity / downward movement
#   red    #c0392b            reserved: actual leak / breach events only
# No plot logic or copy was changed in this pass; colours only.
# =====================================================================

library(shiny)
library(dplyr)
library(ggplot2)
library(scales)
library(visNetwork)
library(gt)
library(gtExtras)
library(igraph)
library(svglite)

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
  body { background:#f5f7f9; color:#243447;
         font-family:'Segoe UI', Arial, sans-serif; }
  .app-sub { color:#777; margin-bottom:18px; }

  /* ---- outer shell: navy navbar ---- */
  .navbar { background:#243447 !important; border:none; border-radius:0;
            box-shadow:0 1px 4px rgba(0,0,0,0.12); }
  .navbar-default .navbar-brand,
  .navbar-default .navbar-nav > li > a { color:#eaf0f5 !important; }
  .navbar-default .navbar-brand { font-weight:700; letter-spacing:.01em; }
  .navbar-default .navbar-nav > li > a:hover { color:#ffffff !important; }
  .navbar-default .navbar-nav > .active > a,
  .navbar-default .navbar-nav > .active > a:hover,
  .navbar-default .navbar-nav > .active > a:focus {
    background:#ffffff !important; color:#243447 !important;
    border-bottom:3px solid #d47a22; font-weight:600; }
  .well { background:#ffffff; border:1px solid #e1e6eb;
          box-shadow:none; border-radius:10px; }

  /* ---- module header band (case-file section header) ---- */
  .mod-band { background:#243447; color:#fff; border-radius:8px 8px 0 0;
              padding:10px 16px; margin:4px 0 0; font-weight:700;
              letter-spacing:.02em; font-size:15px; }
  .mod-band .mod-no { color:#9fb0c2; font-weight:700; margin-right:8px; }

  .metric-card { background:#fff; border:1px solid #e6e6e6; border-radius:10px;
                 padding:16px 18px; text-align:center; height:100%; }
  .metric-label { font-size:12px; color:#888; margin:0; }
  .metric-val   { font-size:24px; font-weight:700; margin:4px 0 0; color:#243447; }
  .metric-shift { font-size:12px; margin-top:4px; }
  .up { color:#d47a22; } .down { color:#3678a8; } .flat { color:#8b97a6; }

  /* Module 1 */
  .now-pill { display:inline-block; background:#243447; color:#fff; border-radius:20px;
              padding:4px 14px; font-size:13px; font-weight:600; }
  .pin-row { display:flex; flex-wrap:wrap; gap:8px; margin:14px 0; }
  .pin { font-size:12px; padding:5px 11px; border-radius:18px; border:1px solid #e0e0e0;
         background:#fff; color:#aaa; }
  .pin.past { background:#eef2f6; color:#555; border-color:#d6e0ea; }
  /* current time is navy (structural), not red */
  .pin.now  { background:#243447; color:#fff; border-color:#243447; font-weight:700; }
  /* red is reserved for an actual leak event */
  .pin.now.leak { background:#c0392b; color:#fff; border-color:#c0392b; }
  .pin.leak.past { background:#fde7e2; color:#a8452a; border-color:#f1c9bb; }
  .feed-msg { background:#fff; border:1px solid #ececec; border-radius:7px;
              padding:9px 12px; margin-bottom:7px; }
  .feed-who { font-size:11px; color:#243447; font-weight:600; }
  .feed-ch  { font-size:10px; color:#aaa; }
  .feed-txt { font-size:12.5px; color:#444; margin-top:3px; line-height:1.4; }
  /* public posts: amber accent (notable channel), not leak-red */
  .feed-txt.pub { border-left:3px solid #d47a22; padding-left:8px; }
  .prior-card { background:#fff7ed; border:1px solid #fed7aa; border-left:4px solid #d47a22;
                border-radius:8px; padding:12px 16px; margin-top:6px; }
  .prior-title { font-size:13px; font-weight:700; color:#9a4a13; letter-spacing:.02em; }
  .prior-body { font-size:12.5px; color:#5b4636; margin-top:6px; line-height:1.5; }

  /* Module 3 */
  .chain-card { background:#fff; border:1px solid #e6e6e6; border-left:4px solid #b8c4d0;
                border-radius:8px; padding:14px 18px; margin-bottom:12px; }
  /* post-consent steps are cleared/resolved evidence: green, not red */
  .chain-card.cleared { border-left-color:#2e7d6e; background:#f3faf7; }
  .chain-step { font-size:12px; color:#999; font-weight:700; letter-spacing:.05em; }
  .chain-headline { font-size:16px; font-weight:700; color:#243447; margin:4px 0 6px; }
  .chain-tag { font-size:12px; color:#666; margin-bottom:8px; }
  .chain-tag.cleared { color:#2e7d6e; font-weight:700; }
  .chain-meta { font-size:11px; color:#aaa; margin-bottom:8px; }
  .chain-body { font-size:13px; color:#444; white-space:pre-wrap; line-height:1.5; }
  .anon-card { background:#fff; border:1px solid #e6e6e6; border-radius:8px;
               padding:12px 14px; margin-bottom:10px; }
  .anon-body { font-size:12.5px; color:#444; line-height:1.45; }
  .anon-auth { font-size:11px; color:#999; margin-top:8px; font-style:italic; }
  /* reveal of concealed authorship: amber (suspicion/concealed conduct), not leak-red */
  .anon-auth.revealed { color:#d47a22; font-weight:700; font-style:normal; }

  /* amber reveal button */
  .btn-reveal { background:#d47a22; border-color:#d47a22; color:#fff; }
  .btn-reveal:hover, .btn-reveal:focus, .btn-reveal:active {
    background:#b86415; border-color:#b86415; color:#fff; }

  /* Module 4 */
  .layer-box { background:#fff; border:1px solid #e6e6e6; border-radius:10px;
               padding:16px 18px; margin-bottom:16px; }
  .verdict-card { background:#fff; border:1px solid #e6e6e6; border-radius:10px;
                  padding:18px 20px; margin-bottom:14px; }
  .verdict-card.secondary { border-left:4px solid #d47a22; }
  .verdict-q { font-size:13px; color:#888; margin:0 0 6px; }
  .verdict-tag { display:inline-block; font-size:10px; font-weight:700; letter-spacing:.04em;
                 padding:2px 8px; border-radius:12px; margin-bottom:8px; }
  .verdict-tag.primary { background:#eef2f6; color:#243447; }
  .verdict-tag.second  { background:#fbeede; color:#9a4a13; }
  .verdict-a { font-size:19px; font-weight:700; color:#243447; margin:0; line-height:1.3; }
  .verdict-a.bad { color:#d47a22; }
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
    div(class = "mod-band", span(class = "mod-no", "MODULE 1"), "THE DAY"),
    div(class = "app-sub",
        "Drag through the timeline and watch the crisis day unfold."),
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
                 visNetworkOutput("day_network", height = "230px")),
             uiOutput("prior_incident")),
      
      column(4,
             uiOutput("day_feed_head"),
             div(style = "max-height:520px; overflow-y:auto;", uiOutput("day_feed")))
    )
  ),
  
  # ===== TAB 2: THE BASELINE =====
  tabPanel(
    "The Baseline",
    div(class = "mod-band", span(class = "mod-no", "MODULE 2"), "THE BASELINE"),
    div(class = "app-sub", "Baseline behaviour against the crisis day."),
    sidebarLayout(
      sidebarPanel(
        width = 4,
        selectInput("agent", "Select agent",
                    choices = agent_choices, selected = "legal_agent"),
        helpText("Baseline is rounds 0 to 12 (May 17 to Jun 4, daily).",
                 "Crisis is rounds 13 to 22 (Jun 5, hourly).",
                 "Because the phases use different time resolutions, figures are",
                 "messages per simulation round, read as direction and magnitude",
                 "rather than exact time-based rates."),
        hr(),
        helpText("Cards compare the selected agent's baseline against the",
                 "crisis day. The chart shows each round's deviation from",
                 "that agent's own baseline."),
        
        # --- NEW PLACEMENT: Table moved to the sidebar ---
        hr(),
        h4("Agent summary", style = "font-size: 15px; font-weight: bold; margin-top: 20px;"),
        p(style = "color:#777; font-size:12px;",
          "The net shift per agent, and their activity across all 23 rounds."),
        # Wrapped in a div to prevent horizontal overlap on small screens
        div(style = "overflow-x: auto; margin-top: 10px;", 
            gt_output("agent_table"))
      ),
      
      mainPanel(
        width = 8,
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
        plotOutput("zplot", height = "380px"),
        br(),
        h4("Every agent at once: baseline to crisis"),
        p(style = "color:#777; font-size:13px;",
          "Each agent's shift from baseline to crisis. Legal and Social-Manager surged;",
          "the supervisory agents reduced activity, with Platform-Trust dropping most."),
        plotOutput("slopeplot", height = "460px")
        
        # --- DELETED: Table UI elements used to be here ---
      )
    )
  ),
  
  # ===== TAB 3: THE TRAIL =====
  tabPanel(
    "The Trail",
    div(class = "mod-band", span(class = "mod-no", "MODULE 3"), "THE TRAIL"),
    div(class = "app-sub", "The planned response, and who was really posting."),
    fluidRow(
      column(6,
             h4("The intent chain"),
             p(style = "color:#777; font-size:13px;",
               "Six messages, in order, from Legal's own communications.",
               "Briefing counsel the day before could look like Legal planned the breach,",
               "but by then a leak was clearly coming. This is a planned response,",
               "not a planned leak. The trigger fires only after the scoop and after consent."),
             uiOutput("chain_cards")),
      column(6,
             h4("The anonymous channel"),
             p(style = "color:#777; font-size:13px;",
               "Twelve posts that appeared to the public as neutral, third party",
               "commentary across the crisis day. Who actually wrote them?"),
             div(style = "margin-bottom:12px;",
                 actionButton("reveal_btn", "Reveal authors",
                              class = "btn-reveal", icon = icon("eye")),
                 actionButton("reset_btn", "Reset", icon = icon("rotate-left"))),
             uiOutput("anon_cards"))
    )
  ),
  
  # ===== TAB 4: THE VERDICT =====
  tabPanel(
    "The Verdict",
    div(class = "mod-band", span(class = "mod-no", "MODULE 4"), "THE VERDICT"),
    div(class = "app-sub",
        "Admit each layer of evidence and watch the answer take shape."),
    sidebarLayout(
      sidebarPanel(
        width = 4,
        div(class = "layer-box",
            h5("Admit evidence layers"),
            p(style = "font-size:12px; color:#888;",
              "Tick layers to admit them into the analysis. The answers",
              "update as the evidence base grows."),
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
            span(class = "verdict-tag primary", "PRIMARY QUESTION"),
            p(class = "verdict-q", "Did an agent cause the embargo breach?"),
            uiOutput("v_origin")),
        div(class = "verdict-card secondary",
            span(class = "verdict-tag second", "SECONDARY FINDING"),
            p(class = "verdict-q", "What separate concealed conduct did the investigation uncover?"),
            uiOutput("v_misconduct")),
        div(class = "verdict-card secondary",
            span(class = "verdict-tag second", "LEADING INDICATOR"),
            p(class = "verdict-q", "Were there warning signs, and did oversight cover them?"),
            uiOutput("v_warning")),
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
      same_hour <- abs(as.numeric(difftime(e$ts, now_ts, units = "mins"))) < 30
      is_past <- e$ts <= now_ts
      cls <- if (same_hour) paste("pin now", if (e$type == "leak") "leak" else "")
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
      geom_vline(xintercept = cr, color = "#243447", linewidth = 1) +
      geom_vline(xintercept = 12.5, linetype = "dashed", color = "grey60") +
      annotate("text", x = 12.5, y = max(rs$n_msgs),
               label = "crisis day", hjust = -0.05, vjust = 1,
               size = 3.3, color = "grey45") +
      scale_fill_manual(values = c("elapsed" = "#243447", "ahead" = "#e6ebf0"),
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
  
  # ----- prior-incident card: the @Elena leading indicator (Q3) -----
  output$prior_incident <- renderUI({
    div(class = "prior-card",
        div(class = "prior-title", "PRIOR INCIDENT  ·  29 MAY, A WEEK BEFORE THE CRISIS"),
        div(class = "prior-body",
            "Social-Manager publicly tagged the CivicLoom CEO with \"big things coming.\" ",
            "A CivicLoom employee liked the post before it was deleted after fourteen minutes. ",
            "This is the one moment an agent put merger-adjacent intent into public, and it is ",
            "what prompted the Judge to be assigned as compliance monitor and a social hold to ",
            "be imposed. A warning sign the system did act on, though the later concealed posting ",
            "ran on a different channel."))
  })
  
  # ----- per-round communication network (safe redraw) -----
  output$day_network <- renderVisNetwork({
    cr <- cur_round()
    id_author <- setNames(comms$agent_id, comms$message_id)
    cm <- comms %>% filter(round_index == cr, !is.na(responding_to))
    e <- cm %>%
      mutate(from = unname(id_author[responding_to]), to = agent_id) %>%
      filter(!is.na(from), !is.na(to), from != to) %>%
      count(from, to, name = "value")
    
    sent <- comms %>% filter(round_index == cr) %>% count(agent_id, name = "n")
    nodes <- net_nodes %>%
      left_join(sent, by = c("id" = "agent_id")) %>%
      mutate(
        value = ifelse(is.na(n), 1, n + 2),
        # Legal is the person of interest (amber), not the culprit (red).
        # Ordinary agents stay neutral grey. Darker border on Legal for emphasis.
        color.background = ifelse(id == "legal_agent", "#d47a22", "#a8b3bf"),
        color.border     = ifelse(id == "legal_agent", "#8c4d0c", "#718096"),
        borderWidth      = ifelse(id == "legal_agent", 3, 1)
      )
    
    edges <- if (nrow(e) == 0) {
      data.frame(from = character(0), to = character(0), value = numeric(0))
    } else e
    
    visNetwork(nodes, edges) %>%
      visNodes(font = list(size = 16)) %>%
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
      # crisis bars amber (heightened activity), baseline neutral grey
      scale_fill_manual(values = c("FALSE"="#b8c4d0","TRUE"="#d47a22"), guide = "none") +
      scale_x_continuous(breaks = seq(0, 22, 2)) +
      labs(title = paste0(lbl, ": message volume anomaly by round"),
           subtitle = "Baseline standard deviations from this agent's own baseline mean",
           x = "Round (0 to 12 baseline, 13 to 22 crisis day)", y = "Z-score") +
      theme_house()
  })
  
  output$slopeplot <- renderPlot({
    dumb <- baseline_stats %>%
      select(agent_id, phase, msgs_per_round) %>%
      tidyr::pivot_wider(names_from = phase, values_from = msgs_per_round) %>%
      left_join(agents %>% select(agent_id, agent_label), by = "agent_id") %>%
      mutate(
        change = crisis - baseline,
        direction = case_when(
          change >  0.5 ~ "Surged",
          change < -0.5 ~ "Reduced activity",
          TRUE          ~ "Little change"
        ),
        agent_label = reorder(agent_label, crisis)
      )
    
    # Surged = amber (activity, not guilt); Reduced = blue; flat = grey
    dir_cols <- c("Surged" = "#d47a22", "Reduced activity" = "#3678a8", "Little change" = "#9aa7b4")
    
    ggplot(dumb, aes(y = agent_label)) +
      geom_segment(aes(x = baseline, xend = crisis,
                       yend = agent_label, color = direction),
                   linewidth = 1.4, lineend = "round") +
      geom_point(aes(x = baseline), color = "#b8c4d0", size = 4) +
      geom_point(aes(x = crisis, color = direction), size = 4.5) +
      geom_text(aes(x = crisis, label = sprintf("%.1f", crisis), color = direction,
                    hjust = ifelse(crisis >= baseline, -0.4, 1.4)),
                size = 3.6, fontface = "bold", show.legend = FALSE) +
      scale_color_manual(values = dir_cols, name = NULL) +
      scale_x_continuous(expand = expansion(mult = c(0.08, 0.12))) +
      labs(
        title = "Messages per simulation round, baseline to crisis",
        subtitle = "Grey dot is baseline, coloured dot is crisis. Each row is one agent.",
        x = "Messages per simulation round", y = NULL
      ) +
      theme_house() +
      theme(legend.position = "top",
            panel.grid.major.y = element_blank())
  })
  
  output$agent_table <- render_gt({
    spark <- comms %>%
      filter(!is.na(agent_id)) %>%
      count(agent_id, round_index, name = "n") %>%
      tidyr::complete(agent_id, round_index = 0:22, fill = list(n = 0)) %>%
      arrange(agent_id, round_index) %>%
      group_by(agent_id) %>%
      summarise(trajectory = list(n), .groups = "drop")
    
    tbl <- baseline_stats %>%
      select(agent_id, phase, msgs_per_round) %>%
      tidyr::pivot_wider(names_from = phase, values_from = msgs_per_round) %>%
      left_join(agents %>% select(agent_id, agent_label), by = "agent_id") %>%
      left_join(spark, by = "agent_id") %>%
      mutate(baseline = round(baseline, 1), crisis = round(crisis, 1),
             change = crisis - baseline) %>%
      arrange(desc(crisis)) %>%
      select(Agent = agent_label, Change = change, Trajectory = trajectory)
    
    tbl %>%
      gt() %>%
      # FIX: Changed type to "default" based on the allowed arguments
      gt_plt_sparkline(Trajectory, type = "default", same_limit = FALSE,
                       label = FALSE,
                       palette = c("#243447", "transparent", "transparent", "#d47a22", "transparent")) %>%
      gt_color_rows(columns = Change, palette = c("#3678a8", "#f4f4f4", "#d47a22"),
                    domain = c(-6, 11)) %>%
      cols_label(Change = "Net change / rd", Trajectory = "All 23 rounds") %>%
      fmt_number(columns = Change, decimals = 1, force_sign = TRUE) %>%
      cols_align(align = "left", columns = Agent) %>%
      tab_options(table.font.size = px(13),
                  heading.title.font.size = px(15),
                  column_labels.font.weight = "bold")
    
  })
     
  # ============ MODULE 3 ============
  output$chain_cards <- renderUI({
    cards <- lapply(seq_len(nrow(intent_chain)), function(i) {
      row <- intent_chain[i, ]; mid <- row$message_id
      # post-consent steps: cleared/resolved evidence (green), not incriminating (red)
      cleared <- mid %in% c("20460605_21_022", "20460605_21_024")
      div(class = paste("chain-card", if (cleared) "cleared" else ""),
          div(class = "chain-step", paste0("STEP ", row$step, " OF 6")),
          div(class = "chain-headline", chain_headline[[mid]] %||% ""),
          div(class = paste("chain-tag", if (cleared) "cleared" else ""), chain_tag[[mid]] %||% ""),
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
  
  # ---- PRIMARY QUESTION: did an agent cause the breach? ----
  # Origin can only be assessed once the public timeline (L1) is admitted.
  # Behavioural anomalies (L2) alone cannot establish origin.
  output$v_origin <- renderUI({
    L <- layers()
    if (!L["L1"]) {
      tagList(p(class = "verdict-a", "Unresolved"),
              p(class = "verdict-note",
                "Admit the public timeline to assess origin. Behavioural anomalies alone cannot establish who put the merger out."))
    } else if (L["L1"] && !L["L2"]) {
      tagList(p(class = "verdict-a", "The timeline points away from the agents"),
              p(class = "verdict-note",
                "SaltWind published at 5:00 PM. The agents' merger-confirming posts follow the scoop, they do not precede it."))
    } else {
      tagList(p(class = "verdict-a", "No agent-originated breach in the logs"),
              p(class = "verdict-note",
                "The timeline plus the behavioural evidence places the origin outside the agents, in the wider information environment: employee posts, the CEO's hints, and departing clients. The exact upstream source is not captured in the logs, but the agents are not it."))
    }
  })
  
  # ---- SECONDARY FINDING: separate concealed conduct ----
  output$v_misconduct <- renderUI({
    L <- layers()
    if (!L["L3"] && !L["L4"]) {
      tagList(p(class = "verdict-a", "Not yet established"),
              p(class = "verdict-note", "Admit the intent chain and anonymous authorship to assess."))
    } else if (L["L3"] && !L["L4"]) {
      tagList(p(class = "verdict-a", "A planned response, defensible on its face"),
              p(class = "verdict-note",
                "The intent chain shows a pre-planned, consent-gated announcement. It activates only after SaltWind and after CivicLoom consent. A planned response, not a leak."))
    } else if (!L["L3"] && L["L4"]) {
      tagList(p(class = "verdict-a", "Undisclosed advocacy by Legal"),
              p(class = "verdict-note",
                "All twelve anonymous posts were authored by Legal-Agent, posing as neutral voices, on a channel with no recorded Judge participation."))
    } else {
      tagList(p(class = "verdict-a", "Concealed advocacy, separate from the breach"),
              p(class = "verdict-note",
                "The planned announcement was consent-gated and defensible, activated only after the scoop and after consent. Separately, all twelve anonymous posts trace to Legal, presenting as independent voices on a channel with no recorded Judge participation. This is undisclosed advocacy and a potential oversight blind spot, not the embargo breach."))
    }
  })
  
  # ---- LEADING INDICATOR: warning signs and oversight coverage ----
  # The full reading needs the behavioural warning (L2) AND the anonymous
  # authorship (L4); either alone is incomplete.
  output$v_warning <- renderUI({
    L <- layers()
    if (!L["L2"] && !L["L4"]) {
      tagList(p(class = "verdict-a", "Admit evidence to assess"),
              p(class = "verdict-note",
                "Admit the behavioural anomalies and the anonymous authorship to see the warning sign and the oversight gap."))
    } else if (L["L2"] && !L["L4"]) {
      tagList(p(class = "verdict-a", "A behavioural warning existed"),
              p(class = "verdict-note",
                "A week before the crisis, an agent slip tagging the counterparty CEO prompted the Judge to be assigned as compliance monitor. The warning was acted on."))
    } else if (!L["L2"] && L["L4"]) {
      tagList(p(class = "verdict-a", "Concealed posting on a channel with no recorded Judge participation"),
              p(class = "verdict-note",
                "Legal's anonymous posts ran on a channel with no recorded Judge participation across all 23 rounds."))
    } else {
      tagList(p(class = "verdict-a", "There was a warning, but oversight and the conduct sat on different channels"),
              p(class = "verdict-note",
                "The 29 May agent slip prompted the Judge's appointment and a social hold, so the warning was acted on. But the recorded Judge activity is in the comms huddle, while Legal's concealed posts ran on the anonymous channel, where the logs show no recorded Judge participation. The warning was addressed; the channel that carried the concealed conduct shows no recorded oversight. Whether the Judge could read that channel is not established by the logs."))
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
                "On the timeline and behavioural evidence alone, this looks like a containment failure with the origin outside the agents. A coherent reading, but an incomplete one."),
              span(class="report-tag", "Reproduces Report A"))
    } else if (!L["L1"] && !L["L2"] && L["L3"] && !L["L4"]) {
      tagList(h5("Interpretation"),
              p(style="font-size:13px;",
                "On the intent chain alone, this looks like a deliberate, pre-planned leak. A real evidential chain, but attached to the wrong act."),
              span(class="report-tag", "Reproduces Report B"))
    } else if (all(L)) {
      tagList(h5("Interpretation: the synthesis"),
              p(style="font-size:13px;",
                "With all four layers admitted, the primary answer and the secondary finding separate cleanly. Primary: the logs do not support an agent-originated breach. The agents' posts follow the scoop, and the origin lies in the wider information environment. The agents' own announcement was planned but consent-gated and defensible. Secondary: a separate concealed advocacy ran alongside it, all twelve anonymous posts authored by Legal on a channel with no recorded oversight. The leak question and the concealed-conduct question have different answers, and only the full evidence base separates them."),
              span(class="report-tag synth", "The complete picture"))
    } else {
      tagList(h5("Interpretation"),
              p(style="font-size:13px;",
                sprintf("%d of 4 layers admitted. Partial evidence yields a partial verdict. Keep admitting layers and the picture resolves.", n)))
    }
  })
}

shinyApp(ui = ui, server = server)
