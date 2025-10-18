# app.R

suppressPackageStartupMessages({
  library(shiny)
  library(tidyverse)
  library(lubridate)
  library(scales)
  library(DT)
  library(janitor)
  library(bslib)
})

# ---------- Palette ----------
med_nav_blue   <- "#0A4D68"
med_blue       <- "#167BAA"
med_accent_y   <- "#F6B400"
med_graydark   <- "#303030"
med_muted      <- "#6E6E6E"
med_bg         <- "#FFFFFF"
med_card_bg    <- "#FFFFFF"
med_border     <- "#E9EDF0"

med_reason_pal <- c(
  "Medication change"     = "#0A4D68",
  "Non-adherence"         = "#3C8DBC",
  "Administrative error"  = "#F6B400",
  "Lost medication"       = "#CC79A7",
  "Expired stock"         = "#009E73",
  "Transfer to hospital"  = "#F0E442",
  "Deceased"              = "#999999",
  "Other"                 = "#8B4513",
  "No waste"              = "#444444"
)

# ---------- Plot theme ----------
med_theme_plot <- function() {
  theme_minimal(base_size = 12, base_family = "Poppins") +
    theme(
      plot.background = element_rect(fill = med_bg, color = NA),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_line(color = "#f4f6f7"),
      panel.grid.major.y = element_line(color = "#f4f6f7"),
      axis.title = element_text(color = med_graydark),
      plot.title = element_text(face = "bold", color = med_nav_blue, size = 14),
      plot.subtitle = element_text(color = med_muted),
      legend.position = "bottom",
      legend.title = element_text(face = "bold")
    )
}

# ---------- Business logic ----------
reason_class <- function(reason) {
  dplyr::case_when(
    reason %in% c("Medication change","Non-adherence","Administrative error","Lost medication","Expired stock") ~ "preventable",
    reason %in% c("Transfer to hospital") ~ "semi-preventable",
    reason %in% c("Deceased","No waste","Other") ~ "non-preventable",
    TRUE ~ "unknown"
  )
}

read_one_csv <- function(path, label = NULL) {
  df <- readr::read_csv(path, show_col_types = FALSE) %>% clean_names()
  if (is.null(label)) label <- tools::file_path_sans_ext(basename(path))
  df %>%
    mutate(
      facility = label,
      scheduled_date = as_date(scheduled_date),
      refill_date = as_date(refill_date),
      unit_cost = if_else(initial_weight > 0, cost_usd / initial_weight, NA_real_),
      waste_units = coalesce(as.numeric(weight_at_stop), 0),
      waste_cost = waste_units * unit_cost,
      waste_reason = as.character(waste_reason),
      reason_class = reason_class(waste_reason)
    )
}

in_control_limits <- function(x) {
  x <- x[!is.na(x)]
  center <- mean(x)
  sigma  <- mad(x) * 1.253314
  tibble(center = center, lcl = center - 3*sigma, ucl = center + 3*sigma, sigma = sigma)
}

# ---------- Theme ----------
med_theme <- bs_theme(
  version = 5,
  base_font = font_google("Poppins", local = TRUE),
  heading_font = font_google("Poppins", local = TRUE),
  bg = med_bg, fg = med_graydark, primary = med_nav_blue
)

# ---------- CSS (header, anchors, feature/contact bands, hero centering) ----------
med_css <- HTML(paste0("
:root{
  --med-nav-blue: ", med_nav_blue, ";
  --med-blue: ", med_blue, ";
  --med-accent: ", med_accent_y, ";
  --med-graydark: ", med_graydark, ";
  --med-muted: ", med_muted, ";
  --med-border: ", med_border, ";
  --med-card-bg: ", med_card_bg, ";
  --med-bg: ", med_bg, ";
}

/* smooth scrolling + offset when jumping to sections */
html { scroll-behavior: smooth; }
.section { scroll-margin-top: 110px; }

body { background: var(--med-bg); font-family: 'Poppins', system-ui, -apple-system, 'Segoe UI', Roboto, 'Helvetica Neue', Arial; color: var(--med-graydark); }

/* ---------- TOP NAVBAR ---------- */
#top-nav {
  position: sticky; top: 0; z-index: 1000;
  display:flex; align-items:center; justify-content:space-between;
  padding: 6px 32px; border-bottom: 1px solid var(--med-border); background:#fff;
}
#top-nav .brand { display:flex; align-items:center; }
#top-nav .brand img { height: 200px; margin-top: -18px; }
#top-nav .nav-links { display:flex; gap:60px; align-items:center; font-weight:600; font-size:18px; letter-spacing:.3px; margin-top:-8px; }
#top-nav .nav-links a { color: var(--med-muted); text-decoration:none; transition: color .2s; }
#top-nav .nav-links a:hover { color: var(--med-nav-blue); }

/* ---------- HERO ---------- */
#hero { padding: 28px 36px 8px 36px; }
.hero-row { display:flex; gap:28px; align-items:center; }   /* <- vertical center BOTH columns */
.hero-left { flex:0 0 45%; padding-right:20px; }
.hero-left .hero-copy { max-width: 560px; }                 /* keep text column narrow-ish */
.hero-headline { font-size:48px; line-height:1.05; color: var(--med-nav-blue); font-weight:700; margin:6px 0 14px 0; }
.hero-sub { color: var(--med-muted); font-size:16px; line-height:1.55; }

/* upload card */
.upload-card { flex:0 0 42%; background: var(--med-card-bg); border:1px solid var(--med-border); border-radius:12px; padding:18px; box-shadow:0 6px 18px rgba(10,77,104,0.04); }
.upload-card h4 { margin-top:0; color:var(--med-nav-blue); font-weight:700; }

/* KPI cards */
.kpi-row { display:flex; gap:14px; margin-top:18px; }
.kpi-card { background:#FBFDFF; border:1px solid var(--med-border); padding:12px 14px; border-radius:10px; flex:1; }
.kpi-card h4 { margin:0; font-size:12px; color:var(--med-muted); font-weight:600; }
.kpi-card h3 { margin:6px 0 0 0; font-size:20px; color:var(--med-nav-blue); font-weight:700; }

/* sections + tabs */
.section-block { padding: 24px 36px; border-top: 1px solid var(--med-border); }
.section-title { color: var(--med-nav-blue); font-weight:800; margin:0 0 8px 0; }

.main-tabs { padding: 18px 36px 36px 36px; }
.btn-med { background: var(--med-accent); border-color: var(--med-accent); color:#072235; font-weight:700; }

/* ---------- FEATURE & CONTACT bands ---------- */
.band { background-color:#0A4D68; color:white; padding:60px 80px; margin-top:20px; overflow-y:auto; }
.band h2, .band h3 { color:#FFFFFF; font-weight:700; margin:0 0 40px 0; }
.band .gold { color:#F6B400; font-weight:600; }
#features .feat-grid { display:flex; flex-wrap:wrap; gap:40px; justify-content:space-between; }
#features .feat-card { flex:1 1 40%; min-width:300px; }
#features .feat-card h4 { margin: 10px 0 6px 0; }

/* contact specifics */
#contact .contact-list { font-size:18px; }
#contact .contact-list a { color:#EAF6FF; text-decoration:underline; }
#contact .contact-list a:hover { color:#F6B400; }
#contact .contact-list b { color: #F6B400; }
"))

# ---------- UI ----------
ui <- page_fluid(
  theme = med_theme, tags$style(med_css),
  
  # Header with anchors
  div(id = "top-nav",
      div(class = "brand", img(src = "logo.png")),  # file at ./www/logo.png
      div(class = "nav-links",
          tags$a(href = "#features",  "Features"),
          tags$a(href = "#visualise", "Visualise"),
          tags$a(href = "#contact",   "Contact")
      )
  ),
  
  # Hero
  div(id = "hero",
      div(class = "hero-row",
          div(class = "hero-left",
              div(class = "hero-copy",
                  div(class = "hero-headline","Data that drives efficiency, savings, and smarter care"),
                  div(class = "hero-sub",
                      "MedHiVE is an intelligent, data-driven dashboard to help nursing homes identify, monitor, and reduce medication waste. ",
                      "By integrating analytics and quality control methods, MedHiVE turns everyday pharmacy data into actionable insights that cut costs and improve operational efficiency.")
              )
          ),
          div(class = "upload-card",
              h4("Upload & Configure Your Data"),
              p(style = "margin-top:0; color:var(--med-muted); font-size:13px;",
                "Import your facility CSVs and set the window and filters for your analysis."),
              tags$hr(),
              fileInput("files", NULL, multiple = TRUE, buttonLabel = "Browse", placeholder = "No file selected", accept = ".csv"),
              dateRangeInput("date", "Date range", start = "2025-08-01", end = "2025-10-31"),
              uiOutput("med_picker"),
              uiOutput("doc_picker"),
              checkboxGroupInput("class_filter", "Filter by cause",
                                 choices = c("preventable","semi-preventable","non-preventable"),
                                 selected = c("preventable","semi-preventable","non-preventable")),
              hr(),
              sliderInput("first_fill_frac", "First-fill fraction (trial supply size)", 0.25, 1.0, 0.5, 0.05),
              checkboxInput("show_points", "Show points on charts", TRUE),
              hr(),
              downloadButton("dl_filtered", "Download filtered data", class = "btn btn-med")
          )
      )
  ),
  
  # -------- Features section (anchor target: #features) --------
  div(id = "features", class = "section band",
      h2("Key Features"),
      div(class = "feat-grid",
          div(class = "feat-card",
              tags$img(src = "icons/analytics.png", height = "40px", style = "margin-bottom:10px;"),
              h4(class = "gold","Smart Analytics"),
              p("Interactive Pareto charts, time trends, and dashboards identify where medication waste occurs most often.")
          ),
          div(class = "feat-card",
              tags$img(src = "icons/alert.png", height = "40px", style = "margin-bottom:10px;"),
              h4(class = "gold","Operational Alerts"),
              p("Flags preventable waste causes such as duplicate prescriptions and unused stock for faster response.")
          ),
          div(class = "feat-card",
              tags$img(src = "icons/simulator.png", height = "40px", style = "margin-bottom:10px;"),
              h4(class = "gold","Policy Simulation Engine"),
              p("Test different supply policies and estimate potential cost savings before real-world implementation.")
          ),
          div(class = "feat-card",
              tags$img(src = "icons/insights.png", height = "40px", style = "margin-bottom:10px;"),
              h4(class = "gold","Quality Insights"),
              p("Six Sigma control checks detect unusual waste patterns for targeted administrative action.")
          )
      )
  ),
  
  # -------- Visualise (main dashboard; anchor target: #visualise) --------
  div(id = "visualise", class = "section",
      div(class = "main-tabs",
          div(class = "kpi-row",
              div(class = "kpi-card", h4("Total waste ($)"), h3(textOutput("kpi_waste_cost"))),
              div(class = "kpi-card", h4("Preventable ($)"), h3(textOutput("kpi_prev_cost"))),
              div(class = "kpi-card", h4("Avg % wasted"), h3(textOutput("kpi_pct"))),
              div(class = "kpi-card", h4("Facilities"), h3(textOutput("kpi_fac")))
          ),
          tabsetPanel(
            tabPanel("Time Series", br(), plotOutput("ts_waste", height = 320)),
            tabPanel("Causes",
                     br(),
                     fluidRow(
                       column(6, plotOutput("cause_stack_med", height = 340)),
                       column(6, plotOutput("cause_stack_doc", height = 340))
                     )),
            tabPanel("Pareto",
                     br(),
                     fluidRow(
                       column(6, plotOutput("pareto_med", height = 320)),
                       column(6, plotOutput("pareto_doc", height = 320))
                     )),
            tabPanel("Distributions",
                     br(),
                     fluidRow(
                       column(6, plotOutput("hist_pct", height = 320)),
                       column(6, plotOutput("hist_cost", height = 320))
                     )),
            tabPanel("Operational Alerts",
                     br(),
                     div(style = "color:var(--med-muted); font-size:13px;",
                         "Flags issues you can influence (policy, prescriber coaching, workflow, transfer handoffs)."),
                     DTOutput("tbl_flags")),
            tabPanel("Policy Simulator",
                     br(),
                     plotOutput("sim_savings_med", height = 340),
                     DTOutput("tbl_sim_detail")),
            tabPanel("Top Prescribers (Waste)", br(), DTOutput("tbl_top_prescribers"))
          )
      )
  ),
  
  # -------- Contact (anchor target: #contact) --------
  div(id = "contact", class = "section band",
      h3("Contact"),
      p("Questions or collaboration? Reach the MedHiVE team:"),
      tags$ul(class = "contact-list",
              tags$li(tags$b("Email: "), tags$a(href="mailto:sc2827@cornell.edu","sc2827@cornell.edu")),
              tags$li(tags$b("GitHub: "), tags$a(href="https://github.com/chatterjee-srinjoy/Control-Freaks-Six-Sigma","github.com/chatterjee-srinjoy/Control-Freaks-Six-Sigma", target="_blank"))
      )
  )
)

# ---------- SERVER ----------
server <- function(input, output, session) {
  
  raw_data <- reactive({
    req(input$files)
    files <- input$files
    bind_rows(lapply(seq_len(nrow(files)), function(i) {
      read_one_csv(files$datapath[i], label = tools::file_path_sans_ext(files$name[i]))
    }))
  })
  
  observeEvent(raw_data(), {
    meds <- raw_data() %>% distinct(medication) %>% arrange(medication) %>% pull()
    docs <- raw_data() %>% distinct(prescriber) %>% arrange(prescriber) %>% pull()
    updateSelectizeInput(session, "meds", choices = meds, server = TRUE)
    updateSelectizeInput(session, "docs", choices = docs, server = TRUE)
  }, ignoreInit = TRUE)
  
  output$med_picker <- renderUI({
    req(raw_data())
    selectizeInput("meds", "Medications", choices = NULL, multiple = TRUE)
  })
  output$doc_picker <- renderUI({
    req(raw_data())
    selectizeInput("docs", "Prescribers", choices = NULL, multiple = TRUE)
  })
  
  filtered <- reactive({
    req(raw_data())
    df <- raw_data() %>%
      filter(scheduled_date >= input$date[1], scheduled_date <= input$date[2])
    if (length(input$meds)) df <- df %>% filter(medication %in% input$meds)
    if (length(input$docs)) df <- df %>% filter(prescriber %in% input$docs)
    df <- df %>% filter(reason_class %in% input$class_filter)
    df
  })
  
  # KPIs
  output$kpi_waste_cost <- renderText({
    req(filtered()); dollar(sum(filtered()$waste_cost, na.rm = TRUE))
  })
  output$kpi_prev_cost <- renderText({
    req(filtered()); dollar(sum(filtered()$waste_cost[filtered()$reason_class == "preventable"], na.rm = TRUE))
  })
  output$kpi_pct <- renderText({
    req(filtered()); percent(mean(filtered()$percent_wasted, na.rm = TRUE)/100)
  })
  output$kpi_fac <- renderText({
    req(filtered()); length(unique(filtered()$facility))
  })
  
  # Time series
  output$ts_waste <- renderPlot({
    req(filtered())
    ts <- filtered() %>%
      mutate(week = floor_date(scheduled_date, "week", week_start = 1)) %>%
      group_by(week) %>%
      summarise(waste_cost = sum(waste_cost, na.rm = TRUE), .groups = "drop")
    req(nrow(ts) > 1)
    lim <- in_control_limits(ts$waste_cost)
    ggplot(ts, aes(week, waste_cost)) +
      geom_line(linewidth = 1, color = med_nav_blue) +
      {if (isTRUE(input$show_points)) geom_point(size = 2, color = med_blue)} +
      geom_hline(yintercept = lim$center, linetype = "dashed", color = med_blue) +
      geom_hline(yintercept = lim$ucl, color = "red3", linetype = "dotted") +
      geom_hline(yintercept = lim$lcl, color = "red3", linetype = "dotted") +
      scale_y_continuous(labels = dollar_format()) +
      labs(x = "week", y = "waste cost (USD)", title = "Weekly waste with 3\u03C3 limits") +
      med_theme_plot()
  })
  
  # Causes
  output$cause_stack_med <- renderPlot({
    req(filtered())
    pdat <- filtered() %>%
      group_by(medication, waste_reason) %>%
      summarise(waste_cost = sum(waste_cost, na.rm = TRUE), .groups = "drop")
    ggplot(pdat, aes(x = reorder(medication, -waste_cost), y = waste_cost, fill = waste_reason)) +
      geom_col() +
      scale_fill_manual(values = med_reason_pal, name = "reason") +
      coord_flip() +
      scale_y_continuous(labels = dollar_format()) +
      labs(x = "medication", y = "waste cost", title = "Waste by reason — medication") +
      med_theme_plot()
  })
  output$cause_stack_doc <- renderPlot({
    req(filtered())
    pdat <- filtered() %>%
      group_by(prescriber, waste_reason) %>%
      summarise(waste_cost = sum(waste_cost, na.rm = TRUE), .groups = "drop")
    ggplot(pdat, aes(x = reorder(prescriber, -waste_cost), y = waste_cost, fill = waste_reason)) +
      geom_col() +
      scale_fill_manual(values = med_reason_pal, name = "reason") +
      coord_flip() +
      scale_y_continuous(labels = dollar_format()) +
      labs(x = "prescriber", y = "waste cost", title = "Waste by reason — prescriber") +
      med_theme_plot()
  })
  
  # Pareto
  output$pareto_med <- renderPlot({
    req(filtered())
    pdat <- filtered() %>%
      group_by(medication) %>%
      summarise(waste_cost = sum(waste_cost, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(waste_cost)) %>%
      mutate(cum_share = cumsum(waste_cost)/sum(waste_cost))
    ggplot(pdat, aes(reorder(medication, waste_cost), waste_cost)) +
      geom_col(fill = med_accent_y, color = med_nav_blue, linewidth = .2) +
      geom_line(aes(y = cum_share * max(waste_cost), group = 1), color = med_nav_blue) +
      geom_point(aes(y = cum_share * max(waste_cost)), color = med_nav_blue) +
      coord_flip() +
      scale_y_continuous(labels = dollar_format(),
                         sec.axis = sec_axis(~ . / max(pdat$waste_cost), labels = percent)) +
      labs(x = "medication", y = "waste cost", title = "Pareto — medication") +
      med_theme_plot()
  })
  output$pareto_doc <- renderPlot({
    req(filtered())
    pdat <- filtered() %>%
      group_by(prescriber) %>%
      summarise(waste_cost = sum(waste_cost, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(waste_cost)) %>%
      mutate(cum_share = cumsum(waste_cost)/sum(waste_cost))
    ggplot(pdat, aes(reorder(prescriber, waste_cost), waste_cost)) +
      geom_col(fill = med_accent_y, color = med_nav_blue, linewidth = .2) +
      geom_line(aes(y = cum_share * max(waste_cost), group = 1), color = med_nav_blue) +
      geom_point(aes(y = cum_share * max(waste_cost)), color = med_nav_blue) +
      coord_flip() +
      scale_y_continuous(labels = dollar_format(),
                         sec.axis = sec_axis(~ . / max(pdat$waste_cost), labels = percent)) +
      labs(x = "prescriber", y = "waste cost", title = "Pareto — prescriber") +
      med_theme_plot()
  })
  
  # Distributions
  output$hist_pct <- renderPlot({
    req(filtered())
    ggplot(filtered(), aes(percent_wasted)) +
      geom_histogram(bins = 20, boundary = 0, closed = "left",
                     fill = med_accent_y, color = med_nav_blue) +
      scale_x_continuous(labels = function(x) paste0(x, "%")) +
      labs(x = "percent wasted", y = "count", title = "Distribution of percent wasted") +
      med_theme_plot()
  })
  output$hist_cost <- renderPlot({
    req(filtered())
    ggplot(filtered(), aes(waste_cost)) +
      geom_histogram(bins = 20, boundary = 0, closed = "left",
                     fill = med_blue, color = med_nav_blue) +
      scale_x_continuous(labels = dollar_format()) +
      labs(x = "waste cost (USD)", y = "count", title = "Distribution of waste cost") +
      med_theme_plot()
  })
  
  # Operational Alerts
  flags_tbl <- reactive({
    req(filtered())
    df <- filtered()
    thr_med_prev_share <- 0.30
    thr_doc_prev_rate_margin <- 0.10
    
    med_prev <- df %>%
      group_by(medication) %>%
      summarise(prev_waste = sum(waste_cost[reason_class=="preventable"], na.rm = TRUE),
                total_waste = sum(waste_cost, na.rm = TRUE), .groups = "drop") %>%
      mutate(share = if_else(total_waste > 0, prev_waste/total_waste, 0)) %>%
      filter(share >= thr_med_prev_share & total_waste > 0) %>%
      transmute(type = "Policy",
                target = medication,
                message = paste0("High preventable share (", percent(share), "). Trial shorter first fill."),
                est_savings = prev_waste * 0.30)
    
    doc_prev <- df %>%
      mutate(prev_flag = reason_class=="preventable") %>%
      group_by(prescriber) %>%
      summarise(rate = mean(prev_flag, na.rm = TRUE), n = n(), .groups="drop") %>%
      filter(n >= 20)
    peer_med <- median(doc_prev$rate, na.rm = TRUE)
    doc_flags <- doc_prev %>%
      filter(rate >= peer_med + thr_doc_prev_rate_margin) %>%
      transmute(type = "Prescriber",
                target = prescriber,
                message = paste0("Preventable waste rate ", percent(rate),
                                 " above peer median ", percent(peer_med), " + margin."),
                est_savings = NA_real_)
    
    dec_cases <- df %>%
      filter(waste_reason=="Deceased", waste_units > 0) %>%
      transmute(type = "Workflow",
                target = facility,
                message = "Waste after death. Ensure auto-halt on refills and rapid EHR notification.",
                est_savings = sum(waste_cost, na.rm = TRUE)) %>%
      distinct(type, target, message, .keep_all = TRUE)
    
    xfer <- df %>%
      filter(waste_reason=="Transfer to hospital") %>%
      group_by(facility) %>%
      summarise(cost = sum(waste_cost, na.rm = TRUE), .groups="drop") %>%
      filter(cost > 0) %>%
      transmute(type = "Transfer",
                target = facility,
                message = "Transfer-related waste. Consider partial fills and tighter handoffs.",
                est_savings = cost * 0.20)
    
    bind_rows(med_prev, doc_flags, dec_cases, xfer) %>%
      arrange(desc(coalesce(est_savings, 0)))
  })
  output$tbl_flags <- renderDT({
    req(flags_tbl())
    dat <- flags_tbl() %>% mutate(est_savings = ifelse(is.na(est_savings), "", dollar(est_savings)))
    datatable(dat, options = list(pageLength = 12, scrollX = TRUE))
  })
  
  # Policy Simulator
  sim_detail <- reactive({
    req(filtered())
    frac <- input$first_fill_frac
    df <- filtered() %>% filter(reason_class == "preventable")
    if (nrow(df) == 0) return(tibble())
    init_prime <- pmax(1, round(df$initial_weight * frac))
    reducible  <- pmax(0, df$initial_weight - init_prime)
    units_saved <- pmin(df$waste_units, reducible)
    df %>%
      mutate(first_fill_frac = frac,
             init_prime = init_prime,
             units_saved = units_saved,
             saving = units_saved * unit_cost)
  })
  output$sim_savings_med <- renderPlot({
    req(sim_detail(), nrow(sim_detail()) > 0)
    agg <- sim_detail() %>%
      group_by(medication) %>%
      summarise(savings = sum(saving, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(savings)) %>%
      slice_head(n = 12)
    ggplot(agg, aes(reorder(medication, savings), savings)) +
      geom_col(fill = med_accent_y, color = med_nav_blue) +
      coord_flip() +
      scale_y_continuous(labels = dollar_format()) +
      labs(x = "medication", y = "estimated savings",
           title = paste0("Estimated savings by medication (first-fill \u00D7 ", input$first_fill_frac, ")")) +
      med_theme_plot()
  })
  output$tbl_sim_detail <- renderDT({
    req(sim_detail(), nrow(sim_detail()) > 0)
    dat <- sim_detail() %>%
      transmute(facility, prescription_id, medication, prescriber,
                initial_weight, init_prime, units_saved,
                unit_cost = dollar(unit_cost),
                saving = dollar(saving),
                waste_reason)
    datatable(dat, options = list(pageLength = 12, scrollX = TRUE))
  })
  
  # Top Prescribers
  output$tbl_top_prescribers <- renderDT({
    req(filtered())
    dat <- filtered() %>%
      group_by(prescriber) %>%
      summarise(
        preventable_waste = sum(waste_cost[reason_class=="preventable"], na.rm = TRUE),
        total_waste = sum(waste_cost, na.rm = TRUE),
        preventable_share = if_else(total_waste>0, preventable_waste/total_waste, 0),
        n_scripts = n(),
        .groups = "drop"
      ) %>%
      arrange(desc(preventable_waste)) %>%
      mutate(
        preventable_waste = dollar(preventable_waste),
        total_waste = dollar(total_waste),
        preventable_share = percent(preventable_share)
      )
    datatable(dat, options = list(pageLength = 15, scrollX = TRUE))
  })
  
  # Download
  output$dl_filtered <- downloadHandler(
    filename = function() paste0("medhive_filtered_", Sys.Date(), ".csv"),
    content = function(file) { readr::write_csv(filtered(), file) }
  )
}

shinyApp(ui, server)