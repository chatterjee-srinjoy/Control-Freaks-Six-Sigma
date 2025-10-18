# app.R — MedHive Themed Actionable Dashboard (clean wording)
# CSV schema:
# prescription_id, patient_id, medication, scheduled_date, refill_date,
# cost_usd, prescriber, initial_weight, weight_at_stop, percent_wasted, waste_reason

suppressPackageStartupMessages({
  library(shiny)
  library(tidyverse)
  library(lubridate)
  library(scales)
  library(DT)
  library(janitor)
  library(bslib)
})

# ---------- Theme colors ----------
bee_yellow <- "#F6C90E"
honey_gold <- "#FFC107"
bee_black  <- "#222222"
wax_cream  <- "#FFF8D6"
nectar     <- "#FFB703"
bee_blue   <- "#2A9D8F"

bee_fill_pal <- c(
  "Medication change"     = "#0072B2",  # strong blue
  "Non-adherence"         = "#56B4E9",  # sky blue
  "Administrative error"  = "#E69F00",  # orange
  "Lost medication"       = "#CC79A7",  # magenta
  "Expired stock"         = "#009E73",  # green
  "Transfer to hospital"  = "#F0E442",  # yellow-gold accent
  "Deceased"              = "#999999",  # gray
  "Other"                 = "#8B4513",  # brown
  "No waste"              = "#444444"   # dark gray
)

bee_theme_plot <- function() {
  theme_minimal(base_family = "system-ui", base_size = 12) +
    theme(
      plot.background = element_rect(fill = "white", color = NA),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_line(color = "#f0f0f0"),
      panel.grid.major.y = element_line(color = "#f0f0f0"),
      axis.title = element_text(color = bee_black),
      plot.title = element_text(face = "bold", color = bee_black),
      plot.subtitle = element_text(color = "#555"),
      legend.position = "bottom"
    )
}

# classify cause
reason_class <- function(reason) {
  dplyr::case_when(
    reason %in% c("Medication change","Non-adherence","Administrative error","Lost medication","Expired stock") ~ "preventable",
    reason %in% c("Transfer to hospital") ~ "semi-preventable",
    reason %in% c("Deceased","No waste","Other") ~ "non-preventable",
    TRUE ~ "unknown"
  )
}

# data ingest
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

# ---------- Theming ----------
bee_theme <- bs_theme(
  version = 5,
  base_font = font_google("Nunito", local = TRUE),
  heading_font = font_google("Nunito", local = TRUE),
  primary = bee_yellow,
  body_bg = "white",
  text = bee_black
)

bee_css <- HTML(paste0(
  "
:root {
  --bee-yellow: ", bee_yellow, ";
  --honey-gold: ", honey_gold, ";
  --bee-black: ", bee_black, ";
  --wax-cream: ", wax_cream, ";
}
#bee-banner {
  background: repeating-linear-gradient(60deg, #fff6bf 0px, #fff6bf 10px, #ffe985 10px, #ffe985 20px);
  border-bottom: 4px solid ", bee_yellow, ";
  padding: 18px 16px; margin-bottom: 16px; border-radius: 0 0 16px 16px;
}
.kpi-card {
  background: ", wax_cream, ";
  border: 2px solid ", honey_gold, ";
  border-radius: 16px; padding: 12px 14px;
  box-shadow: 0 2px 0 rgba(0,0,0,0.05);
}
.nav-tabs .nav-link.active { background: ", honey_gold, "; color: ", bee_black, "; font-weight: 700; }
.btn-primary { background: ", honey_gold, "; border-color: ", honey_gold, "; color: ", bee_black, "; font-weight: 700; }
.small-muted { color:#666; font-size: 12px; }
"))

# ---------- UI ----------
ui <- page_fluid(
  theme = bee_theme, tags$style(bee_css),
  
  div(id = "bee-banner",
      h3("MedHive: Medication Waste Dashboard")),
  
  sidebarLayout(
    sidebarPanel(
      h4("Inputs"),
      fileInput("files", "Upload CSVs (with waste_reason)", multiple = TRUE, accept = ".csv"),
      dateRangeInput("date", "Date range",
                     start = "2025-08-01", end = "2025-10-31"),
      uiOutput("med_picker"),
      uiOutput("doc_picker"),
      checkboxGroupInput("class_filter", "Filter by cause",
                         choices = c("preventable","semi-preventable","non-preventable"),
                         selected = c("preventable","semi-preventable","non-preventable")),
      hr(),
      sliderInput("first_fill_frac", "First-fill fraction (trial supply size)",
                  min = 0.25, max = 1.00, value = 0.5, step = 0.05),
      checkboxInput("show_points", "Show points on charts", TRUE),
      hr(),
      downloadButton("dl_filtered", "Download filtered data", class = "btn btn-primary")
    ),
    
    mainPanel(
      fluidRow(
        column(3, div(class="kpi-card",
                      h4("Total waste ($)"), h3(textOutput("kpi_waste_cost")))),
        column(3, div(class="kpi-card",
                      h4("Preventable ($)"), h3(textOutput("kpi_prev_cost")))),
        column(3, div(class="kpi-card",
                      h4("Avg % wasted"), h3(textOutput("kpi_pct")))),
        column(3, div(class="kpi-card",
                      h4("Facilities"), h3(textOutput("kpi_fac"))))
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
                 div(class="small-muted",
                     "Flags issues you can influence (policy, prescriber coaching, workflow, transfer handoffs)."),
                 DTOutput("tbl_flags")),
        tabPanel("Policy Simulator",
                 br(),
                 plotOutput("sim_savings_med", height = 340),
                 DTOutput("tbl_sim_detail")),
        tabPanel("Top Prescribers (Waste)", br(), DTOutput("tbl_top_prescribers"))
      )
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
      geom_line(linewidth = 1, color = bee_black) +
      {if (isTRUE(input$show_points)) geom_point(size = 2, color = bee_yellow)} +
      geom_hline(yintercept = lim$center, linetype = "dashed", color = bee_blue) +
      geom_hline(yintercept = lim$ucl, color = "red3", linetype = "dotted") +
      geom_hline(yintercept = lim$lcl, color = "red3", linetype = "dotted") +
      scale_y_continuous(labels = dollar_format()) +
      labs(x = "week", y = "waste cost (USD)",
           title = "Weekly waste with 3σ limits") +
      bee_theme_plot()
  })
  
  # Causes
  output$cause_stack_med <- renderPlot({
    req(filtered())
    pdat <- filtered() %>%
      group_by(medication, waste_reason) %>%
      summarise(waste_cost = sum(waste_cost, na.rm = TRUE), .groups = "drop")
    ggplot(pdat, aes(x = reorder(medication, -waste_cost), y = waste_cost, fill = waste_reason)) +
      geom_col() +
      scale_fill_manual(values = bee_fill_pal, name = "reason") +
      coord_flip() +
      scale_y_continuous(labels = dollar_format()) +
      labs(x = "medication", y = "waste cost", title = "Waste by reason — medication") +
      bee_theme_plot()
  })
  output$cause_stack_doc <- renderPlot({
    req(filtered())
    pdat <- filtered() %>%
      group_by(prescriber, waste_reason) %>%
      summarise(waste_cost = sum(waste_cost, na.rm = TRUE), .groups = "drop")
    ggplot(pdat, aes(x = reorder(prescriber, -waste_cost), y = waste_cost, fill = waste_reason)) +
      geom_col() +
      scale_fill_manual(values = bee_fill_pal, name = "reason") +
      coord_flip() +
      scale_y_continuous(labels = dollar_format()) +
      labs(x = "prescriber", y = "waste cost", title = "Waste by reason — prescriber") +
      bee_theme_plot()
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
      geom_col(fill = bee_yellow, color = bee_black, linewidth = .2) +
      geom_line(aes(y = cum_share * max(waste_cost), group = 1), color = bee_black) +
      geom_point(aes(y = cum_share * max(waste_cost)), color = bee_black) +
      coord_flip() +
      scale_y_continuous(labels = dollar_format(),
                         sec.axis = sec_axis(~ . / max(pdat$waste_cost), labels = percent)) +
      labs(x = "medication", y = "waste cost", title = "Pareto — medication") +
      bee_theme_plot()
  })
  output$pareto_doc <- renderPlot({
    req(filtered())
    pdat <- filtered() %>%
      group_by(prescriber) %>%
      summarise(waste_cost = sum(waste_cost, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(waste_cost)) %>%
      mutate(cum_share = cumsum(waste_cost)/sum(waste_cost))
    ggplot(pdat, aes(reorder(prescriber, waste_cost), waste_cost)) +
      geom_col(fill = bee_yellow, color = bee_black, linewidth = .2) +
      geom_line(aes(y = cum_share * max(waste_cost), group = 1), color = bee_black) +
      geom_point(aes(y = cum_share * max(waste_cost)), color = bee_black) +
      coord_flip() +
      scale_y_continuous(labels = dollar_format(),
                         sec.axis = sec_axis(~ . / max(pdat$waste_cost), labels = percent)) +
      labs(x = "prescriber", y = "waste cost", title = "Pareto — prescriber") +
      bee_theme_plot()
  })
  
  # Distributions
  output$hist_pct <- renderPlot({
    req(filtered())
    ggplot(filtered(), aes(percent_wasted)) +
      geom_histogram(bins = 20, boundary = 0, closed = "left",
                     fill = bee_yellow, color = bee_black) +
      scale_x_continuous(labels = function(x) paste0(x, "%")) +
      labs(x = "percent wasted", y = "count", title = "Distribution of percent wasted") +
      bee_theme_plot()
  })
  output$hist_cost <- renderPlot({
    req(filtered())
    ggplot(filtered(), aes(waste_cost)) +
      geom_histogram(bins = 20, boundary = 0, closed = "left",
                     fill = honey_gold, color = bee_black) +
      scale_x_continuous(labels = dollar_format()) +
      labs(x = "waste cost (USD)", y = "count", title = "Distribution of waste cost") +
      bee_theme_plot()
  })
  
  # Operational Alerts (flags)
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
  
  # Policy Simulator (shorter first fill)
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
      geom_col(fill = nectar, color = bee_black) +
      coord_flip() +
      scale_y_continuous(labels = dollar_format()) +
      labs(x = "medication", y = "estimated savings",
           title = paste0("Estimated savings by medication (first-fill ×", input$first_fill_frac, ")")) +
      bee_theme_plot()
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
  
  # Top Prescribers (aggregated)
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
  
  # download
  output$dl_filtered <- downloadHandler(
    filename = function() paste0("medhive_filtered_", Sys.Date(), ".csv"),
    content = function(file) { readr::write_csv(filtered(), file) }
  )
}

shinyApp(ui, server)