# app.R
library(shiny)
library(ggplot2)

# ---- 1) Intervention table (EDIT: costs + effects) ----
# Effects are expressed as:
#  - d_diag: absolute change in diagnosed fraction among PLHIV (e.g., +0.01 = +1 percentage point)
#  - d_art:  absolute change in ART fraction among diagnosed
#  - d_supp: absolute change in suppression fraction among on ART
#  - r_inf_adult: proportional change in adult infections (negative reduces infections)
#  - r_inf_infant: proportional change in infant infections
#  - r_death: proportional change in mortality/deaths

#do we need to add TB treatment?
components <- data.frame(
  id = c(
    "art_provision","mmd","dsd","cotrim","art_init",
    "vl_targeted","vl_dsd","vl_routine",
    "oi_tb_test","oi_tb_tpt","oi_crag_test","oi_crypto_tx",
    "ahd_lam","ahd_crag","ahd_cd4","ahd_flucon",
    "tracking_total","psychosocial",
    "test_facility","test_community","test_kpsti","hivst",
    "infant_proph","pep","condoms","prep_oral","prep_cabla","prep_len",
    "harm_reduction","vmmc",
    "anc_hiv_test","eID"
  ),
  area = c(
    rep("TREATMENT", 18),
    rep("TESTING", 4),
    rep("PREVENTION", 8),
    rep("ANC/PMTCT", 2)
  ),
  label = c(
    "ART provision for all",
    "MMD (multi-month dispensing)",
    "DSD (differentiated service delivery)",
    "Cotrimoxazole prophylaxis",
    "ART initiation (and re-initiation)",
    "VL monitoring: suspected failure",
    "VL for DSD initiation",
    "Routine VL monitoring",
    "OI: TB testing for symptomatic",
    "OI: TB preventive therapy (TPT)",
    "OI: CRAG testing",
    "OI: Cryptococcal treatment",
    "AHD: LAM for Stage 3/4 / seriously ill",
    "AHD: CRAG for Stage 3/4 / IPD",
    "AHD: CD4 testing",
    "AHD: Pre-emptive fluconazole",
    "Tracking & tracing (total)",
    "Psychosocial support / counselling",
    "Testing: facility-based",
    "Testing: community-based",
    "Testing: key populations & STI services",
    "HIV self-testing (HIVST)",
    "Infant prophylaxis",
    "PEP",
    "Condom availability",
    "PrEP (oral)",
    "PrEP (CAB-LA)",
    "PrEP (Lenacapavir)",
    "Harm reduction (PWID)",
    "VMMC",
    "ANC: HIV testing",
    "EID (early infant diagnosis)"
  ),
  target_pop = c(
    rep("plhiv", 18),
    rep("adult", 4),
    rep("adult", 8),
    rep("pregnant", 1),
    rep("infant", 1)
  ),
  unit_cost = c(
    # placeholders ($/person-year fully covered)
    90,12,18,6,25,10,8,15,20,22,5,35,25,8,12,10,6,8,
    5,7,15,4,
    8,6,2,25,120,180,30,20,
    6,35
  ),
  # ---- Placeholder effect sizes (REPLACE later) ----
  d_diag = c(
    0.000,0.000,0.000,0.000,0.002,  0,0,0, 0,0,0,0, 0,0,0,0, 0.000,0.000,
    0.010,0.015,0.008,0.012,
    0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000,
    0.008,0.000
  ),
  d_art = c(
    0.010,0.006,0.008,0.002,0.010,  0,0,0, 0,0,0,0, 0,0,0,0, 0.004,0.004,
    0.000,0.000,0.000,0.000,
    0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000,
    0.000,0.000
  ),
  d_supp = c(
    0.012,0.006,0.010,0.002,0.004,  0.006,0.004,0.008, 0.001,0.001,0.001,0.002,
    0.004,0.002,0.003,0.002, 0.002,0.003,
    0.000,0.000,0.000,0.000,
    0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000,
    0.002,0.000
  ),
  r_inf_adult = c(
    -0.020,0.000,0.000,0.000,-0.010,  0,0,0, 0,0,0,0, 0,0,0,0, -0.002,0.000,
    -0.010,-0.012,-0.006,-0.009,
    0.000,-0.002,-0.006,-0.010,-0.012,-0.014,-0.003,-0.006,
    0.000,0.000
  ),
  r_inf_infant = c(
    0,0,0,0,0, 0,0,0, 0,0,0,0, 0,0,0,0, 0,0,
    0,0,0,0,
    -0.030,0,0,0,0,0,0,0,
    -0.020,-0.020
  ),
  r_death = c(
    -0.010,-0.002,-0.003,-0.002,-0.006,
    -0.002,-0.001,-0.002,
    -0.004,-0.003,-0.001,-0.004,
    -0.006,-0.003,-0.002,-0.002,
    -0.001,-0.001,
    -0.0005,-0.0005,-0.0005,-0.0005,
    -0.002,-0.0005,-0.0005,-0.001,-0.001,-0.0012,-0.0005,-0.001,
    -0.001,-0.002
  ),
  stringsAsFactors = FALSE
)

clamp01 <- function(x) pmin(pmax(x, 0), 1)
fmt_money <- function(x) paste0("$", format(round(x, 0), big.mark=","))

# ---- 2) UI ----
ui <- fluidPage(
  titlePanel("HIV programme toggles → 95–95–95 + absolute outcomes + budget (mock-up)"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Country baseline"),
      numericInput("plhiv", "People living with HIV (PLHIV)", value = 200000, min = 0),
      numericInput("pTB_HIV", "HIV/TB co-infection rate (%)", value = 25, min = 0,max=100),
      
      tags$hr(),
      h4("Baseline cascade counts (annual / current)"),
      numericInput("base_diag", "Diagnosed (count)", value = 170000, min = 0),
      numericInput("base_art", "On treatment / ART (count)", value = 150000, min = 0),
      numericInput("base_supp", "Virally suppressed (count)", value = 135000, min = 0),
      
      tags$hr(),
      h4("Baseline annual outcomes (absolute)"),
      numericInput("base_inf_adult", "Adult infections (annual)", value = 8000, min = 0),
      numericInput("base_inf_infant", "Infant infections (annual)", value = 400, min = 0),
      numericInput("base_deaths", "HIV-related deaths (annual)", value = 2500, min = 0),
      
      tags$hr(),
      h4("Population denominators for costing"),
      numericInput("pop_adult", "Adult population reached by adult-facing services", value = 1e6, min = 0),
      numericInput("pop_pregnant", "Pregnant population (ANC)", value = 80000, min = 0),
      numericInput("pop_infant", "Infant population", value = 50000, min = 0),
      numericInput("pop_plhiv_cost", "PLHIV eligible population for PLHIV services", value = 200000, min = 0),
      
      tags$hr(),
      checkboxInput("show_details", "Show per-component budget table", TRUE),
      actionButton("reset_all", "Reset: turn everything OFF", class = "btn-warning")
    ),
    
    mainPanel(
      fluidRow(
        column(4, wellPanel(h4("Total budget"), textOutput("budget_total", inline = TRUE))),
        column(4, wellPanel(h4("95–95–95"), textOutput("cascade_text", inline = TRUE))),
        column(4, wellPanel(h4("Deaths (annual)"), textOutput("deaths_abs", inline = TRUE)))
      ),
      fluidRow(
        column(6, plotOutput("cascade_plot", height = 260)),
        column(6, plotOutput("outcomes_plot", height = 260))
      ),
      
      tags$hr(),
      h3("Programme components"),
      uiOutput("component_controls"),
      
      tags$hr(),
      conditionalPanel(
        condition = "input.show_details == true",
        h4("Per-component budget (debug)"),
        tableOutput("details_table")
      )
    )
  )
)

# ---- 3) Server ----
server <- function(input, output, session) {
  
  # Dynamic ON/OFF + coverage sliders
  output$component_controls <- renderUI({
    areas <- unique(components$area)
    
    tagList(lapply(areas, function(a) {
      subset <- components[components$area == a, , drop = FALSE]
      tagList(
        h4(a),
        div(style="padding-left: 8px;",
            lapply(seq_len(nrow(subset)), function(i) {
              row <- subset[i, ]
              enable_id <- paste0("on__", row$id)
              cov_id <- paste0("cov__", row$id)
              cond <- sprintf("input['%s'] == true", enable_id)
              
              div(style="margin-bottom: 10px; padding: 8px; border: 1px solid #eee; border-radius: 6px;",
                  strong(row$label),
                  br(),
                  checkboxInput(enable_id, "On", value = FALSE),
                  conditionalPanel(
                    condition = cond,
                    sliderInput(cov_id, "Coverage (%)", min = 0, max = 100, value = 50, step = 1)
                  )
              )
            })
        ),
        hr()
      )
    }))
  })
  
  observeEvent(input$reset_all, {
    lapply(components$id, function(cid) {
      updateCheckboxInput(session, paste0("on__", cid), value = FALSE)
    })
  })
  
  target_denominator <- function(target_pop) {
    switch(target_pop,
           adult = input$pop_adult,
           infant = input$pop_infant,
           pregnant = input$pop_pregnant,
           plhiv = input$pop_plhiv_cost,
           input$pop_adult)
  }
  
  settings_df <- reactive({
    df <- components
    
    df$on <- vapply(df$id, function(cid) isTRUE(input[[paste0("on__", cid)]]), logical(1))
    df$coverage <- vapply(df$id, function(cid) {
      if (isTRUE(input[[paste0("on__", cid)]])) as.numeric(input[[paste0("cov__", cid)]]) / 100 else 0
    }, numeric(1))
    
    df$denom <- vapply(df$target_pop, target_denominator, numeric(1))
    df$budget <- df$coverage * df$denom * df$unit_cost
    
    # Aggregate effects (linear placeholder)
    df$dd_diag <- df$coverage * df$d_diag
    df$dd_art  <- df$coverage * df$d_art
    df$dd_supp <- df$coverage * df$d_supp
    
    df$dr_inf_adult  <- df$coverage * df$r_inf_adult
    df$dr_inf_infant <- df$coverage * df$r_inf_infant
    df$dr_death      <- df$coverage * df$r_death
    
    df
  })
  
  totals <- reactive({
    df <- settings_df()
    
    # ---- Cascade baseline proportions ----
    plhiv <- max(input$plhiv, 0)
    
    base_diag <- min(max(input$base_diag, 0), plhiv)
    base_art  <- min(max(input$base_art, 0), base_diag)
    base_supp <- min(max(input$base_supp, 0), base_art)
    
    p_diag0 <- if (plhiv > 0) base_diag / plhiv else 0
    p_art0  <- if (base_diag > 0) base_art / base_diag else 0
    p_supp0 <- if (base_art > 0) base_supp / base_art else 0
    
    # ---- Apply intervention deltas (absolute p.p. changes) ----
    p_diag1 <- clamp01(p_diag0 + sum(df$dd_diag, na.rm = TRUE))
    p_art1  <- clamp01(p_art0  + sum(df$dd_art,  na.rm = TRUE))
    p_supp1 <- clamp01(p_supp0 + sum(df$dd_supp, na.rm = TRUE))
    
    # Convert back to counts with logical nesting
    diag1 <- round(plhiv * p_diag1)
    art1  <- round(diag1 * p_art1)
    supp1 <- round(art1  * p_supp1)
    
    # ---- Absolute outcomes (apply proportional changes to baseline) ----
    inf_adult0  <- max(input$base_inf_adult, 0)
    inf_infant0 <- max(input$base_inf_infant, 0)
    deaths0     <- max(input$base_deaths, 0)
    
    r_inf_adult  <- sum(df$dr_inf_adult,  na.rm = TRUE)
    r_inf_infant <- sum(df$dr_inf_infant, na.rm = TRUE)
    r_death      <- sum(df$dr_death,      na.rm = TRUE)
    
    inf_adult1  <- max(round(inf_adult0  * (1 + r_inf_adult)),  0)
    inf_infant1 <- max(round(inf_infant0 * (1 + r_inf_infant)), 0)
    deaths1     <- max(round(deaths0     * (1 + r_death)),      0)
    
    list(
      budget = sum(df$budget, na.rm = TRUE),
      plhiv = plhiv,
      diag = diag1, art = art1, supp = supp1,
      p1 = p_diag1, p2 = p_art1, p3 = p_supp1,
      inf_adult = inf_adult1,
      inf_infant = inf_infant1,
      deaths = deaths1
    )
  })
  
  # ---- Topline outputs ----
  output$budget_total <- renderText(fmt_money(totals()$budget))
  
  output$cascade_text <- renderText({
    t <- totals()
    paste0(
      round(100 * t$p1, 1), "% – ",
      round(100 * t$p2, 1), "% – ",
      round(100 * t$p3, 1), "%"
    )
  })
  
  output$deaths_abs <- renderText({
    format(totals()$deaths, big.mark = ",")
  })
  
  # ---- Plots ----
  output$cascade_plot <- renderPlot({
    t <- totals()
    df <- data.frame(
      stage = factor(c("Diagnosed", "On ART", "Suppressed"),
                     levels = c("Diagnosed", "On ART", "Suppressed")),
      count = c(t$diag, t$art, t$supp)
    )
    ggplot(df, aes(x = stage, y = count)) +
      geom_col() +
      labs(
        title = "Cascade counts (resulting)",
        subtitle = paste0("PLHIV = ", format(t$plhiv, big.mark=",")),
        x = NULL, y = "People"
      ) +
      scale_y_continuous(labels = function(x) format(x, big.mark=",")) +
      theme_minimal(base_size = 12)
  })
  
  output$outcomes_plot <- renderPlot({
    t <- totals()
    df <- data.frame(
      outcome = factor(c("Adult infections", "Infant infections", "Deaths"),
                       levels = c("Adult infections", "Infant infections", "Deaths")),
      value = c(t$inf_adult, t$inf_infant, t$deaths)
    )
    ggplot(df, aes(x = outcome, y = value)) +
      geom_col() +
      labs(title = "Annual outcomes (absolute)", x = NULL, y = "People / events") +
      scale_y_continuous(labels = function(x) format(x, big.mark=",")) +
      theme_minimal(base_size = 12)
  })
  
  # ---- Details table ----
  output$details_table <- renderTable({
    df <- settings_df()
    out <- df[, c("area","label","on","coverage","target_pop","unit_cost","denom","budget")]
    out$coverage <- paste0(round(100*out$coverage, 0), "%")
    out$unit_cost <- fmt_money(out$unit_cost)
    out$denom <- format(round(out$denom, 0), big.mark = ",")
    out$budget <- fmt_money(out$budget)
    out
  }, striped = TRUE, bordered = TRUE, spacing = "s")
}

shinyApp(ui, server)
