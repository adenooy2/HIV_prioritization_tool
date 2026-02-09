# ============================================================================
# HIV Intervention Impact Calculator - R Shiny Application
# ============================================================================
# This tool allows users to model the health and cost impacts of scaling
# HIV interventions up or down across prevention, testing, and treatment.
# ============================================================================

library(shiny)
library(bslib)
library(DT)
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
library(httr)
library(readr)
library(readxl)
library(devtools)

#######Things to fix
#Fix baseline infant infections *0.15 in scenarios
#baseline assumption viral suppression


##Source logic file
tryCatch(
  {
    # Source logic – personal/local
    source("/Users/adenooy/Library/CloudStorage/OneDrive-Personal/AMC/HIV Prioritization tool/HIV_prioritization_tool/code/MockUps/MVP/Mock-Up logic.R")
    message("Sourced local file successfully.")
  },
  error = function(e) {
    message("Local source failed. Trying GitHub version...")
    
    tryCatch(
      {
        source("https://raw.githubusercontent.com/adenooy2/HIV_prioritization_tool/refs/heads/main/code/MockUps/MVP/Mock-up%20TIER%20interface%20MVP.R")
        message("Sourced GitHub file successfully.")
      },
      error = function(e2) {
        stop("Both local and GitHub sources failed:\n",
             "Local error: ", e$message, "\n",
             "GitHub error: ", e2$message)
      }
    )
  }
)


# ============================================================================

# ============================================================================
# USER INTERFACE
# ============================================================================

ui <- page_sidebar(
  title = "HIV Intervention Impact Calculator",
  sidebar = sidebar(
    width = 300,
    selectInput(
      "region",
      "Select Regional Profile:",
      choices = names(regional_presets),
      selected = "Eastern Africa - High Prevalence"
    ),
    hr(),
    h5("Epidemic Parameters"),
    numericInput("total_pop", "Total Population:", value = 5000000, min = 0),
    numericInput("prevalence", "HIV Prevalence (%):", value = 4.5, min = 0, max = 100, step = 0.1),
    numericInput("new_infections", "New Infections/Year:", value = 8500, min = 0),
    numericInput("current_dx", "Current Diagnoses/Year:", value = 7000, min = 0),
    numericInput("pct_diagnosed", "% of PLHIV Diagnosed:", value = 85, min = 0, max = 100),  
    numericInput("pct_on_art", "% Diagnosed on ART:", value = 78, min = 0, max = 100),
    numericInput("pct_suppressed", "% on ART Suppressed:", value = 82, min = 0, max = 100),
    numericInput("aids_deaths", "AIDS Deaths/Year:", value = 2200, min = 0)
  ),
  
  navset_card_tab(
    nav_panel(
      "Baseline Coverage",
      uiOutput("baseline_ui")
    ),
    
    nav_panel(
      "Scenarios",
      h4("Adjust intervention coverage for two scenarios"),
      p(strong("Note:"), " Scale up (increase) or scale down (decrease) interventions. Clear labels show whether inputs are absolute numbers (people) or percentages (%)."),
      uiOutput("scenario_ui")
    ),
    
    nav_panel(
      "Results Comparison",
      div(
        style = "height: 80vh; overflow-y: auto; padding-right: 15px;",
        
        # 95-95-95 Goals Tracker
        h3("Progress Toward 95-95-95 Goals", class = "mt-3 mb-3"),
        layout_columns(
          col_widths = c(4, 4, 4),
          card(
            card_header(class = "bg-secondary text-white", "Baseline"),
            card_body(uiOutput("goals_baseline"))
          ),
          card(
            card_header(class = "bg-primary text-white", "Scenario 1"),
            card_body(uiOutput("goals_scenario1"))
          ),
          card(
            card_header(class = "bg-danger text-white", "Scenario 2"),
            card_body(uiOutput("goals_scenario2"))
          )
        ),
        
        # Epidemiological Outcomes Scorecard
        h3("Key Epidemiological Outcomes", class = "mt-4 mb-3"),
        layout_columns(
          col_widths = c(4, 4, 4),
          card(
            card_header(class = "bg-secondary text-white", "Baseline"),
            card_body(uiOutput("epi_baseline"))
          ),
          card(
            card_header(class = "bg-primary text-white", "Scenario 1"),
            card_body(uiOutput("epi_scenario1"))
          ),
          card(
            card_header(class = "bg-danger text-white", "Scenario 2"),
            card_body(uiOutput("epi_scenario2"))
          )
        ),
        
        # Health Outcomes Row
        h3("Health Outcomes (relative to baseline)", class = "mt-4 mb-3"),
        layout_columns(
          col_widths = c(6, 6),
          card(
            card_header(class = "bg-primary text-white", "Scenario 1 - Health Outcomes"),
            card_body(uiOutput("results_scenario1_health"))
          ),
          card(
            card_header(class = "bg-primary text-white", "Scenario 2 - Health Outcomes"),
            card_body(uiOutput("results_scenario2_health"))
          )
        ),
        
        # Cost Analysis Row
        h3("Cost Analysis", class = "mt-4 mb-3"),
        layout_columns(
          col_widths = c(6, 6),
          card(
            card_header(class = "bg-info text-white", "Scenario 1 - Cost Analysis"),
            card_body(uiOutput("results_scenario1_cost"))
          ),
          card(
            card_header(class = "bg-info text-white", "Scenario 2 - Cost Analysis"),
            card_body(uiOutput("results_scenario2_cost"))
          )
        ),
        
        # Cascade Chart - Combined
        h3("HIV Care Cascade: Baseline vs Scenarios", class = "mt-4 mb-3"),
        card(
          card_header("Combined Cascade Comparison"),
          card_body(plotOutput("cascade_combined", height = "500px"))
        ),
        
        # Other Outcomes Row
        h3("Key Outcomes Summary", class = "mt-4 mb-3"),
        layout_columns(
          col_widths = c(6, 6),
          card(
            card_header("Scenario 1 - Impact Summary"),
            card_body(plotOutput("plot_scenario1", height = "400px"))
          ),
          card(
            card_header("Scenario 2 - Impact Summary"),
            card_body(plotOutput("plot_scenario2", height = "400px"))
          )
        ),
        
        # Add some bottom padding
        div(style = "height: 50px;")
      )
    )
  )
)

# ============================================================================
# SERVER LOGIC
# ============================================================================

server <- function(input, output, session) {
  
  # Store original baseline to scale proportionally
  original_population <- reactiveVal(5000000)
  original_baseline <- reactiveVal(NULL)
  
  # Store demographic parameters (will be populated from selected region's CSV data)
  demographic_params <- reactiveValues(
    birth_rate = NULL,
    prop_pop_male = NULL,
    prop_pop_under_14 = NULL
  )
  
  # Load regional preset when selected
  observeEvent(input$region, {
    preset <- regional_presets[[input$region]]
    updateNumericInput(session, "total_pop", value = preset$context$total_population)
    updateNumericInput(session, "prevalence", value = preset$context$hiv_prevalence * 100)
    updateNumericInput(session, "new_infections", value = preset$context$new_infections_per_year)
    updateNumericInput(session, "current_dx", value = preset$context$current_diagnoses)
    updateNumericInput(session, "pct_diagnosed", value = preset$context$percent_diagnosed)  
    updateNumericInput(session, "pct_on_art", value = preset$context$percent_on_art)
    updateNumericInput(session, "pct_suppressed", value = preset$context$percent_suppressed)
    updateNumericInput(session, "aids_deaths", value = preset$context$aids_deaths_per_year)
    
    # Store demographic parameters FROM THE SELECTED REGION (from CSV)
    demographic_params$birth_rate <- preset$context$birth_rate
    demographic_params$prop_pop_male <- preset$context$prop_pop_male
    demographic_params$prop_pop_under_14 <- preset$context$prop_pop_under_14
    
    original_population(preset$context$total_population)
    original_baseline(preset$baseline)
  }, ignoreInit = FALSE)
  
  # Reactive context
  context <- reactive({
    list(
      total_population = input$total_pop,
      hiv_prevalence = input$prevalence / 100,
      new_infections_per_year = input$new_infections,
      current_diagnoses = input$current_dx,
      percent_on_art = input$pct_on_art,
      percent_suppressed = input$pct_suppressed,
      aids_deaths_per_year = input$aids_deaths,
      percent_diagnosed = input$pct_diagnosed,
      # These come from the selected region's CSV data:
      birth_rate = demographic_params$birth_rate,
      prop_pop_male = demographic_params$prop_pop_male,
      prop_pop_under_14 = demographic_params$prop_pop_under_14
    )
    
    
  })
  
  # Validate all numeric inputs to prevent negatives
  # Validate all numeric inputs to prevent negatives
  observe({
    # Validate population inputs
    if (!is.null(input$total_pop) && !is.na(input$total_pop) && input$total_pop < 0) {
      updateNumericInput(session, "total_pop", value = 0)
    }
    if (!is.null(input$new_infections) && !is.na(input$new_infections) && input$new_infections < 0) {
      updateNumericInput(session, "new_infections", value = 0)
    }
    if (!is.null(input$current_dx) && !is.na(input$current_dx) && input$current_dx < 0) {
      updateNumericInput(session, "current_dx", value = 0)
    }
    if (!is.null(input$aids_deaths) && !is.na(input$aids_deaths) && input$aids_deaths < 0) {
      updateNumericInput(session, "aids_deaths", value = 0)
    }
    
    # Validate baseline inputs
    baseline <- baseline_input_values()
    for (group_key in names(intervention_groups)) {
      group <- intervention_groups[[group_key]]
      for (int_key in names(group$interventions)) {
        intervention <- group$interventions[[int_key]]
        value <- baseline[[int_key]]
        
        # Check for negative values
        if (!is.null(value) && !is.na(value) && value < 0) {
          updateNumericInput(session, paste0("baseline_", int_key), value = 0)
        }
        
        # Check for coverage > 100%
        if (intervention$type == "coverage") {
          if (!is.null(value) && !is.na(value) && value > 100) {
            updateNumericInput(session, paste0("baseline_", int_key), value = 100)
          }
        }
      }
    }
    
    # Validate scenario inputs
    for (group_key in names(intervention_groups)) {
      group <- intervention_groups[[group_key]]
      for (int_key in names(group$interventions)) {
        intervention <- group$interventions[[int_key]]
        
        s1_val <- input[[paste0("scenario1_", int_key)]]
        s2_val <- input[[paste0("scenario2_", int_key)]]
        
        if (!is.null(s1_val) && !is.na(s1_val) && s1_val < 0) {
          updateNumericInput(session, paste0("scenario1_", int_key), value = 0)
        }
        if (intervention$type == "coverage") {
          if (!is.null(s1_val) && !is.na(s1_val) && s1_val > 100) {
            updateNumericInput(session, paste0("scenario1_", int_key), value = 100)
          }
        }
        if (!is.null(s2_val) && !is.na(s2_val) && s2_val < 0) {
          updateNumericInput(session, paste0("scenario2_", int_key), value = 0)
        }
        if (intervention$type == "coverage") {
          if (!is.null(s2_val) && !is.na(s2_val) && s2_val > 100) {
            updateNumericInput(session, paste0("scenario2_", int_key), value = 100)
          }
        }
      }
    }
  })
  
  # Calculate populations
  populations <- reactive({
    calculate_populations(context())
  })
  
  # Scaled baseline values
  baseline_values <- reactive({
    if (is.null(original_baseline())) return(list())
    
    scale_factor <- input$total_pop / original_population()
    baseline <- original_baseline()
    
    # Scale absolute interventions only
    scaled <- lapply(names(baseline), function(key) {
      # Find intervention
      for (group_key in names(intervention_groups)) {
        group <- intervention_groups[[group_key]]
        if (key %in% names(group$interventions)) {
          intervention <- group$interventions[[key]]
          if (intervention$type == "absolute") {
            return(round(baseline[[key]] * scale_factor))
          } else {
            return(baseline[[key]])  # Coverage stays same
          }
        }
      }
      return(baseline[[key]])
    })
    names(scaled) <- names(baseline)
    scaled
  })
  
  # Generate baseline UI
  output$baseline_ui <- renderUI({
    baseline <- baseline_values()
    if (length(baseline) == 0) return(NULL)
    
    ui_elements <- lapply(names(intervention_groups), function(group_key) {
      group <- intervention_groups[[group_key]]
      
      interventions_ui <- lapply(names(group$interventions), function(int_key) {
        intervention <- group$interventions[[int_key]]
        value <- ifelse(is.null(baseline[[int_key]]), 0, baseline[[int_key]])
        
        numericInput(
          paste0("baseline_", int_key),
          label = paste0(intervention$name, " (", intervention$unit_label, ")"),
          value = value,
          min = 0,
          step = if(intervention$type == "coverage") 0.1 else 1
        )
      })
      
      tagList(
        h4(group$name, style = paste0("color: ", group$color, "; border-left: 4px solid ", 
                                      group$color, "; padding-left: 10px;")),
        interventions_ui
      )
    })
    
    tagList(ui_elements)
  })
  
  # Generate scenario UI (side-by-side)
  output$scenario_ui <- renderUI({
    baseline <- baseline_input_values()
    if (length(baseline) == 0) return(NULL)
    
    scenario_columns <- lapply(names(intervention_groups), function(group_key) {
      group <- intervention_groups[[group_key]]
      
      interventions_ui <- lapply(names(group$interventions), function(int_key) {
        intervention <- group$interventions[[int_key]]
        base_value <- ifelse(is.null(baseline[[int_key]]), 0, baseline[[int_key]])
        
        # Special styling for MMD interventions
        is_mmd <- grepl("mmd_", int_key)
        
        layout_columns(
          col_widths = c(4, 4, 4),
          div(
            style = "padding-top: 25px; font-size: 0.9em;",
            strong(intervention$name),
            br(),
            span(style = "color: #666;", "Baseline: ",format(round(base_value, 1), big.mark = ",")),
            br(),
            span(style = "color: #999; font-size: 0.85em;", intervention$unit_label)
          ),
          div(
            numericInput(
              paste0("scenario1_", int_key),
              label = "Scenario 1",
              value = base_value,
              min = 0,
              step = if(intervention$type == "coverage") 0.1 else 1
            ),
            if (is_mmd) uiOutput(paste0("mmd_warning1_", int_key))
          ),
          div(
            numericInput(
              paste0("scenario2_", int_key),
              label = "Scenario 2",
              value = base_value,
              min = 0,
              step = if(intervention$type == "coverage") 0.1 else 1
            ),
            if (is_mmd) uiOutput(paste0("mmd_warning2_", int_key))
          )
        )
      })
      
      # Add MMD coverage indicator at end of monitoring section
      if (group_key == "treatment_monitoring") {
        interventions_ui <- c(interventions_ui, list(
          layout_columns(
            col_widths = c(4, 4, 4),
            div(),
            div(
              h6("MMD Coverage:", style = "margin-top: 15px;"),
              uiOutput("mmd_total_scenario1")
            ),
            div(
              h6("MMD Coverage:", style = "margin-top: 15px;"),
              uiOutput("mmd_total_scenario2")
            )
          )
        ))
      }
      
      tagList(
        h4(group$name, style = paste0("color: ", group$color, "; border-left: 4px solid ", 
                                      group$color, "; padding-left: 10px; margin-top: 20px;")),
        interventions_ui
      )
    })
    
    tagList(scenario_columns)
  })
  
  # MMD coverage warnings
  output$mmd_total_scenario1 <- renderUI({
    mmd3 <- input$scenario1_mmd_3month
    mmd6 <- input$scenario1_mmd_6month
    mmd12 <- input$scenario1_mmd_12month
    
    if (is.null(mmd3) || is.null(mmd6) || is.null(mmd12)) return(NULL)
    
    total <- mmd3 + mmd6 + mmd12
    color <- ifelse(total > 100, "red", ifelse(total > 90, "orange", "green"))
    
    tags$div(
      style = paste0("color: ", color, "; font-weight: bold;"),
      paste0(total, "% of stable clients")
    )
  })
  
  output$mmd_total_scenario2 <- renderUI({
    mmd3 <- input$scenario2_mmd_3month
    mmd6 <- input$scenario2_mmd_6month
    mmd12 <- input$scenario2_mmd_12month
    
    if (is.null(mmd3) || is.null(mmd6) || is.null(mmd12)) return(NULL)
    
    total <- mmd3 + mmd6 + mmd12
    color <- ifelse(total > 100, "red", ifelse(total > 90, "orange", "green"))
    
    tags$div(
      style = paste0("color: ", color, "; font-weight: bold;"),
      paste0(total, "% of stable clients")
    )
  })
  
  # Collect baseline values from inputs ONLY (no fallback to avoid circular dependencies)
  baseline_input_values <- reactive({
    baseline <- list()
    for (group_key in names(intervention_groups)) {
      group <- intervention_groups[[group_key]]
      for (int_key in names(group$interventions)) {
        input_id <- paste0("baseline_", int_key)
        value <- input[[input_id]]
        # Use 0 if NULL or NA
        baseline[[int_key]] <- ifelse(is.null(value) || is.na(value), 0, value)
      }
    }
    baseline
  })
  
  # Update baseline AND scenario inputs when baseline_values() changes
  observeEvent(baseline_values(), {
    baseline <- baseline_values()
    if (length(baseline) == 0) return()
    
    # Update baseline inputs
    for (group_key in names(intervention_groups)) {
      group <- intervention_groups[[group_key]]
      for (int_key in names(group$interventions)) {
        baseline_value <- baseline[[int_key]]
        if (!is.null(baseline_value) && !is.na(baseline_value)) {
          # Update baseline input
          updateNumericInput(session, paste0("baseline_", int_key), value = baseline_value)
          # Update scenario 1 input
          updateNumericInput(session, paste0("scenario1_", int_key), value = baseline_value)
          # Update scenario 2 input
          updateNumericInput(session, paste0("scenario2_", int_key), value = baseline_value)
        }
      }
    }
  }, ignoreInit = TRUE)
  
  # Collect scenario 1 values
  scenario1_values <- reactive({
    scenario <- list()
    for (group_key in names(intervention_groups)) {
      group <- intervention_groups[[group_key]]
      for (int_key in names(group$interventions)) {
        input_id <- paste0("scenario1_", int_key)
        value <- input[[input_id]]
        # Use baseline if scenario input is NULL/NA
        if (is.null(value) || is.na(value)) {
          baseline_value <- input[[paste0("baseline_", int_key)]]
          value <- ifelse(is.null(baseline_value) || is.na(baseline_value), 0, baseline_value)
        }
        scenario[[int_key]] <- value
      }
    }
    scenario
  })
  
  # Collect scenario 2 values
  scenario2_values <- reactive({
    scenario <- list()
    for (group_key in names(intervention_groups)) {
      group <- intervention_groups[[group_key]]
      for (int_key in names(group$interventions)) {
        input_id <- paste0("scenario2_", int_key)
        value <- input[[input_id]]
        # Use baseline if scenario input is NULL/NA
        if (is.null(value) || is.na(value)) {
          baseline_value <- input[[paste0("baseline_", int_key)]]
          value <- ifelse(is.null(baseline_value) || is.na(baseline_value), 0, baseline_value)
        }
        scenario[[int_key]] <- value
      }
    }
    scenario
  })
  
  # Calculate impacts
  impact_scenario1 <- reactive({
    req(populations())
    baseline <- baseline_input_values()  # Explicit dependency
    scenario <- scenario1_values()        # Explicit dependency
    req(baseline, scenario)
    calculate_impact(context(), baseline, scenario, populations())
  })
  
  impact_scenario2 <- reactive({
    req(populations())
    baseline <- baseline_input_values()  # Explicit dependency
    scenario <- scenario2_values()        # Explicit dependency
    req(baseline, scenario)
    calculate_impact(context(), baseline, scenario, populations())
  })
  
  # Calculate 95-95-95 metrics
  calculate_95goals <- function(populations, impact = NULL) {
    if (is.null(impact)) {
      # Baseline
      first_95 <- (populations$diagnosed / populations$plhiv) * 100
      second_95 <- (populations$on_art / populations$diagnosed) * 100
      third_95 <- (populations$suppressed / populations$on_art) * 100
    } else {
      # After intervention
      first_95 <- (impact$new_diagnosed / populations$plhiv) * 100
      second_95 <- (impact$new_on_art / impact$new_diagnosed) * 100
      third_95 <- (impact$new_suppressed / impact$new_on_art) * 100
    }
    
    list(
      first_95 = round(first_95, 1),
      second_95 = round(second_95, 1),
      third_95 = round(third_95, 1)
    )
  }
  
  # 95-95-95 Goals - Baseline
  output$goals_baseline <- renderUI({
    pops <- populations()
    goals <- calculate_95goals(pops)
    
    tagList(
      div(class = "d-flex justify-content-between border-bottom pb-2 mb-2",
          span(strong("1st 95:"), " % of PLHIV diagnosed"),
          span(style = "font-size: 1.3em; font-weight: bold;",
               paste0(goals$first_95, "%"))
      ),
      div(class = "d-flex justify-content-between border-bottom pb-2 mb-2",
          span(strong("2nd 95:"), " % of diagnosed on ART"),
          span(style = "font-size: 1.3em; font-weight: bold;",
               paste0(goals$second_95, "%"))
      ),
      div(class = "d-flex justify-content-between border-bottom pb-2 mb-2",
          span(strong("3rd 95:"), " % on ART suppressed"),
          span(style = "font-size: 1.3em; font-weight: bold;",
               paste0(goals$third_95, "%"))
      )
    )
  })
  
  # 95-95-95 Goals - Scenario 1
  output$goals_scenario1 <- renderUI({
    pops <- populations()
    impact <- impact_scenario1()
    baseline_goals <- calculate_95goals(pops)
    goals <- calculate_95goals(pops, impact)
    
    tagList(
      div(class = "d-flex justify-content-between border-bottom pb-2 mb-2",
          span(strong("1st 95:"), " % of PLHIV diagnosed"),
          span(
            style = paste0("font-size: 1.3em; font-weight: bold; color: ",
                           ifelse(goals$first_95 > baseline_goals$first_95, "green",
                                  ifelse(goals$first_95 < baseline_goals$first_95, "red", "gray")), ";"),
            paste0(goals$first_95, "%")
          )
      ),
      div(class = "d-flex justify-content-between border-bottom pb-2 mb-2",
          span(strong("2nd 95:"), " % of diagnosed on ART"),
          span(
            style = paste0("font-size: 1.3em; font-weight: bold; color: ",
                           ifelse(goals$second_95 > baseline_goals$second_95, "green",
                                  ifelse(goals$second_95 < baseline_goals$second_95, "red", "gray")), ";"),
            paste0(goals$second_95, "%")
          )
      ),
      div(class = "d-flex justify-content-between border-bottom pb-2 mb-2",
          span(strong("3rd 95:"), " % on ART suppressed"),
          span(
            style = paste0("font-size: 1.3em; font-weight: bold; color: ",
                           ifelse(goals$third_95 > baseline_goals$third_95, "green",
                                  ifelse(goals$third_95 < baseline_goals$third_95, "red", "gray")), ";"),
            paste0(goals$third_95, "%")
          )
      )
    )
  })
  
  # 95-95-95 Goals - Scenario 2
  output$goals_scenario2 <- renderUI({
    pops <- populations()
    impact <- impact_scenario2()
    baseline_goals <- calculate_95goals(pops)
    goals <- calculate_95goals(pops, impact)
    
    tagList(
      div(class = "d-flex justify-content-between border-bottom pb-2 mb-2",
          span(strong("1st 95:"), " % of PLHIV diagnosed"),
          span(
            style = paste0("font-size: 1.3em; font-weight: bold; color: ",
                           ifelse(goals$first_95 > baseline_goals$first_95, "green",
                                  ifelse(goals$first_95 < baseline_goals$first_95, "red", "gray")), ";"),
            paste0(goals$first_95, "%")
          )
      ),
      div(class = "d-flex justify-content-between border-bottom pb-2 mb-2",
          span(strong("2nd 95:"), " % of diagnosed on ART"),
          span(
            style = paste0("font-size: 1.3em; font-weight: bold; color: ",
                           ifelse(goals$second_95 > baseline_goals$second_95, "green",
                                  ifelse(goals$second_95 < baseline_goals$second_95, "red", "gray")), ";"),
            paste0(goals$second_95, "%")
          )
      ),
      div(class = "d-flex justify-content-between border-bottom pb-2 mb-2",
          span(strong("3rd 95:"), " % on ART suppressed"),
          span(
            style = paste0("font-size: 1.3em; font-weight: bold; color: ",
                           ifelse(goals$third_95 > baseline_goals$third_95, "green",
                                  ifelse(goals$third_95 < baseline_goals$third_95, "red", "gray")), ";"),
            paste0(goals$third_95, "%")
          )
      )
    )
  })
  
  # Epidemiological Outcomes - Baseline
  output$epi_baseline <- renderUI({
    ctx <- context()
    pops <- populations()
    
    # Calculate baseline infant infections
    baseline_infant_infections <- round(pops$hiv_exposed_infants * 0.15) ##Fix
    
    tagList(
      div(class = "d-flex justify-content-between border-bottom pb-2 mb-2",
          span(strong("New Adult Infections:")),
          span(style = "font-size: 1.3em; font-weight: bold;",
               format(ctx$new_infections_per_year, big.mark = ","))
      ),
      div(class = "d-flex justify-content-between border-bottom pb-2 mb-2",
          span(strong("New Infant Infections:")),
          span(style = "font-size: 1.3em; font-weight: bold;",
               format(baseline_infant_infections, big.mark = ","))
      ),
      div(class = "d-flex justify-content-between border-bottom pb-2 mb-2",
          span(strong("HIV-Related Deaths:")),
          span(style = "font-size: 1.3em; font-weight: bold;",
               format(ctx$aids_deaths_per_year, big.mark = ","))
      )
    )
  })
  
  # Epidemiological Outcomes - Scenario 1
  output$epi_scenario1 <- renderUI({
    ctx <- context()
    pops <- populations()
    impact <- impact_scenario1()
    
    baseline_infant_infections <- round(pops$hiv_exposed_infants * 0.15)
    
    tagList(
      div(class = "d-flex justify-content-between border-bottom pb-2 mb-2",
          span(strong("New Adult Infections:")),
          span(
            style = paste0("font-size: 1.3em; font-weight: bold; color: ",
                           ifelse(impact$new_infections < ctx$new_infections_per_year, "green",
                                  ifelse(impact$new_infections > ctx$new_infections_per_year, "red", "gray")), ";"),
            format(impact$new_infections, big.mark = ",")
          )
      ),
      div(class = "d-flex justify-content-between border-bottom pb-2 mb-2",
          span(strong("New Infant Infections:")),
          span(
            style = paste0("font-size: 1.3em; font-weight: bold; color: ",
                           ifelse(impact$new_infant_infections < baseline_infant_infections, "green",
                                  ifelse(impact$new_infant_infections > baseline_infant_infections, "red", "gray")), ";"),
            format(impact$new_infant_infections, big.mark = ",")
          )
      ),
      div(class = "d-flex justify-content-between border-bottom pb-2 mb-2",
          span(strong("HIV-Related Deaths:")),
          span(
            style = paste0("font-size: 1.3em; font-weight: bold; color: ",
                           ifelse(impact$new_deaths < ctx$aids_deaths_per_year, "green",
                                  ifelse(impact$new_deaths > ctx$aids_deaths_per_year, "red", "gray")), ";"),
            format(impact$new_deaths, big.mark = ",")
          )
      )
    )
  })
  
  # Epidemiological Outcomes - Scenario 2
  output$epi_scenario2 <- renderUI({
    ctx <- context()
    pops <- populations()
    impact <- impact_scenario2()
    
    baseline_infant_infections <- round(pops$hiv_exposed_infants * 0.15) ###Fix
    
    tagList(
      div(class = "d-flex justify-content-between border-bottom pb-2 mb-2",
          span(strong("New Adult Infections:")),
          span(
            style = paste0("font-size: 1.3em; font-weight: bold; color: ",
                           ifelse(impact$new_infections < ctx$new_infections_per_year, "green",
                                  ifelse(impact$new_infections > ctx$new_infections_per_year, "red", "gray")), ";"),
            format(impact$new_infections, big.mark = ",")
          )
      ),
      div(class = "d-flex justify-content-between border-bottom pb-2 mb-2",
          span(strong("New Infant Infections:")),
          span(
            style = paste0("font-size: 1.3em; font-weight: bold; color: ",
                           ifelse(impact$new_infant_infections < baseline_infant_infections, "green",
                                  ifelse(impact$new_infant_infections > baseline_infant_infections, "red", "gray")), ";"),
            format(impact$new_infant_infections, big.mark = ",")
          )
      ),
      div(class = "d-flex justify-content-between border-bottom pb-2 mb-2",
          span(strong("HIV-Related Deaths:")),
          span(
            style = paste0("font-size: 1.3em; font-weight: bold; color: ",
                           ifelse(impact$new_deaths < ctx$aids_deaths_per_year, "green",
                                  ifelse(impact$new_deaths > ctx$aids_deaths_per_year, "red", "gray")), ";"),
            format(impact$new_deaths, big.mark = ",")
          )
      )
    )
  })
  
  # Render results - Scenario 1 Health
  output$results_scenario1_health <- renderUI({
    impact <- impact_scenario1()
    
    tagList(
      div(class = "d-flex justify-content-between border-bottom pb-2 mb-2",
          span("Tests Performed:"),
          span(style = "font-weight: bold;", format(impact$tests_performed, big.mark = ","))
      ),
      div(class = "d-flex justify-content-between border-bottom pb-2 mb-2",
          span("New Diagnoses:"),
          span(class = ifelse(impact$new_diagnoses >= 0, "text-primary", "text-warning"),
               style = "font-weight: bold;",
               format(impact$new_diagnoses, big.mark = ","))
      ),
      div(class = "d-flex justify-content-between border-bottom pb-2 mb-2",
          span("Re-engagement in Care:"),
          span(class = ifelse(impact$re_engagement >= 0, "text-primary", "text-warning"),
               style = "font-weight: bold;",
               format(impact$re_engagement, big.mark = ","))
      ),
      div(class = "d-flex justify-content-between border-bottom pb-2 mb-3",
          strong("ART Initiations:"),
          strong(class = ifelse(impact$art_initiations >= 0, "text-success", "text-danger"),
                 format(impact$art_initiations, big.mark = ","))
      ),
      div(class = "d-flex justify-content-between border-bottom pb-2 mb-2",
          span("Adult Infections Averted:"),
          span(class = ifelse(impact$infections_averted >= 0, "text-success", "text-danger"),
               style = "font-weight: bold;",
               format(impact$infections_averted, big.mark = ","))
      ),
      div(class = "d-flex justify-content-between border-bottom pb-2 mb-2",
          span("Infant Infections Averted:"),
          span(class = ifelse(impact$infant_infections_averted >= 0, "text-success", "text-danger"),
               style = "font-weight: bold;",
               format(impact$infant_infections_averted, big.mark = ","))
      ),
      div(class = "d-flex justify-content-between border-bottom pb-2 mb-3",
          strong("Total Infections Averted:"),
          strong(class = ifelse(impact$total_infections_averted >= 0, "text-success", "text-danger"),
                 format(impact$total_infections_averted, big.mark = ","))
      ),
      div(class = "d-flex justify-content-between border-bottom pb-2 mb-2",
          span("Deaths Averted:"),
          span(class = ifelse(impact$deaths_averted >= 0, "text-success", "text-danger"),
               style = "font-weight: bold;",
               format(impact$deaths_averted, big.mark = ","))
      ),
      div(class = "d-flex justify-content-between border-bottom pb-2 mb-2",
          span("Additional Suppressed:"),
          span(class = "text-primary", style = "font-weight: bold;",
               format(impact$additional_suppressed, big.mark = ","))
      )
    )
  })
  
  # Render results - Scenario 2 Health
  output$results_scenario2_health <- renderUI({
    impact <- impact_scenario2()
    
    tagList(
      div(class = "d-flex justify-content-between border-bottom pb-2 mb-2",
          span("Tests Performed:"),
          span(style = "font-weight: bold;", format(impact$tests_performed, big.mark = ","))
      ),
      div(class = "d-flex justify-content-between border-bottom pb-2 mb-2",
          span("New Diagnoses:"),
          span(class = ifelse(impact$new_diagnoses >= 0, "text-primary", "text-warning"),
               style = "font-weight: bold;",
               format(impact$new_diagnoses, big.mark = ","))
      ),
      div(class = "d-flex justify-content-between border-bottom pb-2 mb-2",
          span("Re-engagement in Care:"),
          span(class = ifelse(impact$re_engagement >= 0, "text-primary", "text-warning"),
               style = "font-weight: bold;",
               format(impact$re_engagement, big.mark = ","))
      ),
      div(class = "d-flex justify-content-between border-bottom pb-2 mb-3",
          strong("ART Initiations:"),
          strong(class = ifelse(impact$art_initiations >= 0, "text-success", "text-danger"),
                 format(impact$art_initiations, big.mark = ","))
      ),
      div(class = "d-flex justify-content-between border-bottom pb-2 mb-2",
          span("Adult Infections Averted:"),
          span(class = ifelse(impact$infections_averted >= 0, "text-success", "text-danger"),
               style = "font-weight: bold;",
               format(impact$infections_averted, big.mark = ","))
      ),
      div(class = "d-flex justify-content-between border-bottom pb-2 mb-2",
          span("Infant Infections Averted:"),
          span(class = ifelse(impact$infant_infections_averted >= 0, "text-success", "text-danger"),
               style = "font-weight: bold;",
               format(impact$infant_infections_averted, big.mark = ","))
      ),
      div(class = "d-flex justify-content-between border-bottom pb-2 mb-3",
          strong("Total Infections Averted:"),
          strong(class = ifelse(impact$total_infections_averted >= 0, "text-success", "text-danger"),
                 format(impact$total_infections_averted, big.mark = ","))
      ),
      div(class = "d-flex justify-content-between border-bottom pb-2 mb-2",
          span("Deaths Averted:"),
          span(class = ifelse(impact$deaths_averted >= 0, "text-success", "text-danger"),
               style = "font-weight: bold;",
               format(impact$deaths_averted, big.mark = ","))
      ),
      div(class = "d-flex justify-content-between border-bottom pb-2 mb-2",
          span("Additional Suppressed:"),
          span(class = "text-primary", style = "font-weight: bold;",
               format(impact$additional_suppressed, big.mark = ","))
      )
    )
  })
  
  # Render results - Scenario 1 Cost
  output$results_scenario1_cost <- renderUI({
    impact <- impact_scenario1()
    
    tagList(
      div(class = "d-flex justify-content-between border-bottom pb-2 mb-2",
          span("Intervention Costs (scale-up):"),
          span(class = "text-primary", style = "font-weight: bold;",
               paste0("$", format(impact$total_cost, big.mark = ",")))
      ),
      div(class = "d-flex justify-content-between border-bottom pb-2 mb-2",
          span("Savings (scale-down):"),
          span(class = "text-success", style = "font-weight: bold;",
               paste0("$", format(impact$cost_savings, big.mark = ",")))
      ),
      div(class = "d-flex justify-content-between border-bottom pb-2 mb-2",
          span(style = "font-style: italic; font-size: 0.9em;", "ART Provision (outcome-driven):"),
          span(style = "font-weight: bold; color: #8b5cf6;",
               paste0("$", format(impact$art_provision_cost, big.mark = ",")))
      ),
      div(class = "d-flex justify-content-between border-bottom pb-2 mb-3 bg-light p-2",
          strong("Net Budget Impact:"),
          strong(class = ifelse(impact$net_cost >= 0, "text-primary", "text-success"),
                 paste0(ifelse(impact$net_cost >= 0, "+", ""), "$", 
                        format(impact$net_cost, big.mark = ",")))
      ),
      if (impact$total_infections_averted > 0) {
        div(class = "d-flex justify-content-between border-bottom pb-2 mb-2 mt-3",
            span("Cost per Infection Averted:"),
            span(style = "font-weight: bold;",
                 paste0("$", format(impact$cost_per_infection_averted, big.mark = ",")))
        )
      },
      if (impact$deaths_averted > 0) {
        div(class = "d-flex justify-content-between border-bottom pb-2 mb-2",
            span("Cost per Death Averted:"),
            span(style = "font-weight: bold;",
                 paste0("$", format(impact$cost_per_death_averted, big.mark = ",")))
        )
      }
    )
  })
  
  # Render results - Scenario 2 Cost
  output$results_scenario2_cost <- renderUI({
    impact <- impact_scenario2()
    
    tagList(
      div(class = "d-flex justify-content-between border-bottom pb-2 mb-2",
          span("Intervention Costs (scale-up):"),
          span(class = "text-primary", style = "font-weight: bold;",
               paste0("$", format(impact$total_cost, big.mark = ",")))
      ),
      div(class = "d-flex justify-content-between border-bottom pb-2 mb-2",
          span("Savings (scale-down):"),
          span(class = "text-success", style = "font-weight: bold;",
               paste0("$", format(impact$cost_savings, big.mark = ",")))
      ),
      div(class = "d-flex justify-content-between border-bottom pb-2 mb-2",
          span(style = "font-style: italic; font-size: 0.9em;", "ART Provision (outcome-driven):"),
          span(style = "font-weight: bold; color: #8b5cf6;",
               paste0("$", format(impact$art_provision_cost, big.mark = ",")))
      ),
      div(class = "d-flex justify-content-between border-bottom pb-2 mb-3 bg-light p-2",
          strong("Net Budget Impact:"),
          strong(class = ifelse(impact$net_cost >= 0, "text-primary", "text-success"),
                 paste0(ifelse(impact$net_cost >= 0, "+", ""), "$", 
                        format(impact$net_cost, big.mark = ",")))
      ),
      if (impact$total_infections_averted > 0) {
        div(class = "d-flex justify-content-between border-bottom pb-2 mb-2 mt-3",
            span("Cost per Infection Averted:"),
            span(style = "font-weight: bold;",
                 paste0("$", format(impact$cost_per_infection_averted, big.mark = ",")))
        )
      },
      if (impact$deaths_averted > 0) {
        div(class = "d-flex justify-content-between border-bottom pb-2 mb-2",
            span("Cost per Death Averted:"),
            span(style = "font-weight: bold;",
                 paste0("$", format(impact$cost_per_death_averted, big.mark = ",")))
        )
      }
    )
  })
  
  # COMBINED CASCADE PLOT
  output$cascade_combined <- renderPlot({
    impact1 <- impact_scenario1()
    impact2 <- impact_scenario2()
    pops <- populations()
    
    # Create data frame with all three scenarios
    cascade_data <- data.frame(
      Stage = rep(c("Diagnosed", "On ART", "Suppressed"), 3),
      Scenario = rep(c("Baseline", "Scenario 1", "Scenario 2"), each = 3),
      Value = c(
        # Baseline
        pops$diagnosed, pops$on_art, pops$suppressed,
        # Scenario 1
        impact1$new_diagnosed, impact1$new_on_art, impact1$new_suppressed,
        # Scenario 2
        impact2$new_diagnosed, impact2$new_on_art, impact2$new_suppressed
      )
    )
    
    cascade_data$Stage <- factor(cascade_data$Stage, levels = c("Diagnosed", "On ART", "Suppressed"))
    cascade_data$Scenario <- factor(cascade_data$Scenario, levels = c("Baseline", "Scenario 1", "Scenario 2"))
    
    ggplot(cascade_data, aes(x = Stage, y = Value, color = Scenario, group = Scenario, linetype = Scenario)) +
      geom_line(linewidth = 1.5) +
      geom_point(size = 4) +
      scale_color_manual(values = c("Baseline" = "#666666", "Scenario 1" = "#2563eb", "Scenario 2" = "#dc2626")) +
      scale_linetype_manual(values = c("Baseline" = "solid", "Scenario 1" = "solid", "Scenario 2" = "dashed")) +
      scale_y_continuous(labels = comma) +
      labs(title = "HIV Care Cascade Comparison",
           subtitle = "Gray = Baseline | Blue solid = Scenario 1 | Red dashed = Scenario 2",
           y = "Number of People", x = NULL) +
      theme_minimal() +
      theme(legend.position = "top",
            legend.title = element_blank(),
            plot.title = element_text(size = 16, face = "bold"),
            plot.subtitle = element_text(size = 12),
            axis.text = element_text(size = 12),
            axis.title = element_text(size = 12))
  })
  
  # CASCADE LINE PLOTS (keep individual ones but remove deaths)
  output$cascade_scenario1 <- renderPlot({
    impact <- impact_scenario1()
    pops <- populations()
    
    cascade_data <- data.frame(
      Stage = factor(c("Diagnosed", "On ART", "Suppressed", "Annual Deaths"),
                     levels = c("Diagnosed", "On ART", "Suppressed", "Annual Deaths")),
      Baseline = c(pops$diagnosed, pops$on_art, pops$suppressed, context()$aids_deaths_per_year),
      Intervention = c(impact$new_diagnosed, impact$new_on_art, impact$new_suppressed, impact$new_deaths)
    )
    
    cascade_long <- cascade_data %>%
      pivot_longer(cols = c(Baseline, Intervention), names_to = "Scenario", values_to = "Value")
    
    ggplot(cascade_long, aes(x = Stage, y = Value, color = Scenario, group = Scenario)) +
      geom_line(linewidth = 1.5) +
      geom_point(size = 4) +
      scale_color_manual(values = c("Baseline" = "#666666", "Intervention" = "#2563eb")) +
      scale_y_continuous(labels = comma) +
      labs(title = "HIV Care Cascade: Baseline → Scenario 1",
           subtitle = "Blue line shows impact of intervention changes",
           y = "Number of People", x = NULL) +
      theme_minimal() +
      theme(legend.position = "top",
            plot.title = element_text(size = 14, face = "bold"),
            axis.text = element_text(size = 11))
  })
  
  output$cascade_scenario2 <- renderPlot({
    impact <- impact_scenario2()
    pops <- populations()
    
    cascade_data <- data.frame(
      Stage = factor(c("Diagnosed", "On ART", "Suppressed", "Annual Deaths"),
                     levels = c("Diagnosed", "On ART", "Suppressed", "Annual Deaths")),
      Baseline = c(pops$diagnosed, pops$on_art, pops$suppressed, context()$aids_deaths_per_year),
      Intervention = c(impact$new_diagnosed, impact$new_on_art, impact$new_suppressed, impact$new_deaths)
    )
    
    cascade_long <- cascade_data %>%
      pivot_longer(cols = c(Baseline, Intervention), names_to = "Scenario", values_to = "Value")
    
    ggplot(cascade_long, aes(x = Stage, y = Value, color = Scenario, group = Scenario)) +
      geom_line(linewidth = 1.5, linetype = "dashed") +
      geom_point(size = 4) +
      scale_color_manual(values = c("Baseline" = "#666666", "Intervention" = "#dc2626")) +
      scale_y_continuous(labels = comma) +
      labs(title = "HIV Care Cascade: Baseline → Scenario 2",
           subtitle = "Red dashed line shows impact of intervention changes",
           y = "Number of People", x = NULL) +
      theme_minimal() +
      theme(legend.position = "top",
            plot.title = element_text(size = 14, face = "bold"),
            axis.text = element_text(size = 11))
  })
  
  # OTHER OUTCOMES BAR PLOTS (include deaths here)
  output$plot_scenario1 <- renderPlot({
    impact <- impact_scenario1()
    
    plot_data <- data.frame(
      Outcome = c("Infections\nAverted", "Deaths\nAverted", "ART\nInitiations"),
      Value = c(impact$total_infections_averted, impact$deaths_averted, impact$art_initiations)
    )
    
    ggplot(plot_data, aes(x = Outcome, y = Value, fill = Value >= 0)) +
      geom_col(width = 0.7) +
      scale_fill_manual(values = c("TRUE" = "#10b981", "FALSE" = "#ef4444"), guide = "none") +
      scale_y_continuous(labels = comma) +
      labs(title = "Key Outcomes Summary", y = "Number of People", x = NULL) +
      theme_minimal() +
      theme(axis.text.x = element_text(size = 12),
            axis.text.y = element_text(size = 11),
            plot.title = element_text(size = 14, face = "bold"))
  })
  
  output$plot_scenario2 <- renderPlot({
    impact <- impact_scenario2()
    
    plot_data <- data.frame(
      Outcome = c("Infections\nAverted", "Deaths\nAverted", "ART\nInitiations"),
      Value = c(impact$total_infections_averted, impact$deaths_averted, impact$art_initiations)
    )
    
    ggplot(plot_data, aes(x = Outcome, y = Value, fill = Value >= 0)) +
      geom_col(width = 0.7) +
      scale_fill_manual(values = c("TRUE" = "#10b981", "FALSE" = "#ef4444"), guide = "none") +
      scale_y_continuous(labels = comma) +
      labs(title = "Key Outcomes Summary", y = "Number of People", x = NULL) +
      theme_minimal() +
      theme(axis.text.x = element_text(size = 12),
            axis.text.y = element_text(size = 11),
            plot.title = element_text(size = 14, face = "bold"))
  })
}

# ============================================================================
# RUN APPLICATION
# ============================================================================

shinyApp(ui = ui, server = server)