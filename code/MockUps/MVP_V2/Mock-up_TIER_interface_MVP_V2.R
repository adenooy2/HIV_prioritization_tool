# ============================================================================
# HIV Intervention Impact Calculator - R Shiny Application (FINAL VERSION)
# ============================================================================
# This tool allows users to model the health and cost impacts of scaling
# HIV interventions up or down across prevention, testing, and treatment.
#
# KEY FEATURES:
# - Scenario-based calculations with mortality by cascade stage
# - Complete input validation with dynamic limits
# - Clear user feedback for constraint violations
# ============================================================================

library(shiny)
library(bslib)
library(bsicons)
library(DT)
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
library(httr)
library(readr)
library(readxl)
library(rmarkdown)
library(pagedown)

# Source logic file
tryCatch(
  {
    # Try local source first
    source("/Users/adenooy/Library/CloudStorage/OneDrive-Personal/AMC/HIV Prioritization tool/HIV_prioritization_tool/code/MockUps/MVP_V2/Mock-Up_logic_V2.R")
    message("Sourced local file successfully.")
  },
  error = function(e) {
    message("Local source failed. Trying alternative paths...")
    stop("Could not source logic file")
  }
)

# Source usage logger and initialise log DB.
# Logging failure must NEVER block app startup -- the tryCatch guarantees
# the app runs even if usage_logger.R is missing or init fails.
tryCatch({
  source("/Users/adenooy/Library/CloudStorage/OneDrive-Personal/AMC/HIV Prioritization tool/HIV_prioritization_tool/code/MockUps/MVP_V2/usage_logger.R")
  init_log_db()
}, error = function(e) {
  message("usage_logger.R not found or failed to init -- logging disabled.")
})



# ============================================================================
# USER INTERFACE
# ============================================================================

ui <- page_sidebar(
  tags$head(tags$style(HTML("
    hr { margin-top: 0.75rem; margin-bottom: 0.5rem; }
    .disabled-input { margin: 0; }
    .disabled-input .form-group { margin-bottom: 0; }
    .disabled-input input {
      pointer-events: none;
      background-color: #e9ecef;
      opacity: 1;
    }
  "))),
  tags$script(HTML("
      $(document).on('shiny:value shiny:idle', function() {
        $('.disabled-input input').attr('readonly', true);
      });
    ")),
  tags$script(HTML("
      // Browser-side geo lookup. Calls ipapi.co from the user's browser
      // (so ipapi sees the real client IP, not the server's). Sends the
      // resolved country back to Shiny via setInputValue. Fails silently
      // if blocked or unreachable -- the logger then keeps 'unknown'.
      $(document).on('shiny:connected', function() {
        try {
          fetch('https://ipapi.co/country_name/', {
            method: 'GET',
            // 4-second hard timeout via AbortController
            signal: (function() {
              var c = new AbortController();
              setTimeout(function() { c.abort(); }, 4000);
              return c.signal;
            })()
          })
          .then(function(r) {
            if (!r.ok) throw new Error('ipapi returned ' + r.status);
            return r.text();
          })
          .then(function(country) {
            country = (country || '').trim();
            if (country && country.length > 0 && country.length < 100) {
              Shiny.setInputValue('user_geo_country', country,
                                  {priority: 'event'});
            }
          })
          .catch(function(e) {
            // Silent on any failure -- network, rate-limit, abort, parse, etc.
          });
        } catch (e) { /* swallow */ }
      });
    ")),
  title = tags$div(
    style = "display: flex; align-items: center; gap: 16px;",
    tags$img(src = "IAS-logo.png", height = "60px", alt = "IAS logo"),
    tags$span(
      "TIER-Plus - HIV Intervention Impact Calculator",
      style = "font-size: 24px; font-weight: 600;"
    )
  ),
  sidebar = sidebar(
    width = 300,
    selectInput(
      "region",
      "Select Regional Profile:",
      choices = names(regional_presets),
      selected = names(regional_presets)[1]
    ),
    hr(),
    h5("Epidemic Parameters (Start of Year)"),
    numericInput("total_pop", "Total Population:", value = 5000000, min = 0),
    tags$div(class = "disabled-input",numericInput("prevalence", "HIV Prevalence among people aged 15+ (%):", value = 4.5, min = 0, max = 100, step = 0.1)),
    tags$div(class = "disabled-input",numericInput("new_infections", "New Acquisitions/Year:", value = 8500, min = 0)),
    numericInput("pct_diagnosed", "% of PLHIV Diagnosed:", value = 85, min = 0, max = 100),  
    numericInput("pct_on_art", "% Diagnosed on ART:", value = 78, min = 0, max = 100),
    numericInput("pct_suppressed", "% on ART Suppressed:", value = 82, min = 0, max = 100),
    tags$div(class = "disabled-input",numericInput("aids_deaths", "AIDS Deaths/Year (baseline):", value = 2200, min = 0))
    
    
  ),
  # ---- Feedback strip (above tabs) ----
  div(
    style = paste(
      "display: flex;",
      "justify-content: flex-end;",
      "align-items: center;",
      "gap: 18px;",
      "flex-wrap: wrap;",
      "padding: 4px 8px 8px 8px;",
      "font-size: 0.9em;",
      "color: #595959;"
    ),
    # span(
    #   style = "color: #777; font-size: 0.85em; margin-right: auto;",
    #   em("This app logs anonymised usage data (country of access, scenario inputs, timing). IP addresses are hashed and not stored. No personal or patient data is collected.")
    # ),
    # TEMP: Validation feedback link — remove after validation period ends
    span(
      icon("clipboard-check"), " ",
      tags$a(
        href   = "https://forms.cloud.microsoft/Pages/ResponsePage.aspx?id=f_74E4DG7kuhFmNZHWvuMmCh7_sn9aVLibTDrfIQKjtURENRUFVMQVRXWldOMkFMMU1MSFBDOVk5Ti4u",
        target = "_blank",
        rel    = "noopener noreferrer",
        "Validation Feedback Form",
        style  = "color: #2563eb;"
      )
    ),
    # END TEMP
    #span("Found a bug or have a suggestion?"),
    actionLink("open_feedback",
               label = tagList(icon("comment-dots"), " Share feedback"),
               style = "color: #2563eb;"),
    span(
      icon("envelope"), " ",
      tags$a(href = "mailto:dsd@iasociety.org?subject=TIER-Plus%20Contact",
             "dsd@iasociety.org",
             style = "color: #2563eb;")
    )
  ),
  navset_card_tab(
    id = "main_tabs",
    nav_panel(
      "User Guide",
      div(
        style = "max-width: 900px; padding: 12px 4px; line-height: 1.5;",
        h3("TIER-Plus — User Guide"),
        p(em("A decision-support tool for trade-off analysis in HIV service planning")),
        
        # YouTube intro video — 16:9 responsive embed within the 900px content column
        tags$div(
          style = paste(
            "position: relative;",
            "width: 100%;",
            "max-width: 600px;",         # caps width on wide screens
            "aspect-ratio: 16 / 9;",
            "margin: 0 0 16px 0;"        # left-align, space below
          ),
          tags$iframe(
            src = "https://www.youtube.com/embed/cr4ibbplYi8",
            style = "position: absolute; top: 0; left: 0; width: 100%; height: 100%; border: 0;",
            allow = "accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture",
            allowfullscreen = NA,
            title = "TIER-Plus introduction video",
            loading = "lazy"
          )
        ),
        
      
        
        h4("What is TIER-Plus"),
        p("TIER-Plus is an extension of the original TIER prioritization tool. It aims to enable stakeholders to compare outcome and cost trade-offs across HIV interventions though an accessible, interactive platform.  It takes a current programme picture and lets the user vary the volume of interventions to compare how cascade outcomes, new acquisitions, deaths, and total cost are expected to move in response."),
        p(strong("Important: TIER-Plus is intended as a support tool for prioritization conversations. "), "As such, the tool is ", em("indicative "),
          " of the direction and rough magnitude of impact of different choices. To make the tool quick and easy to use, and applicable across many contexts, it does not have the full complexity of other modelling tools. Specific numbers used for budgeting, target-setting, or operational planning should come from country-led processes, with decisions supported by this tool."),
        
        h4("How to use the tool"),
        tags$ol(
          tags$li(strong("Set the country context. "),
                  "Enter or confirm population and current 95-targets (diagnosed, on ART, suppressed) in the most recent reporting year."),
          tags$li(strong("Enter the baseline scenario. "),
                  "Populate each intervention with the volume and coverage of services delivered in the year prior (see ", em("What the baseline represents"), " below)."),
          tags$li(strong("Build a scenario. "),
                  "Adjust intervention volumes up or down from baseline. You can scale things in either direction."),
          tags$li(strong("Compare. "),
                  "The output shows the estimated differences between each scenario and baseline. This includes the 95-targets, expected acquisitions, infant acquisitions, deaths and budget. Positive ", tags$q("acquisitions averted"), " and ", tags$q("deaths averted"),
                  " mean the scenario outperforms baseline. Negative values mean it does worse — useful for testing budget-cut scenarios."),
          tags$li(strong("Iterate. "),
                  "Run several scenarios side-by-side to see where additional money buys the most impact and where cuts are least harmful.")
        ),
        
        h4("What the baseline scenario coverage represents"),
        p("Baseline coverage values should reflect what was delivered in the most recent year. It serves as a reference point against which alternative scenarios can be compared - i.e if we had the same implementation as last year what impact can we expect and how might this differ with alternative services. Additionally, baseline data is incorporated for model calibration, and hence serves an important purpose." ),
        p("All numerical inputs are annual counts of people reached / units delivered, except where the input is explicitly described as a percentage."),
        
        h4("Intervention definitions and data requirements"),
        
        h5(strong("Prevention")),
        tags$ul(
          tags$li(strong("Oral PrEP: "), "Number of individuals currently receiving and/or initiated on oral PrEP."),
          tags$li(strong("Long-acting PrEP (lenacapavir): "), "Number of individuals currently receiving and/or initiated on long-acting injectable PrEP."),
          tags$li(strong("Condoms: "), "Total number of condoms distributed in the year."),
          tags$li(strong("VMMC: "), "Voluntary medical male circumcisions performed in the year."),
          tags$li(strong("Infant prophylaxis: "), "Percentage of HIV-exposed infants receiving HIV prophylaxis (e.g. NVP) to reduce vertical transmission.")
        ),
        
        h5(strong("Testing and diagnosis")),
        tags$ul(
          tags$li(strong("Facility-based testing (general): "), "Number of HIV tests performed at health facilities (excl. ANC)."),
          tags$li(strong("Community testing: "), "Number of HIV tests performed in community settings."),
          tags$li(strong("Index testing: "), "Number of tests conducted among partners of newly-diagnosed people living with HIV."),
          tags$li(strong("Key populations: "), "Number of HIV tests performed among key populations or through STI services (excluding adolescents)."),
          tags$li(strong("Facility HIVST: "), "Number of HIV self-tests distributed at facilities."),
          tags$li(strong("Community HIVST: "), "Number of HIV self-tests distributed in the community."),
          tags$li(strong("Early Infant Diagnosis (EID): "), "Percentage of HIV-exposed infants receiving HIV testing."),
          tags$li(strong("ANC HIV testing: "), "Percentage of pregnant women receiving ANC HIV testing."),
          tags$li(strong("PNC HIV testing: "), "Percentage of postpartum women (not known to be living with HIV) receiving PNC HIV testing.")
        ),
        
        h5(strong("Treatment, retention and monitoring")),
        tags$ul(
          tags$li(strong("Routine VL monitoring: "), "Percentage of people on ART receiving routine viral load testing."),
          tags$li(strong("ANC and PNC VL testing: "), "Coverage of pregnant and postpartum women living with HIV who receive viral load testing."),
          tags$li(strong("Multi-month dispensing (3-month, 6-month, 12-month): "), "Percentage of stable ART clients enrolled in MMD (categories are mutually exclusive; the three must sum to ≤100%)."),
          tags$li(strong("Community ART pick-up: "), "Percentage of MMD-enrolled clients receiving refills via community pickup instead of facility, applied equally across MMD-3/6/12. Has no effect when MMD enrolment is zero."),
          tags$li(strong("Enhanced Adherence Counselling (EAC):"), "Percentage of individuals identified as unsuppressed (through a recent viral load)."),
          tags$li(strong("Tracking and tracing: "), "Outreach to people lost to follow-up to bring them back into care. Applied after DSD has already prevented some LTFU, against the remaining LTFU pool.")
        ),
        
        h5(strong("Advanced HIV disease")),
        tags$ul(
          tags$li(strong("CD4 testing: "), "Coverage of CD4 testing among individuals initiating ART."),
          tags$li(strong("AHD package: "), "Number of AHD-diagnosed clients receiving the package of care.")
        ),
        
        h4("What the tool does not do"),
        tags$ul(
          tags$li("It does not project multi-year dynamics; each run is a single-year calculation."),
          tags$li("It does not model resistance, age structure, or sub-national heterogeneity."),
          tags$li("It does not optimise; it calculates the consequences of a chosen mix, leaving the decision to the user."),
          tags$li("It represents a complementary tool for country-led analysis and other validated modelling tools.")
        ),
        
        hr(),
        p(em("Use the tool to compare directions and trade-offs. Use country processes for the exact numbers in your plan."),
          style = "color: #595959; text-align: center;")
      )
    ),
    
    nav_panel(
      "Baseline Coverage",
      h4("Adjust baseline coverage values"),
      p(strong("Note:"),"Baseline coverage values should reflect services that were delivered in the most recent year. Current values were pre-populated using available data (e.g GAM) and literature where relvant. Please review the values below, and update any for which you have more accurate values."),
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
        style = "display: flex; justify-content: flex-end; margin-bottom: 8px;",
        downloadButton("download_report",
                       "Download report (PDF)",
                       class = "btn-primary",
                       icon = icon("download"))
      ),
      div(
        style = "background-color: #fffbeb; border: 1px solid #f59e0b; border-radius: 6px; padding: 12px 16px; margin-bottom: 16px;",
        tags$strong(style = "color: #92400e;", "\u26a0 Interpretation note. "),
        tags$span(style = "color: #78350f; font-size: 0.9em;",
                  "This tool is ",
                  tags$em("indicative"),
                  " of the direction and rough magnitude of impact of different choices. Outputs may not be accurate to exact numbers and changes to the percentage 95-95-95 targets should be interpreted in conjunction with the absolute change in the number of people in each group. Costs are limited to treatment and included intervention costs only. As a result, many indirect and other programme costs are excluded. Specific numbers used for budgeting, target-setting, or operational planning should come from country-led processes, supported (but not replaced) by this tool."
                  
        )
      ),
      div(
        style = "height: 80vh; overflow-y: auto; padding-right: 15px;",
        
        # 95-95-95 Goals Tracker
        h3("Progress Toward 95-95-95 Goals (End of Year)", class = "mt-3 mb-3"),
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
        h3("Key Epidemiological Outcomes (End of Year)", class = "mt-4 mb-3"),
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
            card_header(class = "bg-primary text-white", "Scenario 1"),
            card_body(uiOutput("results_scenario1_health"))
          ),
          card(
            card_header(class = "bg-danger text-white", "Scenario 2"),
            card_body(uiOutput("results_scenario2_health"))
          )
        ),
        
        # Cost Analysis Row
        h3("Cost Analysis", class = "mt-4 mb-3"),
        layout_columns(
          col_widths = c(4, 4, 4),
          card(
            card_header(class = "bg-secondary text-white", "Baseline"),
            card_body(uiOutput("results_baseline_cost"))
          ),
          card(
            card_header(class = "bg-primary text-white", "Scenario 1"),
            card_body(uiOutput("results_scenario1_cost"))
          ),
          card(
            card_header(class = "bg-danger text-white", "Scenario 2"),
            card_body(uiOutput("results_scenario2_cost"))
          )
        ),
        
        # Cascade Chart - Combined
        h3("HIV Care Cascade: Baseline vs Scenarios (End of Year)", class = "mt-4 mb-3"),
        card(
          card_header("Combined Cascade Comparison"),
          card_body(plotOutput("cascade_combined", height = "500px"))
        ),
        # card(
        #   card_header("Cascade Numbers"),
        #   card_body(tableOutput("cascade_table"))
        # ),
        
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
        
        div(style = "height: 50px;")
      )
    )
  ),
  tags$footer(
    class = "tier-footer",
    style = paste(
      "margin-top: 0.5rem;",
      "padding: 0 rem 0;",
      "border-top: 1px solid #e5e7eb;",
      "text-align: center;",
      "font-size: 0.75rem;",
      "line-height: 1.2;",
      "color: #6b7280;"
    ),
    HTML("Copyright © 2026. IAS – the International AIDS Society")
  )
)
# ============================================================================
# SERVER LOGIC
# ============================================================================

server <- function(input, output, session) {
  
  # ========================================================================
  # USAGE LOGGING STATE
  # ------------------------------------------------------------------------
  # Sets up per-session logging state. All variables are safe defaults if
  # the usage_logger.R module didn't load -- log_enabled gates every call.
  # ========================================================================
  log_enabled <- exists("log_session_start")
  tier_session_id <- if (log_enabled) {
    digest::digest(paste0(session$token, Sys.time()))
  } else {
    NA_character_
  }
  tier_session_ip <- if (log_enabled) extract_client_ip(session) else NA_character_
  tier_session_user_country <- reactiveVal("unknown")
  tier_session_view_count <- reactiveVal(0L)
  
  if (log_enabled) {
    # Fire session_start row immediately with "unknown" country. The
    # browser-side JS in the UI calls ipapi.co and posts the resolved
    # country back via Shiny.setInputValue. We listen for that input
    # and update tier_session_user_country() when it arrives.
    # Subsequent results_view rows pick up the resolved country.
    # The session_start row stays "unknown" by design -- it's logged
    # before the browser fetch completes (the alternative would be to
    # delay session_start, adding 200-500ms latency to every session).
    log_session_start(tier_session_id, tier_session_ip, "unknown")
    
    observeEvent(input$user_geo_country, {
      country <- input$user_geo_country
      if (is.character(country) && length(country) == 1 && nzchar(country)) {
        tier_session_user_country(country)
      }
    }, ignoreInit = TRUE, ignoreNULL = TRUE)
  }
  
  # ---- Intervention tooltip text (used in Baseline & Scenarios tabs) ----
  # Keys MUST match the int_key values used in intervention_groups.
  # Edit text here — it propagates to every tooltip in both tabs.
  intervention_tooltips <- list(
    # Prevention
    prep_oral           = "Number of individuals currently receiving and/or initiated on oral PrEP.",
    prep_lenacapavir    = "Number of individuals currently receiving and/or initiated on long-acting injectable PrEP (lenacapavir).",
    vmmc                = "Voluntary medical male circumcisions performed in the year.",
    condoms             = "Total number of condoms distributed in the year.",
    infant_prophylaxis  = "Percentage of HIV-exposed infants receiving HIV prophylaxis (e.g. NVP) to reduce vertical transmission.",
    
    # Testing
    test_facility_general = "Number of HIV tests performed at health facilities (excluding ANC).",
    test_network          = "Number of HIV tests performed through social-network testing approaches.",
    test_index            = "Number of tests conducted among partners of newly-diagnosed people living with HIV.",
    test_community        = "Number of HIV tests performed in community settings.",
    test_kpsti            = "Number of HIV tests performed among key populations or through STI services (excluding adolescents).",
    hivst_facility        = "Number of HIV self-tests distributed at facilities.",
    hivst_community       = "Number of HIV self-tests distributed in the community.",
    eid                   = "Percentage of HIV-exposed infants receiving HIV testing.",
    anc_hiv_testing       = "Percentage of pregnant women receiving ANC HIV testing.",
    pnc_hiv_testing       = "Percentage of postpartum women (not known to be living with HIV) receiving PNC HIV testing.",
    
    # Treatment monitoring
    vl_monitoring_routine = "Percentage of people on ART receiving routine viral load testing.",
    mmd_3month            = "Number / percentage of stable ART clients enrolled in 3-month multi-month dispensing.",
    mmd_6month            = "Number / percentage of stable ART clients enrolled in 6-month multi-month dispensing.",
    mmd_12month           = "Number / percentage of stable ART clients enrolled in 12-month multi-month dispensing.",
    community_pickup      = "Percentage of MMD-enrolled stable clients receiving refills via community pickup (overrides MMD facility-pickup mode for that share, equally across MMD-3/6/12). Has no effect if MMD enrolment is 0%.",
    
    # Retention support
    adherence_counseling  = "Percentage of individuals identified as unsuppressed (through a recent viral load) receiving Enhanced Adherence Counselling (EAC).",
    tracking_tracing      = "Outreach to people lost to follow-up to bring them back into care. Applied after DSD has already prevented some LTFU, against the remaining LTFU pool.",
    anc_vl_testing        = "Coverage of pregnant women living with HIV who receive viral load testing.",
    pnc_vl_testing        = "Coverage of postpartum women living with HIV who receive viral load testing.",
    
    # Advanced disease
    cd4_testing  = "Coverage of CD4 testing among individuals initiating ART to identify advanced disease. Required to unlock the AHD package.",
    ahd_package  = "Number / percentage of AHD-diagnosed clients receiving the package of care (LAM, CrAg, fluconazole)."
  )
  
  # Helper: builds the small info-icon tooltip trigger for an intervention.
  # Returns NULL when no tooltip text is defined for that key, so the icon
  # is silently skipped rather than appearing without a message.
  make_intervention_tip <- function(int_key, placement = "right") {
    tip_text <- intervention_tooltips[[int_key]]
    if (is.null(tip_text)) return(NULL)
    tooltip(
      bsicons::bs_icon("info-circle", class = "text-primary",
                       style = "cursor: help; font-size: 0.9em; margin-left: 4px;"),
      tip_text,
      placement = placement
    )
  }
  
  # Null-coalescing helper used by the PDF download handler.
  # Returns x unless x is NULL, then y. Lets us provide defaults for
  # reactive values that may still be NULL on first load.
  `%||%` <- function(x, y) if (is.null(x)) y else x
  
  # Store original baseline to scale proportionally
  original_population <- reactiveVal(5000000)
  original_baseline <- reactiveVal(NULL)
  
  # Store demographic parameters
  demographic_params <- reactiveValues(
    birth_rate = NULL,
    prop_pop_male = NULL,
    prop_pop_under_14 = NULL
  )
  
  # Store CSV-provided PLHIV value (NULL = not set, fall back to derived)
  plhiv_from_csv <- reactiveVal(NULL)        
  
  # Store country-specific calibration fields from preset. These are
  # invisible plumbing -- they ride along from the preset CSV but are not
  # exposed as editable inputs. Required for FOI strata partitioning,
  # testing yield, and re-testing split to remain country-specific. Without
  # them, calculate_scenario_outcomes falls back to generic defaults which
  # can over-estimate suppression delta and drive new_infections to zero
  # under aggressive testing scenarios.
  country_calibration <- reactiveVal(NULL)
  
  # Load regional preset when selected
  observeEvent(input$region, {
    preset <- regional_presets[[input$region]]
    updateNumericInput(session, "total_pop", value = preset$context$total_population)
    updateNumericInput(session, "prevalence", value = preset$context$hiv_prevalence * 100)
    updateNumericInput(session, "new_infections", value = preset$context$new_infections_per_year)
    updateNumericInput(session, "pct_diagnosed", value = preset$context$percent_diagnosed)  
    updateNumericInput(session, "pct_on_art", value = preset$context$percent_on_art)
    updateNumericInput(session, "pct_suppressed", value = preset$context$percent_suppressed)
    updateNumericInput(session, "aids_deaths", value = preset$context$aids_deaths_per_year)
    
    demographic_params$birth_rate <- preset$context$birth_rate
    demographic_params$prop_pop_male <- preset$context$prop_pop_male
    demographic_params$prop_pop_under_14 <- preset$context$prop_pop_under_14
    
    original_population(preset$context$total_population)
    original_baseline(preset$baseline)
    plhiv_from_csv(preset$context$plhiv)
    
    # Capture country-specific calibration fields from the preset.
    # Keeps these out of the UI surface area while ensuring they reach
    # calculate_scenario_outcomes() via context(). NULL values are passed
    # through unchanged -- the logic file handles missing fields with
    # defensive `if (!is.null(context$X))` checks and falls back to
    # hiv_params defaults when absent (e.g. for "Custom Country").
    country_calibration(list(
      circ_prevalence   = preset$context$circ_prevalence,
      prop_high_risk    = preset$context$prop_high_risk,
      rr_high           = preset$context$rr_high,
      test_yield        = preset$context$test_yield,
      prior_year_tests  = preset$context$prior_year_tests,
      prop_retesting    = preset$context$prop_retesting,
      yield_multipliers = preset$context$yield_multipliers,
      current_diagnoses = preset$context$current_diagnoses,
      anc_multiplier    = preset$context$anc_multiplier,   # ANC/adult HIV prev ratio, from CSV
      percent_on_art_pregnant = preset$context$percent_on_art_pregnant,
      use_mortality_calibration = preset$context$use_mortality_calibration,  # per-country mortality calibration flag
      art_cost_standard = preset$context$art_cost_standard,   # per-country ART unit cost (USD/PY); NULL/NA falls back to global in logic
      cost_overrides_test = preset$context$cost_overrides_test,  # named list of country-specific test unit cost overrides; absent/NULL means use global defaults
      bf_duration_months = preset$context$bf_duration_months  # country-specific BF duration (months); NULL falls back to hiv_params in logic
    ))
  }, ignoreInit = FALSE)
  
  # Reactive context
  context <- reactive({
    
    # Use CSV plhiv when available; fall back to derived for safety
    plhiv_val <- if (!is.null(plhiv_from_csv()) && !is.na(plhiv_from_csv())) {
      plhiv_from_csv()
    } else {
      input$total_pop * (input$prevalence / 100)
    }
    
    # Local alias for the calibration reactive -- evaluated once per
    # invocation. May be NULL on first render (before observeEvent fires)
    # or when no preset has been selected; handled per-field below.
    cc <- country_calibration()
    
    list(
      total_population = input$total_pop,
      hiv_prevalence = input$prevalence / 100,
      plhiv = plhiv_val,
      new_infections_per_year = input$new_infections,
      percent_on_art = input$pct_on_art,
      percent_on_art_pregnant = if (!is.null(cc) && !is.null(cc$percent_on_art_pregnant))
        cc$percent_on_art_pregnant else input$pct_on_art,
      percent_suppressed = input$pct_suppressed,
      aids_deaths_per_year = input$aids_deaths,
      percent_diagnosed = input$pct_diagnosed,
      birth_rate = demographic_params$birth_rate,
      prop_pop_male = demographic_params$prop_pop_male,
      prop_pop_under_14 = demographic_params$prop_pop_under_14,
      
      # Country-specific calibration carried through from preset.
      # `cc` may be NULL on first render (before observeEvent fires) or for
      # Custom Country; in both cases the nested $field lookups return NULL
      # and calculate_scenario_outcomes falls back to hiv_params defaults.
      # yield_multipliers must remain a named list, not unlisted.
      circ_prevalence   = if (!is.null(cc)) cc$circ_prevalence   else NULL,
      prop_high_risk    = if (!is.null(cc)) cc$prop_high_risk    else NULL,
      rr_high           = if (!is.null(cc)) cc$rr_high           else NULL,
      test_yield        = if (!is.null(cc)) cc$test_yield        else NULL,
      prior_year_tests  = if (!is.null(cc)) cc$prior_year_tests  else NULL,
      prop_retesting    = if (!is.null(cc)) cc$prop_retesting    else NULL,
      yield_multipliers = if (!is.null(cc)) cc$yield_multipliers else NULL,
      current_diagnoses = if (!is.null(cc)) cc$current_diagnoses else NULL,
      anc_multiplier    = if (!is.null(cc)) cc$anc_multiplier    else NULL,
      use_mortality_calibration = if (!is.null(cc)) cc$use_mortality_calibration else FALSE,
      # Country-specific ART unit cost. NULL when no preset selected (e.g.
      # Custom Country) -- the logic file's `%||% ART_COST_STANDARD` fallback
      # then uses the global Excel/intervention_params value.
      art_cost_standard = if (!is.null(cc)) cc$art_cost_standard else NULL,
      # Country-specific test unit cost overrides (named list, keyed by
      # intervention_key). NULL when no preset selected; in that case the
      # logic file's `context$cost_overrides_test[[int_key]] %||% intervention$unit_cost`
      # lookup safely returns NULL and falls back to the global value.
      cost_overrides_test = if (!is.null(cc)) cc$cost_overrides_test else NULL,
      # Country-specific breastfeeding duration in months. NULL when no preset
      # selected or when the CSV column is missing/blank -- logic file's
      # `%||% hiv_params$bf_duration_months %||% 18` chain handles the fallback.
      bf_duration_months = if (!is.null(cc)) cc$bf_duration_months else NULL
    )
  })
  
  # Calculate populations
  populations <- reactive({
    calculate_populations(context())
  })
  
  # ========================================================================
  # COMPREHENSIVE INPUT VALIDATION
  # ========================================================================
  
  # Validate percentage inputs (0-100%)
  observe({
    if (!is.null(input$prevalence) && !is.na(input$prevalence) && (input$prevalence < 0 || input$prevalence > 100)) {
      updateNumericInput(session, "prevalence", value = max(0, min(100, input$prevalence)))
    }
    if (!is.null(input$pct_diagnosed) && !is.na(input$pct_diagnosed) && (input$pct_diagnosed < 0 || input$pct_diagnosed > 100)) {
      updateNumericInput(session, "pct_diagnosed", value = max(0, min(100, input$pct_diagnosed)))
    }
    if (!is.null(input$pct_on_art) && !is.na(input$pct_on_art) && (input$pct_on_art < 0 || input$pct_on_art > 100)) {
      updateNumericInput(session, "pct_on_art", value = max(0, min(100, input$pct_on_art)))
    }
    if (!is.null(input$pct_suppressed) && !is.na(input$pct_suppressed) && (input$pct_suppressed < 0 || input$pct_suppressed > 100)) {
      updateNumericInput(session, "pct_suppressed", value = max(0, min(100, input$pct_suppressed)))
    }
  })
  
  # Validate non-negative absolute inputs
  observe({
    if (!is.null(input$total_pop) && !is.na(input$total_pop) && input$total_pop < 0) {
      updateNumericInput(session, "total_pop", value = 0)
    }
    if (!is.null(input$new_infections) && !is.na(input$new_infections) && input$new_infections < 0) {
      updateNumericInput(session, "new_infections", value = 0)
    }
    if (!is.null(input$aids_deaths) && !is.na(input$aids_deaths) && input$aids_deaths < 0) {
      updateNumericInput(session, "aids_deaths", value = 0)
    }
  })
  
  # ========================================================================
  # BASELINE INTERVENTION VALIDATION
  # ========================================================================
  
  # Validate all baseline interventions
  observe({
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
  })
  
  # Baseline PrEP Validation - Oral
  observe({
    ctx <- context()
    if (is.null(ctx$total_population) || is.null(ctx$prop_pop_under_14)) return()
    
    adult_pop <- ctx$total_population * (1 - ctx$prop_pop_under_14/100)
    
    isolate({
      prep_oral <- input$baseline_prep_oral
      prep_lena <- input$baseline_prep_lenacapavir
      
      if (!is.null(prep_oral) && !is.null(prep_lena) && !is.na(prep_oral) && !is.na(prep_lena)) {
        if (prep_oral + prep_lena > adult_pop) {
          max_oral <- max(0, adult_pop - prep_lena)
          updateNumericInput(session, "baseline_prep_oral", 
                             value = round(max_oral),
                             max = round(adult_pop))
          showNotification(
            paste0("Baseline: Oral PrEP capped at ", format(round(max_oral), big.mark = ","), 
                   " (max with current Lenacapavir: ", format(round(prep_lena), big.mark = ","), ")"),
            type = "warning",
            duration = 4
          )
        }
      }
    })
  }) %>% bindEvent(input$baseline_prep_oral)
  
  # Baseline PrEP Validation - Lenacapavir
  observe({
    ctx <- context()
    if (is.null(ctx$total_population) || is.null(ctx$prop_pop_under_14)) return()
    
    adult_pop <- ctx$total_population * (1 - ctx$prop_pop_under_14/100)
    
    isolate({
      prep_oral <- input$baseline_prep_oral
      prep_lena <- input$baseline_prep_lenacapavir
      
      if (!is.null(prep_oral) && !is.null(prep_lena) && !is.na(prep_oral) && !is.na(prep_lena)) {
        if (prep_oral + prep_lena > adult_pop) {
          max_lena <- max(0, adult_pop - prep_oral)
          updateNumericInput(session, "baseline_prep_lenacapavir", 
                             value = round(max_lena),
                             max = round(adult_pop))
          showNotification(
            paste0("Baseline: Lenacapavir capped at ", format(round(max_lena), big.mark = ","),
                   " (max with current oral PrEP: ", format(round(prep_oral), big.mark = ","), ")"),
            type = "warning",
            duration = 4
          )
        }
      }
    })
  }) %>% bindEvent(input$baseline_prep_lenacapavir)
  
  # Baseline VMMC Validation
  observe({
    ctx <- context()
    if (is.null(ctx$total_population) || is.null(ctx$prop_pop_male) ||
        is.null(ctx$circ_prevalence)) return()
    
    # Cap = all uncircumcised males (HIV+ and HIV-).
    # prop_pop_male and circ_prevalence are stored as PERCENTAGES (e.g. 50, 30)
    # — divide by 100. hiv_prevalence is stored as a PROPORTION (no /100).
    uncirc_males_all <- ctx$total_population *
      (ctx$prop_pop_male / 100) *
      (1 - ctx$circ_prevalence / 100)
    
    isolate({
      vmmc_val <- input$baseline_vmmc
      
      if (!is.null(vmmc_val) && !is.na(vmmc_val)) {
        if (vmmc_val > uncirc_males_all) {
          updateNumericInput(session, "baseline_vmmc",
                             value = round(uncirc_males_all),
                             max = round(uncirc_males_all))
          showNotification(
            paste0("Baseline: VMMC capped at ", format(round(uncirc_males_all), big.mark = ","),
                   " (total uncircumcised males in population)"),
            type = "warning",
            duration = 4
          )
        }
      }
    })
  }) %>% bindEvent(input$baseline_vmmc)
  # Baseline DSD Validation - 3 month
  observe({
    isolate({
      mmd3 <- input$baseline_mmd_3month
      mmd6 <- input$baseline_mmd_6month
      mmd12 <- input$baseline_mmd_12month
      
      if (!is.null(mmd3) && !is.null(mmd6) && !is.null(mmd12) &&
          !is.na(mmd3)  && !is.na(mmd6)  && !is.na(mmd12)) {
        if (mmd3 + mmd6 + mmd12 > 100) {
          max_mmd3 <- max(0, 100 - mmd6 - mmd12)
          updateNumericInput(session, "baseline_mmd_3month", 
                             value = round(max_mmd3, 1),
                             max = 100)
          showNotification(
            paste0("Baseline: 3-month MMD capped at ", round(max_mmd3, 1), 
                   "% (MMD enrolment cannot exceed 100%)"),
            type = "warning",
            duration = 4
          )
        }
      }
    })
  }) %>% bindEvent(input$baseline_mmd_3month)
  
  # Baseline DSD Validation - 6 month
  observe({
    isolate({
      mmd3 <- input$baseline_mmd_3month
      mmd6 <- input$baseline_mmd_6month
      mmd12 <- input$baseline_mmd_12month
      
      if (!is.null(mmd3) && !is.null(mmd6) && !is.null(mmd12) &&
          !is.na(mmd3)  && !is.na(mmd6)  && !is.na(mmd12)) {
        if (mmd3 + mmd6 + mmd12 > 100) {
          max_mmd6 <- max(0, 100 - mmd3 - mmd12)
          updateNumericInput(session, "baseline_mmd_6month", 
                             value = round(max_mmd6, 1),
                             max = 100)
          showNotification(
            paste0("Baseline: 6-month MMD capped at ", round(max_mmd6, 1), 
                   "% (MMD enrolment cannot exceed 100%)"),
            type = "warning",
            duration = 4
          )
        }
      }
    })
  }) %>% bindEvent(input$baseline_mmd_6month)
  
  # Baseline DSD Validation - 12 month
  observe({
    isolate({
      mmd3 <- input$baseline_mmd_3month
      mmd6 <- input$baseline_mmd_6month
      mmd12 <- input$baseline_mmd_12month
      
      if (!is.null(mmd3) && !is.null(mmd6) && !is.null(mmd12) &&
          !is.na(mmd3)  && !is.na(mmd6)  && !is.na(mmd12)) {
        if (mmd3 + mmd6 + mmd12 > 100) {
          max_mmd12 <- max(0, 100 - mmd3 - mmd6)
          updateNumericInput(session, "baseline_mmd_12month", 
                             value = round(max_mmd12, 1),
                             max = 100)
          showNotification(
            paste0("Baseline: 12-month MMD capped at ", round(max_mmd12, 1), 
                   "% (MMD enrolment cannot exceed 100%)"),
            type = "warning",
            duration = 4
          )
        }
      }
    })
  }) %>% bindEvent(input$baseline_mmd_12month)
  
  # Baseline DSD Validation - Community pick-up
  observe({
    isolate({
      # Community pickup is independent of the MMD sum constraint.
      # MMD-3/6/12 must sum to <= 100% (mutually exclusive enrolment),
      # but community pickup is a delivery mode layered onto MMD
      # enrolment and is capped at 100% on its own.
      cpu <- input$baseline_community_pickup
      
      if (!is.null(cpu) && !is.na(cpu)) {
        if (cpu > 100) {
          max_cpu <- 100
          updateNumericInput(session, "baseline_community_pickup",
                             value = max_cpu,
                             max = 100)
          showNotification(
            paste0("Baseline: Community pick-up capped at 100%"),
            type = "warning",
            duration = 4
          )
        }
      }
    })
  }) %>% bindEvent(input$baseline_community_pickup)
  
  # ========================================================================
  # SCENARIO 1 INTERVENTION VALIDATION
  # ========================================================================
  
  # Validate all scenario 1 interventions
  observe({
    for (group_key in names(intervention_groups)) {
      group <- intervention_groups[[group_key]]
      for (int_key in names(group$interventions)) {
        intervention <- group$interventions[[int_key]]
        value <- input[[paste0("scenario1_", int_key)]]
        
        if (!is.null(value) && !is.na(value) && value < 0) {
          updateNumericInput(session, paste0("scenario1_", int_key), value = 0)
        }
        
        if (intervention$type == "coverage") {
          if (!is.null(value) && !is.na(value) && value > 100) {
            updateNumericInput(session, paste0("scenario1_", int_key), value = 100)
          }
        }
      }
    }
  })
  
  # Scenario 1 PrEP Validation - Oral
  observe({
    ctx <- context()
    if (is.null(ctx$total_population) || is.null(ctx$prop_pop_under_14)) return()
    
    adult_pop <- ctx$total_population * (1 - ctx$prop_pop_under_14/100)
    
    isolate({
      prep_oral <- input$scenario1_prep_oral
      prep_lena <- input$scenario1_prep_lenacapavir
      
      if (!is.null(prep_oral) && !is.null(prep_lena) && !is.na(prep_oral) && !is.na(prep_lena)) {
        if (prep_oral + prep_lena > adult_pop) {
          max_oral <- max(0, adult_pop - prep_lena)
          updateNumericInput(session, "scenario1_prep_oral", 
                             value = round(max_oral),
                             max = round(adult_pop))
          showNotification(
            paste0("Scenario 1: Oral PrEP capped at ", format(round(max_oral), big.mark = ",")),
            type = "warning",
            duration = 4
          )
        }
      }
    })
  }) %>% bindEvent(input$scenario1_prep_oral)
  
  # Scenario 1 PrEP Validation - Lenacapavir
  observe({
    ctx <- context()
    if (is.null(ctx$total_population) || is.null(ctx$prop_pop_under_14)) return()
    
    adult_pop <- ctx$total_population * (1 - ctx$prop_pop_under_14/100)
    
    isolate({
      prep_oral <- input$scenario1_prep_oral
      prep_lena <- input$scenario1_prep_lenacapavir
      
      if (!is.null(prep_oral) && !is.null(prep_lena) && !is.na(prep_oral) && !is.na(prep_lena)) {
        if (prep_oral + prep_lena > adult_pop) {
          max_lena <- max(0, adult_pop - prep_oral)
          updateNumericInput(session, "scenario1_prep_lenacapavir", 
                             value = round(max_lena),
                             max = round(adult_pop))
          showNotification(
            paste0("Scenario 1: Lenacapavir capped at ", format(round(max_lena), big.mark = ",")),
            type = "warning",
            duration = 4
          )
        }
      }
    })
  }) %>% bindEvent(input$scenario1_prep_lenacapavir)
  
  # Scen 1 VMMC Validation
  observe({
    ctx <- context()
    if (is.null(ctx$total_population) || is.null(ctx$prop_pop_male) ||
        is.null(ctx$circ_prevalence)) return()
    
    # Cap = all uncircumcised males (HIV+ and HIV-).
    # prop_pop_male and circ_prevalence are stored as PERCENTAGES (e.g. 50, 30)
    # — divide by 100. hiv_prevalence is stored as a PROPORTION (no /100).
    uncirc_males_all <- ctx$total_population *
      (ctx$prop_pop_male / 100) *
      (1 - ctx$circ_prevalence / 100)
    
    isolate({
      vmmc_val <- input$scenario1_vmmc
      
      if (!is.null(vmmc_val) && !is.na(vmmc_val)) {
        if (vmmc_val > uncirc_males_all) {
          updateNumericInput(session, "scenario2_vmmc",
                             value = round(uncirc_males_all),
                             max = round(uncirc_males_all))
          showNotification(
            paste0("Scenario 1: VMMC capped at ", format(round(uncirc_males_all), big.mark = ","),
                   " (total uncircumcised males in population)"),
            type = "warning",
            duration = 4
          )
        }
      }
    })
  }) %>% bindEvent(input$scenario1_vmmc)
  
  
  # Scenario 1 DSD Validation
  observe({
    isolate({
      mmd3 <- input$scenario1_mmd_3month
      mmd6 <- input$scenario1_mmd_6month
      mmd12 <- input$scenario1_mmd_12month
      
      if (!is.null(mmd3) && !is.null(mmd6) && !is.null(mmd12) &&
          !is.na(mmd3)  && !is.na(mmd6)  && !is.na(mmd12)) {
        if (mmd3 + mmd6 + mmd12 > 100) {
          max_mmd3 <- max(0, 100 - mmd6 - mmd12)
          updateNumericInput(session, "scenario1_mmd_3month", 
                             value = round(max_mmd3, 1),
                             max = 100)
          showNotification(
            paste0("Scenario 1: 3-month MMD capped at ", round(max_mmd3, 1), "%"),
            type = "warning",
            duration = 4
          )
        }
      }
    })
  }) %>% bindEvent(input$scenario1_mmd_3month)
  
  observe({
    isolate({
      mmd3 <- input$scenario1_mmd_3month
      mmd6 <- input$scenario1_mmd_6month
      mmd12 <- input$scenario1_mmd_12month
      
      if (!is.null(mmd3) && !is.null(mmd6) && !is.null(mmd12) &&
          !is.na(mmd3)  && !is.na(mmd6)  && !is.na(mmd12)) {
        if (mmd3 + mmd6 + mmd12 > 100) {
          max_mmd6 <- max(0, 100 - mmd3 - mmd12)
          updateNumericInput(session, "scenario1_mmd_6month", 
                             value = round(max_mmd6, 1),
                             max = 100)
          showNotification(
            paste0("Scenario 1: 6-month MMD capped at ", round(max_mmd6, 1), "%"),
            type = "warning",
            duration = 4
          )
        }
      }
    })
  }) %>% bindEvent(input$scenario1_mmd_6month)
  
  observe({
    isolate({
      mmd3 <- input$scenario1_mmd_3month
      mmd6 <- input$scenario1_mmd_6month
      mmd12 <- input$scenario1_mmd_12month
      
      if (!is.null(mmd3) && !is.null(mmd6) && !is.null(mmd12) &&
          !is.na(mmd3)  && !is.na(mmd6)  && !is.na(mmd12)) {
        if (mmd3 + mmd6 + mmd12 > 100) {
          max_mmd12 <- max(0, 100 - mmd3 - mmd6)
          updateNumericInput(session, "scenario1_mmd_12month", 
                             value = round(max_mmd12, 1),
                             max = 100)
          showNotification(
            paste0("Scenario 1: 12-month MMD capped at ", round(max_mmd12, 1), "%"),
            type = "warning",
            duration = 4
          )
        }
      }
    })
  }) %>% bindEvent(input$scenario1_mmd_12month)
  
  observe({
    isolate({
      # Community pickup is independent of the MMD sum constraint.
      # MMD-3/6/12 must sum to <= 100% (mutually exclusive enrolment),
      # but community pickup is a delivery mode layered onto MMD
      # enrolment and is capped at 100% on its own.
      cpu <- input$scenario1_community_pickup
      
      if (!is.null(cpu) && !is.na(cpu)) {
        if (cpu > 100) {
          max_cpu <- 100
          updateNumericInput(session, "scenario1_community_pickup",
                             value = max_cpu,
                             max = 100)
          showNotification(
            paste0("Scenario 1: Community pick-up capped at ", round(max_cpu, 1), "%"),
            type = "warning",
            duration = 4
          )
        }
      }
    })
  }) %>% bindEvent(input$scenario1_community_pickup)
  
  # ========================================================================
  # SCENARIO 2 INTERVENTION VALIDATION
  # ========================================================================
  
  # Validate all scenario 2 interventions
  observe({
    for (group_key in names(intervention_groups)) {
      group <- intervention_groups[[group_key]]
      for (int_key in names(group$interventions)) {
        intervention <- group$interventions[[int_key]]
        value <- input[[paste0("scenario2_", int_key)]]
        
        if (!is.null(value) && !is.na(value) && value < 0) {
          updateNumericInput(session, paste0("scenario2_", int_key), value = 0)
        }
        
        if (intervention$type == "coverage") {
          if (!is.null(value) && !is.na(value) && value > 100) {
            updateNumericInput(session, paste0("scenario2_", int_key), value = 100)
          }
        }
      }
    }
  })
  
  # Scenario 2 PrEP Validation - Oral
  observe({
    ctx <- context()
    if (is.null(ctx$total_population) || is.null(ctx$prop_pop_under_14)) return()
    
    adult_pop <- ctx$total_population * (1 - ctx$prop_pop_under_14/100)
    
    isolate({
      prep_oral <- input$scenario2_prep_oral
      prep_lena <- input$scenario2_prep_lenacapavir
      
      if (!is.null(prep_oral) && !is.null(prep_lena) && !is.na(prep_oral) && !is.na(prep_lena)) {
        if (prep_oral + prep_lena > adult_pop) {
          max_oral <- max(0, adult_pop - prep_lena)
          updateNumericInput(session, "scenario2_prep_oral", 
                             value = round(max_oral),
                             max = round(adult_pop))
          showNotification(
            paste0("Scenario 2: Oral PrEP capped at ", format(round(max_oral), big.mark = ",")),
            type = "warning",
            duration = 4
          )
        }
      }
    })
  }) %>% bindEvent(input$scenario2_prep_oral)
  
  # Scenario 2 PrEP Validation - Lenacapavir
  observe({
    ctx <- context()
    if (is.null(ctx$total_population) || is.null(ctx$prop_pop_under_14)) return()
    
    adult_pop <- ctx$total_population * (1 - ctx$prop_pop_under_14/100)
    
    isolate({
      prep_oral <- input$scenario2_prep_oral
      prep_lena <- input$scenario2_prep_lenacapavir
      
      if (!is.null(prep_oral) && !is.null(prep_lena) && !is.na(prep_oral) && !is.na(prep_lena)) {
        if (prep_oral + prep_lena > adult_pop) {
          max_lena <- max(0, adult_pop - prep_oral)
          updateNumericInput(session, "scenario2_prep_lenacapavir", 
                             value = round(max_lena),
                             max = round(adult_pop))
          showNotification(
            paste0("Scenario 2: Lenacapavir capped at ", format(round(max_lena), big.mark = ",")),
            type = "warning",
            duration = 4
          )
        }
      }
    })
  }) %>% bindEvent(input$scenario2_prep_lenacapavir)
  
  # Scen 2 VMMC Validation
  observe({
    ctx <- context()
    if (is.null(ctx$total_population) || is.null(ctx$prop_pop_male) ||
        is.null(ctx$circ_prevalence)) return()
    
    # Cap = all uncircumcised males (HIV+ and HIV-).
    # prop_pop_male and circ_prevalence are stored as PERCENTAGES (e.g. 50, 30)
    # — divide by 100. hiv_prevalence is stored as a PROPORTION (no /100).
    uncirc_males_all <- ctx$total_population *
      (ctx$prop_pop_male / 100) *
      (1 - ctx$circ_prevalence / 100)
    
    isolate({
      vmmc_val <- input$scenario2_vmmc
      
      if (!is.null(vmmc_val) && !is.na(vmmc_val)) {
        if (vmmc_val > uncirc_males_all) {
          updateNumericInput(session, "scenario2_vmmc",
                             value = round(uncirc_males_all),
                             max = round(uncirc_males_all))
          showNotification(
            paste0("Scenario 2: VMMC capped at ", format(round(uncirc_males_all), big.mark = ","),
                   " (total uncircumcised males in population)"),
            type = "warning",
            duration = 4
          )
        }
      }
    })
  }) %>% bindEvent(input$scenario2_vmmc)
  
  # Scenario 2 DSD Validation
  observe({
    isolate({
      mmd3 <- input$scenario2_mmd_3month
      mmd6 <- input$scenario2_mmd_6month
      mmd12 <- input$scenario2_mmd_12month
      
      if (!is.null(mmd3) && !is.null(mmd6) && !is.null(mmd12) &&
          !is.na(mmd3)  && !is.na(mmd6)  && !is.na(mmd12)) {
        if (mmd3 + mmd6 + mmd12 > 100) {
          max_mmd3 <- max(0, 100 - mmd6 - mmd12)
          updateNumericInput(session, "scenario2_mmd_3month", 
                             value = round(max_mmd3, 1),
                             max = 100)
          showNotification(
            paste0("Scenario 2: 3-month MMD capped at ", round(max_mmd3, 1), "%"),
            type = "warning",
            duration = 4
          )
        }
      }
    })
  }) %>% bindEvent(input$scenario2_mmd_3month)
  
  observe({
    isolate({
      mmd3 <- input$scenario2_mmd_3month
      mmd6 <- input$scenario2_mmd_6month
      mmd12 <- input$scenario2_mmd_12month
      
      if (!is.null(mmd3) && !is.null(mmd6) && !is.null(mmd12) &&
          !is.na(mmd3)  && !is.na(mmd6)  && !is.na(mmd12)) {
        if (mmd3 + mmd6 + mmd12 > 100) {
          max_mmd6 <- max(0, 100 - mmd3 - mmd12)
          updateNumericInput(session, "scenario2_mmd_6month", 
                             value = round(max_mmd6, 1),
                             max = 100)
          showNotification(
            paste0("Scenario 2: 6-month MMD capped at ", round(max_mmd6, 1), "%"),
            type = "warning",
            duration = 4
          )
        }
      }
    })
  }) %>% bindEvent(input$scenario2_mmd_6month)
  
  observe({
    isolate({
      mmd3 <- input$scenario2_mmd_3month
      mmd6 <- input$scenario2_mmd_6month
      mmd12 <- input$scenario2_mmd_12month
      
      if (!is.null(mmd3) && !is.null(mmd6) && !is.null(mmd12) &&
          !is.na(mmd3)  && !is.na(mmd6)  && !is.na(mmd12)) {
        if (mmd3 + mmd6 + mmd12 > 100) {
          max_mmd12 <- max(0, 100 - mmd3 - mmd6)
          updateNumericInput(session, "scenario2_mmd_12month", 
                             value = round(max_mmd12, 1),
                             max = 100)
          showNotification(
            paste0("Scenario 2: 12-month MMD capped at ", round(max_mmd12, 1), "%"),
            type = "warning",
            duration = 4
          )
        }
      }
    })
  }) %>% bindEvent(input$scenario2_mmd_12month)
  
  observe({
    isolate({
      # Community pickup is independent of the MMD sum constraint.
      # MMD-3/6/12 must sum to <= 100% (mutually exclusive enrolment),
      # but community pickup is a delivery mode layered onto MMD
      # enrolment and is capped at 100% on its own.
      cpu <- input$scenario2_community_pickup
      
      if (!is.null(cpu) && !is.na(cpu)) {
        if (cpu > 100) {
          max_cpu <- 100
          updateNumericInput(session, "scenario2_community_pickup",
                             value = max_cpu,
                             max = 100)
          showNotification(
            paste0("Scenario 2: Community pick-up capped at ", round(max_cpu, 1), "%"),
            type = "warning",
            duration = 4
          )
        }
      }
    })
  }) %>% bindEvent(input$scenario2_community_pickup)
  
  # ========================================================================
  # BASELINE VALUES AND UI GENERATION
  # ========================================================================
  
  # Scaled baseline values
  baseline_values <- reactive({
    if (is.null(original_baseline())) return(list())
    
    scale_factor <- input$total_pop / original_population()
    baseline <- original_baseline()
    
    scaled <- lapply(names(baseline), function(key) {
      for (group_key in names(intervention_groups)) {
        group <- intervention_groups[[group_key]]
        if (key %in% names(group$interventions)) {
          intervention <- group$interventions[[key]]
          if (intervention$type == "absolute") {
            return(round(baseline[[key]] * scale_factor))
          } else {
            return(baseline[[key]])
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
        
        # Set min and max based on type
        min_val <- 0
        max_val <- if(intervention$type == "coverage") 100 else NA
        
        # Build a label that includes a small info icon with a tooltip.
        label_with_tip <- tagList(
          paste0(intervention$name, " (", intervention$unit_label, ")"),
          make_intervention_tip(int_key)
        )
        
        numericInput(
          paste0("baseline_", int_key),
          label = label_with_tip,
          value = value,
          min = min_val,
          max = max_val,
          step = if(intervention$type == "coverage") 0.1 else 1
        )
      })
      
      if (group_key == "prevention") {
        interventions_ui <- c(interventions_ui, list(
          div(
            style = "margin-top: 15px; padding: 10px; background-color: #f8f9fa; border-radius: 5px;",
            h6("Total PrEP Coverage:", style = "margin-bottom: 5px;"),
            uiOutput("prep_total_baseline")
          )
        ))
      }
      
      if (group_key == "treatment_monitoring") {
        interventions_ui <- c(interventions_ui, list(
          div(
            style = "margin-top: 15px; padding: 10px; background-color: #f8f9fa; border-radius: 5px;",
            h6("Total DSD Coverage:", style = "margin-bottom: 5px;"),
            uiOutput("mmd_total_baseline")
          )
        ))
      }
      
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
        
        # Set min and max based on type
        min_val <- 0
        max_val <- if(intervention$type == "coverage") 100 else NA
        
        layout_columns(
          col_widths = c(4, 4, 4),
          div(
            style = "padding-top: 25px; font-size: 0.9em;",
            tagList(strong(intervention$name), make_intervention_tip(int_key)),
            br(),
            span(style = "color: #666;", "Baseline: ", format(round(base_value, 1), big.mark = ",")),
            br(),
            span(style = "color: #999; font-size: 0.85em;", intervention$unit_label)
          ),
          div(
            numericInput(
              paste0("scenario1_", int_key),
              label = "Scenario 1",
              value = base_value,
              min = min_val,
              max = max_val,
              step = if(intervention$type == "coverage") 0.1 else 1
            )
          ),
          div(
            numericInput(
              paste0("scenario2_", int_key),
              label = "Scenario 2",
              value = base_value,
              min = min_val,
              max = max_val,
              step = if(intervention$type == "coverage") 0.1 else 1
            )
          )
        )
      })
      
      if (group_key == "prevention") {
        interventions_ui <- c(interventions_ui, list(
          layout_columns(
            col_widths = c(4, 4, 4),
            div(),
            div(
              h6("Total PrEP:", style = "margin-top: 15px;"),
              uiOutput("prep_total_scenario1")
            ),
            div(
              h6("Total PrEP:", style = "margin-top: 15px;"),
              uiOutput("prep_total_scenario2")
            )
          )
        ))
      }
      
      if (group_key == "treatment_monitoring") {
        interventions_ui <- c(interventions_ui, list(
          layout_columns(
            col_widths = c(4, 4, 4),
            div(),
            div(
              h6("Total MMD:", style = "margin-top: 15px;"),
              uiOutput("mmd_total_scenario1")
            ),
            div(
              h6("Total MMD:", style = "margin-top: 15px;"),
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
  
  # ========================================================================
  # PREP AND MMD TOTAL INDICATORS
  # ========================================================================
  
  output$prep_total_baseline <- renderUI({
    prep_oral <- input$baseline_prep_oral
    prep_lena <- input$baseline_prep_lenacapavir
    
    if (is.null(prep_oral) || is.null(prep_lena)) return(NULL)
    
    ctx <- context()
    if (is.null(ctx$total_population) || is.null(ctx$prop_pop_under_14)) return(NULL)
    
    adult_pop <- ctx$total_population * (1 - ctx$prop_pop_under_14/100)
    total_prep <- prep_oral + prep_lena
    pct <- (total_prep / adult_pop) * 100
    
    color <- ifelse(total_prep > adult_pop, "red", 
                    ifelse(pct > 90, "orange", "green"))
    
    tags$div(
      style = paste0("color: ", color, "; font-weight: bold;"),
      paste0(format(round(total_prep), big.mark = ","), " people"),
      br(),
      span(style = "font-size: 0.85em;",
           paste0("(", round(pct, 1), "% of adult pop)"))
    )
  })
  
  output$prep_total_scenario1 <- renderUI({
    prep_oral <- input$scenario1_prep_oral
    prep_lena <- input$scenario1_prep_lenacapavir
    
    if (is.null(prep_oral) || is.null(prep_lena)) return(NULL)
    
    ctx <- context()
    if (is.null(ctx$total_population) || is.null(ctx$prop_pop_under_14)) return(NULL)
    
    adult_pop <- ctx$total_population * (1 - ctx$prop_pop_under_14/100)
    total_prep <- prep_oral + prep_lena
    pct <- (total_prep / adult_pop) * 100
    
    color <- ifelse(total_prep > adult_pop, "red", 
                    ifelse(pct > 90, "orange", "green"))
    
    tags$div(
      style = paste0("color: ", color, "; font-weight: bold;"),
      paste0(format(round(total_prep), big.mark = ","), " people"),
      br(),
      span(style = "font-size: 0.85em;",
           paste0("(", round(pct, 1), "% of adult pop)"))
    )
  })
  
  output$prep_total_scenario2 <- renderUI({
    prep_oral <- input$scenario2_prep_oral
    prep_lena <- input$scenario2_prep_lenacapavir
    
    if (is.null(prep_oral) || is.null(prep_lena)) return(NULL)
    
    ctx <- context()
    if (is.null(ctx$total_population) || is.null(ctx$prop_pop_under_14)) return(NULL)
    
    adult_pop <- ctx$total_population * (1 - ctx$prop_pop_under_14/100)
    total_prep <- prep_oral + prep_lena
    pct <- (total_prep / adult_pop) * 100
    
    color <- ifelse(total_prep > adult_pop, "red", 
                    ifelse(pct > 90, "orange", "green"))
    
    tags$div(
      style = paste0("color: ", color, "; font-weight: bold;"),
      paste0(format(round(total_prep), big.mark = ","), " people"),
      br(),
      span(style = "font-size: 0.85em;",
           paste0("(", round(pct, 1), "% of adult pop)"))
    )
  })
  
  output$mmd_total_baseline <- renderUI({
    mmd3  <- input$baseline_mmd_3month
    mmd6  <- input$baseline_mmd_6month
    mmd12 <- input$baseline_mmd_12month
    cpu   <- input$baseline_community_pickup
    
    if (is.null(mmd3) || is.null(mmd6) || is.null(mmd12) ||
        is.null(cpu)) return(NULL)
    
    total <- mmd3 + mmd6 + mmd12
    color <- ifelse(total > 100, "red", ifelse(total > 90, "orange", "green"))
    
    tags$div(
      style = paste0("color: ", color, "; font-weight: bold;"),
      paste0(round(total, 1), "% of stable clients on MMD")
    )
  })
  
  output$mmd_total_scenario1 <- renderUI({
    mmd3  <- input$scenario1_mmd_3month
    mmd6  <- input$scenario1_mmd_6month
    mmd12 <- input$scenario1_mmd_12month
    cpu   <- input$scenario1_community_pickup
    
    if (is.null(mmd3) || is.null(mmd6) || is.null(mmd12) ||
        is.null(cpu)) return(NULL)
    
    total <- mmd3 + mmd6 + mmd12
    color <- ifelse(total > 100, "red", ifelse(total > 90, "orange", "green"))
    
    tags$div(
      style = paste0("color: ", color, "; font-weight: bold;"),
      paste0(round(total, 1), "% of stable clients on MMD")
    )
  })
  
  output$mmd_total_scenario2 <- renderUI({
    mmd3  <- input$scenario2_mmd_3month
    mmd6  <- input$scenario2_mmd_6month
    mmd12 <- input$scenario2_mmd_12month
    cpu   <- input$scenario2_community_pickup
    
    if (is.null(mmd3) || is.null(mmd6) || is.null(mmd12) ||
        is.null(cpu)) return(NULL)
    
    total <- mmd3 + mmd6 + mmd12 
    color <- ifelse(total > 100, "red", ifelse(total > 90, "orange", "green"))
    
    tags$div(
      style = paste0("color: ", color, "; font-weight: bold;"),
      paste0(round(total, 1), "% of stable clients on MMD")
    )
  })
  
  # ========================================================================
  # COLLECT INPUT VALUES
  # ========================================================================
  
  baseline_input_values <- reactive({
    # Fallback source: the scaled regional preset. Used when the Baseline tab
    # hasn't been rendered yet (so input$baseline_* is still NULL) — without
    # this, a user who jumps straight to Scenarios sees baseline=0 for every
    # intervention and any scenario diff is computed against zero, not the
    # preset.
    preset <- baseline_values()
    
    baseline <- list()
    for (group_key in names(intervention_groups)) {
      group <- intervention_groups[[group_key]]
      for (int_key in names(group$interventions)) {
        input_id <- paste0("baseline_", int_key)
        value <- input[[input_id]]
        if (is.null(value) || is.na(value)) {
          # widget hasn't rendered yet (or was cleared) — use the preset
          value <- preset[[int_key]]
        }
        baseline[[int_key]] <- ifelse(is.null(value) || is.na(value), 0, value)
      }
    }
    baseline
  })
  
  observeEvent(baseline_values(), {
    baseline <- baseline_values()
    if (length(baseline) == 0) return()
    
    for (group_key in names(intervention_groups)) {
      group <- intervention_groups[[group_key]]
      for (int_key in names(group$interventions)) {
        baseline_value <- baseline[[int_key]]
        if (!is.null(baseline_value) && !is.na(baseline_value)) {
          updateNumericInput(session, paste0("baseline_", int_key), value = baseline_value)
          updateNumericInput(session, paste0("scenario1_", int_key), value = baseline_value)
          updateNumericInput(session, paste0("scenario2_", int_key), value = baseline_value)
        }
      }
    }
  }, ignoreInit = TRUE)
  
  scenario1_values <- reactive({
    preset <- baseline_values()
    scenario <- list()
    for (group_key in names(intervention_groups)) {
      group <- intervention_groups[[group_key]]
      for (int_key in names(group$interventions)) {
        input_id <- paste0("scenario1_", int_key)
        value <- input[[input_id]]
        if (is.null(value) || is.na(value)) {
          # Scenario widget hasn't rendered or is blank — try the baseline
          # widget; if THAT hasn't rendered either, fall back to the preset.
          baseline_value <- input[[paste0("baseline_", int_key)]]
          if (is.null(baseline_value) || is.na(baseline_value)) {
            baseline_value <- preset[[int_key]]
          }
          value <- ifelse(is.null(baseline_value) || is.na(baseline_value), 0, baseline_value)
        }
        scenario[[int_key]] <- value
      }
    }
    scenario
  })
  
  scenario2_values <- reactive({
    preset <- baseline_values()
    scenario <- list()
    for (group_key in names(intervention_groups)) {
      group <- intervention_groups[[group_key]]
      for (int_key in names(group$interventions)) {
        input_id <- paste0("scenario2_", int_key)
        value <- input[[input_id]]
        if (is.null(value) || is.na(value)) {
          baseline_value <- input[[paste0("baseline_", int_key)]]
          if (is.null(baseline_value) || is.na(baseline_value)) {
            baseline_value <- preset[[int_key]]
          }
          value <- ifelse(is.null(baseline_value) || is.na(baseline_value), 0, baseline_value)
        }
        scenario[[int_key]] <- value
      }
    }
    scenario
  })
  
  # ========================================================================
  # CALCULATE OUTCOMES
  # ========================================================================
  
  outcomes_baseline <- reactive({
    req(populations())
    baseline <- baseline_input_values()
    req(baseline)
    calculate_scenario_outcomes(context(), baseline, populations(),
                                is_baseline           = TRUE,
                                baseline_interventions = baseline)
  })
  
  outcomes_scenario1 <- reactive({
    req(populations())
    scenario <- scenario1_values()
    req(scenario)
    calculate_scenario_outcomes(context(), scenario, populations(),
                                baseline_interventions          = baseline_input_values(),
                                baseline_additional_suppressed  = outcomes_baseline()$additional_suppressed,
                                baseline_end_suppressed         = outcomes_baseline()$end_suppressed,
                                mortality_calibration_factor    = outcomes_baseline()$mortality_calibration_factor)
  })
  
  outcomes_scenario2 <- reactive({
    req(populations())
    scenario <- scenario2_values()
    req(scenario)
    calculate_scenario_outcomes(context(), scenario, populations(),
                                baseline_interventions          = baseline_input_values(),
                                baseline_additional_suppressed  = outcomes_baseline()$additional_suppressed,
                                baseline_end_suppressed         = outcomes_baseline()$end_suppressed,
                                mortality_calibration_factor    = outcomes_baseline()$mortality_calibration_factor)
  })
  
  diff_scenario1 <- reactive({
    calculate_scenario_difference(outcomes_scenario1(), outcomes_baseline())
  })
  
  diff_scenario2 <- reactive({
    calculate_scenario_difference(outcomes_scenario2(), outcomes_baseline())
  })
  
  # ========================================================================
  # USAGE LOGGING: results-view trigger
  # ------------------------------------------------------------------------
  # Fires each time the user opens the "Results Comparison" tab. Captures
  # the current state of both scenarios + baseline, plus the delta from
  # the first-view snapshot.
  #
  # ignoreInit = TRUE: skip the initial reactive flush so we don't log a
  # spurious "User Guide" tab activation on app load.
  # ========================================================================
  observeEvent(input$main_tabs, {
    if (!log_enabled) return()
    if (!identical(input$main_tabs, "Results Comparison")) return()
    
    tier_session_view_count(tier_session_view_count() + 1L)
    
    # Use existing reactive accessors so logged values match the values
    # the rest of the app is computing with. tryCatch guards against
    # partial initialisation on first view.
    s1 <- tryCatch(scenario1_values(),      error = function(e) list())
    s2 <- tryCatch(scenario2_values(),      error = function(e) list())
    bv <- tryCatch(baseline_input_values(), error = function(e) list())
    
    # delta is no longer computed at log time -- meaningful deltas
    # (scenario vs baseline) are derived in the analysis script from
    # the per-row baseline_json, scenario1_json, scenario2_json fields.
    
    log_results_view(
      session_id                = tier_session_id,
      ip                        = tier_session_ip,
      user_country              = tier_session_user_country(),
      model_country             = isolate(input$region) %||% NA_character_,
      scenario1_inputs          = s1,
      scenario2_inputs          = s2,
      baseline_inputs           = bv,
      view_count                = tier_session_view_count()
    )
  }, ignoreInit = TRUE)
  
  # ---- Feedback modal ----
  observeEvent(input$open_feedback, {
    showModal(modalDialog(
      title = "Share your feedback",
      easyClose = TRUE,
      size = "l",
      footer = modalButton("Close"),
      
      p("Your input helps improve TIER-Plus."),
      # p(style = "color: #595959; font-size: 0.9em;",
      #   "If the form does not load, email us at ",
      #   tags$a(href = "mailto:your.email@example.com?subject=TIER-Plus%20Feedback",
      #          "your.email@example.com"), "."),
      tags$iframe(
        src    = "https://forms.office.com/r/k71dTjF1uy?embed=true",
        style  = "width: 100%; height: 700px; border: 0; margin-top: 8px;",
        frameborder  = "0",
        marginwidth  = "0",
        marginheight = "0",
        "Loading the feedback form…"
      )
    ))
  })
  # # ========================================================================
  # # DEBUG OBSERVER — TEMPORARY
  # # Prints cascade numbers for baseline + scenarios to the R console every
  # # time inputs change. Remove this block once 1st-95 movement is confirmed.
  # # ========================================================================
  # observe({
  #   pops <- populations()
  #   b  <- outcomes_baseline()
  #   s1 <- outcomes_scenario1()
  #   s2 <- outcomes_scenario2()
  #   
  #   cat("\n=========== CASCADE DEBUG ===========\n")
  #   cat(sprintf("populations$plhiv      = %s\n", format(round(pops$plhiv), big.mark = ",")))
  #   cat(sprintf("populations$diagnosed  = %s  (input %% diagnosed: %.2f%%)\n",
  #               format(round(pops$diagnosed), big.mark = ","),
  #               100 * pops$diagnosed / pops$plhiv))
  #   cat("\n                         BASELINE      SCENARIO 1    SCENARIO 2\n")
  #   cat(sprintf("new_diagnoses       :  %10s    %10s    %10s\n",
  #               format(b$new_diagnoses,  big.mark=","),
  #               format(s1$new_diagnoses, big.mark=","),
  #               format(s2$new_diagnoses, big.mark=",")))
  #   cat(sprintf("end_diagnosed       :  %10s    %10s    %10s\n",
  #               format(b$end_diagnosed,  big.mark=","),
  #               format(s1$end_diagnosed, big.mark=","),
  #               format(s2$end_diagnosed, big.mark=",")))
  #   cat(sprintf("end_plhiv           :  %10s    %10s    %10s\n",
  #               format(b$end_plhiv,  big.mark=","),
  #               format(s1$end_plhiv, big.mark=","),
  #               format(s2$end_plhiv, big.mark=",")))
  #   cat(sprintf("end_new_infections  :  %10s    %10s    %10s\n",
  #               format(b$end_new_infections,  big.mark=","),
  #               format(s1$end_new_infections, big.mark=","),
  #               format(s2$end_new_infections, big.mark=",")))
  #   cat(sprintf("deaths_undiagnosed  :  %10s    %10s    %10s\n",
  #               format(b$deaths_undiagnosed,  big.mark=","),
  #               format(s1$deaths_undiagnosed, big.mark=","),
  #               format(s2$deaths_undiagnosed, big.mark=",")))
  #   cat(sprintf("1st 95 (raw %%)      :  %10.4f    %10.4f    %10.4f\n",
  #               b$end_diagnosed  / b$end_plhiv  * 100,
  #               s1$end_diagnosed / s1$end_plhiv * 100,
  #               s2$end_diagnosed / s2$end_plhiv * 100))
  #   cat(sprintf("1st 95 (round 1dp)  :  %10.1f    %10.1f    %10.1f\n",
  #               round(b$end_diagnosed  / b$end_plhiv  * 100, 1),
  #               round(s1$end_diagnosed / s1$end_plhiv * 100, 1),
  #               round(s2$end_diagnosed / s2$end_plhiv * 100, 1)))
  #   cat("=====================================\n\n")
  # })
  
  # ========================================================================
  # 95-95-95 GOALS DISPLAY
  # ========================================================================
  
  output$goals_baseline <- renderUI({
    pops <- populations()
    outcomes <- outcomes_baseline()
    
    # Calculate 95-95-95 with comprehensive guards
    first_95 <- 0
    second_95 <- 0
    third_95 <- 0
    
    if (outcomes$end_plhiv>0 && !is.null(outcomes$end_diagnosed) && 
        !is.na(pops$plhiv) && !is.na(outcomes$end_diagnosed) && pops$plhiv > 0) {
      first_95 <- (outcomes$end_diagnosed / outcomes$end_plhiv) * 100
    }
    
    #FLAG if improvement from excess deaths
    first_95_counterfactual <- ifelse(
      (outcomes$end_plhiv + outcomes$deaths_undiagnosed) > 0,
      (outcomes$end_diagnosed / (outcomes$end_plhiv + outcomes$deaths_undiagnosed)) * 100,
      0
    )
    
    mortality_inflated_1st95 <- outcomes$deaths_undiagnosed > 0 & 
      first_95 > first_95_counterfactual
    
    if (!is.null(outcomes$end_diagnosed) && !is.null(outcomes$end_on_art) &&
        !is.na(outcomes$end_diagnosed) && !is.na(outcomes$end_on_art) && outcomes$end_diagnosed > 0) {
      second_95 <- (outcomes$end_on_art / outcomes$end_diagnosed) * 100
    }
    
    if (!is.null(outcomes$end_on_art) && !is.null(outcomes$end_suppressed) &&
        !is.na(outcomes$end_on_art) && !is.na(outcomes$end_suppressed) && outcomes$end_on_art > 0) {
      third_95 <- (outcomes$end_suppressed / outcomes$end_on_art) * 100
    }
    
    # Cap at 100% and handle NaN
    first_95 <- ifelse(is.nan(first_95) | is.infinite(first_95), 0, min(first_95, 100))
    second_95 <- ifelse(is.nan(second_95) | is.infinite(second_95), 0, min(second_95, 100))
    third_95 <- ifelse(is.nan(third_95) | is.infinite(third_95), 0, min(third_95, 100))
    
    tagList(
      div(class = "d-flex justify-content-between border-bottom pb-2 mb-2",
          span(strong("1st 95:"), " % of PLHIV diagnosed"),
          span(style = "font-size: 1.3em; font-weight: bold;",
               paste0(round(first_95, 1), "%"))
      ),
      div(class = "d-flex justify-content-between border-bottom pb-2 mb-2",
          span(strong("2nd 95:"), " % of diagnosed on ART"),
          span(style = "font-size: 1.3em; font-weight: bold;",
               paste0(round(second_95, 1), "%"))
      ),
      div(class = "d-flex justify-content-between border-bottom pb-2 mb-2",
          span(strong("3rd 95:"), " % on ART suppressed"),
          span(style = "font-size: 1.3em; font-weight: bold;",
               paste0(round(third_95, 1), "%"))
      )
    )
  })
  
  output$goals_scenario1 <- renderUI({
    pops <- populations()
    outcomes <- outcomes_scenario1()
    outcomes_base <- outcomes_baseline()
    
    # Calculate with comprehensive guards
    first_95 <- 0
    second_95 <- 0
    third_95 <- 0
    first_95_base <- 0
    second_95_base <- 0
    third_95_base <- 0
    
    # Scenario 1 values
    if (outcomes$end_plhiv>0 && !is.null(outcomes$end_diagnosed) && 
        !is.na(pops$plhiv) && !is.na(outcomes$end_diagnosed) && pops$plhiv > 0) {
      first_95 <- (outcomes$end_diagnosed / outcomes$end_plhiv) * 100
    }
    
    #FLAG if improvement from excess deaths
    first_95_counterfactual <- ifelse(
      (outcomes$end_plhiv + outcomes$deaths_undiagnosed) > 0,
      (outcomes$end_diagnosed / (outcomes$end_plhiv + outcomes$deaths_undiagnosed)) * 100,
      0
    )
    
    mortality_inflated_1st95 <- outcomes$deaths_undiagnosed > 0 & 
      first_95 > first_95_counterfactual
    if (!is.null(outcomes$end_diagnosed) && !is.null(outcomes$end_on_art) &&
        !is.na(outcomes$end_diagnosed) && !is.na(outcomes$end_on_art) && outcomes$end_diagnosed > 0) {
      second_95 <- (outcomes$end_on_art / outcomes$end_diagnosed) * 100
    }
    if (!is.null(outcomes$end_on_art) && !is.null(outcomes$end_suppressed) &&
        !is.na(outcomes$end_on_art) && !is.na(outcomes$end_suppressed) && outcomes$end_on_art > 0) {
      third_95 <- (outcomes$end_suppressed / outcomes$end_on_art) * 100
    }
    
    # Baseline values
    if ( outcomes$end_plhiv>0 && !is.null(outcomes_base$end_diagnosed) && 
         !is.na(pops$plhiv) && !is.na(outcomes_base$end_diagnosed) && pops$plhiv > 0) {
      first_95_base <- (outcomes_base$end_diagnosed / outcomes_base$end_plhiv) * 100
    }
    if (!is.null(outcomes_base$end_diagnosed) && !is.null(outcomes_base$end_on_art) &&
        !is.na(outcomes_base$end_diagnosed) && !is.na(outcomes_base$end_on_art) && outcomes_base$end_diagnosed > 0) {
      second_95_base <- (outcomes_base$end_on_art / outcomes_base$end_diagnosed) * 100
    }
    if (!is.null(outcomes_base$end_on_art) && !is.null(outcomes_base$end_suppressed) &&
        !is.na(outcomes_base$end_on_art) && !is.na(outcomes_base$end_suppressed) && outcomes_base$end_on_art > 0) {
      third_95_base <- (outcomes_base$end_suppressed / outcomes_base$end_on_art) * 100
    }
    
    # Cap at 100% and handle NaN
    first_95 <- ifelse(is.nan(first_95) | is.infinite(first_95), 0, min(first_95, 100))
    second_95 <- ifelse(is.nan(second_95) | is.infinite(second_95), 0, min(second_95, 100))
    third_95 <- ifelse(is.nan(third_95) | is.infinite(third_95), 0, min(third_95, 100))
    first_95_base <- ifelse(is.nan(first_95_base) | is.infinite(first_95_base), 0, min(first_95_base, 100))
    second_95_base <- ifelse(is.nan(second_95_base) | is.infinite(second_95_base), 0, min(second_95_base, 100))
    third_95_base <- ifelse(is.nan(third_95_base) | is.infinite(third_95_base), 0, min(third_95_base, 100))
    
    diff_first <- first_95 - first_95_base
    diff_second <- second_95 - second_95_base
    diff_third <- third_95 - third_95_base
    
    tagList(
      div(class = "d-flex justify-content-between border-bottom pb-2 mb-2",
          span(strong("1st 95:"), " % of PLHIV diagnosed"),
          span(
            span(style = "font-size: 1.3em; font-weight: bold;",
                 paste0(round(first_95, 1), "%")),
            br(),
            span(style = paste0("font-size: 0.9em; color: ",
                                ifelse(diff_first > 0, "green",
                                       ifelse(diff_first < 0, "red", "gray")), ";"),
                 paste0("(", ifelse(diff_first > 0, "+", ""), round(diff_first, 1), "pp)"))
          )
      ),
      div(class = "d-flex justify-content-between border-bottom pb-2 mb-2",
          span(strong("2nd 95:"), " % of diagnosed on ART"),
          span(
            span(style = "font-size: 1.3em; font-weight: bold;",
                 paste0(round(second_95, 1), "%")),
            br(),
            span(style = paste0("font-size: 0.9em; color: ",
                                ifelse(diff_second > 0, "green",
                                       ifelse(diff_second < 0, "red", "gray")), ";"),
                 paste0("(", ifelse(diff_second > 0, "+", ""), round(diff_second, 1), "pp)"))
          )
      ),
      div(class = "d-flex justify-content-between border-bottom pb-2 mb-2",
          span(strong("3rd 95:"), " % on ART suppressed"),
          span(
            span(style = "font-size: 1.3em; font-weight: bold;",
                 paste0(round(third_95, 1), "%")),
            br(),
            span(style = paste0("font-size: 0.9em; color: ",
                                ifelse(diff_third > 0, "green",
                                       ifelse(diff_third < 0, "red", "gray")), ";"),
                 paste0("(", ifelse(diff_third > 0, "+", ""), round(diff_third, 1), "pp)"))
          )
      )
    )
  })
  
  output$goals_scenario2 <- renderUI({
    pops <- populations()
    outcomes <- outcomes_scenario2()
    outcomes_base <- outcomes_baseline()
    
    # Calculate with comprehensive guards
    first_95 <- 0
    second_95 <- 0
    third_95 <- 0
    first_95_base <- 0
    second_95_base <- 0
    third_95_base <- 0
    
    # Scenario 2 values
    if ( outcomes$end_plhiv>0 && !is.null(outcomes$end_diagnosed) && 
         !is.na(pops$plhiv) && !is.na(outcomes$end_diagnosed) && pops$plhiv > 0) {
      first_95 <- (outcomes$end_diagnosed / outcomes$end_plhiv) * 100
    }
    
    #FLAG if improvement from excess deaths
    first_95_counterfactual <- ifelse(
      (outcomes$end_plhiv + outcomes$deaths_undiagnosed) > 0,
      (outcomes$end_diagnosed / (outcomes$end_plhiv + outcomes$deaths_undiagnosed)) * 100,
      0
    )
    
    mortality_inflated_1st95 <- outcomes$deaths_undiagnosed > 0 & 
      first_95 > first_95_counterfactual
    if (!is.null(outcomes$end_diagnosed) && !is.null(outcomes$end_on_art) &&
        !is.na(outcomes$end_diagnosed) && !is.na(outcomes$end_on_art) && outcomes$end_diagnosed > 0) {
      second_95 <- (outcomes$end_on_art / outcomes$end_diagnosed) * 100
    }
    if (!is.null(outcomes$end_on_art) && !is.null(outcomes$end_suppressed) &&
        !is.na(outcomes$end_on_art) && !is.na(outcomes$end_suppressed) && outcomes$end_on_art > 0) {
      third_95 <- (outcomes$end_suppressed / outcomes$end_on_art) * 100
    }
    
    # Baseline values
    if ( outcomes$end_plhiv>0 && !is.null(outcomes_base$end_diagnosed) && 
         !is.na(pops$plhiv) && !is.na(outcomes_base$end_diagnosed) && pops$plhiv > 0) {
      first_95_base <- (outcomes_base$end_diagnosed / outcomes_base$end_plhiv) * 100
    }
    if (!is.null(outcomes_base$end_diagnosed) && !is.null(outcomes_base$end_on_art) &&
        !is.na(outcomes_base$end_diagnosed) && !is.na(outcomes_base$end_on_art) && outcomes_base$end_diagnosed > 0) {
      second_95_base <- (outcomes_base$end_on_art / outcomes_base$end_diagnosed) * 100
    }
    if (!is.null(outcomes_base$end_on_art) && !is.null(outcomes_base$end_suppressed) &&
        !is.na(outcomes_base$end_on_art) && !is.na(outcomes_base$end_suppressed) && outcomes_base$end_on_art > 0) {
      third_95_base <- (outcomes_base$end_suppressed / outcomes_base$end_on_art) * 100
    }
    
    # Cap at 100% and handle NaN
    first_95 <- ifelse(is.nan(first_95) | is.infinite(first_95), 0, min(first_95, 100))
    second_95 <- ifelse(is.nan(second_95) | is.infinite(second_95), 0, min(second_95, 100))
    third_95 <- ifelse(is.nan(third_95) | is.infinite(third_95), 0, min(third_95, 100))
    first_95_base <- ifelse(is.nan(first_95_base) | is.infinite(first_95_base), 0, min(first_95_base, 100))
    second_95_base <- ifelse(is.nan(second_95_base) | is.infinite(second_95_base), 0, min(second_95_base, 100))
    third_95_base <- ifelse(is.nan(third_95_base) | is.infinite(third_95_base), 0, min(third_95_base, 100))
    
    diff_first <- first_95 - first_95_base
    diff_second <- second_95 - second_95_base
    diff_third <- third_95 - third_95_base
    
    tagList(
      div(class = "d-flex justify-content-between border-bottom pb-2 mb-2",
          span(strong("1st 95:"), " % of PLHIV diagnosed"),
          span(
            span(style = "font-size: 1.3em; font-weight: bold;",
                 paste0(round(first_95, 1), "%")),
            br(),
            span(style = paste0("font-size: 0.9em; color: ",
                                ifelse(diff_first > 0, "green",
                                       ifelse(diff_first < 0, "red", "gray")), ";"),
                 paste0("(", ifelse(diff_first > 0, "+", ""), round(diff_first, 1), "pp)"))
          )
      ),
      div(class = "d-flex justify-content-between border-bottom pb-2 mb-2",
          span(strong("2nd 95:"), " % of diagnosed on ART"),
          span(
            span(style = "font-size: 1.3em; font-weight: bold;",
                 paste0(round(second_95, 1), "%")),
            br(),
            span(style = paste0("font-size: 0.9em; color: ",
                                ifelse(diff_second > 0, "green",
                                       ifelse(diff_second < 0, "red", "gray")), ";"),
                 paste0("(", ifelse(diff_second > 0, "+", ""), round(diff_second, 1), "pp)"))
          )
      ),
      div(class = "d-flex justify-content-between border-bottom pb-2 mb-2",
          span(strong("3rd 95:"), " % on ART suppressed"),
          span(
            span(style = "font-size: 1.3em; font-weight: bold;",
                 paste0(round(third_95, 1), "%")),
            br(),
            span(style = paste0("font-size: 0.9em; color: ",
                                ifelse(diff_third > 0, "green",
                                       ifelse(diff_third < 0, "red", "gray")), ";"),
                 paste0("(", ifelse(diff_third > 0, "+", ""), round(diff_third, 1), "pp)"))
          )
      )
    )
  })
  
  # ========================================================================
  # EPIDEMIOLOGICAL OUTCOMES DISPLAY (included in Supplementary_outputs.R)
  # ========================================================================
  # See Supplementary_outputs.R for:
  # - output$epi_baseline, output$epi_scenario1, output$epi_scenario2
  # - output$results_baseline_health, output$results_scenario1_health, output$results_scenario2_health
  # - output$results_baseline_cost, output$results_scenario1_cost, output$results_scenario2_cost
  # - output$plot_scenario1, output$plot_scenario2
  
  # ========================================================================
  # EPIDEMIOLOGICAL OUTCOMES DISPLAY
  # ========================================================================
  
  output$epi_baseline <- renderUI({
    outcomes <- outcomes_baseline()
    
    tagList(
      div(class = "d-flex border-bottom pb-2 mb-2", style = "gap: 0.5rem;",
          span(style = "width: 110px; flex-shrink: 0;", strong("New Adult Acquisitions:")),
          span(style = "font-size: 1.3em; font-weight: bold;",
               format(outcomes$end_new_infections, big.mark = ","))
      ),
      div(class = "d-flex border-bottom pb-2 mb-2", style = "gap: 0.5rem;",
          span(style = "width: 110px; flex-shrink: 0;", strong("New Infant Acquisitions:")),
          span(style = "font-size: 1.3em; font-weight: bold;",
               format(outcomes$end_infant_infections, big.mark = ","))
      ),
      div(class = "d-flex border-bottom pb-2 mb-2", style = "gap: 0.5rem;",
          span(style = "width: 110px; flex-shrink: 0;", strong("HIV-Related Deaths:")),
          span(style = "font-size: 1.3em; font-weight: bold;",
               format(outcomes$end_deaths, big.mark = ","))
      ),
      # Calibration note: shown when calibration factor is meaningfully ≠ 1
      if (!is.null(outcomes$mortality_calibration_factor) &&
          abs(outcomes$mortality_calibration_factor - 1) > 0.05) {
        div(class = "small text-muted mb-2", style = "font-size: 0.85em; font-style: italic;",
            sprintf("Mortality calibrated to country UNAIDS target (factor: %.2f). Scenarios show relative changes from this baseline.",
                    outcomes$mortality_calibration_factor))
      }
    )
  })
  
  output$epi_scenario1 <- renderUI({
    outcomes <- outcomes_scenario1()
    diff <- diff_scenario1()
    
    tagList(
      div(class = "d-flex border-bottom pb-2 mb-2", style = "gap: 0.5rem;",
          span(style = "width: 110px; flex-shrink: 0;", strong("New Adult Acquisitions:")),
          span(style = "white-space: nowrap;",
               span(style = paste0("font-size: 1.3em; font-weight: bold; color: ",
                                   ifelse(diff$diff_new_infections < 0, "green",
                                          ifelse(diff$diff_new_infections > 0, "red", "gray")), ";"),
                    format(outcomes$end_new_infections, big.mark = ",")),
               span(style = paste0("font-size: 0.9em; color: ",
                                   ifelse(diff$diff_new_infections < 0, "green",
                                          ifelse(diff$diff_new_infections > 0, "red", "gray")), ";"),
                    paste0(" (", ifelse(diff$diff_new_infections > 0, "+", ""),
                           format(diff$diff_new_infections, big.mark = ","), ")"))
          )
      ),
      div(class = "d-flex border-bottom pb-2 mb-2", style = "gap: 0.5rem;",
          span(style = "width: 110px; flex-shrink: 0;", strong("New Infant Acquisitions:")),
          span(style = "white-space: nowrap;",
               span(style = paste0("font-size: 1.3em; font-weight: bold; color: ",
                                   ifelse(diff$diff_infant_infections < 0, "green",
                                          ifelse(diff$diff_infant_infections > 0, "red", "gray")), ";"),
                    format(outcomes$end_infant_infections, big.mark = ",")),
               span(style = paste0("font-size: 0.9em; color: ",
                                   ifelse(diff$diff_infant_infections < 0, "green",
                                          ifelse(diff$diff_infant_infections > 0, "red", "gray")), ";"),
                    paste0(" (", ifelse(diff$diff_infant_infections > 0, "+", ""),
                           format(diff$diff_infant_infections, big.mark = ","), ")"))
          )
      ),
      div(class = "d-flex border-bottom pb-2 mb-2", style = "gap: 0.5rem;",
          span(style = "width: 110px; flex-shrink: 0;", strong("HIV-Related Deaths:")),
          span(style = "white-space: nowrap;",
               span(style = paste0("font-size: 1.3em; font-weight: bold; color: ",
                                   ifelse(diff$diff_deaths < 0, "green",
                                          ifelse(diff$diff_deaths > 0, "red", "gray")), ";"),
                    format(outcomes$end_deaths, big.mark = ",")),
               span(style = paste0("font-size: 0.9em; color: ",
                                   ifelse(diff$diff_deaths < 0, "green",
                                          ifelse(diff$diff_deaths > 0, "red", "gray")), ";"),
                    paste0(" (", ifelse(diff$diff_deaths > 0, "+", ""),
                           format(diff$diff_deaths, big.mark = ","), ")"))
          )
      )
    )
  })
  
  output$epi_scenario2 <- renderUI({
    outcomes <- outcomes_scenario2()
    diff <- diff_scenario2()
    
    tagList(
      div(class = "d-flex border-bottom pb-2 mb-2", style = "gap: 0.5rem;",
          span(style = "width: 110px; flex-shrink: 0;", strong("New Adult Acquisitions:")),
          span(style = "white-space: nowrap;",
               span(style = paste0("font-size: 1.3em; font-weight: bold; color: ",
                                   ifelse(diff$diff_new_infections < 0, "green",
                                          ifelse(diff$diff_new_infections > 0, "red", "gray")), ";"),
                    format(outcomes$end_new_infections, big.mark = ",")),
               span(style = paste0("font-size: 0.9em; color: ",
                                   ifelse(diff$diff_new_infections < 0, "green",
                                          ifelse(diff$diff_new_infections > 0, "red", "gray")), ";"),
                    paste0(" (", ifelse(diff$diff_new_infections > 0, "+", ""),
                           format(diff$diff_new_infections, big.mark = ","), ")"))
          )
      ),
      div(class = "d-flex border-bottom pb-2 mb-2", style = "gap: 0.5rem;",
          span(style = "width: 110px; flex-shrink: 0;", strong("New Infant Acquisitions:")),
          span(style = "white-space: nowrap;",
               span(style = paste0("font-size: 1.3em; font-weight: bold; color: ",
                                   ifelse(diff$diff_infant_infections < 0, "green",
                                          ifelse(diff$diff_infant_infections > 0, "red", "gray")), ";"),
                    format(outcomes$end_infant_infections, big.mark = ",")),
               span(style = paste0("font-size: 0.9em; color: ",
                                   ifelse(diff$diff_infant_infections < 0, "green",
                                          ifelse(diff$diff_infant_infections > 0, "red", "gray")), ";"),
                    paste0(" (", ifelse(diff$diff_infant_infections > 0, "+", ""),
                           format(diff$diff_infant_infections, big.mark = ","), ")"))
          )
      ),
      div(class = "d-flex border-bottom pb-2 mb-2", style = "gap: 0.5rem;",
          span(style = "width: 110px; flex-shrink: 0;", strong("HIV-Related Deaths:")),
          span(style = "white-space: nowrap;",
               span(style = paste0("font-size: 1.3em; font-weight: bold; color: ",
                                   ifelse(diff$diff_deaths < 0, "green",
                                          ifelse(diff$diff_deaths > 0, "red", "gray")), ";"),
                    format(outcomes$end_deaths, big.mark = ",")),
               span(style = paste0("font-size: 0.9em; color: ",
                                   ifelse(diff$diff_deaths < 0, "green",
                                          ifelse(diff$diff_deaths > 0, "red", "gray")), ";"),
                    paste0(" (", ifelse(diff$diff_deaths > 0, "+", ""),
                           format(diff$diff_deaths, big.mark = ","), ")"))
          )
      )
    )
  })
  # ============================================================================
  # SUPPLEMENTARY CODE - ADD TO INTERFACE FILE
  # ============================================================================
  # These are the remaining output sections for health outcomes and costs
  # Insert these in the server section of Mock-up_TIER_interface_MVP_NEW.R
  # ============================================================================
  
  # ========================================================================
  # HEALTH OUTCOMES DISPLAY (RELATIVE TO BASELINE)
  # ========================================================================
  
  output$results_baseline_health <- renderUI({
    tagList(
      div(class = "text-center py-3",
          p(style = "font-style: italic; color: #666;", "Baseline reference values"),
          p(style = "font-size: 0.9em; color: #999;", "Scenarios show changes relative to this baseline")
      )
    )
  })
  
  # Format a percentage with sign + green/red coloring.
  # good_direction = +1 if a positive % is good (ART, suppressed),
  #                  -1 if a negative % is good (infections, deaths).
  fmt_pct <- function(p, good_direction) {
    if (is.na(p)) return(span(style = "color: gray;", " (n/a)"))
    is_good <- (p * good_direction) > 0
    col <- if (p == 0) "gray" else if (is_good) "green" else "red"
    span(style = paste0("color: ", col, ";"),
         sprintf(" (%+.1f%%)", p))
  }
  
  output$results_scenario1_health <- renderUI({
    outcomes <- outcomes_scenario1()
    diff <- diff_scenario1()
    
    tagList(
      div(class = "d-flex justify-content-between border-bottom pb-2 mb-3",
          strong("Change in ART Initiations:"),
          strong(
            style = paste0("color: ",
                           ifelse(diff$diff_art_initiations > 0, "green",
                                  ifelse(diff$diff_art_initiations < 0, "red", "gray")), ";"),
            paste0(ifelse(diff$diff_art_initiations > 0, "+", ""),
                   format(diff$diff_art_initiations, big.mark = ",")),
            fmt_pct(diff$pct_art_initiations, good_direction = 1)
          )
      ),
      div(class = "d-flex justify-content-between border-bottom pb-2 mb-2",
          strong("Change in Acquisitions:"),
          span(
            # diff_new_infections: +ve = more infections (bad, red); -ve = fewer (good, green)
            class = ifelse(diff$diff_new_infections < 0, "text-success",
                           ifelse(diff$diff_new_infections > 0, "text-danger", "text-muted")),
            style = "font-weight: bold;",
            paste0(ifelse(diff$diff_new_infections > 0, "+", ""),
                   format(diff$diff_new_infections, big.mark = ",")),
            fmt_pct(diff$pct_new_infections, good_direction = -1)
          )
      ),
      div(class = "d-flex justify-content-between border-bottom pb-2 mb-2",
          strong("Change in Deaths:"),
          span(
            # diff_deaths: +ve = more deaths (bad, red); -ve = fewer (good, green)
            class = ifelse(diff$diff_deaths < 0, "text-success",
                           ifelse(diff$diff_deaths > 0, "text-danger", "text-muted")),
            style = "font-weight: bold;",
            paste0(ifelse(diff$diff_deaths > 0, "+", ""),
                   format(diff$diff_deaths, big.mark = ",")),
            fmt_pct(diff$pct_deaths, good_direction = -1)
          )
      ),
      div(class = "d-flex justify-content-between border-bottom pb-2 mb-2",
          strong("Change in Suppressed:"),
          span(
            style = paste0("font-weight: bold; color: ",
                           ifelse(diff$diff_suppressed > 0, "green",
                                  ifelse(diff$diff_suppressed < 0, "red", "gray")), ";"),
            paste0(ifelse(diff$diff_suppressed > 0, "+", ""),
                   format(diff$diff_suppressed, big.mark = ",")),
            fmt_pct(diff$pct_suppressed, good_direction = 1)
          )
      )
    )
  })
  
  output$results_scenario2_health <- renderUI({
    outcomes <- outcomes_scenario2()
    diff <- diff_scenario2()
    
    tagList(
      div(class = "d-flex justify-content-between border-bottom pb-2 mb-3",
          strong("Change in ART Initiations:"),
          strong(
            style = paste0("color: ",
                           ifelse(diff$diff_art_initiations > 0, "green",
                                  ifelse(diff$diff_art_initiations < 0, "red", "gray")), ";"),
            paste0(ifelse(diff$diff_art_initiations > 0, "+", ""),
                   format(diff$diff_art_initiations, big.mark = ",")),
            fmt_pct(diff$pct_art_initiations, good_direction = 1)
          )
      ),
      div(class = "d-flex justify-content-between border-bottom pb-2 mb-2",
          strong("Change in Acquisitions:"),
          span(
            # diff_new_infections: +ve = more infections (bad, red); -ve = fewer (good, green)
            class = ifelse(diff$diff_new_infections < 0, "text-success",
                           ifelse(diff$diff_new_infections > 0, "text-danger", "text-muted")),
            style = "font-weight: bold;",
            paste0(ifelse(diff$diff_new_infections > 0, "+", ""),
                   format(diff$diff_new_infections, big.mark = ",")),
            fmt_pct(diff$pct_new_infections, good_direction = -1)
          )
      ),
      div(class = "d-flex justify-content-between border-bottom pb-2 mb-2",
          strong("Change in Deaths:"),
          span(
            # diff_deaths: +ve = more deaths (bad, red); -ve = fewer (good, green)
            class = ifelse(diff$diff_deaths < 0, "text-success",
                           ifelse(diff$diff_deaths > 0, "text-danger", "text-muted")),
            style = "font-weight: bold;",
            paste0(ifelse(diff$diff_deaths > 0, "+", ""),
                   format(diff$diff_deaths, big.mark = ",")),
            fmt_pct(diff$pct_deaths, good_direction = -1)
          )
      ),
      div(class = "d-flex justify-content-between border-bottom pb-2 mb-2",
          strong("Change in Suppressed:"),
          span(
            style = paste0("font-weight: bold; color: ",
                           ifelse(diff$diff_suppressed > 0, "green",
                                  ifelse(diff$diff_suppressed < 0, "red", "gray")), ";"),
            paste0(ifelse(diff$diff_suppressed > 0, "+", ""),
                   format(diff$diff_suppressed, big.mark = ",")),
            fmt_pct(diff$pct_suppressed, good_direction = 1)
          )
      )
    )
  })
  
  # ========================================================================
  # COST ANALYSIS DISPLAY
  # ========================================================================
  
  output$results_baseline_cost <- renderUI({
    outcomes <- outcomes_baseline()
    
    tagList(
      div(class = "d-flex justify-content-between border-bottom pb-2 mb-2",
          span("Intervention Costs:"),
          span(class = "text-primary", style = "font-weight: bold;",
               paste0("$", format(outcomes$total_intervention_cost, big.mark = ",")))
      ),
      div(class = "d-flex justify-content-between border-bottom pb-2 mb-2",
          span(style = "font-style: italic; font-size: 0.9em;", "ART Provision:"),
          span(style = "font-weight: bold; color: #8b5cf6;",
               paste0("$", format(outcomes$art_provision_cost, big.mark = ",")))
      ),
      div(class = "d-flex justify-content-between border-bottom pb-2 mb-3 bg-light p-2",
          strong("Total Program Cost:"),
          strong(paste0("$", format(outcomes$total_cost, big.mark = ",")))
      )
    )
  })
  
  output$results_scenario1_cost <- renderUI({
    outcomes <- outcomes_scenario1()
    diff <- diff_scenario1()
    
    tagList(
      div(class = "d-flex justify-content-between border-bottom pb-2 mb-2",
          span("Intervention Costs:"),
          span(
            span(class = "text-primary", style = "font-weight: bold;",
                 paste0("$", format(outcomes$total_intervention_cost, big.mark = ","))),
            br(),
            span(style = paste0("font-size: 0.9em; color: ",
                                ifelse(diff$diff_intervention_cost > 0, "red",
                                       ifelse(diff$diff_intervention_cost < 0, "green", "gray")), ";"),
                 paste0("(", ifelse(diff$diff_intervention_cost > 0, "+",
                                    ifelse(diff$diff_intervention_cost < 0, "-", "")), "$",
                        format(abs(diff$diff_intervention_cost), big.mark = ","), ")"))
          )
      ),
      div(class = "d-flex justify-content-between border-bottom pb-2 mb-2",
          span(style = "font-style: italic; font-size: 0.9em;", "ART Provision:"),
          span(
            span(style = "font-weight: bold; color: #8b5cf6;",
                 paste0("$", format(outcomes$art_provision_cost, big.mark = ","))),
            br(),
            span(style = paste0("font-size: 0.9em; color: ",
                                ifelse(diff$diff_art_provision_cost > 0, "red",
                                       ifelse(diff$diff_art_provision_cost < 0, "green", "gray")), ";"),
                 paste0("(", ifelse(diff$diff_art_provision_cost > 0, "+",
                                    ifelse(diff$diff_art_provision_cost < 0, "-", "")), "$",
                        format(abs(diff$diff_art_provision_cost), big.mark = ","), ")"))
          )
      ),
      div(class = "d-flex justify-content-between border-bottom pb-2 mb-3 bg-light p-2",
          strong("Total Program Cost:"),
          span(
            strong(paste0("$", format(outcomes$total_cost, big.mark = ","))),
            br(),
            span(style = paste0("font-size: 0.9em; color: ",
                                ifelse(diff$diff_total_cost > 0, "red",
                                       ifelse(diff$diff_total_cost < 0, "green", "gray")), ";"),
                 paste0("(", ifelse(diff$diff_total_cost > 0, "+",
                                    ifelse(diff$diff_total_cost < 0, "-", "")), "$",
                        format(abs(diff$diff_total_cost), big.mark = ","), ")"))
          )
      )
    )
  })
  
  output$results_scenario2_cost <- renderUI({
    outcomes <- outcomes_scenario2()
    diff <- diff_scenario2()
    
    tagList(
      div(class = "d-flex justify-content-between border-bottom pb-2 mb-2",
          span("Intervention Costs:"),
          span(
            span(class = "text-primary", style = "font-weight: bold;",
                 paste0("$", format(outcomes$total_intervention_cost, big.mark = ","))),
            br(),
            span(style = paste0("font-size: 0.9em; color: ",
                                ifelse(diff$diff_intervention_cost > 0, "red",
                                       ifelse(diff$diff_intervention_cost < 0, "green", "gray")), ";"),
                 paste0("(", ifelse(diff$diff_intervention_cost > 0, "+",
                                    ifelse(diff$diff_intervention_cost < 0, "-", "")), "$",
                        format(abs(diff$diff_intervention_cost), big.mark = ","), ")"))
          )
      ),
      
      div(class = "d-flex justify-content-between border-bottom pb-2 mb-2",
          span(style = "font-style: italic; font-size: 0.9em;", "ART Provision:"),
          span(
            span(style = "font-weight: bold; color: #8b5cf6;",
                 paste0("$", format(outcomes$art_provision_cost, big.mark = ","))),
            br(),
            span(style = paste0("font-size: 0.9em; color: ",
                                ifelse(diff$diff_art_provision_cost > 0, "red",
                                       ifelse(diff$diff_art_provision_cost < 0, "green", "gray")), ";"),
                 paste0("(", ifelse(diff$diff_art_provision_cost > 0, "+",
                                    ifelse(diff$diff_art_provision_cost < 0, "-", "")), "$",
                        format(abs(diff$diff_art_provision_cost), big.mark = ","), ")"))
          )
      ),
      div(class = "d-flex justify-content-between border-bottom pb-2 mb-3 bg-light p-2",
          strong("Total Program Cost:"),
          span(
            strong(paste0("$", format(outcomes$total_cost, big.mark = ","))),
            br(),
            span(style = paste0("font-size: 0.9em; color: ",
                                ifelse(diff$diff_total_cost > 0, "red",
                                       ifelse(diff$diff_total_cost < 0, "green", "gray")), ";"),
                 paste0("(", ifelse(diff$diff_total_cost > 0, "+",
                                    ifelse(diff$diff_total_cost < 0, "-", "")), "$",
                        format(abs(diff$diff_total_cost), big.mark = ","), ")"))
          )
      )
    )
  })
  
  # ========================================================================
  # SUMMARY PLOTS
  # ========================================================================
  
  output$plot_scenario1 <- renderPlot({
    outcomes <- outcomes_scenario1()
    diff <- diff_scenario1()
    diff2 <- diff_scenario2()    # needed for the shared y-axis range
    
    plot_data <- data.frame(
      Outcome = c("Acquisitions", "Deaths", "ART\nInitiations"),
      Value   = c(diff$diff_new_infections,
                  diff$diff_deaths,
                  diff$diff_art_initiations),
      # is_good = TRUE when the change is favourable (green); FALSE when adverse (red).
      # Infections/Deaths: fewer is better -> Value < 0 is good.
      # ART Initiations:   more is better  -> Value > 0 is good.
      is_good = c(diff$diff_new_infections < 0,
                  diff$diff_deaths        < 0,
                  diff$diff_art_initiations >= 0),
      Baseline = c(outcomes_baseline()$end_new_infections,
                   outcomes_baseline()$end_deaths,
                   outcomes_baseline()$art_initiations)
    )
    
    # Shared y-axis range across BOTH scenario plots so bar heights are
    # directly comparable. Computed from the union of both scenarios' diffs.
    all_vals <- c(diff$diff_new_infections,  diff$diff_deaths,  diff$diff_art_initiations,
                  diff2$diff_new_infections, diff2$diff_deaths, diff2$diff_art_initiations)
    y_lim <- range(c(all_vals, 0), na.rm = TRUE)
    
    ggplot(plot_data, aes(x = Outcome, y = Value, fill = is_good)) +
      geom_col(width = 0.7) +
      geom_hline(yintercept = 0, colour = "#374151", linewidth = 0.4) +
      scale_fill_manual(values = c("TRUE" = "#10b981", "FALSE" = "#ef4444"), guide = "none") +
      scale_y_continuous(labels = comma, limits = y_lim) +
      labs(title = "Additional Impact vs Baseline", 
           y = "Number of People (relative to baseline)", x = NULL) +
      theme_minimal() +
      theme(axis.text.x = element_text(size = 12),
            axis.text.y = element_text(size = 11),
            plot.title = element_text(size = 14, face = "bold"))
  })
  
  output$plot_scenario2 <- renderPlot({
    outcomes <- outcomes_scenario2()
    diff <- diff_scenario2()
    diff1 <- diff_scenario1()    # needed for the shared y-axis range
    
    plot_data <- data.frame(
      Outcome = c("Acquisitions", "Deaths", "ART\nInitiations"),
      Value   = c(diff$diff_new_infections,
                  diff$diff_deaths,
                  diff$diff_art_initiations),
      # is_good = TRUE when the change is favourable (green); FALSE when adverse (red).
      # Infections/Deaths: fewer is better -> Value < 0 is good.
      # ART Initiations:   more is better  -> Value > 0 is good.
      is_good = c(diff$diff_new_infections < 0,
                  diff$diff_deaths        < 0,
                  diff$diff_art_initiations >= 0),
      Baseline = c(outcomes_baseline()$end_new_infections,
                   outcomes_baseline()$end_deaths,
                   outcomes_baseline()$art_initiations)
    )
    
    # Shared y-axis range — must match the calculation in plot_scenario1.
    all_vals <- c(diff$diff_new_infections,  diff$diff_deaths,  diff$diff_art_initiations,
                  diff1$diff_new_infections, diff1$diff_deaths, diff1$diff_art_initiations)
    y_lim <- range(c(all_vals, 0), na.rm = TRUE)
    
    ggplot(plot_data, aes(x = Outcome, y = Value, fill = is_good)) +
      geom_col(width = 0.7) +
      geom_hline(yintercept = 0, colour = "#374151", linewidth = 0.4) +
      scale_fill_manual(values = c("TRUE" = "#10b981", "FALSE" = "#ef4444"), guide = "none") +
      scale_y_continuous(labels = comma, limits = y_lim) +
      labs(title = "Additional Impact vs Baseline", 
           y = "Number of People (relative to baseline)", x = NULL) +
      theme_minimal() +
      theme(axis.text.x = element_text(size = 12),
            axis.text.y = element_text(size = 11),
            plot.title = element_text(size = 14, face = "bold"))
  })
  # ========================================================================
  # CASCADE PLOT
  # ========================================================================
  
  output$cascade_combined <- renderPlot({
    outcomes_base <- outcomes_baseline()
    outcomes_s1 <- outcomes_scenario1()
    outcomes_s2 <- outcomes_scenario2()
    pops <- populations()
    
    cascade_data <- data.frame(
      Stage = rep(c("PLHIV", "Diagnosed", "On ART", "Suppressed"), 3),
      Scenario = rep(c("Baseline", "Scenario 1", "Scenario 2"), each = 4),
      Value = c(
        outcomes_base$end_plhiv, outcomes_base$end_diagnosed, outcomes_base$end_on_art, outcomes_base$end_suppressed,
        outcomes_s1$end_plhiv,   outcomes_s1$end_diagnosed,   outcomes_s1$end_on_art,   outcomes_s1$end_suppressed,
        outcomes_s2$end_plhiv,   outcomes_s2$end_diagnosed,   outcomes_s2$end_on_art,   outcomes_s2$end_suppressed
      )
    )
    
    cascade_data$Stage <- factor(cascade_data$Stage, levels = c("PLHIV", "Diagnosed", "On ART", "Suppressed"))
    cascade_data$Scenario <- factor(cascade_data$Scenario, levels = c("Baseline", "Scenario 1", "Scenario 2"))
    
    ggplot(cascade_data, aes(x = Stage, y = Value, color = Scenario, group = Scenario, linetype = Scenario)) +
      geom_line(linewidth = 1.5) +
      geom_point(size = 4) +
      scale_color_manual(values = c("Baseline" = "#666666", "Scenario 1" = "#2563eb", "Scenario 2" = "#dc2626")) +
      scale_linetype_manual(values = c("Baseline" = "solid", "Scenario 1" = "solid", "Scenario 2" = "dashed")) +
      scale_y_continuous(labels = comma) +
      labs(title = "HIV Care Cascade: End of Year Outcomes",
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
  
  output$cascade_table <- renderTable({
    outcomes_base <- outcomes_baseline()
    outcomes_s1   <- outcomes_scenario1()
    outcomes_s2   <- outcomes_scenario2()
    
    data.frame(
      Stage      = c("PLHIV", "Diagnosed", "On ART", "Suppressed"),
      Baseline   = format(c(outcomes_base$end_plhiv, outcomes_base$end_diagnosed,
                            outcomes_base$end_on_art, outcomes_base$end_suppressed),
                          big.mark = ",", scientific = FALSE),
      Scenario_1 = format(c(outcomes_s1$end_plhiv, outcomes_s1$end_diagnosed,
                            outcomes_s1$end_on_art, outcomes_s1$end_suppressed),
                          big.mark = ",", scientific = FALSE),
      Scenario_2 = format(c(outcomes_s2$end_plhiv, outcomes_s2$end_diagnosed,
                            outcomes_s2$end_on_art, outcomes_s2$end_suppressed),
                          big.mark = ",", scientific = FALSE),
      check.names = FALSE
    )
  }, striped = TRUE, hover = TRUE, bordered = TRUE, align = "lrrr",
  colnames = TRUE)
  
  # ========================================================================
  # DOWNLOAD REPORT (PDF)
  # ------------------------------------------------------------------------
  # Renders an R Markdown template (report_template.Rmd, located next to
  # this app file) and converts it to PDF via headless Chromium.
  # Requires on the server:
  #   - R packages: rmarkdown, pagedown
  #   - chromium-browser (or chromium) installed system-wide
  # The reactive values current at the moment the user clicks Download are
  # snapshotted via isolate() and passed to the template as `params`, so
  # the PDF reflects exactly what was on screen at click time.
  # ========================================================================
  output$download_report <- downloadHandler(
    filename = function() {
      paste0("Tier_Plus_Report_",
             format(Sys.time(), "%Y%m%d_%H%M"),
             ".pdf")
    },
    content = function(file) {
      
      # Locate the template. We try the app directory first (production),
      # then the current working directory (interactive runs).
      app_dir <- tryCatch(
        dirname(sys.frame(1)$ofile),
        error = function(e) NULL
      )
      tpl_candidates <- c(
        if (!is.null(app_dir)) file.path(app_dir, "report_template.Rmd"),
        "report_template.Rmd",
        file.path(getwd(), "report_template.Rmd")
      )
      tpl <- tpl_candidates[file.exists(tpl_candidates)][1]
      if (is.na(tpl)) {
        stop("report_template.Rmd not found. Place it in the same ",
             "directory as the app file.")
      }
      
      # Copy to a temp location so concurrent downloads do not collide.
      tmp_rmd <- tempfile(fileext = ".Rmd")
      file.copy(tpl, tmp_rmd, overwrite = TRUE)
      
      # Snapshot reactive values now so the PDF reflects click-time state.
      report_params <- list(
        country_name    = isolate(input$region) %||% "—",
        generated_at    = format(Sys.time(), "%d %B %Y, %H:%M"),
        baseline_vals   = isolate(baseline_input_values()),
        scenario1_vals  = isolate(scenario1_values()),
        scenario2_vals  = isolate(scenario2_values()),
        outcomes_base   = isolate(outcomes_baseline()),
        outcomes_s1     = isolate(outcomes_scenario1()),
        outcomes_s2     = isolate(outcomes_scenario2()),
        diff_s1         = isolate(diff_scenario1()),
        diff_s2         = isolate(diff_scenario2()),
        populations     = isolate(populations()),
        interventions   = intervention_groups
      )
      
      # Render Rmd -> HTML, then HTML -> PDF via headless Chromium.
      out_html <- tempfile(fileext = ".html")
      rmarkdown::render(
        input         = tmp_rmd,
        output_file   = out_html,
        params        = report_params,
        envir         = new.env(parent = globalenv()),
        quiet         = TRUE
      )
      
      pagedown::chrome_print(
        input         = out_html,
        output        = file,
        format        = "pdf",
        timeout       = 120,
        extra_args    = c("--no-sandbox")   # required in many container setups
      )
    },
    contentType = "application/pdf"
  )
}

# ============================================================================
# RUN APPLICATION
# ============================================================================

shinyApp(ui = ui, server = server)