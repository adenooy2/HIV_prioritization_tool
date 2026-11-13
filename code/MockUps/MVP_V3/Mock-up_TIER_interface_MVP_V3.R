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
library(shinyWidgets)
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
    source("/Users/adenooy/Library/CloudStorage/OneDrive-Personal/AMC/HIV Prioritization tool/HIV_prioritization_tool/code/MockUps/MVP_V3/Mock-Up_logic_V3.R")
    message("Sourced local file successfully.")
  },
  error = function(e) {
    message("Local source failed. Trying alternative paths...")
    stop("Could not source logic file")
  }
)

# 
# # Source usage logger and initialise log DB.
# # Logging failure must NEVER block app startup -- the tryCatch guarantees
# # the app runs even if usage_logger.R is missing or init fails.
tryCatch({
  source("/Users/adenooy/Library/CloudStorage/OneDrive-Personal/AMC/HIV Prioritization tool/HIV_prioritization_tool/code/MockUps/MVP_V3/usage_logger.R")
  init_log_db()
}, error = function(e) {
  message("usage_logger.R not found or failed to init -- logging disabled.")
})


# ============================================================================
# STARTUP PrEP ENTRY MODE
# ============================================================================
# Country baselines are collected as product TOTALS (baseline_testing.csv
# columns prep_oral / prep_lenacapavir), so the app opens in "total" and users
# switch to "By group" only if they want to override the derived split.
#
# THIS CONSTANT MUST BE USED IN TWO PLACES OR THE UI DOUBLE-RENDERS:
#   1. the radioGroupButtons `selected` argument (the widget's own default), and
#   2. every `input$prep_entry_mode %||% PREP_ENTRY_MODE_DEFAULT` fallback (7
#      sites) -- the fallback is what renderUI sees on the FIRST pass, before
#      the client has reported the widget's value back to the server.
# If the two disagree, the first render draws the 8 per-group PrEP fields (and
# registers their 18 validation observers), then flips to total mode the moment
# the input arrives. Do not hard-code the literal at either site.
PREP_ENTRY_MODE_DEFAULT <- "total"

# ============================================================================
# RESULTS-PAGE METRIC TOGGLES
# ============================================================================
# ART initiations is hidden by default: the cascade-identity ceiling makes it
# FALL when retention improves (a smaller diagnosed-not-on-ART gap lowers
# max_art_initiations), which reads as a bug to external reviewers. The metric
# is still computed in the logic layer and still returned by diff_scenario1()
# / diff_scenario2() -- only the display is suppressed.
# Flip to TRUE to restore it in the health-outcome cards, the two scenario bar
# charts and the PDF report. All four display sites read this one constant;
# it is passed into the Rmd via report_params rather than relying on the
# report's environment inheriting globalenv().
SHOW_ART_INITIATIONS <- FALSE

# ============================================================================
# COMMA-FORMATTED NUMERIC INPUT (thousands separators for large counts)
# ============================================================================
# Drop-in numericInput() replacement for "absolute" (raw-count) fields where
# large values benefit from live comma formatting as the user types (e.g.
# condoms, PrEP, testing volumes, Total Population). Thin wrapper around
# shinyWidgets::autonumericInput() (autoNumeric.js), keeping the same call
# signature as numericInput() so call sites need only swap the function.
# input$<id> is delivered to the server as a plain numeric, exactly as with
# numericInput(). updateNumericInput() also keeps working against these
# fields unchanged: it sends {value: ...} via session$sendInputMessage(),
# which autonumericInputBinding.receiveMessage() picks up via
# data.hasOwnProperty("value") regardless of which binding sent it --
# Shiny routes update messages generically by DOM id, not by input type.
# NOTE: requires the shinyWidgets package (new dependency -- install on
# both the local machine and the Hetzner server).
commaNumericInput <- function(inputId, label, value, min = NA, max = NA,
                              step = NA, width = NULL) {
  shinyWidgets::autonumericInput(
    inputId             = inputId,
    label               = label,
    # htmltools serializes large doubles (e.g. 5000000) as "5e+06" in the
    # HTML value attribute, which autoNumeric.js cannot parse -- force
    # plain-decimal formatting to avoid the field rendering blank/broken.
    value               = format(value, scientific = FALSE, trim = TRUE),
    width               = width,
    align               = "left",
    decimalPlaces       = 0,
    digitGroupSeparator = ",",
    decimalCharacter    = ".",
    minimumValue        = if (!is.na(min)) min else NULL,
    maximumValue        = if (!is.na(max)) max else NULL,
    emptyInputBehavior  = "null"
  )
}

# Money fields. Same autoNumeric machinery as commaNumericInput() -- comma for
# thousands, period for the decimal -- but 2 decimal places, because unit costs
# are per-person figures where the cents are real and user-entered.
#
# Deliberately NOT used for aggregate program costs: the logic round()s those
# (Mock-Up_logic_V3.R ~3793), so a trailing .00 would assert precision the model
# has already thrown away.
#
# minimumValue defaults to NULL, not 0, because the ART cost adjustment fields
# are signed (negative = saving). Callers needing a floor pass min explicitly.
currencyNumericInput <- function(inputId, label, value, min = NA, max = NA,
                                 width = NULL) {
  shinyWidgets::autonumericInput(
    inputId             = inputId,
    label               = label,
    # Same scientific-notation guard as commaNumericInput(). The extra NA check
    # is needed here and not there: a DSD key with no sheet unit_cost yields
    # NA_real_, and format(NA) would render the literal string "NA" in the box.
    value               = if (length(value) == 1 && !is.na(value)) {
      format(value, scientific = FALSE, trim = TRUE)
    } else NULL,
    width               = width,
    align               = "left",
    decimalPlaces       = 2,
    digitGroupSeparator = ",",
    decimalCharacter    = ".",
    minimumValue        = if (!is.na(min)) min else NULL,
    maximumValue        = if (!is.na(max)) max else NULL,
    emptyInputBehavior  = "null"
  )
}

# Small bounded fields that carry real decimals -- efficacy percentages (0-100)
# and durations in months (0-12). Same autoNumeric machinery as the two above,
# and THAT IS THE POINT: numericInput() emits <input type="number">, whose
# decimal separator follows the CLIENT's locale -- a comma on an en-ZA machine,
# against the period the cost boxes are pinned to. decimalCharacter = "." here
# makes the whole Parameters tab agree regardless of who opens it.
#
# digitGroupSeparator is carried over from the two helpers above for
# consistency only; a 0-100 field cannot reach four digits, so it never renders.
#
# decimalPlaces is the resolution the box can EXPRESS, and it bounds a round
# trip that decides correctness, not just looks: sheet -> box -> echo ->
# override. A sheet value carrying more precision than the box is rounded at
# first render, echoes back rounded, and is then recorded as a "changed"
# parameter on a tab nobody touched -- the failure the DSD absolutes hit. The
# tightest consumer sets the floor: second_shot_return_rate is a raw 0-1
# probability, so 2 dp would leave it just two significant figures. 4 dp gives
# every field on the tab at least four significant figures (efficacy: 74.1234%
# -> a 6 dp fraction; duration: years_to_mo() already rounds to 2 dp, so the
# box is exactly lossless). Widen HERE, not at a call site.
#
# allowDecimalPadding = FALSE is what makes 4 dp free rather than ugly:
# autoNumeric pads to decimalPlaces by default, so 0.5 would render "0.5000",
# 74 as "74.0000" and 6 months as "6.0000". Padding is right for money -- the
# cents in currencyNumericInput() are real -- and wrong for everything here.
#
# min/max are handed to autoNumeric, which CLAMPS rather than warns, so an
# out-of-range value cannot be typed. The chk_range() outputs are kept anyway:
# they still catch an emptied box (emptyInputBehavior = "null" -> NA).
decimalNumericInput <- function(inputId, label, value, min = NA, max = NA,
                                decimalPlaces = 4, width = NULL) {
  shinyWidgets::autonumericInput(
    inputId             = inputId,
    label               = label,
    # NA guard as per currencyNumericInput(): a missing sheet row yields
    # NA_real_, and format(NA) would render the literal string "NA" in the box.
    value               = if (length(value) == 1 && !is.na(value)) {
      format(value, scientific = FALSE, trim = TRUE)
    } else NULL,
    width               = width,
    align               = "left",
    decimalPlaces       = decimalPlaces,
    allowDecimalPadding = FALSE,
    digitGroupSeparator = ",",
    decimalCharacter    = ".",
    minimumValue        = if (!is.na(min)) min else NULL,
    maximumValue        = if (!is.na(max)) max else NULL,
    emptyInputBehavior  = "null"
  )
}

# ============================================================================
# USER INTERFACE
# ============================================================================

ui <- page_sidebar(
  tags$head(tags$style(HTML("
    hr { margin-top: 0.75rem; margin-bottom: 0.5rem; }
       /* Parameters-tab spacing. Every bslib::layout_columns() grid carries class
       `bslib-mb-spacing` (~1rem margin-bottom) -- THAT is the tab's real vertical
       spacer, not .form-group. Cost rows bundle box+validation+caption in one
       .dsd-cell (one grid/row); effectiveness rows use a separate validation grid
       (two grids/row), so effectiveness sections carried an extra grid margin and
       read taller. Fix: hide a validation grid outright when all its outputs are
       empty. It's identified as a grid that HAS outputs but NO .shiny-input-container
       -- which excludes input grids and cost grids (their outputs sit beside an
       input in the same grid). Bare (non-grid) validation outputs still collapse
       via :empty. When a message fires, the output is non-empty and the grid
       reappears. */
    .param-tab .shiny-html-output:empty { display: none; }
    .param-tab .bslib-grid:has(.shiny-html-output):not(:has(.shiny-input-container)):not(:has(.shiny-html-output:not(:empty))) {
      display: none;
    }
    .disabled-input { margin: 0; }
    .disabled-input .form-group { margin-bottom: 0; }
    /* ART cost adjustment cells: box and its caption read as one unit, so drop
       the Bootstrap form-group gap between them. */
    .dsd-cell .form-group { margin-bottom: 0; }
    .dsd-cell .shiny-input-container { margin-bottom: 0; }
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
    commaNumericInput("total_pop", "Total Population:", value = 5000000, min = 0),
    tags$div(class = "disabled-input",numericInput("prevalence", "HIV Prevalence among people aged 15+ (%):", value = 4.5, min = 0, max = 100, step = 0.1)),
    tags$div(class = "disabled-input",commaNumericInput("new_infections", "New HIV Acquisitions/Year:", value = 8500, min = 0)),
    numericInput("pct_diagnosed", "% of PLHIV Diagnosed:", value = 85, min = 0, max = 100),  
    numericInput("pct_on_art", "% Diagnosed on ART:", value = 78, min = 0, max = 100),
    numericInput("pct_suppressed", "% on ART Suppressed:", value = 82, min = 0, max = 100),
    tags$div(class = "disabled-input",commaNumericInput("aids_deaths", "AIDS Deaths/Year (baseline):", value = 2200, min = 0))
    
    
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
    # # TEMP: Validation feedback link — remove after validation period ends
    # span(
    #   icon("clipboard-check"), " ",
    #   tags$a(
    #     href   = "https://forms.cloud.microsoft/Pages/ResponsePage.aspx?id=f_74E4DG7kuhFmNZHWvuMmCh7_sn9aVLibTDrfIQKjtURENRUFVMQVRXWldOMkFMMU1MSFBDOVk5Ti4u",
    #     target = "_blank",
    #     rel    = "noopener noreferrer",
    #     "Validation Feedback Form",
    #     style  = "color: #2563eb;"
    #   )
    # ),
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
            #src = "https://www.youtube.com/embed/cr4ibbplYi8",
            src="https://www.youtube.com/embed/t4k7DgLyEIs",
            style = "position: absolute; top: 0; left: 0; width: 100%; height: 100%; border: 0;",
            allow = "accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture",
            allowfullscreen = NA,
            title = "TIER-Plus introduction video",
            loading = "lazy"
          )
        ),
        
        
        
        
        h4("What is TIER-Plus"),
        p("TIER-Plus is a planning tool for national HIV programme stakeholders to support annual budget allocation and resource prioritization decisions. It enables users to model how resource allocation across different HIV interventions—prevention, testing, treatment, and retention—translates into projected changes in key outcomes, including progress toward the 95-95-95 targets, new HIV acquisitions, mortality, and programme costs."),
        p("The tool uses the most recent programme data as a baseline reference point and allows country teams to systematically model the epidemiological and financial implications of scaling up or scaling down different interventions relative to current levels. Through an accessible, scenario-based platform, users can evaluate multiple allocation strategies side-by-side, compare their relative impact on outcomes, and make evidence-informed prioritization decisions that can be substantiated during planning and advocacy processes."),
        h4("Important to know:"),
        p(" TIER-Plus is designed to inform and support annual planning deliberations within country-led processes. The tool provides indicative estimates of direction and relative magnitude of impact, which facilitates comparison across scenarios. All final budgetary allocations, epidemiological targets, and implementation specifications should be derived from country-led planning processes grounded in local epidemiological context and operational capacity. TIER-Plus functions as a complementary analytical resource and does not replace other validated modeling methodologies or country decision-making authority."),
        
        h4("How to use the tool"),
        tags$ol(
          tags$li(strong("Set the country context. "),
                  "Enter or confirm population and current 95-targets (diagnosed, on ART, suppressed) in the most recent reporting year."),
          tags$li(strong("Enter the baseline scenario. "),
                  "Populate each intervention with the volume and coverage of services delivered in the year prior (see ", em("What the baseline represents"), " below)."),
          tags$li(strong("Build a scenario. "),
                  "Adjust intervention volumes up or down from baseline. You can scale things in either direction."),
          tags$li(strong("Adjust assumptions. "),
                  "Model assumptions surrounding intervention effectiveness and cost can be adjusted from the defaults if required."),
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
          tags$li(strong("Oral PrEP / Lenacapavir: "), "Number of individuals currently receiving and/or initiated on PrEP, entered either as a total or separately for FSW, MSM, and AGYW (15-24) and the general population."),
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
          tags$li(strong("Tracking and tracing: "), "Outreach to people lost to follow-up to bring them back into care. Applied after DSD has already prevented some LTFU, against the remaining LTFU pool."),
          tags$li(strong("Frequency of clinical visits for stable clients: "),"The frequency at which stable ART clients must return for clinical visits, either every 6 months of every 12 months.")
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
      p(strong("Note:"),"Baseline coverage values should reflect services that were delivered in the most recent year. Current values were pre-populated using available data (e.g GAM) and literature where relevant. Please review the values below, and update any for which you have more accurate values."),
      # PrEP entry-mode toggle (GLOBAL): governs the Baseline tab AND both
      # Scenarios. Rendered here in the static UI (created once) so its state
      # in input$prep_entry_mode is stable and never reset by a re-render.
      #   "disaggregated" = enter FSW/MSM/AGYW/General oral + lenacapavir directly.
      #   "total"         = enter one oral total and one lenacapavir total; each
      #                     is auto-split across groups using the shares in
      #                     prep_alloc_shares() (logic file) -- read its SHARE
      #                     PROVENANCE block before quoting the split.
      # Opens in "total" (PREP_ENTRY_MODE_DEFAULT): every country baseline
      # collected to date reports PrEP as product totals, so that is what users
      # should see first. They can switch to "By group" to override the split.
      div(
        style = "margin-bottom: 12px; padding: 10px; background:#eef3f8; border-radius:5px;",
        shinyWidgets::radioGroupButtons(
          inputId      = "prep_entry_mode",
          label        = "PrEP data availability (applies to Baseline and both Scenarios):",
          choiceNames  = c("By group (FSW / MSM / AGYW / General)", "Total"),
          choiceValues = c("disaggregated", "total"),
          selected     = PREP_ENTRY_MODE_DEFAULT,
          justified    = TRUE, size = "sm",
          checkIcon    = list(yes = icon("check"))
        )
      ),
      uiOutput("baseline_ui")
    ),
    
    nav_panel(
      "Scenarios",
      h4("Adjust intervention coverage for two scenarios"),
      p(strong("Note:"), " Scale up (increase) or scale down (decrease) interventions. Clear labels show whether inputs are absolute numbers (people) or percentages (%)."),
      div(
        style = "display: flex; justify-content: flex-end; margin-bottom: 8px;",
        actionButton("reset_scenarios", "Reset both scenarios to baseline",
                     class = "btn-outline-secondary btn-sm",
                     icon = icon("rotate-left"))
      ),
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
        
        # Cascade Chart - Combined
        h3("HIV Care Cascade: Baseline vs Scenarios (End of Year)", class = "mt-4 mb-3"),
        card(
          card_header("Combined Cascade Comparison"),
          card_body(plotOutput("cascade_combined", height = "500px"))
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
        
        # Cost by programme area (stacked) - sits below the Cost Analysis cards
        h3("Cost by Programme Area (US$)", class = "mt-4 mb-3"),
        card(
          card_header("Stacked cost breakdown: Baseline vs Scenarios"),
          card_body(
            plotOutput("cost_breakdown_stacked", height = "400px")
          )
        ),
        
        # card(
        #   card_header("Cascade Numbers"),
        #   card_body(tableOutput("cascade_table"))
        # ),
        
        # Other Outcomes Row
        # h3("Key Outcomes Summary", class = "mt-4 mb-3"),
        # layout_columns(
        #   col_widths = c(6, 6),
        #   card(
        #     card_header("Scenario 1 - Impact Summary"),
        #     card_body(plotOutput("plot_scenario1", height = "400px"))
        #   ),
        #   card(
        #     card_header("Scenario 2 - Impact Summary"),
        #     card_body(plotOutput("plot_scenario2", height = "400px"))
        #   )
        # ),
        
        div(style = "height: 50px;")
      )
    ),
    nav_panel(
      "Parameters",
      h4("Effectiveness and unit cost assumptions"),
      p(strong("Note:"), " Values below represent those used as the defaults for the modelling results, informed by literature or country data where available.",
        "If these differ from your expectations, you can override them for this country session. Selecting a different country will reset these values to the defaults.",
        "This can also be done using the reset button at the bottom of the tab."),
      uiOutput("param_tab_ui")
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
    prep_oral_fsw          = "Number of FSW currently receiving and/or initiated on oral PrEP.",
    prep_oral_msm          = "Number of MSM currently receiving and/or initiated on oral PrEP.",
    prep_oral_agyw         = "Number of AGYW (15-24) currently receiving and/or initiated on oral PrEP.",
    prep_lenacapavir_fsw   = "Number of FSW currently receiving and/or initiated on long-acting injectable PrEP (lenacapavir).",
    prep_lenacapavir_msm   = "Number of MSM currently receiving and/or initiated on long-acting injectable PrEP (lenacapavir).",
    prep_lenacapavir_agyw  = "Number of AGYW (15-24) currently receiving and/or initiated on long-acting injectable PrEP (lenacapavir).",
    prep_oral_general      = "Number of people who are NOT FSW/MSM/AGYW but receive oral PrEP for another reason (e.g. serodiscordant partners, general high risk). Split across the general population internally.",
    prep_lenacapavir_general = "Number of people who are NOT FSW/MSM/AGYW but receive long-acting injectable PrEP (lenacapavir) for another reason. Split across the general population internally.",
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
    clinical_visit_12month = "Frequency at which stable ART clients must return for clinical visits, either every 6 months or every 12 months.",
    
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
  
  # Real population cap for a group (FSW/MSM/AGYW), matching exactly what
  # the logic file's cost loop (min(intervention_value, strata_val$n_*))
  # and compute_prevention_adjustments()'s cov() clip use as denominator.
  # Replaces the former adult_pop x prop_fsw approximation, which ignored
  # the sexually-active/HIV-negative narrowing partition_into_strata()
  # applies and so overstated the cap (e.g. Botswana FSW: ~11,000 approx
  # vs ~8,294 actual).
  group_pop <- function(group = c("fsw", "msm", "agyw", "general")) {
    group <- match.arg(group)
    s <- strata_sizes()
    val <- switch(group,
                  fsw = s$n_fsw, msm = s$n_msm, agyw = s$n_agyw,
                  # General PrEP is capped against the combined general population (the
                  # same denominator the logic file's cost loop and the female/male split
                  # distribute across). This is the total the single general entry covers.
                  general = (s$n_general_female %||% 0) +
                    (s$n_general_male_uncirc %||% 0) +
                    (s$n_general_male_circ %||% 0))
    if (is.null(val) || is.na(val)) NA else val
  }
  
  # ── PrEP total→group allocation (used ONLY in "total" entry mode) ──────────
  # The shares and the splitting algorithm now live in the LOGIC file
  # (prep_alloc_shares() / allocate_prep_totals()), because build_country_presets()
  # needs the identical split to turn the product totals countries report in
  # baseline_testing.csv into the 8 group keys. One implementation, no drift.
  # Read the SHARE PROVENANCE block above prep_alloc_shares() in the logic file
  # before quoting the split to anyone: the PEPFAR FY22-24 shares it defaults to
  # are computed on a denominator that sums four overlapping MER disaggregates,
  # so FSW/MSM/AGYW are understated and General overstated.
  
  # Total DELIVERABLE capacity = combined HIV-negative population across all
  # PrEP groups. With overflow cascading into General (see allocate_prep_totals),
  # this is the largest total that can be fully delivered; beyond it, even a
  # saturated General can't absorb more and the remainder is discarded.
  # Returns Inf if no population is known yet (nothing to cap against).
  prep_total_cap <- function() {
    caps <- tryCatch(
      c(group_pop("fsw"), group_pop("msm"), group_pop("agyw"), group_pop("general")),
      error = function(e) rep(NA_real_, 4))
    if (all(is.na(caps))) return(Inf)
    sum(caps, na.rm = TRUE)
  }
  
  # `%||%` in the interface file does not catch NA. Total-mode PrEP fields can
  # arrive as NULL (widget not rendered -- the tab is a suspended renderUI) or
  # NA (user cleared the box). Both mean "not entered": fall through.
  coalesce_num <- function(x, fallback) {
    if (is.null(x) || length(x) != 1 || is.na(x)) fallback else x
  }
  
  # ---- Scenario "touched" tracking -----------------------------------------
  # Scenario boxes follow baseline until the user types in them, then stay put.
  # PLAIN ENVIRONMENTS, not reactiveValues: if scenario_ui took a dependency on
  # these it would re-render the moment a field was first touched -- while the
  # user is still typing in it.
  scen_seed    <- new.env(parent = emptyenv())  # id -> vector of values WE recently wrote
  scen_touched <- new.env(parent = emptyenv())  # TRUE once a real edit is seen
  
  scen_get   <- function(e, id) if (exists(id, envir = e, inherits = FALSE)) get(id, envir = e) else NULL
  scen_mark  <- function(e, id, v) assign(id, v, envir = e)
  is_touched <- function(id) isTRUE(scen_get(scen_touched, id))
  
  # The browser echoes our writes back asynchronously. If we write seed A and
  # then seed B before A's echo lands, the echo of A arrives and is compared
  # against B -- they differ, so the field is wrongly marked touched and freezes
  # forever. (This happens on every startup: input$total_pop hasn't echoed back
  # yet, so baseline_values() renders once at the wrong scale, then re-renders.)
  # Keep the last few seeds and treat a match against ANY of them as our echo.
  SCEN_SEED_HISTORY <- 5L
  scen_record_seed <- function(id, v) {
    prev <- scen_get(scen_seed, id)
    assign(id, utils::tail(unique(c(prev, v)), SCEN_SEED_HISTORY), envir = scen_seed)
  }
  scen_is_echo <- function(id, v) {
    s <- scen_get(scen_seed, id)
    !is.null(s) && any(vapply(s, function(x) isTRUE(all.equal(v, x)), logical(1)))
  }
  
  # Seed value for a scenario field: its own current value if touched, else the
  # baseline value. Records whichever it used.
  scen_seed_value <- function(id, base_value) {
    v <- if (is_touched(id)) {
      cur <- suppressWarnings(as.numeric(isolate(input[[id]])))
      if (length(cur) == 1 && !is.na(cur)) cur else base_value
    } else base_value
    scen_record_seed(id, v) 
    v
  }
  
  # Write a value into a scenario field WITHOUT marking it touched.
  # unname(): updateNumericInput() serialises a named numeric as a JSON object
  # and blanks the box (the reset bug from the earlier session).
  scen_push <- function(id, value) {
    value <- unname(value)
    if (is.null(value) || length(value) != 1 || is.na(value)) return(invisible(NULL))
    scen_record_seed(id, value) 
    if (grepl("clinical_visit_12month$", id)) {
      shinyWidgets::updateRadioGroupButtons(session, id, selected = if (value >= 50) 100 else 0)
    } else {
      updateNumericInput(session, id, value = value)
    }
    invisible(NULL)
  }
  
  # One observer per scenario field. Marks touched only when the reported value
  # differs from the last value we wrote.
  local({
    int_keys <- unlist(lapply(intervention_groups, function(g) names(g$interventions)),
                       use.names = FALSE)
    scen_ids <- c(paste0("scenario1_", int_keys), paste0("scenario2_", int_keys),
                  "scenario1_prep_total_oral", "scenario1_prep_total_lena",
                  "scenario2_prep_total_oral", "scenario2_prep_total_lena")
    lapply(scen_ids, function(id) {
      observeEvent(input[[id]], {
        v <- suppressWarnings(as.numeric(input[[id]]))   # radioGroupButtons returns character
        if (length(v) != 1 || is.na(v)) return()
        s <- scen_get(scen_seed, id)
        if (scen_is_echo(id, v)) return()   # our own write, echoed back (possibly late)
        scen_mark(scen_touched, id, TRUE)
      }, ignoreInit = TRUE)
    })
  })
  
  # Country switch resets scenarios -- forget which fields were touched.
  # priority = 100 so this runs before the preset observer repopulates them.
  observeEvent(input$region, {
    rm(list = ls(scen_touched, all.names = TRUE), envir = scen_touched)
    rm(list = ls(scen_seed,    all.names = TRUE), envir = scen_seed)
  }, ignoreInit = TRUE, priority = 100)
  
  # Thin wrapper over the logic file's allocate_prep_totals(). Supplies the caps
  # from group_pop(), which reads the same partition_into_strata() output the
  # cost loop and compute_prevention_adjustments() use as denominator.
  split_prep_total <- function(total_oral, total_lena) {
    # isolate(): the caps come from strata_sizes() -> define_strata_params(context()),
    # so without this, baseline_input_values() takes a dependency on the WHOLE of
    # context() -- and any Parameters-tab edit re-renders scenario_ui and wipes the
    # user's scenario entries. define_strata_params() reads only prop_fsw/rr_fsw/
    # prop_msm/rr_msm/prop_agyw/rr_agyw/prop_pop_male/circ_prevalence, none of which
    # param_overrides can change, so isolating loses no real reactivity here.
    # The caps DO change on a country or total_pop switch -- both of those also
    # change baseline_values(), which re-invalidates this path and re-reads them.
    # tryCatch mirrors prep_total_cap() above: strata_sizes() may be unavailable
    # on an early flush, before the input$region observer has populated
    # country_calibration()/original_baseline(). allocate_prep_totals() tests
    # each cap for NA before clipping, so NA caps mean "no clip" -- safe at
    # startup, and the real caps arrive on the next flush.
    caps <- isolate(tryCatch(
      list(fsw     = group_pop("fsw"),
           msm     = group_pop("msm"),
           agyw    = group_pop("agyw"),
           general = group_pop("general")),
      error = function(e) list(fsw = NA_real_, msm = NA_real_,
                               agyw = NA_real_, general = NA_real_)))
    allocate_prep_totals(total_oral, total_lena, caps = caps)
  }
  
  # Shared builder for the PrEP cap warnings so the oral and lenacapavir
  # messages stay symmetric and both state the REAL binding constraint: it is
  # the COMBINED oral + lenacapavir total that is capped at the population, not
  # the edited field on its own. Quoting the population without the other
  # product's value (the old oral message) makes the cap look like an
  # arithmetic error, since cap = population - other_product.
  prep_cap_message <- function(scenario_prefix, field_label, capped_to,
                               other_label, other_val, limit_label, limit_val) {
    paste0(scenario_prefix, ": ", field_label, " capped at ",
           format(round(capped_to), big.mark = ","),
           " — oral + lenacapavir combined can't exceed ", limit_label,
           " (", format(round(limit_val), big.mark = ","), "). ",
           other_label, " is currently ",
           format(round(other_val), big.mark = ","), ".")
  }
  
  # Registers the oral/lenacapavir cross-validation pair for one scenario +
  # group (FSW/MSM/AGYW), replacing the old single adult_pop cap that used
  # to cross-validate baseline_prep_oral against baseline_prep_lenacapavir
  # directly. Called once per scenario x group combination below (9 calls,
  # 18 observers total -- matches the 18 new group-specific PrEP inputs).
  register_prep_group_validation <- function(input, session, scenario_prefix, group, group_label) {
    oral_id <- paste0(scenario_prefix, "_prep_oral_", group)
    lena_id <- paste0(scenario_prefix, "_prep_lenacapavir_", group)
    
    observe({
      n_group <- group_pop(group)
      if (is.na(n_group)) return()
      isolate({
        oral_val <- input[[oral_id]]; lena_val <- input[[lena_id]]
        if (!is.null(oral_val) && !is.null(lena_val) && !is.na(oral_val) && !is.na(lena_val)) {
          if (oral_val + lena_val > n_group) {
            max_oral <- max(0, n_group - lena_val)
            updateNumericInput(session, oral_id, value = round(max_oral), max = round(n_group))
            showNotification(
              prep_cap_message(scenario_prefix,
                               paste0("Oral PrEP (", group_label, ")"), max_oral,
                               "Lenacapavir", lena_val,
                               paste0("the estimated ", group_label, " population"), n_group),
              type = "warning", duration = 6)
          }
        }
      })
    }) %>% bindEvent(input[[oral_id]])
    
    observe({
      n_group <- group_pop(group)
      if (is.na(n_group)) return()
      isolate({
        oral_val <- input[[oral_id]]; lena_val <- input[[lena_id]]
        if (!is.null(oral_val) && !is.null(lena_val) && !is.na(oral_val) && !is.na(lena_val)) {
          if (oral_val + lena_val > n_group) {
            max_lena <- max(0, n_group - oral_val)
            updateNumericInput(session, lena_id, value = round(max_lena), max = round(n_group))
            showNotification(
              prep_cap_message(scenario_prefix,
                               paste0("Lenacapavir (", group_label, ")"), max_lena,
                               "Oral PrEP", oral_val,
                               paste0("the estimated ", group_label, " population"), n_group),
              type = "warning", duration = 6)
          }
        }
      })
    }) %>% bindEvent(input[[lena_id]])
  }
  
  for (sp in c("baseline", "scenario1", "scenario2")) {
    register_prep_group_validation(input, session, sp, "fsw",     "FSW")
    register_prep_group_validation(input, session, sp, "msm",     "MSM")
    register_prep_group_validation(input, session, sp, "agyw",    "AGYW")
    register_prep_group_validation(input, session, sp, "general", "General")
  }
  
  # Total-mode hard cap: when the entered oral + lenacapavir totals would push
  # any group over 100% of its population, rewrite the just-edited field down to
  # the maximum feasible value (mirrors the per-group cross-validation above).
  register_prep_total_validation <- function(scenario_prefix) {
    oral_id <- paste0(scenario_prefix, "_prep_total_oral")
    lena_id <- paste0(scenario_prefix, "_prep_total_lena")
    
    observe({
      t_max <- prep_total_cap(); if (!is.finite(t_max)) return()
      isolate({
        o <- input[[oral_id]]; l <- input[[lena_id]]
        if (!is.null(o) && !is.null(l) && !is.na(o) && !is.na(l) && (o + l) > t_max) {
          new_o <- max(0, t_max - l)
          updateNumericInput(session, oral_id, value = round(new_o), max = round(t_max))
          showNotification(
            prep_cap_message(scenario_prefix, "Total oral PrEP", new_o,
                             "Total lenacapavir", l,
                             "the estimated HIV-negative population across all PrEP groups", t_max),
            type = "warning", duration = 6)
        }
      })
    }) %>% bindEvent(input[[oral_id]])
    
    observe({
      t_max <- prep_total_cap(); if (!is.finite(t_max)) return()
      isolate({
        o <- input[[oral_id]]; l <- input[[lena_id]]
        if (!is.null(o) && !is.null(l) && !is.na(o) && !is.na(l) && (o + l) > t_max) {
          new_l <- max(0, t_max - o)
          updateNumericInput(session, lena_id, value = round(new_l), max = round(t_max))
          showNotification(
            prep_cap_message(scenario_prefix, "Total lenacapavir", new_l,
                             "Total oral PrEP", o,
                             "the estimated HIV-negative population across all PrEP groups", t_max),
            type = "warning", duration = 6)
        }
      })
    }) %>% bindEvent(input[[lena_id]])
  }
  for (sp in c("baseline", "scenario1", "scenario2")) register_prep_total_validation(sp)
  
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
  
  # ==========================================================================
  # PARAMETER TAB — session-scoped overrides
  # --------------------------------------------------------------------------
  # Scoped to the session, never global: two people using the tool at once must
  # not see each other's numbers. Cleared on country switch (input$region)
  # because PrEP/ART unit costs are country-specific and carrying an edit across
  # countries would silently price one country at another's numbers.
  #
  # STORAGE is per-key, matching the sheet:
  #   $eff[[intervention_key]][[parameter_type]] -> numeric
  #   $cost[[intervention_key]]                  -> numeric (8 PrEP unit costs)
  #   $art_cost                                  -> numeric or NULL
  #   $dsd_abs[[intervention_key]]               -> numeric (total USD per person-year under the model)
  #
  # The UI is deliberately NARROWER than the storage: one shared efficacy input
  # per product fans out to that product's four group keys. So the sheet keeps
  # its per-key shape (and derive_prep_efficacy(), test_14 and the logic file
  # are untouched by this tab), while the user sees the structure they actually
  # reason in.
  #
  # UNITS: durations are entered in MONTHS and efficacies in PERCENT; both are
  # converted at the UI boundary (/12, /100). The sheet stays in person-years
  # and fractions -- entering months into the sheet is exactly what
  # derive_prep_efficacy()'s py > 1 guard exists to reject, and a percent would
  # trip its eff_adherent [0,1] stop() at Mock-Up_logic_V3.R ~325. Conversion
  # lives in one place: mo_to_years() / years_to_mo() / pct_to_frac() /
  # frac_to_pct() below.
  # ==========================================================================
  # dsd_abs was missing from this init: param_overrides$dsd_abs was NULL until the
  # first country switch, which is what the capdsd_ guard further down is working
  # around. Initialised here for symmetry with the reset; the guard stays.
  param_overrides <- reactiveValues(eff = list(), cost = list(), cost_test = list(),
                                    cost_flat = list(), dsd_abs = list(),
                                    art_cost = NULL)
  
  # ART cost adjustments. The SHEET unit_cost for these five keys stays a FRACTION
  # of art_cost_standard (negative = saving), not USD -- see Mock-Up_logic_V3.R
  # ~2584 (DSD bundle) and ~2481 (clinical_visit_12month). Two accumulators, one
  # convention. The Parameters tab takes the TOTAL ART cost/PY under each model
  # and converts to that fraction at the boundary as (total - art_cost)/art_cost.
  # The Parameters tab takes USD and converts at the reactive boundary.
  # Split only for LAYOUT (one row each). DSD_COST_KEYS stays the single source
  # of truth for the conversion, the observers and the reset -- never iterate
  # the two halves separately outside the UI.
  MMD_KEYS       <- c("mmd_3month", "mmd_6month", "mmd_12month")
  DSD_OTHER_KEYS <- c("community_pickup", "clinical_visit_12month")
  DSD_COST_KEYS  <- c(MMD_KEYS, DSD_OTHER_KEYS)
  
  ORAL_KEYS <- c("prep_oral_fsw", "prep_oral_msm", "prep_oral_agyw", "prep_oral_general")
  LEN_KEYS  <- c("prep_lenacapavir_fsw", "prep_lenacapavir_msm",
                 "prep_lenacapavir_agyw", "prep_lenacapavir_general")
  PREP_KEYS <- c(ORAL_KEYS, LEN_KEYS)
  
  # ==========================================================================
  # COST KEY SETS -- three channels, not one
  # --------------------------------------------------------------------------
  # A unit cost reaches the model by exactly one of three routes, and picking the
  # wrong one gives a box that moves while the number doesn't:
  #
  #   COST_PREV_KEYS  -> context$cost_overrides_prep  (Mock-Up_logic_V3.R ~3339)
  #   COST_TEST_KEYS  -> context$cost_overrides_test  (~2389 / ~2395)
  #   COST_FLAT_KEYS  -> effective_intervention_params()$unit_cost -> ig_src (~2223)
  #
  # The first two are read as `overrides[[int_key]] %||% intervention$unit_cost`.
  # `%||%` only falls through on NULL, so for any key the COUNTRY CSV populates,
  # injecting into unit_cost is SHADOWED and does nothing -- the failure already
  # documented on the cost_overrides_prep line in context() below.
  # basic_hiv_data.csv populates all nine COST_TEST_KEYS for every country, so
  # those MUST take route 2.
  #
  # vmmc/condoms have no CSV column and would work on route 3 today, but they are
  # charged in the prevention loop whose lookup is generic over int_key -- so
  # route 1 is future-proof against someone adding a `cost_vmmc` column and
  # silently re-creating the shadow. infant_prophylaxis is NOT in that loop
  # (~2431), so it cannot use route 1.
  #
  # eid is deliberately absent from COST_TEST_KEYS: the logic's test_cost_keys
  # list (~1205) excludes EID/VL/CD4, and eid's cost is charged post-loop off
  # all_interventions$eid$unit_cost (~3507). Route 3.
  # ==========================================================================
  COST_PREV_KEYS <- c(PREP_KEYS, "vmmc", "condoms")
  COST_TEST_KEYS <- c("test_facility_general", "test_network", "test_index",
                      "test_community", "test_kpsti",
                      "hivst_facility", "hivst_community",
                      "anc_hiv_testing", "pnc_hiv_testing")
  COST_FLAT_KEYS <- c("infant_prophylaxis", "eid", "vl_monitoring_routine",
                      "adherence_counseling", "tracking_tracing",
                      "anc_vl_testing", "pnc_vl_testing",
                      "cd4_testing", "ahd_package")
  ALL_COST_KEYS  <- c(COST_PREV_KEYS, COST_TEST_KEYS, COST_FLAT_KEYS)
  
  # key -> group name, built the same way the logic flattens ig_src (~2223), so a
  # key added to intervention_groups is found here without a second list to
  # maintain. Built off the GLOBAL intervention_groups: this resolves DEFAULTS,
  # and a default must not move when the user edits.
  INT_GROUP_OF <- local({
    m <- list()
    for (g in names(intervention_groups))
      for (k in names(intervention_groups[[g]]$interventions)) m[[k]] <- g
    m
  })
  
  # The unit cost the logic will actually charge for `key` at defaults --
  # resolved off the BUILT intervention_groups, not sheet_val(). This matters:
  # several definitions close over a fallback (pnc_vl_testing's `%||% 0`, the PrEP
  # per-group -> blended chain), and a raw sheet read would show a blank box next
  # to a model charging the fallback. NA when the value is absent or numeric(0);
  # currencyNumericInput() renders that as an empty box.
  built_unit_cost <- function(key) {
    g <- INT_GROUP_OF[[key]]
    v <- if (!is.null(g)) intervention_groups[[g]]$interventions[[key]]$unit_cost else NULL
    if (length(v) == 1 && !is.na(v)) unname(as.numeric(v)) else NA_real_
  }
  
  # Duration is capped at 12 months. The model runs a single year, so protection
  # generated per initiation cannot exceed one person-year; 12 months is the
  # same ceiling derive_prep_efficacy() enforces as py <= 1, expressed in the
  # unit the user is typing in.
  MAX_DURATION_MONTHS <- 12
  mo_to_years <- function(m) m / 12
  years_to_mo <- function(y) {
    if (length(y) == 1 && !is.na(y)) unname(round(y * 12, 2)) else NA_real_
  }
  
  # Efficacy is ENTERED as a percentage (74) and STORED as a fraction (0.74).
  # Same boundary discipline as mo_to_years(): fractions are what the sheet,
  # derive_prep_efficacy(), test_14 and the logic file speak, and none of them
  # change. round() at 8 dp, not 2: this round trip (sheet -> frac_to_pct ->
  # box -> echo -> pct_to_frac) must land inside all.equal()'s 1.5e-8 relative
  # tolerance in set_eff_override(), or an untouched box reports itself as a
  # change -- 0.74 * 100 is not exactly 74 in binary floating point. 8 dp leaves
  # ~5e-11 absolute error and does not alter the display of any 2 dp sheet
  # value (0.74 -> "74").
  pct_to_frac <- function(p) p / 100
  frac_to_pct <- function(f) {
    if (length(f) == 1 && !is.na(f)) unname(round(f * 100, 8)) else NA_real_
  }
  
  # Vectorised display formatters for shared_default()'s conflict detail, which
  # must read in the SAME unit as the box above it. Not the inverse of the
  # converters above -- these take the sheet value and format it for display,
  # NA-safe because ifelse() in shared_default() substitutes "missing" after the
  # fact (sprintf(NA) would otherwise print "NA%").
  pct_detail_fmt <- function(x) sprintf("%g%%", x * 100)
  mo_detail_fmt  <- function(x) sprintf("%g mo", x * 12)
  
  prep_group_label <- function(key) {
    if (grepl("_fsw$", key))     "FSW"
    else if (grepl("_msm$", key))  "MSM"
    else if (grepl("_agyw$", key)) "AGYW"
    else                           "General population"
  }
  
  sheet_val <- function(key, pt, ip = intervention_params) {
    v <- subset(ip, intervention_key == key)[[pt]]
    if (length(v) == 1) unname(as.numeric(v)) else NA_real_
  }
  sheet_src <- function(key, pt, ip = intervention_params) {
    v <- subset(ip, intervention_key == key)[[paste0("src_", pt)]]
    if (length(v) == 1 && !is.na(v) && nzchar(v)) v else NA_character_
  }
  
  # A value the UI presents as SHARED across a product's four group keys, read
  # back from four independent sheet rows. If those rows disagree, one input
  # cannot represent them -- so surface it rather than silently overwrite three
  # of the four on the first edit.
  shared_default <- function(keys, pt, fmt = format) {
    # USE.NAMES = FALSE is load-bearing: vapply over a CHARACTER vector returns a
    # NAMED result by default, and a named numeric survives into
    # updateNumericInput(), which serialises it to JSON as an object
    # ({"prep_oral_fsw": 0.74}) rather than a scalar -- the client then renders
    # the box empty. numericInput() hides this at first render because it runs
    # the value through as.character(), which drops names. unname() below is the
    # belt to this braces.
    vals <- vapply(keys, sheet_val, numeric(1), pt = pt, USE.NAMES = FALSE)
    present <- vals[!is.na(vals)]
    u <- unique(round(present, 10))
    list(value    = if (length(present) >= 1) unname(present[1]) else NA_real_,
         conflict = length(u) > 1,
         detail   = paste(sprintf("%s: %s",
                                  vapply(keys, prep_group_label, character(1), USE.NAMES = FALSE),
                                  ifelse(is.na(vals), "missing", fmt(vals))),
                          collapse = "; "))
  }
  
  # DSD entries count as a change only when the typed TOTAL differs from the
  # sheet default total. Typing the default back in leaves the stored total in
  # place -- the box stays authoritative, the result stays order-independent --
  # but it must not report as a change that isn't one.
  # Compare at the precision the box can express. The field is 2dp, so a sheet
  # fraction of -0.05 against Botswana's art_cost_standard of 221.82 shows as a
  # total of round(221.82 * 0.95, 2) = 210.73; comparing the derived fraction
  # (210.73 - 221.82)/221.82 = -0.049995 against -0.05 fails all.equal (diff
  # 4.5e-6 vs tolerance 1.5e-8). Every art_cost_standard carries 2 decimals, so
  # ac*(1+frac) rarely lands on a clean 2dp value. Comparing totals at 2dp -- the
  # value the box echoes on first render -- keeps an untouched tab at zero changes.
  #
  # Consequence: at "default" the model applies (round(ac*(1+f),2) - ac)/ac
  # rather than f exactly -- ~1e-5 relative error, far below parameter uncertainty.
  
  n_dsd_changed <- reactive({
    ac <- effective_art_cost()
    if (!ac_ok(ac)) return(0L)
    ks <- names(param_overrides$dsd_abs)
    if (length(ks) == 0) return(0L)
    sum(vapply(ks, function(k) {
      d <- dsd_cost_default(k, ac)$total
      !(length(d) == 1 && !is.na(d) &&
          isTRUE(all.equal(round(param_overrides$dsd_abs[[k]], 2), round(d, 2))))
    }, logical(1)))
  })
  
  # The three cost lists are length()-counted rather than compared: the observers
  # DROP an entry the moment its value returns to the default, the same contract
  # set_eff_override() keeps. n_dsd_changed() has to compare because the DSD boxes
  # store a derived absolute, not a raw override.
  n_overrides <- reactive({
    length(unlist(param_overrides$eff)) +
      length(param_overrides$cost) + length(param_overrides$cost_test) +
      length(param_overrides$cost_flat) +
      as.integer(!is.null(param_overrides$art_cost)) + n_dsd_changed()
  })
  
  # Clear on country switch. priority = 100 so this runs BEFORE the preset
  # observer rebuilds country_calibration(); otherwise context() would rebuild
  # once with the new country's CSV costs and the old country's edits on top.
  observeEvent(input$region, {
    had <- isolate(n_overrides()) > 0
    param_overrides$eff       <- list()
    param_overrides$cost      <- list()
    # Clearing cost_test on country switch is not optional: the nine test costs
    # are country-specific in basic_hiv_data.csv, so an edit carried across
    # countries would price one country at another's numbers -- the reason this
    # observer exists.
    param_overrides$cost_test <- list()
    param_overrides$cost_flat <- list()
    param_overrides$dsd_abs   <- list()
    param_overrides$art_cost  <- NULL
    if (had) {
      showNotification(
        "Parameter edits were reset to the new country's defaults.",
        type = "warning", duration = 6)
    }
  }, ignoreInit = TRUE, priority = 100)
  
  effective_intervention_params <- reactive({
    ip <- intervention_params
    for (k in names(param_overrides$eff)) {
      for (pt in names(param_overrides$eff[[k]])) {
        ip[[pt]][ip$intervention_key == k] <- param_overrides$eff[[k]][[pt]]
      }
    }
    # Flat unit costs (COST_FLAT_KEYS only). Safe HERE and nowhere else: none of
    # these keys appears in cost_overrides_prep/_test, so nothing shadows the
    # injection. Doing the same for a COST_TEST_KEY would move the box and not the
    # model. Disjoint from the DSD keys written below.
    for (k in names(param_overrides$cost_flat)) {
      ip$unit_cost[ip$intervention_key == k] <- param_overrides$cost_flat[[k]]
    }
    # DSD TOTALS -> sheet fractions at the ART cost in force RIGHT NOW. The box
    # holds the full ART cost/PY under the model; the sheet fraction the logic
    # multiplies is the DELTA over standard, so recover it as (total - ac)/ac.
    # The ac used here and the ac the logic multiplies by are the same number,
    # so the typed total is what gets charged regardless of later edits. ac <= 0
    # leaves the sheet fraction; the caption says the entry isn't applied.
    ac <- effective_art_cost()
    if (ac_ok(ac)) {
      for (k in names(param_overrides$dsd_abs)) {
        ip$unit_cost[ip$intervention_key == k] <- (param_overrides$dsd_abs[[k]] - ac) / ac
      }
    }
    ip
  })
  
  # build_intervention_groups() re-runs derive_prep_efficacy() on the edited
  # values. This is the point of the design: the derived figure on the card and
  # the figure the model uses come from the same call, so they cannot drift.
  effective_intervention_groups <- reactive({
    build_intervention_groups(effective_intervention_params())
  })
  
  # Write one value to one (key, parameter_type). Dropping the override when the
  # value returns to the sheet default keeps the report's "changed parameters"
  # table honest -- otherwise it would list a change that isn't one.
  set_eff_override <- function(key, pt, v) {
    e <- param_overrides$eff
    sheet <- sheet_val(key, pt)
    if (length(sheet) == 1 && !is.na(sheet) && isTRUE(all.equal(v, sheet))) {
      if (!is.null(e[[key]])) {
        e[[key]][[pt]] <- NULL
        if (length(e[[key]]) == 0) e[[key]] <- NULL
      }
    } else {
      if (is.null(e[[key]])) e[[key]] <- list()
      e[[key]][[pt]] <- v
    }
    param_overrides$eff <- e
  }
  
  # Renamed from prep_cost_default(): the prevention cost loop's override lookup
  # is generic over int_key, so vmmc and condoms resolve through exactly the same
  # chain as the eight PrEP keys. The fallback now goes through built_unit_cost()
  # rather than reading $unit_cost directly -- vmmc/condoms have no `%||%` chain in
  # their definitions, so a missing sheet row gave numeric(0) here.
  prevention_cost_default <- function(key, cc) {
    csv_val <- if (!is.null(cc)) cc$cost_overrides_prep[[key]] else NULL
    if (!is.null(csv_val) && !is.na(csv_val)) {
      return(list(value = csv_val, source = "country data (basic_hiv_data.csv)"))
    }
    list(value = built_unit_cost(key), source = "global default (intervention_params)")
  }
  
  # Same shape, other channel. All nine keys are populated by basic_hiv_data.csv
  # for every country in the file, so in practice this always returns the country
  # branch -- the global branch is the Custom Country / missing-column path.
  test_cost_default <- function(key, cc) {
    csv_val <- if (!is.null(cc)) cc$cost_overrides_test[[key]] else NULL
    if (!is.null(csv_val) && !is.na(csv_val)) {
      return(list(value = csv_val, source = "country data (basic_hiv_data.csv)"))
    }
    list(value = built_unit_cost(key), source = "global default (intervention_params)")
  }
  
  # No country channel exists for these keys BY DESIGN: the logic's test_cost_keys
  # list (~1205) excludes EID/VL/CD4. $source is therefore always global, for every
  # country -- which is exactly why the captions below spell the source out rather
  # than leaving the user to assume the tab is uniformly country-specific.
  flat_cost_default <- function(key) {
    list(value = built_unit_cost(key), source = "global default (intervention_params)")
  }
  
  # WHICH reactiveValues list a key's override lives in, and WHICH default it is
  # measured against. One place each, so the input observers, the captions, the
  # reset and n_overrides() cannot disagree about where a key went.
  cost_store_of <- function(key) {
    if (key %in% COST_TEST_KEYS) "cost_test"
    else if (key %in% COST_FLAT_KEYS) "cost_flat"
    else "cost"
  }
  cost_default_of <- function(key, cc) {
    if (key %in% COST_TEST_KEYS) test_cost_default(key, cc)
    else if (key %in% COST_FLAT_KEYS) flat_cost_default(key)
    else prevention_cost_default(key, cc)
  }
  
  art_cost_default <- function(cc) {
    v <- if (!is.null(cc)) cc$art_cost_standard else NULL
    if (!is.null(v) && !is.na(v)) {
      list(value = v, source = "country data (basic_hiv_data.csv)")
    } else {
      list(value = ART_COST_STANDARD, source = "global default (intervention_params)")
    }
  }
  
  # The ART unit cost the logic will actually use, resolved through the existing
  # art_cost_default() chain rather than a second copy of it. context() builds
  # `art_cost_standard` the same way at line ~1485 and the logic closes it with
  # `%||% ART_COST_STANDARD`. If this ever diverges from context(), the USD the
  # user types and the USD the model charges diverge silently.
  effective_art_cost <- reactive({
    param_overrides$art_cost %||% art_cost_default(country_calibration())$value
  })
  
  # Sheet fraction for a DSD key + the USD/person-year it currently equals.
  dsd_cost_default <- function(key, ac) {
    f <- sheet_val(key, "unit_cost")
    ok <- length(f) == 1 && !is.na(f)
    # frac: signed sheet fraction (saving = negative). abs: the USD DELTA it
    # equals (f * ac). total: the FULL ART cost/PY under the model the box now
    # shows = standard + delta. Box is parameterised on `total`; the boundary
    # recovers the delta as (total - ac)/ac back into the sheet fraction.
    list(frac  = f,
         abs   = if (ok) unname(f * ac)      else NA_real_,
         total = if (ok) unname(ac + f * ac) else NA_real_)
  }
  
  # Short display names for the cost boxes. NOT all_interventions[[key]]$name:
  # those are the long Scenarios-tab labels ("Testing: facility-based (general)")
  # and wrap to three lines in a 3/12 column. The UNIT lives in the param_hdr()
  # above each row rather than being repeated twenty times -- except condoms,
  # whose unit (per condom, not per person) differs from everything around it and
  # whose units_costed is the raw uncapped entry (Mock-Up_logic_V3.R ~3316).
  cost_label <- function(key) switch(key,
                                     vmmc                  = "VMMC",
                                     condoms               = "Condoms (per condom)",
                                     infant_prophylaxis    = "Infant prophylaxis",
                                     test_facility_general = "Facility (general)",
                                     test_network          = "Network",
                                     test_index            = "Index",
                                     test_community        = "Community",
                                     test_kpsti            = "KP / STI services",
                                     hivst_facility        = "Self-test: facility",
                                     hivst_community       = "Self-test: community",
                                     anc_hiv_testing       = "ANC HIV testing",
                                     pnc_hiv_testing       = "PNC HIV testing",
                                     eid                   = "Early infant diagnosis",
                                     vl_monitoring_routine = "Routine viral load",
                                     adherence_counseling  = "Enhanced adherence counselling",
                                     tracking_tracing      = "Tracking & tracing",
                                     anc_vl_testing        = "ANC viral load",
                                     pnc_vl_testing        = "PNC viral load",
                                     cd4_testing           = "CD4 testing",
                                     ahd_package           = "AHD package",
                                     key)
  
  # Short: these sit in 4/12-width columns under a param_hdr(), so the long
  # form ("MMD: 3-month dispensing") wrapped to three lines.
  dsd_label <- function(key) switch(key,
                                    mmd_3month             = "MMD: 3-month",
                                    mmd_6month             = "MMD: 6-month",
                                    mmd_12month            = "MMD: 12-month",
                                    community_pickup       = "Community pick-up",
                                    clinical_visit_12month = "Annual clinical visit",
                                    key)
  
  ac_ok  <- function(ac) length(ac) == 1 && !is.na(ac) && ac > 0
  usd_fmt <- function(x) scales::dollar(x, accuracy = 0.01)
  pct_fmt <- function(f) sprintf("%+.1f%%", f * 100)
  
  # ==========================================================================
  # CHANGED-PARAMETER SUMMARY (PDF appendix)
  # --------------------------------------------------------------------------
  # Returns data.frame(Section, Parameter, Default, `Set to`) listing every
  # value that currently differs from its default, or a ZERO-ROW frame when
  # nothing has been touched. The report omits the appendix entirely on zero
  # rows, so this function -- not n_overrides() -- is the single arbiter of
  # "was anything changed". They can disagree by design: n_overrides() counts
  # the fanned-out store entries for the badge, this collapses them for reading.
  #
  # Three things this deliberately does NOT do the naive way:
  #   * The shared efficacy inputs FAN OUT to four group keys each (see the
  #     param_oral_eff / param_len_eff / param_len_dur observers). Listing the
  #     raw store would print four identical rows for one edit, so those types
  #     collapse to a single row -- but ONLY when all four keys carry the
  #     override AND agree. A partial or divergent state falls through to the
  #     per-key loop rather than hiding behind one summary line.
  #   * DSD costs are tested by COMPARISON, not presence, because the box stores
  #     a derived absolute and the (round(ac*(1+f),2) - ac)/ac round trip carries
  #     ~1e-5 relative error -- the same reason n_dsd_changed() compares.
  #     Presence-testing would report untouched rows as changed.
  #   * Values are formatted in the unit the PARAMETERS TAB shows (percent,
  #     months, USD), not the unit the sheet stores (fraction, years), so the
  #     appendix and the box the user actually edited read the same.
  #
  # Overrides are cleared on country switch (the input$region observer above),
  # so this is always relative to the currently selected country -- no staleness.
  param_override_summary <- reactive({
    cc <- country_calibration()
    ac <- effective_art_cost()
    
    rows <- list()
    add <- function(section, parameter, default, set_to) {
      rows[[length(rows) + 1]] <<- data.frame(
        Section = section, Parameter = parameter,
        Default = default, `Set to` = set_to,
        check.names = FALSE, stringsAsFactors = FALSE
      )
    }
    
    ok1      <- function(x) length(x) == 1 && !is.na(x)
    f_pct    <- function(f) if (ok1(f)) sprintf("%g%%", round(f * 100, 6)) else "\u2014"
    f_mo     <- function(y) if (ok1(y)) sprintf("%g months", round(y * 12, 2)) else "\u2014"
    f_usd    <- function(x) if (ok1(x)) usd_fmt(x) else "\u2014"
    int_name <- function(k) {
      g  <- INT_GROUP_OF[[k]]
      nm <- if (!is.null(g)) intervention_groups[[g]]$interventions[[k]]$name else NULL
      if (is.null(nm)) k else nm
    }
    
    e <- param_overrides$eff
    
    # ---- efficacy and duration: collapse the fan-out types first ------------
    collapsed <- character(0)
    fanout <- list(
      list(keys = ORAL_KEYS, pt = "eff_adherent",
           label = "Oral PrEP \u2014 efficacy when adherent",   fmt = f_pct),
      list(keys = LEN_KEYS,  pt = "eff_adherent",
           label = "Lenacapavir \u2014 efficacy when adherent", fmt = f_pct),
      list(keys = LEN_KEYS,  pt = "shot_coverage_years",
           label = "Lenacapavir \u2014 protection per shot",    fmt = f_mo)
    )
    for (f in fanout) {
      vals <- lapply(f$keys, function(k) e[[k]][[f$pt]])
      if (!all(vapply(vals, ok1, logical(1)))) next
      if (length(unique(round(unlist(vals), 10))) != 1) next
      defs <- vapply(f$keys, function(k) sheet_val(k, f$pt), numeric(1))
      add("Efficacy", f$label,
          if (length(unique(round(defs, 10))) == 1) f$fmt(defs[[1]]) else "varies by group",
          f$fmt(vals[[1]]))
      collapsed <- c(collapsed, paste(f$keys, f$pt, sep = "|"))
    }
    
    pt_label <- c(eff_adherent         = "efficacy when adherent",
                  shot_coverage_years  = "protection per shot",
                  person_years_on_prep = "time on PrEP per initiate",
                  efficacy             = "suppression impact")
    pt_fmt <- list(eff_adherent         = f_pct,
                   shot_coverage_years  = f_mo,
                   person_years_on_prep = f_mo,
                   efficacy             = f_pct)
    for (k in names(e)) {
      for (pt in names(e[[k]])) {
        if (paste(k, pt, sep = "|") %in% collapsed) next
        fmt <- pt_fmt[[pt]]
        if (is.null(fmt)) fmt <- function(x) if (ok1(x)) format(x) else "\u2014"
        lab <- pt_label[[pt]]
        if (is.null(lab) || is.na(lab)) lab <- pt
        add("Efficacy", paste0(int_name(k), " \u2014 ", lab),
            fmt(sheet_val(k, pt)), fmt(e[[k]][[pt]]))
      }
    }
    
    # ---- unit costs: three stores, one default resolver ---------------------
    for (store in c("cost", "cost_test", "cost_flat")) {
      lst <- param_overrides[[store]]
      for (k in names(lst)) {
        d <- cost_default_of(k, cc)
        add("Unit costs", paste0(cost_label(k), " (default from ", d$source, ")"),
            f_usd(d$value), f_usd(lst[[k]]))
      }
    }
    
    # ---- ART cost per person-year, and the DSD models priced off it ---------
    if (!is.null(param_overrides$art_cost)) {
      d <- art_cost_default(cc)
      add("ART cost",
          paste0("Standard ART cost per person-year (default from ", d$source, ")"),
          f_usd(d$value), f_usd(param_overrides$art_cost))
    }
    if (ac_ok(ac)) {
      for (k in names(param_overrides$dsd_abs)) {
        d <- dsd_cost_default(k, ac)$total
        v <- param_overrides$dsd_abs[[k]]
        # Same comparison n_dsd_changed() uses -- see the header note.
        if (ok1(d) && isTRUE(all.equal(round(v, 2), round(d, 2)))) next
        add("ART cost",
            paste0(dsd_label(k), " \u2014 total ART cost per person-year"),
            f_usd(d), f_usd(v))
      }
    }
    
    if (length(rows) == 0) {
      return(data.frame(Section = character(0), Parameter = character(0),
                        Default = character(0), `Set to` = character(0),
                        check.names = FALSE, stringsAsFactors = FALSE))
    }
    do.call(rbind, rows)
  })
  
  # Info bubble, same construction as make_intervention_tip() on the Scenarios
  # tab. Returns NULL when there's no text, so the icon is skipped rather than
  # appearing with an empty bubble.
  param_tip <- function(txt) {
    if (!(length(txt) == 1 && !is.na(txt) && nzchar(txt))) return(NULL)
    tooltip(
      bsicons::bs_icon("info-circle", class = "text-primary",
                       style = "cursor: help; font-size: 0.9em; margin-left: 4px;"),
      txt, placement = "right"
    )
  }
  lbl_tip <- function(text, txt) tagList(text, param_tip(txt))
  
  # A parameter description that heads a row of per-group inputs. Styled as a
  # real control-label so it sits at the SAME level as the shared inputs'
  # labels ("Efficacy when adherent") -- the two are the same kind of thing.
  # The group names below it are then sub-labels, not peers.
  param_hdr <- function(text, txt = NA_character_) {
    tags$label(class = "control-label",
               style = "display:block; margin:5px 0 1px;", 
               text, param_tip(txt))
  }
  # Group name under a param_hdr: deliberately lighter than the header above it.
  grp_lbl <- function(key, txt) {
    tagList(tags$span(style = "font-size:11px; color:#6b7280; font-weight:400;",
                      prep_group_label(key)),
            param_tip(txt))
  }
  # Same idea for the DSD boxes, which sit under their own param_hdr().
  dsd_lbl <- function(key, txt) {
    tagList(tags$span(style = "font-size:11px; color:#6b7280; font-weight:400;",
                      dsd_label(key)),
            param_tip(txt))
  }
  # And for the unit-cost boxes. Separate from grp_lbl() because these keys are
  # distinct interventions, not four groups of one product.
  cost_lbl <- function(key, txt) {
    tagList(tags$span(style = "font-size:11px; color:#6b7280; font-weight:400;",
                      cost_label(key)),
            param_tip(txt))
  }
  
  # A cost box whose default could not be resolved at all. This is a WARNING, not
  # a caption: it fires only when there is no value to show, and stays silent
  # otherwise -- the source-of-default captions that used to live here were
  # removed as noise (they rendered under all 29 boxes).
  #
  # It is worth keeping because a key with no unit_cost row reaches the logic as
  # numeric(0), and charge_cost()'s `total + numeric(0)` is numeric(0) -- one
  # missing row can zero a whole cost category. It deliberately does NOT claim the
  # model charges nothing; it says the sheet is incomplete, which is the part that
  # is actually known.
  #
  # $source is once again computed and never rendered by the default helpers
  # above. Left in place: it is the natural hook if the source ever needs showing.
  missing_default_note <- function(d) {
    if (length(d$value) == 1 && !is.na(d$value)) return(NULL)
    div(style = "font-size:11px; color:#b45309; margin-top:2px;",
        "No default — intervention_params has no unit cost for this key.")
  }
  
  # Amber inline warning, shown only when a product's four sheet rows disagree
  # and one shared input therefore cannot represent them.
  conflict_note <- function(d) {
    if (!isTRUE(d$conflict)) return(NULL)
    div(style = "font-size: 11px; color: #b45309; margin-top: -8px;",
        sprintf("Sheet values differ by group (%s). Editing applies one value to all four.",
                d$detail))
  }
  
  # ---- Parameter tab: card rendering ---------------------------------------
  # Depends on input$region ONLY. Depending on the edits would recreate the
  # numericInputs on every keystroke and throw the cursor out of the box; the
  # re-render on country switch is what restores the fields to defaults.
  output$param_tab_ui <- renderUI({
    input$region
    cc <- isolate(country_calibration())
    
    oral_eff_d <- shared_default(ORAL_KEYS, "eff_adherent", fmt = pct_detail_fmt)
    len_eff_d  <- shared_default(LEN_KEYS,  "eff_adherent", fmt = pct_detail_fmt)
    len_dur_d  <- shared_default(LEN_KEYS,  "shot_coverage_years", fmt = mo_detail_fmt)
    
    # Two header tiers. grp_hdr = top-level group (Effectiveness parameters /
    # Unit costs / ART cost adjustments): largest, bordered. sec_hdr = subsection
    # within a group (Oral PrEP, Prevention, ...): sized ABOVE the theme's control
    # labels (17px vs 16px) so it reads as a header, with a smaller top margin now
    # that grp_hdr carries the major separation between groups.
    grp_hdr <- function(txt, top = 14) {                          # was top = 22
      div(style = sprintf(paste0("font-size:20px; font-weight:700; color:#111827; ",
                                 "margin:%dpx 0 3px; padding-bottom:3px; ",       # was 0 6px / pb 4px
                                 "border-bottom:2px solid #cbd5e1;"), top),
          txt)
    }
    sec_hdr <- function(txt) {
      tagList(div(style = "font-weight:700; font-size:17px; margin:5px 0 1px;", txt),
              hr(style = "margin:1px 0 4px;"))
    }
    
    # step is gone with the swap to decimalNumericInput(): autoNumeric has no
    # spinners, so step had nothing to drive. min/max now CLAMP rather than warn.
    dur_input <- function(key) {
      decimalNumericInput(paste0("param_dur_", key),
                          grp_lbl(key, sheet_src(key, "person_years_on_prep")),
                          value = years_to_mo(sheet_val(key, "person_years_on_prep")),
                          min = 0, max = MAX_DURATION_MONTHS, width = "100%")
    }
    # Shown as a percentage (0-100); the sheet stores a raw 0-1 fraction, so
    # frac_to_pct() in and pct_to_frac() out (observer below). step is gone
    # with the swap, as with dur_input(): autoNumeric has no spinners.
    ret_input <- function(key) {
      decimalNumericInput(paste0("param_ret_", key),
                          grp_lbl(key, sheet_src(key, "second_shot_return_rate")),
                          value = frac_to_pct(sheet_val(key, "second_shot_return_rate")),
                          min = 0, max = 100, width = "100%")
    }
    # Tooltip text comes from the sheet's src_unit_cost column -- no default
    # value baked in here. Until that column is populated for a key, the key
    # simply has no info bubble.
    # lblf is the only thing that varies: PrEP boxes carry the FSW/MSM/AGYW/
    # General sub-label, every other cost box is a distinct intervention and
    # carries its own short name. The default resolves through cost_default_of(),
    # so the box shows whichever of the three channels will actually be read.
    cost_input <- function(key, lblf = cost_lbl) {
      currencyNumericInput(paste0("cost_", key),
                           lblf(key, sheet_src(key, "unit_cost")),
                           value = cost_default_of(key, cc)$value,
                           min = 0, width = "100%")
    }
    # One CELL per key -- box, check and source caption in the same column.
    # .dsd-cell is reused deliberately: box-above-message in one column is the
    # same shape as the ART-adjustment cells, and the class only drops the
    # Bootstrap form-group gap.
    cost_cell <- function(key, lblf) {
      div(class = "dsd-cell",
          cost_input(key, lblf),
          uiOutput(paste0("chkcost_", key)),
          uiOutput(paste0("capcost_", key)))
    }
    # col_widths padded to 12 with a NULL child so a 2-box row keeps the same box
    # width as a 4-box row -- same trick as dsd_row() below.
    cost_row_one <- function(keys, lblf) {
      n   <- length(keys)
      pad <- if (n < 4) list(NULL)
      do.call(layout_columns,
              c(list(col_widths = c(rep(3, n), if (n < 4) 12 - 3 * n)),
                unname(c(lapply(keys, cost_cell, lblf = lblf), pad))))
    }
    cost_rows <- function(keys, lblf = cost_lbl) {
      do.call(tagList,
              unname(lapply(split(keys, ceiling(seq_along(keys) / 4)),
                            cost_row_one, lblf = lblf)))
    }
    art_d <- art_cost_default(cc)
    
    # Render-time ART cost. isolate() is load-bearing: param_tab_ui depends on
    # input$region ONLY (see the comment above this renderUI) -- a live
    # dependency here rebuilds every numericInput on each ART keystroke and
    # throws the cursor out of the box.
    # In practice param_overrides$art_cost is always NULL here (cleared on
    # country switch, which is the only thing that re-renders this tab), so this
    # equals art_d$value -- but don't rely on that invariant holding.
    ac_render <- isolate(effective_art_cost())
    
    # min = 0: the box now holds a TOTAL ART cost/PY under the model, which
    # cannot be negative. (Old signed-delta floor was -art_cost; total >= 0 is
    # the same floor expressed as a total.)
    dsd_cost_input <- function(key) {
      currencyNumericInput(paste0("dsdcost_", key),
                           dsd_lbl(key, sheet_src(key, "unit_cost")),
                           value = dsd_cost_default(key, ac_render)$total,
                           min   = 0,
                           width = "100%")
    }
    # One row of DSD boxes with matching check and caption rows beneath -- the
    # same shape as the PrEP cost rows above. Each box previously took a
    # col_widths = c(3, 9) row of its own and wasted three quarters of it.
    # col_widths is padded to 12 with a NULL child so a 2-box row keeps the
    # same box width as a 3-box row.
    # One CELL per key -- box, check and caption in the same column -- rather
    # than three parallel layout_columns rows. Two fewer row gaps, and the
    # caption sits against its own box instead of against the tallest one.
    dsd_cell <- function(key) {
      div(class = "dsd-cell",
          dsd_cost_input(key),
          uiOutput(paste0("chkdsd_", key)),
          uiOutput(paste0("capdsd_", key)))
    }
    dsd_row <- function(keys) {
      n   <- length(keys)
      w   <- c(rep(4, n), if (n < 3) 12 - 4 * n)
      pad <- if (n < 3) list(NULL)
      do.call(layout_columns,
              c(list(col_widths = w), unname(c(lapply(keys, dsd_cell), pad))))
    }
    
    div(
      class = "param-tab",
      style = "max-height: 78vh; overflow-y: auto; padding-right: 15px;",
      
      # ---------------- Effectiveness parameters ----------------
      grp_hdr("Effectiveness parameters", top = 2),
      
      # ---------------- Oral PrEP ----------------
      sec_hdr("Oral PrEP"),
      layout_columns(
        col_widths = c(3, 9),
        decimalNumericInput("param_oral_eff",
                            lbl_tip("Effectiveness (%)",
                                    sheet_src(ORAL_KEYS[1], "eff_adherent")),
                            value = frac_to_pct(oral_eff_d$value),
                            min = 0, max = 100, width = "100%"),
        NULL
      ),
      uiOutput("chk_param_oral_eff"),
      conflict_note(oral_eff_d),
      param_hdr("Average duration on PrEP, by group (months)"),
      do.call(layout_columns, c(list(col_widths = rep(3, 4)), unname(lapply(ORAL_KEYS, dur_input)))),
      do.call(layout_columns, c(list(col_widths = rep(3, 4)),
                                unname(lapply(ORAL_KEYS, function(k) uiOutput(paste0("chk_param_dur_", k)))))),
      
      # ---------------- Lenacapavir ----------------
      sec_hdr("Lenacapavir"),
      layout_columns(
        col_widths = c(3, 3.2, 6),
        decimalNumericInput("param_len_eff",
                            lbl_tip("Effectiveness (%)",
                                    sheet_src(LEN_KEYS[1], "eff_adherent")),
                            value = frac_to_pct(len_eff_d$value),
                            min = 0, max = 100, width = "100%"),
        decimalNumericInput("param_len_dur",
                            lbl_tip("Duration of protection per dose (months)",
                                    sheet_src(LEN_KEYS[1], "shot_coverage_years")),
                            value = years_to_mo(len_dur_d$value),
                            min = 0, max = MAX_DURATION_MONTHS, width = "100%"),
        NULL
      ),
      uiOutput("chk_param_len_eff"),
      uiOutput("chk_param_len_dur"),
      conflict_note(len_eff_d),
      conflict_note(len_dur_d),
      param_hdr("Probability of returning for second injection , by group (%)"),
      do.call(layout_columns, c(list(col_widths = rep(3, 4)), unname(lapply(LEN_KEYS, ret_input)))),
      do.call(layout_columns, c(list(col_widths = rep(3, 4)),
                                unname(lapply(LEN_KEYS, function(k) uiOutput(paste0("chk_param_ret_", k)))))),
      
      # ---------------- Treatment monitoring & quality ----------------
      # Effect assumption (NOT a cost, so it sits with the PrEP effect boxes
      # above, not in the ART-cost region). pp gain in viral suppression among
      # established clients from annual vs 6-monthly visits. Reaches the model as
      # an $eff override on clinical_visit_12month$efficacy -- same channel as the
      # PrEP effectiveness boxes -- so the logic file and test_13 are untouched.
      # Sheet stores a fraction (0.01 = +1pp), box shows pp: frac_to_pct in /
      # pct_to_frac out. Default resolves from the sheet; blank until the
      # efficacy row is populated in intervention_params.
      sec_hdr("Treatment monitoring & quality"),
      layout_columns(
        col_widths = c(6, 6),
        decimalNumericInput(
          "param_cv12_supp_impact",
          lbl_tip("Suppression impact of annual vs 6-monthly clinical visits (%)",
                  sheet_src("clinical_visit_12month", "efficacy")),
          value = frac_to_pct(sheet_val("clinical_visit_12month", "efficacy")),
          min = 0, max = 10, width = "100%"),
        NULL
      ),
      uiOutput("chk_param_cv12_supp_impact"),
      div(style = "font-size:11px; color:#6b7280; margin:-2px 0 5px;", 
          "Percentage-point gain in viral suppression among established ART ",
          "clients (on ART >1 year). Applied only where annual clinical visits ",
          "are switched on in a scenario."),
      
      # ---------------- Unit costs ----------------
      # ---------------- Unit costs ----------------
      # Sectioned by the five cost categories the model reports (cost_by_cat in
      # the logic; prevention_cost / testing_cost / ... in the return), so a user
      # who sees testing dominate the stacked breakdown finds a section with the
      # same name here. The section a key sits in is its intervention_groups
      # group -- NOT its override channel, which is invisible to the user by
      # design and is the tab's business, not theirs.
      grp_hdr("Unit costs (USD)"),
      sec_hdr("Prevention"),
      param_hdr("Oral PrEP, cost of a full 12 months per person"),
      cost_rows(ORAL_KEYS, lblf = grp_lbl),
      param_hdr("Lenacapavir, per person initiating"),
      cost_rows(LEN_KEYS, lblf = grp_lbl),
      param_hdr("Other prevention, per unit delivered"),
      cost_rows(c("vmmc", "condoms", "infant_prophylaxis")),
      
      sec_hdr("Testing & diagnosis"),
      param_hdr("HIV testing services, per test performed"),
      cost_rows(c("test_facility_general", "test_network", "test_index",
                  "test_community", "test_kpsti")),
      param_hdr("HIV self-testing, per kit distributed"),
      cost_rows(c("hivst_facility", "hivst_community")),
      param_hdr("Antenatal, postnatal and infant, per person tested"),
      cost_rows(c("anc_hiv_testing", "pnc_hiv_testing", "eid")),
      
      sec_hdr("Treatment monitoring & quality"),
      layout_columns(
        col_widths = c(3, 9),
        currencyNumericInput("cost_art_standard",
                             lbl_tip("ART, per person-year",
                                     paste("Standard of care cost for providing one year of treatment, excluding viral load testing.")),
                             value = art_d$value, min = 0, width = "100%"),
        NULL
      ),
      uiOutput("chkcost_art"),
      param_hdr("Viral load monitoring, per test performed"),
      cost_rows("vl_monitoring_routine"),
      
      sec_hdr("Retention & adherence support"),
      param_hdr("Per person reached"),
      cost_rows(c("adherence_counseling", "tracking_tracing",
                  "anc_vl_testing", "pnc_vl_testing")),
      
      sec_hdr("Advanced HIV disease"),
      param_hdr("Per person reached"),
      cost_rows(c("cd4_testing", "ahd_package")),
      
      # ---------------- ART cost adjustments ----------------
      grp_hdr("ART cost adjustments"),
      div(style = "font-size:11px; color:#6b7280; margin:-2px 0 5px;",
          "Total ART cost per person-year under each delivery model, charged on ",
          "enrolled stable clients. The difference from the standard ART unit ",
          "cost above is what the model applies. Lower values input here represent cost savings."),
      param_hdr("Multi-month dispensing"),
      dsd_row(MMD_KEYS),
      param_hdr("Other differentiated service delivery"),
      dsd_row(DSD_OTHER_KEYS),
      
      # ---------------- Reset ----------------
      div(style = "margin-top:10px;",
          actionButton("reset_all_params", "Reset all values to defaults",
                       class = "btn-outline-secondary btn-sm"),
          uiOutput("param_reset_status"))
    )
  })
  
  # ---- Parameter tab: validation + ingestion -------------------------------
  # Validation strategy: an out-of-range entry is NOT written to
  # param_overrides. The model stays on the previous value and the need()
  # message says so, rather than the value reaching derive_prep_efficacy() and
  # taking the session down with a red stop().
  chk_range <- function(v, lo, hi, unit = "") {
    validate(
      need(!is.null(v) && !is.na(v),
           "Enter a number — the default is still being used."),
      need(v >= lo && v <= hi,
           sprintf("Must be between %g and %g%s. Not applied — the default is still being used.",
                   lo, hi, unit))
    )
    NULL
  }
  ok_range <- function(v, lo, hi) !(is.null(v) || is.na(v) || v < lo || v > hi)
  
  output$chk_param_oral_eff <- renderUI({ chk_range(input$param_oral_eff, 0, 100, "%") })
  output$chk_param_len_eff  <- renderUI({ chk_range(input$param_len_eff,  0, 100, "%") })
  output$chk_param_cv12_supp_impact <- renderUI({ chk_range(input$param_cv12_supp_impact, 0, 10, " pp") })
  output$chk_param_len_dur  <- renderUI({
    chk_range(input$param_len_dur, 0, MAX_DURATION_MONTHS, " months")
  })
  
  # Shared efficacy inputs fan out to their product's four group keys.
  observeEvent(input$param_oral_eff, {
    v <- input$param_oral_eff
    if (!ok_range(v, 0, 100)) return()
    for (key in ORAL_KEYS) set_eff_override(key, "eff_adherent", pct_to_frac(v))
  }, ignoreInit = TRUE)
  
  observeEvent(input$param_len_eff, {
    v <- input$param_len_eff
    if (!ok_range(v, 0, 100)) return()
    for (key in LEN_KEYS) set_eff_override(key, "eff_adherent", pct_to_frac(v))
  }, ignoreInit = TRUE)
  
  observeEvent(input$param_len_dur, {
    v <- input$param_len_dur
    if (!ok_range(v, 0, MAX_DURATION_MONTHS)) return()
    for (key in LEN_KEYS) set_eff_override(key, "shot_coverage_years", mo_to_years(v))
  }, ignoreInit = TRUE)
  
  observeEvent(input$param_cv12_supp_impact, {
    v <- input$param_cv12_supp_impact
    if (!ok_range(v, 0, 10)) return()
    set_eff_override("clinical_visit_12month", "efficacy", pct_to_frac(v))
  }, ignoreInit = TRUE)
  
  # Per-group inputs + per-group derived readouts. local() so each closure
  # captures THIS key, not the loop's last value.
  for (.key in PREP_KEYS) {
    local({
      key   <- .key
      is_len <- key %in% LEN_KEYS
      
      if (is_len) {
        output[[paste0("chk_param_ret_", key)]] <- renderUI({
          chk_range(input[[paste0("param_ret_", key)]], 0, 100, "%")
        })
        observeEvent(input[[paste0("param_ret_", key)]], {
          v <- input[[paste0("param_ret_", key)]]
          if (!ok_range(v, 0, 100)) return()
          set_eff_override(key, "second_shot_return_rate", pct_to_frac(v))
        }, ignoreInit = TRUE)
      } else {
        output[[paste0("chk_param_dur_", key)]] <- renderUI({
          chk_range(input[[paste0("param_dur_", key)]], 0, MAX_DURATION_MONTHS, " months")
        })
        observeEvent(input[[paste0("param_dur_", key)]], {
          v <- input[[paste0("param_dur_", key)]]
          if (!ok_range(v, 0, MAX_DURATION_MONTHS)) return()
          set_eff_override(key, "person_years_on_prep", mo_to_years(v))
        }, ignoreInit = TRUE)
      }
    })
  }
  
  # ---- Unit costs: one loop, every cost key --------------------------------
  # Replaces the per-key cost block that used to live inside the PREP_KEYS loop
  # above. Same contract as before, now for all 28 cost boxes: an entry is
  # DROPPED when the value returns to its default, so n_overrides() -- and the
  # report's changed-parameter count -- never claims a change on an untouched box.
  # cost_store_of() / cost_default_of() do the routing; nothing in this loop knows
  # or cares which of the three channels a key uses.
  # local() so each closure captures THIS key, not the loop's last value.
  for (.key in ALL_COST_KEYS) {
    local({
      key   <- .key
      store <- cost_store_of(key)
      
      output[[paste0("chkcost_", key)]] <- renderUI({
        v <- input[[paste0("cost_", key)]]
        validate(
          need(!is.null(v) && !is.na(v),
               "Enter a number — the default is still being used."),
          need(v >= 0, "A unit cost cannot be negative. Not applied.")
        )
        NULL
      })
      
      # Depends on country_calibration() only, not on param_overrides: whether a
      # key HAS a default is a property of the sheet and the CSV, not of what the
      # user typed. Renders nothing in the normal case.
      output[[paste0("capcost_", key)]] <- renderUI({
        missing_default_note(cost_default_of(key, country_calibration()))
      })
      
      observeEvent(input[[paste0("cost_", key)]], {
        v <- input[[paste0("cost_", key)]]
        if (is.null(v) || is.na(v) || v < 0) return()
        d   <- cost_default_of(key, country_calibration())
        cst <- param_overrides[[store]]
        # NA default (no sheet row, no CSV column) -> nothing to compare against,
        # so every entry counts as a change. Correct: the box was blank.
        if (length(d$value) == 1 && !is.na(d$value) && isTRUE(all.equal(v, d$value))) {
          cst[[key]] <- NULL
        } else {
          cst[[key]] <- v
        }
        param_overrides[[store]] <- cst
      }, ignoreInit = TRUE)
    })
  }
  
  # ---- ART cost adjustments (DSD + annual clinical visit) -------------------
  # The CAPTION, not the box, is the authority on what the model charges: it
  # resolves the fraction exactly as effective_intervention_params() does, so
  # the two cannot disagree.
  # Nothing here writes to an input. That is deliberate -- auto-updating these
  # boxes on an ART cost change would reintroduce the late-echo-read-as-user-
  # edit failure (scen_seed / scen_touched, Edits 8-15 of the reactive session).
  # local() so each closure captures THIS key, not the loop's last value.
  for (.key in DSD_COST_KEYS) {
    local({
      key   <- .key
      # margin-top was -6px when the caption lived in its own layout_columns row
      # and had to claw back that row's gap. Inside .dsd-cell it sits directly
      # under the box, so a negative margin would ride up over the input.
      amber <- "font-size:11px; color:#b45309; margin-top:2px;"
      grey  <- "font-size:11px; color:#6b7280; margin-top:2px;"
      
      output[[paste0("capdsd_", key)]] <- renderUI({
        ac <- effective_art_cost()
        if (!ac_ok(ac)) {
          return(div(style = amber,
                     "ART unit cost is zero or unset — no adjustment can be applied."))
        }
        # NOT a direct param_overrides$dsd_abs lookup by key: dsd_abs is
        # list() until the first entry, and a double-bracket character lookup
        # on a list with no matching name throws "subscript out of bounds"
        # rather than returning NULL. This renderUI fires the moment the tab is
        # first shown, with dsd_abs still empty, so the unguarded form errors
        # on every caption.
        dsd <- param_overrides$dsd_abs
        ovr <- if (key %in% names(dsd)) dsd[[key]] else NULL
        if (!is.null(ovr)) {
          # ovr is the TOTAL ART cost/PY under the model; the model applies the
          # delta over standard. min = 0 keeps ovr >= 0, so the per-key floor is
          # unreachable (the aggregate floor is handled in the logic file).
          delta <- ovr - ac
          div(style = grey, sprintf(
            "Total %s/PY vs standard %s/PY → %s/PY applied to enrolled clients (%s).",
            usd_fmt(ovr), usd_fmt(ac), usd_fmt(delta), pct_fmt(delta / ac)))
        } else {
          d <- dsd_cost_default(key, ac)
          if (is.na(d$frac)) {
            div(style = amber, "No sheet value — no adjustment applied.")
          } else {
            div(style = grey, sprintf(
              "Sheet default: total %s/PY (%s of standard %s/PY). Type a total to fix it in dollars.",
              usd_fmt(d$total), pct_fmt(d$frac), usd_fmt(ac)))
          }
        }
      })
      
      output[[paste0("chkdsd_", key)]] <- renderUI({
        v  <- input[[paste0("dsdcost_", key)]]
        ac <- effective_art_cost()
        validate(
          need(!is.null(v) && !is.na(v),
               "Enter a number — the sheet default is still being used."),
          need(!ac_ok(ac) || v >= 0,
               "A total ART cost cannot be negative. Not applied.")
        )
        NULL
      })
      
      observeEvent(input[[paste0("dsdcost_", key)]], {
        v  <- input[[paste0("dsdcost_", key)]]
        # observeEvent's handler is isolated: reading effective_art_cost() here
        # creates no dependency, so a later ART edit does NOT re-fire this and
        # overwrite the stored total That is the point -- the total is
        # what the user typed, and only the user retypes it.
        ac <- effective_art_cost()
        if (is.null(v) || is.na(v))  return()
        if (!ac_ok(ac) || v < 0)     return()
        d <- param_overrides$dsd_abs
        d[[key]] <- unname(as.numeric(v))
        param_overrides$dsd_abs <- d
      }, ignoreInit = TRUE)
    })
  }
  
  output$chkcost_art <- renderUI({
    v <- input$cost_art_standard
    validate(
      need(!is.null(v) && !is.na(v),
           "Enter a number — the default is still being used."),
      need(v >= 0, "A unit cost cannot be negative. Not applied.")
    )
    NULL
  })
  observeEvent(input$cost_art_standard, {
    v <- input$cost_art_standard
    if (is.null(v) || is.na(v) || v < 0) return()
    d <- art_cost_default(country_calibration())
    param_overrides$art_cost <-
      if (length(d$value) == 1 && !is.na(d$value) && isTRUE(all.equal(v, d$value))) NULL else v
  }, ignoreInit = TRUE)
  
  # ---- Single reset --------------------------------------------------------
  observeEvent(input$reset_all_params, {
    n <- isolate(n_overrides())
    param_overrides$eff       <- list()
    param_overrides$cost      <- list()
    param_overrides$cost_test <- list()
    param_overrides$cost_flat <- list()
    param_overrides$dsd_abs   <- list()
    param_overrides$art_cost  <- NULL
    
    cc <- country_calibration()
    # param_oral_eff / param_len_eff / param_len_dur / param_dur_* / param_ret_*
    # are autonumericInput now (decimalNumericInput), but updateNumericInput() still
    # drives them -- see the note above commaNumericInput(). frac_to_pct() /
    # years_to_mo() here because the box is in percent / months and the sheet is
    # in fractions / person-years.
    updateNumericInput(session, "param_oral_eff",
                       value = frac_to_pct(shared_default(ORAL_KEYS, "eff_adherent")$value))
    updateNumericInput(session, "param_len_eff",
                       value = frac_to_pct(shared_default(LEN_KEYS, "eff_adherent")$value))
    updateNumericInput(session, "param_len_dur",
                       value = years_to_mo(shared_default(LEN_KEYS, "shot_coverage_years")$value))
    updateNumericInput(session, "param_cv12_supp_impact",
                       value = frac_to_pct(sheet_val("clinical_visit_12month", "efficacy")))
    for (key in ORAL_KEYS) {
      updateNumericInput(session, paste0("param_dur_", key),
                         value = years_to_mo(sheet_val(key, "person_years_on_prep")))
    }
    for (key in LEN_KEYS) {
      updateNumericInput(session, paste0("param_ret_", key),
                         value = frac_to_pct(sheet_val(key, "second_shot_return_rate")))
    }
    # These three are autonumericInput now (currencyNumericInput), but
    # updateNumericInput() still drives them -- see the note above
    # commaNumericInput(): Shiny routes update messages by DOM id, and
    # autonumericInputBinding.receiveMessage() honours {value: ...} whichever
    # binding sent it. Staying on updateNumericInput keeps this consistent with
    # the ~30 other call sites that already update commaNumericInput fields.
    # ALL_COST_KEYS, not PREP_KEYS. NA guard as per the DSD loop below: a key
    # with no sheet row and no CSV column resolves to NA, and pushing NA into the
    # field would blank it rather than restore a default -- and the field was
    # already blank, so there is nothing to restore.
    for (key in ALL_COST_KEYS) {
      v <- cost_default_of(key, cc)$value
      if (length(v) == 1 && !is.na(v)) {
        updateNumericInput(session, paste0("cost_", key), value = v)
      }
    }
    updateNumericInput(session, "cost_art_standard", value = art_cost_default(cc)$value)
    # art_cost_default(cc) IS effective_art_cost() at this point -- the override
    # was set to NULL a few lines up. Reusing it avoids a second copy of the
    # resolution chain.
    ac_reset <- art_cost_default(cc)$value
    for (key in DSD_COST_KEYS) {
      # NA guard: a DSD key with no sheet unit_cost yields NA_real_, and pushing
      # NA into the field would blank it rather than restore a default.
      v <- dsd_cost_default(key, ac_reset)$total
      if (length(v) == 1 && !is.na(v)) {
        updateNumericInput(session, paste0("dsdcost_", key), value = v)
      }
    }
    
    showNotification(
      if (n > 0) "All parameters reset to defaults." else "Already at defaults.",
      type = "message", duration = 4)
  })
  
  output$param_reset_status <- renderUI({
    n <- n_overrides()
    div(style = "font-size:12px; color:#6b7280; margin-top:6px;",
        if (n == 0) "No parameters have been changed from their defaults."
        else sprintf("%d parameter value%s changed from default%s.",
                     n, if (n == 1) "" else "s", if (n == 1) "" else "s"))
  })
  
  # Load regional preset when selected
  observeEvent(input$region, {
    preset <- regional_presets[[input$region]]
    # freeze: original_population() is set below immediately, but input$total_pop
    # only echoes back from the client a flush later. Without this, everything
    # downstream renders once with scale_factor = old_total_pop / new_country_pop
    # (5,000,000 / 2,562,122 = 1.95 on a fresh session) and then corrects. That
    # transient double-render is what was spuriously "touching" scenario fields.
    freezeReactiveValue(input, "total_pop")
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
      prop_fsw          = preset$context$prop_fsw,
      rr_fsw            = preset$context$rr_fsw,
      prop_msm          = preset$context$prop_msm,
      rr_msm            = preset$context$rr_msm,
      prop_agyw         = preset$context$prop_agyw,
      rr_agyw           = preset$context$rr_agyw,
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
      cost_overrides_prep = preset$context$cost_overrides_prep,  # named list of country-specific PrEP unit cost overrides (8 keys); absent/NULL -> global defaults
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
      prop_fsw          = if (!is.null(cc)) cc$prop_fsw          else NULL,
      rr_fsw            = if (!is.null(cc)) cc$rr_fsw            else NULL,
      prop_msm          = if (!is.null(cc)) cc$prop_msm          else NULL,
      rr_msm            = if (!is.null(cc)) cc$rr_msm            else NULL,
      prop_agyw         = if (!is.null(cc)) cc$prop_agyw         else NULL,
      rr_agyw           = if (!is.null(cc)) cc$rr_agyw           else NULL,
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
      art_cost_standard = param_overrides$art_cost %||%
        (if (!is.null(cc)) cc$art_cost_standard else NULL),
      # Country-specific test unit cost overrides (named list, keyed by
      # intervention_key). NULL when no preset selected; in that case the
      # logic file's `context$cost_overrides_test[[int_key]] %||% intervention$unit_cost`
      # lookup safely returns NULL and falls back to the global value.
      # Parameter tab: user test-cost edits layer ON TOP of the country CSV,
      # exactly as cost_overrides_prep does below and for exactly the same reason.
      # The logic reads `cost_overrides_test[[int_key]] %||% intervention$unit_cost`
      # (~2389 / ~2395), so a value injected into unit_cost would be shadowed by
      # the CSV -- the box would move and the result wouldn't.
      cost_overrides_test = modifyList(
        (if (!is.null(cc)) cc$cost_overrides_test else NULL) %||% list(),
        param_overrides$cost_test
      ),
      # Country-specific PrEP unit cost overrides (named list, 8 PrEP keys).
      # NULL when no preset selected; logic file's
      # `context$cost_overrides_prep[[int_key]] %||% intervention$unit_cost`
      # then falls back to the global value.
      # Parameter tab: user cost edits layer ON TOP of the country CSV, not
      # inside intervention_groups. The cost loop reads
      # `cost_overrides_prep[[k]] %||% intervention$unit_cost`, so a value
      # injected into unit_cost would be shadowed by the CSV and do nothing --
      # the box would move and the result wouldn't.
      cost_overrides_prep = modifyList(
        (if (!is.null(cc)) cc$cost_overrides_prep else NULL) %||% list(),
        param_overrides$cost
      ),
      # Country-specific breastfeeding duration in months. NULL when no preset
      # selected or when the CSV column is missing/blank -- logic file's
      # `%||% hiv_params$bf_duration_months %||% 18` chain handles the fallback.
      bf_duration_months = if (!is.null(cc)) cc$bf_duration_months else NULL,
      # Parameter tab: session efficacy overrides reach the model here.
      # calculate_scenario_outcomes() reads
      # `context$intervention_groups %||% intervention_groups`, so NULL is safe
      # and the tests (which never set this) keep using the global.
      # NOTE: not yet a COMPLETE override -- define_strata_params() still reads
      # the global directly for vmmc_risk_reduction. Fine at current scope
      # (PrEP only); must be closed before VMMC efficacy is ever exposed.
      intervention_groups = effective_intervention_groups()
    )
  })
  
  # Calculate populations
  populations <- reactive({
    calculate_populations(context())
  })
  
  # Real per-group population sizes (FSW/MSM/AGYW), computed via the same
  # define_strata_params()/partition_into_strata() path the logic file's
  # cost loop and compute_prevention_adjustments() use. Replaces the old
  # approx_group_pop() UI-side approximation so the input caps shown to
  # users match the actual cost/coverage denominator exactly.
  strata_sizes <- reactive({
    strata_params <- define_strata_params(context())
    partition_into_strata(populations(), strata_params)
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
  
  # NOTE: the previous "all-or-nothing" snapping observer for
  # baseline_clinical_visit_12month has been removed. That input is now a
  # two-option radioGroupButtons (6 vs 12 months), so an intermediate value
  # can no longer be entered and the snap/notify guard is redundant.
  
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
  
  # NOTE: scenario1_clinical_visit_12month snapping observer removed — now a
  # two-option radioGroupButtons, so intermediate values are impossible.
  
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
  
  # NOTE: scenario2_clinical_visit_12month snapping observer removed — now a
  # two-option radioGroupButtons, so intermediate values are impossible.
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
  
  # ---- Baseline form SHAPE -------------------------------------------------
  # The form is built once per SHAPE, not once per value change. "Shape" means
  # WHICH boxes exist, which depends only on the PrEP entry mode, plus a one-off
  # flag for whether any country has loaded yet. Values are pushed into the
  # existing boxes by observeEvent(baseline_values()) further down.
  #
  # WHY: baseline_ui previously depended on baseline_values(), so every country
  # switch and every total_pop edit tore down and rebuilt all ~28 boxes and
  # their tooltips and shipped that HTML over the websocket. Measured from
  # Johannesburg to the Helsinki host (216 ms round trip) a country switch cost
  # ~9.7 s online vs ~2.1 s locally; the rebuild/echo loop was the bulk of it.
  #
  # The identical() guard is EXPLICIT rather than relying on reactiveVal's
  # de-duplication, so an unchanged shape cannot re-fire the rebuild.
  baseline_ui_shape <- reactiveVal(NULL)
  observe({
    shape <- paste0(input$prep_entry_mode %||% PREP_ENTRY_MODE_DEFAULT, "|",
                    if (length(baseline_values()) > 0) "ready" else "empty")
    if (!identical(isolate(baseline_ui_shape()), shape)) baseline_ui_shape(shape)
  })
  
  # Generate baseline UI
  output$baseline_ui <- renderUI({
    # ONLY reactive dependency is the shape. Values are read under isolate().
    req(baseline_ui_shape())
    baseline <- isolate(baseline_values())
    if (length(baseline) == 0) return(NULL)
    
    # PrEP entry mode drives whether the per-group PrEP fields or a single
    # oral/lenacapavir total pair are shown (see prevention block below).
    # isolate(): the mode already reaches this renderUI through baseline_ui_shape;
    # reading it live as well would restore a second dependency for no gain.
    prep_mode <- isolate(input$prep_entry_mode) %||% PREP_ENTRY_MODE_DEFAULT
    prep_group_keys <- c("prep_oral_fsw", "prep_oral_msm", "prep_oral_agyw", "prep_oral_general",
                         "prep_lenacapavir_fsw", "prep_lenacapavir_msm", "prep_lenacapavir_agyw", "prep_lenacapavir_general")
    
    ui_elements <- lapply(names(intervention_groups), function(group_key) {
      group <- intervention_groups[[group_key]]
      
      interventions_ui <- lapply(names(group$interventions), function(int_key) {
        intervention <- group$interventions[[int_key]]
        # In "total" mode the per-group PrEP fields are hidden; the oral +
        # lenacapavir totals are rendered in the prevention block instead.
        if (identical(prep_mode, "total") && int_key %in% prep_group_keys) return(NULL)
        value <- ifelse(is.null(baseline[[int_key]]), 0, baseline[[int_key]])
        
        # Set min and max based on type
        min_val <- 0
        max_val <- if(intervention$type == "coverage") 100 else NA
        
        # Build a label that includes a small info icon with a tooltip.
        label_with_tip <- tagList(
          paste0(intervention$name, " (", intervention$unit_label, ")"),
          make_intervention_tip(int_key)
        )
        
        # clinical_visit_12month is a binary 6-vs-12-month choice (all-or-
        # nothing per country). Render it as a two-option button group instead
        # of a numeric field. choiceValues 0/100 keep the same numeric contract
        # the logic expects; radioGroupButtons returns them as CHARACTER, so the
        # collection reactives coerce with as.numeric() (see baseline_input_values).
        if (int_key == "clinical_visit_12month") {
          shinyWidgets::radioGroupButtons(
            inputId      = paste0("baseline_", int_key),
            label        = label_with_tip,
            choiceNames  = c("Every 6 months", "Every 12 months"),
            choiceValues = c(0, 100),
            selected     = if (!is.null(value) && !is.na(value) && value >= 50) 100 else 0,
            justified    = TRUE,
            size         = "sm",
            checkIcon    = list(yes = icon("check"))
          )
        } else {
          numeric_widget <- if (intervention$type == "coverage") numericInput else commaNumericInput
          numeric_widget(
            paste0("baseline_", int_key),
            label = label_with_tip,
            value = value,
            min = min_val,
            max = max_val,
            step = if(intervention$type == "coverage") 0.1 else 1
          )
        }
      })
      
      if (group_key == "prevention") {
        if (identical(prep_mode, "total")) {
          interventions_ui <- c(list(
            div(
              style = "margin-top: 10px; padding: 8px; background:#f3f7fb; border-radius:5px;",
              commaNumericInput("baseline_prep_total_oral",
                                label = "Total oral PrEP (all groups):",
                                value = ifelse(is.null(baseline[["prep_oral_fsw"]]), 0,
                                               (baseline[["prep_oral_fsw"]] %||% 0) + (baseline[["prep_oral_msm"]] %||% 0) +
                                                 (baseline[["prep_oral_agyw"]] %||% 0) + (baseline[["prep_oral_general"]] %||% 0)),
                                min = 0),
              commaNumericInput("baseline_prep_total_lena",
                                label = "Total lenacapavir (all groups):",
                                value = ifelse(is.null(baseline[["prep_lenacapavir_fsw"]]), 0,
                                               (baseline[["prep_lenacapavir_fsw"]] %||% 0) + (baseline[["prep_lenacapavir_msm"]] %||% 0) +
                                                 (baseline[["prep_lenacapavir_agyw"]] %||% 0) + (baseline[["prep_lenacapavir_general"]] %||% 0)),
                                min = 0),
              tags$small(style = "color:#666;", "Enter the national totals delivered. Capped at the estimated PrEP-eligible population. Switch to \"By group\" if you want to set the allocation across FSW/MSM/AGYW/General yourself.")
            )
          ), interventions_ui)
        }
        # interventions_ui <- c(interventions_ui, list(
        #   div(
        #     style = "margin-top: 15px; padding: 10px; background-color: #f8f9fa; border-radius: 5px;",
        #     h6("Total PrEP Coverage:", style = "margin-bottom: 5px;"),
        #     uiOutput("prep_total_baseline")
        #   )
        # ))
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
    
    prep_mode <- input$prep_entry_mode %||% PREP_ENTRY_MODE_DEFAULT
    prep_group_keys <- c("prep_oral_fsw", "prep_oral_msm", "prep_oral_agyw", "prep_oral_general",
                         "prep_lenacapavir_fsw", "prep_lenacapavir_msm", "prep_lenacapavir_agyw", "prep_lenacapavir_general")
    
    scenario_columns <- lapply(names(intervention_groups), function(group_key) {
      group <- intervention_groups[[group_key]]
      
      interventions_ui <- lapply(names(group$interventions), function(int_key) {
        intervention <- group$interventions[[int_key]]
        if (identical(prep_mode, "total") && int_key %in% prep_group_keys) return(NULL)
        base_value <- ifelse(is.null(baseline[[int_key]]), 0, baseline[[int_key]])
        s1_value <- scen_seed_value(paste0("scenario1_", int_key), base_value)
        s2_value <- scen_seed_value(paste0("scenario2_", int_key), base_value)
        
        # Set min and max based on type
        min_val <- 0
        max_val <- if(intervention$type == "coverage") 100 else NA
        numeric_widget <- if (intervention$type == "coverage") numericInput else commaNumericInput
        
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
            if (int_key == "clinical_visit_12month") {
              shinyWidgets::radioGroupButtons(
                inputId      = paste0("scenario1_", int_key),
                label        = "Scenario 1",
                choiceNames  = c("6 months", "12 months"),
                choiceValues = c(0, 100),
                selected     = if (s1_value >= 50) 100 else 0,
                justified    = TRUE,
                size         = "sm",
                checkIcon    = list(yes = icon("check"))
              )
            } else {
              numeric_widget(
                paste0("scenario1_", int_key),
                label = "Scenario 1",
                value = s1_value,
                min = min_val,
                max = max_val,
                step = if(intervention$type == "coverage") 0.1 else 1
              )
            }
          ),
          div(
            if (int_key == "clinical_visit_12month") {
              shinyWidgets::radioGroupButtons(
                inputId      = paste0("scenario2_", int_key),
                label        = "Scenario 2",
                choiceNames  = c("6 months", "12 months"),
                choiceValues = c(0, 100),
                selected     = if (s2_value >= 50) 100 else 0,
                justified    = TRUE,
                size         = "sm",
                checkIcon    = list(yes = icon("check"))
              )
            } else {
              numeric_widget(
                paste0("scenario2_", int_key),
                label = "Scenario 2",
                value = s2_value,
                min = min_val,
                max = max_val,
                step = if(intervention$type == "coverage") 0.1 else 1
              )
            }
          )
        )
      })
      
      if (group_key == "prevention") {
        if (identical(prep_mode, "total")) {
          base_oral_total <- (baseline[["prep_oral_fsw"]] %||% 0) + (baseline[["prep_oral_msm"]] %||% 0) +
            (baseline[["prep_oral_agyw"]] %||% 0) + (baseline[["prep_oral_general"]] %||% 0)
          base_lena_total <- (baseline[["prep_lenacapavir_fsw"]] %||% 0) + (baseline[["prep_lenacapavir_msm"]] %||% 0) +
            (baseline[["prep_lenacapavir_agyw"]] %||% 0) + (baseline[["prep_lenacapavir_general"]] %||% 0)
          total_rows <- list(
            layout_columns(
              col_widths = c(4, 4, 4),
              div(style = "padding-top: 25px; font-size: 0.9em;",
                  strong("Total oral PrEP"), br(),
                  span(style = "color: #666;", "Baseline: ", format(round(base_oral_total), big.mark = ",")), br(),
                  span(style = "color: #999; font-size: 0.85em;", "people (all groups)")),
              div(commaNumericInput("scenario1_prep_total_oral", label = "Scenario 1",
                                    value = scen_seed_value("scenario1_prep_total_oral", base_oral_total), min = 0)),
              div(commaNumericInput("scenario2_prep_total_oral", label = "Scenario 2",
                                    value = scen_seed_value("scenario2_prep_total_oral", base_oral_total), min = 0))
            ),
            layout_columns(
              col_widths = c(4, 4, 4),
              div(style = "padding-top: 25px; font-size: 0.9em;",
                  strong("Total lenacapavir"), br(),
                  span(style = "color: #666;", "Baseline: ", format(round(base_lena_total), big.mark = ",")), br(),
                  span(style = "color: #999; font-size: 0.85em;", "people (all groups)")),
              div(commaNumericInput("scenario1_prep_total_lena", label = "Scenario 1",
                                    value = scen_seed_value("scenario1_prep_total_lena", base_lena_total), min = 0)),
              div(commaNumericInput("scenario2_prep_total_lena", label = "Scenario 2",
                                    value = scen_seed_value("scenario2_prep_total_lena", base_lena_total), min = 0))
            )
          )
          interventions_ui <- c(total_rows, interventions_ui)
        }
        # interventions_ui <- c(interventions_ui, list(
        #   layout_columns(
        #     col_widths = c(4, 4, 4),
        #     div(),
        #     div(
        #       h6("Total PrEP:", style = "margin-top: 15px;"),
        #       uiOutput("prep_total_scenario1")
        #     ),
        #     div(
        #       h6("Total PrEP:", style = "margin-top: 15px;"),
        #       uiOutput("prep_total_scenario2")
        #     )
        #   )
        # ))
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
  
  # Both tabs are renderUI inside nav_panels, so Shiny suspends them until the
  # tab is first shown. That is what makes Results stale:
  #   - baseline_ui suspended => input$baseline_* is NULL on a fresh session,
  #     so anyone who opens Results or Scenarios first is served fallbacks.
  #   - scenario_ui suspended => a baseline edit invalidates it but does not
  #     re-run it, so input$scenario1_*/scenario2_* keep the PRE-EDIT values
  #     and the scenario bars on Results do not move until Scenarios is visited.
  # Rendering eagerly makes the existing design behave as written.
  outputOptions(output, "baseline_ui", suspendWhenHidden = FALSE)
  outputOptions(output, "scenario_ui", suspendWhenHidden = FALSE)
  # ========================================================================
  # PREP AND MMD TOTAL INDICATORS
  # ========================================================================
  
  # Builds the per-group PrEP coverage summary (FSW/MSM/AGYW/General) for one
  # scenario. Population sizes come from group_pop() -- the same UI-side
  # approximation used for validation caps, not the authoritative FOI
  # denominator. Replaces the old single blended oral+lenacapavir/adult_pop
  # total, which no longer makes sense once PrEP is targeted per group.
  render_prep_group_summary <- function(scenario_prefix) {
    ctx <- context()
    if (is.null(ctx$total_population) || is.null(ctx$prop_pop_under_14)) return(NULL)
    
    prep_mode <- input$prep_entry_mode %||% PREP_ENTRY_MODE_DEFAULT
    
    # ---- TOTAL MODE: echo back the entered totals, NOT the internal split ----
    # The split into FSW/MSM/AGYW/General still happens (allocate_prep_totals()
    # feeds the FOI, and because rr_fsw >> 1 the allocation materially drives
    # infections averted) -- but a user who entered a national total has NOT
    # asserted an allocation, and showing them one here reads as if they had.
    # "By group" mode is where the allocation is visible and editable.
    # The DISCARD note is the one thing that must survive: it is the only case
    # where what is modelled differs from what the user typed.
    # The overflow note is deliberately dropped in this mode -- "reallocated to
    # General" is meaningless to someone who has not been shown the groups.
    if (identical(prep_mode, "total")) {
      to <- suppressWarnings(as.numeric(input[[paste0(scenario_prefix, "_prep_total_oral")]] %||% 0))
      tl <- suppressWarnings(as.numeric(input[[paste0(scenario_prefix, "_prep_total_lena")]] %||% 0))
      if (is.na(to)) to <- 0
      if (is.na(tl)) tl <- 0
      sp   <- split_prep_total(to, tl)
      comb <- to + tl
      # Denominator = combined HIV-negative population across all PrEP groups,
      # i.e. the same quantity prep_total_cap() clamps the inputs against.
      cap  <- prep_total_cap()
      pct  <- if (is.finite(cap) && cap > 0) (comb / cap) * 100 else NA_real_
      
      note <- NULL
      if ((sp$discarded %||% 0) > 0.5) {
        note <- tags$div(
          style = "font-size:0.8em; margin-top:3px;",
          paste0(format(round(sp$discarded), big.mark = ","),
                 " of the entered total cannot be delivered — the PrEP-eligible population is saturated."))
      }
      
      return(tagList(
        tags$div(style = "font-size: 0.85em; margin-bottom: 2px;",
                 paste0("Oral PrEP: ", format(round(to), big.mark = ","))),
        tags$div(style = "font-size: 0.85em; margin-bottom: 2px;",
                 paste0("Lenacapavir: ", format(round(tl), big.mark = ","))),
        tags$div(style = "font-size: 0.85em; margin-bottom: 2px; font-weight: bold;",
                 paste0("Combined: ", format(round(comb), big.mark = ","),
                        if (is.na(pct)) "" else
                          paste0(" (", round(pct, 1),
                                 "% of the estimated PrEP-eligible population)"))),
        note
      ))
    }
    
    # ---- BY-GROUP MODE: the user entered the allocation, so show it back ----
    groups <- list(fsw = "FSW", msm = "MSM", agyw = "AGYW", general = "General")
    rows <- lapply(names(groups), function(group) {
      oral_val <- input[[paste0(scenario_prefix, "_prep_oral_", group)]] %||% 0
      lena_val <- input[[paste0(scenario_prefix, "_prep_lenacapavir_", group)]] %||% 0
      n_group  <- group_pop(group)
      if (is.na(n_group) || n_group <= 0) return(NULL)
      
      total <- oral_val + lena_val
      pct   <- (total / n_group) * 100
      
      tags$div(
        style = "font-size: 0.85em; margin-bottom: 2px;",
        paste0(groups[[group]], ": ", format(round(total), big.mark = ","),
               " (", round(pct, 1), "% of approx. ", groups[[group]], " pop.)")
      )
    })
    tagList(rows)
  }
  
  output$prep_total_baseline  <- renderUI({ render_prep_group_summary("baseline") })
  output$prep_total_scenario1 <- renderUI({ render_prep_group_summary("scenario1") })
  output$prep_total_scenario2 <- renderUI({ render_prep_group_summary("scenario2") })
  
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
  # Preset PrEP product totals. The fallback for every total-mode read taken
  # before the owning tab has rendered -- mirrors the `preset[[int_key]]`
  # fallback the per-intervention loops already use.
  preset_prep_totals <- reactive({
    p <- baseline_values()
    list(
      oral = (p[["prep_oral_fsw"]]  %||% 0) + (p[["prep_oral_msm"]]  %||% 0) +
        (p[["prep_oral_agyw"]] %||% 0) + (p[["prep_oral_general"]] %||% 0),
      lena = (p[["prep_lenacapavir_fsw"]]  %||% 0) + (p[["prep_lenacapavir_msm"]]  %||% 0) +
        (p[["prep_lenacapavir_agyw"]] %||% 0) + (p[["prep_lenacapavir_general"]] %||% 0)
    )
  })
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
        value <- ifelse(is.null(value) || is.na(value), 0, value)
        # radioGroupButtons (clinical_visit_12month) returns character "0"/"100";
        # coerce so downstream arithmetic (eligible * value/100) works.
        if (int_key == "clinical_visit_12month") value <- as.numeric(value)
        baseline[[int_key]] <- value
      }
    }
    # In "total" entry mode the per-group PrEP inputs aren't rendered; derive
    # the 8 group buckets from the entered oral/lenacapavir totals here so the
    # rest of the pipeline (and the logic file) is unchanged.
    if (identical(input$prep_entry_mode %||% PREP_ENTRY_MODE_DEFAULT, "total")) {
      pt <- preset_prep_totals()
      sp <- split_prep_total(coalesce_num(input[["baseline_prep_total_oral"]], pt$oral),
                             coalesce_num(input[["baseline_prep_total_lena"]], pt$lena))
      for (k in c("prep_oral_fsw", "prep_oral_msm", "prep_oral_agyw", "prep_oral_general",
                  "prep_lenacapavir_fsw", "prep_lenacapavir_msm", "prep_lenacapavir_agyw", "prep_lenacapavir_general"))
        baseline[[k]] <- sp[[k]]
    }
    baseline
  })
  
  observeEvent(baseline_values(), {
    baseline <- baseline_values()
    if (length(baseline) == 0) return()
    
    for (group_key in names(intervention_groups)) {
      group <- intervention_groups[[group_key]]
      for (int_key in names(group$interventions)) {
        baseline_value <- unname(baseline[[int_key]])
        if (is.null(baseline_value) || is.na(baseline_value)) next
        
        updateNumericInput(session, paste0("baseline_", int_key), value = baseline_value)
        # clinical_visit_12month is a radioGroupButtons -- updateNumericInput
        # won't drive it, so sync it explicitly.
        if (int_key == "clinical_visit_12month") {
          shinyWidgets::updateRadioGroupButtons(session, paste0("baseline_", int_key),
                                                selected = if (baseline_value >= 50) 100 else 0)
        }
        # Scenario fields follow baseline only while untouched. scen_push()
        # records the write so the echo isn't read as a user edit.
        for (pfx in c("scenario1_", "scenario2_")) {
          id <- paste0(pfx, int_key)
          if (!is_touched(id)) scen_push(id, baseline_value)
        }
      }
    }
    
    # Total-mode PrEP boxes are NOT intervention keys, so the loop above never
    # reaches them. While baseline_ui rebuilt on every value change this was
    # invisible -- the rebuild recomputed them from baseline. Now the form is
    # built once, so they must be pushed explicitly or they go stale on a
    # country switch. Summed exactly as baseline_ui derives them, so the box and
    # the form can never disagree. In "By group" mode these inputs do not exist
    # and the update messages are harmlessly dropped.
    oral_tot <- (baseline[["prep_oral_fsw"]]  %||% 0) + (baseline[["prep_oral_msm"]]  %||% 0) +
      (baseline[["prep_oral_agyw"]] %||% 0) + (baseline[["prep_oral_general"]] %||% 0)
    lena_tot <- (baseline[["prep_lenacapavir_fsw"]]  %||% 0) + (baseline[["prep_lenacapavir_msm"]]  %||% 0) +
      (baseline[["prep_lenacapavir_agyw"]] %||% 0) + (baseline[["prep_lenacapavir_general"]] %||% 0)
    updateNumericInput(session, "baseline_prep_total_oral", value = oral_tot)
    updateNumericInput(session, "baseline_prep_total_lena", value = lena_tot)
  }, ignoreInit = TRUE)
  # ---- Reset scenarios to baseline ----------------------------------------
  # Clears the touched flags and pushes the current baseline into every scenario
  # field. Uses scen_push() rather than re-rendering scenario_ui, so there's no
  # DOM rebuild and no focus loss. scen_push() records each write, so the echoes
  # coming back are not read as user edits.
  observeEvent(input$reset_scenarios, {
    baseline <- baseline_input_values()
    if (length(baseline) == 0) return()
    
    n <- length(ls(scen_touched, all.names = TRUE))
    rm(list = ls(scen_touched, all.names = TRUE), envir = scen_touched)
    rm(list = ls(scen_seed,    all.names = TRUE), envir = scen_seed)
    
    for (group_key in names(intervention_groups)) {
      group <- intervention_groups[[group_key]]
      for (int_key in names(group$interventions)) {
        v <- unname(baseline[[int_key]])
        if (is.null(v) || is.na(v)) next
        scen_push(paste0("scenario1_", int_key), v)
        scen_push(paste0("scenario2_", int_key), v)
      }
    }
    
    # Total-mode PrEP boxes are separate inputs, not intervention keys. Summed
    # exactly the way scenario_ui derives its "Baseline:" label, so the box and
    # the label always agree. In "By group" mode these inputs don't exist and
    # the update messages are harmlessly dropped.
    oral <- (baseline[["prep_oral_fsw"]]  %||% 0) + (baseline[["prep_oral_msm"]]  %||% 0) +
      (baseline[["prep_oral_agyw"]] %||% 0) + (baseline[["prep_oral_general"]] %||% 0)
    lena <- (baseline[["prep_lenacapavir_fsw"]]  %||% 0) + (baseline[["prep_lenacapavir_msm"]]  %||% 0) +
      (baseline[["prep_lenacapavir_agyw"]] %||% 0) + (baseline[["prep_lenacapavir_general"]] %||% 0)
    scen_push("scenario1_prep_total_oral", oral)
    scen_push("scenario2_prep_total_oral", oral)
    scen_push("scenario1_prep_total_lena", lena)
    scen_push("scenario2_prep_total_lena", lena)
    
    showNotification(
      if (n > 0) paste0("Both scenarios reset to baseline (", n, " edited field",
                        if (n == 1) "" else "s", " cleared).")
      else "Both scenarios reset to baseline.",
      type = "message", duration = 4)
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
        # radioGroupButtons (clinical_visit_12month) returns character; coerce.
        if (int_key == "clinical_visit_12month") value <- as.numeric(value)
        scenario[[int_key]] <- value
      }
    }
    if (identical(input$prep_entry_mode %||% PREP_ENTRY_MODE_DEFAULT, "total")) {
      pt <- preset_prep_totals()
      sp <- split_prep_total(
        coalesce_num(input[["scenario1_prep_total_oral"]],
                     coalesce_num(input[["baseline_prep_total_oral"]], pt$oral)),
        coalesce_num(input[["scenario1_prep_total_lena"]],
                     coalesce_num(input[["baseline_prep_total_lena"]], pt$lena)))
      for (k in c("prep_oral_fsw", "prep_oral_msm", "prep_oral_agyw", "prep_oral_general",
                  "prep_lenacapavir_fsw", "prep_lenacapavir_msm", "prep_lenacapavir_agyw", "prep_lenacapavir_general"))
        scenario[[k]] <- sp[[k]]
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
        # radioGroupButtons (clinical_visit_12month) returns character; coerce.
        if (int_key == "clinical_visit_12month") value <- as.numeric(value)
        scenario[[int_key]] <- value
      }
    }
    if (identical(input$prep_entry_mode %||% PREP_ENTRY_MODE_DEFAULT, "total")) {
      pt <- preset_prep_totals()
      sp <- split_prep_total(
        coalesce_num(input[["scenario2_prep_total_oral"]],
                     coalesce_num(input[["baseline_prep_total_oral"]], pt$oral)),
        coalesce_num(input[["scenario2_prep_total_lena"]],
                     coalesce_num(input[["baseline_prep_total_lena"]], pt$lena)))
      for (k in c("prep_oral_fsw", "prep_oral_msm", "prep_oral_agyw", "prep_oral_general",
                  "prep_lenacapavir_fsw", "prep_lenacapavir_msm", "prep_lenacapavir_agyw", "prep_lenacapavir_general"))
        scenario[[k]] <- sp[[k]]
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
  
  # Impact-label highlight threshold: a difference strictly below this
  # magnitude (in percentage points, i.e. |diff| < 0.1) is treated as
  # visually insignificant and shown in gray with no +/- sign. Without this,
  # a truly sub-0.1pp change (e.g. 0.04pp) rounds to "0" but was still
  # getting a "+" sign and green highlight, reading as a real change when
  # it wasn't. Per GDoc UI feedback: "highlight only if greater than 0.1%
  # change" (boundary treated as inclusive: exactly 0.1pp is highlighted).
  IMPACT_LABEL_THRESHOLD_PP <- 0.1
  
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
    
    # Round here (not just at display time) so the highlight threshold check
    # and the displayed value can never disagree -- e.g. a raw diff of
    # 0.0999997 (float noise from the upstream division) rounds to "0.1" for
    # display but fails a raw ">= 0.1" check, showing "(0.1pp)" in gray.
    diff_first <- round(first_95 - first_95_base, 1)
    diff_second <- round(second_95 - second_95_base, 1)
    diff_third <- round(third_95 - third_95_base, 1)
    
    tagList(
      div(class = "d-flex justify-content-between border-bottom pb-2 mb-2",
          span(strong("1st 95:"), " % of PLHIV diagnosed"),
          span(
            span(style = "font-size: 1.3em; font-weight: bold;",
                 paste0(round(first_95, 1), "%")),
            br(),
            span(style = paste0("font-size: 0.9em; color: ",
                                ifelse(abs(diff_first) >= IMPACT_LABEL_THRESHOLD_PP,
                                       ifelse(diff_first > 0, "green", "red"), "gray"), ";"),
                 paste0("(", ifelse(diff_first >= IMPACT_LABEL_THRESHOLD_PP, "+", ""), round(diff_first, 1), "pp)"))
          )
      ),
      div(class = "d-flex justify-content-between border-bottom pb-2 mb-2",
          span(strong("2nd 95:"), " % of diagnosed on ART"),
          span(
            span(style = "font-size: 1.3em; font-weight: bold;",
                 paste0(round(second_95, 1), "%")),
            br(),
            span(style = paste0("font-size: 0.9em; color: ",
                                ifelse(abs(diff_second) >= IMPACT_LABEL_THRESHOLD_PP,
                                       ifelse(diff_second > 0, "green", "red"), "gray"), ";"),
                 paste0("(", ifelse(diff_second >= IMPACT_LABEL_THRESHOLD_PP, "+", ""), round(diff_second, 1), "pp)"))
          )
      ),
      div(class = "d-flex justify-content-between border-bottom pb-2 mb-2",
          span(strong("3rd 95:"), " % on ART suppressed"),
          span(
            span(style = "font-size: 1.3em; font-weight: bold;",
                 paste0(round(third_95, 1), "%")),
            br(),
            span(style = paste0("font-size: 0.9em; color: ",
                                ifelse(abs(diff_third) >= IMPACT_LABEL_THRESHOLD_PP,
                                       ifelse(diff_third > 0, "green", "red"), "gray"), ";"),
                 paste0("(", ifelse(diff_third >= IMPACT_LABEL_THRESHOLD_PP, "+", ""), round(diff_third, 1), "pp)"))
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
    
    # Round here (not just at display time) so the highlight threshold check
    # and the displayed value can never disagree -- e.g. a raw diff of
    # 0.0999997 (float noise from the upstream division) rounds to "0.1" for
    # display but fails a raw ">= 0.1" check, showing "(0.1pp)" in gray.
    diff_first <- round(first_95 - first_95_base, 1)
    diff_second <- round(second_95 - second_95_base, 1)
    diff_third <- round(third_95 - third_95_base, 1)
    
    tagList(
      div(class = "d-flex justify-content-between border-bottom pb-2 mb-2",
          span(strong("1st 95:"), " % of PLHIV diagnosed"),
          span(
            span(style = "font-size: 1.3em; font-weight: bold;",
                 paste0(round(first_95, 1), "%")),
            br(),
            span(style = paste0("font-size: 0.9em; color: ",
                                ifelse(abs(diff_first) >= IMPACT_LABEL_THRESHOLD_PP,
                                       ifelse(diff_first > 0, "green", "red"), "gray"), ";"),
                 paste0("(", ifelse(diff_first >= IMPACT_LABEL_THRESHOLD_PP, "+", ""), round(diff_first, 1), "pp)"))
          )
      ),
      div(class = "d-flex justify-content-between border-bottom pb-2 mb-2",
          span(strong("2nd 95:"), " % of diagnosed on ART"),
          span(
            span(style = "font-size: 1.3em; font-weight: bold;",
                 paste0(round(second_95, 1), "%")),
            br(),
            span(style = paste0("font-size: 0.9em; color: ",
                                ifelse(abs(diff_second) >= IMPACT_LABEL_THRESHOLD_PP,
                                       ifelse(diff_second > 0, "green", "red"), "gray"), ";"),
                 paste0("(", ifelse(diff_second >= IMPACT_LABEL_THRESHOLD_PP, "+", ""), round(diff_second, 1), "pp)"))
          )
      ),
      div(class = "d-flex justify-content-between border-bottom pb-2 mb-2",
          span(strong("3rd 95:"), " % on ART suppressed"),
          span(
            span(style = "font-size: 1.3em; font-weight: bold;",
                 paste0(round(third_95, 1), "%")),
            br(),
            span(style = paste0("font-size: 0.9em; color: ",
                                ifelse(abs(diff_third) >= IMPACT_LABEL_THRESHOLD_PP,
                                       ifelse(diff_third > 0, "green", "red"), "gray"), ";"),
                 paste0("(", ifelse(diff_third >= IMPACT_LABEL_THRESHOLD_PP, "+", ""), round(diff_third, 1), "pp)"))
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
    
    # Whichever row comes first keeps the wider bottom margin (mb-3) so the
    # card's internal spacing is identical whether or not ART is shown.
    acq_cls <- if (SHOW_ART_INITIATIONS)
      "d-flex justify-content-between border-bottom pb-2 mb-2"
    else
      "d-flex justify-content-between border-bottom pb-2 mb-3"
    
    # tagList() drops NULL children, so an `if` with no `else` is enough to
    # omit the ART row entirely -- see SHOW_ART_INITIATIONS at top of file.
    tagList(
      if (SHOW_ART_INITIATIONS) div(
        class = "d-flex justify-content-between border-bottom pb-2 mb-3",
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
      div(class = acq_cls,
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
    
    # Whichever row comes first keeps the wider bottom margin (mb-3) so the
    # card's internal spacing is identical whether or not ART is shown.
    acq_cls <- if (SHOW_ART_INITIATIONS)
      "d-flex justify-content-between border-bottom pb-2 mb-2"
    else
      "d-flex justify-content-between border-bottom pb-2 mb-3"
    
    # tagList() drops NULL children, so an `if` with no `else` is enough to
    # omit the ART row entirely -- see SHOW_ART_INITIATIONS at top of file.
    tagList(
      if (SHOW_ART_INITIATIONS) div(
        class = "d-flex justify-content-between border-bottom pb-2 mb-3",
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
      div(class = acq_cls,
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
  
  # ---- Cost by programme area -------------------------------------------------
  cost_cat_levels <- c("ART provision", "Prevention", "Testing",
                       "Treatment monitoring & quality",
                       "Retention & Adherence", "Advanced HIV")
  cost_cat_colors <- c(
    "ART provision"                  = "#475569",  # slate (new)
    "Prevention"                     = "#10b981",  # group colours, reused
    "Testing"                        = "#3b82f6",
    "Treatment monitoring & quality" = "#f59e0b",
    "Retention & Adherence"          = "#ec4899",
    "Advanced HIV"                   = "#8b5cf6"
  )
  
  cost_breakdown_df <- reactive({
    pull <- function(o) c(o$art_provision_cost, o$prevention_cost, o$testing_cost,
                          o$treatment_monitoring_cost, o$retention_cost,
                          o$advanced_disease_cost)
    data.frame(
      Scenario = factor(rep(c("Baseline", "Scenario 1", "Scenario 2"),
                            each = length(cost_cat_levels)),
                        levels = c("Baseline", "Scenario 1", "Scenario 2")),
      Category = factor(rep(cost_cat_levels, times = 3), levels = cost_cat_levels),
      Cost     = c(pull(outcomes_baseline()),
                   pull(outcomes_scenario1()),
                   pull(outcomes_scenario2()))
    )
  })
  
  output$cost_breakdown_stacked <- renderPlot({
    df  <- cost_breakdown_df()
    tot <- tapply(df$Cost, df$Scenario, sum)
    df$bar_total <- as.numeric(tot[as.character(df$Scenario)])
    df$frac      <- ifelse(df$bar_total > 0, df$Cost / df$bar_total, 0)
    totals <- data.frame(Scenario  = factor(names(tot), levels = levels(df$Scenario)),
                         bar_total = as.numeric(tot))
    
    ggplot(df, aes(x = Scenario, y = Cost, fill = Category)) +
      # reverse => first level (ART provision) sits at the BOTTOM (cascade order)
      geom_col(width = 0.62, position = position_stack(reverse = TRUE)) +
      # full-dollar value per segment; hidden on slivers (<3% of the bar) to avoid overlap
      geom_text(aes(label = ifelse(frac >= 0.03, scales::dollar(Cost, accuracy = 1), "")),
                position = position_stack(vjust = 0.5, reverse = TRUE),
                colour = "white", fontface = "bold", size = 3.2) +
      # grand total above each bar
      geom_text(data = totals, inherit.aes = FALSE,
                aes(x = Scenario, y = bar_total,
                    label = scales::dollar(bar_total, accuracy = 1)),
                vjust = -0.4, fontface = "bold", size = 3.5) +
      scale_fill_manual(values = cost_cat_colors, breaks = cost_cat_levels) +
      scale_y_continuous(labels = scales::comma, expand = expansion(mult = c(0, 0.10))) +
      labs(x = NULL, y = "Cost (US$)", fill = NULL) +
      theme_minimal(base_size = 13) +
      theme(legend.position = "right",
            panel.grid.major.x = element_blank())
  })
  
  output$plot_scenario1 <- renderPlot({
    outcomes <- outcomes_scenario1()
    diff <- diff_scenario1()
    diff2 <- diff_scenario2()    # needed for the shared y-axis range
    
    # ART initiations is an optional third bar (SHOW_ART_INITIATIONS, top of
    # file). c() drops NULL, so every column shrinks together and data.frame()
    # still sees equal-length vectors.
    plot_data <- data.frame(
      Outcome = c("Acquisitions", "Deaths",
                  if (SHOW_ART_INITIATIONS) "ART\nInitiations"),
      Value   = c(diff$diff_new_infections,
                  diff$diff_deaths,
                  if (SHOW_ART_INITIATIONS) diff$diff_art_initiations),
      # is_good = TRUE when the change is favourable (green); FALSE when adverse (red).
      # Infections/Deaths: fewer is better -> Value < 0 is good.
      # ART Initiations:   more is better  -> Value > 0 is good.
      is_good = c(diff$diff_new_infections < 0,
                  diff$diff_deaths        < 0,
                  if (SHOW_ART_INITIATIONS) (diff$diff_art_initiations >= 0)),
      Baseline = c(outcomes_baseline()$end_new_infections,
                   outcomes_baseline()$end_deaths,
                   if (SHOW_ART_INITIATIONS) outcomes_baseline()$art_initiations)
    )
    
    # Shared y-axis range across BOTH scenario plots so bar heights are
    # directly comparable. Computed from the union of both scenarios' diffs.
    all_vals <- c(diff$diff_new_infections,  diff$diff_deaths,
                  diff2$diff_new_infections, diff2$diff_deaths,
                  if (SHOW_ART_INITIATIONS) c(diff$diff_art_initiations,
                                              diff2$diff_art_initiations))
    y_lim <- range(c(all_vals, 0), na.rm = TRUE)
    
    ggplot(plot_data, aes(x = Outcome, y = Value, fill = is_good)) +
      # narrower bars when ART is hidden so two columns do not look stretched
      geom_col(width = if (SHOW_ART_INITIATIONS) 0.7 else 0.5) +
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
    
    # ART initiations is an optional third bar (SHOW_ART_INITIATIONS, top of
    # file). c() drops NULL, so every column shrinks together and data.frame()
    # still sees equal-length vectors.
    plot_data <- data.frame(
      Outcome = c("Acquisitions", "Deaths",
                  if (SHOW_ART_INITIATIONS) "ART\nInitiations"),
      Value   = c(diff$diff_new_infections,
                  diff$diff_deaths,
                  if (SHOW_ART_INITIATIONS) diff$diff_art_initiations),
      # is_good = TRUE when the change is favourable (green); FALSE when adverse (red).
      # Infections/Deaths: fewer is better -> Value < 0 is good.
      # ART Initiations:   more is better  -> Value > 0 is good.
      is_good = c(diff$diff_new_infections < 0,
                  diff$diff_deaths        < 0,
                  if (SHOW_ART_INITIATIONS) (diff$diff_art_initiations >= 0)),
      Baseline = c(outcomes_baseline()$end_new_infections,
                   outcomes_baseline()$end_deaths,
                   if (SHOW_ART_INITIATIONS) outcomes_baseline()$art_initiations)
    )
    
    # Shared y-axis range — must match the calculation in plot_scenario1.
    all_vals <- c(diff$diff_new_infections,  diff$diff_deaths,
                  diff1$diff_new_infections, diff1$diff_deaths,
                  if (SHOW_ART_INITIATIONS) c(diff$diff_art_initiations,
                                              diff1$diff_art_initiations))
    y_lim <- range(c(all_vals, 0), na.rm = TRUE)
    
    ggplot(plot_data, aes(x = Outcome, y = Value, fill = is_good)) +
      # narrower bars when ART is hidden so two columns do not look stretched
      geom_col(width = if (SHOW_ART_INITIATIONS) 0.7 else 0.5) +
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
  
  # Per-scenario PrEP allocation table for the PDF report. Mirrors
  # render_prep_group_summary(): in total mode it splits the entered oral/lena
  # totals; in by-group mode it reads the per-group inputs directly. Returns a
  # ready-to-render table plus the entered totals and overflow/discard so the
  # report can explain where a "total" entry landed. Called under isolate().
  prep_delivery_summary <- function(scenario_prefix) {
    mode   <- input$prep_entry_mode %||% PREP_ENTRY_MODE_DEFAULT
    groups <- c("fsw", "msm", "agyw", "general")
    glab   <- c(fsw = "FSW", msm = "MSM", agyw = "AGYW", general = "General")
    
    if (identical(mode, "total")) {
      to <- input[[paste0(scenario_prefix, "_prep_total_oral")]] %||% 0
      tl <- input[[paste0(scenario_prefix, "_prep_total_lena")]] %||% 0
      sp <- split_prep_total(to, tl)
      oral <- vapply(groups, function(g) sp[[paste0("prep_oral_", g)]] %||% 0, numeric(1))
      lena <- vapply(groups, function(g) sp[[paste0("prep_lenacapavir_", g)]] %||% 0, numeric(1))
      entered_oral <- as.numeric(to); entered_lena <- as.numeric(tl)
      overflow <- sp$overflow_to_general %||% 0; discarded <- sp$discarded %||% 0
    } else {
      oral <- vapply(groups, function(g) input[[paste0(scenario_prefix, "_prep_oral_", g)]] %||% 0, numeric(1))
      lena <- vapply(groups, function(g) input[[paste0(scenario_prefix, "_prep_lenacapavir_", g)]] %||% 0, numeric(1))
      entered_oral <- NA_real_; entered_lena <- NA_real_
      overflow <- 0; discarded <- 0
    }
    pops  <- vapply(groups, function(g) { pp <- group_pop(g); if (is.null(pp) || is.na(pp)) NA_real_ else as.numeric(pp) }, numeric(1))
    total <- as.numeric(oral) + as.numeric(lena)
    covg  <- ifelse(is.na(pops) | pops <= 0, NA_real_, round(100 * total / pops, 1))
    
    tbl <- data.frame(
      Group         = unname(glab[groups]),
      `Oral PrEP`   = round(as.numeric(oral)),
      Lenacapavir   = round(as.numeric(lena)),
      Total         = round(total),
      Population    = round(pops),
      `Coverage %`  = covg,
      check.names = FALSE, stringsAsFactors = FALSE
    )
    list(mode = mode, entered_oral = entered_oral, entered_lena = entered_lena,
         overflow_to_general = overflow, discarded = discarded, table = tbl)
  }
  
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
        interventions   = intervention_groups,
        # Passed explicitly rather than relying on the Rmd's env inheriting
        # globalenv() -- that coupling is invisible and breaks silently.
        show_art_initiations = SHOW_ART_INITIATIONS,
        # Changed-parameter appendix. Zero-row data.frame when nothing was
        # touched -- the template drops the whole appendix on that.
        param_overrides = isolate(param_override_summary()),
        # Per-scenario PrEP allocation so the report can show where an entered
        # total landed (delivered per group, coverage, overflow-to-General).
        prep_delivery   = isolate(list(
          baseline  = prep_delivery_summary("baseline"),
          scenario1 = prep_delivery_summary("scenario1"),
          scenario2 = prep_delivery_summary("scenario2")
        ))
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