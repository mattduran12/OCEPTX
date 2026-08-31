# ============================================================
# EL PASO CRIME LAB
# TRAFFIC STOP RACIAL PROFILING ANALYSIS
# BETA
#
# VERSION
# - Validation-first user experience
# - State-mandated data quality / completeness framing
# - Combined traffic stop + search data entry
# - Traffic stops are the master total
# - Searches Conducted automatically calculated from Yes + No
# - Search validation tied to total traffic stops
# - Column-level validation by race/ethnicity
# - Contraband validation tied to Searches Conducted = Yes
# - Contraband description validation
# - Contraband arrest validation
# - Missing-data validation
# - Missing required cells highlighted after validation
# - Error messages optimized for users
# - Texas jurisdiction selector
# - Texas jurisdiction data loaded from LOCAL CSV
# - 2020 Census 18+ population benchmark
# - NO Census API
# - Population benchmark uses Population_18Plus
# - Race/ethnicity population breakdown loaded from CSV
# - Plotly population comparison
# - Contraband hit rates
# - No Comparative Analysis Report
# - No PDF report
# - Technical Details
# - CSV data download
# - No data are permanently saved
# ============================================================


# ============================================================
# PACKAGES
# ============================================================

library(shiny)
library(bslib)
library(plotly)
library(scales)


# ============================================================
# EPCL COLORS
# ============================================================

EPCL_BLUE <- "#6FA8C2"
EPCL_DARK <- "#263238"
EPCL_GOLD <- "#C9A24A"
EPCL_LIGHT <- "#F5F8FA"
EPCL_GRAY <- "#667085"

EPCL_BURGUNDY <- "#6B1F2B"
EPCL_CHART_GOLD <- "#C9A24A"


# ============================================================
# RACIAL / ETHNIC CATEGORIES
# ============================================================

race_cols <- c(
  "White",
  "Black",
  "Hispanic / Latino",
  "Asian / Pacific Islander",
  "Alaska Native / American Indian"
)


# ============================================================
# TEXAS CENSUS JURISDICTION FILE
#
# Expected structure:
#
# State
# Census_Year
# Census_Source
# Census_Dataset
# Census_Table
# Population_Universe
# Geography_Type
# Geography_ID
# Geography_Name
# Population_18Plus
# Hispanic_Latino_18Plus
# Not_Hispanic_18Plus
# Not_Hispanic_White_18Plus
# Not_Hispanic_Black_18Plus
# Not_Hispanic_AIAN_18Plus
# Not_Hispanic_Asian_18Plus
# Not_Hispanic_NHPI_18Plus
# Not_Hispanic_Other_Race_18Plus
# Not_Hispanic_TwoPlus_Races_18Plus
# ============================================================


census_file <- "texas_census_2020_age18_race_ethnicity.csv"


# ============================================================
# VERIFY CENSUS FILE EXISTS
# ============================================================

if (!file.exists(census_file)) {
  
  stop(
    paste0(
      "Texas Census benchmark file was not found at:\n\n",
      normalizePath(
        census_file,
        winslash = "/",
        mustWork = FALSE
      ),
      "\n\n",
      "Make sure texas_census_2020_age18_race_ethnicity.csv ",
      "is located in the application's data folder."
    )
  )
  
}


# ============================================================
# LOAD TEXAS CENSUS DATA
# ============================================================

texas_geographies <- read.csv(
  census_file,
  stringsAsFactors = FALSE,
  check.names = FALSE
)


# ============================================================
# VERIFY REQUIRED COLUMNS
# ============================================================

required_census_columns <- c(
  "State",
  "Census_Year",
  "Census_Source",
  "Census_Dataset",
  "Census_Table",
  "Population_Universe",
  "Geography_Type",
  "Geography_ID",
  "Geography_Name",
  "Population_18Plus",
  "Hispanic_Latino_18Plus",
  "Not_Hispanic_18Plus",
  "Not_Hispanic_White_18Plus",
  "Not_Hispanic_Black_18Plus",
  "Not_Hispanic_AIAN_18Plus",
  "Not_Hispanic_Asian_18Plus",
  "Not_Hispanic_NHPI_18Plus",
  "Not_Hispanic_Other_Race_18Plus",
  "Not_Hispanic_TwoPlus_Races_18Plus"
)


missing_census_columns <- setdiff(
  required_census_columns,
  names(texas_geographies)
)


if (length(missing_census_columns) > 0) {
  
  stop(
    paste0(
      "The Texas Census benchmark file is missing the following ",
      "required columns:\n\n",
      paste(
        missing_census_columns,
        collapse = ", "
      )
    )
  )
  
}


# ============================================================
# STANDARDIZE CENSUS COLUMNS
# ============================================================

texas_geographies$State <-
  as.character(texas_geographies$State)

texas_geographies$Census_Year <-
  as.character(texas_geographies$Census_Year)

texas_geographies$Census_Source <-
  as.character(texas_geographies$Census_Source)

texas_geographies$Census_Dataset <-
  as.character(texas_geographies$Census_Dataset)

texas_geographies$Census_Table <-
  as.character(texas_geographies$Census_Table)

texas_geographies$Population_Universe <-
  as.character(texas_geographies$Population_Universe)

texas_geographies$Geography_Type <-
  as.character(texas_geographies$Geography_Type)

texas_geographies$Geography_ID <-
  as.character(texas_geographies$Geography_ID)

texas_geographies$Geography_Name <-
  as.character(texas_geographies$Geography_Name)


# ============================================================
# NUMERIC CENSUS VARIABLES
# ============================================================

numeric_cols <- c(
  "Population_18Plus",
  "Hispanic_Latino_18Plus",
  "Not_Hispanic_18Plus",
  "Not_Hispanic_White_18Plus",
  "Not_Hispanic_Black_18Plus",
  "Not_Hispanic_AIAN_18Plus",
  "Not_Hispanic_Asian_18Plus",
  "Not_Hispanic_NHPI_18Plus",
  "Not_Hispanic_Other_Race_18Plus",
  "Not_Hispanic_TwoPlus_Races_18Plus"
)


for (col in numeric_cols) {
  
  texas_geographies[[col]] <-
    suppressWarnings(
      as.numeric(
        gsub(
          ",",
          "",
          trimws(
            as.character(
              texas_geographies[[col]]
            )
          )
        )
      )
    )
  
}


# ============================================================
# POPULATION BENCHMARK CONSISTENCY
# ============================================================

texas_geographies$Total_Population <-
  texas_geographies$Population_18Plus


# ============================================================
# KEEP TEXAS COUNTIES AND PLACES ONLY
# ============================================================

texas_geographies <-
  texas_geographies[
    tolower(trimws(texas_geographies$Geography_Type)) %in%
      c("county", "place"),
  ]


# ============================================================
# REMOVE INVALID JURISDICTIONS
# ============================================================

texas_geographies <-
  texas_geographies[
    !is.na(texas_geographies$Geography_ID) &
      texas_geographies$Geography_ID != "" &
      !is.na(texas_geographies$Geography_Name) &
      texas_geographies$Geography_Name != "" &
      !is.na(texas_geographies$Population_18Plus),
  ]


# ============================================================
# REMOVE DUPLICATE GEOGRAPHIES
# ============================================================

texas_geographies <-
  texas_geographies[
    !duplicated(texas_geographies$Geography_ID),
  ]


# ============================================================
# DISPLAY LABEL
# ============================================================

texas_geographies$Display <-
  paste0(
    texas_geographies$Geography_Name,
    " — ",
    texas_geographies$Geography_Type
  )


# ============================================================
# SORT JURISDICTIONS
# ============================================================

texas_geographies <-
  texas_geographies[
    order(
      texas_geographies$Geography_Type,
      texas_geographies$Geography_Name
    ),
  ]


# ============================================================
# JURISDICTION SELECTOR CHOICES
# ============================================================

texas_jurisdiction_choices <-
  setNames(
    texas_geographies$Geography_ID,
    texas_geographies$Display
  )


# ============================================================
# DEFAULT JURISDICTION
# ============================================================

default_jurisdiction <-
  texas_geographies$Geography_ID[
    grepl(
      "El Paso",
      texas_geographies$Geography_Name,
      ignore.case = TRUE
    ) &
      tolower(texas_geographies$Geography_Type) == "county"
  ]


if (length(default_jurisdiction) == 0) {
  
  default_jurisdiction <-
    texas_geographies$Geography_ID[1]
  
} else {
  
  default_jurisdiction <-
    default_jurisdiction[1]
  
}


# ============================================================
# POPULATION SOURCE
# ============================================================

population_source <- paste0(
  "Data downloaded August 30, 2026, from the ",
  "U.S. Census Bureau's 2020 Decennial Census. ",
  "The population benchmark represents the adult population age 18 and older."
)


# ============================================================
# CENSUS DATA WEBSITE
# ============================================================

acs_url <- "https://data.census.gov/"


# ============================================================
# TRAFFIC STOP ROWS
# ============================================================

traffic_rows <- c(
  "Number of Stops",
  "Gender",
  "Female",
  "Male",
  "Reason for Stop",
  "Violation of Law",
  "Preexisting Knowledge",
  "Moving Traffic Violation",
  "Vehicle Traffic Violation",
  "Result of Stop",
  "Verbal Warning",
  "Written Warning",
  "Citation",
  "Written Warning and Arrest",
  "Citation and Arrest",
  "Arrest",
  "Arrest Based On",
  "Violation of Penal Code",
  "Violation of Traffic Law",
  "Violation of City Ordinance",
  "Outstanding Warrant",
  "Physical Force Resulting in Bodily Injury Used?",
  "No",
  "Yes"
)


# ============================================================
# SEARCH SECTIONS
# ============================================================

search_sections <- list(
  
  list(
    title = "Searches Conducted",
    rows = c("Yes", "No")
  ),
  
  list(
    title = "Was Contraband Discovered?",
    rows = c("Yes", "No")
  ),
  
  list(
    title = "Description of Contraband",
    rows = c(
      "Drugs",
      "Weapons",
      "Currency",
      "Alcohol",
      "Stolen Property",
      "Other"
    )
  ),
  
  list(
    title = "Did Discovery of Contraband Result in Arrest?",
    rows = c("Yes", "No")
  )
  
)


# ============================================================
# SECTION HEADERS
# ============================================================

traffic_section_headers <- c(
  "Gender",
  "Reason for Stop",
  "Result of Stop",
  "Arrest Based On",
  "Physical Force Resulting in Bodily Injury Used?"
)


# ============================================================
# VALIDATION GROUPS
# ============================================================

traffic_validation_map <- list(
  
  "Gender" = c(
    "Female",
    "Male"
  ),
  
  "Reason for Stop" = c(
    "Violation of Law",
    "Preexisting Knowledge",
    "Moving Traffic Violation",
    "Vehicle Traffic Violation"
  ),
  
  "Result of Stop" = c(
    "Verbal Warning",
    "Written Warning",
    "Citation",
    "Written Warning and Arrest",
    "Citation and Arrest",
    "Arrest"
  ),
  
  "Arrest Based On" = c(
    "Violation of Penal Code",
    "Violation of Traffic Law",
    "Violation of City Ordinance",
    "Outstanding Warrant"
  ),
  
  "Physical Force Resulting in Bodily Injury Used?" = c(
    "No",
    "Yes"
  )
  
)


# ============================================================
# CLEAN INPUT ID
# ============================================================

clean_id <- function(x) {
  
  x <- tolower(x)
  
  x <- gsub(
    "[^a-z0-9]+",
    "_",
    x
  )
  
  x <- gsub(
    "^_+|_+$",
    "",
    x
  )
  
  x
  
}


# ============================================================
# SEARCH INPUT ID
# ============================================================

search_id <- function(
    section,
    value,
    r
) {
  
  paste0(
    "search_",
    clean_id(section),
    "_",
    clean_id(value),
    "_",
    r
  )
  
}


# ============================================================
# UI
# ============================================================

ui <- fluidPage(
  
  theme = bs_theme(
    version = 5,
    bg = "#FFFFFF",
    fg = EPCL_DARK,
    primary = EPCL_BLUE,
    secondary = EPCL_GOLD
  ),
  
  tags$head(
    
    tags$style(
      
      HTML(
        
        "
        body {
          background: #FFFFFF;
          color: #263238;
        }

        .main-container {
          max-width: 1450px;
          margin: 0 auto;
          padding: 40px 45px 70px 45px;
        }

        .app-title {
          font-size: 2.2rem;
          font-weight: 700;
          color: #263238;
          margin-bottom: 5px;
          letter-spacing: -0.02em;
        }

        .app-subtitle {
          color: #667085;
          font-size: 1rem;
          margin-bottom: 30px;
        }

        .nav-tabs {
          border-bottom: 1px solid #D0D5DD;
        }

        .nav-tabs .nav-link {
          color: #667085;
          font-weight: 600;
          padding: 12px 18px;
        }

        .nav-tabs .nav-link.active {
          color: #263238;
          border-color: #D0D5DD #D0D5DD #FFFFFF;
          border-top: 3px solid #6FA8C2;
        }

        .instructions {
          background: #F5F8FA;
          border-left: 4px solid #6FA8C2;
          padding: 20px 24px;
          margin: 20px 0;
          border-radius: 5px;
          color: #475467;
          line-height: 1.65;
        }

        .instructions h4 {
          margin-top: 0;
          color: #263238;
          font-weight: 700;
        }

        .step-card {
          border: 1px solid #D0D5DD;
          border-radius: 8px;
          padding: 20px 22px;
          margin: 15px 0;
          background: #FFFFFF;
        }

        .step-number {
          display: inline-flex;
          align-items: center;
          justify-content: center;
          width: 30px;
          height: 30px;
          border-radius: 50%;
          background: #6FA8C2;
          color: #FFFFFF;
          font-weight: 700;
          margin-right: 10px;
        }

        .step-title {
          font-weight: 700;
          color: #263238;
          font-size: 1rem;
        }

        .data-table {
          width: 100%;
          border-collapse: separate;
          border-spacing: 0;
          border: 1px solid #D0D5DD;
          border-radius: 7px;
          overflow: hidden;
          margin-bottom: 30px;
        }

        .data-table th {
          background: #263238;
          color: #FFFFFF;
          padding: 13px 10px;
          text-align: center;
          font-size: 0.84rem;
          font-weight: 600;
          vertical-align: middle;
        }

        .data-table th:first-child {
          text-align: left;
          min-width: 310px;
        }

        .data-table td {
          padding: 7px 9px;
          border-top: 1px solid #E4E7EC;
          background: #FFFFFF;
          vertical-align: middle;
        }

        .section-row td {
          background: #F5F8FA !important;
          color: #263238 !important;
          font-weight: 700;
          font-size: 0.9rem;
          padding: 12px 10px;
          border-top: 2px solid #D0D5DD;
        }

        .subcategory {
          padding-left: 32px !important;
          color: #475467 !important;
        }

        .data-table .form-group {
          margin-bottom: 0;
        }

        .data-table .form-control {
          text-align: right;
          border: 1px solid #D0D5DD;
          border-radius: 5px;
          padding: 7px 8px;
          font-size: 0.9rem;
          height: 38px;
        }

        .data-table .form-control:focus {
          border-color: #6FA8C2;
          box-shadow: 0 0 0 0.15rem rgba(111, 168, 194, 0.20);
        }

        .total-cell {
          background: #F9FAFB !important;
          font-weight: 700;
          text-align: right;
          color: #263238;
          min-width: 90px;
        }

        .generate-button {
          background: #6FA8C2 !important;
          color: #FFFFFF !important;
          border: none !important;
          padding: 12px 22px;
          border-radius: 6px;
          font-weight: 600;
          font-size: 0.95rem;
        }

        .generate-button:hover {
          background: #5B91AA !important;
        }

        .reset-button,
        .download-data-button {
          margin-left: 10px;
        }

        .chart-card {
          border: 1px solid #E4E7EC;
          border-radius: 8px;
          padding: 25px;
          margin-top: 25px;
          background: #FFFFFF;
        }

        .benchmark-card {
          background: #FFFFFF;
          border: 1px solid #D0D5DD;
          border-radius: 8px;
          padding: 20px;
          margin-top: 20px;
        }

        .status-note {
          padding: 16px 20px;
          border-radius: 6px;
          margin: 18px 0;
          line-height: 1.6;
        }

        .status-complete {
          background: #ECFDF3;
          border: 1px solid #ABEFC6;
          color: #067647;
        }

        .status-warning {
          background: #FFFAEB;
          border: 1px solid #FEDF89;
          color: #B54708;
        }

        .status-error {
          background: #FEF3F2;
          border: 1px solid #FECDCA;
          color: #B42318;
        }

        .validation-summary {
          margin-top: 20px;
          margin-bottom: 20px;
        }

        .validation-header {
          font-size: 1.05rem;
          font-weight: 700;
          margin-bottom: 8px;
        }

        .validation-row {
          padding: 12px 14px;
          border-bottom: 1px solid #E4E7EC;
          line-height: 1.55;
        }

        .validation-row:last-child {
          border-bottom: none;
        }

        .validation-check {
          color: #198754;
          font-weight: 700;
          font-size: 18px;
          margin-right: 8px;
        }

        .validation-cross {
          color: #DC3545;
          font-weight: 700;
          font-size: 18px;
          margin-right: 8px;
        }

        .validation-action {
          margin-top: 12px;
          font-weight: 600;
        }

        .validation-missing {
          border: 2px solid #DC3545 !important;
          box-shadow: 0 0 0 1px rgba(220, 53, 69, 0.15) !important;
          background-color: #FFF8F8 !important;
        }

        .validation-missing:focus {
          border: 2px solid #DC3545 !important;
          box-shadow: 0 0 0 2px rgba(220, 53, 69, 0.12) !important;
        }

        .validation-legend {
          display: inline-flex;
          align-items: center;
          margin-top: 10px;
          color: #667085;
          font-size: 0.88rem;
        }

        .validation-legend-box {
          width: 15px;
          height: 15px;
          border: 2px solid #DC3545;
          background: #FFF8F8;
          border-radius: 3px;
          margin-right: 7px;
        }

        .hit-rate-card {
          border: 1px solid #D0D5DD;
          border-radius: 8px;
          padding: 25px;
          margin-top: 25px;
          background: #FFFFFF;
        }

        .hit-rate-title {
          font-size: 1.25rem;
          font-weight: 700;
          color: #263238;
          margin-bottom: 5px;
        }

        .hit-rate-subtitle {
          color: #667085;
          font-size: 0.9rem;
          margin-bottom: 20px;
        }

        .hit-rate-overall {
          background: #F5F8FA;
          border-left: 4px solid #C9A24A;
          border-radius: 5px;
          padding: 18px 20px;
          margin-bottom: 20px;
        }

        .hit-rate-number {
          font-size: 2rem;
          font-weight: 700;
          color: #263238;
        }

        .hit-rate-label {
          color: #667085;
          font-size: 0.88rem;
        }

        .rate-table {
          width: 100%;
          border-collapse: collapse;
          margin-top: 10px;
        }

        .rate-table th {
          background: #263238;
          color: #FFFFFF;
          padding: 11px 10px;
          text-align: left;
          font-size: 0.85rem;
        }

        .rate-table td {
          padding: 11px 10px;
          border-bottom: 1px solid #E4E7EC;
          font-size: 0.9rem;
        }

        .rate-table td.numeric {
          text-align: right;
        }

        .footnote {
          color: #667085;
          font-size: 0.82rem;
          margin-top: 10px;
          line-height: 1.5;
        }

        .jurisdiction-info {
          margin-top: 10px;
          color: #667085;
          font-size: 0.88rem;
        }

        .technical-card {
          background: #F5F8FA;
          border: 1px solid #D0D5DD;
          border-radius: 8px;
          padding: 25px 30px;
          margin-top: 20px;
          line-height: 1.7;
        }

        .technical-card h4 {
          color: #263238;
          font-weight: 700;
        }

        .required-badge {
          display: inline-block;
          background: #FEF3F2;
          color: #B42318;
          border: 1px solid #FECDCA;
          padding: 3px 8px;
          border-radius: 12px;
          font-size: 0.75rem;
          font-weight: 700;
          margin-left: 6px;
        }

        @media (max-width: 900px) {

          .main-container {
            padding: 25px 15px 50px 15px;
          }

          .data-table {
            display: block;
            overflow-x: auto;
          }

          .data-table th,
          .data-table td {
            white-space: nowrap;
          }

        }
        "
        
      )
      
    ),
    
    # ========================================================
    # JAVASCRIPT FOR VALIDATION CELL HIGHLIGHTING
    # ========================================================
    
    tags$script(
      
      HTML(
        
        "
        Shiny.addCustomMessageHandler(
          'validation_mark_cells',
          function(message) {

            $('.validation-missing').removeClass('validation-missing');

            if (
              message &&
              message.ids &&
              message.ids.length > 0
            ) {

              message.ids.forEach(
                function(id) {

                  var element =
                    document.getElementById(id);

                  if (element) {

                    var input =
                      element.querySelector('input');

                    if (input) {
                      $(input).addClass('validation-missing');
                    } else {
                      $(element).addClass('validation-missing');
                    }

                  }

                }
              );

            }

          }
        );

        $(document).on(
          'input change',
          '.data-table input',
          function() {

            $(this).removeClass('validation-missing');

          }
        );
        "
        
      )
      
    )
    
  ),
  
  div(
    
    class = "main-container",
    
    div(
      class = "app-title",
      "Traffic Stop Racial Profiling Analysis (BETA)"
    ),
    
    div(
      class = "app-subtitle",
      "El Paso Crime Lab"
    ),
    
    tabsetPanel(
      
      id = "tabs",
      type = "tabs",
      
      
      # ======================================================
      # TAB 1 — VALIDATION & SUBMISSION CHECK
      # ======================================================
      
      tabPanel(
        
        title = "Data Check",
        
        br(),
        
        h3(
          "Data Check",
          style = "font-weight:700;"
        ),
        
        div(
          
          class = "instructions",
          
          h4(
            ""
          ),
          
          p(
            tags$strong(
              "Purpose: "
            ),
            "This tool is designed to help you review the traffic-stop and search data required for state-mandated reporting. "
          ),
          
          p(
            "The data check looks for missing information and verifies that related totals agree with one another. A successful validation means the numbers entered in the application are internally consistent. It does not independently verify the accuracy of the underlying records."
          )
          
        ),
        
        div(
          
          class = "step-card",
          
          tags$span(
            class = "step-number",
            "1"
          ),
          
          tags$span(
            class = "step-title",
            "Enter your traffic stop data"
          ),
          
          p(
            style = "margin:10px 0 0 40px;",
            "Go to the Traffic Stops & Searches tab and enter the required counts for each race/ethnicity category."
          )
          
        ),
        
        div(
          
          class = "step-card",
          
          tags$span(
            class = "step-number",
            "2"
          ),
          
          tags$span(
            class = "step-title",
            "Run the validation check"
          ),
          
          p(
            style = "margin:10px 0 0 40px;",
            "Return to this tab and select the Run Validation button. The application will identify missing information and internal inconsistencies."
          )
          
        ),
        
        div(
          
          class = "step-card",
          
          tags$span(
            class = "step-number",
            "3"
          ),
          
          tags$span(
            class = "step-title",
            "Fix anything flagged"
          ),
          
          p(
            style = "margin:10px 0 0 40px;",
            "Cells requiring missing information will be outlined in red. Enter or correct the value and the red outline will disappear."
          )
          
        ),
        
        div(
          
          class = "instructions",
          
          tags$strong(
            "The validation rules are:"
          ),
          
          tags$ul(
            
            tags$li(
              "Number of Stops is the master total."
            ),
            
            tags$li(
              "Gender, reason for stop, result of stop, arrest basis, and physical-force totals must equal Number of Stops."
            ),
            
            tags$li(
              "Search = Yes + Search = No must equal Number of Stops."
            ),
            
            tags$li(
              "Contraband = Yes + Contraband = No must equal Searches Conducted."
            ),
            
            tags$li(
              "Contraband descriptions must equal Contraband Discovered = Yes."
            ),
            
            tags$li(
              "Contraband Resulting in Arrest = Yes + No must equal Contraband Discovered = Yes."
            )
            
          ),
          
          div(
            
            class = "validation-legend",
            
            div(
              class = "validation-legend-box"
            ),
            
            "Red outline = information that requires attention."
            
          )
          
        ),
        
        actionButton(
          "check_all_totals",
          "Run Validation",
          class = "generate-button"
        ),
        
        actionButton(
          "reset_all_data",
          "Clear All Data",
          class = "btn btn-outline-secondary reset-button"
        ),
        
        downloadButton(
          "download_data",
          "Download Data",
          class = "btn btn-outline-secondary download-data-button"
        ),
        
        uiOutput(
          "validation_summary"
        )
        
      ),
      
      
      # ======================================================
      # TAB 2 — TRAFFIC STOPS & SEARCHES
      # ======================================================
      
      tabPanel(
        
        title = "Traffic Stops & Searches",
        
        br(),
        
        h3(
          "Traffic Stops & Searches",
          style = "font-weight:700;"
        ),
        
        div(
          
          class = "instructions",
          
          p(
            "Enter the number of traffic stops and related outcomes for each race/ethnicity category."
          ),
          
          
        ),
        
        uiOutput(
          "combined_table"
        ),
        
        uiOutput(
          "hit_rate_output"
        )
        
      ),
      
      
      # ======================================================
      # TAB 3 — POPULATION COMPARISON
      # ======================================================
      
      tabPanel(
        
        title = "Population Comparison",
        
        br(),
        
        h3(
          "Population Comparison",
          style = "font-weight:700;"
        ),
        
        p(
          "Select a Texas county or place and compare the entered traffic-stop or search volume with the adult resident population (18+) reported in the Texas Census 2020 benchmark file."
        ),
        
        radioButtons(
          
          "comparison_source",
          
          "Compare",
          
          choices = c(
            "Traffic Stops" = "stops",
            "Searches" = "searches"
          ),
          
          selected = "stops",
          
          inline = TRUE
          
        ),
        
        selectizeInput(
          
          inputId = "location_select",
          
          label = "Search for a County or Place:",
          
          choices = texas_jurisdiction_choices,
          
          selected = default_jurisdiction,
          
          multiple = FALSE,
          
          options = list(
            
            placeholder = "Type a county or place...",
            
            maxOptions = 500,
            
            searchField = c(
              "label",
              "value"
            )
            
          )
          
        ),
        
        uiOutput(
          "selected_jurisdiction_info"
        ),
        
        div(
          
          class = "instructions",
          
          tags$strong(
            "Population benchmark"
          ),
          
          br(),
          
          "The selected jurisdiction's adult (18+) population and demographic estimates are based on data obtained from ",
          
          tags$strong(
            "the U.S. Census Bureau's 2020 Decennial Census"
          ),
          
          ", accessed August 30, 2026.",
          
          br(),
          br(),
          
          tags$strong(
            "Source: "
          ),
          
          tags$a(
            href = acs_url,
            target = "_blank",
            "U.S. Census Bureau / 2020 Decennial Census"
          )
          
        ),
        
        actionButton(
          "generate",
          "Generate Population Comparison",
          class = "generate-button"
        ),
        
        br(),
        br(),
        
        uiOutput(
          "population_status"
        ),
        
        uiOutput(
          "benchmark_table"
        ),
        
        uiOutput(
          "chart_container"
        )
        
      ),
      
      
      # ======================================================
      # TAB 4 — TECHNICAL DETAILS
      # ======================================================
      
      tabPanel(
        
        title = "Technical Details",
        
        br(),
        
        h3(
          "Technical Details",
          style = "font-weight:700;"
        ),
        
        div(
          
          class = "technical-card",
          
          p(
            
            tags$strong(
              "What this tool does: "
            ),
            
            "This application organizes traffic-stop and search data and provides descriptive information by race or ethnicity. It is designed to help users review required reporting data before submission."
            
          ),
          
          p(
            
            tags$strong(
              "Validation logic: "
            ),
            
            "The Number of Stops is treated as the master total. Searches Conducted is calculated automatically as Searches = Yes + No. The resulting number must equal the total number of stops."
            
          ),
          
          tags$ul(
            
            tags$li(
              "Searches Conducted = Yes + No"
            ),
            
            tags$li(
              "Searches Conducted must equal Number of Stops"
            ),
            
            tags$li(
              "Contraband Discovered = Yes + No"
            ),
            
            tags$li(
              "Contraband Discovered must equal Searches Conducted"
            ),
            
            tags$li(
              "Description of Contraband must equal Contraband Discovered = Yes"
            ),
            
            tags$li(
              "Contraband Resulting in Arrest must equal Contraband Discovered = Yes"
            )
            
          ),
          
          p(
            
            tags$strong(
              "Hit rate: "
            ),
            
            "The contraband hit rate is the percentage of searches in which contraband was discovered. It is calculated as Contraband Discovered = Yes divided by Searches Conducted."
            
          ),
          
          p(
            
            tags$strong(
              "Texas jurisdiction benchmark: "
            ),
            
            "The application reads a local copy of the",
            
            tags$strong(
              "2020 Decennial Census Data (age 18+)"
            ),
            
            " and allows the user to select any Texas county or place contained in that file."
            
          ),
          
          p(
            
            tags$strong(
              "Population data: "
            ),
            
            "The benchmark file contains 2020 Decennial Census 18+ adult population counts and race/ethnicity estimates for each jurisdiction."
            
          ),
          
          p(
            
            tags$strong(
              "Important interpretation: "
            ),
            
            "Population comparisons are descriptive. A difference between the volume of traffic stops and the resident population should not, by itself, be treated as proof of racial profiling or unlawful discrimination."
            
          ),
          
          p(
            
            tags$strong(
              "Data quality: "
            ),
            
            "All entered values should be reviewed for completeness and internal consistency before the results are used in an official report."
            
          ),
          
          p(
            
            tags$strong(
              "Data storage: "
            ),
            
            "Data entered into this application are not permanently saved by the application. Users should retain their original source data separately."
            
          ),
          
          p(
            
            tags$strong(
              "Disclaimer: "
            ),
            
            "This application does not determine whether racial profiling or discrimination occurred. It provides descriptive statistical information and data-quality checks for review and reporting purposes."
            
          )
          
        )
        
      )
      
    )
    
  )
  
)


# ============================================================
# SERVER
# ============================================================

server <- function(input, output, session) {
  
  
  # ==========================================================
  # STATE
  # ==========================================================
  
  check_requested <- reactiveVal(FALSE)
  
  
  # ==========================================================
  # HELPER: GET INPUT VALUE
  # ==========================================================
  
  get_input_value <- function(input_id) {
    
    value <- input[[input_id]]
    
    if (
      is.null(value) ||
      length(value) == 0 ||
      is.na(value)
    ) {
      
      return(NA_real_)
      
    }
    
    suppressWarnings(
      as.numeric(value)
    )
    
  }
  
  
  # ==========================================================
  # HELPER: GET ROW VALUES
  # ==========================================================
  
  get_row_values <- function(
    prefix,
    row_name
  ) {
    
    safe_name <- clean_id(row_name)
    
    values <- sapply(
      
      seq_along(race_cols),
      
      function(r) {
        
        get_input_value(
          paste0(
            prefix,
            safe_name,
            "_",
            r
          )
        )
        
      }
      
    )
    
    as.numeric(values)
    
  }
  
  
  # ==========================================================
  # TRAFFIC STOP COUNTS
  # ==========================================================
  
  get_stop_counts <- reactive({
    
    get_row_values(
      "t1_",
      "Number of Stops"
    )
    
  })
  
  
  # ==========================================================
  # SEARCHES CONDUCTED — YES
  # ==========================================================
  
  get_search_conducted_yes <- reactive({
    
    sapply(
      
      seq_along(race_cols),
      
      function(r) {
        
        get_input_value(
          search_id(
            "Searches Conducted",
            "Yes",
            r
          )
        )
        
      }
      
    )
    
  })
  
  
  # ==========================================================
  # SEARCHES CONDUCTED — NO
  # ==========================================================
  
  get_search_conducted_no <- reactive({
    
    sapply(
      
      seq_along(race_cols),
      
      function(r) {
        
        get_input_value(
          search_id(
            "Searches Conducted",
            "No",
            r
          )
        )
        
      }
      
    )
    
  })
  
  
  # ==========================================================
  # SEARCHES CONDUCTED — AUTOMATIC TOTAL
  # ==========================================================
  
  # ==========================================================
  # SEARCHES CONDUCTED — AUTOMATIC TOTAL
  # ==========================================================
  
  get_search_conducted <- reactive({
    
    yes <- get_search_conducted_yes()
    no  <- get_search_conducted_no()
    
    result <- numeric(length(race_cols))
    
    for (r in seq_along(race_cols)) {
      
      yes_value <- yes[r]
      no_value  <- no[r]
      
      if (is.na(yes_value) && is.na(no_value)) {
        
        result[r] <- NA_real_
        
      } else {
        
        if (is.na(yes_value)) {
          yes_value <- 0
        }
        
        if (is.na(no_value)) {
          no_value <- 0
        }
        
        result[r] <- yes_value + no_value
        
      }
      
    }
    
    names(result) <- race_cols
    
    result
    
  })
  
  # ==========================================================
  # CONTRABAND HIT-RATE DATA
  #
  # This is intentionally separate from the UI so the
  # calculations update whenever the underlying inputs change.
  # ==========================================================
  
  hit_rate_data <- reactive({
    
    searches <- get_search_conducted()
    
    contraband_yes <- get_contraband_discovered_yes()
    
    searches_clean <- searches
    searches_clean[is.na(searches_clean)] <- 0
    
    contraband_clean <- contraband_yes
    contraband_clean[is.na(contraband_clean)] <- 0
    
    total_searches <- sum(
      searches_clean,
      na.rm = TRUE
    )
    
    total_hits <- sum(
      contraband_clean,
      na.rm = TRUE
    )
    
    overall_rate <- NA_real_
    
    if (total_searches > 0) {
      
      overall_rate <-
        (total_hits / total_searches) * 100
      
    }
    
    individual_rates <- rep(
      NA_real_,
      length(race_cols)
    )
    
    for (i in seq_along(race_cols)) {
      
      if (
        !is.na(searches[i]) &&
        searches[i] > 0
      ) {
        
        individual_rates[i] <-
          (
            contraband_clean[i] /
              searches[i]
          ) * 100
        
      }
      
    }
    
    data.frame(
      Group = race_cols,
      Searches = searches_clean,
      Contraband_Found = contraband_clean,
      Hit_Rate = individual_rates,
      stringsAsFactors = FALSE
    ) |>
      structure(
        total_searches = total_searches,
        total_hits = total_hits,
        overall_rate = overall_rate,
        class = c(
          "hit_rate_data",
          "data.frame"
        )
      )
    
  })
  
  
  # ==========================================================
  # CONTRABAND DISCOVERED — YES
  # ==========================================================
  
  get_contraband_discovered_yes <- reactive({
    
    sapply(
      
      seq_along(race_cols),
      
      function(r) {
        
        get_input_value(
          search_id(
            "Was Contraband Discovered?",
            "Yes",
            r
          )
        )
        
      }
      
    )
    
  })
  
  
  # ==========================================================
  # CONTRABAND DISCOVERED — NO
  # ==========================================================
  
  get_contraband_discovered_no <- reactive({
    
    sapply(
      
      seq_along(race_cols),
      
      function(r) {
        
        get_input_value(
          search_id(
            "Was Contraband Discovered?",
            "No",
            r
          )
        )
        
      }
      
    )
    
  })
  
  
  # ==========================================================
  # CONTRABAND DESCRIPTION
  # ==========================================================
  
  get_contraband_description <- function(
    row_name
  ) {
    
    sapply(
      
      seq_along(race_cols),
      
      function(r) {
        
        get_input_value(
          search_id(
            "Description of Contraband",
            row_name,
            r
          )
        )
        
      }
      
    )
    
  }
  
  
  # ==========================================================
  # CONTRABAND ARREST
  # ==========================================================
  
  get_contraband_arrest <- function(
    row_name
  ) {
    
    sapply(
      
      seq_along(race_cols),
      
      function(r) {
        
        get_input_value(
          search_id(
            "Did Discovery of Contraband Result in Arrest?",
            row_name,
            r
          )
        )
        
      }
      
    )
    
  }
  
  
  # ==========================================================
  # COMBINED TABLE
  # ==========================================================
  
  output$combined_table <- renderUI({
    
    header <- tags$thead(
      
      tags$tr(
        
        tags$th(
          "Traffic Stop / Search Information"
        ),
        
        lapply(
          race_cols,
          tags$th
        ),
        
        tags$th(
          "Total"
        )
        
      )
      
    )
    
    
    body_rows <- list()
    
    
    # --------------------------------------------------------
    # TRAFFIC STOP ROWS
    # --------------------------------------------------------
    
    for (row_name in traffic_rows) {
      
      if (
        row_name %in% traffic_section_headers
      ) {
        
        body_rows[[
          length(body_rows) + 1
        ]] <- tags$tr(
          
          class = "section-row",
          
          tags$td(
            colspan = 7,
            row_name
          )
          
        )
        
        next
        
      }
      
      
      safe_name <- clean_id(row_name)
      
      cells <- list()
      
      
      cells[[1]] <- tags$td(
        
        class = ifelse(
          row_name == "Number of Stops",
          "",
          "subcategory"
        ),
        
        row_name
        
      )
      
      
      for (r in seq_along(race_cols)) {
        
        input_id <-
          paste0(
            "t1_",
            safe_name,
            "_",
            r
          )
        
        
        cells[[length(cells) + 1]] <-
          tags$td(
            
            numericInput(
              
              inputId = input_id,
              
              label = NULL,
              
              value = NULL,
              
              min = 0,
              
              step = 1,
              
              width = "100%"
              
            )
            
          )
        
      }
      
      
      cells[[length(cells) + 1]] <-
        tags$td(
          
          class = "total-cell",
          
          textOutput(
            paste0(
              "total_",
              safe_name
            ),
            inline = TRUE
          )
          
        )
      
      
      body_rows[[
        length(body_rows) + 1
      ]] <- tags$tr(cells)
      
    }
    
    
    # --------------------------------------------------------
    # SEARCH SECTIONS
    # --------------------------------------------------------
    
    for (section in search_sections) {
      
      body_rows[[
        length(body_rows) + 1
      ]] <- tags$tr(
        
        class = "section-row",
        
        tags$td(
          colspan = 7,
          section$title
        )
        
      )
      
      
      for (row_name in section$rows) {
        
        cells <- list()
        
        
        cells[[1]] <-
          tags$td(
            class = "subcategory",
            row_name
          )
        
        
        for (r in seq_along(race_cols)) {
          
          input_id <-
            search_id(
              section$title,
              row_name,
              r
            )
          
          
          cells[[length(cells) + 1]] <-
            tags$td(
              
              numericInput(
                
                inputId = input_id,
                
                label = NULL,
                
                value = NULL,
                
                min = 0,
                
                step = 1,
                
                width = "100%"
                
              )
              
            )
          
        }
        
        
        total_id <-
          paste0(
            "search_total_",
            clean_id(section$title),
            "_",
            clean_id(row_name)
          )
        
        
        cells[[length(cells) + 1]] <-
          tags$td(
            
            class = "total-cell",
            
            textOutput(
              total_id,
              inline = TRUE
            )
            
          )
        
        
        body_rows[[
          length(body_rows) + 1
        ]] <- tags$tr(cells)
        
      }
      
    }
    
    
    tags$table(
      
      class = "data-table",
      
      header,
      
      tags$tbody(body_rows)
      
    )
    
  })
  
  
  # ==========================================================
  # TRAFFIC ROW TOTALS
  # ==========================================================
  
  observe({
    
    for (row_name in traffic_rows) {
      
      if (
        row_name %in% traffic_section_headers
      ) {
        
        next
        
      }
      
      
      local({
        
        current_row <- row_name
        
        output_id <-
          paste0(
            "total_",
            clean_id(current_row)
          )
        
        
        output[[output_id]] <-
          renderText({
            
            values <-
              get_row_values(
                "t1_",
                current_row
              )
            
            
            if (
              all(is.na(values))
            ) {
              
              return("")
              
            }
            
            
            comma(
              sum(
                values,
                na.rm = TRUE
              )
            )
            
          })
        
      })
      
    }
    
  })
  
  
  # ==========================================================
  # SEARCH ROW TOTALS
  # ==========================================================
  
  observe({
    
    for (section in search_sections) {
      
      for (row_name in section$rows) {
        
        local({
          
          current_section <-
            section$title
          
          current_row <-
            row_name
          
          output_id <-
            paste0(
              "search_total_",
              clean_id(current_section),
              "_",
              clean_id(current_row)
            )
          
          
          output[[output_id]] <-
            renderText({
              
              values <-
                sapply(
                  
                  seq_along(race_cols),
                  
                  function(r) {
                    
                    get_input_value(
                      search_id(
                        current_section,
                        current_row,
                        r
                      )
                    )
                    
                  }
                  
                )
              
              
              if (
                all(is.na(values))
              ) {
                
                return("")
                
              }
              
              
              comma(
                sum(
                  values,
                  na.rm = TRUE
                )
              )
              
            })
          
        })
        
      }
      
    }
    
  })
  
  
  # ==========================================================
  # VALIDATION
  # ==========================================================
  
  validation_results <- reactive({
    
    req(check_requested())
    
    results <- list()
    
    
    # --------------------------------------------------------
    # STOP COUNTS
    # --------------------------------------------------------
    
    stop_counts <-
      get_stop_counts()
    
    
    missing_stop_groups <-
      race_cols[
        is.na(stop_counts)
      ]
    
    
    if (
      length(missing_stop_groups) > 0
    ) {
      
      results[[
        length(results) + 1
      ]] <- list(
        
        label = "Number of Stops",
        
        valid = FALSE,
        
        message =
          paste0(
            "Enter the Number of Stops for: ",
            paste(
              missing_stop_groups,
              collapse = ", "
            ),
            ". Each race/ethnicity category must have a value."
          )
        
      )
      
    }
    
    
    stop_counts_clean <-
      ifelse(
        is.na(stop_counts),
        0,
        stop_counts
      )
    
    
    total_stops <-
      sum(
        stop_counts_clean
      )
    
    
    # --------------------------------------------------------
    # TRAFFIC STOP VALIDATION GROUPS
    # --------------------------------------------------------
    
    for (
      section_name in names(traffic_validation_map)
    ) {
      
      child_rows <-
        traffic_validation_map[[section_name]]
      
      
      section_values <-
        matrix(
          0,
          nrow = length(race_cols),
          ncol = 1
        )
      
      
      section_has_data <-
        rep(
          FALSE,
          length(race_cols)
        )
      
      
      for (
        child_row in child_rows
      ) {
        
        values <-
          get_row_values(
            "t1_",
            child_row
          )
        
        
        section_has_data <-
          section_has_data |
          !is.na(values)
        
        
        values[
          is.na(values)
        ] <- 0
        
        
        section_values[, 1] <-
          section_values[, 1] +
          values
        
      }
      
      
      missing_categories <-
        race_cols[
          !section_has_data
        ]
      
      
      if (
        length(missing_categories) > 0
      ) {
        
        results[[
          length(results) + 1
        ]] <- list(
          
          label = section_name,
          
          valid = FALSE,
          
          message =
            paste0(
              "Enter all required ",
              section_name,
              " values for: ",
              paste(
                missing_categories,
                collapse = ", "
              ),
              ". The ",
              section_name,
              " subtotal must account for every reported stop."
            )
          
        )
        
      }
      
      
      mismatches <- character(0)
      
      
      for (
        r in seq_along(race_cols)
      ) {
        
        if (
          !is.na(stop_counts[r]) &&
          section_has_data[r]
        ) {
          
          if (
            section_values[r, 1] !=
            stop_counts[r]
          ) {
            
            mismatches <-
              c(
                mismatches,
                paste0(
                  race_cols[r],
                  ": ",
                  comma(
                    section_values[r, 1]
                  ),
                  " reported for ",
                  section_name,
                  "; ",
                  comma(
                    stop_counts[r]
                  ),
                  " stops are reported. Check the ",
                  section_name,
                  " entries."
                )
              )
            
          }
          
        }
        
      }
      
      
      if (
        length(mismatches) > 0
      ) {
        
        results[[
          length(results) + 1
        ]] <- list(
          
          label = section_name,
          
          valid = FALSE,
          
          message =
            paste(
              mismatches,
              collapse = " "
            )
          
        )
        
      } else if (
        length(missing_categories) == 0
      ) {
        
        results[[
          length(results) + 1
        ]] <- list(
          
          label = section_name,
          
          valid = TRUE,
          
          message =
            "All race/ethnicity values add to the reported Number of Stops."
          
        )
        
      }
      
    }
    
    
    # --------------------------------------------------------
    # SEARCH VALIDATION
    # --------------------------------------------------------
    
    search_yes <-
      get_search_conducted_yes()
    
    search_no <-
      get_search_conducted_no()
    
    
    search_conducted <-
      ifelse(
        is.na(search_yes),
        0,
        search_yes
      ) +
      ifelse(
        is.na(search_no),
        0,
        search_no
      )
    
    
    missing_search_categories <-
      race_cols[
        is.na(search_yes) |
          is.na(search_no)
      ]
    
    
    if (
      length(missing_search_categories) > 0
    ) {
      
      results[[
        length(results) + 1
      ]] <- list(
        
        label = "Searches Conducted",
        
        valid = FALSE,
        
        message =
          paste0(
            "Enter both Search = Yes and Search = No for: ",
            paste(
              missing_search_categories,
              collapse = ", "
            ),
            ". Both values are required because Searches Conducted is calculated from them."
          )
        
      )
      
    }
    
    
    search_mismatches <- character(0)
    
    
    for (
      r in seq_along(race_cols)
    ) {
      
      if (
        !is.na(stop_counts[r]) &&
        !is.na(search_yes[r]) &&
        !is.na(search_no[r])
      ) {
        
        if (
          search_conducted[r] !=
          stop_counts[r]
        ) {
          
          search_mismatches <-
            c(
              search_mismatches,
              paste0(
                race_cols[r],
                ": Search = Yes + Search = No equals ",
                comma(
                  search_conducted[r]
                ),
                ", but Number of Stops is ",
                comma(
                  stop_counts[r]
                ),
                ". Check the two search entries."
              )
            )
          
        }
        
      }
      
    }
    
    
    if (
      length(search_mismatches) > 0
    ) {
      
      results[[
        length(results) + 1
      ]] <- list(
        
        label = "Searches Conducted",
        
        valid = FALSE,
        
        message =
          paste(
            search_mismatches,
            collapse = " "
          )
        
      )
      
    } else if (
      length(missing_search_categories) == 0
    ) {
      
      results[[
        length(results) + 1
      ]] <- list(
        
        label = "Searches Conducted",
        
        valid = TRUE,
        
        message =
          "Search = Yes + Search = No equals Number of Stops for every race/ethnicity category."
        
      )
      
    }
    
    
    # --------------------------------------------------------
    # CONTRABAND VALIDATION
    # --------------------------------------------------------
    
    contraband_yes <-
      get_contraband_discovered_yes()
    
    contraband_no <-
      get_contraband_discovered_no()
    
    
    contraband_total <-
      ifelse(
        is.na(contraband_yes),
        0,
        contraband_yes
      ) +
      ifelse(
        is.na(contraband_no),
        0,
        contraband_no
      )
    
    
    missing_contraband_categories <-
      race_cols[
        is.na(contraband_yes) |
          is.na(contraband_no)
      ]
    
    
    if (
      length(missing_contraband_categories) > 0
    ) {
      
      results[[
        length(results) + 1
      ]] <- list(
        
        label = "Contraband Discovered",
        
        valid = FALSE,
        
        message =
          paste0(
            "Enter both Contraband = Yes and Contraband = No for: ",
            paste(
              missing_contraband_categories,
              collapse = ", "
            ),
            ". Both values are required for each race/ethnicity category."
          )
        
      )
      
    }
    
    
    contraband_mismatches <- character(0)
    
    
    for (
      r in seq_along(race_cols)
    ) {
      
      if (
        !is.na(search_conducted[r]) &&
        !is.na(contraband_yes[r]) &&
        !is.na(contraband_no[r])
      ) {
        
        if (
          contraband_total[r] !=
          search_conducted[r]
        ) {
          
          contraband_mismatches <-
            c(
              contraband_mismatches,
              paste0(
                race_cols[r],
                ": Contraband = Yes + Contraband = No equals ",
                comma(
                  contraband_total[r]
                ),
                ", but Searches Conducted is ",
                comma(
                  search_conducted[r]
                ),
                ". Check the contraband Yes/No entries."
              )
            )
          
        }
        
      }
      
    }
    
    
    if (
      length(contraband_mismatches) > 0
    ) {
      
      results[[
        length(results) + 1
      ]] <- list(
        
        label = "Contraband Discovered",
        
        valid = FALSE,
        
        message =
          paste(
            contraband_mismatches,
            collapse = " "
          )
        
      )
      
    } else if (
      length(missing_contraband_categories) == 0
    ) {
      
      results[[
        length(results) + 1
      ]] <- list(
        
        label = "Contraband Discovered",
        
        valid = TRUE,
        
        message =
          "Contraband = Yes + Contraband = No equals Searches Conducted for every race/ethnicity category."
        
      )
      
    }
    
    
    # --------------------------------------------------------
    # CONTRABAND DESCRIPTION
    # --------------------------------------------------------
    
    description_total <-
      rep(
        0,
        length(race_cols)
      )
    
    
    description_entered <-
      rep(
        FALSE,
        length(race_cols)
      )
    
    
    for (
      row_name in c(
        "Drugs",
        "Weapons",
        "Currency",
        "Alcohol",
        "Stolen Property",
        "Other"
      )
    ) {
      
      values <-
        get_contraband_description(
          row_name
        )
      
      
      description_entered <-
        description_entered |
        !is.na(values)
      
      
      values[
        is.na(values)
      ] <- 0
      
      
      description_total <-
        description_total +
        values
      
    }
    
    
    missing_description_categories <-
      race_cols[
        is.na(contraband_yes) |
          (
            contraband_yes > 0 &
              !description_entered
          )
      ]
    
    
    description_mismatches <-
      character(0)
    
    
    for (
      r in seq_along(race_cols)
    ) {
      
      if (
        !is.na(contraband_yes[r]) &&
        description_entered[r]
      ) {
        
        if (
          description_total[r] !=
          contraband_yes[r]
        ) {
          
          description_mismatches <-
            c(
              description_mismatches,
              paste0(
                race_cols[r],
                ": contraband descriptions total ",
                comma(
                  description_total[r]
                ),
                ", but Contraband Discovered = Yes is ",
                comma(
                  contraband_yes[r]
                ),
                ". Check the description categories."
              )
            )
          
        }
        
      }
      
    }
    
    
    if (
      length(missing_description_categories) > 0
    ) {
      
      results[[
        length(results) + 1
      ]] <- list(
        
        label = "Description of Contraband",
        
        valid = FALSE,
        
        message =
          paste0(
            "Enter contraband description counts for: ",
            paste(
              missing_description_categories,
              collapse = ", "
            ),
            ". A description is required when Contraband Discovered = Yes is greater than zero."
          )
        
      )
      
    } else if (
      length(description_mismatches) > 0
    ) {
      
      results[[
        length(results) + 1
      ]] <- list(
        
        label = "Description of Contraband",
        
        valid = FALSE,
        
        message =
          paste(
            description_mismatches,
            collapse = " "
          )
        
      )
      
    } else {
      
      results[[
        length(results) + 1
      ]] <- list(
        
        label = "Description of Contraband",
        
        valid = TRUE,
        
        message =
          "Contraband description totals match Contraband Discovered = Yes."
        
      )
      
    }
    
    
    # --------------------------------------------------------
    # CONTRABAND ARREST VALIDATION
    # --------------------------------------------------------
    
    arrest_yes <-
      get_contraband_arrest("Yes")
    
    arrest_no <-
      get_contraband_arrest("No")
    
    
    arrest_total <-
      ifelse(
        is.na(arrest_yes),
        0,
        arrest_yes
      ) +
      ifelse(
        is.na(arrest_no),
        0,
        arrest_no
      )
    
    
    missing_arrest_categories <-
      race_cols[
        is.na(arrest_yes) |
          is.na(arrest_no)
      ]
    
    
    arrest_mismatches <-
      character(0)
    
    
    for (
      r in seq_along(race_cols)
    ) {
      
      if (
        !is.na(contraband_yes[r]) &&
        !is.na(arrest_yes[r]) &&
        !is.na(arrest_no[r])
      ) {
        
        if (
          arrest_total[r] !=
          contraband_yes[r]
        ) {
          
          arrest_mismatches <-
            c(
              arrest_mismatches,
              paste0(
                race_cols[r],
                ": Arrest = Yes + Arrest = No equals ",
                comma(
                  arrest_total[r]
                ),
                ", but Contraband Discovered = Yes is ",
                comma(
                  contraband_yes[r]
                ),
                ". Check the arrest Yes/No entries."
              )
            )
          
        }
        
      }
      
    }
    
    
    if (
      length(missing_arrest_categories) > 0
    ) {
      
      results[[
        length(results) + 1
      ]] <- list(
        
        label = "Contraband Resulting in Arrest",
        
        valid = FALSE,
        
        message =
          paste0(
            "Enter both Arrest = Yes and Arrest = No for: ",
            paste(
              missing_arrest_categories,
              collapse = ", "
            ),
            ". Both values are required for each race/ethnicity category."
          )
        
      )
      
    } else if (
      length(arrest_mismatches) > 0
    ) {
      
      results[[
        length(results) + 1
      ]] <- list(
        
        label = "Contraband Resulting in Arrest",
        
        valid = FALSE,
        
        message =
          paste(
            arrest_mismatches,
            collapse = " "
          )
        
      )
      
    } else {
      
      results[[
        length(results) + 1
      ]] <- list(
        
        label = "Contraband Resulting in Arrest",
        
        valid = TRUE,
        
        message =
          "Arrest = Yes + Arrest = No equals Contraband Discovered = Yes for every race/ethnicity category."
        
      )
      
    }
    
    
    # --------------------------------------------------------
    # OVERALL TOTALS
    # --------------------------------------------------------
    
    total_check_messages <-
      character(0)
    
    
    total_stop_count <-
      sum(
        stop_counts_clean
      )
    
    
    total_search_count <-
      sum(
        search_conducted
      )
    
    
    if (
      total_search_count !=
      total_stop_count
    ) {
      
      total_check_messages <-
        c(
          total_check_messages,
          paste0(
            "Total Searches Conducted (",
            comma(total_search_count),
            ") does not match Total Number of Stops (",
            comma(total_stop_count),
            ")."
          )
        )
      
    }
    
    
    total_contraband_count <-
      sum(
        contraband_total
      )
    
    
    if (
      total_contraband_count !=
      total_search_count
    ) {
      
      total_check_messages <-
        c(
          total_check_messages,
          paste0(
            "Total Contraband Outcomes (",
            comma(total_contraband_count),
            ") does not match Total Searches Conducted (",
            comma(total_search_count),
            ")."
          )
        )
      
    }
    
    
    total_description_count <-
      sum(
        description_total
      )
    
    
    total_contraband_yes <-
      sum(
        contraband_yes,
        na.rm = TRUE
      )
    
    
    if (
      total_description_count !=
      total_contraband_yes
    ) {
      
      total_check_messages <-
        c(
          total_check_messages,
          paste0(
            "Total Contraband Descriptions (",
            comma(total_description_count),
            ") does not match Total Contraband Discovered = Yes (",
            comma(total_contraband_yes),
            ")."
          )
        )
      
    }
    
    
    total_arrest_count <-
      sum(
        arrest_total
      )
    
    
    if (
      total_arrest_count !=
      total_contraband_yes
    ) {
      
      total_check_messages <-
        c(
          total_check_messages,
          paste0(
            "Total Arrest Outcomes (",
            comma(total_arrest_count),
            ") does not match Total Contraband Discovered = Yes (",
            comma(total_contraband_yes),
            ")."
          )
        )
      
    }
    
    
    if (
      length(total_check_messages) > 0
    ) {
      
      results[[
        length(results) + 1
      ]] <- list(
        
        label = "Overall Totals",
        
        valid = FALSE,
        
        message =
          paste(
            total_check_messages,
            collapse = " "
          )
        
      )
      
    } else {
      
      results[[
        length(results) + 1
      ]] <- list(
        
        label = "Overall Totals",
        
        valid = TRUE,
        
        message =
          "All overall totals are internally consistent."
        
      )
      
    }
    
    
    results
    
  })
  
  
  # ==========================================================
  # VALIDATION CELL IDS
  #
  # This is a presentation layer only.
  # It does not change the validation calculations.
  # ==========================================================
  
  validation_missing_ids <- reactive({
    
    req(check_requested())
    
    ids <- character(0)
    
    
    # --------------------------------------------------------
    # NUMBER OF STOPS
    # --------------------------------------------------------
    
    stop_counts <-
      get_stop_counts()
    
    
    for (r in seq_along(race_cols)) {
      
      if (is.na(stop_counts[r])) {
        
        ids <- c(
          ids,
          paste0(
            "t1_number_of_stops_",
            r
          )
        )
        
      }
      
    }
    
    
    # --------------------------------------------------------
    # TRAFFIC STOP GROUPS
    # --------------------------------------------------------
    
    for (
      section_name in names(traffic_validation_map)
    ) {
      
      child_rows <-
        traffic_validation_map[[section_name]]
      
      
      section_values <-
        matrix(
          0,
          nrow = length(race_cols),
          ncol = 1
        )
      
      
      section_has_data <-
        rep(
          FALSE,
          length(race_cols)
        )
      
      
      for (
        child_row in child_rows
      ) {
        
        values <-
          get_row_values(
            "t1_",
            child_row
          )
        
        
        section_has_data <-
          section_has_data |
          !is.na(values)
        
        
        values[
          is.na(values)
        ] <- 0
        
        
        section_values[, 1] <-
          section_values[, 1] +
          values
        
      }
      
      
      for (r in seq_along(race_cols)) {
        
        if (!section_has_data[r]) {
          
          for (child_row in child_rows) {
            
            ids <- c(
              ids,
              paste0(
                "t1_",
                clean_id(child_row),
                "_",
                r
              )
            )
            
          }
          
        } else if (
          !is.na(stop_counts[r]) &&
          section_values[r, 1] != stop_counts[r]
        ) {
          
          for (child_row in child_rows) {
            
            value <-
              get_input_value(
                paste0(
                  "t1_",
                  clean_id(child_row),
                  "_",
                  r
                )
              )
            
            if (!is.na(value)) {
              
              ids <- c(
                ids,
                paste0(
                  "t1_",
                  clean_id(child_row),
                  "_",
                  r
                )
              )
              
            }
            
          }
          
        }
        
      }
      
    }
    
    
    # --------------------------------------------------------
    # SEARCHES
    # --------------------------------------------------------
    
    search_yes <-
      get_search_conducted_yes()
    
    search_no <-
      get_search_conducted_no()
    
    stop_counts <-
      get_stop_counts()
    
    
    for (r in seq_along(race_cols)) {
      
      if (is.na(search_yes[r])) {
        
        ids <- c(
          ids,
          search_id(
            "Searches Conducted",
            "Yes",
            r
          )
        )
        
      }
      
      if (is.na(search_no[r])) {
        
        ids <- c(
          ids,
          search_id(
            "Searches Conducted",
            "No",
            r
          )
        )
        
      }
      
      
      if (
        !is.na(search_yes[r]) &&
        !is.na(search_no[r]) &&
        !is.na(stop_counts[r])
      ) {
        
        search_total <-
          search_yes[r] +
          search_no[r]
        
        
        if (
          search_total != stop_counts[r]
        ) {
          
          ids <- c(
            ids,
            search_id(
              "Searches Conducted",
              "Yes",
              r
            ),
            search_id(
              "Searches Conducted",
              "No",
              r
            )
          )
          
        }
        
      }
      
    }
    
    
    # --------------------------------------------------------
    # CONTRABAND
    # --------------------------------------------------------
    
    contraband_yes <-
      get_contraband_discovered_yes()
    
    contraband_no <-
      get_contraband_discovered_no()
    
    
    search_conducted <-
      get_search_conducted()
    
    
    for (r in seq_along(race_cols)) {
      
      if (is.na(contraband_yes[r])) {
        
        ids <- c(
          ids,
          search_id(
            "Was Contraband Discovered?",
            "Yes",
            r
          )
        )
        
      }
      
      if (is.na(contraband_no[r])) {
        
        ids <- c(
          ids,
          search_id(
            "Was Contraband Discovered?",
            "No",
            r
          )
        )
        
      }
      
      
      if (
        !is.na(contraband_yes[r]) &&
        !is.na(contraband_no[r]) &&
        !is.na(search_conducted[r])
      ) {
        
        contraband_total <-
          contraband_yes[r] +
          contraband_no[r]
        
        
        if (
          contraband_total !=
          search_conducted[r]
        ) {
          
          ids <- c(
            ids,
            search_id(
              "Was Contraband Discovered?",
              "Yes",
              r
            ),
            search_id(
              "Was Contraband Discovered?",
              "No",
              r
            )
          )
          
        }
        
      }
      
    }
    
    
    # --------------------------------------------------------
    # CONTRABAND DESCRIPTION
    # --------------------------------------------------------
    
    description_entered <-
      rep(
        FALSE,
        length(race_cols)
      )
    
    
    description_total <-
      rep(
        0,
        length(race_cols)
      )
    
    
    description_rows <- c(
      "Drugs",
      "Weapons",
      "Currency",
      "Alcohol",
      "Stolen Property",
      "Other"
    )
    
    
    for (row_name in description_rows) {
      
      values <-
        get_contraband_description(
          row_name
        )
      
      
      description_entered <-
        description_entered |
        !is.na(values)
      
      
      values[
        is.na(values)
      ] <- 0
      
      
      description_total <-
        description_total +
        values
      
    }
    
    
    for (r in seq_along(race_cols)) {
      
      if (
        !is.na(contraband_yes[r]) &&
        contraband_yes[r] > 0 &&
        !description_entered[r]
      ) {
        
        for (row_name in description_rows) {
          
          ids <- c(
            ids,
            search_id(
              "Description of Contraband",
              row_name,
              r
            )
          )
          
        }
        
      } else if (
        !is.na(contraband_yes[r]) &&
        description_entered[r] &&
        description_total[r] != contraband_yes[r]
      ) {
        
        for (row_name in description_rows) {
          
          value <-
            get_input_value(
              search_id(
                "Description of Contraband",
                row_name,
                r
              )
            )
          
          if (!is.na(value)) {
            
            ids <- c(
              ids,
              search_id(
                "Description of Contraband",
                row_name,
                r
              )
            )
            
          }
          
        }
        
      }
      
    }
    
    
    # --------------------------------------------------------
    # CONTRABAND ARREST
    # --------------------------------------------------------
    
    arrest_yes <-
      get_contraband_arrest("Yes")
    
    arrest_no <-
      get_contraband_arrest("No")
    
    
    for (r in seq_along(race_cols)) {
      
      if (is.na(arrest_yes[r])) {
        
        ids <- c(
          ids,
          search_id(
            "Did Discovery of Contraband Result in Arrest?",
            "Yes",
            r
          )
        )
        
      }
      
      if (is.na(arrest_no[r])) {
        
        ids <- c(
          ids,
          search_id(
            "Did Discovery of Contraband Result in Arrest?",
            "No",
            r
          )
        )
        
      }
      
      
      if (
        !is.na(arrest_yes[r]) &&
        !is.na(arrest_no[r]) &&
        !is.na(contraband_yes[r])
      ) {
        
        arrest_total <-
          arrest_yes[r] +
          arrest_no[r]
        
        
        if (
          arrest_total !=
          contraband_yes[r]
        ) {
          
          ids <- c(
            ids,
            search_id(
              "Did Discovery of Contraband Result in Arrest?",
              "Yes",
              r
            ),
            search_id(
              "Did Discovery of Contraband Result in Arrest?",
              "No",
              r
            )
          )
          
        }
        
      }
      
    }
    
    
    unique(ids)
    
  })
  
  
  # ==========================================================
  # SEND VALIDATION CELL HIGHLIGHTS
  # ==========================================================
  
  observe({
    
    req(check_requested())
    
    ids <-
      validation_missing_ids()
    
    session$sendCustomMessage(
      "validation_mark_cells",
      list(
        ids = ids
      )
    )
    
  })
  
  
  # ==========================================================
  # VALIDATION SUMMARY
  # ==========================================================
  
  output$validation_summary <- renderUI({
    
    if (
      !check_requested()
    ) {
      
      return(
        
        div(
          
          class = "status-note status-warning",
          
          tags$strong(
            "Validation has not been run yet."
          ),
          
          br(),
          
          "Enter your data on the Traffic Stops & Searches tab, then return here and select Run Validation."
          
        )
        
      )
      
    }
    
    
    results <-
      validation_results()
    
    
    all_valid <-
      all(
        sapply(
          results,
          function(x) x$valid
        )
      )
    
    
    rows <-
      lapply(
        results,
        function(x) {
          
          if (x$valid) {
            
            div(
              
              class = "validation-row",
              
              HTML(
                '<span class="validation-check">✓</span>'
              ),
              
              tags$strong(
                x$label
              ),
              
              " — ",
              
              x$message
              
            )
            
          } else {
            
            div(
              
              class = "validation-row",
              
              HTML(
                '<span class="validation-cross">✕</span>'
              ),
              
              tags$strong(
                x$label
              ),
              
              " — ",
              
              x$message
              
            )
            
          }
          
        }
      )
    
    
    if (all_valid) {
      
      div(
        
        class =
          "status-note status-complete validation-summary",
        
        div(
          class = "validation-header",
          "Validation successful"
        ),
        
        "All required fields are complete and the reported totals are internally consistent.",
        
        br(),
        br(),
        
        "This check confirms internal consistency within the data entered into this application. You should still review the source records and applicable state reporting requirements before submission.",
        
        br(),
        br(),
        
        rows
        
      )
      
    } else {
      
      div(
        
        class =
          "status-note status-error validation-summary",
        
        div(
          class = "validation-header",
          "Action required before submission"
        ),
        
        "One or more required fields or totals need attention. Review the items below, correct the data on the Traffic Stops & Searches tab, and run validation again.",
        
        br(),
        br(),
        
        rows
        
      )
      
    }
    
  })
  
  
  # ==========================================================
  # CHECK BUTTON
  # ==========================================================
  
  observeEvent(
    
    input$check_all_totals,
    
    {
      
      check_requested(TRUE)
      
    }
    
  )
  
  
  # ==========================================================
  # RESET DATA
  # ==========================================================
  
  observeEvent(
    
    input$reset_all_data,
    
    {
      
      check_requested(FALSE)
      
      
      session$sendCustomMessage(
        "validation_mark_cells",
        list(
          ids = character(0)
        )
      )
      
      
      for (
        row_name in traffic_rows
      ) {
        
        if (
          row_name %in%
          traffic_section_headers
        ) {
          
          next
          
        }
        
        
        safe_name <-
          clean_id(row_name)
        
        
        for (
          r in seq_along(race_cols)
        ) {
          
          updateNumericInput(
            
            session = session,
            
            inputId =
              paste0(
                "t1_",
                safe_name,
                "_",
                r
              ),
            
            value = NA
            
          )
          
        }
        
      }
      
      
      for (
        section in search_sections
      ) {
        
        for (
          row_name in section$rows
        ) {
          
          for (
            r in seq_along(race_cols)
          ) {
            
            updateNumericInput(
              
              session = session,
              
              inputId =
                search_id(
                  section$title,
                  row_name,
                  r
                ),
              
              value = NA
              
            )
            
          }
          
        }
        
      }
      
    }
    
  )
  
  
  # ==========================================================
  # DOWNLOAD DATA
  # ==========================================================
  
  output$download_data <- downloadHandler(
    
    filename = function() {
      
      paste0(
        "Traffic_Stop_Racial_Profiling_Data_",
        format(
          Sys.Date(),
          "%Y%m%d"
        ),
        ".csv"
      )
      
    },
    
    contentType =
      "text/csv; charset=UTF-8",
    
    content = function(file) {
      
      rows <- list()
      
      
      for (
        row_name in traffic_rows
      ) {
        
        if (
          row_name %in%
          traffic_section_headers
        ) {
          
          next
          
        }
        
        
        values <-
          get_row_values(
            "t1_",
            row_name
          )
        
        
        rows[[
          length(rows) + 1
        ]] <-
          data.frame(
            
            Section =
              ifelse(
                row_name == "Number of Stops",
                "Traffic Stops",
                NA_character_
              ),
            
            Variable =
              row_name,
            
            White =
              values[1],
            
            Black =
              values[2],
            
            Hispanic_Latino =
              values[3],
            
            Asian_Pacific_Islander =
              values[4],
            
            Alaska_Native_American_Indian =
              values[5],
            
            stringsAsFactors = FALSE
            
          )
        
      }
      
      
      for (
        section in search_sections
      ) {
        
        for (
          row_name in section$rows
        ) {
          
          values <-
            sapply(
              seq_along(race_cols),
              function(r) {
                
                get_input_value(
                  search_id(
                    section$title,
                    row_name,
                    r
                  )
                )
                
              }
            )
          
          
          rows[[
            length(rows) + 1
          ]] <-
            data.frame(
              
              Section =
                section$title,
              
              Variable =
                row_name,
              
              White =
                values[1],
              
              Black =
                values[2],
              
              Hispanic_Latino =
                values[3],
              
              Asian_Pacific_Islander =
                values[4],
              
              Alaska_Native_American_Indian =
                values[5],
              
              stringsAsFactors = FALSE
              
            )
          
        }
        
      }
      
      
      export_data <-
        do.call(
          rbind,
          rows
        )
      
      
      utils::write.csv(
        export_data,
        file = file,
        row.names = FALSE,
        na = ""
      )
      
    }
    
  )
  
  
  # ==========================================================
  # CONTRABAND HIT RATES
  # ==========================================================
  
  output$hit_rate_output <- renderUI({
    
    dat <- hit_rate_data()
    
    total_searches <-
      attr(
        dat,
        "total_searches"
      )
    
    total_hits <-
      attr(
        dat,
        "total_hits"
      )
    
    overall_rate <-
      attr(
        dat,
        "overall_rate"
      )
    
    
    # --------------------------------------------------------
    # NO SEARCHES YET
    # --------------------------------------------------------
    
    if (
      is.na(total_searches) ||
      total_searches == 0
    ) {
      
      return(
        
        div(
          
          class = "hit-rate-card",
          
          div(
            class = "hit-rate-title",
            "Contraband Hit Rates"
          ),
          
          div(
            class = "hit-rate-subtitle",
            "Hit rate = searches in which contraband was discovered ÷ total searches conducted."
          ),
          
          div(
            
            class = "status-note status-warning",
            
            tags$strong(
              "Hit rates are not available yet."
            ),
            
            br(),
            
            "Enter Search = Yes and Search = No counts to calculate Searches Conducted."
            
          )
          
        )
        
      )
      
    }
    
    
    # --------------------------------------------------------
    # RACE / ETHNICITY RATE ROWS
    # --------------------------------------------------------
    
    rate_rows <-
      
      lapply(
        
        seq_len(nrow(dat)),
        
        function(i) {
          
          rate <- dat$Hit_Rate[i]
          
          
          rate_display <-
            
            if (
              is.na(rate)
            ) {
              
              "—"
              
            } else {
              
              paste0(
                sprintf(
                  "%.1f",
                  rate
                ),
                "%"
              )
              
            }
          
          
          tags$tr(
            
            tags$td(
              dat$Group[i]
            ),
            
            tags$td(
              
              comma(
                dat$Searches[i]
              ),
              
              class = "numeric"
              
            ),
            
            tags$td(
              
              comma(
                dat$Contraband_Found[i]
              ),
              
              class = "numeric"
              
            ),
            
            tags$td(
              
              rate_display,
              
              class = "numeric"
              
            )
            
          )
          
        }
        
      )
    
    
    # --------------------------------------------------------
    # HIT-RATE CARD
    # --------------------------------------------------------
    
    div(
      
      class = "hit-rate-card",
      
      div(
        class = "hit-rate-title",
        "Contraband Hit Rates"
      ),
      
      div(
        class = "hit-rate-subtitle",
        "A hit rate is the percentage of searches in which contraband was discovered."
      ),
      
      div(
        
        class = "hit-rate-overall",
        
        div(
          class = "hit-rate-label",
          "Overall contraband hit rate"
        ),
        
        div(
          
          class = "hit-rate-number",
          
          paste0(
            sprintf(
              "%.1f",
              overall_rate
            ),
            "%"
          )
          
        ),
        
        div(
          
          class = "hit-rate-label",
          
          paste0(
            comma(total_hits),
            " of ",
            comma(total_searches),
            " searches resulted in contraband discovery."
          )
          
        )
        
      ),
      
      tags$table(
        
        class = "rate-table",
        
        tags$thead(
          
          tags$tr(
            
            tags$th(
              "Race / Ethnicity"
            ),
            
            tags$th(
              "Searches Conducted"
            ),
            
            tags$th(
              "Contraband Found"
            ),
            
            tags$th(
              "Hit Rate"
            )
            
          )
          
        ),
        
        tags$tbody(
          rate_rows
        )
        
      ),
      
      div(
        
        class = "footnote",
        
        "Hit rate is calculated separately for each race/ethnicity category as Contraband Discovered = Yes divided by Searches Conducted. When no searches are reported for a category, the hit rate is displayed as — rather than treating it as zero."
        
      )
      
    )
    
  })
  
  # ==========================================================
  # KEEP HIT-RATE OUTPUT REACTIVE ACROSS TAB CHANGES
  # ==========================================================
  
  outputOptions(
    output,
    "hit_rate_output",
    suspendWhenHidden = FALSE
  )
  
  # ==========================================================
  # SELECTED JURISDICTION
  # ==========================================================
  
  selected_jurisdiction <- reactive({
    
    req(
      input$location_select
    )
    
    
    selected_id <-
      as.character(
        input$location_select
      )
    
    
    result <-
      texas_geographies[
        texas_geographies$Geography_ID ==
          selected_id,
      ]
    
    
    if (
      nrow(result) != 1
    ) {
      
      return(NULL)
      
    }
    
    
    result
    
  })
  
  
  # ==========================================================
  # JURISDICTION INFO DISPLAY
  # ==========================================================
  
  output$selected_jurisdiction_info <- renderUI({
    
    dat <-
      selected_jurisdiction()
    
    
    if (
      is.null(dat)
    ) {
      
      return(NULL)
      
    }
    
    
    div(
      
      class = "jurisdiction-info",
      
      tags$strong(
        dat$Geography_Name[1]
      ),
      
      " | ",
      
      dat$Geography_Type[1],
      
      " | Census ID: ",
      
      dat$Geography_ID[1],
      
      " | 2020 Adult Pop (18+): ",
      
      comma(
        dat$Population_18Plus[1]
      )
      
    )
    
  })
  
  
  # ==========================================================
  # POPULATION COMPARISON
  # ==========================================================
  
  comparison_data <- eventReactive(
    
    input$generate,
    
    {
      
      if (
        input$comparison_source == "stops"
      ) {
        
        counts <-
          get_stop_counts()
        
        source_label <-
          "Traffic Stops"
        
      } else {
        
        counts <-
          get_search_conducted()
        
        source_label <-
          "Searches"
        
      }
      
      
      if (
        all(is.na(counts))
      ) {
        
        showNotification(
          
          paste0(
            "Enter ",
            tolower(source_label),
            " data before generating the comparison."
          ),
          
          type = "warning",
          
          duration = 5
          
        )
        
        return(NULL)
        
      }
      
      
      counts[
        is.na(counts)
      ] <- 0
      
      
      total_source <-
        sum(counts)
      
      
      if (
        total_source == 0
      ) {
        
        showNotification(
          
          paste0(
            "Enter ",
            tolower(source_label),
            " data before generating the comparison."
          ),
          
          type = "warning",
          
          duration = 5
          
        )
        
        return(NULL)
        
      }
      
      
      jurisdiction_id <-
        input$location_select
      
      
      if (
        is.null(jurisdiction_id) ||
        length(jurisdiction_id) == 0 ||
        is.na(jurisdiction_id) ||
        jurisdiction_id == ""
      ) {
        
        showNotification(
          
          "Select a Texas county or place.",
          
          type = "error",
          
          duration = 5
          
        )
        
        return(NULL)
        
      }
      
      
      jurisdiction_info <-
        texas_geographies[
          texas_geographies$Geography_ID ==
            as.character(jurisdiction_id),
        ]
      
      
      if (
        nrow(jurisdiction_info) != 1
      ) {
        
        showNotification(
          
          "Select a valid Texas jurisdiction.",
          
          type = "error",
          
          duration = 5
          
        )
        
        return(NULL)
        
      }
      
      
      jurisdiction_population <-
        jurisdiction_info$Population_18Plus[1]
      
      
      # ----------------------------------------------------------
      # CENSUS DEMOGRAPHIC COUNTS
      # ----------------------------------------------------------
      
      pop_white <-
        as.numeric(
          jurisdiction_info$Not_Hispanic_White_18Plus[1]
        )
      
      pop_black <-
        as.numeric(
          jurisdiction_info$Not_Hispanic_Black_18Plus[1]
        )
      
      pop_hispanic <-
        as.numeric(
          jurisdiction_info$Hispanic_Latino_18Plus[1]
        )
      
      pop_asian <-
        as.numeric(
          jurisdiction_info$Not_Hispanic_Asian_18Plus[1]
        )
      
      pop_nhpi <-
        as.numeric(
          jurisdiction_info$Not_Hispanic_NHPI_18Plus[1]
        )
      
      pop_asian_pi <-
        pop_asian +
        pop_nhpi
      
      pop_aian <-
        as.numeric(
          jurisdiction_info$Not_Hispanic_AIAN_18Plus[1]
        )
      
      pop_other <-
        as.numeric(
          jurisdiction_info$Not_Hispanic_Other_Race_18Plus[1]
        )
      
      pop_two_plus <-
        as.numeric(
          jurisdiction_info$Not_Hispanic_TwoPlus_Races_18Plus[1]
        )
      
      
      pop_counts <-
        c(
          pop_white,
          pop_black,
          pop_hispanic,
          pop_asian_pi,
          pop_aian
        )
      
      
      pop_counts[
        is.na(pop_counts)
      ] <- 0
      
      # ----------------------------------------------------------
      # CENSUS POPULATION RECONCILIATION
      # ----------------------------------------------------------
      
      census_category_total <-
        pop_hispanic +
        pop_white +
        pop_black +
        pop_aian +
        pop_asian +
        pop_nhpi +
        pop_other +
        pop_two_plus
      
      census_residual <-
        jurisdiction_population -
        census_category_total
      
      
      # ----------------------------------------------------------
      # CENSUS PERCENTAGES
      #
      # Percentage of the entire 18+ adult population
      # ----------------------------------------------------------
      
      if (
        !is.na(jurisdiction_population) &&
        jurisdiction_population > 0
      ) {
        
        pop_percents <-
          (
            pop_counts /
              jurisdiction_population
          ) * 100
        
      } else {
        
        pop_percents <-
          rep(
            NA_real_,
            length(pop_counts)
          )
        
      }
      
      
      source_per_1000 <-
        ifelse(
          
          pop_counts > 0,
          
          (
            counts /
              pop_counts
          ) * 1000,
          
          0
          
        )
      
      
      result <-
        data.frame(
          
          Group =
            race_cols,
          
          Source_Count =
            counts,
          
          Source_Percent =
            (
              counts /
                total_source
            ) * 100,
          
          Population_Count =
            pop_counts,
          
          Population_Percent =
            pop_percents,
          
          Source_Per_1000 =
            source_per_1000,
          
          stringsAsFactors = FALSE
          
        )
      
      
      attr(
        result,
        "jurisdiction"
      ) <-
        jurisdiction_info$Display[1]
      
      attr(
        result,
        "census_category_total"
      ) <-
        census_category_total
      
      attr(
        result,
        "census_residual"
      ) <-
        census_residual
      
      attr(
        result,
        "geography_id"
      ) <-
        jurisdiction_info$Geography_ID[1]
      
      
      attr(
        result,
        "geography_type"
      ) <-
        jurisdiction_info$Geography_Type[1]
      
      
      attr(
        result,
        "jurisdiction_population"
      ) <-
        jurisdiction_population
      
      
      attr(
        result,
        "source_label"
      ) <-
        source_label
      
      
      result
      
    }
    
  )
  
  
  # ==========================================================
  # POPULATION STATUS
  # ==========================================================
  
  output$population_status <- renderUI({
    
    dat <-
      comparison_data()
    
    
    if (
      is.null(dat)
    ) {
      
      return(NULL)
      
    }
    
    
    div(
      
      class = "status-note status-complete",
      
      tags$strong(
        "Population benchmark generated."
      ),
      
      br(),
      
      "Compared: ",
      
      attr(
        dat,
        "source_label"
      ),
      
      br(),
      
      "Geographic benchmark: ",
      
      attr(
        dat,
        "jurisdiction"
      ),
      
      br(),
      
      "Total entered: ",
      
      comma(
        sum(dat$Source_Count)
      ),
      
      br(),
      
      "Jurisdiction Adult Population (18+): ",
      
      comma(
        attr(
          dat,
          "jurisdiction_population"
        )
      ),
      
      br(),
      
      "Source: ",
      
      tags$strong(
        "2020 Decennial Census, Ages 18 years and older (downloaded on 08/30/2026)"
      )
      
    )
    
  })
  
  
  # ==========================================================
  # BENCHMARK TABLE
  # ==========================================================
  
  output$benchmark_table <- renderUI({
    
    dat <-
      comparison_data()
    
    
    if (
      is.null(dat)
    ) {
      
      return(NULL)
      
    }
    
    
    jurisdiction <-
      attr(
        dat,
        "jurisdiction"
      )
    
    
    population_total <-
      attr(
        dat,
        "jurisdiction_population"
      )
    
    
    source_lbl <-
      attr(
        dat,
        "source_label"
      )
    
    
    table_output <-
      tags$table(
        
        class = "data-table",
        
        tags$thead(
          
          tags$tr(
            
            tags$th(
              "Demographic Group"
            ),
            
            tags$th(
              paste0(
                source_lbl,
                " Count"
              )
            ),
            
            tags$th(
              paste0(
                source_lbl,
                " %"
              )
            ),
            
            tags$th(
              "Census 18+ Pop Count"
            ),
            
            tags$th(
              "Census 18+ Population %"
            ),
            
            tags$th(
              paste0(
                source_lbl,
                " per 1,000 Group Residents"
              )
            )
            
          )
          
        ),
        
        tags$tbody(
          
          lapply(
            
            seq_len(nrow(dat)),
            
            function(i) {
              
              tags$tr(
                
                tags$td(
                  dat$Group[i]
                ),
                
                tags$td(
                  comma(
                    dat$Source_Count[i]
                  ),
                  style =
                    "text-align:right;"
                ),
                
                tags$td(
                  paste0(
                    sprintf(
                      "%.1f",
                      dat$Source_Percent[i]
                    ),
                    "%"
                  ),
                  style =
                    "text-align:right;"
                ),
                
                tags$td(
                  comma(
                    dat$Population_Count[i]
                  ),
                  style =
                    "text-align:right;"
                ),
                
                tags$td(
                  paste0(
                    sprintf(
                      "%.1f",
                      dat$Population_Percent[i]
                    ),
                    "%"
                  ),
                  style =
                    "text-align:right;"
                ),
                
                tags$td(
                  sprintf(
                    "%.2f",
                    dat$Source_Per_1000[i]
                  ),
                  style =
                    "text-align:right;"
                )
                
              )
              
            }
            
          )
          
        )
        
      )
    
    
    div(
      
      class = "benchmark-card",
      
      h4(
        paste0(
          "Population & Demographic Summary — ",
          jurisdiction
        ),
        style = "font-weight:700;"
      ),
      
      p(
        "Total jurisdiction adult population (18+): ",
        tags$strong(
          comma(population_total)
        )
      ),
      
      table_output,
      
      div(
        
        class = "footnote",
        
        "Population Data Source: 2020 Decennial Census Data (obtained on 08/30/2026)",
        "",
        
       "Source % shows the share of all traffic stops or searches represented by each group. Census 18+ Population % shows the share of the jurisdiction’s adult population (age 18+) represented by each group. Rate per 1,000 shows the number of traffic stops or searches for each group for every 1,000 adults in that group’s population."
        
      )
      
    )
    
  })
  
  
  # ==========================================================
  # CHART CONTAINER
  # ==========================================================
  
  output$chart_container <- renderUI({
    
    dat <-
      comparison_data()
    
    
    if (
      is.null(dat)
    ) {
      
      return(NULL)
      
    }
    
    
    div(
      
      class = "chart-card",
      
      plotlyOutput(
        "population_plot",
        height = "650px"
      )
      
    )
    
  })
  
  
  # ==========================================================
  # POPULATION CHART
  # ==========================================================
  
  output$population_plot <- renderPlotly({
    
    dat <-
      comparison_data()
    
    
    req(dat)
    
    
    source_label <-
      attr(
        dat,
        "source_label"
      )
    
    
    jurisdiction <-
      attr(
        dat,
        "jurisdiction"
      )
    
    
    jurisdiction_population <-
      attr(
        dat,
        "jurisdiction_population"
      )
    
    
    p <-
      plot_ly() %>%
      
      add_bars(
        
        data = dat,
        
        x = ~Group,
        
        y = ~Source_Percent,
        
        name = source_label,
        
        text =
          ~paste0(
            sprintf("%.1f", Source_Percent),
            "%"
          ),
        
        textposition = "outside",
        
        cliponaxis = FALSE,
        
        marker = list(
          color = EPCL_BURGUNDY
        ),
        
        hovertext =
          ~paste0(
            "<b>", Group, "</b>",
            "<br>",
            source_label,
            ": ",
            sprintf("%.1f", Source_Percent),
            "%",
            "<br>",
            "Count: ",
            format(
              Source_Count,
              big.mark = ",",
              scientific = FALSE
            ),
            "<br>",
            source_label,
            " per 1,000 residents: ",
            sprintf(
              "%.2f",
              Source_Per_1000
            )
          ),
        
        hovertemplate =
          "%{hovertext}<extra></extra>"
        
      ) %>%
      
      add_bars(
        
        data = dat,
        
        x = ~Group,
        
        y = ~Population_Percent,
        
        name = "Census 18+ Adult Pop %",
        
        text =
          ~paste0(
            sprintf(
              "%.1f",
              Population_Percent
            ),
            "%"
          ),
        
        textposition = "outside",
        
        cliponaxis = FALSE,
        
        marker = list(
          color = EPCL_CHART_GOLD
        ),
        
        hovertext =
          ~paste0(
            "<b>", Group, "</b>",
            "<br>",
            "Census 18+ Adult Population: ",
            sprintf(
              "%.1f",
              Population_Percent
            ),
            "%",
            "<br>",
            "Count: ",
            format(
              Population_Count,
              big.mark = ",",
              scientific = FALSE
            )
          ),
        
        hovertemplate =
          "%{hovertext}<extra></extra>"
        
      ) %>%
      
      layout(
        
        barmode = "group",
        
        title = list(
          
          text =
            paste0(
              
              "<b>",
              source_label,
              " vs. Census Adult Population Benchmark",
              "</b><br>",
              
              "<span style='font-size:15px;'>",
              
              jurisdiction,
              
              " | 18+ Adult Population: ",
              
              format(
                jurisdiction_population,
                big.mark = ",",
                scientific = FALSE
              ),
              
              "</span>"
              
            ),
          
          font = list(
            size = 20
          ),
          
          y = 0.96
          
        ),
        
        margin = list(
          t = 155,
          b = 125,
          l = 70,
          r = 40
        ),
        
        xaxis = list(
          title = "",
          tickangle = -25
        ),
        
        yaxis = list(
          
          title = "Percentage",
          
          range = c(
            0,
            100
          ),
          
          ticksuffix = "%",
          
          dtick = 10,
          
          fixedrange = TRUE
          
        ),
        
        legend = list(
          
          orientation = "h",
          
          x = 0.5,
          
          xanchor = "center",
          
          y = 1.035,
          
          yanchor = "bottom"
          
        ),
        
        hovermode = "closest"
        
      )
    
    p
    
  })
  
}


# ============================================================
# RUN APP
# ============================================================

shinyApp(
  ui = ui,
  server = server
)