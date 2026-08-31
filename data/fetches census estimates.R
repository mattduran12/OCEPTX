################################################################################
# RP ANALYZER
# 2020 DECENNIAL CENSUS
#
# TEXAS COUNTIES + CENSUS PLACES
#
# Population age 18+ by race/ethnicity
#
# Source:
# 2020 Decennial Census
# Redistricting Data (PL 94-171)
# Table P4
################################################################################


################################################################################
# 1. PACKAGES
################################################################################

packages <- c(
  "httr2",
  "jsonlite",
  "dplyr",
  "readr"
)

for (p in packages) {
  
  if (!requireNamespace(p, quietly = TRUE)) {
    install.packages(p)
  }
  
}

library(httr2)
library(jsonlite)
library(dplyr)
library(readr)


################################################################################
# 2. SETTINGS
################################################################################

CENSUS_YEAR <- 2020

STATE <- "48"

API_KEY <- "6bd53b7acd8614393fce1efb2470d0ce2684b2b6"


################################################################################
# 3. OUTPUT
################################################################################

DATA_DIR <- file.path(
  getwd(),
  "data"
)

if (!dir.exists(DATA_DIR)) {
  dir.create(
    DATA_DIR,
    recursive = TRUE
  )
}

OUTPUT_FILE <- file.path(
  DATA_DIR,
  "texas_census_2020_age18_race_ethnicity.csv"
)


################################################################################
# 4. P4 VARIABLES
################################################################################
#
# P4 =
#
# Hispanic or Latino, and Not Hispanic or Latino by Race
# for the Population 18 Years and Over
#
################################################################################

variables <- c(
  
  "NAME",
  
  # Total population 18+
  "P4_001N",
  
  # Hispanic or Latino
  "P4_002N",
  
  # Not Hispanic or Latino
  "P4_003N",
  
  # Not Hispanic - one race
  "P4_004N",
  
  # Not Hispanic - White alone
  "P4_005N",
  
  # Not Hispanic - Black or African American alone
  "P4_006N",
  
  # Not Hispanic - American Indian and Alaska Native alone
  "P4_007N",
  
  # Not Hispanic - Asian alone
  "P4_008N",
  
  # Not Hispanic - Native Hawaiian and Other Pacific Islander alone
  "P4_009N",
  
  # Not Hispanic - Some Other Race alone
  "P4_010N",
  
  # Not Hispanic - Two or more races
  "P4_011N"
  
)


################################################################################
# 5. FUNCTION TO DOWNLOAD CENSUS DATA
################################################################################

download_census <- function(
    geography
) {
  
  base_url <- paste0(
    "https://api.census.gov/data/",
    CENSUS_YEAR,
    "/dec/pl"
  )
  
  
  # Build URL manually.
  # This avoids problems with encoding the Census "for" and "in"
  # geography parameters.
  
  url <- paste0(
    
    base_url,
    
    "?get=",
    
    paste(
      variables,
      collapse = ","
    ),
    
    "&for=",
    
    geography,
    
    ":*",
    
    "&in=state:",
    
    STATE,
    
    "&key=",
    
    API_KEY
    
  )
  
  
  cat("\n")
  cat("============================================================\n")
  cat("Downloading:", geography, "\n")
  cat("============================================================\n")
  
  
  response <- request(url) |>
    
    req_user_agent(
      "RP Analyzer - Census Benchmark Application"
    ) |>
    
    req_perform()
  
  
  status <- resp_status(response)
  
  
  cat(
    "HTTP status:",
    status,
    "\n"
  )
  
  
  # --------------------------------------------------------------------------
  # Get response as text
  # --------------------------------------------------------------------------
  
  response_text <- resp_body_string(
    response
  )
  
  
  # --------------------------------------------------------------------------
  # STOP if Census returned an error
  # --------------------------------------------------------------------------
  
  if (status != 200) {
    
    cat("\nCensus response:\n")
    cat(response_text)
    
    stop(
      paste0(
        "\n\nCensus API returned HTTP ",
        status,
        "."
      ),
      call. = FALSE
    )
    
  }
  
  
  # --------------------------------------------------------------------------
  # Make sure response is actually JSON
  # --------------------------------------------------------------------------
  
  if (
    grepl(
      "^\\s*<",
      response_text
    )
  ) {
    
    cat(
      "\nThe Census server returned HTML instead of JSON.\n\n"
    )
    
    cat(
      "Beginning of response:\n\n"
    )
    
    cat(
      substr(
        response_text,
        1,
        1000
      )
    )
    
    stop(
      "\n\nCensus returned HTML instead of JSON.",
      call. = FALSE
    )
    
  }
  
  
  # --------------------------------------------------------------------------
  # Convert JSON to data frame
  # --------------------------------------------------------------------------
  
  result <- fromJSON(
    response_text,
    simplifyDataFrame = TRUE
  )
  
  
  result <- as.data.frame(
    result,
    stringsAsFactors = FALSE
  )
  
  
  # --------------------------------------------------------------------------
  # First row contains column names
  # --------------------------------------------------------------------------
  
  names(result) <- as.character(
    result[1, ]
  )
  
  
  result <- result[-1, , drop = FALSE]
  
  
  rownames(result) <- NULL
  
  
  # --------------------------------------------------------------------------
  # Check that NAME exists
  # --------------------------------------------------------------------------
  
  if (!"NAME" %in% names(result)) {
    
    stop(
      paste0(
        "\nNAME was not returned by the Census API.\n\n",
        "Columns returned:\n",
        paste(
          names(result),
          collapse = ", "
        )
      ),
      call. = FALSE
    )
    
  }
  
  
  return(result)
  
}


################################################################################
# 6. DOWNLOAD COUNTIES
################################################################################

county_data <- download_census(
  geography = "county"
)


################################################################################
# 7. DOWNLOAD PLACES
################################################################################

place_data <- download_census(
  geography = "place"
)


################################################################################
# 8. CLEAN COUNTY DATA
################################################################################

counties <- county_data %>%
  
  mutate(
    
    Geography_Type = "County",
    
    Geography_ID = paste0(
      state,
      county
    ),
    
    Geography_Name = sub(
      ", Texas$",
      "",
      NAME
    ),
    
    Population_18Plus = as.numeric(
      P4_001N
    ),
    
    Hispanic_Latino_18Plus = as.numeric(
      P4_002N
    ),
    
    Not_Hispanic_18Plus = as.numeric(
      P4_003N
    ),
    
    Not_Hispanic_White_18Plus = as.numeric(
      P4_005N
    ),
    
    Not_Hispanic_Black_18Plus = as.numeric(
      P4_006N
    ),
    
    Not_Hispanic_AIAN_18Plus = as.numeric(
      P4_007N
    ),
    
    Not_Hispanic_Asian_18Plus = as.numeric(
      P4_008N
    ),
    
    Not_Hispanic_NHPI_18Plus = as.numeric(
      P4_009N
    ),
    
    Not_Hispanic_Other_Race_18Plus = as.numeric(
      P4_010N
    ),
    
    Not_Hispanic_TwoPlus_Races_18Plus = as.numeric(
      P4_011N
    )
    
  ) %>%
  
  select(
    
    Geography_Type,
    
    Geography_ID,
    
    Geography_Name,
    
    Population_18Plus,
    
    Hispanic_Latino_18Plus,
    
    Not_Hispanic_18Plus,
    
    Not_Hispanic_White_18Plus,
    
    Not_Hispanic_Black_18Plus,
    
    Not_Hispanic_AIAN_18Plus,
    
    Not_Hispanic_Asian_18Plus,
    
    Not_Hispanic_NHPI_18Plus,
    
    Not_Hispanic_Other_Race_18Plus,
    
    Not_Hispanic_TwoPlus_Races_18Plus
    
  )


################################################################################
# 9. CLEAN PLACE DATA
################################################################################

places <- place_data %>%
  
  mutate(
    
    Geography_Type = "Place",
    
    Geography_ID = paste0(
      state,
      place
    ),
    
    Geography_Name = sub(
      ", Texas$",
      "",
      NAME
    ),
    
    Population_18Plus = as.numeric(
      P4_001N
    ),
    
    Hispanic_Latino_18Plus = as.numeric(
      P4_002N
    ),
    
    Not_Hispanic_18Plus = as.numeric(
      P4_003N
    ),
    
    Not_Hispanic_White_18Plus = as.numeric(
      P4_005N
    ),
    
    Not_Hispanic_Black_18Plus = as.numeric(
      P4_006N
    ),
    
    Not_Hispanic_AIAN_18Plus = as.numeric(
      P4_007N
    ),
    
    Not_Hispanic_Asian_18Plus = as.numeric(
      P4_008N
    ),
    
    Not_Hispanic_NHPI_18Plus = as.numeric(
      P4_009N
    ),
    
    Not_Hispanic_Other_Race_18Plus = as.numeric(
      P4_010N
    ),
    
    Not_Hispanic_TwoPlus_Races_18Plus = as.numeric(
      P4_011N
    )
    
  ) %>%
  
  select(
    
    Geography_Type,
    
    Geography_ID,
    
    Geography_Name,
    
    Population_18Plus,
    
    Hispanic_Latino_18Plus,
    
    Not_Hispanic_18Plus,
    
    Not_Hispanic_White_18Plus,
    
    Not_Hispanic_Black_18Plus,
    
    Not_Hispanic_AIAN_18Plus,
    
    Not_Hispanic_Asian_18Plus,
    
    Not_Hispanic_NHPI_18Plus,
    
    Not_Hispanic_Other_Race_18Plus,
    
    Not_Hispanic_TwoPlus_Races_18Plus
    
  )


################################################################################
# 10. COMBINE
################################################################################

texas_census <- bind_rows(
  
  counties,
  
  places
  
)


################################################################################
# 11. ADD METADATA
################################################################################

texas_census <- texas_census %>%
  
  mutate(
    
    State = "Texas",
    
    Census_Year = 2020,
    
    Census_Source = "U.S. Census Bureau",
    
    Census_Dataset = "2020 Decennial Census PL 94-171",
    
    Census_Table = "P4",
    
    Population_Universe = "Population 18 years and over"
    
  ) %>%
  
  select(
    
    State,
    
    Census_Year,
    
    Census_Source,
    
    Census_Dataset,
    
    Census_Table,
    
    Population_Universe,
    
    Geography_Type,
    
    Geography_ID,
    
    Geography_Name,
    
    Population_18Plus,
    
    Hispanic_Latino_18Plus,
    
    Not_Hispanic_18Plus,
    
    Not_Hispanic_White_18Plus,
    
    Not_Hispanic_Black_18Plus,
    
    Not_Hispanic_AIAN_18Plus,
    
    Not_Hispanic_Asian_18Plus,
    
    Not_Hispanic_NHPI_18Plus,
    
    Not_Hispanic_Other_Race_18Plus,
    
    Not_Hispanic_TwoPlus_Races_18Plus
    
  )


################################################################################
# 12. SORT
################################################################################

texas_census <- texas_census %>%
  
  arrange(
    Geography_Type,
    Geography_Name
  )


################################################################################
# 13. SAVE
################################################################################

write_csv(
  
  texas_census,
  
  OUTPUT_FILE,
  
  na = ""
  
)


################################################################################
# 14. VALIDATION
################################################################################

cat("\n")
cat("============================================================\n")
cat("DOWNLOAD COMPLETE\n")
cat("============================================================\n\n")

cat(
  "File:",
  OUTPUT_FILE,
  "\n\n"
)

cat(
  "Total geographies:",
  nrow(texas_census),
  "\n"
)

cat(
  "Counties:",
  sum(
    texas_census$Geography_Type == "County"
  ),
  "\n"
)

cat(
  "Places:",
  sum(
    texas_census$Geography_Type == "Place"
  ),
  "\n\n"
)


################################################################################
# 15. EL PASO COUNTY CHECK
################################################################################

cat("============================================================\n")
cat("EL PASO CHECK\n")
cat("============================================================\n\n")

print(
  texas_census %>%
    
    filter(
      Geography_Name == "El Paso"
    )
)


################################################################################
# END
################################################################################