# Creates AQI Info data for internal plotting use.

# TODO: Credit PWFSLSmoke.

# US
# Loading EPA AQI categorical index info from PWFSL Smoke.
# The breaks in this are by PM.
US <- PWFSLSmoke::AQI
# Adding minimums and maximums of AQI breaks.
# TODO: Can I condense this into one series of breaks.
US[["aqi_breaks"]] <- data.frame(minimums = c(0, 51, 101, 151, 201, 301),
                                 maximums = c(50, 100, 150, 200,
                                              300, 500)
)

US[["aqi_pm_mins"]] <- c(-Inf, 12.1, 35.5, 55.5, 150.5, 250.5)
US[["aqi_pm_maxs"]] <- c(12, 35.4, 55.4, 150.4, 250.4, Inf)
# India
IN <- list(
  index_category = c("Good", "Satisfactory", "Moderate",
                     "Poor", "Very Poor", "Severe"),
  aqi_pm_mins = c(-Inf, 31, 61, 91, 121, 251),
  aqi_pm_maxs = c(30, 60, 90, 120, 250, Inf),
  breaks_24 = c(-Inf, 31, 61, 91, 121, 251, Inf),
  aqi_breaks = c(0, 50, 100, 200, 300, 400, 500),
  names = c("Good", "Satisfactory", "Moderate",
            "Poor", "Very Poor", "Severe"),
  actions = c("Minimal Impact",
              "May cause minor breathing discomfort to sensitive people.",
              paste0("May cause breathing discomfort to people with lung ",
                     "disease such as asthma and discomfort to people with ",
                     "heart disease, children and older adults."),
              paste0("May cause breathing discomfort to people on prolonged ",
                     "exposure and discomfort to people with heart disease ",
                     "with short exposure."),
              paste0("May cause respiratory illness to the people on ",
                     "prolonged exposution. Effect may be more pronounced ",
                     "in people with lung and heart diseases."),
              paste0("May cause respiratory effects on even healthy people ",
                     "and serious health impacts on people with lung/heart ",
                     "diseases. The health impacts may be experienced even ",
                     "during light physical activity.")
  ),
  colors = c("#006600", "#009900", "#FFFF00", "#FF6600", "#CC0000", "#990000")
)

aqi_info <- list(
  US = US,
  IN = IN
)

usethis::use_data(aqi_info, overwrite = TRUE)
usethis::use_data(aqi_info, internal = TRUE, overwrite = TRUE)
