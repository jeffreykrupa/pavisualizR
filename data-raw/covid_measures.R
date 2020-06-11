# TODO: Update to come from online API/DB

path = "C://Users/iozeroff/Earthwatch/Anna Woodroof - Operation Healthy Air/7.Data and Field Reports/2020/Post Covid-19 materials/OHA-COVID-policy-responses.csv"

# Reading COVID-19 Data
covid_measures <- readr::read_csv(file = path) %>%
  mutate(
    # Parsing dates.
    start_date = lubridate::parse_date_time(x = start_date,
                                            orders = "d!-m!-y! H!:M!",
                                            tz = "UTC"),
    # Parsing End Date.
    end_date = if_else(end_date == "Ongoing",
                       # If a measure is still happening, it's included as "Ongoing", so replacing that with lubridate::now().
                       lubridate::now(tzone="UTC"),
                       lubridate::parse_date_time(x = end_date, orders = "d!-m!-y! H!:M!", tz = "UTC"))
)

usethis::use_data(covid_measures, internal = TRUE, overwrite = TRUE)
usethis::use_data(covid_measures, overwrite = TRUE)
