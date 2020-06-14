#' @export
#'
#' @title Visualize the proportion of sensor measurements that fall within Air
#'  Quality Index (AQI) categories for each hour of the given day.
#'
#' @description Generate a bar chart of AQI measurements for a sensor.
#'
#' @param sensor An AirSensor object.
#' @param aqi_country The country who's Air Quality Index (AQI) should be used
#'   for plotting, as an ISO 2 letter country code. Current options are "US" and
#'   "IN".
#' @param day Character vector containing days of the week to show in plot (all
#'   lowercase), or 'all' to use all days of the week.
#' @param position Whether to use stacked bar chart showing counts or filled bar
#'   chart showing normalized proportions.
#'
#' @return a ggplot object.
#'
#' @example \dontrun{ plot <- day_of_week_aqiBar(sensor = example_sensor,
#' aqi_country = "IN", day = c("monday, "tuesday", "wednseday"), position =
#' "fill") }


day_of_week_aqiBar <- function(
  sensor,
  aqi_country,
  day = "all",
  position = "stack"
) {


  position_opts <- c(
    "fill",
    "stack"
  )

  aqi_country_opts <- c(
    "US",
    "IN"
  )

  day_opts <- c(
    "monday",
    "tuesday",
    "wednesday",
    "thursday",
    "friday",
    "saturday",
    "sunday",
    "all"
  )


  if( !AirSensor::sensor_isSensor(sensor) ) {
    stop("sensor must be of class 'airsensor'.")
  }
  if ( !(position %in% position_opts) ) {
    stop("position must be one of 'fill' or 'stack'.")
  }

  if ( !(aqi_country %in% aqi_country_opts) ) {
    stop("aqi_country must be one of 'US' or 'IN'")
  }

  if ( any(!(day %in% day_opts)) ) {
    stop(paste0("day must be one or more valid, unabbreviated, ",
                "day of the week in English, or 'all'"))
  }

#------------------------------------------------------------------------------

  if ( day == "all" ) {
    day <- day_opts[day_opts != "all"]
  }

  day <- stringr::str_to_title(day)

  # Loading AQI Categorical Index info for plotting.
  aqi <- aqi_info[[aqi_country]]

  meta <- sensor %>%
    AirSensor::sensor_extractMeta()

  # Ready plotting set.
  data <- sensor %>%
    AirSensor::sensor_extractData() %>%
    dplyr::rename(pm25 = 2) %>%
    dplyr::mutate(
      # Get AQI category of pm25
      aqi_category = cut(pm25,
                         breaks = aqi$breaks_24,
                         labels = aqi$names,
                         ordered_result = TRUE),
      # Get day of week for each hour.
      weekday = factor(
        weekdays(datetime),
        levels = stringr::str_to_title(day_opts[day_opts != "all"]),
        ordered = TRUE),
      # Extract hour.
      hour = lubridate::hour(datetime)
    ) %>%
    dplyr::filter(!is.na(pm25),
           weekday %in% day)

  # Getting Timezone from data.
  timezone <- attr(data$datetime,"tzone")

  # Creating x label with timezone.
  x_label <- paste("Hour of Day ", "(", timezone, ")", sep = "")
  if (position == "fill") {
    y_label <- "Proportion of Hours"
    position_func <- paste("position_", position, sep = "")
  }

  if (position == "stack") {
    y_label <- "Number of Hours"
    position_func <- paste("position_", position, sep = "")
  }

  plot <- data %>%
      ggplot2::ggplot(aes(x = factor(hour), fill = aqi_category)) +
      ggplot2::geom_bar(position = do.call(what = position_func,
                                           args = list(reverse = TRUE))) +
      ggplot2::scale_fill_manual(aesthetics = "fill",
                                 values = aqi$colors,
                                 na.translate = FALSE,
                                 labels = aqi$names
                                 ) +
      ggplot2::scale_x_discrete() +
      ggplot2::labs(
        title = "Average Hourly PM25 AQI Category by Hour and Day",
        subtitle = meta$label,
        x = x_label,
        y = y_label,
        fill = "AQI Category")

  if ( length(day) > 1 ) {

    plot <- plot +
     ggplot2::facet_wrap(~weekday, nrow = 2)

  }

  return(plot)
}
