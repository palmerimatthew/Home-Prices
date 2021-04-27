### loading libraries and data -----
library(tidyverse)
library(plotly)
library(assertthat)

ZIP_Info <- readr::read_csv("./Data/ZIP_Info.csv")
ZHVI_Price_Info <- readr::read_csv("./Data/ZHVI_Price_Info.csv")

### initial Nashville exploration
Nashville_Info <- ZIP_Info %>%
  dplyr::filter(City == "Nashville" & StateName == "TN") %>%
  dplyr::select(RegionID, RegionName) %>%
  dplyr::left_join(ZHVI_Price_Info, by = "RegionID")


### price_plotter -----
price_plotter <- function(data, 
                          start_date = NULL, 
                          end_date = NULL, 
                          filter_to_dates = F,
                          percent_diff_min = NULL,
                          percent_diff_max = NULL,
                          price_floor = 0,
                          price_ceiling = 100000000) {
  
  if (!is.null(start_date) & !is.null(end_date)) {
    assertthat::assert_that(
      is.date(start_date),
      is.date(end_date),
      msg = "start_date and end_date must be NULL (all data) or both be dates"
    )
    
    assertthat::assert_that(
      start_date < end_date,
      msg = "start_date must be before end_date"
    )
    
    assertthat::assert_that(
      is.logical(filter_to_dates),
      msg = "filter_to_dates must be a logical"
    )
    
    if (!is.null(percent_diff_min)) {
      assertthat::assert_that(
        is.numeric(percent_diff_min),
        msg = "percent_diff_min must either be NULL (no minimum) or a number"
      )
    }
    
    if (!is.null(percent_diff_max)) {
      assertthat::assert_that(
        is.numeric(percent_diff_max),
        msg = "percent_diff_max must either be NULL (no max) or a number"
      )
    }
    
    # adjusting start_date and end_date to end of the given month
    start_date <- lubridate::floor_date(start_date, "month")
    end_date <- lubridate::floor_date(end_date, "month")
    
    # filter to date ranges if wanted
    if (filter_to_dates) {
      data <- dplyr::filter(data, Month >= start_date & Month <= end_date)
    }
    
    # calculating percent differences
    percent_diff <- data %>%
      dplyr::mutate(Month = dplyr::case_when(Month == start_date ~ "Start",
                                             Month == end_date ~ "End",
                                             T ~ "Neither")) %>%
      dplyr::filter(Month == "Start" | Month == "End") %>%
      tidyr::pivot_wider(names_from = "Month", values_from = "MedianZHVI") %>%
      dplyr::mutate(percent_difference = (End-Start)/Start,
                    percent_difference = percent_difference*100)
    
    # filter to min percent change cutoff
    if (!is.null(percent_diff_min)) {
      percent_diff <- dplyr::filter(percent_diff, percent_difference >= percent_diff_min)
    }
    
    # filter to max percent change cutoff
    if (!is.null(percent_diff_max)) {
      percent_diff <- dplyr::filter(percent_diff, percent_difference <= percent_diff_max)
    }
    
    # applying percentage cutoffs
    percent_diff <- dplyr::select(percent_diff, RegionID, percent_difference)
    
    data <- dplyr::inner_join(data, percent_diff, by="RegionID")
  }
  
  # filtering to price_floor and price_ceiling
  final_price <- data %>%
    dplyr::filter(Month == max(Month) & MedianZHVI >= price_floor & MedianZHVI <= price_ceiling) %>%
    dplyr::select(RegionID)
  
  data <- dplyr::inner_join(data, final_price, by="RegionID")
  
  # graphing data
  g <- ggplot(data, aes(x = Month, y = MedianZHVI, group = RegionName, label = percent_difference)) +
    geom_line()
  
  p <- plotly::ggplotly(g)
  
  return(list(plot = plotly::ggplotly(p), data = data))
}

price_plotter(Nashville_Info,
              start_date = as.Date("2015-01-01"),
              end_date = as.Date("2021-01-01"),
              filter_to_dates = F,
              percent_diff_min = 50,
              percent_diff_max = NULL,
              price_floor = 0,
              price_ceiling = 400000)$plot



