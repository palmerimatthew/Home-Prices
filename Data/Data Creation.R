library(tidyverse)

## ZILLOW ZIP ZHVI DATA -------

ZIP_Info <- read.csv("./Data/Raw Data/Zillow_ZHVI_Data.csv") %>%
  dplyr::filter(State == "TN") %>%
  dplyr::select(RegionID:CountyName)

data.table::fwrite(ZIP_Info, "./Data/ZIP_Info.csv")

ZHVI_Price_Info <- read.csv("./Data/Raw Data/Zillow_ZHVI_Data.csv") %>%
  dplyr::filter(State == "TN") %>%
  dplyr::select(-(SizeRank:CountyName)) %>%
  tidyr::pivot_longer(cols = -RegionID, names_to = "Month", values_to = "MedianZHVI") %>%
  dplyr::mutate(Month = gsub("X", "", Month),
                Month = as.Date(Month, "%Y.%m.%d"),
                Month = lubridate::floor_date(Month, "month")) %>%
  dplyr::filter(!is.na(MedianZHVI))

data.table::fwrite(ZHVI_Price_Info, "./Data/ZHVI_Price_Info.csv")

## ZILLOW ZIP ZORI DATA ------

ZORI_Price_Info <- read.csv("./Data/Raw Data/Zillow_ZORI_Data.csv") %>%
  dplyr::left_join(read.csv("./Data/ZIP_Info.csv"), by="RegionID") %>%
  dplyr::filter(State == "TN") %>%
  dplyr::select(-(RegionName.x:MsaName)) %>%
  dplyr::select(-(SizeRank.y:CountyName)) %>%
  tidyr::pivot_longer(cols = -RegionID, names_to = "Month", values_to = "MedianZORI") %>%
  dplyr::mutate(Month = gsub("X", "", Month),
                Month = as.Date(Month, "%Y.%M"),
                Month = lubridate::floor_date(Month, "month")) %>%
  dplyr::filter(!is.na(MedianZORI))

data.table::fwrite(ZORI_Price_Info, "./Data/ZORI_Price_Info.csv")

## Freddie Mac 30 year mortgage rate -----
raw_sheet <- openxlsx::read.xlsx("./Data/Raw Data/30YR_Mortgage_Rates.xlsx")[-1,]

year_rows <- c(1, 16, 31, 46, 61, 76, 91, 106)
year_cols <- c(2, 4, 6, 8, 10, 12, 14)

Mortgage_Rates <- purrr::map_df(year_rows, function(i) {
  purrr::map_df(year_cols, function(j) {
    year <- as.numeric(raw_sheet[i,j])
    temp <- raw_sheet[(i+2):(i+13), c(1, j:(j+1))] %>%
      `colnames<-`(c("Month", "Rate", "Points")) %>%
      dplyr::mutate(Year = year) %>%
      dplyr::select(Year, Month, Rate, Points)
    temp
  })
}) %>%
  dplyr::filter(!is.na(Year)) %>%
  replace(., . == "n.a.", NA) %>%
  dplyr::mutate(Rate = round(as.numeric(Rate), 2),
                Points = round(as.numeric(Points), 2),
                Month = match(Month, month.name),
                Month = as.Date(paste(1, Month, Year, sep = "-"), "%d-%m-%Y")) %>%
  dplyr::select(-Year)

data.table::fwrite(Mortgage_Rates, "./Data/Mortgage_Rates_30.csv")
