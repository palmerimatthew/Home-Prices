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

