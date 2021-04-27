## ZILLOW NEIGHBORHOOD ZHVI DATA -------

Neighborhood_Info <- read.csv("./Data/Zillow ZHVI NEI Data/Zillow_ZHVI_Data.csv") %>%
  dplyr::select(RegionID:CountyName)

data.table::fwrite(Neighborhood_Info, "./Data/Zillow ZHVI NEI Data/Neighborhood_Info.csv")

Price_Info <- read.csv("./Data/Zillow ZHVI NEI Data/Zillow_ZHVI_Data.csv") %>%
  dplyr::select(-(SizeRank:CountyName)) %>%
  tidyr::pivot_longer(cols = -RegionID, names_to = "Month", values_to = "MedianPrice") %>%
  dplyr::mutate(Month = gsub("X", "", Month),
                Month = as.Date(Month, "%m.%d.%y")) %>%
  dplyr::filter(!is.na(MedianPrice))

data.table::fwrite(Price_Info, "./Data/Zillow ZHVI NEI Data/Price_Info.csv")
