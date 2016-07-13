library(devtools)
library(readr)
library(dplyr)
document()

rm(list = ls())
countrycode_data = read.csv('data/countrycode_data.csv', stringsAsFactors=FALSE, na.strings='')

countrycode_data <- countrycode_data %>%
  rename(country.name = country_name) %>%
  mutate(`regex` = iconv(`regex`, to='UTF-8'))

gwcodes <- read_tsv("data/gwcodes.tsv", col_names = c("gwn", "gwc", "gw_country", "gw_start", "gw_end"),
                    col_types = cols("gw_start" = col_date("%d:%m:%Y"), "gw_end" = col_date("%d:%m:%Y")))

# remove duplicates
gw_set <- gwcodes %>%
  group_by(gwn, gwc) %>%
  summarize() 

countrycode_data <- countrycode_data %>%
  left_join(gw_set, by = c("cown" = "gwn"))

countrycode_data %>%
  filter(cowc != gwc) 

# find missing
get_missing_data <- function(countrycodes, gwcodes) {
  gwcodes %>%
    anti_join(countrycodes, by = c("gwn" = "cown")) %>%
    group_by(gwn, gwc, gw_country) %>%
    summarize()
}

missing_data <- get_missing_data(countrycode_data, gwcodes)

countrycode_data <- countrycode_data %>%
  left_join(missing_data, by = c("country.name" = "gw_country")) %>%
  mutate(gwn = ifelse(is.na(gwn), cown, gwn),
         gwc = ifelse(is.na(gwc.x), gwc.y, gwc.x)) %>%
  select(country.name, cowc, cown, gwc, gwn, iso3c, iso3n, iso2c, fao, fips105, imf, ioc, un, wb, regex, continent, region, eu28, ar5)
  
save(countrycode_data, file='data/countrycode_data.rda')


