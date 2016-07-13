library(devtools)
library(readr)
library(dplyr)
document()

countrycode_data = read.csv('data/countrycode_data.csv', stringsAsFactors=FALSE, na.strings='')
countrycode_data <- countrycode_data %>%
  rename(country.name = country_name) %>%
  mutate(`regex` = iconv(`regex`, to='UTF-8'))

gwcodes <- read_tsv("data/iisystem.dat", col_names = c("gwn", "gwc", "gw_country", "gw_start", "gw_end"),
                    col_types = cols("gw_start" = col_date("%d:%m:%Y"), "gw_end" = col_date("%d:%m:%Y")))

# remove duplicates
gw_set <- gwcodes %>%
  group_by(gwn, gwc) %>%
  summarize() 

new_data <- countrycode_data %>%
  left_join(gw_set, by = c("cown" = "gwn"))

new_data %>%
  filter(cowc != gwc) 

new_data <- new_data %>%
  mutate(gwn = cown,
         iso2c = ifelse(country.name=='Namibia', 'NA', iso2c))

       



gwcodes %>%
  anti_join(new_data, by = c("gwn" = "cown")) %>%
  filter(!is.na(gwn)) %>%
  arrange(gwn) %>%
  select(gwn, gwc, gw_country)

save(countrycode_data, file='data/countrycode_data.rda')


