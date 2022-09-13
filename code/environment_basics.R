# install.packages("tidyverse")
# install.packages("scales")
# install.packages("sf")
# install.packages("janitor")
# install.packages("tigris")
# install.packages("tidycensus")
# install.packages("ggpubr")
# install.packages('treemapify')
# install.packages('lubridate')
# install.packages('showtext')

library(tidyverse); library(tigris)


# Import BEA data ---------------------------------------------------------

# https://apps.bea.gov/iTable/iTable.cfm?reqid=70&step=30&isuri=1&major_area=4&area=xx&year=2015&tableid=20&year_end=-1&classification=non-industry&state=17000&statistic=2&yearbegin=-1&unit_of_measure=levels
data1 <- read_csv('data/bea_il_gdp15.csv',skip = 4) |> 
  janitor::clean_names() |> 
  rename(gdp_15 = x2015) |> 
  filter(!is.na(gdp_15), # why do this?
         geo_fips > 17000) # why do this?

# https://apps.bea.gov/iTable/iTable.cfm?reqid=70&step=30&isuri=1&major_area=4&area=xx&year=2015&tableid=533&year_end=-1&classification=non-industry&state=17000&statistic=1&yearbegin=-1&unit_of_measure=levels
data2 <- read_csv('data/bea_il_pop15.csv',skip = 4) |> 
  janitor::clean_names() |> 
  rename(pop_15 = x2015) |> 
  filter(!is.na(pop_15), 
         geo_fips > 17000) 

bea_data <- data1 |> 
  left_join(data2, by = c('geo_fips','geo_name')) |> 
  mutate(gdp_per_capita = round(gdp_15/pop_15,1))

# write_csv(bea_data,'data/bea_data.csv') # you don't need to save this (it's already in the folder)

