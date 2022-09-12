library(tidyverse); library(ggplot2); library(tigris)

bea_data <- read_csv('data/bea_data.csv')

# Download spatial data ---------------------------------------------------

il_counties <- counties(state = 'IL', cb = T, resolution = '20m')
plot(il_counties$geometry)
plot(il_counties |> filter(NAME == 'Alexander') |> select(geometry))

# il_counties <- counties(state = 'IL', cb = T, resolution = '500k')
# plot(il_counties |> filter(NAME == 'Alexander') |> select(geometry))


# Merge and plot ----------------------------------------------------------

il_spatial <- il_counties |> 
  mutate(geo_fips = as.numeric(GEOID)) |> 
  left_join(bea_data, by = 'geo_fips')

ggplot() +
  geom_sf(data = il_spatial, aes(fill = gdp_15), color = 'black', size = .2) +
  theme_void() +
  scale_fill_gradient(high = '#13294B', low = 'white',
                      name = 'GPD', labels = scales::dollar) +
  labs(title = 'Raw Gross County Product', 
       caption = 'Source: BEA, 2015')

ggplot() +
  geom_sf(data = il_spatial, aes(fill = gdp_15), color = 'black', size = .2) +
  theme_void() +
  scale_fill_gradient(high = '#13294B', low = 'white', trans = 'log', 
                      # log lets you keep using linear gradient for exponential distribution
                      name = 'Log of GDP', labels = scales::dollar) +
  labs(title = 'Logged Gross County Product', 
       caption = 'Source: BEA, 2015')

ggplot() +
  geom_sf(data = il_spatial, aes(fill = gdp_per_capita), color = 'black', size = .2) +
  theme_void() +
  scale_fill_gradient(high = '#FF5F05', low = 'white',
                      name = 'GPD Per-Capita', labels = scales::dollar) +
  labs(title = 'Population-Normalized Gross County Product', 
       caption = 'Source: BEA, 2015')


# Using Tidy Census to get decennial census data --------------------------

county_seats <- read_csv('data/cty_seat.csv') |> 
  mutate(GEOID = as.character(city_fips))
rucc <- read_csv('data/rucc.csv') |> 
  mutate(cty_fips = as.character(cty_fips))

tidycensus::census_api_key('88f7ff2ffb34430c5a2fa22e2755b70f08387b2b')
city_populations <- tidycensus::get_decennial(geography = 'place',
                                              variables = 'P1_001N',
                                              year = 2020,
                                              state = 'IL',
                                              geometry = T)

city_blobs <- city_populations |> 
  left_join(county_seats, by = 'GEOID') |> 
  left_join(rucc, by = 'cty_fips') 

ggplot() +
  geom_sf(data = il_spatial, color = 'black', size = .2, alpha = 0) +
  geom_sf(data = city_blobs |> filter(cty_seat == 1), aes(fill = value), color = NA) +
  theme_void() +
  scale_fill_viridis_c(trans = 'log', breaks = c(1000,10000,100000,1000000), 
                       labels = scales::comma, name = 'Population') +
  labs(title = 'Population in Illinois County Seats', 
       caption = 'Source: US Census, 2020')

city_dots <- city_blobs |> 
  sf::st_centroid()

ggplot() +
  geom_sf(data = il_spatial, color = 'black', size = .2, alpha = 0) +
  geom_sf(data = city_dots |> filter(cty_seat == 1), aes(color = value), size = 3) +
  theme_void() +
  scale_color_viridis_c(trans = 'log', breaks = c(1000,10000,100000,1000000), 
                        labels = scales::comma, name = 'Population') +
  labs(title = 'Population in Illinois County Seats', 
       caption = 'Source: US Census, 2020')

ggplot() +
  geom_sf(data = il_spatial, color = 'black', size = .2, alpha = 0) +
  geom_sf(data = city_dots |> filter(cty_seat == 1, rucc > 3), aes(color = value), size = 3) +
  theme_void() +
  scale_color_viridis_c(trans = 'log', breaks = c(1000,10000,100000,1000000), 
                        labels = scales::comma, name = 'Population') +
  labs(title = 'Population in Illinois County Seats', 
       caption = 'Source: US Census, 2020')
