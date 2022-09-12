library(tidyverse); library(ggplot2); library(tigris); library(sf)

mfg <- read_csv('data/illinois_mfg21.csv') |> 
  st_as_sf(coords = c("lon","lat"), crs = 4326, remove = F) |> 
  st_transform(crs = 6454) |> 
  mutate(size = 'Large',
         size = ifelse(num_employees < 25, 'Small', size),
         size = ifelse(num_employees >= 25 & num_employees < 100, 'Medium', size))

rucc <- read_csv('data/rucc.csv') |> 
  mutate(GEOID = as.character(cty_fips))

# Simple histogram --------------------------------------------------------

# ggplot(mfg) +
  ggplot(mfg |> filter(num_employees <= 100)) +
  aes(x = num_employees) +
  geom_histogram(stat = 'bin', bins = 100, color = 'black', fill = '#FF5F05') +
  theme_minimal(base_size = 16, base_family = 'Georgia') +
  # theme_minimal() +
  labs(x = 'Number of Employees', y = 'Number of Establishments', 
       caption = 'Source: Data Axle, 2022',
       title = 'Distribution of Illinois Manufacturing Establishments by Size') +
  scale_y_continuous(labels = scales::comma) +
  theme(plot.title = element_text(face = 'bold', hjust = .5, size = 15),
        axis.title = element_text(face = 'bold'))

# Spatial distribution ----------------------------------------------------

# generate county shape files
il_counties <- counties(state = 'IL', cb = T, resolution = '20m') |> 
  st_transform(crs = 6454)

ggplot() +
  geom_sf(data = mfg, alpha = .7, size = .4, aes(color = factor(size))) +
  geom_sf(data = il_counties, alpha = 0, color = 'black', size = .2) +
  theme_void() +
  scale_color_manual(values = c('firebrick','dodgerblue','forestgreen'), name = 'Firm Size')

janitor::tabyl(mfg$size)

ggplot() +
  geom_sf(data = mfg |> filter(size != 'Small'), 
          alpha = .7, size = .4, aes(color = factor(size))) +
  geom_sf(data = il_counties, alpha = 0, color = 'black', size = .2) +
  theme_void() +
  scale_color_manual(values = c('firebrick','dodgerblue','forestgreen'), name = 'Firm Size')

# Count number of establishments by county/size ---------------------------

# use GIS to assign each point to a county, then tabulate by county
mfg_county <- st_intersection(mfg,(il_counties |> select(GEOID))) |> 
  st_drop_geometry() |> 
  group_by(GEOID) |> 
  summarize(small_mfg = sum(num_employees > 4 & num_employees < 25),
            med_mfg = sum(num_employees > 24 & num_employees < 100),
            large_mfg = sum(num_employees > 99))


# Merge county MFG totals and RUCC codes back with original county shape file
il_cty_mfg <- il_counties |> 
  left_join(mfg_county, by = 'GEOID') |> 
  left_join(rucc, by = 'GEOID')

ggplot() +
  geom_sf(data = il_cty_mfg, color = 'black', alpha = .7, size = .2, aes(fill = med_mfg)) +
  theme_void() +
  scale_fill_viridis_c(option = 'B', trans = 'pseudo_log', 
                       name = '# of Midsize Manufacturers',
                       breaks = c(3,30,300,3000), labels = scales::comma) +
  guides(fill = guide_colourbar(barheight = 0.35, barwidth = 15, title.position="top")) +
  theme(legend.title = element_text(face = 'bold',hjust = .5),
        legend.position = 'bottom') 

# Take metropolitan counties out of the distribution
ggplot() +
  geom_sf(data = il_cty_mfg, color = 'white', alpha = 1, fill = '#000004', size = .1) +
  geom_sf(data = il_cty_mfg |> filter(rucc > 3), 
          color = 'white', alpha = 1, size = .2, aes(fill = med_mfg)) +
  theme_void() +
  scale_fill_viridis_c(option = 'A', trans = 'pseudo_log', 
                       name = '# of Midsize Manufacturers',
                       breaks = c(3,30,300,3000), labels = scales::comma) +
  guides(fill = guide_colourbar(barheight = 0.35, barwidth = 15, title.position="top")) +
  theme(legend.title = element_text(face = 'bold',hjust = .5),
        legend.position = 'bottom') 
