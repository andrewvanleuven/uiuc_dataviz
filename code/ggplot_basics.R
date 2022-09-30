library(tidyverse); library(ggplot2); library(lubridate)

# Read in NASS data -------------------------------------------------------

# https://quickstats.nass.usda.gov/results/35EEFEDB-0ED9-3329-B43C-7564E1CC24CC

soy <- read_csv('data/il_soy.csv') |> 
  janitor::clean_names() |> 
  filter(str_detect(period,'MARKETING YEAR', negate = T)) |> 
  mutate(date = dmy(paste0('01',period,year)),
         value = as.numeric(value)) |> 
  filter(!is.na(value),
         year >= 1990 & year <= 2000)


# points -----------------------------------------------------------------

ggplot(soy) +
  aes(x = date, y = value) +
  geom_point()


# Linear best fit line
ggplot(soy) +
  aes(x = date, y = value) +
  geom_smooth(method = "lm") +
  geom_point()

# new color (universal...applied to the geom)
ggplot(soy) +
  aes(x = date, y = value) +
  geom_point(color = 'red')

# new color (applied to the aesthetic mapping)
ggplot(soy) +
  aes(x = date, y = value, color = year) +
  geom_point()

ggplot(soy) +
  aes(x = date, y = value, color = factor(year)) +
  geom_point()

ggplot(soy) +
  aes(x = date, y = value, color = factor(year)) +
  geom_point(shape = 15, size = 3, alpha = .6) +
  theme_minimal(base_family = 'Georgia') +
  labs(x = 'Year', y = 'Value', color = 'Year')

# new theme (including family, aka. font)
# new shape and size
# alpha controls transparency
# axes and legend labeled

# lines -------------------------------------------------------------------

  ggplot(soy) +
  aes(x = date, y = value) +
  geom_line()

# bars --------------------------------------------------------------------

  ggplot(soy) +
  aes(x = date, weight = value) +
  # aes(x = date, weight = value, fill = factor(period)) +
  # aes(x = date, weight = value, fill = factor(period == 'APR')) +
  # scale_fill_manual(values = c('orange','navy'), labels = c('Other Months','April'),
  #                   name = 'Month') +
  geom_bar()

# steps -------------------------------------------------------------------

ggplot(soy) +
  aes(x = date, y = value) +
  geom_step()




