# install.packages('lubridate')
# install.packages('showtext')

library(tidyverse); library(ggplot2); library(lubridate)

# Read in NASS data -------------------------------------------------------

# https://quickstats.nass.usda.gov/results/35EEFEDB-0ED9-3329-B43C-7564E1CC24CC
df1 <- read_csv('data/il_wheat.csv') |> 
  janitor::clean_names() |> 
  filter(str_detect(period,'MARKETING YEAR', negate = T)) |> 
  mutate(date = dmy(paste0('01',period,year)),
         value = as.numeric(value)) |> 
  filter(!is.na(value))

df2 <- read_csv('data/il_soy.csv') |> 
  janitor::clean_names() |> 
  filter(str_detect(period,'MARKETING YEAR', negate = T)) |> 
  mutate(date = dmy(paste0('01',period,year)),
         value = as.numeric(value)) |> 
  filter(!is.na(value))

df3 <- read_csv('data/il_oats.csv') |> 
  janitor::clean_names() |> 
  filter(str_detect(period,'MARKETING YEAR', negate = T)) |> 
  mutate(date = dmy(paste0('01',period,year)),
         value = as.numeric(value)) |> 
  filter(!is.na(value))

# Plot each time series ---------------------------------------------------

ggplot() +
  geom_line(data = df1, aes(x = date, y = value), color = '#BB0000') +
  geom_line(data = df2, aes(x = date, y = value), color = '#13294B') +
  geom_line(data = df3, aes(x = date, y = value), color = '#FC7300') +
  theme_minimal() +
  labs(x = 'Date', y = 'PRICE RECEIVED, MEASURED IN $ / BU') +
  scale_y_continuous(labels = scales::dollar) +
  scale_x_date(limits = as.Date(c('1960-01-01','2000-01-01')))

all_three <- rbind(df1 |> select(commodity, date, value),
                   df2 |> select(commodity, date, value),
                   df3 |> select(commodity, date, value))

ggplot() +
  geom_line(data = all_three, aes(x = date, y = value, color = commodity)) +
  theme_minimal() +
  labs(x = 'Date', y = 'PRICE RECEIVED, MEASURED IN $ / BU') +
  scale_y_continuous(labels = scales::dollar) +
  scale_x_date(limits = as.Date(c('1960-01-01','2000-01-01'))) +
  scale_color_manual(values = c('#BB0000','#13294B','#FC7300'), name = 'Commodity')

# Create index ------------------------------------------------------------

cutoff_a <- '1974-12-31'
cutoff_b <- '1980-01-01'
ind_date <- '1975-01-01'

df_ind <- all_three |> 
  filter(date > cutoff_a,
         date < cutoff_b) |> 
  arrange(date) |> 
  group_by(commodity) |> 
  mutate(index = (value/first(value))*100,
         index_2 = ifelse(date == ind_date, value, 0),
         index_2 = sum(index_2),
         index_2 = (value/index_2)*100)


ggplot() +
  geom_line(data = df_ind, aes(x = date, y = index_2, color = commodity), size = 1, alpha = .7) +
  theme_minimal() +
  labs(x = 'Date', y = 'PRICE RECEIVED INDEX (1970 = 100)') +
  scale_color_manual(values = c('#BB0000','#13294B','#FC7300'), name = 'Commodity')


library(showtext)
font_add_google("IBM Plex Mono", "gfont"); showtext_auto()

ggplot() +
  geom_line(data = df_ind, aes(x = date, y = index_2, color = commodity), size = 1, alpha = .7) +
  theme_minimal(base_family = 'gfont', base_size = 32) +
  labs(x = 'DATE', y = 'PRICE RECEIVED INDEX (1970 = 100)') +
  scale_x_date(breaks = '1 year', labels= scales::date_format("%Y")) +
  scale_color_manual(values = c('#BB0000','#13294B','#FC7300'), name = 'COMMODITY') +
  theme(legend.title = element_text(face = 'bold', hjust = .5),
        axis.title = element_text(face = 'bold'))

