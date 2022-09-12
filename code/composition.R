# install.packages('treemapify')

library(tidyverse); library(treemapify)

# https://www.nass.usda.gov/Quick_Stats/CDQT/chapter/1/table/35/state/IL

ag_census <- read_csv('data/nass_agcensus_il.csv') |> 
  janitor::clean_names() |> 
  filter(str_detect(data_item,'ACRES HARVESTED'),
         is.na(domain_category)) |> 
  select(1,5,6,8) |> 
  mutate(value = str_replace_all(value,',',''),
         value = as.numeric(ifelse(str_detect(value,'D'),0,value))) |> 
  group_by(commodity) |> 
  filter(value == max(value)) |> 
  ungroup() |> 
  filter(value > 0) |> 
  select(-data_item) |> 
  distinct() |> 
  arrange(desc(value)) |> 
  filter(commodity != 'HAY',
         commodity != 'HAYLAGE') |> # removes "HAY" and "HAYLAGE" because "HAY AND HAYLAGE" already in the data
  slice(1:8)

# Make a treemap ----------------------------------------------------------
pal <- c("#ffffb3","#fb8072","#80b1d3","#b3de69","#fccde5","#8dd3c7","#fdb462","#bebada")
# https://colorbrewer2.org/#type=qualitative&scheme=Set3&n=8

ggplot(ag_census, aes(area = value, fill = commodity, label = commodity,
                 subgroup = commodity)) +
  geom_treemap(color = 'black', size = .75) +
  geom_treemap_subgroup_border(color = 'black', size = .5) +
  geom_treemap_subgroup_text(place = "centre", grow = F, alpha = 0.85, colour = "black",
                             min.size = 0) +
  theme(legend.position = 0) +
  scale_fill_manual(values = pal)

