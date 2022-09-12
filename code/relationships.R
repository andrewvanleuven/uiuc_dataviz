library(tidyverse)

# Import, clean, and merge data -------------------------------------------

rucc <- read_csv('data/rucc.csv') |> 
  mutate(county = str_replace(cty,' County',''),
         county = str_replace(county,'DeKalb','Dekalb'),
         county = str_replace(county,'De Witt','Dewitt'),
         county = str_replace(county,'St. Clair','Saint Clair'),
         county = str_replace(county,'DuPage','Dupage')) |> 
  filter(st == 'IL') |> 
  select(county,rucc)

# https://nces.ed.gov/ccd/files.asp
nces <- read_csv('data/nces_edge.csv') |> 
  janitor::clean_names() |> 
  mutate(uid = paste(sch_name, mcity, sep = '-')) |> 
  select(uid, zip = mzip)

# https://www.isbe.net/ilreportcarddata
report_card <- readxl::read_xlsx('data/il_report_card.xlsx') |> 
  janitor::clean_names() |> 
  select(1:9,number_student_enrollment,percent_student_enrollment_low_income,teacher_retention_rate,avg_class_size_all_grades,
         number_crdc_incidents_of_violence,percent_crdc_incidents_of_violence,high_school_4_year_graduation_rate_total) |> 
  filter(type == 'School') |> 
  mutate(school_name = str_replace(school_name, 'Schoo$','School'),
         school_name = str_replace(school_name, 'Scho$','School'),
         uid = paste(school_name, city, sep = '-')) |> 
  left_join(nces, by = 'uid') |> 
  select(uid,zip,everything()) 

nrow(report_card |> filter(is.na(zip))) # some error in merging; only working with ~93% of schools

acs <- tidycensus::get_acs(geography = 'zcta', 
                           variables = 'B19013_001', 
                           state = 'IL', 
                           year = 2018) |> 
  mutate(zip = as.numeric(GEOID)) |> 
  select(zip, med_income = estimate) # be aware that we're not really worrying about M.O.E.


schools <- report_card |> 
  left_join(rucc, by = 'county') |> 
  left_join(acs, by = 'zip') |> 
  filter(!is.na(zip)) |> 
  filter(!is.na(med_income)) |> 
  mutate(rural = as.factor(ifelse(rucc>3,'Non-Metropolitan','Metropolitan')))

# Visualize some relationships --------------------------------------------

schools |>
  filter(school_type %in% 'HIGH SCHOOL') |>
  filter(!is.na(rural)) |>
  ggplot() +
  aes(
    x = percent_student_enrollment_low_income/100,
    y = high_school_4_year_graduation_rate_total/100,
    # color = rural
  ) +
  geom_point(shape = "circle", size = 1.5) +
  # geom_smooth() + 
  # geom_smooth(method='lm', formula= y~x) +
  labs(x = 'Percent Low Income', y = 'Four-Year Graduation Rate', color = '',
       title = 'Student Income Levels and Graduation Rates') +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(labels = scales::percent) +
  scale_color_manual(values = c('dodgerblue','red')) +
  theme(plot.title = element_text(face = 'bold', hjust = .5, size = 18),
        axis.title = element_text(face = 'bold'))

schools |> 
  filter(school_type %in% c('ELEMENTARY','MIDDLE SCHL','HIGH SCHOOL'),
         !is.na(rural), teacher_retention_rate > 10) |> 
  ggplot() +
  aes(x = number_crdc_incidents_of_violence,
      y = teacher_retention_rate/100,
      # color = as.factor(school_type)
      ) +
  geom_point(shape = "circle", size = 1.5, alpha = .7) +
  geom_smooth(method='lm', formula= y~x) +
  scale_y_continuous(labels = scales::percent) +
  labs( x = '# of School Violence Incidents', y = 'Teacher Retention Rate',
        title = 'School Violence and Teacher Retention') +
  theme_minimal() +
  scale_color_manual(values = c('red','dodgerblue','forestgreen'), name = 'School Type',
                     labels = c('Elementary','Middle School','High School')) +
  theme(legend.title = element_text(face = 'bold', hjust = .5),
        plot.title = element_text(face = 'bold', hjust = .5, size = 18),
        axis.title = element_text(face = 'bold'))

schools |>
  filter(school_type %in% c('ELEMENTARY','MIDDLE SCHL','HIGH SCHOOL'), !is.na(rural)) |>
  ggplot() +
  aes(x = avg_class_size_all_grades, 
      y = med_income, 
      # colour = rural
      ) +
  geom_point(shape = "circle", size = 1.5, alpha = .6) +
  geom_smooth(method='lm', formula= y~x) +
  scale_color_manual(values = c('dodgerblue','red'), name = '') +
  labs(x = 'Average Class Size', y = 'Median Household Income',
       title = 'Class Size and Local Household Incomes') +
  scale_y_continuous(labels = scales::dollar) +
  theme_minimal() +
  theme(legend.title = element_text(face = 'bold', hjust = .5),
        plot.title = element_text(face = 'bold', hjust = .5, size = 18),
        axis.title = element_text(face = 'bold'))

