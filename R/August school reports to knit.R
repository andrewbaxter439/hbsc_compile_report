# global setup ------------------------------------------------------------

library(rmarkdown)
library(readr)
library(tidyverse)
library(scales)
library(flextable)
library(officer)
library(haven)
library(patchwork)

primary_colour <-  "#C00000"
secondary_colour <- "#016bb2"
main_colour <- "#333333"
global_all_pupils_colour <- "#C00000"
global_girls_colour <- "#2F5597"
global_boys_colour <- "#DAE3F3"
global_s2_colour <- "#548235"
global_s4_colour <- "#C5E0B4"
global_good_colour <- "#8d96a3"
global_excel_colour <- "#00798c"

source("R/import and clean data.R")
source("R/functions.R")




theme_set(theme_minimal() +
            theme(text = element_text(colour = "black"),
                  axis.text.y = element_text(size = 10),
                  axis.text.x = element_text(size = 12),
                  legend.text = element_text(size = 12),
                  legend.key.size = unit(0.4, "cm"),
                  line = element_line(colour = main_colour),
                  panel.grid.major.x = element_blank(),
                  panel.grid.minor.x = element_blank(),
                  # panel.grid.major.y = element_line(colour = "grey"),
                  axis.title.y = element_text(margin = margin(r = 10, unit = "pt")),
                  axis.title.x = element_blank(),
                  legend.position = "none",
                  strip.text = element_blank()
            ))

update_geom_defaults("bar", list(fill = primary_colour))

# import all data

schools_to_knit <- hbsc2022 |> 
  filter(school_level == "Secondary") |> 
  group_by(SCHOOL_number, Grade) |> 
  tally() |> 
  pivot_wider(names_from = Grade, values_from = n) |> 
  mutate(total = sum(`Secondary 2`, `Secondary 4`, na.rm = FALSE)) |> 
  filter(!is.na(total), if_all(starts_with("Secondary"), ~.x > 5)) |> 
  arrange(total)

out_dir <- "Q:/Project Recipient Data/HBSC 2022/School ID/Report tracking/Reports for checking August 2022"

secondaries_to_knit <-
  readxl::read_excel(
    "Q:\\Project Recipient Data\\HBSC 2022\\School ID\\Report tracking\\Report overview 020822.xlsx",
    sheet = "Secondary schools"
  ) |>
  select(school_name = Name, LA, id = `School iD`) |>
  mutate(SCHOOL_number = str_extract(id, "(?<=^S)\\d{3}"),
         .keep = "unused")


primaries_to_knit <-  readxl::read_excel(
  "Q:\\Project Recipient Data\\HBSC 2022\\School ID\\Report tracking\\Report overview 020822.xlsx",
  sheet = "Primary schools"
) |>
  select(school_name = Name, LA, id = `School iD`) |>
  mutate(SCHOOL_number = str_extract(id, "(?<=^P7)\\d{3}"),
         .keep = "unused")


# function_idea -----------------------------------------------------------


secondaries_to_knit |> 
  write_reports(out_dir = out_dir)
