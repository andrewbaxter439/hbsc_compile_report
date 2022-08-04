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

# Read files for school names

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


# Knit all by template type -----------------------------------------------------------


secondaries_to_knit |> 
  write_reports(template = "secondary_report_template.Rmd", out_dir = out_dir, folder = "Test")

primaries_to_knit |>
  write_reports(template = "primary_report_template.Rmd", out_dir = out_dir, folder = "Test")
