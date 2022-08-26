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

out_dir <- Sys.getenv("out_dir")


# Read files for school names --------------------------------------------------

secondaries_to_knit <-
  readxl::read_excel(
    file.path(out_dir, "Report overview 020822.xlsx"),
    sheet = "Secondary schools"
  ) |>
  select(school_name = Name, LA, id = `School iD`) |>
  mutate(SCHOOL_number = str_extract(id, "(?<=^S)\\d{3}"),
         .keep = "unused") |> 
bind_rows(
  readxl::read_excel(
    file.path(out_dir, "Report overview 020822.xlsx"),
    sheet = "Pre-Easter secondary"
  ) |>
  select(school_name = Name, LA, id = `School iD`) |>
  mutate(SCHOOL_number = str_extract(id, "(?<=^S)\\d{3}"),
         .keep = "unused")
)


primaries_to_knit <-  readxl::read_excel(
  file.path(out_dir, "Report overview 020822.xlsx"),
  sheet = "Primary schools"
) |>
  select(school_name = Name, LA, id = `School iD`) |>
  mutate(SCHOOL_number = str_extract(id, "(?<=^P7)\\d{3}"),
         .keep = "unused")


# Knit all by template type -----------------------------------------------------------

secondaries_to_knit |> 
  write_reports(template = "secondary_report_template.Rmd", out_dir = file.path(out_dir, "Reports for checking August 2022"), folder = "Secondary")

primaries_to_knit |>
  write_reports(template = "primary_report_template.Rmd", out_dir = file.path(out_dir, "Reports for checking August 2022"), folder = "Primary")


# Knit all needing a new version ---------------------------------------------

schools_missing_sex <- hbsc2022 |> 
  count(SCHOOL_number, school_level, sex) |> 
  pivot_wider(names_from = sex, values_from = n) |> 
  filter(`NA` > 0) |> 
  arrange(desc(`NA`))

inner_join(secondaries_to_knit, schools_missing_sex |> filter(school_level == "Secondary"), by = "SCHOOL_number") |> select(-school_name, - LA) |>  arrange(desc(`NA`))
inner_join(primaries_to_knit, schools_missing_sex |> filter(school_level == "Primary"), by = "SCHOOL_number") |> select(-school_name, - LA) |>  arrange(desc(`NA`))


hbsc2022 |> 
  count(SCHOOL_number, sex) |> 
  pivot_wider(names_from = sex, values_from = n) |> 
  arrange(desc(`NA`))



# redoing 066 -------------------------------------------------------------

readxl::read_excel(
  file.path(out_dir, "Report overview 020822.xlsx"),
  sheet = "Reports under consideration"
) |>
  select(school_name = Name, LA, id = `School iD`) |> 
  mutate(SCHOOL_number = str_extract(id, "(?<=^P7)\\d{3}"),
         .keep = "unused") |> 
  filter(SCHOOL_number == "066") |> 
  write_reports(template = "primary_report_template.Rmd", out_dir = file.path(out_dir, "To send June 2022"), folder = "")



# final run of schools ----------------------------------------------------

readxl::read_excel(
  file.path(out_dir, "Report overview JULY 2022.xlsx"),
  sheet = "Primary school"
) |>
  select(school_name = Name, LA, id = `School iD`) |> 
  mutate(SCHOOL_number = str_extract(id, "(?<=^P7)\\d{3}"),
         .keep = "unused") |> 
  filter(SCHOOL_number %in% c("060", "077", "084", "020", "042", "037", "046")) |> 
  write_reports(template = "primary_report_template.Rmd", out_dir = file.path(out_dir, "Reports for checking August 2022"), folder = "Rerun reports with small numbers")



readxl::read_excel(
  file.path(out_dir, "Report overview JULY 2022.xlsx"),
  sheet = "Secondary "
) |>
  select(school_name = Name, LA, id = `School iD`) |> 
  mutate(SCHOOL_number = str_extract(id, "(?<=^S)\\d{3}"),
         .keep = "unused") |> 
  filter(SCHOOL_number %in% c("095", "045")) |> 
  write_reports(template = "secondary_report_template.Rmd", out_dir = file.path(out_dir, "Reports for checking August 2022"), folder = "Rerun reports with small numbers")


readxl::read_excel(
  file.path(out_dir, "Report overview 020822.xlsx"),
  sheet = "Reports under consideration"
) |>
  select(school_name = Name, LA, id = `School iD`) |> 
  mutate(SCHOOL_number = str_extract(id, "(?<=^P7)\\d{3}"),
         .keep = "unused") |> 
  filter(SCHOOL_number != "066") |> 
  write_reports(template = "primary_report_template.Rmd", out_dir = file.path(out_dir, "Reports for checking August 2022"), folder = "Rerun reports with small numbers")


# rerun for no gender split -----------------------------------------------

readxl::read_excel(
  file.path(out_dir, "Report overview JULY 2022.xlsx"),
  sheet = "Primary school"
) |>
  select(school_name = Name, LA, id = `School iD`) |> 
  mutate(SCHOOL_number = str_extract(id, "(?<=^P7)\\d{3}"),
         .keep = "unused") |> 
  filter(SCHOOL_number %in% c("020", "042", "037", "046")) |> 
  write_reports(template = "primary_report_template.Rmd", out_dir = file.path(out_dir, "Reports for checking August 2022"), folder = "Test")

readxl::read_excel(
  file.path(out_dir, "Report overview 020822.xlsx"),
  sheet = "Reports under consideration"
) |>
  select(school_name = Name, LA, id = `School iD`) |> 
  mutate(SCHOOL_number = str_extract(id, "(?<=^P7)\\d{3}"),
         .keep = "unused") |> 
  filter(SCHOOL_number %in% c("006", "073", "013", "061", "048")) |> 
  write_reports(template = "primary_report_template.Rmd", out_dir = file.path(out_dir, "Reports for checking August 2022"), folder = "Test")

readxl::read_excel(
  file.path(out_dir, "Report overview 020822.xlsx"),
  sheet = "Reports under consideration"
) |>
  select(school_name = Name, LA, id = `School iD`) |> 
  mutate(SCHOOL_number = str_extract(id, "(?<=^P7)\\d{3}"),
         .keep = "unused") |> 
  filter(SCHOOL_number != "066") |> 
  write_reports(template = "primary_report_template.Rmd", out_dir = file.path(out_dir, "Reports for checking August 2022"), folder = "Test")