# global setup ------------------------------------------------------------


knitr::opts_chunk$set(
  echo = FALSE,
  message = FALSE,
  warning = FALSE,
  error = FALSE,
  # eval = FALSE,
  fig.width = 6.2,
  fig.height = 4.1,
  fig.topcaption = TRUE,
  fig.cap.style = "Caption",
  tab.cap.style = "Caption",
  dpi = 300
)
library(rmarkdown)
library(readr)
library(tidyverse)
library(scales)
library(flextable)
library(officer)
library(haven)
library(patchwork)

primary_colour <-  "#2e3192"
secondary_colour <- "#016bb2"
main_colour <- "#333333"
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


# anonymous test sampling -------------------------------------------------


hbsc2022 |> 
  filter(school_level == "Secondary") |> 
  pull(SCHOOL_number) |> 
  unique() |> 
  sample(5) |> 
  map(function(school) {
    message("Running for school ", school)
    render("secondary_report_template.Rmd", output_file = paste0("pilot_out/school_", school, ".docx"),
           params = list(school = school, censor = TRUE))
  })


# knitting secondaries with complete s2 and s4 ----------------------------



schools_to_knit <- hbsc2022 |> 
  filter(school_level == "Secondary") |> 
  group_by(SCHOOL_number, Grade) |> 
  tally() |> 
  pivot_wider(names_from = Grade, values_from = n) |> 
  mutate(total = sum(`Secondary 2`, `Secondary 4`, na.rm = FALSE)) |> 
  filter(!is.na(total)) |> 
  arrange(total)


# anonymous sampling by size ----------------------------------------------

schools_to_knit |> 
  ungroup() |> 
  mutate(size = as.numeric(cut_number(total, 4))) |> 
  group_by(size) |> 
  slice_sample(n = 2) |> 
  {\(x) map(x$SCHOOL_number, function(school) {
    message("Running for school ", school)
    render("secondary_report_template.Rmd", output_file = paste0("pilot_out/school_", school, ".docx"),
           params = list(school = school, censor = TRUE))
  })}()

# importing school ids ----------------------------------------------------

out_dir <- "Q:\\Project Recipient Data\\HBSC 2022\\School ID\\Report tracking"
  
schools <-
  readxl::read_excel(
    "Q:\\Project Recipient Data\\HBSC 2022\\School ID\\Report tracking\\Report overview_pre Easter.xlsx",
    sheet = "S2 and S4 pre Easter"
  ) |>
  select(school_name = `School Name`, LA, id = `Research ID (S2)`) |>
  mutate(SCHOOL_number = str_extract(id, "(?<=^S\\d)\\d{3}(?=\\.xlsx$)"),
         .keep = "unused")


# interim final output ----------------------------------------------------

library(rmarkdown)

schools_to_knit |> 
  tail(5) |> 
  left_join(schools, by = "SCHOOL_number") |> 
  rowwise() |>
  group_walk(function(df, key) {
    
    dir_write <- file.path(out_dir, "draft report outputs", df$LA)
    
    if (!dir.exists(dir_write)) {
      dir.create(dir_write)
    }
    
    render(
      "secondary_report_template.Rmd",
      params = list(
        school = df$SCHOOL_number,
        school_name = df$school_name,
        censor = TRUE
      ),
      output_file = file.path(dir_write, paste0(
        str_remove_all(df$LA, "[:punctuation:]"), " - ",
        str_remove_all(df$school_name, "[:punctuation:]"), ".docx"
      ))
    )
    
  })
  