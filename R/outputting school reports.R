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

source("R/import and clean data.R")

hbsc2022 |> 
  filter(school_level == "Secondary") |> 
  pull(SCHOOL_number) |> 
  unique() |> 
  sample(2) |> 
  map(function(school) {
    render("secondary_report_template.Rmd", output_file = paste0("pilot_out/school_", school, ".docx"),
           params = list(school = school, censor = TRUE))
  })
