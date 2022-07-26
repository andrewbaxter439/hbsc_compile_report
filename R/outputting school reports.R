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

out_dir <- "Q:\\Project Recipient Data\\HBSC 2022\\School ID\\Report tracking"
  
schools <-
  readxl::read_excel(
    "Q:\\Project Recipient Data\\HBSC 2022\\School ID\\Report tracking\\Report overview_pre Easter_27 06 22.xlsx",
    sheet = "S2 and S4 pre Easter"
  ) |>
  select(school_name = `School Name`, LA, id = `Research ID (S2)`, `Break for summer`) |>
  mutate(SCHOOL_number = str_extract(id, "(?<=^S\\d)\\d{3}(?=\\.xlsx$)"),
         .keep = "unused")

primaries_to_knit <- hbsc2022 |> 
  filter(school_level == "Primary") |> 
  count(SCHOOL_number) |> 
  arrange(n)

primary_dates <- readxl::read_excel(file.path(out_dir, "Report overview_pre Easter_27 06 22.xlsx")) |> 
  select(school_name = `School Name`, LA, id = `Research ID`, `Break for summer`) |>
  mutate(SCHOOL_number = str_extract(id, "(?<=^P7)\\d{3}"),
         .keep = "unused")

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




# render just 059 for test

render("secondary_report_template.Rmd", output_file = paste0("pilot_out/school_059_v2.docx"),
           params = list(school = "059", censor = TRUE),
       output_options = list(tables = list(conditional = list(last_row = TRUE, last_column = TRUE))))


# anonymous sampling by size ----------------------------------------------

schools_to_knit |> 
  ungroup() |> 
  mutate(size = cut_number(total, 3, labels = c("small", "medium", "large"))) |>
  group_by(size) |> 
  slice_sample(n = 2) |> 
  {\(x) map2(x$SCHOOL_number, x$size, function(school, size) {
    message("Running for school ", school)
    render("secondary_report_template.Rmd", output_file = paste0("pilot_out/", size, "_", "school_", school, ".docx"),
           params = list(school = school, censor = TRUE))
  })}()

# importing school ids ----------------------------------------------------



# interim final output ----------------------------------------------------

library(rmarkdown)

schools_to_knit |> 
  # tail(5) |> 
  left_join(schools, by = "SCHOOL_number") |> 
  filter(str_detect(`Break for summer`, "^23rd"))  |> 
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
        str_remove_all(df$LA, "[:punctuation:]"),
        str_remove_all(df$school_name, "[:punctuation:]"), ".docx"
      ))
    )
    
  })


# trialling primary reports -----------------------------------------------


primaries_to_knit |> 
  # filter(n > 13) |> 
  inner_join(
    primary_dates |> 
      filter(str_detect(`Break for summer`, "24", negate = TRUE))
  ) |> 
  sample_n(1) |>
  pull(SCHOOL_number) |> 
  map(function(school) {
    message("Running for school ", school)
    render("primary_report_template.Rmd", output_file = paste0("pilot_out/primary_school_", school, ".docx"),
           params = list(school = school, censor = TRUE))
  })


# render primaries by name ------------------------------------------------


primaries_to_knit |> 
  # filter(n > 13) |> 
  inner_join(
    primary_dates |> 
      filter(str_detect(`Break for summer`, "24", negate = TRUE))
  ) |> 
  rowwise() |>
  group_walk(function(df, key) {
    
    dir_write <- file.path(out_dir, "primary report outputs for checking", df$LA)
    
    if (!dir.exists(dir_write)) {
      dir.create(dir_write)
    }
    
    render(
      "primary_report_template.Rmd",
      params = list(
        school = df$SCHOOL_number,
        school_name = df$school_name,
        censor = TRUE
      ),
      output_file = file.path(dir_write, paste0(
        str_remove_all(df$LA, "[:punctuation:]"), " - ",
        str_remove_all(df$school_name, "[:punctuation:]"), "_v2.docx"
      ))
    )
    
  })


# Extra knits


primaries_to_knit |>
  inner_join(primary_dates |>
               filter(SCHOOL_number == "040")) |>
  rowwise() |>
  group_walk(function(df, key) {
    dir_write <-
      file.path(out_dir, "primary report outputs for checking", df$LA)
    
    if (!dir.exists(dir_write)) {
      dir.create(dir_write)
    }
    
    render(
      "primary_report_template.Rmd",
      params = list(
        school = df$SCHOOL_number,
        school_name = df$school_name,
        censor = TRUE
      ),
      output_file = file.path(
        dir_write,
        paste0(
          str_remove_all(df$LA, "[:punctuation:]"),
          " - ",
          str_remove_all(df$school_name, "[:punctuation:]"),
          "_v3.docx"
        )
      )
    )
    
  })

schools_to_knit |>
  left_join(schools, by = "SCHOOL_number") |>
  filter(SCHOOL_number == "084") |>
  rowwise() |>
  group_walk(function(df, key) {
    dir_write <-
      file.path(out_dir, "report outputs for checking", df$LA)
    
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
      output_file = file.path(
        dir_write,
        paste0(
          str_remove_all(df$LA, "[:punctuation:]"),
          " - ",
          str_remove_all(df$school_name, "[:punctuation:]"),
          "_v3.docx"
        )
      )
    )
    
  })

