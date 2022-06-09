library(rmarkdown)


source("R/import and clean data.R")

hbsc2022 |> 
  filter(school_level == "Secondary") |> 
  pull(SCHOOL_number) |> 
  unique() |> 
  sample(5) |> 
  map(function(school) {
    render("secondary_report_template.Rmd", output_file = paste0("pilot_out/school_", school, ".docx"),
           params = list(school = school, censor = TRUE))
  })
