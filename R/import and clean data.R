# new 2022 data -----------------------------------------------------------

library(haven)
library(tidyverse)

if (Sys.getenv("data_dir") == "") {
  data_dir <- readline("Please specify a data file directory: ")
  Sys.setenv("data_dir", data_dir)
}

hbsc2022 <- read_spss(Sys.getenv("data_dir")) |> 
  mutate(across(where(is.labelled), as_factor),
         school_id = str_extract(School_filename, "^\\w*"))