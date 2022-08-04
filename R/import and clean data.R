# new 2022 data -----------------------------------------------------------

library(haven)
library(tidyverse)

if (Sys.getenv("data_dir") == "") {
  dir_entry <- readline("Please specify a data file directory: ")
  Sys.setenv(data_dir =  dir_entry)
}

file_name <- "Nat_SCT22 V1  2_8_22.sav"

if (file_name %in% dir(Sys.getenv("data_dir"))) {
  cat(paste0("Processing ", file_name))
} else {
  files_in_folder <- dir(Sys.getenv("data_dir")) |> 
    str_subset("Nat") 
  
  files_in_folder |> 
    iwalk(~ cat("[", .y, "]\t", .x, sep = ""))
  
  file_num <- readline("Please choose a data file:")
  file_name <- files_in_folder[as.numeric(file_num)]
}

hbsc2022 <- read_spss(file.path(Sys.getenv("data_dir"), file_name)) |> 
  mutate(across(where(is.labelled), as_factor),
         school_id = str_extract(School_filename, "^\\w*"))
