bar_by_gender <- function(var, .count_var = "Yes", .data = school_dat) {
  require(tidyverse)
  require(scales)
  
  var <- enquo(var)
  .data %>%
    group_by(!!var, sex) %>% 
    count() %>% 
    group_by(sex) %>% 
    mutate(denom = sum(n),
           prop =  n / denom,
           perc_label = paste0(round(100 * prop, 0),
                               "%")) %>%
    mutate(text_y = if_else(prop < 0.9, prop + 0.1, prop - 0.1)) %>% 
    filter(!!var == .count_var) %>% 
    ggplot(aes(sex, prop, fill = sex)) +
    geom_bar(stat = "identity") +
    scale_y_continuous(
      "Percentage",
      labels = percent_format(accuracy = 1),
      breaks = c(0, 0.5, 1),
      minor_breaks = seq(0, 1, 0.1),
      limits = c(0,1)) +
    geom_text(aes(y = text_y, label = perc_label),
              colour = "#121212",
              size = 8) +
    theme(axis.title.y = element_text(angle = 0,vjust = 0.5),
          panel.grid.major.x = element_blank(), 
          panel.grid.minor.x = element_blank(),
          axis.title.y.left = element_text(angle = 90),
          axis.text.x = element_text(hjust = 1)) +
    scale_fill_manual(
      values = c(
        "Girl" = global_girls_colour,
        "Boy" = global_boys_colour
      )
    ) 
  
}

#' * chart should not be created if there are ≤3 students in the numerator of
#' any variable.
#' * only separate by sex if there are ≥7 girls AND ≥7 boys in the denominator
#' of any variable
#' * for secondary schools, only separate by year if there are ≥7 S2 AND ≥7 S4). 
#' * if there are ≤14 students, the chart should only present a single column
#' representing all students.
#' 
#' @param var The name of the variable to graph by
#' @param .data The school data


bar_by_cat <- function(var,
                       success = "Yes",
                       .data = school_dat) {
  var <- enquo(var)
  
  p1 <- .data |>
    group_by(sex) |>
    mutate(success = !!var %in% success) |>
    summarise(prop = sum(success) / n()) |>
    filter(!is.na(sex)) |> 
    ggplot(aes(sex, prop, fill = sex)) +
    geom_bar(stat = "identity") +
    scale_fill_hbsc() +
    scale_y_continuous("%", labels = percent, limits = c(0, 1))
  
  p2 <- .data |>
    group_by(grade) |>
    mutate(success = !!var %in% success) |>
    summarise(prop = sum(success) / n()) |>
    filter(!is.na(grade)) |> 
    ggplot(aes(grade, prop, fill = grade)) +
    geom_bar(stat = "identity") +
    scale_fill_hbsc() +
    scale_y_continuous("%", labels = percent, limits = c(0, 1), position = "right")
  
  p1 + p2
}

# denoms - check all >= 7
# school_dat |> 
#   count()

#numerators - check all > 3
# school_dat |> 
  # summarise(sum = sum(health))
