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
  
  df_sex <- .data |>
    group_by(sex) |>
    mutate(success = !!var %in% success) |>
    summarise(numerator = sum(success),
              denom = n()) |>
    filter(!is.na(sex))
  
  if (all(df_sex$numerator > 3) & all(df_sex$denom >= 7)) {
    # * chart should not be created if there are ≤3 students in the numerator of
    #   any variable.
    # * only separate by sex if there are ≥7 girls AND ≥7 boys in the denominator
    #   of any variable
    
    p1 <- df_sex |>
      mutate(prop = numerator / denom) |>
      ggplot(aes(sex, prop, fill = sex)) +
      geom_bar(stat = "identity") +
      scale_fill_hbsc() +
      scale_y_continuous("%", labels = percent, limits = c(0, 1))
    
  } else if (all(df_sex$numerator > 3) & sum(df_sex$denom <= 14)) {
    # * if there are ≤14 students, the chart should only present a single column
    #   representing all students.
    
    p1 <- df_sex |>
      summarise(prop = sum(numerator) / sum(denom)) |>
      ggplot(aes("All pupils", prop)) +
      geom_bar(stat = "identity") +
      scale_fill_hbsc() +
      scale_y_continuous("%", labels = percent, limits = c(0, 1))
    
  } else {
    p1 <- ggplot() +
      geom_text(aes(x = 1, y = 0.5, label = "Chart ommitted\ndue to low numbers"),
                size = 12) +
      scale_x_discrete(breaks = 1, labels = "") +
      scale_y_continuous("%", labels = percent, limits = c(0, 1))
    
  }
  
  df_school <- .data |>
    group_by(grade) |>
    mutate(success = !!var %in% success) |>
    summarise(numerator = sum(success),
              denom = n()) |>
    filter(!is.na(grade))
  
  if (all(df_school$numerator >= 7)) {
    # * for secondary schools, only separate by year if there are ≥7 S2 AND ≥7 S4). 
    
    p2 <- df_school |> 
      summarise(prop = sum(numerator) / sum(denom)) |>
      ggplot(aes(grade, prop, fill = grade)) +
    geom_bar(stat = "identity") +
    scale_fill_hbsc() +
    scale_y_continuous("%", labels = percent, limits = c(0, 1), position = "right")
  } else {
    p2 <- NULL
  }
  
  p1 + p2
}

# test
# bar_by_cat(health, c("Good", "Excellent"))

