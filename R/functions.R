bar_by_gender <-
  function(var,
           .count_var = "Yes",
           .data = school_dat) {
    require(tidyverse)
    require(scales)
    
    var <- enquo(var)
    .data %>%
      group_by(!!var, sex) %>%
      count() %>%
      group_by(sex) %>%
      mutate(
        denom = sum(n),
        prop =  n / denom,
        perc_label = paste0(round(100 * prop, 0),
                            "%")
      ) %>%
      mutate(text_y = if_else(prop < 0.9, prop + 0.1, prop - 0.1)) %>%
      filter(!!var == .count_var) %>%
      ggplot(aes(sex, prop, fill = sex)) +
      geom_bar(stat = "identity") +
      scale_y_continuous(
        "Percentage",
        labels = percent_format(accuracy = 1),
        breaks = c(0, 0.5, 1),
        minor_breaks = seq(0, 1, 0.1),
        limits = c(0, 1)
      ) +
      geom_text(aes(y = text_y, label = perc_label),
                colour = "#121212",
                size = 8) +
      theme(
        axis.title.y = element_text(angle = 0, vjust = 0.5),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.title.y.left = element_text(angle = 90),
        axis.text.x = element_text(hjust = 1)
      ) +
      scale_fill_manual(values = c("Girl" = global_girls_colour,
                                   "Boy" = global_boys_colour))
    
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
                       .data = school_dat,
                       .censor = TRUE) {
  var <- enquo(var)
  
  df_sex <- .data |>
    group_by(sex) |>
    mutate(success = !!var %in% success) |>
    summarise(numerator = sum(success),
              denom = n()) |>
    filter(!is.na(sex))
  
  if ((all(df_sex$numerator > 3) & all(df_sex$denom >= 7)) | !.censor) {
    # * chart should not be created if there are ≤3 students in the numerator of
    #   any variable.
    # * only separate by sex if there are ≥7 girls AND ≥7 boys in the denominator
    #   of any variable
    
    p1 <- df_sex |>
      mutate(prop = numerator / denom) |>
      ggplot(aes(sex, prop, fill = sex)) +
      geom_bar_t(stat = "identity") +
      scale_fill_hbsc() +
      scale_y_continuous("%", labels = percent, limits = c(0, 1))
    
  } else if (all(df_sex$numerator > 3) & sum(df_sex$denom <= 14)) {
    # * if there are ≤14 students, the chart should only present a single column
    #   representing all students.
    
    p1 <- df_sex |>
      summarise(prop = sum(numerator) / sum(denom)) |>
      ggplot(aes("All pupils", prop)) +
      geom_bar_t(stat = "identity") +
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
  
  if (length(df_school$grade) == 2 &
      all(df_school$numerator >= 7)) {
    # * for secondary schools, only separate by year if there are ≥7 S2 AND ≥7 S4).
    
    p2 <- df_school |>
      summarise(prop = sum(numerator) / sum(denom)) |>
      ggplot(aes(grade, prop, fill = grade)) +
      geom_bar_t(stat = "identity") +
      scale_fill_hbsc() +
      scale_y_continuous("%",
                         labels = percent,
                         limits = c(0, 1),
                         position = "right")
  } else {
    p2 <- NULL
  }
  
  p1 + p2
}

# test
# bar_by_cat(health, c("Good", "Excellent"))


# graphing multiple vars --------------------------------------------------


# bar_multiple_vars <- function(..., labels, success = c("More than once a week", "About every day"), .data = school_dat) {
#
#
#   vars <- enquos(...)
#
# .data |>
#   select(!!!vars) |>
#   summarise(across(everything(), ~ sum(.x %in% success)),
#             denom = n()) |>
#   pivot_longer(-denom, names_to = "var", values_to = "n") |>
#   mutate(censor = if_else(n < 3, 1, 0),
#          labels = cat_labels,
#          prop = n/denom) |>
#   ggplot(aes(fct_inorder(labels), prop)) +
#   geom_bar_t(stat = "identity") +
#   scale_fill_hbsc() +
#   scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
#   scale_y_continuous("%", labels = percent, limits = c(0, 1))
#
# }
#
# # test
#
# bar_multiple_vars(
#   headache,
#   stomachache,
#   backache,
#   dizzy,
#   feellow,
#   nervous,
#   irritable,
#   sleepdificulty,
#   labels =   c(
#     "Headache",
#     "Stomach-ache",
#     "Backache",
#     "Dizziness",
#     "Feeling low",
#     "Feeling nervous",
#     "Feeling irritable",
#     "Sleep difficulties"
#   ),
#   success = c("More than once a week", "About every day")
# )

# v2 - varslist

bar_multiple_vars <-
  function(varslist,
           success = c("More than once a week", "About every day"),
           .data = school_dat,
           .censor = TRUE) {
    
    .data |>
      select(!!!syms(names(varslist))) |>
      summarise(across(everything(), ~ sum(.x %in% success)),
                denom = n()) |>
      pivot_longer(-denom, names_to = "var", values_to = "n") |>
      rowwise() |>
      mutate(
        censored = if_else(n < 3 & .censor, 1, 0),
        labels = varslist[[var]][1],
        prop = n / denom,
        prop = if_else(censored == 1, 0.5, prop)
      ) |>
      ggplot(aes(fct_inorder(labels), prop, alpha = factor(censored), linetype = factor(censored))) +
      geom_bar_t(stat = "identity", colour = primary_colour) +
      scale_alpha_manual(values = c("1" = 0.2, "0" = 1)) +
      scale_linetype_manual(values = c("1" = "dashed", "0" = "solid")) +
      scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
      scale_y_continuous("%", labels = percent, limits = c(0, 1))
    
  }

# test

# bar_multiple_vars(
#   list(
#     headache =   "Headache",
#     stomachache =   "Stomach-ache",
#     backache =   "Backache",
#     dizzy =   "Dizziness",
#     feellow =   "Feeling low",
#     nervous =   "Feeling nervous",
#     irritable =   "Feeling irritable",
#     sleepdificulty =   "Sleep difficulties"
#   )
# )

# customising graphs ------------------------------------------------------

#' scale_fill_hbsc to globalise fill colours

scale_fill_hbsc <- \(x) scale_fill_manual(
  values = c(
    "Girls" = global_girls_colour,
    "Boys" = global_boys_colour,
    "S2" = global_s2_colour,
    "S4" = global_s4_colour
  )
)

#' Thinner geom_bar

geom_bar_t <- function (..., width = 0.5) {
  geom_bar(..., width = width)
}