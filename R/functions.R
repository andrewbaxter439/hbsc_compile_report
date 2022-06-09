# graphing prevalence of single var ---------------------------------------

#' * chart should not be created if there are ≤3 students in the numerator of
#' any variable.
#' * only separate by sex if there are ≥7 girls AND ≥7 boys in the denominator
#' of any variable
#' * for secondary schools, only separate by year if there are ≥7 S2 AND ≥7 S4).
#' * if there are ≤14 students, the chart should only present a single column
#' representing all students.
#'
#' @param var The name of the variable to graph by
#' @param success Character vector of responses to be counted as 'successes'
#' @param .data Data file to use
#' @param .censor Whether to censor low numbers (default as TRUE for final
#'   output)


bar_by_cat <- function(var,
                       success = "Yes",
                       .data = school_dat,
                       .censor = TRUE) {
  var <- enquo(var)
  
  df_sex <- .data |>
    group_by(sex) |>
    mutate(success = !!var %in% success) |>
    summarise(numerator = sum(success, na.rm = TRUE),
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
      scale_y_continuous("%", labels = percent, limits = c(0, 1))+
      geom_text(aes(label = percent(prop, suffix="", accuracy = 1)),
                vjust = 0, 
                nudge_y = 0.05,
                size = 4)
    
  } else if (all(df_sex$numerator > 3) & sum(df_sex$denom <= 14)) {
    # * if there are ≤14 students, the chart should only present a single column
    #   representing all students.
    
    p1 <- df_sex |>
      summarise(prop = sum(numerator) / sum(denom)) |>
      ggplot(aes("All pupils", prop)) +
      geom_bar_t(stat = "identity") +
      scale_fill_hbsc() +
      scale_y_continuous("%", labels = percent, limits = c(0, 1))+
      geom_text(aes(label = percent(prop, suffix="", accuracy = 1)),
                vjust = 0, 
                nudge_y = 0.05,
                size = 4)
    
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
              denom = n(),
              .groups = "keep") |>
    filter(!is.na(grade))
  
  if ((length(df_school$grade) == 2 & all(df_sex$numerator > 3) & all(df_sex$denom >= 7) &
      all(df_school$numerator >= 7) | (length(df_school$grade) == 2 & .censor == FALSE))) {
    # * for secondary schools, only separate by year if there are ≥7 S2 AND ≥7 S4).
    
    p2 <- df_school |>
      summarise(prop = sum(numerator) / sum(denom)) |>
      ggplot(aes(grade, prop, fill = grade)) +
      geom_bar_t(stat = "identity") +
      scale_fill_hbsc() +
      scale_y_continuous(" ",
                         labels = percent,
                         position = "right",
                         limits = c(0, 1)
                         ) +
      theme(axis.ticks.y = element_line(colour = "white"),
            axis.text.y = element_text(colour = "white")) +
      geom_text(aes(label = percent(prop, suffix="", accuracy = 1)),
                vjust = 0, 
                nudge_y = 0.05,
                size = 4)
  } else {
    p2 <- NULL
  }
  
  p1 + p2
}

# graphing mean of single var ---------------------------------------

#' * chart should not be created if there are ≤3 students in the numerator of
#' any variable.
#' * only separate by sex if there are ≥7 girls AND ≥7 boys in the denominator
#' of any variable
#' * for secondary schools, only separate by year if there are ≥7 S2 AND ≥7 S4).
#' * if there are ≤14 students, the chart should only present a single column
#' representing all students.
#'
#' @param var The name of the variable to graph by
#' @param .data Data file to use
#' @param .censor Whether to censor low numbers (default as TRUE for final
#'   output)


bar_mean_by_cat <- function(var,
                       .data = school_dat,
                       .censor = TRUE,
                       ymax = max(.data[[rlang::as_name(var)]], na.rm = TRUE),
                       ylab = "Mean") {
  require(rlang)
  var <- enquo(var)
  
  rlang::eval_tidy(var, data = school_dat)
  
  df_sex <- .data |>
    group_by(sex) |>
    summarise(mean_var = mean(!!var, na.rm = TRUE),
              denom = n()) |>
    filter(!is.na(sex))
  
  # max_var <- .data |> 
  #   summarise(max_var = max(!!var)) |> 
  #   pull(max_var)
  
  if (all(df_sex$denom >= 7) | !.censor) {
    # * chart should not be created if there are ≤3 students in the numerator of
    #   any variable.
    # * only separate by sex if there are ≥7 girls AND ≥7 boys in the denominator
    #   of any variable
    
    p1 <- df_sex |>
      ggplot(aes(sex, mean_var, fill = sex)) +
      geom_bar_t(stat = "identity") +
      scale_fill_hbsc() +
      scale_y_continuous(ylab, limits = c(0, ymax))+
      geom_text(aes(label = round(mean_var, 1)),
                vjust = 0, 
                nudge_y = 0.05 * ymax,
                size = 4)
    
  } else if (all(df_sex$denom > 3)) {
    # * if there are ≤14 students, the chart should only present a single column
    #   representing all students.
    
    p1 <- df_sex |>
      ggplot(aes("All pupils", mean_var)) +
      geom_bar_t(stat = "identity") +
      scale_fill_hbsc() +
      scale_y_continuous(ylab, limits = c(0, ymax))+
      geom_text(aes(label = round(mean_var, 1)),
                vjust = 0, 
                nudge_y = 0.05 * ymax,
                size = 4)
    
  } else {
    p1 <- ggplot() +
      geom_text(aes(x = 1, y = 0.5, label = "Chart ommitted\ndue to low numbers"),
                size = 12) +
      scale_x_discrete(breaks = 1, labels = "") +
      scale_y_continuous(ylab, breaks = 0:1, limits = c(0, ymax))
    
  }
  
  df_school <- .data |>
    group_by(grade) |>
    summarise(mean_var = mean(Schooldays_sleep_hrs, na.rm = TRUE),
              denom = n()) |>
    filter(!is.na(grade))
  
  if ((length(df_school$grade) == 2 & all(df_sex$denom > 3) &
       all(df_school$numerator >= 7) | (length(df_school$grade) == 2 & .censor == FALSE))) {
    # * for secondary schools, only separate by year if there are ≥7 S2 AND ≥7 S4).
    
    p2 <- df_school |>
      ggplot(aes(grade, mean_var, fill = grade)) +
      geom_bar_t(stat = "identity") +
      scale_fill_hbsc() +
      scale_y_continuous(" ",
                         position = "right",
                         limits = c(0, ymax)
      ) +
      theme(axis.ticks.y = element_line(colour = "white"),
            axis.text.y = element_text(colour = "white")) +
      geom_text(aes(label = round(mean_var, 1)),
                vjust = 0, 
                nudge_y = 0.05 * ymax,
                size = 4)
  } else {
    p2 <- NULL
  }
  
  p1 + p2
}



# test

# bar_mean_by_cat(Schooldays_sleep_hrs, .censor = FALSE)

# graphing multiple vars --------------------------------------------------

#' Graphing multiple variables as bars with percentage prevalence in each
#' category.
#'
#' @param varslist A named list of variables to use and the corresponding axis
#'   titles
#' @param success Character vector of responses to be counted as 'successes'
#' @param .data Data file to use
#' @param .censor Whether to censor low numbers (default as TRUE for final
#'   output)

bar_multiple_vars <-
  function(varslist,
           success = c("More than once a week", "About every day"),
           group = c("none", "grade", "sex"),
           .data = school_dat,
           .censor = TRUE) {
    
    group <- match.arg(group)
    
    .data |>
    mutate(grouping = case_when(
      group == "none" ~ "1",
      group == "sex" ~ as.character(sex),
      group == "grade" ~ as.character(grade)
    )) |> 
      group_by(grouping) |> 
      select(!!!syms(names(varslist))) |>
      summarise(across(everything(), ~ sum(.x %in% success)),
                denom = n()) |>
      pivot_longer(-c(grouping, denom), names_to = "var", values_to = "n") |> 
      rowwise() |>
      mutate(
        censored = if_else(n < 3 & .censor, 1, 0),
        labels = varslist[[var]][1],
        prop = n / denom,
        prop = if_else(censored == 1, 1, prop),
        bar_lab_main = if_else(censored == 1, "", percent(prop, suffix="", accuracy = 1)),
        bar_lab_cens = if_else(censored == 1, "Numbers too low to show", ""),
        grouping = factor(grouping, levels = c("Girls", "Boys", "S2", "S4", "1"))
      ) |>
      filter(!is.na(grouping)) |> 
      ggplot(aes(fct_inorder(labels), prop, alpha = factor(censored), linetype = factor(censored), fill = grouping, colour = grouping)) +
      geom_bar_t(stat = "identity", position = position_dodge(width = 0.6)) +
      scale_alpha_manual(values = c("1" = 0.2, "0" = 1), guide = guide_none()) +
      scale_linetype_manual(values = c("1" = "dashed", "0" = "solid"), guide = guide_none()) +
      scale_x_discrete(guide = guide_axis(angle = 45)) +
      scale_fill_hbsc(aesthetics = c("fill", "colour"), name = "",  limits = force) +
      theme(legend.position = if_else(group == "none", "none", "bottom")) +
      scale_y_continuous("%", labels = percent, limits = c(0, 1)) +
      geom_text(aes(label = bar_lab_main),
                vjust = -0.5, 
                # nudge_y = 0.05,
                colour = "black",
                position = position_dodge(width = 0.6),
                size = 4) +
      geom_text(aes(label = bar_lab_cens, y = 0.5),
                # nudge_y = 0.05,
                vjust = 0.5,
                angle = 90,
                colour = "black",
                position = position_dodge(width = 0.6),
                size = 4)
    
  }

# test
# 
# bar_multiple_vars(
#   varslist = list(
#     ls_teamsp_wk = "Team sports",
#     ls_indsport_wk = "Individual sports",
#     ls_arts_wk = "Artistic activities",
#     ls_youth_wk = "Youth organisation",
#     ls_club_wk = "Club activity",
#     ls_relig_wk = "Religious activity"
#   ),
#   success = "At least weekly",
#   group = "sex",
#   .censor = TRUE
# )

# mean multiple vars --------------------------------------------------

#' Graphing multiple variables as bars with percentage prevalence in each
#' category.
#'
#' @param varslist A named list of variables to use and the corresponding axis
#'   titles
#' @param .data Data file to use
#' @param .censor Whether to censor low numbers (default as TRUE for final
#'   output)

bar_mean_multiple_vars <-
  function(varslist,
           success = c("More than once a week", "About every day"),
           group = c("none", "grade", "sex"),
           .data = school_dat,
           .censor = TRUE,
           limits = c(`Poor quality` = 1,
                      `High quality` = 6),
           ymax = limits[2],
           ylab = "Mean") {
    
    group <- match.arg(group)
    
    clean_dat <- .data |>
    mutate(grouping = case_when(
      group == "none" ~ "1",
      group == "sex" ~ as.character(sex),
      group == "grade" ~ as.character(grade)
    )) |> 
      group_by(grouping) |> 
      select(!!!syms(names(varslist))) |>
      mutate(across(everything(),
                    function(score){
                         chr_score <-  as.character(score)
                         if_else(
                           chr_score %in% names(limits),
                           unname(limits[chr_score]),
                           as.numeric(chr_score)
                         )
                    })) |> 
      summarise(across(everything(),
                       function(score) {
                         mean(score, na.rm = TRUE)
                       }),
                denom = n()) |> 
      pivot_longer(-c(grouping, denom), names_to = "var", values_to = "mean") |> 
      rowwise() |>
      filter(!is.na(grouping)) |> 
      mutate(
        censored = if_else(denom < 3 & .censor, 1, 0),
        labels = varslist[[var]][1],
        mean = if_else(censored == 1, ymax/2, mean),
        bar_lab_main = if_else(censored == 1, na_dbl, round(mean, 1)),
        bar_lab_cens = if_else(censored == 1, "Numbers too low to show", ""),
        grouping = factor(grouping, levels = c("Girls", "Boys", "S2", "S4", "1"))
      )
    
      ggplot(clean_dat, aes(fct_inorder(labels), mean, alpha = factor(censored), linetype = factor(censored), fill = grouping, colour = grouping)) +
      geom_bar_t(stat = "identity", position = position_dodge(width = 0.6)) +
      scale_alpha_manual(values = c("1" = 0.2, "0" = 1), guide = guide_none()) +
      scale_linetype_manual(values = c("1" = "dashed", "0" = "solid"), guide = guide_none()) +
      scale_x_discrete(guide = guide_axis(angle = 45)) +
      scale_fill_hbsc(aesthetics = c("fill", "colour"), name = "",  limits = force) +
      theme(legend.position = if_else(group == "none", "none", "bottom")) +
      scale_y_continuous(ylab, limits = c(0, ymax)) +
      geom_text(aes(label = round(mean, 1)),
                vjust = -0.5, 
                # nudge_y = 0.05,
                colour = "black",
                position = position_dodge(width = 0.6),
                size = 4) +
        geom_text(aes(label = bar_lab_cens, y = ymax/2),
                  # nudge_y = 0.05,
                  vjust = 0.5,
                  angle = 90,
                  colour = "black",
                  position = position_dodge(width = 0.6),
                  size = 4)
    
  }

# test
# 
# bar_mean_multiple_vars(
#   list(
#     SleepQual_GTB = "Bedtime behaviours",
#     SleepQual_FARS = "Sleep efficiency",
#     SleepQual_RTW = "Morning wakefulness"
#   ),
#   group = "sex",
#   .censor = TRUE,
#   ymax = 6,
#   ylab = "Score"
# )


# covid concerns graphs ---------------------------------------------------

bar_diverging <- function(category, .data = school_dat, ordervals = c(
  "covidlife" = "Life as a whole",
  "covidhealth" = "Health",
  "covidfamrel" = "Family relations",
  "covidfriendrel" = "Friendships",
  "covidmh" = "Mental Health",
  "covidsch" = "School performance",
  "covidactivity" = "Physical activity",
  "coviddiet" = "Diet",
  "covidfuture" = "Future",
  "covidfinance" = "Family finances"
)) {
  
  global_girls_colour <- "#2F5597"
  global_boys_colour <- "#DAE3F3"
  global_s2_colour <- "#548235"
  global_s4_colour <- "#C5E0B4"
  
  colours <- list(Girls = c(global_girls_colour, global_boys_colour),
                  Boys = c(global_girls_colour, global_boys_colour),
                  S2 = c(global_s2_colour, global_s4_colour),
                  S4 = c(global_s2_colour, global_s4_colour),
                  `S.` = c(global_s2_colour, global_s4_colour))
  
  .data |> 
    select(sex, grade, starts_with("covid")) |>
    pivot_longer(-c(sex, grade), names_to = "topic", values_to = "response") |> 
    pivot_longer(c(sex, grade), names_to = "cat", values_to = "group") |> 
    group_by(cat, group, topic) |> 
    summarise(perc_pos = sum(response == "Positive", na.rm = TRUE)/n(),
              perc_neg = -sum(response == "Negative", na.rm = TRUE)/n()) |> 
    pivot_longer(starts_with("perc"), names_to = "dir", values_to = "value", names_prefix = "perc_") |> 
    mutate(topic = factor(topic, levels = names(ordervals), labels = ordervals)) |> 
    filter(group == str_match(group, category)) |> 
    ggplot(aes(x = value, y = fct_rev(topic), fill = dir)) +
    geom_vline(xintercept = 0) +
    geom_col(width = 0.6) +
    scale_fill_manual("",
                      labels = c("Negative", "Positive"),
                      values = colours[[category]]) +
    theme(panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_line(),
          axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 12),
          legend.position = "top") +
    ylab("") +
    scale_x_continuous(breaks = seq(-1, 1, 0.2),
                       limits = c(-1, 1),
                       labels = percent(c(seq(1, 0, -0.2), seq(0.2, 1, 0.2))))  +
    geom_text(aes(label = percent(abs(value), suffix="", accuracy = 1),
                  x = value + 0.1*if_else(dir == "neg", -1, 1)),
              size = 4)
}

# test

# bar_diverging("Girls")
# bar_diverging("Boys")
# bar_diverging("S2")
# bar_diverging("S4")
# bar_diverging("S.")



# return percentage -------------------------------------------------------

perc_success <- function(var, success, .data = school_dat) {
  var <- enquo(var)
  
  .data |> 
    summarise(perc = sum(!!var %in% success)/n()) |> 
    mutate(perc = percent(perc, accuracy = 0.1)) |> 
    pull(perc)
  
}

# perc_success(EMC_Problem, "Problematic social media user")

# customising graphs ------------------------------------------------------

#' scale_fill_hbsc to globalise fill colours

scale_fill_hbsc <- function(x, ...) scale_fill_manual(
  values = c(
    "Girls" = global_girls_colour,
    "Boys" = global_boys_colour,
    "S2" = global_s2_colour,
    "S4" = global_s4_colour,
    "1" = primary_colour
  ),
  ...
)

#' Thinner geom_bar

geom_bar_t <- function (..., width = 0.5) {
  geom_bar(..., width = width)
}