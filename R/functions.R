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


bar_by_cat <- function(.data,
                       var,
                       success = "Yes",
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
      scale_y_continuous("%", labels = percent)+
      geom_text(aes(label = percent(prop, suffix="", accuracy = 1)),
                vjust = 0, 
                nudge_y = 0.05,
                size = 4) +
      theme(plot.margin = unit(c(0.8, 0.5, 0.5, 1),  "cm")) +
      coord_cartesian(ylim = c(0, 1), clip = "off")
    
  } else { # Test semi-censored version
    
  # } else if (all(df_sex$numerator > 3) & sum(df_sex$denom <= 14)) {
    # * if there are ≤14 students, the chart should only present a single column
    #   representing all students.
    
    p1 <- df_sex |>
      summarise(prop = sum(numerator) / sum(denom)) |>
      ggplot(aes("All pupils", prop)) +
      geom_bar_t(stat = "identity") +
      scale_fill_hbsc() +
      scale_y_continuous("%", labels = percent)+
      geom_text(aes(label = percent(prop, suffix="", accuracy = 1)),
                vjust = 0, 
                nudge_y = 0.05,
                size = 4) +
      theme(plot.margin = unit(c(0.8, 0.5, 0.5, 1),  "cm")) +
      coord_cartesian(ylim = c(0, 1), clip = "off")
  }
  # For full censoring
  # } else {
  #   p1 <- ggplot() +
  #     geom_text(aes(x = 1, y = 0.5, label = "Chart ommitted\ndue to low numbers"),
  #               size = 12) +
  #     scale_x_discrete(breaks = 1, labels = "") +
  #     scale_y_continuous("%", labels = percent, limits = c(0, 1))
  #   
  # }
  
  df_school <- .data |>
    group_by(grade) |>
    mutate(success = !!var %in% success) |>
    summarise(numerator = sum(success),
              denom = n(),
              .groups = "keep") |>
    filter(!is.na(grade))
  
  if (((length(df_school$grade) == 2 & all(df_sex$numerator > 3) & all(df_sex$denom >= 7) &
      all(df_school$denom >= 7)) | (length(df_school$grade) == 2 & .censor == FALSE))) {
    # * for secondary schools, only separate by year if there are ≥7 S2 AND ≥7 S4).
    
    p2 <- df_school |>
      summarise(prop = sum(numerator) / sum(denom)) |>
      ggplot(aes(grade, prop, fill = grade)) +
      geom_bar_t(stat = "identity") +
      scale_fill_hbsc() +
      scale_y_continuous("", 
                         labels = NULL,
                         position = "right"
                         ) +
      theme(axis.ticks.y = element_line(colour = "white"),
            axis.text.y = element_text(colour = "white"),
            plot.margin = unit(c(0.8, 0.5, 0.5, 1),  "cm")) +
      geom_text(aes(label = percent(prop, suffix="", accuracy = 1)),
                vjust = 0, 
                nudge_y = 0.05,
                size = 4) +
      coord_cartesian(ylim = c(0, 1), clip = "off")
  } else {
    p2 <- NULL
  }
  
  p1 + p2
}

# test
# school_dat |>
#   bar_by_cat(health, c("Good", "Excellent"),
#              .censor = params$censor)


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


bar_mean_by_cat <- function(.data, 
                            var,
                       .censor = TRUE,
                       ymax = max(.data[[rlang::as_name(var)]], na.rm = TRUE),
                       ylab = "Mean",
                       ybreaks = NULL) {
  require(rlang)
  var <- enquo(var)
  
  if (is.null(ybreaks)) {
    ybreaks_fun <- waiver
  } else {
    ybreaks_fun <- \() ybreaks
  }
    
  
  rlang::eval_tidy(var, data = .data)
  
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
      scale_y_continuous(ylab, breaks = ybreaks_fun()) +
      geom_text(aes(label = sprintf("%.1f", mean_var)),
                vjust = 0, 
                nudge_y = 0.05 * ymax,
                size = 4) +
      coord_cartesian(ylim = c(0, ymax), clip = "off")
    
    } else { # Test semi-censored version 
      
  # } else if (all(df_sex$denom > 3)) {
    # * if there are ≤14 students, the chart should only present a single column
    #   representing all students.
    
    p1 <- df_sex |>
      summarise(mean_var = weighted.mean(mean_var, w = denom)) |> 
      ggplot(aes("All pupils", mean_var)) +
      geom_bar_t(stat = "identity") +
      scale_fill_hbsc() +
      scale_y_continuous(ylab, breaks = ybreaks_fun())+
      geom_text(aes(label = sprintf("%.1f", mean_var)),
                vjust = 0, 
                nudge_y = 0.05 * ymax,
                size = 4) +
      coord_cartesian(ylim = c(0, ymax), clip = "off")
    
    }
    
  # For full censoring
  # } else {
  #   p1 <- ggplot() +
  #     geom_text(aes(x = 1, y = 0.5, label = "Chart ommitted\ndue to low numbers"),
  #               size = 12) +
  #     scale_x_discrete(breaks = 1, labels = "") +
  #     scale_y_continuous(ylab, breaks = 0:1, limits = c(0, ymax))
  #   
  # }
  
  df_school <- .data |>
    group_by(grade) |>
    summarise(mean_var = mean(!!var, na.rm = TRUE),
              denom = n()) |>
    filter(!is.na(grade))
  
  if ((length(df_school$grade) == 2 & all(df_sex$denom >= 7) &
       all(df_school$denom >= 7)) | (length(df_school$grade) == 2 & .censor == FALSE)) {
    # * for secondary schools, only separate by year if there are ≥7 S2 AND ≥7 S4).
    
    p2 <- df_school |>
      ggplot(aes(grade, mean_var, fill = grade)) +
      geom_bar_t(stat = "identity") +
      scale_fill_hbsc() +
      scale_y_continuous(" ",
                         position = "right", breaks = ybreaks_fun()
      ) +
      theme(axis.ticks.y = element_line(colour = "white"),
            axis.text.y = element_text(colour = "white"),
            plot.margin = unit(c(0.5, 0.5, 0.5, 1),  "cm")) +
      geom_text(aes(label = sprintf("%.1f", mean_var)),
                vjust = 0, 
                nudge_y = 0.05 * ymax,
                size = 4) +
      coord_cartesian(ylim = c(0, ymax), clip = "off")
  } else {
    p2 <- NULL
  }
  
  p1 + p2
}

# school_dat |>
#   bar_mean_by_cat(
#     var = CohenPSS4,
#     .censor = params$censor,
#     ymax = 16,
#     ylab = "Score"
#   )

# test

# school_dat |>
#   bar_mean_by_cat(Schooldays_sleep_hrs, .censor = params$censor, ylab = "Hours")


# test multiple vars for denominator size ---------------------------------

test_bar_multiple_vars <-
  function(.data,
           varslist,
           success) {
    
    missing_success <- rlang::is_missing(success)
    
    .data |> 
      group_by(sex) |> 
      select(sex, !!!syms(names(varslist))) |>
      summarise(across(everything(), function(vars) {
        if(missing_success) {
          sum(!is.na(vars))
        } else {
          sum(vars %in% success)
        }
        }),
                denom = n()) |>
      pivot_longer(-c(sex, denom), names_to = "var", values_to = "n") |> 
      filter(!is.na(sex)) |> 
      mutate(include = denom >= 7 & n >= 3) |> 
      summarise(include = all(include)) |> 
      pull(include)
  }


# school_dat |>
#   test_bar_multiple_vars(
#     list(# drunk1life = "Been drunk",
#       smokever = "Tried tobacco",
#       ecigever = "Tried vaping"),
#     success = c("drunk once or more", "Tried smoking", "Tried e-cigarette")
#   )
# school_dat |>
#   test_bar_multiple_vars(
#     list(
#       sit_gamehr = "Gaming",
#       sit_socnethr = "Social media",
#       sit_watchhr = "Watching TV",
#       sit_browhr = "Browsing internet"
#     ))

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
  function(.data, 
           varslist,
           success = c("More than once a week", "About every day"),
           group = c("none", "grade", "sex"),
           .censor = TRUE) {
    
    group <- match.arg(group)
    
    clean_dat <- .data |>
    mutate(grouping = case_when(
      group == "none" ~ "All pupils",
      group == "sex" ~ as.character(sex),
      group == "grade" ~ as.character(grade)
    )) |> 
      group_by(grouping) |> 
      select(grouping, !!!syms(names(varslist))) |>
      summarise(across(everything(), ~ sum(.x %in% success)),
                denom = n()) |>
      pivot_longer(-c(grouping, denom), names_to = "var", values_to = "n") |> 
      rowwise() |>
      mutate(
        censored = if_else(n < 3 & .censor, 1, 0),
        labels = str_wrap(varslist[[var]][1], 12),
        prop = n / denom,
        prop = if_else(censored == 1, 0.05, prop),
        bar_lab_main = if_else(censored == 1, "*", percent(prop, suffix="", accuracy = 1)),
        bar_lab_cens = if_else(censored == 1, "Numbers too low to show", ""),
        grouping = factor(grouping, levels = c("Girls", "Boys", "S2", "S4", "All pupils"))
      ) |>
      filter(!is.na(grouping)) 
    
    clean_dat |> 
      ggplot(aes(fct_inorder(labels), prop, linetype = factor(censored), fill = grouping, colour = grouping, group = grouping)) +
      geom_bar_t(aes(alpha = factor(censored)), stat = "identity", position = position_dodge(width = 0.6)) +
      scale_alpha_manual(values = c("1" = 0.6, "0" = 1), guide = guide_none()) +
      scale_linetype_manual(values = c("1" = "dashed", "0" = "solid"), guide = guide_none()) +
      scale_x_discrete(guide = guide_axis(n.dodge = ceiling(length(varslist) / 4))) +
      scale_fill_hbsc(aesthetics = c("fill", "colour"), name = "",  limits = force) +
      theme(legend.position = "bottom",
            plot.margin = unit(c(0.8, 0.5, 0.5, 0),  "cm"),
            plot.caption = element_text(hjust = 1, size = 10, face = "italic")) +
      scale_y_continuous("%", labels = percent) +
      geom_text(aes(label = bar_lab_main),
                vjust = -0.5, 
                # nudge_y = 0.05,
                colour = "black",
                position = position_dodge(width = 0.6),
                size = 4) +
      # geom_text(aes(label = bar_lab_cens, y = 0.15),
      #           # nudge_y = 0.05,
      #           vjust = 0.5,
      #           hjust = 0,
      #           angle = 90,
      #           colour = "black",
      #           position = position_dodge(width = 0.6),
      #           size = 4) +
      coord_cartesian(ylim = c(0, 1), clip = "off") +
      labs(caption = if_else(any(clean_dat$censored == 1), "* Numbers too low to show", ""))
    
  }

# test
# 
# school_dat |>
#   bar_multiple_vars(
#     list(
#       fruits_2 = "Fruit",
#       vegetables_2 = "Vegetables",
#       chips3 = "Chips",
#       sweets_2 = "Sweets",
#       fruitjuice = "Fruit juice",
#       softdrinks_2 = "Soft drinks",
#       energydrink = "Energy drinks"
#     ),
#     success = c("Once a day, every day", "Every day, more than once"),
#     group = "sex",
#     .censor = params$censor
#   ) 

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
  function(.data, 
           varslist,
           group = c("none", "grade", "sex"),
           .censor = TRUE,
           limits = c(`Poor quality` = 1,
                      `High quality` = 6),
           ymax = limits[2],
           ylab = "Mean") {
    
    require(rlang)
    group <- match.arg(group)
    
    clean_dat <- .data |>
    mutate(grouping = case_when(
      group == "none" ~ "1",
      group == "sex" ~ as.character(sex),
      group == "grade" ~ as.character(grade)
    )) |> 
      select(grouping, !!!syms(names(varslist))) |>
      filter(if_all(.fns = ~!is.na(.x))) |> 
      group_by(grouping) |> 
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
        labels = str_wrap(varslist[[var]][1], 12),
        mean = if_else(censored == 1, ymax/20, mean),
        bar_lab_main = if_else(censored == 1, "*", sprintf("%.1f", mean)),
        bar_lab_cens = if_else(censored == 1, "Numbers too low to show", ""),
        grouping = factor(grouping, levels = c("Girls", "Boys", "S2", "S4", "1"))
      )
    
      ggplot(clean_dat, aes(fct_inorder(labels), mean, linetype = factor(censored), fill = grouping, colour = grouping, group = grouping)) +
      geom_bar_t(aes(alpha = factor(censored)), stat = "identity", position = position_dodge(width = 0.6)) +
      scale_alpha_manual(values = c("1" = 0.6, "0" = 1), guide = guide_none()) +
      scale_linetype_manual(values = c("1" = "dashed", "0" = "solid"), guide = guide_none()) +
      scale_x_discrete(guide = guide_axis(n.dodge = ceiling(length(varslist) / 4))) +
      scale_fill_hbsc(aesthetics = c("fill", "colour"), name = "",  limits = force) +
      theme(legend.position = if_else(group == "none", "none", "bottom"),
            plot.margin = unit(c(0.5, 0.5, 0.5, 0),  "cm")) +
      scale_y_continuous(ylab) +
      geom_text(aes(label = bar_lab_main),
                vjust = -0.5, 
                # nudge_y = 0.05,
                colour = "black",
                position = position_dodge(width = 0.6),
                size = 4) +
        # geom_text(aes(label = bar_lab_cens, y = ymax/2),
        #           # nudge_y = 0.05,
        #           vjust = 0.5,
        #           angle = 90,
        #           colour = "black",
        #           position = position_dodge(width = 0.6),
        #           size = 4) +
        coord_cartesian(ylim = c(0, ymax), clip = "off") +
        labs(caption = if_else(any(clean_dat$censored == 1), "* Numbers too low to show", ""))
    
  }

# test
# 
# bar_mean_multiple_vars(
#   school_dat,
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

bar_diverging <- function(.data, category, ordervals = c(
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
), 
.censor = TRUE) {
  
  global_girls_colour <- "#2F5597"
  global_boys_colour <- "#DAE3F3"
  global_s2_colour <- "#548235"
  global_s4_colour <- "#C5E0B4"
  
  colours <- list(Girls = c(global_girls_colour, global_boys_colour),
                  Boys = c(global_girls_colour, global_boys_colour),
                  S2 = c(global_s2_colour, global_s4_colour),
                  S4 = c(global_s2_colour, global_s4_colour),
                  `All pupils` = c(primary_colour, "#f37575"),
                  `S.` = c(global_s2_colour, global_s4_colour))
  
  clean_dat <- .data |>
    select(sex, grade, starts_with("covid")) |>
    pivot_longer(-c(sex, grade), names_to = "topic", values_to = "response") |>
    pivot_longer(c(sex, grade), names_to = "cat", values_to = "group") |>
    group_by(cat, group, topic) |>
    summarise(
      n = n(),
      n_pos = sum(response == "Positive", na.rm = TRUE),
      n_neg = sum(response == "Negative", na.rm = TRUE),
      perc_pos = n_pos / n,
      perc_neg = -n_neg / n,
      .groups = "drop"
    ) |> pivot_longer(
      n_pos:perc_neg,
      names_to = c("var", "dir"),
      values_to = "val",
      names_sep = "_"
    ) |> 
    filter(group == str_match(group, category)) |> 
    pivot_wider(
      names_from = "var",
      values_from = "val",
      id_cols = c(cat, group, topic, dir)
    ) |> 
    mutate(topic = factor(topic, levels = names(ordervals), labels = ordervals),
           censored = if_else(n < 3 & .censor, 1, 0),
           perc = if_else(censored == 1, 0.05 * if_else(dir == "neg", -1, 1), perc),
           bar_lab_main = if_else(censored == 1, "*", percent(abs(perc), suffix="", accuracy = 1))) |>
    filter(group == str_match(group, category))
  
    clean_dat |>
    ggplot(aes(x = perc, y = fct_rev(topic), fill = dir,
               linetype = factor(censored))) +
    geom_vline(xintercept = 0) +
    geom_col(aes(alpha = factor(censored)), width = 0.6, size = 2) +
    scale_fill_manual("",
                      labels = c("Negative", "Positive"),
                      # aesthetics = c("fill", "colour"),
                      values = colours[[category]]) +
    theme(panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_line(),
          axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 12),
          plot.margin = unit(c(0.5, 0.5, 0.5, 0),  "cm"),
          plot.caption = element_text(hjust = 1, size = 10, face = "italic"),
          legend.position = "top") +
    ylab("") +
    scale_alpha_manual(values = c("1" = 0.5, "0" = 1), guide = guide_none()) +
    scale_linetype_manual(values = c("1" = "dashed", "0" = "solid"), guide = guide_none()) +
    scale_x_continuous(breaks = seq(-1, 1, 0.2),
                       labels = percent(c(seq(1, 0, -0.2), seq(0.2, 1, 0.2))))  +
    geom_text(aes(label = bar_lab_main,
                  x = perc + 0.1*if_else(dir == "neg", -1, 1)),
              size = 4,
              colour = "black") +
    coord_cartesian(xlim = c(-1, 1), clip = "off") +
    labs(caption = if_else(any(clean_dat$censored == 1), "* Numbers too low to show", ""))
}

# test
# 
# bar_diverging(school_dat, "Girls")
# bar_diverging(school_dat, "Boys")
# bar_diverging(school_dat, "S2")
# bar_diverging(school_dat, "S4")
# bar_diverging("S.")



# return percentage -------------------------------------------------------

perc_success <- function(.data, var, success) {
  var <- enquo(var)
  
  .data |> 
    summarise(perc = sum(!!var %in% success)/n()) |> 
    mutate(perc = percent(perc, accuracy = 1)) |> 
    pull(perc)
  
}

# perc_success(EMC_Problem, "Problematic social media user")

# customising graphs ------------------------------------------------------

#' scale_fill_hbsc to globalise fill colours

scale_fill_hbsc <- function(...) {
  scale_fill_manual(
    values = c(
      "Girls" = global_girls_colour,
      "Boys" = global_boys_colour,
      "S2" = global_s2_colour,
      "S4" = global_s4_colour,
      "All pupils" = primary_colour,
      "1" = primary_colour
    ),
    ...
  )
}


#' Thinner geom_bar

geom_bar_t <- function (..., width = 0.5) {
  geom_bar(..., width = width)
}


# three most common health complaints -------------------------------------

common_health_complaints <- function(.data, 
                                     varslist,
                                     success = c("More than once a week", "About every day"),
                                     group = c("none", "grade", "sex"),
                                     .censor = TRUE) {
  group <- match.arg(group)
  
  clean_dat <- .data |>
    mutate(grouping = case_when(
      group == "none" ~ "1",
      group == "sex" ~ as.character(sex),
      group == "grade" ~ as.character(grade)
    )) |> 
    group_by(grouping) |> 
    select(grouping, !!!syms(names(varslist))) |>
    summarise(across(everything(), ~ sum(.x %in% success)),
              denom = n()) |>
    pivot_longer(-c(grouping, denom), names_to = "var", values_to = "n") |> 
    rowwise() |>
    mutate(
      labels = varslist[[var]][1],
      prop = n / denom,
      grouping = factor(grouping, levels = c("Girls", "Boys", "S2", "S4", "1"))
    ) |>
    filter(!is.na(grouping))|> 
    group_by(var) |> 
    mutate(overall_perc = weighted.mean(prop, w = denom)) |> 
    mutate(prop = if_else(n < 3, rlang::na_dbl, prop)) |> 
    select(grouping, labels, prop, overall_perc) |> 
    pivot_wider(names_from = grouping, values_from = prop) |> 
    ungroup() |> 
    arrange(desc(overall_perc)) |> 
    head(3) |> 
    mutate(across(where(is.numeric), percent, accuracy = 1))
  
  clean_dat |> 
    rowwise() |> 
    group_walk(function(df, k) {
      cat("* ", df$labels, ": ", df$overall_perc, sep = "")
      
      if (!is.na(df$Boys) & !is.na(df$Girls)) {
            cat(" (", df$Girls, " of girls, ", df$Boys, " of boys)\n", sep = "")
      } else {
        cat("\n")
      }
    })
  
}

  # school_dat |>
  #   common_health_complaints(
  #     list(
  #       headache =   "Headache",
  #       stomachache =   "Stomach-ache",
  #       backache =   "Backache",
  #       dizzy =   "Dizziness",
  #       feellow =   "Feeling low",
  #       nervous =   "Feeling nervous",
  #       irritable =   "Feeling irritable",
  #       sleepdificulty =   "Sleep difficulties"
  #     ),
  #     group = "sex",
  #     .censor = params$censor
  #   )
