bar_by_gender <- function(var, .count_var = "Yes", .data = school_dat) {
  require(tidyverse)
  require(scales)
  
  var <- enquo(var)
  
  # .data %>% 
  #   select(sex, AGECAT, !!var) %>% 
  .data %>%
    group_by(!!var, sex) %>% 
    # mutate(denom = n()) %>% 
    # group_by(!!sym(exposure$variable), !!sym(outcome$variable)) %>% 
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


# school_dat %>%
#   group_by(breakfastwd, sex) %>% 
#   # mutate(denom = n()) %>% 
#   # group_by(!!sym(exposure$variable), !!sym(outcome$variable)) %>% 
#   count() %>% 
#   group_by(sex) %>% 
#   mutate(denom = sum(n),
#          prop =  n / denom,
#          perc_label = paste0(round(100 * prop, 0),
#                              "%")) %>%
#   mutate(text_y = if_else(prop < 0.9, prop + 0.1, prop - 0.1)) %>% 
#   filter(breakfastwd == "Yes") %>% 
#   ggplot(aes(sex, prop, fill = sex)) +
#   geom_bar(stat = "identity") +
#   scale_y_continuous(
#     "Percentage",
#   labels = percent_format(accuracy = 1),
#   breaks = c(0, 0.5, 1),
#   minor_breaks = seq(0, 1, 0.1),
#   limits = c(0,1)) +
#   geom_text(aes(y = text_y, label = perc_label),
#             colour = "#121212",
#             size = 8) +
#   theme(axis.title.y = element_text(angle = 0,vjust = 0.5),
#         panel.grid.major.x = element_blank(), 
#         panel.grid.minor.x = element_blank(),
#         axis.title.y.left = element_text(angle = 90),
#         axis.text.x = element_text(hjust = 1)) +
#   scale_fill_manual(
#     values = c(
#       "Girl" = global_girls_colour,
#       "Boy" = global_boys_colour
#     )
#     ) 
