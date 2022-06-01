success  <- c("Good", "Excellent")
.data <-  school_dat


df_sex <- .data |>
  group_by(sex) |>
  mutate(success = health %in% success) |>
  summarise(numerator = sum(success),
            denom = n()) |>
  filter(!is.na(sex))

if (all(df_sex$numerator > 3) & all(df_sex$denom) >= 7) {
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
  ggplot() +
    geom_text(aes(x = 1, y = 0.5, label = "Chart ommitted\ndue to low numbers"),
              size = 12) +
    scale_x_discrete(breaks = 1, labels = "") +
    scale_y_continuous("%", labels = percent, limits = c(0, 1))
  
}



# test 2

df_sex <- .data |>
  group_by(sex) |>
  mutate(success = health %in% success) |>
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
  mutate(success = health %in% success) |>
  summarise(numerator = sum(success),
            denom = n()) |>
  filter(!is.na(grade))

if (length(df_school$grade) == 2 & all(df_school$numerator >= 7)) {
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



# across cats graphs ------------------------------------------------------


success <- c("More than once a week", "About every day")
cat_labels <-
  c(
    "Headache",
    "Stomach-ache",
    "Backache",
    "Dizziness",
    "Feeling low",
    "Feeling nervous",
    "Feeling irritable",
    "Sleep difficulties"
  )


# bar_multiple_vars <- function(..., labels, success = c("More than once a week", "About every day"), .data = school_dat)
  
  # vars <- enquos(...)

cat_labels <-   list(
  headache =   "Headache",
  stomachache =   "Stomach-ache",
  backache =   "Backache",
  dizzy =   "Dizziness",
  feellow =   "Feeling low",
  nervous =   "Feeling nervous",
  irritable =   "Feeling irritable",
  sleepdificulty =   "Sleep difficulties"
)

# group = "grade"

school_dat |> 
  mutate(grouping = case_when(
    group == "none" ~ "1",
    group == "sex" ~ as.character(sex),
    group == "grade" ~ as.character(grade)
  )) |> 
  group_by(grouping) |> 
  select(headache, stomachache, backache, dizzy, feellow, nervous, irritable, sleepdificulty) |> 
  summarise(across(everything(), ~ sum(.x %in% success)),
            denom = n()) |> 
  pivot_longer(-c(grouping, denom), names_to = "var", values_to = "n") |> 
  rowwise() |>
  mutate(
    censored = if_else(n < 3 & .censor, 1, 0),
    labels = cat_labels[[var]][1],
    prop = n / denom,
    prop = if_else(censored == 1, 0.5, prop),
    grouping = factor(grouping, levels = c("Girls", "Boys", "S2", "S4", "1"))
  ) |>
  ggplot(aes(fct_inorder(labels), prop, alpha = factor(censored), linetype = factor(censored), fill = grouping, colour = grouping)) +
  geom_bar_t(stat = "identity", position = position_dodge(width = 0.6)) +
  scale_alpha_manual(values = c("1" = 0.2, "0" = 1), guide = guide_none()) +
  scale_linetype_manual(values = c("1" = "dashed", "0" = "solid"), guide = guide_none()) +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  scale_fill_hbsc(aesthetics = c("fill", "colour"), name = "",  limits = force) +
  theme(legend.position = if_else(group == "none", "none", "bottom")) +
  scale_y_continuous("%", labels = percent, limits = c(0, 1))


# smoother as named list?
varslist <- list(
headache =   "Headache",
stomachache =   "Stomach-ache",
backache =   "Backache",
dizzy =   "Dizziness",
feellow =   "Feeling low",
nervous =   "Feeling nervous",
irritable =   "Feeling irritable",
sleepdificulty =   "Sleep difficulties")

bar_multiple_vars <- function(varslist = varslist, success = c("More than once a week", "About every day"), .data = school_dat) {
  
  .data |> 
    select(!!!syms(names(varslist))) |> 
    summarise(across(everything(), ~ sum(.x %in% success)),
              denom = n()) |>
    pivot_longer(-denom, names_to = "var", values_to = "n") |>
    rowwise() |> 
    mutate(censor = if_else(n < 3, 1, 0),
           labels = varslist[[var]][1],
           prop = n/denom) |>
    ggplot(aes(fct_inorder(labels), prop)) +
    geom_bar_t(stat = "identity") +
    scale_fill_hbsc() +
    scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
    scale_y_continuous("%", labels = percent, limits = c(0, 1))
  
}

bar_multiple_vars(varslist = varslist)


# all figures -------------------------------------------------------------

all_text <- read_lines("templates/pilot_secondary.md")

str_subset(all_text, "Figure \\d{1,2}.?:") |> 
  str_extract("Figure .*[^\\*{2}]") |> 
  str_split(":", simplify = TRUE) |> 
  `colnames<-`(c("Figure", "Caption")) |> 
  as_tibble() |> 
  mutate(across(.fns = str_trim)) |> 
  write_csv("templates/secondary figures.csv")


# testing how to pass vector? ---------------------------------------------
