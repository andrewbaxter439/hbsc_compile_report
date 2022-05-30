# new 2022 data -----------------------------------------------------------

library(haven)
library(tidyverse)

hbsc2022 <- read_spss("T:/projects/HBSC Scotland (308202-01)/2022 survey data/Final dataset for reports/Nat22 combined P7 S2 S4  Pre_easter v3.sav") |> 
  mutate(across(where(is.labelled), as_factor),
         school_id = str_extract(School_filename, "^\\w*"))



# old data ----------------------------------------------------------------

# 
# 
# library(readr)
# library(tidyverse)
# 
# hbsc2014 <- readRDS("import/HBSC2014.rds")
# 
# 
# hbsc_data <- hbsc2014 %>%
#   filter(COUNTRYno == 826002) %>%
#   select(
#     ClassID,
#     sex,
#     AGECAT,
#     breakfastwd,
#     fruits,
#     sweets,
#     family_meal_eve = m12,
#     timeexe,
#     tvwd,
#     playgamewd,
#     bulliedothers,
#     beenbullied,
#     cbullmess,
#     cbullpict,
#     talkfather,
#     talkstepfa,
#     talkmother,
#     talkstepmo,
#     talk_listen = m79,
#     famhelp,
#     famsup,
#     famdec,
#     famtalk,
#     friends_ph_int = m90,
#     acachieve,
#     studtogether,
#     studaccept,
#     teacheraccept,
#     teachercare,
#     teachertust,
#     fasbedroom,
#     welloff,
#     likeschool,
#     schoolpressure,
#     thinkbody,
#     feellow,
#     nervous,
#     sleepdificulty,
#     health,
#     lifesat,
#     smok30d_2,
#     drunk30d
#   )
# 
# labs_cats <- tribble(	~variable, ~lab, ~cat, ~question,
#                       "sex",  "Gender", "group",  "",
#                       "AGECAT", "Age category", "group",  "",
#                       "breakfastwd",  "Breakfast on schooldays", "exposure",  "Do you have breakfast every day on schooldays?",
#                       "fruits",	"Eating fruit", "exposure",	"How many times a week do you usually eat fruits?",
#                       "sweets",	"Eating sweets", "exposure",	"How many times a week do you usually eat sweets or chocolate?",
#                       "family_meal_eve",	"Family meals in the evening", "exposure", "How often do you have an evening meal together with your mother or father?",
#                       "timeexe",	"Time exercising", "exposure",	"Outside of school hours, how often do you usually exercise?",
#                       "tvwd",	"Time watching TV", "exposure",	"How many hours a day on weekdays do you spend watching TV?",
#                       "playgamewd",	"Playing computer games", "exposure",	"How many hours a day on weekdays do you spend playing computer games?",
#                       "bulliedothers",	"Bullying others", "exposure",	"Have you taken part in bullying others in the last couple of months?",
#                       "beenbullied",	"Being bullied", "exposure",	"Have you been bullied at school in the past couple of months?",
#                       "cbullmess",	"",	"",	"",
#                       "cbullpict",	"",	"",	"",
#                       "talkfather",	"",	"",	"",
#                       "talkstepfa",	"",	"",	"",
#                       "talkmother",	"",	"",	"",
#                       "talkstepmo",	"",	"",	"",
#                       "talk_listen",	"",	"",	"",
#                       "famhelp",	"Family helpful", "exposure",	"\"My family really tries to help me\"",
#                       "famsup",	"",	"",	"",
#                       "famdec",	"",	"",	"",
#                       "famtalk",	"",	"",	"",
#                       "friends_ph_int", "Talk to friends - phone and internet", "exposure",	"How often do you talk to your friends on the phone or internet?",
#                       "acachieve",	"",	"",	"",
#                       "studtogether",	"",	"",	"",
#                       "studaccept",	"",	"",	"",
#                       "teacheraccept",	"",	"",	"",
#                       "teachercare",	"",	"",	"",
#                       "teachertust",	"",	"",	"",
#                       "fasbedroom",	"",	"",	"",
#                       "welloff",	"",	"",	"",
#                       "likeschool",	"",	"",	"",
#                       "thinkbody",	"Thinking about body",	"",	"Do you think your body is...?",
#                       "feellow",	"Feeling low",  "outcome",	"% who feel low more than once a week",
#                       "nervous",	"Feeling nervous",  "",	"In the last 6 months, have you often felt nervous?",
#                       "sleepdificulty",	"Difficulty sleeping",  "",	"In the last 6 months, have you often had difficulies getting to sleep?",
#                       "health",	"Health",  "outcome",	"% who rate their health as good or excellent",
#                       "lifesat",	"Life satisfaction",  "outcome",	"% who report 'high' life satisfaction",
#                       "schoolpressure", "Pressured by schoolwork", "exposure", "How pressured do you feel by the schoolwork you have to do?",
#                       "smok30d_2", "Smoked in the past 30 days", "exposure", "Have you smoked a cigarette in the past 30 days?",
#                       "drunk30d", "Been drunk in the past 30 days", "exposure", "Have you been drunk in the past 30 days?"
# ) %>% filter(cat != "")
# 
# 
# influences_data <-
#   hbsc_data %>%
#   transmute(
#     ClassID = ClassID,
#     sex = factor(sex, labels = c("1" = "Boy", "2" = "Girl")),
#     AGECAT = factor(AGECAT, labels = c(
#       "11 year-olds", "13 year-olds", "15 year-olds"
#     )),
#     breakfastwd = factor(breakfastwd == 6, labels = c("No", "Yes")),
#     fruits = factor(fruits == 6, labels = c("Less often", "Every day")),
#     sweets = factor(sweets == 6, labels = c("Less often", "Every day")),
#     # family_meal_eve = factor(family_meal_eve == 6, labels = c("Less often", "Every day")),
#     # timeexe = factor(timeexe < 4, labels = c("2-3 times a week or more", "Less often")),
#     # tvwd = factor(tvwd > 4, labels = c("Less than 3 hours", "3 or more")),
#     # playgamewd = factor(playgamewd > 3, labels = c("Less than 2 hours", "2 or more")),
#     bulliedothers = factor(bulliedothers > 2, labels = c("No", "Yes")),
#     beenbullied = factor(beenbullied > 2, labels = c("No", "Yes")),
#     famhelp = factor(famhelp > 5, labels = c("No", "Yes")),
#     # friends_ph_int = factor(friends_ph_int == 4, labels = c("Less often", "Every day")),
#     # thinkbody = factor(thinkbody < 3, labels = c("Less often", "More than once a week")),
#     feellow = factor(feellow < 3, labels = c("Less often", "In the last 6 months, I have felt low more than once a week")),
#     nervous = factor(nervous < 3, labels = c("Less often", "In the last 6 months, I have felt nervous more than once a week")),
#     sleepdificulty = factor(sleepdificulty < 3, labels = c("Less often", "In the last 6 months, I have had difficulties getting to sleep more than once a week")),
#     health = factor(health < 3, labels = c("Fair or Poor", "I would rate my health 'Good' or 'Excellent'")),
#     lifesat = factor(lifesat > 7, labels = c("Fair or Poor", "I have 'high' life satisfaction")),
#     schoolpressure = factor(schoolpressure > 2, labels = c("A little or not at all", "Some or a lot")),
#     smok30d_2 = factor(smok30d_2 > 1, labels = c("No", "Yes")),
#     drunk30d = factor(drunk30d > 1, labels = c("No", "Yes"))
#   )
# 
