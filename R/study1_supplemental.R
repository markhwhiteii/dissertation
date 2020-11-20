library(lme4)
library(lmerTest)
library(emmeans)
library(tidyverse)

auth1 <- read_csv("../data/study1.csv") %>% 
  mutate(
    auth_muslim = (
      antimuslim1_1 + antimuslim1_2 + antimuslim1_3 + antimuslim1_4 +
        antimuslim2_1 + antimuslim2_2 + antimuslim2_3 + antimuslim2_4 +
        antimuslim3_1 + antimuslim3_2 + antimuslim3_3 + antimuslim3_4
    ) / 12,
    auth_politician = (
      politicians1_1 + politicians1_2 + politicians1_3 + politicians1_4 +
        politicians2_1 + politicians2_2 + politicians2_3 + politicians2_4 +
        politicians3_1 + politicians3_2 + politicians3_3 + politicians3_4
    ) / 12,
    auth_control = (
      filler1_1 + filler1_2 + filler1_3 + filler1_4 +
        filler2_1 + filler2_2 + filler2_3 + filler2_4 +
        filler3_1 + filler3_2 + filler3_3 + filler3_4
    ) / 12,
    gender = as.factor(gender - 1),
    religion = as.factor(religion),
    race = as.factor(race),
    rw_polid = (conservative + (8 - democrat)) / 2,
    prej_muslim = (
      muslim1 + muslim2 + muslim3 + (8 - muslim4) + (8 - muslim5) +
        (8 - muslim6) + (8 - muslims7)
    ) / 7,
    prej_politician = (
      (8 - politicians_1) + (8 - politicians_2) + (8 - politicians_3) +
        politicians_4 + politicians_5 + (8 - politicians_6) + 
        (8 - politicians_7)
    ) / 7,
    prej_control = ((8 - filler_1) + (8 - filler_2) + (8 - filler_3)) / 3,
    disl_pizza = (8 - filler_1),
    auth_pizza = (filler1_1 + filler1_2 + filler1_3 + filler1_4) / 4,
    disl_beach = (8 - filler_2),
    auth_beach = (filler2_1 + filler2_2 + filler2_3 + filler2_4) / 4,
    disl_cookies = (8 - filler_3),
    auth_cookies = (filler3_1 + filler3_2 + filler3_3 + filler3_4) / 4
  )

# pivot ------------------------------------------------------------------------
auth1_l <- auth1 %>%
  mutate(id = 1:n()) %>%
  select(
    id,
    starts_with("auth"),
    starts_with("prej"),
    -auth_control,
    -prej_control
  ) %>% 
  pivot_longer(
    starts_with("auth"), 
    names_to = "auth_group", 
    values_to = "auth"
  ) %>% 
  pivot_longer(
    starts_with("prej"), 
    names_to = "prej_group", 
    values_to = "prej"
  ) %>% 
  mutate(
    match = auth_group == "auth_muslim" & prej_group == "prej_muslim" |
      auth_group == "auth_politician" & prej_group == "prej_politician",
    group = paste0(prej_group, "-", auth_group)
  )

# final model ------------------------------------------------------------------
auth1_l$auth_group <- factor(case_when(
  auth1_l$auth_group == "auth_muslim" ~ "Muslims",
  auth1_l$auth_group == "auth_politician" ~ "Politicians",
  auth1_l$auth_group == "auth_pizza" ~ "Pizza",
  auth1_l$auth_group == "auth_beach" ~ "Beach",
  auth1_l$auth_group == "auth_cookies" ~ "Cookies"
), c("Muslims", "Politicians", "Pizza", "Beach", "Cookies"))

auth1_l$prej_group <- factor(case_when(
  auth1_l$prej_group == "prej_muslim" ~ "Anti-Muslim Prejudice",
  auth1_l$prej_group == "prej_politician" ~ "Anti-Politician Prejudice"
), c("Anti-Muslim Prejudice", "Anti-Politician Prejudice"))

m5 <- lmer(auth ~ prej * auth_group * prej_group + (1 | id), auth1_l)
anova(m5)

m5 %>% 
  emtrends(
    ~ prej_group + auth_group, 
    var = "prej", 
    lmer.df = "satterthwaite"
  ) %>% 
  test() %>% 
  as_tibble() %>% 
  arrange(prej_group) %>% 
  mutate_if(is.double, round, 3)

sjPlot::plot_model(
  m5, 
  type = "int",
  ci.lvl = NA,
  colors = "bw"
) +
  theme_light() +
  theme(text = element_text(size = 18)) +
  scale_x_continuous(lim = c(1, 7), breaks = 1:7) +
  scale_y_continuous(lim = c(1, 7), breaks = 1:7) +
  labs(title = NULL, x = "Prejudice", y = "Perceived Authenticity\n") +
  scale_linetype_discrete(name = "Authenticity\nTarget")
