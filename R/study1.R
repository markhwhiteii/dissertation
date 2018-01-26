library(tidyverse)
library(cocor)

## tidying data
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

## correlations between variables
vars <- c(
  "auth_muslim", "auth_politician", "auth_pizza", "auth_beach", "auth_cookies",
  "prej_muslim", "prej_politician", "disl_pizza", "disl_beach", "disl_cookies"
)
round(cor(auth1[ ,vars]), 2)[6:10, 1:5]
cor.test(~ prej_muslim + auth_muslim, auth1)
cor.test(~ prej_politician + auth_politician, auth1) # r >= |.18| sig cor

## creating wrapper for cocor
# CAUTION: function may not handle NAs well
cor_diff_overlap <- function(y, x1, x2, data) {
  cocor.dep.groups.overlap(
    r.jk = cor(data[,c(x1, x2, y)])[1, 3],
    r.jh = cor(data[,c(x1, x2, y)])[2, 3],
    r.kh = cor(data[,c(x1, x2, y)])[1, 2],
    n = nrow(data),
    test = "zou2007"
  )
}

## testing differences between correlations
cor_diff_overlap("prej_muslim", "auth_muslim", "auth_politician", auth1)
cor_diff_overlap("prej_politician", "auth_politician", "auth_muslim", auth1)

## demographics
nrow(auth1)
prop.table(table(auth1$gender))
summary(auth1$age)
sd(auth1$age)
round(prop.table(table(auth1$religion)), 2) # 9 = muslim
round(prop.table(table(auth1$race)), 2) # 8 = white

## figure
auth1_fig <- auth1 %>% 
  select(prej_muslim, prej_politician, auth_muslim, auth_politician) %>% 
  gather(var, value, prej_muslim:auth_politician) %>% 
  separate(var, c("var", "group")) %>% 
  group_by(var) %>% 
  mutate(tmp = 1:n()) %>% 
  spread(var, value) %>% 
  select(-tmp) %>% 
  transmute(
    Prejudice = prej, 
    `Perceived Authenticity` = auth, 
    group = gsub(".*_", "", group),
    group = ifelse(group == "muslim", "Muslims", "Politicians")
  )

ggplot(auth1_fig, aes(x = Prejudice, y = `Perceived Authenticity`)) +
  geom_jitter(alpha = .9, height = .1) +
  geom_smooth(method = "lm", se = FALSE, color = "black", size = .7) +
  facet_wrap(~ group) +
  theme_light() +
  theme(text = element_text(size = 14))
ggsave(file = "../docs/figure1.pdf", width = 8, height = 6, dpi = 300)
