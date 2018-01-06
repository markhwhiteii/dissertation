library(tidyverse)
library(mscelns) # not on CRAN, see https://github.com/markhwhiteii/mscelns
auth4 <- read_csv("../data/study4.csv")

## tidying data
auth4 <- auth4[-c(61, 195), ] %>% # didn't do manipulation
  mutate(
    cond = as.factor(cond),
    cond_relevel = factor(cond, levels = c("Suppression", "Expression")),
    authneg = (authneg11 + authneg12 + authneg13 + authneg14 + 
                 authneg21 + authneg22 + authneg23 + authneg24) / 8,
    authpos = (authpos11 + authpos12 + authpos13 + authpos14 + 
                 authpos21 + authpos22 + authpos23 + authpos24) / 8,
    rulesneg = (rulesneg1 + rulesneg2) / 2,
    rulespos = (rulespos1 + rulespos2) / 2,
    legitneg = (legitneg1 + legitneg2) / 2,
    legitpos = (legitpos1 + legitpos2) / 2,
    dislike = (prej_1 + prej_2 + prej_3 + prej_4 + 
                 prej_5 + prej_6 + prej_7) / 7,
    fear = (prej_8 + prej_9 + prej_10) / 3,
    willpower = (prej_11 + prej_12 + prej_13) / 3 
  )

## demographics
summary(auth4$age)
sd(auth4$age)
prop.table(table(auth4$gender))
table(auth4$race) # .76

## manip check
t_table(auth4, c("rulesneg", "rulespos"), "cond", FALSE)

## main effects
t_table(auth4, c("dislike", "authneg"), "cond", FALSE)

## analyses
# interaction and dislike simple slope at cond = expression
summary(lm(authneg ~ cond * dislike, auth4))
# simple slope at cond = suppression
round(summary(lm(authneg ~ cond_relevel * dislike, auth4))$coef[3, ], 3)
# additional predictors
summary(lm(authneg ~ cond * dislike + willpower + fear, auth4))
# positive statements
summary(lm(authpos ~ cond * dislike, auth4))

## problem: correlation between rule-breaking and authenticity is *positive*
## so this supports correspondent inference, but the condition does not?
cor.test(auth4$rulesneg, auth4$authneg)
