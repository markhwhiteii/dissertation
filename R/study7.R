library(tidyverse)
library(mscelns) # https://github.com/markhwhiteii/mscelns

## tidying data
auth7 <- read_csv("../data/study7.csv") %>% 
  mutate(cond = as.factor(cond),
         auth = (dvs_1 + dvs_2) / 2,
         prej = (atts_1 + atts_2 + atts_3) / 3,
         rw_polid = (conserv + (8 - democrat)) / 3)

## demographics
with(auth7, list(
  length(age), summary(age), sd(age), 
  prop.table(table(gender)), prop.table(table(ethnicity))
))

## manip check
t_table(auth7, c("check_self", "check_norm"), "cond", FALSE)
table(auth7$check_self, auth7$cond)
round(prop.table(table(auth7$check_self, auth7$cond), 2), 3)

## scale
cor.test(~ dvs_1 + dvs_2, auth7)
cor(auth7[, c("atts_1", "atts_2", "atts_3")])
psych::fa(auth7[, c("atts_1", "atts_2", "atts_3")], nfactors = 1, fm = "pa")

## primary hypothesis
summary(lm(auth ~ prej * cond, auth7))
summary(lm(auth ~ prej * relevel(cond, ref = "auth_good"), auth7))

## condition on authenticity and prejudice separately
t_table(auth7, c("auth", "prej"), "cond")

## replicate previous findings that it is correlated?
with(auth7, cor.test(auth, prej))

## primary hypothesis works when using the manipulation check
## but p-value isn't really in the range you'd want after digging around in data
summary(lm(auth ~ prej * check_norm, auth7))
cor.test(as.numeric(auth7$cond) - 1, auth7$check_norm)

## figure
ggplot(auth7, aes(x = prej, y = auth, 
                  shape = relevel(cond, ref = "auth_good"), 
                  linetype = relevel(cond, ref = "auth_good"))) +
  geom_jitter(alpha = .9, height = .1) +
  scale_shape_manual(values = c(16, 21), name = "Authenticity is:", 
                     labels = c("Good", "Bad")) +
  geom_smooth(method = "lm", se = FALSE, color = "black", size = .7) +
  scale_linetype_discrete(name = "Authenticity is:", 
                          labels = c("Good", "Bad")) +
  labs(x = "Prejudice", y = "Perceived Authenticity") +
  theme_light() +
  theme(text = element_text(size = 14), legend.position = "top")
ggsave(file = "../docs/figure7.pdf", width = 8, height = 6, dpi = 300)
