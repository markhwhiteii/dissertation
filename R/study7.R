library(tidyverse)
library(ggExtra)
auth7 <- read_csv("../data/study7.csv") %>% 
  mutate(
    auth = (auth1 + auth2 + auth3 + auth4) / 4,
    symrac = (symrac1 + symrac2 + (8 - symrac3) + symrac4 + (8 - symrac5) + 
                (8 - symrac6) + (8 - symrac7) + symrac8) / 8
  )

summary(auth7[, grepl("symrac", names(auth7))])

psych::fa.parallel(auth7[, c("auth1", "auth2", "auth3", "auth4")], fm = "pa")
psych::fa.parallel(auth7[, c("symrac1", "symrac2", "symrac3", "symrac4",
                           "symrac5", "symrac6", "symrac7", "symrac8")], 
                   fm = "pa")
psych::fa(auth7[, c("symrac1", "symrac2", "symrac3", "symrac4",
                  "symrac5", "symrac6", "symrac7", "symrac8")],
          nfactors = 1, fm = "pa")

mod <- lm(auth ~ symrac * cond, auth7)
summary(mod)

plot <- ggplot(auth7, aes(x = symrac, y = auth, group = cond, color = cond)) +
  geom_jitter(size = 2) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Prejudice", y = "Authenticity") +
  theme_light() +
  theme(legend.title = element_blank(), legend.position = "bottom",
        text = element_text(size = 16))

ggMarginal(plot, type = "histogram")

cor.test(~ symrac + auth, auth7)
summary(censReg(auth ~ symrac, 1, 7, auth7))

t.test(auth ~ cond, auth7)
t.test(time ~ cond, auth7)
t.test(symrac ~ cond, auth7)
