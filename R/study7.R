library(tidyverse)
library(ggExtra)
auth7 <- read_csv("../data/study7.csv") %>% 
  mutate(
    cond = as.factor(cond),
    auth = (auth1 + auth2 + auth3 + auth4) / 4,
    symrac = (symrac1 + symrac2 + (8 - symrac3) + symrac4 + (8 - symrac5) + 
                (8 - symrac6) + (8 - symrac7) + symrac8) / 8
  )

nrow(auth7)
c(summary(auth7$age), sd(auth7$age))
prop.table(table(auth7$gender))
prop.table(table(auth7$race))

summary(auth7[, grepl("symrac", names(auth7))])

mod <- lm(auth ~ symrac * cond, auth7)
summary(mod)
summary(lm(auth ~ symrac * relevel(cond, ref = "Directional"), auth7))

summary(glm(I(auth == 7) ~ symrac * cond, family = binomial, data = auth7))

plot <- ggplot(auth7, aes(x = symrac, y = auth, group = cond, color = cond)) +
  geom_jitter(size = 2) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Prejudice", y = "Authenticity") +
  theme_light() +
  theme(legend.title = element_blank(), legend.position = "bottom",
        text = element_text(size = 16))

ggMarginal(plot, type = "histogram")

cor.test(~ symrac + auth, auth7)
