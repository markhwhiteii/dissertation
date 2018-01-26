library(tidyverse)
auth8 <- read_csv("../data/study8.csv") %>% 
  mutate(
    cond = as.factor(cond),
    auth = (auth1 + auth2 + auth3 + auth4) / 4,
    symrac = (symrac1 + symrac2 + (8 - symrac3) + symrac4 + (8 - symrac5) + 
                (8 - symrac6) + (8 - symrac7) + symrac8) / 8
  )

nrow(auth8)
c(summary(auth8$age), sd(auth8$age))
prop.table(table(auth8$gender))
prop.table(table(auth8$race))

summary(auth8[, grepl("symrac", names(auth8))])

mod <- lm(auth ~ symrac * cond, auth8)
summary(mod)
summary(lm(auth ~ symrac * relevel(cond, ref = "Directional"), auth8))

summary(glm(I(auth == 7) ~ symrac * cond, family = binomial, data = auth8))

cor.test(~ symrac + auth, auth8)

# www.github.com/markhwhiteii/mscelns
mscelns::t_table(auth8, "time", "cond")
summary(lm(time ~ cond * symrac, auth8))
cor.test(~ time + symrac, auth8)

## figure
ggplot(auth8, aes(x = symrac, y = auth, 
                  shape = relevel(cond, ref = "Directional"), 
                  linetype = relevel(cond, ref = "Directional"))) +
  geom_jitter(alpha = .9, height = .1) +
  scale_shape_manual(values = c(16, 21)) +
  geom_smooth(method = "lm", se = FALSE, color = "black", size = .7) +
  theme_light() +
  theme(legend.title = element_blank(), legend.position = "top",
        text = element_text(size = 14)) +
  labs(x = "Prejudice", y = "Perceived Authenticity")
ggsave(file = "../docs/figure8.pdf", width = 8, height = 6, dpi = 300)
