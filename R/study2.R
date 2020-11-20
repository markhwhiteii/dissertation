library(emmeans)
library(tidyverse)
auth2 <- read_csv("../data/study2.csv")
nrow(auth2)
auth2 <- auth2[complete.cases(auth2), ]
nrow(auth2)

## tidying data
auth2 <- auth2 %>% 
  mutate(
    cond = as.factor(ifelse(cond == 1, "ill_imm", "ksu")),
    cond_relevel = factor(cond, levels = c("ksu", "ill_imm")),
    auth = (quote1_1 + quote1_2 + quote1_3 + quote1_4 + 
              quote2_1 + quote2_2 + quote2_3 + quote2_4) / 8,
    ill_imm = (ill_imm_1 + ill_imm_2 + (8 - ill_imm_3) + 
                 (8 - ill_imm_4) + (8 - ill_imm_5)) / 5,
    ksu = ((8 - ksu_1) + (8 - ksu_2) + (8 - ksu_3) + 
             ksu_4 + (8 - ksu_5)) / 5
  )

## correlation between prejudices
cor.test(auth2$ill_imm, auth2$ksu)

## analyses
# ill_imm by condition
model_illimm <- lm(auth ~ ksu + ill_imm * cond, data = auth2)
summary(model_illimm)
test(emtrends(model_illimm, ~ cond, var = "ill_imm"))

# ksu by condition
model_ksu <- lm(auth ~ ill_imm + ksu * cond, data = auth2)
summary(model_ksu)
test(emtrends(model_ksu, ~ cond, var = "ksu"))

## figure
auth2_fig <- auth2 %>% 
  select(cond, auth:ksu) %>% 
  mutate(id = 1:n()) %>% 
  gather(group, prej, ill_imm, ksu) %>% 
  mutate(cond = ifelse(cond == "ill_imm", 
                       "Illegal Immigrants",
                       "Kansas State Students"),
         group = ifelse(group == "ill_imm", 
                        "Illegal Immigrants",
                        "Kansas State Students"))

ggplot(auth2_fig, aes(x = prej, y = auth, shape = group, linetype = group)) +
  geom_jitter(alpha = .9, height = .1) +
  scale_shape_manual(values = c(16, 21)) +
  geom_smooth(method = "lm", se = FALSE, color = "black", size = .7) +
  theme_light() +
  theme(legend.title = element_blank(), legend.position = "bottom",
        text = element_text(size = 14)) +
  facet_wrap(~ cond) +
  labs(x = "Prejudice", y = "Perceived Authenticity")
ggsave(file = "../docs/figure2.pdf", width = 8, height = 6, dpi = 300)
