library(tidyverse)
dat <- read_csv("../data/study5.csv")
dat <- dat[-73, ] # partial response
dat <- dat %>% 
  mutate(
    neg_auth = (nowill_auth_gen + nowill_true_honest + 
                  lazy_auth_gen + lazy_true_honest) / 4,
    neg_pc = (nowill_pc + nowill_careful_polite +
                lazy_pc + lazy_careful_polite) / 4,
    pos_auth = (enviro_auth_gen + enviro_true_honest + 
                  genes_auth_gen + genes_true_honest) / 4,
    pos_pc = (enviro_pc + enviro_careful_polite +
                genes_pc + genes_careful_polite) / 4,
    prej = (prej_1 + prej_2 + prej_3 + prej_4 +
              prej_5 + prej_6 + prej_7) / 7
  )

round(cor(dat[, 21:28]), 2)
psych::fa(cov(dat[, 21:28]), nfactors = 2, fm = "pa", rotate = "oblimin")

with(dat, cor.test(neg_auth, neg_pc))

t.test(prej ~ cond, dat)

mod1 <- lm(neg_auth ~ prej * cond, dat)
summary(mod1)
#plot(mod1)

ggplot(dat, aes(x = prej, y = neg_auth, color = cond)) +
  geom_jitter() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  labs(x = "Prejudice", y = "Perceived Authenticity") +
  theme(text = element_text(size = 16), legend.position = "top",
        legend.title = element_blank())

mod2 <- lm(neg_pc ~ prej * cond, dat)
summary(mod2)
#plot(mod2)

ggplot(dat, aes(x = prej, y = neg_pc, color = cond)) +
  geom_jitter() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  labs(x = "Prejudice", y = "Perceived 'PC'") +
  theme(text = element_text(size = 16), legend.position = "top",
        legend.title = element_blank())

ggplot(dat, aes(x = prej, y = pos_auth, color = cond)) +
  geom_jitter() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  labs(x = "Prejudice", y = "Perceived Authenticity") +
  theme(text = element_text(size = 16), legend.position = "top",
        legend.title = element_blank())

with(dat, cor.test(prej, pos_auth))

ggplot(dat, aes(x = prej, y = pos_pc, color = cond)) +
  geom_jitter() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  labs(x = "Prejudice", y = "Perceived 'PC'") +
  theme(text = element_text(size = 16), legend.position = "top",
        legend.title = element_blank())

with(dat, cor.test(prej, pos_pc))

round(cor(dat[, c("prej", "neg_auth", "neg_pc", "pos_auth", "pos_pc")]), 3)
