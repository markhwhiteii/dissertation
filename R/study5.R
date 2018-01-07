library(tidyverse)
auth5 <- read_csv("../data/study5.csv") %>% 
  mutate(
    cond = as.factor(cond),
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
auth5 <- auth5[-73, ] # partial response

nrow(auth5)
c(summary(auth5$age), sd(auth5$age))
prop.table(table(auth5$gender))
table(auth5$race)

round(cor(auth5[, 21:28]), 2)
psych::fa.parallel(auth5[, 21:28], fm = "pa")
psych::fa.parallel(auth5[, 29:36], fm = "pa")
psych::fa(cov(auth5[, 21:28]), nfactors = 2, fm = "pa", rotate = "oblimin")
psych::fa(cov(auth5[, 29:36]), nfactors = 2, fm = "pa", rotate = "oblimin")

with(auth5, cor.test(neg_auth, neg_pc))
with(auth5, cor.test(pos_auth, pos_pc))
cor(auth5[, c("prej", "neg_auth", "neg_pc")])

t.test(prej ~ cond, auth5)

mod1 <- lm(neg_auth ~ prej * cond, auth5)
summary(mod1)
summary(lm(neg_auth ~ prej * relevel(cond, "Suppression"), auth5))
#plot(mod1)

with(auth5, cor.test(neg_auth, prej))

mod2 <- lm(neg_pc ~ prej * cond, auth5)
summary(mod2)
#plot(mod2)

with(auth5, cor.test(neg_pc, prej))

mod3 <- lm(pos_auth ~ prej * cond, auth5)
summary(mod3)

mod4 <- lm(pos_pc ~ prej * cond, auth5)
summary(mod4)

ggplot(auth5, aes(x = prej, y = neg_auth, color = cond)) +
  geom_jitter() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  labs(x = "Prejudice", y = "Perceived Authenticity") +
  theme(text = element_text(size = 16), legend.position = "top",
        legend.title = element_blank())

ggplot(auth5, aes(x = prej, y = neg_pc, color = cond)) +
  geom_jitter() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  labs(x = "Prejudice", y = "Perceived 'PC'") +
  theme(text = element_text(size = 16), legend.position = "top",
        legend.title = element_blank())

ggplot(auth5, aes(x = prej, y = pos_auth, color = cond)) +
  geom_jitter() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  labs(x = "Prejudice", y = "Perceived Authenticity") +
  theme(text = element_text(size = 16), legend.position = "top",
        legend.title = element_blank())

ggplot(auth5, aes(x = prej, y = pos_pc, color = cond)) +
  geom_jitter() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  labs(x = "Prejudice", y = "Perceived 'PC'") +
  theme(text = element_text(size = 16), legend.position = "top",
        legend.title = element_blank())
