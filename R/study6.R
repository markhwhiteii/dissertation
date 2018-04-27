library(tidyverse)
auth6 <- read_csv("../data/study6.csv") %>% 
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
auth6 <- auth6[-73, ] # partial response

nrow(auth6)
c(summary(auth6$age), sd(auth6$age))
prop.table(table(auth6$gender))
table(auth6$race)

round(cor(auth6[, 21:28]), 2)
psych::fa.parallel(auth6[, 21:28], fm = "pa")
psych::fa.parallel(auth6[, 29:36], fm = "pa")
psych::fa(cov(auth6[, 21:28]), nfactors = 2, fm = "pa", rotate = "oblimin")
psych::fa(cov(auth6[, 29:36]), nfactors = 2, fm = "pa", rotate = "oblimin")

with(auth6, cor.test(neg_auth, neg_pc))
with(auth6, cor.test(pos_auth, pos_pc))
cor(auth6[, c("prej", "neg_auth", "neg_pc")])

t.test(prej ~ cond, auth6)

mod1 <- lm(neg_auth ~ prej * cond, auth6)
summary(mod1)
summary(lm(neg_auth ~ prej * relevel(cond, "Suppression"), auth6))
#plot(mod1)

with(auth6, cor.test(neg_auth, prej))

mod2 <- lm(neg_pc ~ prej * cond, auth6)
summary(mod2)
#plot(mod2)

with(auth6, cor.test(neg_pc, prej))

mod3 <- lm(pos_auth ~ prej * cond, auth6)
summary(mod3)

mod4 <- lm(pos_pc ~ prej * cond, auth6)
summary(mod4)

ggplot(auth6, aes(x = prej, y = neg_auth, color = cond)) +
  geom_jitter() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  labs(x = "Prejudice", y = "Perceived Authenticity") +
  theme(text = element_text(size = 16), legend.position = "top",
        legend.title = element_blank())

ggplot(auth6, aes(x = prej, y = neg_pc, color = cond)) +
  geom_jitter() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  labs(x = "Prejudice", y = "Perceived 'PC'") +
  theme(text = element_text(size = 16), legend.position = "top",
        legend.title = element_blank())

ggplot(auth6, aes(x = prej, y = pos_auth, color = cond)) +
  geom_jitter() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  labs(x = "Prejudice", y = "Perceived Authenticity") +
  theme(text = element_text(size = 16), legend.position = "top",
        legend.title = element_blank())

ggplot(auth6, aes(x = prej, y = pos_pc, color = cond)) +
  geom_jitter() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  labs(x = "Prejudice", y = "Perceived 'PC'") +
  theme(text = element_text(size = 16), legend.position = "top",
        legend.title = element_blank())

# bayesian analyses, per holger's defense comment, using study 5 estimates
# as informative priors
library(rstanarm)
set.seed(1839)
bayes_mod <- stan_glm(
  formula = neg_auth ~ cond * prej, 
  family = gaussian(),
  data = auth6,
  prior_intercept = normal(5.75882, 0.26368),
  prior = normal(c(-1.69598, -0.01981, 0.36342), c(0.34549, 0.08474, .11513))
)
summary(bayes_mod, digits = 2)
prop.table(table(as.data.frame(bayes_mod)[["condSuppression:prej"]] > 0))
