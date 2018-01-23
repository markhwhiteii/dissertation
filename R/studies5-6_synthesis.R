library(tidyverse)
auth5 <- read_csv("../data/study5.csv")
auth5 <- auth5[-c(61, 195), ] %>%
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
auth6 <- auth6[-73, ]

dat <- auth5[, c("cond", "dislike", "authneg")] %>% 
  transmute(cond = cond, prej = dislike, neg_auth = authneg) %>% 
  bind_rows(auth6[, c("cond", "prej", "neg_auth")]) %>% 
  mutate(study = factor(c(
    rep("study5", nrow(auth5)), rep("study6", nrow(auth6))
  )))

summary(lm(neg_auth ~ prej * cond, dat))
summary(lm(neg_auth ~ prej * relevel(cond, "Suppression"), dat))
summary(lm(neg_auth ~ prej * cond * study, dat))


