library(tidyverse)
library(metafor)
source("rmat.R")
## loading in just variables needed
## collapsing across experimental condition in studies 5 - 8
auth1 <- read_csv("../data/study1.csv") %>% 
  transmute(
    auth_muslim = (
      antimuslim1_1 + antimuslim1_2 + antimuslim1_3 + antimuslim1_4 +
        antimuslim2_1 + antimuslim2_2 + antimuslim2_3 + antimuslim2_4 +
        antimuslim3_1 + antimuslim3_2 + antimuslim3_3 + antimuslim3_4
    ) / 12,
    auth_politician = (
      politicians1_1 + politicians1_2 + politicians1_3 + politicians1_4 +
        politicians2_1 + politicians2_2 + politicians2_3 + politicians2_4 +
        politicians3_1 + politicians3_2 + politicians3_3 + politicians3_4
    ) / 12,
    prej_muslim = (
      muslim1 + muslim2 + muslim3 + (8 - muslim4) + (8 - muslim5) +
        (8 - muslim6) + (8 - muslims7)
    ) / 7,
    prej_politician = (
      (8 - politicians_1) + (8 - politicians_2) + (8 - politicians_3) +
        politicians_4 + politicians_5 + (8 - politicians_6) + 
        (8 - politicians_7)
    ) / 7
  )

auth2 <- read_csv("../data/study2.csv") %>% 
  filter(complete.cases(.)) %>% 
  transmute(
    auth = (quote1_1 + quote1_2 + quote1_3 + quote1_4 + 
              quote2_1 + quote2_2 + quote2_3 + quote2_4) / 8,
    ill_imm_auth = ifelse(cond == 1, auth, NA),
    ksu_auth = ifelse(cond == 0, auth, NA),
    ill_imm_prej = (ill_imm_1 + ill_imm_2 + (8 - ill_imm_3) + 
                 (8 - ill_imm_4) + (8 - ill_imm_5)) / 5,
    ksu_prej = ((8 - ksu_1) + (8 - ksu_2) + (8 - ksu_3) + 
             ksu_4 + (8 - ksu_5)) / 5
  ) %>% 
  select(-auth)
auth2a <- auth2[is.na(auth2$ksu_auth), grepl("ill_imm", names(auth2))]
auth2b <- auth2[!is.na(auth2$ksu_auth), !grepl("ill_imm", names(auth2))]

auth3 <- read_csv("../data/study3.csv")
colnames(auth3)[1:20] <- c(
  "black_desc", "trans_desc", "fat_desc", "police_desc", "lawyers_desc",
  "business_desc", "prostitutes_desc", "drug_desc", "blind_desc", "deaf_desc", 
  "black_pres", "trans_pres", "fat_pres", "police_pres", "lawyers_pres", 
  "business_pres", "prostitutes_pres", "drug_pres", "blind_pres", "deaf_pres"
)
colnames(auth3)[61:70] <- c(
  "black_therm", "trans_therm", "fat_therm", "police_therm", 
  "lawyers_therm", "business_therm", "prostitutes_therm", "drug_therm", 
  "blind_therm", "deaf_therm"
)
auth3 <- auth3 %>% 
  transmute(
    black_auth = (black_1 + black_2 + black_3 + black_4) / 4,
    black_prej = (100 - black_therm),
    trans_auth = (trans_1 + trans_2 + trans_3 + trans_4) / 4,
    trans_prej = (100 - trans_therm),
    fat_auth = (fat_1 + fat_2 + fat_3 + fat_4) / 4,
    fat_prej = (100 - fat_therm),
    police_auth = (police_1 + police_2 + police_3 + police_4) / 4,
    police_prej = (100 - police_therm),
    lawyers_auth = (lawyers_1 + lawyers_2 + lawyers_3 + lawyers_4) / 4,
    lawyers_prej = (100 - lawyers_therm),
    business_auth = (business_1 + business_2 + business_3 + business_4) / 4,
    business_prej = (100 - business_therm),
    prostitutes_auth = (prostitutes_1 + prostitutes_2 + 
                          prostitutes_3 + prostitutes_4) / 4,
    prostitutes_prej = (100 - prostitutes_therm),
    drug_auth = (dealers_1 + dealers_2 + dealers_3 + dealers_4) / 4,
    drug_prej = (100 - drug_therm),
    blind_auth = (blind_1 + blind_2 + blind_3 + blind_4) / 4,
    blind_prej = (100 - blind_therm),
    deaf_auth = (deaf_1 + deaf_2 + deaf_3 + deaf_4) / 4,
    deaf_prej = (100 - deaf_therm)
  ) %>% 
  filter(complete.cases(.))

auth5 <- read_csv("../data/study5.csv")
auth5 <- auth5[-c(61, 195), ] %>% 
  transmute(
    authneg = (authneg11 + authneg12 + authneg13 + authneg14 + 
                 authneg21 + authneg22 + authneg23 + authneg24) / 8,
    dislike = (prej_1 + prej_2 + prej_3 + prej_4 + 
                 prej_5 + prej_6 + prej_7) / 7
  )

auth6 <- read_csv("../data/study6.csv") %>% 
  transmute(
    neg_auth = (nowill_auth_gen + nowill_true_honest + 
                  lazy_auth_gen + lazy_true_honest) / 4,
    prej = (prej_1 + prej_2 + prej_3 + prej_4 +
              prej_5 + prej_6 + prej_7) / 7
  )
auth6 <- auth6[-73, ]

auth7 <- read_csv("../data/study7.csv") %>% 
  transmute(
    auth = (dvs_1 + dvs_2) / 2,
    prej = (atts_1 + atts_2 + atts_3) / 3
  )

auth8 <- read_csv("../data/study8.csv") %>% 
  transmute(
    auth = (auth1 + auth2 + auth3 + auth4) / 4,
    symrac = (symrac1 + symrac2 + (8 - symrac3) + symrac4 + (8 - symrac5) + 
                (8 - symrac6) + (8 - symrac7) + symrac8) / 8
  )

dfs <- list(auth1, auth2a, auth2b, auth3, auth5, auth6, auth7, auth8)
ns <- lapply(dfs, nrow)
cor_mats <- lapply(dfs, function(x) cor(x, use = "pairwise.complete.obs"))

target_cors <- c(
  "auth_muslim.prej_muslim", "auth_politician.prej_politician", 
  "ill_imm_auth.ill_imm_prej", "ksu_auth.ksu_prej", "black_auth.black_prej",
  "trans_auth.trans_prej", "fat_auth.fat_prej", "police_auth.police_prej",
  "lawyers_auth.lawyers_prej", "business_auth.business_prej", 
  "prostitutes_auth.prostitutes_prej", "drug_auth.drug_prej", 
  "blind_auth.blind_prej", "deaf_auth.deaf_prej",
  "authneg.dislike", "neg_auth.prej", "auth.prej", "auth.symrac"
)

for (i in seq_along(dfs)) {
  if (i == 1) {
    dat <- rmat(cor_mats[i], n = ns[[i]])$dat
    dat <- dat[which(dat$var1var2 %in% target_cors), -c(3:4)]
    
    V <- rmat(cor_mats[i], n = ns[[i]])$V
    V <- V[rownames(V) %in% target_cors, colnames(V) %in% target_cors]
  } else {
    dat_ <- rmat(cor_mats[i], n = ns[[i]])$dat
    dat_ <- dat_[which(dat_$var1var2 %in% target_cors), -c(3:4)]
    dat_$id <- i
    dat <- rbind(dat, dat_)
    
    V_ <- rmat(cor_mats[i], n = ns[[i]])$V
    V_ <- V_[rownames(V_) %in% target_cors, colnames(V_) %in% target_cors]
    V <- as.matrix(bdiag(V, V_))
  }
}

# fixing mistake in the loop
dat$id[4] <- 2
dat$id[dat$id == 4] <- 3

fit <- rma.mv(yi = dat$yi, V = V)
fit

statements <- paste0("~", gsub("[.]", "+", dat$var1var2))
datanames <- paste0("auth", dat$id)
lapply(seq_along(statements), function(x) {
  cor.test(as.formula(statements[x]), get(datanames[x]))
})
