library(tidyverse)
library(lme4)
library(lmerTest)
library(mscelns) # not on CRAN, see https://github.com/markhwhiteii/mscelns

auth3 <- read_csv("../data/study3.csv")

## tidying data
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
  mutate(
    black_auth = (black_1 + black_2 + black_3 + black_4) / 4,
    trans_auth = (trans_1 + trans_2 + trans_3 + trans_4) / 4,
    fat_auth = (fat_1 + fat_2 + fat_3 + fat_4) / 4,
    police_auth = (police_1 + police_2 + police_3 + police_4) / 4,
    lawyers_auth = (lawyers_1 + lawyers_2 + lawyers_3 + lawyers_4) / 4,
    business_auth = (business_1 + business_2 + business_3 + business_4) / 4,
    prostitutes_auth = (prostitutes_1 + prostitutes_2 + 
                          prostitutes_3 + prostitutes_4) / 4,
    drug_auth = (dealers_1 + dealers_2 + dealers_3 + dealers_4) / 4,
    blind_auth = (blind_1 + blind_2 + blind_3 + blind_4) / 4,
    deaf_auth = (deaf_1 + deaf_2 + deaf_3 + deaf_4) / 4,
    pc = ((pc_1 - 14) + (pc_2 - 14) + (pc_3 - 14) + (pc_4 - 14) + 
            (pc_5 - 14) + (pc_6 - 14)) / 6,
    rw_polid = ((8 - democrat) + conservative)/ 2
  )

## demographics
nrow(auth3)
summary(auth3$age)
sd(auth3$age, na.rm = TRUE)
table(auth3$gender)
table(auth3$race)
cor.test(auth3$democrat, auth3$conservative)

## power for norm expt
cor.test(~ I(100 - trans_therm) + trans_desc, data = auth3)

## to tidy format
auth3 <- auth3 %>% 
  mutate(id = row_number()) %>% 
  gather(key, value, c(black_desc:deaf_pres, black_therm:deaf_therm, 
                       deaf_auth:black_auth)) %>% 
  separate(key, c("group", "measure"), "[_]") %>% 
  spread(measure, value) %>% 
  rename(authenticity = auth, descriptive = desc, prescriptive = pres) %>% 
  mutate(
    group = as.factor(group),
    id = as.factor(id),
    prejudice = scale(100 - therm),
    descriptive = scale(descriptive),
    prescriptive = scale(prescriptive),
    authenticity = scale(authenticity),
    normativity = scale((descriptive + prescriptive) / 2),
    pc = scale(pc),
    rw_polid = scale(rw_polid)
  )

## analyses
model1 <- lmer(authenticity ~ prejudice + (1 + prejudice | id), data = auth3)
summary(model1)
model1_ranefs <- coef(model1)$id
ggplot() +
  geom_abline(slope = model1_ranefs[ , 2], intercept = model1_ranefs[ , 1], 
              color = "grey", alpha = .7, size = .25) +
  geom_abline(slope = summary(model1)$coef[2, 1], 
              intercept = summary(model1)$coef[1, 1], size = 1) +
  scale_x_continuous(limits = c(-1.5 ,2), name = "Prejudice") +
  scale_y_continuous(limits = c(-2.5, 2.5), name = "Authenticity") +
  theme_light() +
  theme(text = element_text(size = 18))

model2 <- lmer(authenticity ~ descriptive + (1 + descriptive | id), auth3)
summary(model2)

model3 <- lmer(authenticity ~ prejudice * prescriptive + 
                 (1 + prejudice * prescriptive | id), data = auth3)
summary(model3)
mlm_ss(model3, -1, 0, 1)$simple_slopes

model4 <- lmer(authenticity ~ prejudice * pc + 
                 (1 + prejudice | id), data = auth3)
summary(model4)

## "mediation"
med_fit <- lmer(normativity ~ prejudice + 
                  (1 + prejudice | id), 
                data = auth3)
round(summary(med_fit)$coef, 5)
out_fit <- lmer(authenticity ~ prejudice + normativity + 
                  (1 + prejudice + normativity | id), 
                data = auth3)
summary(out_fit)$coef


## prej x pc interaction for each target group separately
lapply(levels(auth3$group), function(x) {
  summary(lm(authenticity ~ prejudice * pc, data = auth3[auth3$group == x, ]))
})
levels(auth3$group)[7]
