library(tidyverse)
library(Rmisc)
library(mscelns) # www.github.com/markhwhiteii/mscelns
auth4 <- read_csv("../data/study4.csv") %>% 
  mutate(auth = (auth_auth_1 + auth_auth_2 + auth_auth_3 + auth_auth_4) / 4,
         cond = as.factor(cond))

c(summary(auth4$age), sd(auth4$age))
prop.table(table(auth4$gender))
prop.table(table(auth4$race))

t_table(data = auth4, dvs= c("auth", "descriptive"), iv = "cond")
cor.test(~ auth + descriptive, auth4)

auth4_sumstats <- summarySE(auth4, measure = "auth", groupvars = "cond")

ggplot(auth4, aes(x = cond, y = auth)) +
  geom_violin() +
  labs(x = "Normativity of Prejudice", 
       y = "Perceived Authenticity") +
  scale_x_discrete(labels = c("High", "Low")) +
  theme_light() +
  theme(text = element_text(size = 14)) +
  geom_errorbar(data = auth4_sumstats, 
                mapping = aes(ymax = auth + ci, ymin = auth - ci), 
                width = .05) +
  geom_point(data = auth4_sumstats, stat = "identity") +
  coord_flip()
ggsave(file = "../docs/figure5.pdf", width = 8, height = 6, dpi = 300)
