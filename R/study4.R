library(tidyverse)
library(mscelns) # www.github.com/markhwhiteii/mscelns
auth4 <- read_csv("../data/study4.csv") %>% 
  mutate(auth = (auth_auth_1 + auth_auth_2 + auth_auth_3 + auth_auth_4) / 4,
         cond = as.factor(cond))

c(summary(auth4$age), sd(auth4$age))
prop.table(table(auth4$gender))
prop.table(table(auth4$race))

t_table(data = auth4, dvs= c("auth", "descriptive"), iv = "cond")
cor.test(~ auth + descriptive, auth4)

ggplot(auth4, aes(x = cond, y = descriptive)) +
  geom_boxplot() +
  labs(x = "Normativity of Prejudice", 
       y = "Perceived Normativity", 
       title = "Manipulation Check Good", 
       subtitle = "t(206) = 9.40, p < .001, d = 1.30 95% CI [1.00, 1.60]") +
  scale_x_discrete(labels = c("High", "Low")) +
  theme_minimal() +
  theme(text = element_text(size = 16))

ggplot(auth4, aes(x = cond, y = auth)) +
  geom_boxplot() +
  labs(x = "Normativity of Prejudice", 
       y = "Perceived Authenticity", 
       title = "God Dammit", 
       subtitle = "t(206) = 1.19, p = .236, d = .16 95% CI [-.11, .44]") +
  scale_x_discrete(labels = c("High", "Low")) +
  theme_minimal() +
  theme(text = element_text(size = 16))
