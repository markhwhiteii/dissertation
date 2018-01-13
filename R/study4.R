library(tidyverse)
auth4 <- read_csv("../data/study4.csv") %>% 
  mutate(auth = (auth_auth_1 + auth_auth_2 + auth_auth_3 + auth_auth_4) / 4,
         cond = as.factor(cond))

t.test(descriptive ~ cond, auth4, var.equal = TRUE)
t.test(auth ~ cond, auth4, var.equal = TRUE)
cor.test(~ auth + descriptive, auth4)

MBESS::ci.smd(ncp = t.test(auth ~ cond, auth4)[[1]][[1]],
              n.1 = sum(auth4$cond == "hi"), n.2 = sum(auth4$cond == "lo"))

MBESS::ci.smd(ncp = t.test(descriptive ~ cond, auth4)[[1]][[1]],
              n.1 = sum(auth4$cond == "hi"), n.2 = sum(auth4$cond == "lo"))

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
