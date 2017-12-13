library(foreign)
library(data.table)
library(magrittr)
library(sandwich)
library(lmtest)

d <- read.csv("/Users/iwang/Documents/GitHub/241_data/response_data/emails_with_times.csv")
d <- data.table(d) 

d$hours <- as.numeric(as.character(d$hours))

ate <- d %>%
  .[ , .(m = mean(hours)), keyby = .(from)] %>%
  .[ , diff(m)]
ate

#differ on sectors
m1 <- lm(d$hours ~ d$sector)
summary(m1)

#differ on sender
m2 <- lm(d$hours ~ d$from)
summary(m2)

anova(m1)
anova(m2)
