library(foreign)
library(data.table)
library(magrittr)
library(sandwich)
library(lmtest)

d <- read.csv("/Users/iwang/Documents/GitHub/241_data/response_data/emails_with_times.csv")
d <- data.table(d) 

d$hours <- as.numeric(as.character(d$hours))

d$hours <- na.omit(d$hours)

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

#combine sector and sender
m3<- lm(d$hours ~ d$sector + d$from)
summary(m3)
anova(m3)

cl <- function(fm, cluster){
  require(sandwich, quietly = TRUE)
  require(lmtest, quietly = TRUE)
  M <- length(unique(cluster))
  N <- length(cluster)
  K <- fm$rank
  dfc <- (M/(M-1))*((N-1)/(N-K))
  uj <- apply(estfun(fm),2, function(x) tapply(x, cluster, sum));
  vcovCL <- dfc*sandwich(fm, meat=crossprod(uj)/N)
  coeftest(fm, vcovCL)
}

cl(m3, d$from)

cl(m3, d$from)[11]


#combine sector and sender
m4<- lm(d$hours ~ d$from + d$sector)
summary(m4)
anova(m4)

cl(m4, d$sector)
cl(m4, d$sector)[11]
