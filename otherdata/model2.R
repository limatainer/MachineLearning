library(tidyverse)
library(readr)
library(GGally)
library(MASS)
library(car)

data <- read_csv("deploy1.csv")

model <- lm(NPS ~ Year+Week+Site+Tier+Language+Channel+TTR, data = data)
bc <- boxCox(model, family = "yjPower")


mylambda = bc$x[which.max(bc$y)]

data$NPS2 <- yjPower(data$NPS, mylambda)


data <- drop_na(data, TTR)
model2 <- lm(NPS2 ~ Year + Week + TTR + Site + Tier + Language + Channel, data = data)
data <- drop_na(data, TTR)
model2 <- lm(NPS2 ~ as.factor(Year) + as.factor(Week) + TTR + Site + Tier + Language + Channel, data = data)


data$rstandard <- rstandard(model2)

data <- data %>% filter(rstandard > -3 & rstandard < 3)
model2 <- lm(NPS ~ Year + Week + TTR + Site + Tier + Language + Channel, data = data)

saveRDS(model2, 'model2.rds')

