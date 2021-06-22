# Imports
library("lme4")
library("merTools")
library("tidyverse")

# Load data
yield = read_csv("../data/FAOSTAT_yield.csv") %>%
  rename(year=Year, yield=Value)

nino = read_csv("../data/nino34.csv") %>%
  gather("month", "nino34", -year) 

amo = read_csv("../data/amo.csv") %>%
  gather("month", "amo", -year)

nao = read_csv("../data/nao.csv") 

dmi = read_csv("../data/dmi.csv") %>%
  gather("month", "dmi", -year)

# Wrangle
data = yield %>%
  filter(Area=="Australia") %>%
  select(year, yield) %>%
  full_join(nino, by=c("year")) %>%
  # full_join(nao, by=c("year", "month")) %>%
  full_join(amo, by=c("year", "month")) %>%
  full_join(dmi, by=c("year", "month")) %>%
  drop_na()

# Linear model
display(lm(yield ~ nino34, data=full))

display(lm(yield ~ nino34 + year, data=full))

# Generalised linear model
display(glm(yield ~ nino34 + year, data=full))

display(glm(yield ~ nino34:year, data=full))

# LME4
display(lmer(yield ~ nino34|year, data=full))

mod1 = lmer(yield ~ (1|year) + (nino34|year), data=data)

summary(lmer(yield ~ (1|year) + nino34, data=data))

summary(mod1)
