# Imports
library(tidyverse)

# Data
yield = read_csv("../data/FAOSTAT_yield.csv") %>%
  select(Year, Area, Value) %>%
  spread(Area, -Year) %>%
  rename_at(names(.), .funs=tolower)

# Munge
data_s = yield %>%
  select(c('year', 'argentina', 'australia', 'chile', 'south africa')) %>%
  left_join(read_csv("../data/nino34_annual_mean_south.csv"), 'year') %>%
  left_join(read_csv("../data/amo_annual_mean_south.csv"), 'year') %>%
  left_join(read_csv("../data/nao_annual_mean_south.csv"), 'year') %>%
  left_join(read_csv("../data/dmi_annual_mean_south.csv"), 'year') %>%
  left_join(read_csv("../data/pdo_annual_mean_south.csv"), 'year') %>%
  left_join(read_csv("../data/nino34_annual_variance_south.csv"), 'year') %>%
  left_join(read_csv("../data/amo_annual_variance_south.csv"), 'year') %>%
  left_join(read_csv("../data/nao_annual_variance_south.csv"), 'year') %>%
  left_join(read_csv("../data/dmi_annual_variance_south.csv"), 'year') %>%
  left_join(read_csv("../data/pdo_annual_variance_south.csv"), 'year') %>%
  drop_na()
names(data_s) = gsub(" ", "_", names(data_s))

data_n = yield %>%
  select(c('year', 'france', 'italy', 'spain', 'united states of america')) %>%
  left_join(read_csv("../data/nino34_annual_mean_north.csv"), 'year') %>%
  left_join(read_csv("../data/amo_annual_mean_north.csv"), 'year') %>%
  left_join(read_csv("../data/nao_annual_mean_north.csv"), 'year') %>%
  left_join(read_csv("../data/dmi_annual_mean_north.csv"), 'year') %>%
  left_join(read_csv("../data/pdo_annual_mean_north.csv"), 'year') %>%
  left_join(read_csv("../data/nino34_annual_variance_north.csv"), 'year') %>%
  left_join(read_csv("../data/amo_annual_variance_north.csv"), 'year') %>%
  left_join(read_csv("../data/nao_annual_variance_north.csv"), 'year') %>%
  left_join(read_csv("../data/dmi_annual_variance_north.csv"), 'year') %>%
  left_join(read_csv("../data/pdo_annual_variance_north.csv"), 'year') %>%
  drop_na()
names(data_n) = gsub(" ", "_", names(data_n))

# Linear model

north = c('france', 'italy', 'spain', 'united_states_of_america')
south = c('argentina', 'australia', 'chile', 'south_africa')

mods = list()
for (val in south) {
  fms = " ~ pdo_mean_south + nino34_mean_south + dmi_mean_south + amo_mean_south + nao_mean_south"
  mods[[val]] = summary(lm(paste(val, fms), data=data_s))
}

modsv = list()
for (val in south) {
  fms = " ~ pdo_variance_south + nino34_variance_south + dmi_variance_south + amo_variance_south + nao_variance_south"
  modsv[[val]] = summary(lm(paste(val, fms), data=data_s))
}

modnv = list()
for (val in north) {
  fmn = " ~ pdo_variance_north + nino34_variance_north + dmi_variance_north + amo_variance_north + nao_variance_north"
  modnv[[val]] = summary(lm(paste(val, fmn), data=data_n))
}

modnv
