# Imports
library(dlnm)

# Data
# data2 <- as.data.frame(data)

yield = read_csv("../datadrive/TeamG/data/FAOSTAT_yield.csv") %>%
  select(Year, Area, Value) %>%
  spread(Area, -Year) %>%
  rename_at(names(.), .funs=tolower)

nino = read_csv("../datadrive/TeamG/data/nino34.csv") %>%
  gather("month", "nino34", -year) %>%
  mutate(month=as.numeric(month))

amo = read_csv("../datadrive/TeamG/data/amo.csv") %>%
  gather("month", "amo", -year) %>%
  mutate(month=as.numeric(month))

nao = read_csv("../datadrive/TeamG/data/nao.csv") 

dmi = read_csv("../datadrive/TeamG/data/dmi.csv") %>%
  gather("month", "dmi", -year) %>%
  mutate(month=as.numeric(month))

pdo = read_csv("../datadrive/TeamG/data/pdo.csv") %>%
  mutate(date=as.Date(date)) %>%
  mutate(year=as.numeric(format(date, "%Y"))) %>%
  mutate(month=as.numeric(format(date, "%m")))

data = yield %>%
  full_join(nino, by=c('year')) %>%
  left_join(amo, by=c('year', 'month')) %>%
  left_join(nao, by=c('year', 'month')) %>%
  left_join(dmi, by=c('year', 'month')) %>%
  left_join(pdo, by=c('year', 'month')) %>%
  select(-date) %>%
  drop_na() %>%
  as.data.frame()
names(data) = gsub(" ", "_", names(data))


# Crossbases
cr_nino = crossbasis(data$nino34, lag = 12, argvar = list(fun="bs"), arglag = list(fun="poly", degree=3))
cr_amo = crossbasis(data$amo, lag = 12, argvar = list(fun="bs"), arglag = list(fun="poly", degree=3))
cr_nao = crossbasis(data$nao, lag = 12, argvar = list(fun="bs"), arglag = list(fun="poly", degree=3))
cr_dmi = crossbasis(data$dmi, lag = 12, argvar = list(fun="bs"), arglag = list(fun="poly", degree=3))
cr_pdo = crossbasis(data$pdo, lag = 12, argvar = list(fun="bs"), arglag = list(fun="poly", degree=3))

# Models
# mod_nino <- lm(spain ~ cr_nino, data=data)
# mod_amo <- lm(spain ~ cr_amo, data=data)
# mod_nao <- lm(chile ~ cr_nao, data=data)
# mod_dmi <- lm(chile ~ cr_dmi, data=data)
# mod_pdo <- lm(chile ~ cr_pdo, data=data)

ag_nino <- crosspred(cr_nino, lm(argentina ~ cr_nino, data=data), by = 2)
au_nino <- crosspred(cr_nino, lm(australia ~ cr_nino, data=data), by = 2)
ch_nino <- crosspred(cr_nino, lm(chile ~ cr_nino, data=data), by = 2)
fr_nino <- crosspred(cr_nino, lm(france ~ cr_nino, data=data), by = 2)
it_nino <- crosspred(cr_nino, lm(italy ~ cr_nino, data=data), by = 2)
sa_nino <- crosspred(cr_nino, lm(south_africa ~ cr_nino, data=data), by = 2)
sp_nino <- crosspred(cr_nino, lm(spain ~ cr_nino, data=data), by = 2)
us_nino <- crosspred(cr_nino, lm(united_states_of_america ~ cr_nino, data=data), by = 2)

ag_amo <- crosspred(cr_amo, lm(argentina ~ cr_amo, data=data), by = 2)
au_amo <- crosspred(cr_amo, lm(australia ~ cr_amo, data=data), by = 2)
ch_amo <- crosspred(cr_amo, lm(chile ~ cr_amo, data=data), by = 2)
fr_amo <- crosspred(cr_amo, lm(france ~ cr_amo, data=data), by = 2)
it_amo <- crosspred(cr_amo, lm(italy ~ cr_amo, data=data), by = 2)
sa_amo <- crosspred(cr_amo, lm(south_africa ~ cr_amo, data=data), by = 2)
sp_amo <- crosspred(cr_amo, lm(spain ~ cr_amo, data=data), by = 2)
us_amo <- crosspred(cr_amo, lm(united_states_of_america ~ cr_amo, data=data), by = 2)

ag_nao <- crosspred(cr_nao, lm(argentina ~ cr_nao, data=data), by = 2)
au_nao <- crosspred(cr_nao, lm(australia ~ cr_nao, data=data), by = 2)
ch_nao <- crosspred(cr_nao, lm(chile ~ cr_nao, data=data), by = 2)
fr_nao <- crosspred(cr_nao, lm(france ~ cr_nao, data=data), by = 2)
it_nao <- crosspred(cr_nao, lm(italy ~ cr_nao, data=data), by = 2)
sa_nao <- crosspred(cr_nao, lm(south_africa ~ cr_nao, data=data), by = 2)
sp_nao <- crosspred(cr_nao, lm(spain ~ cr_nao, data=data), by = 2)
us_nao <- crosspred(cr_nao, lm(united_states_of_america ~ cr_nao, data=data), by = 2)

ag_dmi <- crosspred(cr_dmi, lm(argentina ~ cr_dmi, data=data), by = 2)
au_dmi <- crosspred(cr_dmi, lm(australia ~ cr_dmi, data=data), by = 2)
ch_dmi <- crosspred(cr_dmi, lm(chile ~ cr_dmi, data=data), by = 2)
fr_dmi <- crosspred(cr_dmi, lm(france ~ cr_dmi, data=data), by = 2)
it_dmi <- crosspred(cr_dmi, lm(italy ~ cr_dmi, data=data), by = 2)
sa_dmi <- crosspred(cr_dmi, lm(south_africa ~ cr_dmi, data=data), by = 2)
sp_dmi <- crosspred(cr_dmi, lm(spain ~ cr_dmi, data=data), by = 2)
us_dmi <- crosspred(cr_dmi, lm(united_states_of_america ~ cr_dmi, data=data), by = 2)

ag_pdo <- crosspred(cr_pdo, lm(argentina ~ cr_pdo, data=data), by = 2)
au_pdo <- crosspred(cr_pdo, lm(australia ~ cr_pdo, data=data), by = 2)
ch_pdo <- crosspred(cr_pdo, lm(chile ~ cr_pdo, data=data), by = 2)
fr_pdo <- crosspred(cr_pdo, lm(france ~ cr_pdo, data=data), by = 2)
it_pdo <- crosspred(cr_pdo, lm(italy ~ cr_pdo, data=data), by = 2)
sa_pdo <- crosspred(cr_pdo, lm(south_africa ~ cr_pdo, data=data), by = 2)
sp_pdo <- crosspred(cr_pdo, lm(spain ~ cr_pdo, data=data), by = 2)
us_pdo <- crosspred(cr_pdo, lm(united_states_of_america ~ cr_pdo, data=data), by = 2)

# Create plots
png(file="../datadrive/TeamG/plots/ag_nino_lagplot.png", width=800, height=800, res=100)
plot(ag_nino,
     main = "Lagged Grape Yield: Argentina vs ENSO",
     xlab = "Niño 3.4 index",
     ylab = "Time lag (months)",
     zlab = "Yield anomaly (hg/ha)")
dev.off()
png(file="../datadrive/TeamG/plots/ag_amo_lagplot.png", width=800, height=800, res=100)
plot(ag_amo,
     main = "Lagged Grape Yield: Argentina vs AMO",
     xlab = "AMO index",
     ylab = "Time lag (months)",
     zlab = "Yield anomaly (hg/ha)")
dev.off()
png(file="../datadrive/TeamG/plots/ag_nao_lagplot.png", width=800, height=800, res=100)
plot(ag_nao,
     main = "Lagged Grape Yield: Argentina vs NAO",
     xlab = "NAO index",
     ylab = "Time lag (months)",
     zlab = "Yield anomaly (hg/ha)")
dev.off()
png(file="../datadrive/TeamG/plots/ag_dmi_lagplot.png", width=800, height=800, res=100)
plot(ag_dmi,
     main = "Lagged Grape Yield: Argentina vs IOD",
     xlab = "DMI index",
     ylab = "Time lag (months)",
     zlab = "Yield anomaly (hg/ha)")
dev.off()
png(file="../datadrive/TeamG/plots/ag_pdo_lagplot.png", width=800, height=800, res=100)
plot(ag_pdo,
     main = "Lagged Grape Yield: Argentina vs PDO",
     xlab = "PDO index",
     ylab = "Time lag (months)",
     zlab = "Yield anomaly (hg/ha)")
dev.off()

png(file="../datadrive/TeamG/plots/au_nino_lagplot.png", width=800, height=800, res=100)
plot(au_nino,
     main = "Lagged Grape Yield: Australia vs ENSO",
     xlab = "Niño 3.4 index",
     ylab = "Time lag (months)",
     zlab = "Yield anomaly (hg/ha)")
dev.off()
png(file="../datadrive/TeamG/plots/au_amo_lagplot.png", width=800, height=800, res=100)
plot(au_amo,
     main = "Lagged Grape Yield: Australia vs AMO",
     xlab = "AMO index",
     ylab = "Time lag (months)",
     zlab = "Yield anomaly (hg/ha)")
dev.off()
png(file="../datadrive/TeamG/plots/au_nao_lagplot.png", width=800, height=800, res=100)
plot(au_nao,
     main = "Lagged Grape Yield: Australia vs NAO",
     xlab = "NAO index",
     ylab = "Time lag (months)",
     zlab = "Yield anomaly (hg/ha)")
dev.off()
png(file="../datadrive/TeamG/plots/au_dmi_lagplot.png", width=800, height=800, res=100)
plot(au_dmi,
     main = "Lagged Grape Yield: Australia vs IOD",
     xlab = "DMI index",
     ylab = "Time lag (months)",
     zlab = "Yield anomaly (hg/ha)")
dev.off()
png(file="../datadrive/TeamG/plots/au_pdo_lagplot.png", width=800, height=800, res=100)
plot(au_pdo,
     main = "(d) Lagged Grape Yield: Australia vs PDO",
     xlab = "PDO index (°C)",
     ylab = "Time lag (months)",
     zlab = "Yield anomaly (hg/ha)")
dev.off()

png(file=".ch_nino_lagplot.png", width=800, height=800, res=100)
plot(ch_nino,
     main = "Lagged Grape Yield: Chile vs ENSO",
     xlab = "Niño 3.4 index",
     ylab = "Time lag (months)",
     zlab = "Yield anomaly (hg/ha)")
dev.off()
png(file="../datadrive/TeamG/plots/ch_amo_lagplot.png", width=800, height=800, res=100)
plot(ch_amo,
     main = "Lagged Grape Yield: Chile vs AMO",
     xlab = "AMO index",
     ylab = "Time lag (months)",
     zlab = "Yield anomaly (hg/ha)")
dev.off()
png(file="../datadrive/TeamG/plots/ch_nao_lagplot.png", width=800, height=800, res=100)
plot(ch_nao,
     main = "Lagged Grape Yield: Chile vs NAO",
     xlab = "NAO index",
     ylab = "Time lag (months)",
     zlab = "Yield anomaly (hg/ha)")
dev.off()
png(file="../datadrive/TeamG/plots/ch_dmi_lagplot.png", width=800, height=800, res=100)
plot(ch_dmi,
     main = "Lagged Grape Yield: Chile vs IOD",
     xlab = "DMI index",
     ylab = "Time lag (months)",
     zlab = "Yield anomaly (hg/ha)")
dev.off()
png(file="../datadrive/TeamG/plots/ch_pdo_lagplot.png", width=800, height=800, res=100)
plot(ch_pdo,
     main = "Lagged Grape Yield: Chile vs PDO",
     xlab = "PDO index",
     ylab = "Time lag (months)",
     zlab = "Yield anomaly (hg/ha)")
dev.off()

png(file="../datadrive/TeamG/plots/fr_nino_lagplot.png", width=800, height=800, res=100)
plot(fr_nino,
     main = "Lagged Grape Yield: France vs ENSO",
     xlab = "Niño 3.4 index",
     ylab = "Time lag (months)",
     zlab = "Yield anomaly (hg/ha)")
dev.off()
png(file="../datadrive/TeamG/plots/fr_amo_lagplot.png", width=800, height=800, res=100)
plot(fr_amo,
     main = "Lagged Grape Yield: France vs AMO",
     xlab = "AMO index",
     ylab = "Time lag (months)",
     zlab = "Yield anomaly (hg/ha)")
dev.off()
png(file="../datadrive/TeamG/plots/fr_nao_lagplot.png", width=800, height=800, res=100)
plot(fr_nao,
     main = "Lagged Grape Yield: France vs NAO",
     xlab = "NAO index",
     ylab = "Time lag (months)",
     zlab = "Yield anomaly (hg/ha)")
dev.off()
png(file="../datadrive/TeamG/plots/fr_dmi_lagplot.png", width=800, height=800, res=100)
plot(fr_dmi,
     main = "Lagged Grape Yield: France vs IOD",
     xlab = "DMI index",
     ylab = "Time lag (months)",
     zlab = "Yield anomaly (hg/ha)")
dev.off()
png(file="../datadrive/TeamG/plots/fr_pdo_lagplot.png", width=800, height=800, res=100)
plot(fr_pdo,
     main = "Lagged Grape Yield: France vs PDO",
     xlab = "PDO index",
     ylab = "Time lag (months)",
     zlab = "Yield anomaly (hg/ha)")
dev.off()

png(file="../datadrive/TeamG/plots/it_nino_lagplot.png", width=800, height=800, res=100)
plot(it_nino,
     main = "Lagged Grape Yield: Italy vs ENSO",
     xlab = "Niño 3.4 index",
     ylab = "Time lag (months)",
     zlab = "Yield anomaly (hg/ha)")
dev.off()
png(file="../datadrive/TeamG/plots/it_amo_lagplot.png", width=800, height=800, res=100)
plot(it_amo,
     main = "Lagged Grape Yield: Italy vs AMO",
     xlab = "AMO index",
     ylab = "Time lag (months)",
     zlab = "Yield anomaly (hg/ha)")
dev.off()
png(file="../datadrive/TeamG/plots/it_nao_lagplot.png", width=800, height=800, res=100)
plot(it_nao,
     main = "Lagged Grape Yield: Italy vs NAO",
     xlab = "NAO index",
     ylab = "Time lag (months)",
     zlab = "Yield anomaly (hg/ha)")
dev.off()
png(file="../datadrive/TeamG/plots/it_dmi_lagplot.png", width=800, height=800, res=100)
plot(it_dmi,
     main = "Lagged Grape Yield: Italy vs IOD",
     xlab = "DMI index",
     ylab = "Time lag (months)",
     zlab = "Yield anomaly (hg/ha)")
dev.off()
png(file="../datadrive/TeamG/plots/it_pdo_lagplot.png", width=800, height=800, res=100)
plot(it_pdo,
     main = "Lagged Grape Yield: Italy vs PDO",
     xlab = "PDO index",
     ylab = "Time lag (months)",
     zlab = "Yield anomaly (hg/ha)")
dev.off()

png(file="../datadrive/TeamG/plots/sa_nino_lagplot.png", width=800, height=800, res=100)
plot(sa_nino,
     main = "Lagged Grape Yield: South Africa vs ENSO",
     xlab = "Niño 3.4 index",
     ylab = "Time lag (months)",
     zlab = "Yield anomaly (hg/ha)")
dev.off()
png(file="../datadrive/TeamG/plots/sa_amo_lagplot.png", width=800, height=800, res=100)
plot(sa_amo,
     main = "Lagged Grape Yield: South Africa vs AMO",
     xlab = "AMO index",
     ylab = "Time lag (months)",
     zlab = "Yield anomaly (hg/ha)")
dev.off()
png(file="../datadrive/TeamG/plots/sa_nao_lagplot.png", width=800, height=800, res=100)
plot(sa_nao,
     main = "Lagged Grape Yield: South Africa vs NAO",
     xlab = "NAO index",
     ylab = "Time lag (months)",
     zlab = "Yield anomaly (hg/ha)")
dev.off()
png(file="../datadrive/TeamG/plots/sa_dmi_lagplot.png", width=800, height=800, res=100)
plot(sa_dmi,
     main = "Lagged Grape Yield: South Africa vs IOD",
     xlab = "DMI index",
     ylab = "Time lag (months)",
     zlab = "Yield anomaly (hg/ha)")
dev.off()
png(file="../datadrive/TeamG/plots/sa_pdo_lagplot.png", width=800, height=800, res=100)
plot(sa_pdo,
     main = "Lagged Grape Yield: South Africa vs PDO",
     xlab = "PDO index",
     ylab = "Time lag (months)",
     zlab = "Yield anomaly (hg/ha)")
dev.off()

png(file="../datadrive/TeamG/plots/sp_nino_lagplot.png", width=800, height=800, res=100)
plot(sp_nino,
     main = "Lagged Grape Yield: Spain vs ENSO",
     xlab = "Niño 3.4 index",
     ylab = "Time lag (months)",
     zlab = "Yield anomaly (hg/ha)")
dev.off()
png(file="../datadrive/TeamG/plots/sp_amo_lagplot.png", width=800, height=800, res=100)
plot(sp_amo,
     main = "Lagged Grape Yield: Spain vs AMO",
     xlab = "AMO index",
     ylab = "Time lag (months)",
     zlab = "Yield anomaly (hg/ha)")
dev.off()
png(file="../datadrive/TeamG/plots/sp_nao_lagplot.png", width=800, height=800, res=100)
plot(sp_nao,
     main = "Lagged Grape Yield: Spain vs NAO",
     xlab = "NAO index",
     ylab = "Time lag (months)",
     zlab = "Yield anomaly (hg/ha)")
dev.off()
png(file="../datadrive/TeamG/plots/sp_dmi_lagplot.png", width=800, height=800, res=100)
plot(sp_dmi,
     main = "Lagged Grape Yield: Spain vs IOD",
     xlab = "DMI index",
     ylab = "Time lag (months)",
     zlab = "Yield anomaly (hg/ha)")
dev.off()
png(file="../datadrive/TeamG/plots/sp_pdo_lagplot.png", width=800, height=800, res=100)
plot(sp_pdo,
     main = "Lagged Grape Yield: Spain vs PDO",
     xlab = "PDO index",
     ylab = "Time lag (months)",
     zlab = "Yield anomaly (hg/ha)")
dev.off()

png(file="../datadrive/TeamG/plots/us_nino_lagplot.png", width=800, height=800, res=100)
plot(us_nino,
     main = "(c) Lagged Grape Yield: USA vs ENSO",
     xlab = "Niño 3.4 index (°C)",
     ylab = "Time lag (months)",
     zlab = "Yield anomaly (hg/ha)")
dev.off()
png(file="../datadrive/TeamG/plots/us_amo_lagplot.png", width=800, height=800, res=100)
plot(us_amo,
     main = "Lagged Grape Yield: USA vs AMO",
     xlab = "AMO index",
     ylab = "Time lag (months)",
     zlab = "Yield anomaly (hg/ha)")
dev.off()
png(file="../datadrive/TeamG/plots/us_nao_lagplot.png", width=800, height=800, res=100)
plot(us_nao,
     main = "Lagged Grape Yield: USA vs NAO",
     xlab = "NAO index",
     ylab = "Time lag (months)",
     zlab = "Yield anomaly (hg/ha)")
dev.off()
png(file="../datadrive/TeamG/plots/us_dmi_lagplot.png", width=800, height=800, res=100)
plot(us_dmi,
     main = "Lagged Grape Yield: USA vs IOD",
     xlab = "DMI index",
     ylab = "Time lag (months)",
     zlab = "Yield anomaly (hg/ha)")
dev.off()
png(file="../datadrive/TeamG/plots/us_pdo_lagplot.png", width=800, height=800, res=100)
plot(us_pdo,
     main = "Lagged Grape Yield: USA vs PDO",
     xlab = "PDO index",
     ylab = "Time lag (months)",
     zlab = "Yield anomaly (hg/ha)")
dev.off()

# plot(au_nino)
# plot(au_amo)
# plot(au_nao)
# plot(au_dmi)
# plot(au_pdo)


