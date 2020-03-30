library(data.table)
library(ggplot2)
library(scales)


# Exponential method ------------------------------------------------------

conf = fread("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv")

names(conf)[1:2] = c("province", "country")

# Mexico ------------------------------------------------------------------

mex = conf[country == "Mexico", -c("province", "Lat", "Long")]
mex = melt(mex, id.vars = "country", variable.name = "date", variable.factor = F, value.name = "conf")
mex[, date := as.Date(date, "%m/%d/%y")]
mex = mex[conf > 0]
mex[, t := .I]
mod1 = glm(conf ~ t, data = mex, family = gaussian("log"))
mex[, predicted := predict(mod1, type = "response")]

mex_pred = data.table(
  country = mex[, unique(country)],
  date = seq(mex[, max(date) + 1], mex[, max(date) + 11], 1),
  conf = NA,
  t = mex[, max(t) + 1]:mex[, max(t) + 11]
)
mex_pred[, predicted := predict(
  mod1,
  newdata = mex_pred[, .(t)],
  type = "response"
)]
mex_pred = rbindlist(list(mex, mex_pred))

expected = percent(exp(mod1[[1]][2]) - 1)

ggplot(mex_pred, aes(x = date, y = conf)) +
  geom_point() +
  geom_line(aes(y = predicted, color = expected)) + 
  scale_x_date(date_breaks = "2 days", date_labels = "%d %b")

# Spain -------------------------------------------------------------------

spn = conf[country == "Spain", -c("province", "Lat", "Long")]
spn = melt(spn, id.vars = "country", variable.name = "date", variable.factor = F, value.name = "conf")
spn[, date := as.Date(date, "%m/%d/%y")]
spn = spn[conf > 0]
spn[, t := .I]
mod1 = glm(conf ~ t, data = spn, family = gaussian("log"))

c.0 = min(spn$conf) * 0.5
mod0 = lm(log(conf - c.0) ~ t, data = spn)
start = list(a = exp(coef(mod0)[1]), b = coef(mod0)[2], c = c.0)
mod2 = nls(conf ~ SSlogis(t),
           data = spn)

nls(conf ~ SSlogis(t),
    data = spn)


spn[, predicted := predict(mod1, type = "response")]

spn_pred = data.table(
  country = spn[, unique(country)],
  date = seq(spn[, max(date) + 1], spn[, max(date) + 11], 1),
  conf = NA,
  t = spn[, max(t) + 1]:spn[, max(t) + 11]
)
spn_pred[, predicted := predict(
  mod1,
  newdata = spn_pred[, .(t)],
  type = "response"
)]
spn_pred = rbindlist(list(spn, spn_pred))

expected = percent(exp(mod1[[1]][2]) - 1)

ggplot(spn_pred, aes(x = date, y = conf)) +
  geom_point() +
  geom_line(aes(y = predicted, color = expected)) + 
  scale_x_date(date_breaks = "5 days", date_labels = "%d %b")



# ECSE --------------------------------------------------------------------

download.file("https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide-2020-03-19.xlsx", paste0("covid19-ECDC-", Sys.Date(), ".xlsx"))
ecdc = as.data.table(readxl::read_excel("covid19-ECDC-2020-03-20.xlsx"))

ecdc[GeoId == "ES"]


