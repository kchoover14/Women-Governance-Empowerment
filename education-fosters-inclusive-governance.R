######################## LIBRARIES

library(vdemdata) #data
library(wbstats) #data
library(countrycode) #standardize countrycodes
library(dplyr) #data wrangling
library(ggplot2) #static plots
library(gganimate) #animated plot
library(gifski) #animation
library(psych) #descriptives
library(glue) #string interpolation for printed output

######################## GET VDEM DATA

democracy = vdem |>
  filter(year >= 2003) |>
  select(
    country = country_name,
    vDemCtryId = country_id,
    year,
    egalDem = v2x_egaldem,
    suff = v2x_suffr,
    region = e_regionpol_6C
  ) |>
  mutate(region = case_match(region,
    1 ~ "Eastern Europe",
    2 ~ "Latin America",
    3 ~ "Middle East",
    4 ~ "Africa",
    5 ~ "The West",
    6 ~ "Asia")
  )

######################## GET WORLD BANK DATA

indicators = c(womenRep = "SG.GEN.PARL.ZS", eduEqual = "SE.ENR.PRSC.FM.ZS")

womenEmp = wb_data(indicators, mrv = 50) |>
  select(!iso2c) |>
  rename(year = date)

######################## MERGE VDEM AND WORLD BANK

democracy = democracy |>
  mutate(iso3c = countrycode(
    sourcevar = vDemCtryId,
    origin = "vdem",
    destination = "wb")
  )

demWomen = left_join(democracy, womenEmp, by = c("iso3c", "year")) |>
  rename(country = country.x) |>
  select(!country.y) |>
  relocate(iso3c, .before = vDemCtryId) |>
  relocate(region, .before = year)

######################## CHECK MISSING DATA

glue("Missing cases for women's representation: {sum(is.na(womenEmp$womenRep))}")
glue("Missing cases for educational equality: {sum(is.na(womenEmp$eduEqual))}")

######################## CORRELATION PLOT

png("correlation_plot.png", width = 800, height = 800, res = 120)
pairs.panels(
  demWomen[, 6:9],
  lm = TRUE,
  cor = TRUE,
  method = "pearson",
  ellipses = TRUE,
  hist.col = "lightsteelblue4",
  smoother = TRUE,
  pch = 21,
  stars = TRUE,
  ci = TRUE
)
dev.off()

######################## ANIMATED PLOT

demWomen$year = as.integer(demWomen$year)

p = ggplot(demWomen, aes(eduEqual, egalDem, size = womenRep, colour = region)) +
  geom_point(alpha = 0.7, show.legend = FALSE) +
  scale_color_viridis_d() +
  facet_wrap(~region) +
  labs(
    title = "Year: {frame_time}",
    x = "Gender Education Parity",
    y = "Egalitarian Democracy",
    caption = "Sources: V-Dem Institute; World Bank"
  ) +
  transition_time(year) +
  enter_fade() +
  exit_fade() +
  ease_aes("linear") +
  theme_classic()

animate(p, nframes = 200, fps = 3)
anim_save("women_governance_animation.gif")

######################## REGIONAL SUMMARY STATISTICS

demSummaryRegionMean = demWomen |>
  group_by(region) |>
  summarize(
    egalDem = mean(egalDem, na.rm = TRUE),
    suff = mean(suff, na.rm = TRUE),
    eduEqual = mean(eduEqual, na.rm = TRUE),
    womenRep = mean(womenRep, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(desc(womenRep))

demSummaryRegionMedian = demWomen |>
  group_by(region) |>
  summarize(
    egalDem = median(egalDem, na.rm = TRUE),
    suff = median(suff, na.rm = TRUE),
    eduEqual = median(eduEqual, na.rm = TRUE),
    womenRep = median(womenRep, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(desc(womenRep))

demSummaryRegionIQR = demWomen |>
  group_by(region) |>
  summarize(
    egalDem = IQR(egalDem, na.rm = TRUE),
    eduEqual = IQR(eduEqual, na.rm = TRUE),
    womenRep = IQR(womenRep, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(desc(womenRep))

######################## COUNTRY SUMMARY STATISTICS

demSummaryCountryMedian = demWomen |>
  group_by(country) |>
  summarize(
    womenRep = median(womenRep, na.rm = TRUE),
    egalDem = median(egalDem, na.rm = TRUE),
    suff = median(suff, na.rm = TRUE),
    eduEqual = median(eduEqual, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(country)

demSummaryCountryIQR = demWomen |>
  group_by(country) |>
  summarize(
    womenRep = round(IQR(womenRep, na.rm = TRUE), digits = 3),
    egalDem = round(IQR(egalDem, na.rm = TRUE), digits = 3),
    eduEqual = round(IQR(eduEqual, na.rm = TRUE), digits = 3),
    .groups = "drop"
  ) |>
  arrange(country)

######################## COMPARATIVE MEDIAN TABLE

affluentMed1 = c("United States of America", "Japan", "China")
affluentMed2 = c("Germany", "United Kingdom", "France")
affluentMed3 = c("Mexico", "Brazil", "Chile")
affluentMed4 = c("South Africa", "Nigeria", "Kenya")
affluentMed5 = c("United Arab Emirates", "Oman", "Saudi Arabia")
interestMed  = c("Rwanda", "Lithuania", "Czechia", "Netherlands")

comparativeMed = bind_rows(
  demSummaryCountryMedian |> filter(country %in% affluentMed1),
  demSummaryCountryMedian |> filter(country %in% affluentMed2),
  demSummaryCountryMedian |> filter(country %in% affluentMed3),
  demSummaryCountryMedian |> filter(country %in% affluentMed4),
  demSummaryCountryMedian |> filter(country %in% affluentMed5),
  demSummaryCountryMedian |> filter(country %in% interestMed)
)

######################## COMPARATIVE IQR TABLE

affluentIQR1 = c("United States of America", "Japan", "China")
affluentIQR2 = c("Germany", "United Kingdom", "France")
affluentIQR3 = c("Mexico", "Brazil", "Chile")
affluentIQR4 = c("South Africa", "Nigeria", "Kenya")
affluentIQR5 = c("United Arab Emirates", "Oman", "Saudi Arabia")
interestIQR  = c("Rwanda", "Lithuania", "Czechia", "Netherlands")

comparativeIQR = bind_rows(
  demSummaryCountryIQR |> filter(country %in% affluentIQR1),
  demSummaryCountryIQR |> filter(country %in% affluentIQR2),
  demSummaryCountryIQR |> filter(country %in% affluentIQR3),
  demSummaryCountryIQR |> filter(country %in% affluentIQR4),
  demSummaryCountryIQR |> filter(country %in% affluentIQR5),
  demSummaryCountryIQR |> filter(country %in% interestIQR)
)

######################## SAVE TABLES AS CSV

write.csv(comparativeMed, "comparative_median.csv", row.names = FALSE)
write.csv(comparativeIQR, "comparative_iqr.csv", row.names = FALSE)
write.csv(demSummaryRegionMean, "summary_region_mean.csv", row.names = FALSE)
write.csv(demSummaryRegionMedian, "summary_region_median.csv", row.names = FALSE)
write.csv(demSummaryRegionIQR, "summary_region_iqr.csv", row.names = FALSE)
