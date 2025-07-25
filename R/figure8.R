library("tidyr")
library("scales")
library("data.table")

## Load ECB datasets ------

YC_PUB <- tibble(KEY = c("B.U2.EUR.4F.G_N_A.SV_C_YM.SR_1Y",
                         "B.U2.EUR.4F.G_N_A.SV_C_YM.SR_10Y")) |>
  mutate(data = map(KEY, ~ paste0("https://sdw-wsrest.ecb.europa.eu/service/data/YC/", .) |>
                      rsdmx::readSDMX() |>
                      as_tibble())) |>
  unnest(cols = c(data))

## English ------

figure8 <- YC_PUB |>
  filter(DATA_TYPE_FM %in% c("SR_10Y", "SR_1Y")) |>
  mutate(date = as.Date(obsTime)) |>
  arrange(desc(date)) |>
  mutate(Data_type_fm = factor(DATA_TYPE_FM,
                                 levels = c("SR_10Y", "SR_1Y"),
                                 labels = c("Yield curve spot rate, 10-year maturity",
                                            "Yield curve spot rate, 1-year maturity"))) |>
  select(date, OBS_VALUE = obsValue, Data_type_fm)


ggplot(data = figure8)  + geom_line(aes(x = date, y = OBS_VALUE/100, color = Data_type_fm)) +
  theme_minimal() + xlab("") + ylab("Interest Rates (%)") +
  scale_x_date(breaks = as.Date(paste0(seq(1960, 2100, 2), "-01-01")),
               labels = date_format("%Y")) +
  scale_y_continuous(breaks = 0.01*seq(-10, 50, 0.5),
                     labels = percent_format(accuracy = .1)) +
  theme(legend.position = c(0.6, 0.9),
        legend.title = element_blank())


write_csv(figure8, "csv/figure8.csv")
ggsave("png/figure8.png", width = 1.25*6, height = 1.25*3.375, bg = "white")
ggsave("pdf/figure8.pdf", width = 1.25*6, height = 1.25*3.375)


## Français ------

figure8_fr <- YC_PUB |>
  filter(DATA_TYPE_FM %in% c("SR_10Y", "SR_1Y")) |>
  mutate(date = as.Date(obsTime)) |>
  arrange(desc(date)) |>
  mutate(Data_type_fm = factor(DATA_TYPE_FM,
                               levels = c("SR_10Y", "SR_1Y"),
                               labels = c("Taux spot, échéance à 10 ans",
                                          "Taux spot, échéance à 1 an"))) |>
  select(date, OBS_VALUE = obsValue, Data_type_fm)


ggplot(data = figure8_fr)  + geom_line(aes(x = date, y = OBS_VALUE/100, color = Data_type_fm)) +
  theme_minimal() + xlab("") + ylab("Taux d'intérêt (%)") +
  scale_x_date(breaks = as.Date(paste0(seq(1960, 2100, 2), "-01-01")),
               labels = date_format("%Y")) +
  scale_y_continuous(breaks = 0.01*seq(-10, 50, 0.5),
                     labels = percent_format(accuracy = .1)) +
  theme(legend.position = c(0.6, 0.9),
        legend.title = element_blank())


write_csv(figure8_fr, "csv/figure8_fr.csv")
ggsave("png/figure8_fr.png", width = 1.25*6, height = 1.25*3.375, bg = "white")
ggsave("pdf/figure8_fr.pdf", width = 1.25*6, height = 1.25*3.375)

