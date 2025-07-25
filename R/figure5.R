
source("_rinit.R")

## Load ECB datasets ------

FM <- tibble(KEY = c("D.U2.EUR.4F.KR.DFR.LEV",
                     "D.U2.EUR.4F.KR.MLFR.LEV",
                     "D.U2.EUR.4F.KR.MRR_RT.LEV")) |>
  mutate(data = map(KEY, ~ paste0("https://sdw-wsrest.ecb.europa.eu/service/data/FM/", .) |>
                      rsdmx::readSDMX() |>
                      as_tibble())) |>
  unnest(cols = c(data)) %>%
  rename(TIME_PERIOD = obsTime, OBS_VALUE = obsValue)

## English ------

Sys.setlocale("LC_TIME", "en_US.UTF-8")

dates_ecb <- FM |>
  filter(PROVIDER_FM_ID == "DFR") |>
  mutate(date = as.Date(TIME_PERIOD)) |>
  select(-TIME_PERIOD) |>
  mutate(OBS_VALUE = OBS_VALUE-lag(OBS_VALUE)) |>
  filter(OBS_VALUE != 0) |>
  pull(date)

figure5 <- FM |>
  filter(PROVIDER_FM_ID %in% c("DFR", "MLFR", "MRR_RT"),
         DATA_TYPE_FM == "LEV",
         FREQ == "D") |>
  mutate(date = as.Date(TIME_PERIOD)) |>
  select(-TIME_PERIOD) |>
  filter(date >= as.Date("2022-01-01")) |>
  arrange(desc(date)) |>
  mutate(PROVIDER_FM_ID = factor(PROVIDER_FM_ID,
                                 levels = c("MLFR", "MRR_RT", "DFR"),
                                 labels = c("ECB Marginal lending facility",
                                            "ECB Main refinancing operations",
                                            "ECB Deposit facility"))) |>
  select(date, OBS_VALUE, PROVIDER_FM_ID)


ggplot(data = figure5) + geom_line(aes(x = date, y = OBS_VALUE / 100, color = PROVIDER_FM_ID)) + 
  theme_minimal() + xlab("") + ylab("Interest Rates (%)") +
  scale_x_date(breaks = c(dates_ecb),
               labels = date_format("%d %B %Y")) +
  scale_y_continuous(breaks = 0.01*seq(-10, 50, 0.5),
                     labels = percent_format(accuracy = .1)) +
  theme(legend.position = c(0.65, 0.2),
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  geom_label_repel(data = figure5 |> filter(date == max(date)),
                   aes(x = date, y = OBS_VALUE / 100,label = percent(OBS_VALUE / 100, acc = 0.01), color = PROVIDER_FM_ID))


write_csv(figure5, "csv/figure5.csv")
ggsave("png/figure5.png", width = 1.25*6, height = 1.25*3.375, bg = "white")
ggsave("pdf/figure5.pdf", width = 1.25*6, height = 1.25*3.375)


## Français ------

Sys.setlocale("LC_TIME", "fr_FR.UTF-8")

figure5_fr <- FM |>
  filter(PROVIDER_FM_ID %in% c("DFR", "MLFR", "MRR_RT"),
         DATA_TYPE_FM == "LEV",
         FREQ == "D") |>
  mutate(date = as.Date(TIME_PERIOD)) |>
  select(-TIME_PERIOD) |>
  filter(date >= as.Date("2022-01-01")) |>
  arrange(desc(date)) |>
  mutate(PROVIDER_FM_ID = factor(PROVIDER_FM_ID,
                                 levels = c("MLFR", "MRR_RT", "DFR"),
                                 labels = c("Facilité de prêt marginal de la BCE",
                                            "Opérations principales de refinancement de la BCE",
                                            "Facilité de dépôt de la BCE"))) |>
  select(date, OBS_VALUE, PROVIDER_FM_ID)

ggplot(data = figure5_fr) + geom_line(aes(x = date, y = OBS_VALUE / 100, color = PROVIDER_FM_ID)) + 
  theme_minimal() + xlab("") + ylab("Taux d'intérêt (%)") +
  scale_x_date(breaks = c(dates_ecb),
               labels = date_format("%d %B %Y")) +
  scale_y_continuous(breaks = 0.01*seq(-10, 50, 0.5),
                     labels = percent_format(accuracy = .1)) +
  theme(legend.position = c(0.65, 0.2),
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  geom_label_repel(data = figure5_fr |> filter(date == max(date)),
                   aes(x = date, y = OBS_VALUE / 100,label = percent(OBS_VALUE / 100, acc = 0.01), color = PROVIDER_FM_ID))


write_csv(figure5_fr, "csv/figure5_fr.csv")
ggsave("png/figure5_fr.png", width = 1.25*6, height = 1.25*3.375, bg = "white")
ggsave("pdf/figure5_fr.pdf", width = 1.25*6, height = 1.25*3.375)

