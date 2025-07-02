library("tidyverse")
library("scales")
library("data.table")

## Load ECB datasets ------

datasets_ECB <- c("FM")

for (dataset in datasets_ECB){
  download.file(paste0("https://sdw-wsrest.ecb.europa.eu/service/data/", dataset, "?format=csvdata"),
                destfile = paste0(dataset, ".csv"),
                quiet = F)
  assign(dataset, fread(paste0(dataset, ".csv")))
  unlink(paste0(dataset, ".csv"))
}

## English ------

dates_ecb <- FM %>%
  filter(PROVIDER_FM_ID == "DFR") %>%
  mutate(date = as.Date(TIME_PERIOD)) %>%
  select(-TIME_PERIOD) %>%
  mutate(OBS_VALUE = OBS_VALUE-lag(OBS_VALUE)) %>%
  filter(OBS_VALUE != 0) %>%
  pull(date)

figure5 <- FM %>%
  filter(PROVIDER_FM_ID %in% c("DFR", "MLFR", "MRR_RT"),
         DATA_TYPE_FM == "LEV",
         FREQ == "D") %>%
  mutate(date = as.Date(TIME_PERIOD)) %>%
  select(-TIME_PERIOD) %>%
  filter(date >= as.Date("2022-01-01")) %>%
  #add_row(date = as.Date("2025-06-11"), PROVIDER_FM_ID = "DFR", OBS_VALUE = 2) %>%
  #add_row(date = as.Date("2025-06-11"), PROVIDER_FM_ID = "MRR_RT", OBS_VALUE = 2.15) %>%
  #add_row(date = as.Date("2025-06-11"), PROVIDER_FM_ID = "MLFR", OBS_VALUE = 2.4) %>%
  #add_row(date = as.Date("2025-06-10"), PROVIDER_FM_ID = "DFR", OBS_VALUE = 2.25) %>%
  #add_row(date = as.Date("2025-06-10"), PROVIDER_FM_ID = "MRR_RT", OBS_VALUE = 2.4) %>%
  #add_row(date = as.Date("2025-06-10"), PROVIDER_FM_ID = "MLFR", OBS_VALUE = 2.65) %>%
  arrange(desc(date)) %>%
  #add_row(date = as.Date("2024-09-17"), PROVIDER_FM_ID = "DFR", OBS_VALUE = 3.5) %>%
  #add_row(date = as.Date("2024-09-17"), PROVIDER_FM_ID = "MRR_RT", OBS_VALUE = 3.65) %>%
  #add_row(date = as.Date("2024-09-17"), PROVIDER_FM_ID = "MLFR", OBS_VALUE = 3.9) %>%
  left_join(PROVIDER_FM_ID,  by = "PROVIDER_FM_ID") %>%
  mutate(Provider_fm_id = factor(PROVIDER_FM_ID,
                                 levels = c("MLFR", "MRR_RT", "DFR"),
                                 labels = c("ECB Marginal lending facility",
                                            "ECB Main refinancing operations - Minimum bid rate/fixed rate",
                                            "ECB Deposit facility")))


ggplot(data = figure5) + geom_line(aes(x = date, y = OBS_VALUE / 100, color = Provider_fm_id)) + 
  theme_minimal() + xlab("") + ylab("Interest Rates (%)") +
  scale_x_date(breaks = c(dates_ecb),
               labels = date_format("%d %B %Y")) +
  scale_y_continuous(breaks = 0.01*seq(-10, 50, 0.5),
                     labels = percent_format(accuracy = .1)) +
  theme(legend.position = c(0.65, 0.2),
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  geom_label_repel(data = . %>% filter(date == max(date)),
                   aes(x = date, y = OBS_VALUE / 100, label = percent(OBS_VALUE / 100, acc = 0.01), color = Provider_fm_id))


write_csv(figure5, "figure5.csv")
ggsave("figure5.png", width = 1.25*6, height = 1.25*3.375, bg = "white")
ggsave("figure5.pdf", width = 1.25*6, height = 1.25*3.375)


## Français ------

figure5_fr <- prc_hicp_midx |>
  filter(coicop %in% c("TOT_X_NRG_FOOD", "CP00"),
         unit == "I15") |>
  mutate(year = substr(time, 1, 4)) |>
  #filter(!(Geo %in% intersect(eurozone_countries, CEEC_countries))) |>
  left_join(prc_hicp_cow |>
              filter(statinfo == "COWEA20", geo != "EA20") |>
              transmute(geo, year = time, country_weights = values, uniform_weights = 1),
            by = c("geo", "year")) |>
  mutate(date = as.Date(paste0(time, "-01"))) |>
  filter(date >= as.Date("2000-01-01")) |>
  group_by(geo, coicop) |>
  arrange(date) |>
  mutate(`Inflation sur 3 ans` = (values/lag(values, 36))^(1/3)-1,
         `Inflation sur 1 an` = values/lag(values, 12)-1) |>
  ungroup() |>
  gather(variable, values, `Inflation sur 3 ans`, `Inflation sur 1 an`) |>
  filter(!is.na(values)) |>
  group_by(coicop, date, variable) |>
  summarise(values = sqrt(Hmisc::wtd.var(values, country_weights))) |>
  mutate(coicop = factor(coicop, 
                       levels = c("TOT_X_NRG_FOOD", "CP00"),
                       labels = c("Inflation sous-jacente", "Inflation globale")))

ggplot(data = figure5_fr) + geom_line(aes(x = date, y = values, color = coicop, linetype = coicop)) + 
  theme_minimal() + xlab("") + ylab("Écart-type de l'inflation annuelle\nPondérée, Zone Euro (20 pays) (%)") +
  scale_x_date(breaks = as.Date(paste0(seq(1960, 2100, 2), "-01-01")),
               labels = date_format("%Y")) +
  scale_color_manual(values = c("#1E1C1C", "#A81630")) + 
  scale_y_continuous(breaks = 0.01*seq(-20, 20, .5),
                     labels = percent_format(a = .1)) + 
  theme(legend.position = "top",
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) + 
  facet_wrap(~ variable)


write_csv(figure5_fr, "figure5_fr.csv")
ggsave("figure5_fr.png", width = 1.25*6, height = 1.25*3.375, bg = "white")
ggsave("figure5_fr.pdf", width = 1.25*6, height = 1.25*3.375)

