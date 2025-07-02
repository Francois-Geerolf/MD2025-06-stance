library("tidyverse")
library("scales")
library("data.table")

## Load ECB datasets ------

MIR <- tibble(KEY = c("M.U2.B.A2I.AM.R.A.2240.EUR.N", "M.U2.B.A2C.AM.R.A.2250.EUR.N")) |>
  mutate(data = map(KEY, ~ paste0("https://sdw-wsrest.ecb.europa.eu/service/data/MIR/", .) |>
                      rsdmx::readSDMX() |>
                      as_tibble())) |>
  unnest(cols = c(data))

## English ------

figure7 <- MIR |>
  rename(TIME_PERIOD = obsTime, OBS_VALUE = obsValue) |>
  mutate(date = as.Date(paste0(TIME_PERIOD, "-01"))) |>
  mutate(TITLE = factor(KEY,
                        levels = c("M.U2.B.A2I.AM.R.A.2240.EUR.N",
                                        "M.U2.B.A2C.AM.R.A.2250.EUR.N"),
                        labels = c("Cost of borrowing for corporations",
                                   "Cost of borrowing for households for house purchase"))) |>
  select(date, OBS_VALUE, TITLE)

ggplot(data = figure7) + 
  geom_line(aes(x = date, y = OBS_VALUE / 100, color = TITLE)) + 
  theme_minimal() + xlab("") + ylab("Interest Rates (%)") +
  scale_x_date(breaks = as.Date(paste0(seq(1960, 2100, 2), "-01-01")),
               labels = date_format("%Y")) +
  scale_y_continuous(breaks = 0.01*seq(-10, 50, 1),
                     labels = percent_format(accuracy = 1)) +
  theme(legend.position = c(0.65, 0.9),
        legend.title = element_blank())


write_csv(figure7, "figure7.csv")
ggsave("figure7.png", width = 1.25*6, height = 1.25*3.375, bg = "white")
ggsave("figure7.pdf", width = 1.25*6, height = 1.25*3.375)


## Français ------

figure7_fr <-  MIR |>
  rename(TIME_PERIOD = obsTime, OBS_VALUE = obsValue) |>
  mutate(date = as.Date(paste0(TIME_PERIOD, "-01"))) |>
  mutate(TITLE = factor(KEY,
                        levels = c("M.U2.B.A2I.AM.R.A.2240.EUR.N",
                                   "M.U2.B.A2C.AM.R.A.2250.EUR.N"),
                        labels = c("Coût de l'emprunt pour les entreprises",
                                   "Coût de l'emprunt pour les ménages pour l'achat de leur logement"))) |>
  select(date, OBS_VALUE, TITLE)

ggplot(data = figure7_fr) + 
  geom_line(aes(x = date, y = OBS_VALUE / 100, color = TITLE)) + 
  theme_minimal() + xlab("") + ylab("Taux d'intérêt (%)") +
  scale_x_date(breaks = as.Date(paste0(seq(1960, 2100, 2), "-01-01")),
               labels = date_format("%Y")) +
  scale_y_continuous(breaks = 0.01*seq(-10, 50, 1),
                     labels = percent_format(accuracy = 1)) +
  theme(legend.position = c(0.65, 0.9),
        legend.title = element_blank())


write_csv(figure7_fr, "figure7_fr.csv")
ggsave("figure7_fr.png", width = 1.25*6, height = 1.25*3.375, bg = "white")
ggsave("figure7_fr.pdf", width = 1.25*6, height = 1.25*3.375)

