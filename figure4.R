library("tidyverse")
library("scales")
library("eurostat")

## Load Eurostat datasets ------

datasets_eurostat <- c("prc_hicp_midx", "prc_hicp_cow")

for (dataset in datasets_eurostat){
  assign(dataset, 
         get_eurostat(dataset, time_format = "raw", stringsAsFactors = F, cache = F) |>
           rename(time = TIME_PERIOD)
  )
}

## English ------

figure4 <- prc_hicp_midx |>
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
  mutate(`3-year inflation` = (values/lag(values, 36))^(1/3)-1,
         `1-year inflation` = values/lag(values, 12)-1) |>
  ungroup() |>
  gather(variable, values, `3-year inflation`, `1-year inflation`) |>
  filter(!is.na(values)) |>
  group_by(coicop, date, variable) |>
  summarise(values = sqrt(Hmisc::wtd.var(values, country_weights))) |>
  mutate(coicop = factor(coicop, 
                         levels = c("TOT_X_NRG_FOOD", "CP00"),
                         labels = c("Core inflation", "Headline inflation")))

ggplot(data = figure4) + geom_line(aes(x = date, y = values, color = coicop, linetype = coicop)) + 
  theme_minimal() + xlab("") + ylab("Standard Deviation of Annual Inflation\nWeighted, EA-20 (%)") +
  scale_x_date(breaks = as.Date(paste0(seq(1960, 2100, 2), "-01-01")),
               labels = date_format("%Y")) +
  scale_color_manual(values = c("#1E1C1C", "#A81630")) + 
  scale_y_continuous(breaks = 0.01*seq(-20, 20, .5),
                     labels = percent_format(a = .1)) + 
  theme(legend.position = "top",
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) + 
  facet_wrap(~ variable)

write_csv(figure4, "figure4.csv")
ggsave("figure4.png", width = 1.25*6, height = 1.25*3.375, bg = "white")
ggsave("figure4.pdf", width = 1.25*6, height = 1.25*3.375)


## Français ------

figure4_fr <- prc_hicp_midx |>
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

ggplot(data = figure4_fr) + geom_line(aes(x = date, y = values, color = coicop, linetype = coicop)) + 
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


write_csv(figure4_fr, "figure4_fr.csv")
ggsave("figure4_fr.png", width = 1.25*6, height = 1.25*3.375, bg = "white")
ggsave("figure4_fr.pdf", width = 1.25*6, height = 1.25*3.375)

