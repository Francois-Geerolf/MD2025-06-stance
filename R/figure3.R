library("tidyverse")
library("scales")
library("eurostat")

## Load Eurostat datasets ------

datasets_eurostat <- c("prc_hicp_manr", "prc_hicp_cow")

for (dataset in datasets_eurostat){
  assign(dataset, 
         get_eurostat(dataset, time_format = "raw", stringsAsFactors = F, cache = F) |>
           rename(time = TIME_PERIOD)
  )
}

## English ------

figure3 <- prc_hicp_manr |>
  filter(coicop %in% c("TOT_X_NRG_FOOD", "CP00")) |>
  mutate(coicop = factor(coicop, 
                         levels = c("TOT_X_NRG_FOOD", "CP00"),
                         labels = c("Core inflation", "Headline inflation"))) |>
  mutate(year = substr(time, 1, 4)) |>
  inner_join(prc_hicp_cow |>
              filter(statinfo == "COWEA20", geo != "EA20") |>
              transmute(geo, year = time, country_weights = values, uniform_weights = 1),
            by = c("geo", "year")) |>
  mutate(date = as.Date(paste0(time, "-01"))) |>
  select(-time) |>
  filter(date >= as.Date("2000-01-01")) |>
  group_by(coicop, date) |>
  summarise(`Unweighted` = sqrt(Hmisc::wtd.var(values/100, uniform_weights)),
            `Weighted` = sqrt(Hmisc::wtd.var(values/100, country_weights))) |>
  gather(weighted_yes_no, values, -date, -coicop) |>
  arrange(desc(date))

ggplot(data = figure3) +
  geom_line(aes(x = date, y = values, color = coicop, linetype = weighted_yes_no)) + 
  theme_minimal() + xlab("") + ylab("Standard Deviation of Inflation, EA-20 (%)") +
  scale_x_date(breaks = as.Date(paste0(seq(1960, 2100, 2), "-01-01")),
               labels = date_format("%Y")) +
  scale_color_manual(values = c("#1E1C1C", "#A81630")) + 
  scale_linetype_manual(values = c("dashed", "solid")) + 
  scale_y_continuous(breaks = 0.01*seq(-20, 20, 1),
                     labels = percent_format(a = 1)) + 
  theme(legend.position = c(0.6, 0.80),
        legend.title = element_blank()) +
  guides(color = guide_legend(order = 1),
         linetype = guide_legend(order = 2))

write_csv(figure3, "csv/figure3.csv")
ggsave("png/figure3.png", width = 1.25*6, height = 1.25*3.375, bg = "white")
ggsave("pdf/figure3.pdf", width = 1.25*6, height = 1.25*3.375)


## French ------

figure3_fr <- prc_hicp_manr |>
  filter(coicop %in% c("TOT_X_NRG_FOOD", "CP00")) |>
  mutate(year = substr(time, 1, 4)) |>
  mutate(coicop = factor(coicop, 
                         levels = c("TOT_X_NRG_FOOD", "CP00"),
                          labels = c("Inflation sous-jacente", "Inflation globale"))) |>
  left_join(prc_hicp_cow |>
              filter(statinfo == "COWEA20", geo != "EA20") |>
              transmute(geo, year = time, country_weights = values, uniform_weights = 1),
            by = c("geo", "year")) |>
  mutate(date = as.Date(paste0(time, "-01"))) |>
  select(-time) |>
  filter(date >= as.Date("2000-01-01")) |>
  group_by(coicop, date) |>
  summarise(`Écart-type non-pondéré` = sqrt(Hmisc::wtd.var(values/100, uniform_weights)),
            `Écart-type pondéré` = sqrt(Hmisc::wtd.var(values/100, country_weights))) |>
  gather(weighted_yes_no, values, -date, -coicop) |>
  arrange(desc(date))


ggplot(data = figure3_fr) + geom_line(aes(x = date, y = values, color = coicop, linetype = weighted_yes_no)) + 
  theme_minimal() + xlab("") + ylab("Écart-type de l'inflation en zone Euro (20 pays), en %") +
  scale_x_date(breaks = as.Date(paste0(seq(1960, 2100, 2), "-01-01")),
               labels = date_format("%Y")) +
  scale_color_manual(values = c("#1E1C1C", "#A81630")) + 
  scale_linetype_manual(values = c("dashed", "solid")) + 
  scale_y_continuous(breaks = 0.01*seq(-20, 20, 1),
                     labels = percent_format(a = 1)) + 
  theme(legend.position = c(0.6, 0.80),
        legend.title = element_blank())+
  guides(color = guide_legend(order = 1),
         linetype = guide_legend(order = 2))


write_csv(figure3_fr, "csv/figure3_fr.csv")
ggsave("png/figure3_fr.png", width = 1.25*6, height = 1.25*3.375, bg = "white")
ggsave("Pdf/figure3_fr.pdf", width = 1.25*6, height = 1.25*3.375)

