library("tidyverse")
library("scales")
library("eurostat")

datasets_eurostat <- c("prc_hicp_manr", "prc_hicp_cow")

geos_EA_20 <- c("AT", "BE", "CY", "DE", "EE", "EL", "ES", "FI", "FR", "IE",
                "IT", "LT", "LU", "LV", "MT", "NL", "PT", "SI", "SK", "HR")

for (dataset in datasets_eurostat){
  assign(dataset, 
         get_eurostat(dataset, time_format = "raw", stringsAsFactors = F, cache = F) %>%
           rename(time = TIME_PERIOD)
  )
}

## English ------

coicop <- tibble(coicop = "CP00", Coicop = "Headline inflation") %>%
  add_row(coicop = "TOT_X_NRG_FOOD", Coicop = "Core inflation")

prc_hicp_manr %>%
  filter(coicop %in% c("TOT_X_NRG_FOOD", "CP00"),
         geo %in% geos_EA_20) %>%
  mutate(year = substr(time, 1, 4)) %>%
  left_join(prc_hicp_cow %>%
              filter(statinfo == "COWEA20", geo != "EA20") %>%
              transmute(geo, year = time, country_weights = values, uniform_weights = 1),
            by = c("geo", "year")) %>%
  mutate(date = as.Date(paste0(time, "-01"))) %>%
  select(-time) %>%
  filter(date >= as.Date("2000-01-01")) %>%
  group_by(coicop, date) %>%
  summarise(`Unweighted` = sqrt(Hmisc::wtd.var(values/100, uniform_weights)),
            `Weighted` = sqrt(Hmisc::wtd.var(values/100, country_weights))) %>%
  gather(weighted_yes_no, values, -date, -coicop) %>%
  left_join(coicop, by = "coicop") %>%
  arrange(desc(date)) %>%
  ggplot(.) + geom_line(aes(x = date, y = values, color = Coicop, linetype = weighted_yes_no)) + 
  theme_minimal() + xlab("") + ylab("Standard Deviation of Inflation, EA-20 (%)") +
  scale_x_date(breaks = seq(1960, 2100, 2) %>% paste0("-01-01") %>% as.Date,
               labels = date_format("%Y")) +
  scale_color_manual(values = c("#1E1C1C", "#A81630")) + 
  scale_linetype_manual(values = c("dashed", "solid")) + 
  scale_y_continuous(breaks = 0.01*seq(-20, 20, 1),
                     labels = percent_format(a = 1)) + 
  theme(legend.position = c(0.6, 0.80),
        legend.title = element_blank())+
  guides(color = guide_legend(order = 1),
         linetype = guide_legend(order = 2))


ggsave("figure3.png", width = 1.25*6, height = 1.25*3.375, bg = "white")
ggsave("figure3.pdf", width = 1.25*6, height = 1.25*3.375)


## French ------

coicop <- tibble(coicop = "CP00", Coicop = "Inflation globale") %>%
  add_row(coicop = "TOT_X_NRG_FOOD", Coicop = "Inflation sous-jacente")

prc_hicp_manr %>%
  filter(coicop %in% c("TOT_X_NRG_FOOD", "CP00"),
         geo %in% geos_EA_20) %>%
  mutate(year = substr(time, 1, 4)) %>%
  left_join(prc_hicp_cow %>%
              filter(statinfo == "COWEA20", geo != "EA20") %>%
              transmute(geo, year = time, country_weights = values, uniform_weights = 1),
            by = c("geo", "year")) %>%
  mutate(date = as.Date(paste0(time, "-01"))) %>%
  select(-time) %>%
  filter(date >= as.Date("2000-01-01")) %>%
  group_by(coicop, date) %>%
  summarise(`Non-pondérée` = sqrt(Hmisc::wtd.var(values/100, uniform_weights)),
            `Pondérée` = sqrt(Hmisc::wtd.var(values/100, country_weights))) %>%
  gather(weighted_yes_no, values, -date, -coicop) %>%
  left_join(coicop, by = "coicop") %>%
  arrange(desc(date)) %>%
  ggplot(.) + geom_line(aes(x = date, y = values, color = Coicop, linetype = weighted_yes_no)) + 
  theme_minimal() + xlab("") + ylab("Écart-type de l'inflation dans la zone euro (20 pays), en %") +
  scale_x_date(breaks = seq(1960, 2100, 2) %>% paste0("-01-01") %>% as.Date,
               labels = date_format("%Y")) +
  scale_color_manual(values = c("#1E1C1C", "#A81630")) + 
  scale_linetype_manual(values = c("dashed", "solid")) + 
  scale_y_continuous(breaks = 0.01*seq(-20, 20, 1),
                     labels = percent_format(a = 1)) + 
  theme(legend.position = c(0.6, 0.80),
        legend.title = element_blank())+
  guides(color = guide_legend(order = 1),
         linetype = guide_legend(order = 2))


ggsave("figure3_fr.png", width = 1.25*6, height = 1.25*3.375, bg = "white")
ggsave("figure3_fr.pdf", width = 1.25*6, height = 1.25*3.375)

