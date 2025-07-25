source("_rinit.R")

## Load Eurostat datasets ------

datasets_eurostat <- c("prc_hicp_manr", "prc_hicp_ctrb")

for (dataset in datasets_eurostat){
  assign(dataset, 
         get_eurostat(dataset, stringsAsFactors = F, cache = F) |>
           rename(date = TIME_PERIOD)
  )
}

## English -------

Sys.setlocale("LC_TIME", "en_US.UTF-8")

figure2_line <- prc_hicp_manr %>%
  filter(coicop %in% c("CP00"),
         geo == "EA") %>%
  filter(date >= as.Date("2019-01-01")) %>%
  bind_rows(prc_hicp_ctrb %>%
              filter(coicop %in% c("SERV", "IGD_NNRG"),
                     geo == "EA") %>%
              filter(date >= as.Date("2019-01-01")) %>%
              group_by(date, unit, geo) %>%
              summarise(values = sum(values)) %>%
              ungroup %>%
              mutate(coicop  = "TOT_X_NRG_FOOD")) %>%
  mutate(Line = factor(coicop, levels = c("CP00", "TOT_X_NRG_FOOD"),
                       labels = c("Inflation (HICP)", "Inflation without energy and food"))) %>%
  select(date, Line, values)


figure2 <- prc_hicp_ctrb %>%
  filter(coicop %in% c("FOOD", "NRG", "IGD_NNRG", "SERV"),
         geo == "EA") %>%
  select(-geo, -unit) %>%
  filter(date >= as.Date("2019-01-01")) %>%
  select(date, coicop, values)

figure2 %>%
  mutate(Coicop_factor = factor(coicop, levels = c("SERV", "IGD_NNRG", "FOOD", "NRG"),
                                labels = c("Services",
                                           "Manufactured goods",
                                           "Food",
                                           "Energy"))) %>%
  ggplot(., aes(x = date, y = values/100)) +
  geom_col(aes(fill = Coicop_factor), alpha = 1) +
  geom_line(data = figure2_line, aes(linetype = Line), size = 1.2) +
  theme_minimal() + xlab("") + ylab("Contributions to inflation in the Euro area") +
  # c("orange", "red", "blue", "darkgreen")
  scale_fill_manual(values = viridis(4)[1:4]) +
  scale_x_date(breaks = "6 months",
               expand = c(.01, 0), date_labels = "%b %Y") +
  scale_y_continuous(breaks = 0.01*seq(-10, 30, 1),
                     labels = percent_format(accuracy = 1)) +
  theme(legend.position = c(0.2, 0.77),
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        legend.key.size = unit(0.4, "cm"))



write.csv(figure2, "csv/figure2.csv")
write.csv(figure2_line, "csv/figure2_line.csv")

ggsave("pdf/figure2.pdf", width = 7, height = 4, device = cairo_pdf)
ggsave("png/figure2.png", width = 7, height = 4)

## French -------

Sys.setlocale("LC_TIME", "fr_FR.UTF-8")

figure2_fr_line <- prc_hicp_manr %>%
  filter(coicop %in% c("CP00"),
         geo == "EA") %>%
  filter(date >= as.Date("2019-01-01")) %>%
  bind_rows(prc_hicp_ctrb %>%
              filter(coicop %in% c("SERV", "IGD_NNRG"),
                     geo == "EA") %>%
              filter(date >= as.Date("2019-01-01")) %>%
              group_by(date, unit, geo) %>%
              summarise(values = sum(values)) %>%
              ungroup %>%
              mutate(coicop  = "TOT_X_NRG_FOOD")) %>%
  mutate(Line = factor(coicop, levels = c("CP00", "TOT_X_NRG_FOOD"),
                          labels = c("Inflation (IPCH)", "Inflation sans énergie et alimentation"))) %>%
  select(date, Line, values)


figure2_fr <- prc_hicp_ctrb %>%
  filter(coicop %in% c("FOOD", "NRG", "IGD_NNRG", "SERV"),
         geo == "EA") %>%
  select(-geo, -unit) %>%
  filter(date >= as.Date("2019-01-01")) %>%
  select(date, coicop, values)

figure2_fr %>%
  mutate(Coicop_factor = factor(coicop, levels = c("SERV", "IGD_NNRG", "FOOD", "NRG"),
                                labels = c("Services",
                                           "Biens manufacturés",
                                           "Alimentation",
                                           "Energie"))) %>%
  ggplot(., aes(x = date, y = values/100)) +
  geom_col(aes(fill = Coicop_factor), alpha = 1) +
  geom_line(data = figure2_fr_line, aes(linetype = Line), size = 1.2) +
  theme_minimal() + xlab("") + ylab("Contributions à l'inflation en zone Euro") +
  # c("orange", "red", "blue", "darkgreen")
  scale_fill_manual(values = viridis(4)[1:4]) +
  scale_x_date(breaks = "6 months",
               expand = c(.01, 0), date_labels = "%b %Y") +
  scale_y_continuous(breaks = 0.01*seq(-10, 30, 1),
                     labels = percent_format(accuracy = 1)) +
  theme(legend.position = c(0.2, 0.77),
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        legend.key.size = unit(0.4, "cm"))

write.csv(figure2_fr, "csv/figure2_fr.csv")
write.csv(figure2_fr_line, "csv/figure2_fr_line.csv")

ggsave("pdf/figure2_fr.pdf", width = 7, height = 4, device = cairo_pdf)
ggsave("png/figure2_fr.png", width = 7, height = 4)


