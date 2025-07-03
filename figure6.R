library("tidyverse")
library("scales")
library("data.table")
library("fredr")
source("~/iCloud/passwords/keys.R")

## Load Fred datasets ------

# Add passwords file: fred_key <- "Your FRED Key"
fredr_set_key(fred_key)

money <- map_dfr(c("WALCL", "ECBASSETSW"), fredr)


## English ------

figure6 <- money |>
  mutate(value = value/1000) |>
  mutate(Variable = factor(series_id,
                           levels = c("WALCL", "ECBASSETSW"),
                           labels = c("Federal Reserve", "European Central Bank"))) |>
  select(date, value, Variable) |>
  filter(date >= as.Date("2003-01-01"))


ggplot(data = figure6) + geom_line(aes(x = date, y = value, color = Variable)) +
  xlab("") + ylab("Euros (ECB) or Dollars (Fed)") + theme_minimal() +
  scale_color_manual(values = c("#003399", "#B22234")) +
  scale_x_date(breaks = as.Date(paste0(seq(2003, 2100, 2), "-01-01")),
               labels = date_format("%Y")) +
  theme(legend.position = c(0.4, 0.9),
        legend.title = element_blank()) +
  scale_y_continuous(breaks = 1000*seq(-100, 90, .5),
                     labels = scales::dollar_format(acc = 1, su = " Bn", pre = ""),
                     limits = c(0, 9000))


write_csv(figure6, "figure6.csv")
ggsave("figure6.png", width = 1.25*6, height = 1.25*3.375, bg = "white")
ggsave("figure6.pdf", width = 1.25*6, height = 1.25*3.375)


## Français ------

figure6_fr <- money |>
  mutate(value = value/1000) |>
  mutate(Variable = factor(series_id,
                           levels = c("WALCL", "ECBASSETSW"),
                           labels = c("Réserve Fédérale", "Banque Centrale Européenne"))) |>
  select(date, value, Variable) |>
  filter(date >= as.Date("2003-01-01"))


ggplot(data = figure6_fr) + geom_line(aes(x = date, y = value, color = Variable)) +
  xlab("") + ylab("Euros (ECB) ou Dollars (Fed)") + theme_minimal() +
  scale_color_manual(values = c("#003399", "#B22234")) +
  scale_x_date(breaks = as.Date(paste0(seq(2003, 2100, 2), "-01-01")),
               labels = date_format("%Y")) +
  theme(legend.position = c(0.4, 0.9),
        legend.title = element_blank()) +
  scale_y_continuous(breaks = 1000*seq(-100, 90, .5),
                     labels = scales::dollar_format(acc = 1, su = " Md", pre = ""),
                     limits = c(0, 9000))


write_csv(figure6_fr, "figure6_fr.csv")
ggsave("figure6_fr.png", width = 1.25*6, height = 1.25*3.375, bg = "white")
ggsave("figure6_fr.pdf", width = 1.25*6, height = 1.25*3.375)

