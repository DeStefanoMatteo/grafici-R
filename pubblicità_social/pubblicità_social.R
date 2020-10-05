# GRAFICO PUBBLICITA' SOCIAL

library(extrafont)
library(tidyverse)
library(ggplot2)

windowsFonts("Helvetica" = windowsFont("Helvetica"))

setwd("C:/.../pubblicità_social")

data <- read.csv("dati_pubblicità.txt", header=F)

# Modifico nome variabili
data <- data %>% 
  rename(
    social = V1,
    post = V2,
    ad = V3
  )

# Creo variabile "ad_perc"
data <- data %>% 
  mutate(ad_perc=round((ad/(post+ad))*100, 0))

# Creo grafico
social_plot <- data %>%
  arrange(-ad_perc) %>%
  mutate(social=factor(social, levels=social)) %>%
  # mutate(freq_label=c(rep('', 17), paste(tail(round(freq, 1), 3), '%', sep=''))) %>%
  ggplot() +
  geom_bar(aes(x=social, y=rep(100, 7)),
           stat='identity', width = 0.8) +
  geom_bar(aes(x=social, y=ad_perc),
           stat='identity', fill="black",
           width = 0.8) +
  labs(
    title = "", # "Quanta pubblicità ti viene mostrata?"
    x="", y="") +
  coord_fixed(ratio = 0.035) +
  theme_minimal() +
  theme(
    text=element_text(family="Helvetica"),
    plot.title=element_text(face = "bold"),
    plot.subtitle=element_text(margin=margin(0,0,0,0)),
    plot.margin = margin(10,0,0,0),
    axis.text.y = element_text(size=14),
    axis.text.x = element_text(size=14),
    panel.ontop = TRUE,
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color="white", linetype="dashed"),
    panel.grid.minor.y = element_blank(),
    axis.ticks.y = element_blank()
  )

# Salvo grafico in formato PNG
ggsave("pub_social.png", plot=social_plot)
