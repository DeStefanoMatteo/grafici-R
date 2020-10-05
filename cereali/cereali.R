# GRAFICO CEREALI

library(extrafont)
library(tidyverse)
library(ggplot2)

windowsFonts("Helvetica Neue" = windowsFont("HelveticaNeue"))

setwd("C:/.../cereali")

# Carico dati
data <- read.csv("dati_cereali.txt", header=TRUE)

# Creo grafico
cereali_plot <- data %>%
  arrange(-calorie) %>%
  mutate(cereali=factor(cereali, levels=cereali)) %>%
  ggplot() +
  geom_bar(aes(x=cereali, y=calorie),
           stat='identity', fill="black",
           width = 0.9) +
  labs(
    title = "Quanto sono calorici i cereali che mangi?",
    subtitle = "Calorie in 100g di prodotto",
    x="", y="") +
  scale_y_continuous(breaks = seq(0, 500, by = 100), 
                     labels = paste(seq(0, 500, by = 100), c(rep("", 5), "kcal"), sep = ""))+
  theme_minimal() +
  theme(
    text=element_text(family="Helvetica Neue"),
    plot.title=element_text(face = "bold", margin=margin(10,0,6,0)),
    plot.subtitle=element_text(margin=margin(0,0,20,0)),
    axis.text.y = element_text(size=12),
    # axis.text.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
  )

# Salvo grafico in formato PNG
ggsave("grafico_cereali.png", plot=cereali_plot)
