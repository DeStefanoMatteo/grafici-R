# GRAFICO NUMERO PREFERITO

library(extrafont)
library(tidyverse)
library(ggplot2)

windowsFonts("Helvetica" = windowsFont("Helvetica"))

setwd("C:/.../numero_preferito")

# Carico dati
data <- read.csv("dati_num_favoriti.csv", header=F, dec=",")

# Modifico nomi variabili
data <- data %>% 
  rename(
    num = V1,
    freq = V2
  )

# Creo grafico
num_plot <- data %>%
  arrange(freq) %>%
  mutate(num=factor(num, levels=num)) %>%
  top_n(20, freq) %>%
  mutate(freq_label=c(rep('', 17), paste(tail(round(freq, 1), 3), '%', sep=''))) %>%
  ggplot(aes(x=freq, y=num)) +
  geom_bar(stat='identity', fill="red") +
  #scale_x_continuous(breaks=c(0:99), labels=as.character(c(0:99))) +
  scale_x_continuous(sec.axis = dup_axis(), breaks=c(0, 5, 10, 15),
                     labels=c("", "5%", "10%", "15%"), expand = c(0,0), limits = c(0, 20)) +
  labs(
    title = "Qual è il tuo numero preferito?",
    subtitle = "La risposta di 15'000 persone per scoprire\nil numero più amato al mondo",
    x="", y="") +
  geom_text(aes(label=freq_label, fontface=2),
            hjust=1.1, vjust=0.4,
            color="white", size=3.5, face="bold") +
  coord_fixed(ratio = 1.5) +
  theme_minimal() +
  theme(
    text=element_text(family="Helvetica"),
    plot.title=element_text(face = "bold"),
    plot.subtitle=element_text(margin=margin(0,0,0,0)),
    plot.margin = margin(10,0,0,0),
    axis.text.y = element_text(size=11, face = "bold"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.ticks.y = element_blank()
  )

# Salvo grafico in formato PNG
ggsave("numeri_preferiti.png", plot=num_plot)
