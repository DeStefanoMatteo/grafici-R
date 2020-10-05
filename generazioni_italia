# GRAFICO GENERAZIONI IN ITALIA

library(extrafont)
library(tidyverse)
library(ggplot2)

windowsFonts("Helvetica" = windowsFont("Helvetica"))

setwd("C:/.../generazioni_italia")

data <- read.csv("dati_pop_ita.csv", header=T)

data <- data %>%
  select("Territorio", "Sesso", "ETA1", "Value")

# Filtro i dati
data <- data %>%
  filter(Territorio=="Italia" & ETA1!="TOTAL")
nrow(data)

data <- data %>%
  select("Sesso", "ETA1", "Value")

# Modifico il nome delle variabili
data <- data %>% 
  rename(
    sesso = Sesso,
    eta = ETA1,
    cont = Value
  )

data$eta <- readr::parse_number(data$eta)
data$anno <- 2020 - data$eta

# Creo variabile "gen" che indica la generazione di appartenenza
# a seconda dell'età
df_stats <- filter(data, sesso == "totale")
df_stats <- df_stats %>% mutate(gen = case_when(
  eta > (2020 - 1928) ~ " ",
  eta > (2020 - 1946)  & eta <= (2020 - 1928)  ~ "Generazione Silenziosa",
  eta > (2020 - 1965)  & eta <= (2020 - 1946)  ~ "Boomer",
  eta > (2020 - 1981)  & eta <= (2020 - 1965) ~ "Generazione X",
  eta > (2020 - 1997)  & eta <= (2020 - 1981) ~ "Millennials",
  eta > (2020 - 2013)  & eta <= (2020 - 1997) ~ "Generazione Z",
  eta <= (2020 - 2013) ~ "Generazione Alfa",
  )
)

# Calcolo percentuali delle generazioni sulla popolazione totale
df_stats <- df_stats %>% group_by(gen) %>% summarise(tot = sum(cont))
tot_pop <- sum(df_stats$tot)
df_stats$perc <- df_stats$tot / tot_pop

# Creo grafico

gen_year <- c(1928, 1946, 1965, 1981, 1997, 2013)
gen <- data.frame("year" = gen_year, "text_height" = rep(1150, 6))
x_ticks <- seq.int(1920, 2020, 10)

pop_plot <- data %>%
  filter(sesso != "totale") %>%
  ggplot(aes(x=anno, y=cont/1000, fill=sesso)) +
  geom_bar(position="stack", stat="identity", 
           width=0.85, show.legend=FALSE) +
  labs(
    title = "", # "Popolazione per generazione"
    # subtitle="con numero di persone vive"
    x="ANNO DI NASCITA", y="MIGLIAIA\nDI PERSONE") +
  coord_fixed(ratio = 0.05) +
  geom_vline(xintercept = gen$year-0.5, linetype="dashed", size=0.7) +
  scale_x_continuous(breaks=x_ticks, labels=as.character(x_ticks), 
                     limits=c(1917, 2021), expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0), limits=c(0, 1350)) +
  scale_fill_manual(values=c("#FFC0CB", "#7ABFFF")) +
  theme_minimal() +
  theme(
    #text=element_text(family="Helvetica"),
    plot.title=element_text(face = "bold"),
    axis.title = element_text(size=10),
    axis.title.x = element_text(hjust=0),
    axis.title.y = element_text(vjust=1, angle=0),
    axis.text = element_text(size=10),
    axis.ticks.x = element_line(color="black"),
    axis.ticks.length=unit(.3, "cm"),
    axis.ticks.y = element_line(color="black"),
    axis.line.x.bottom = element_line(color="black"),
    panel.ontop = TRUE,
    panel.grid = element_blank(),
    plot.background = element_rect(color="#F7FAFF", fill="#F7FAFF"),
    legend.position="none"
    
  )

# Salvo grafico in formato PNG
ggsave("gen_ita.png", plot=pop_plot)
