# GRAFICO CALENDARIO NASCITE

library(extrafont)
library(tidyverse)
library(ggplot2)
library(pdftools)

windowsFonts("Helvetica Neue" = windowsFont("HelveticaNeue"))
windowsFonts("Century Schoolbook" = windowsFont("Century Schoolbook"))

setwd("C:/.../calendario_nascite")

# Creo data.frame vuoto in cui salvare i dati

data <- data.frame()

# Estraggo i dati dai file pdf annuali.
# I file del 2001, 2002 e 2003 hanno valori aggiuntivi e vengono elaborati diversamente

for (anno_doc in 2001:2018) {
  file_name <- paste0("dati/dati_nascite_", anno_doc, ".pdf")
  # Scarico dati delle pagine
  value_list_1 <- unlist(str_split(unlist(str_split(pdf_text(file_name)[[1]], "\r\n"))[8:38], " +"))
  value_list_1 <- value_list_1[lapply(value_list_1, nchar) > 0]
  
  # Controllo se l'anno è bisestile
  if (anno_doc %% 4 == 0) {
    # Inserisco valori vuoti mancanti
    for (pos in c(380, 380, 393, 393, 397, 397, 401, 401)) {
      value_list_1 <- append(value_list_1, "", pos)
    }
  } else if (anno_doc == 2017) {
    value_list_1 <- value_list_1[-c(368, 369)]
    # Inserisco valori vuoti mancanti
    for (pos in c(367, 367, 380, 380, 393, 393, 397, 397, 401, 401)) {
      value_list_1 <- append(value_list_1, "", pos)
    }
  } else {
    # Inserisco valori vuoti mancanti
    for (pos in c(367, 367, 380, 380, 393, 393, 397, 397, 401, 401)) {
      value_list_1 <- append(value_list_1, "", pos)
    }
  }
  
  value_list_2 <- unlist(str_split(unlist(str_split(pdf_text(file_name)[[2]], "\r\n"))[12:42], " +"))
  value_list_2 <- value_list_2[lapply(value_list_2, nchar) > 0]
  
  if (anno_doc == 2001) {
    value_list_2 <- value_list_2[-c(426, 429)]
    # Inserisco valori vuoti mancanti
    for (pos in c(425, 425, 429, 429)) {
      value_list_2 <- append(value_list_2, "", pos)
    }
  } else {
    # Inserisco valori vuoti mancanti
    for (pos in c(425, 425, 429, 429)) {
      value_list_2 <- append(value_list_2, "", pos)
    }
  }
  
  
  # Individuo giorno e mese di ogni valore della pagina 1/2 del pdf e lo inserisco nel data.frame
  for (i in (1:length(value_list_1))) {
    giorno <- ((i - 1) %/% 13) + 1
    resto <- i %% 13
    if (resto == 3) {
      mese <- "Gennaio"
    } else if (resto == 5) {
      mese <- "Febbraio"
    } else if (resto == 7) {
      mese <- "Marzo"
    } else if (resto == 9) {
      mese <- "Aprile"
    } else if (resto == 11) {
      mese <- "Maggio"
    } else if (resto == 0) {
      mese <- "Giugno"
    }
    if (resto == 0 | (resto %% 2 == 1 & resto != 1)) {
      data <- rbind(data, c(value_list_1[i], giorno, mese, anno_doc))
    }
  }
  
  # Individuo giorno e mese di ogni valore della pagina 2/2 del pdf e lo inserisco nel data.frame
  for (i in (1:length(value_list_2))) {
    giorno <- ((i - 1) %/% 14) + 1
    resto <- i %% 14
    if (resto == 3) {
      mese <- "Luglio"
    } else if (resto == 5) {
      mese <- "Agosto"
    } else if (resto == 7) {
      mese <- "Settembre"
    } else if (resto == 9) {
      mese <- "Ottobre"
    } else if (resto == 11) {
      mese <- "Novembre"
    } else if (resto == 13) {
      mese <- "Dicembre"
    }
    if (resto %% 2 == 1 & resto != 1) {
      data <- rbind(data, c(value_list_2[i], giorno, mese, anno_doc))
    }
  }
}

# Rinonimo le variabili

colnames(data) <- c("valore", "giorno", "mese", "anno")

# Rimuovo dati vuoti

data <- data[!(data$valore == ""), ]

# Modifico la tipologia delle variabili

data$valore <- as.numeric(sub("\\.", "", data$valore))
data$giorno <- as.numeric(data$giorno)

mean_valore <- mean(data$valore)
 
# Creo grafico

nascite_plot <- 
  data %>%
  mutate(mese = factor(mese, levels = unique(mese)),
         giorno = factor(giorno, levels = unique(giorno))) %>%
  group_by(giorno, mese) %>%
  summarise(valore = mean(valore)) %>%
  mutate(day_level = round(valore / mean_valore, 2)) %>%
  mutate(text_color = day_level < 0.8) %>%
  ggplot(aes(x = giorno, y = reorder(mese, desc(mese)),
             fill = day_level, label = day_level)) +
  geom_tile() +
  geom_text(aes(color = text_color), size = 2.4) +
  labs(
    title = "I COMPLEANNI PIÙ COMUNI IN ITALIA",
    x="Giorno", y="Mese") +
  scale_x_discrete(position = "top") +
  scale_fill_gradientn(colors = c("#004E7A", "#007EC7", "#009FFA", "#4BBAFB", "#F2F2E6",
                                  "#FBAF4B", "#FA8F00", "#C77100", "#7A4500"),
                       limits = c(0.65, 1.35)) +
  scale_color_manual(values = c('TRUE' = 'white', 'FALSE' = 'black'), guide = "none") +
  coord_fixed(ratio = 0.9) +
  theme_minimal() +
  theme(
    text=element_text(family = "Helvetica Neue"), 
    plot.title=element_text(family = "Century Schoolbook", face = "bold"), # , margin=margin(10,0,6,0)
    # plot.subtitle=element_text(margin=margin(0,0,20,0)),
    # axis.text.y = element_text(size=12),
    axis.title = element_blank(),
    axis.text = element_text(face = "bold"),
    panel.grid = element_blank(),
    legend.position = "none"
  )

# Salvo il grafico come file PNG
ggsave("nascite_raw.png", plot=nascite_plot)
