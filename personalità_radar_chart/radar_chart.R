# RADAR CHART PERSONALITA'

library(fmsb)
library(tidyr)

# Crea il dataframe che contiene i dati
df <- data.frame(
  "trait" = c("Openness\nto experience", "Agreeableness", 
              "Conscientiousness", "Negative\nemotionality",
              "Extraversion"), 
  "value" = c(0.94, 0.78, 0.94, 0.05, 0.62))

# Pivot dei dati
df <- spread(df, trait, value)

# Agginge massimo (1) e minimo (0) per ogni asse
df <- rbind(rep(1, 5) , rep(0, 5) , df)

# Radar chart 
radarchart(df, plwd = 3, 
           # personalizza la griglia
           cglcol="grey", cglty=1, axislabcol="grey", 
           caxislabels=seq(0,100,25), cglwd=1.5)
