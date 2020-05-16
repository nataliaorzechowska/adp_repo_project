library(dplyr)
library(tidyverse)
library(ggplot2)
library(readxl)
library(sf)

powiaty <- st_read( dsn = 'Data/jednostki_administracyjne/PRG_jednostki_administracyjne_v40_SZPRG/Powiaty.shp' )

y_rozwody <- readxl::read_xlsx('Data/rozwody.xlsx', sheet = 2)
x_wynagrodzenie <- readxl::read_xlsx('Data/wynagrodzenie.xlsx', sheet = 2)
x_pracujacy_sekcje <- readxl::read_xlsx('Data/pracujacy_sekcje.xlsx', sheet = 2)
x_absolwenci <- readxl::read_xlsx('Data/absolwenci.xlsx', sheet = 2)

View(y_rozwody)
View(x_pracujacy_sekcje)

powiaty$JPT_KOD_JE <- paste0(powiaty$JPT_KOD_JE,'000')

head(powiaty$JPT_KOD_JE)
x_wynagrodzenie$`przeciętne miesięczne wynagrodzenia brutto w relacji do średniej krajowej (Polska=100)`

#łączenie
first_df <- left_join(y_rozwody,select(x_wynagrodzenie,Kod,`przeciętne miesięczne wynagrodzenia brutto w relacji do średniej krajowej (Polska=100)`) , by = "Kod")
View(first_df)
second_df <- left_join(first_df,x_pracujacy_sekcje,by = "Kod")
View(second_df)

# usuwanie kolumny 
second_df$Nazwa.y <- NULL

View(second_df)

second_df <- second_df[!is.na(second_df$Kod),]
second_df <- rename(second_df,ilość_rozwodów = ogółem)

View(second_df)

third_df <- left_join(second_df,x_wynagrodzenie, by = "Kod")

View(third_df)

third_df$Nazwa <- NULL

View(third_df)

fourth_df <- left_join(third_df,x_absolwenci, by = "Kod")

View(fourth_df)

fourth_df$Nazwa <- NULL

View(fourth_df)

final_df <- powiaty %>% left_join(fourth_df, by = c ( 'JPT_KOD_JE' = 'Kod'))

final_df %>% summary()

final_df <- rename(final_df,przecietne_miesieczne_wynagrodzenie = `przeciętne miesięczne wynagrodzenia brutto w relacji do średniej krajowej (Polska=100).y`)

ggplot(final_df,aes(fill = ilość_rozwodów, )) + geom_sf()



