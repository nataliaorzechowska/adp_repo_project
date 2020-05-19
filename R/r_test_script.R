library(dplyr)
library(tidyverse)
library(ggplot2)
library(readxl)
library(sf)
library(corrplot)

install.packages("Hmisc")

library("Hmisc")

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
second_df <- rename(second_df,ilosc_rozwodow = ogolem)

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

#długo się ładuje 
#ggplot(final_df,aes(fill = ilość_rozwodów, )) + geom_sf()

dane_bez_sf <- fourth_df

summary(dane_bez_sf)

# z tekstu na liczby

dane_bez_sf$ilosc_rozwodow = as.numeric(as.character(dane_bez_sf$ilosc_rozwodow ))
dane_bez_sf$`przeciętne miesięczne wynagrodzenia brutto w relacji do średniej krajowej (Polska=100).x` = 
  as.numeric(as.character(dane_bez_sf$`przeciętne miesięczne wynagrodzenia brutto w relacji do średniej krajowej (Polska=100).x` ))
dane_bez_sf$`rolnictwo, leśnictwo, łowiectwo i rybactwo` = as.numeric(as.character(dane_bez_sf$`rolnictwo, leśnictwo, łowiectwo i rybactwo`))
dane_bez_sf$`przemysł i budownictwo` = as.numeric(as.character(dane_bez_sf$`przemysł i budownictwo` ))
dane_bez_sf$`handel; naprawa pojazdów samochodowych; transport i gospodarka magazynowa; zakwaterowanie i gastronomia; informacja i komunikacja` = 
  as.numeric(as.character(dane_bez_sf$`handel; naprawa pojazdów samochodowych; transport i gospodarka magazynowa; zakwaterowanie i gastronomia; informacja i komunikacja`))
dane_bez_sf$`działalność finansowa i ubezpieczeniowa; obsługa rynku nieruchomości` = 
  as.numeric(as.character(dane_bez_sf$`działalność finansowa i ubezpieczeniowa; obsługa rynku nieruchomości`)) 
dane_bez_sf$`pozostałe usługi` = as.numeric(as.character(dane_bez_sf$`pozostałe usługi`)) 
dane_bez_sf$`przeciętne miesięczne wynagrodzenia brutto w relacji do średniej krajowej (Polska=100).y` = 
  as.numeric(as.character(dane_bez_sf$`przeciętne miesięczne wynagrodzenia brutto w relacji do średniej krajowej (Polska=100).y`)) 
dane_bez_sf$`absolwenci ogółem` = as.numeric(as.character(dane_bez_sf$`absolwenci ogółem`))


#zastąpienie braków w danych mediana z kolumny 

#mediany <- lapply(dane_bez_sf, 0 , na.rm = TRUE)
dane_bez_sf[is.na(dane_bez_sf)] <-0

dane_bez_sf <- dane_bez_sf %>%
  rename(rozw = ilość_rozwodów,
         wynag =`przeciętne miesięczne wynagrodzenia brutto w relacji do średniej krajowej (Polska=100).x`,
         rolnic = `rolnictwo, leśnictwo, łowiectwo i rybactwo`,
         przem = `przemysł i budownictwo`,
         handel = `handel; naprawa pojazdów samochodowych; transport i gospodarka magazynowa; zakwaterowanie i gastronomia; informacja i komunikacja`,
         finans = `działalność finansowa i ubezpieczeniowa; obsługa rynku nieruchomości`,
         poz_usl = `pozostałe usługi`,
         absol =  `absolwenci ogółem`)

dane_bez_sf$`przeciętne miesięczne wynagrodzenia brutto w relacji do średniej krajowej (Polska=100).y` <- NULL

View(dane_bez_sf)

# macierz korelacji 
macierz_korelacji <-cor(dane_bez_sf[sapply(dane_bez_sf,is.numeric)])

corrplot(macierz_korelacji,method="color")

# duza dodatnia korelacja miedzy liczba rozwodow a iloscia absolwentów
# mała dodatnia korelacja miedzy liczbą rozwodów a wynagrodzeniem 
# mała dodatnia korelacja miedzy liczba absolwentow a wynagrodzeniem
# mała dodatanie korelacja miedzy praca w finansach a wynagrodzeniem


dane_z_sf <-  powiaty %>% left_join(dane_bez_sf, by = c ( 'JPT_KOD_JE' = 'Kod'))

summary(dane_z_sf)

# rozwody w powiatach 
ggplot(dane_z_sf,aes(fill = rozw )) + geom_sf()+
  scale_fill_gradient( low = 'white' ,  high = 'red')

#wynagrodzenie w powiatach 
ggplot(dane_z_sf,aes(fill = wynag )) + geom_sf()+
  scale_fill_gradient( low = 'white' ,  high = 'dark green')

#ilość absolwentów
ggplot(dane_z_sf,aes(fill = absol )) + geom_sf()+
  scale_fill_gradient( low = 'white' ,  high = 'dark blue')

wojewodztwa <- dane_bez_sf %>% select(Nazwa.x,Kod) %>% filter(Nazwa.x == toupper(Nazwa.x))
wojewodztwa <- wojewodztwa[-1,]
View(wojewodztwa)

dane_bez_sf$woj <- 'text'


# bedzie dodanie kolumny z wojewodztwem zeby zrobic dla kazdego wojewodztwa osobne histogramy 
for ( i in  1:nrow(wojewodztwa)){
  nazwa_woj <- wojewodztwa$Nazwa.x[i]
  kod_dwie_pierwsze <- substr(wojewodztwa$Kod[i],1,2)
  for (j in 1:nrow(dane_bez_sf)){
    if ( substr(dane_bez_sf$Kod[j],1,2) == kod_dwie_pierwsze){
      dane_bez_sf$woj <- nazwa_woj
    }
  }
}

View(dane_bez_sf)







