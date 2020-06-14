library(dplyr)
library(tidyverse)
library(ggplot2)
library(readxl)
library(sf)
library(corrplot)

library("Hmisc")

powiaty <- st_read( dsn = 'Data/jednostki_administracyjne/PRG_jednostki_administracyjne_v40_SZPRG/Powiaty.shp' )

y_rozwody <- readxl::read_xlsx('Data/rozwody.xlsx', sheet = 2)
x_wynagrodzenie <- readxl::read_xlsx('Data/wynagrodzenie.xlsx', sheet = 2)
x_pracujacy_sekcje <- readxl::read_xlsx('Data/pracujacy_sekcje.xlsx', sheet = 2)
x_absolwenci <- readxl::read_xlsx('Data/absolwenci.xlsx', sheet = 2)

View(y_rozwody)
View(x_pracujacy_sekcje)
View(x_wynagrodzenie)

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

View(final_df)

final_df <- rename(final_df,przecietne_miesieczne_wynagrodzenie = `przeciętne miesięczne wynagrodzenia brutto w relacji do średniej krajowej (Polska=100).y`)

#długo się ładuje 
#ggplot(final_df,aes(fill = ilość_rozwodów, )) + geom_sf()

dane_bez_sf <- fourth_df

View(dane_bez_sf)
dane_bez_sf$`przeciętne miesięczne wynagrodzenia brutto w relacji do średniej krajowej (Polska=100).y` <- NULL

summary(dane_bez_sf)

View(fourth_df)

# z tekstu na liczby

dane_bez_sf$rozwody = as.numeric(as.character(dane_bez_sf$rozwody ))
dane_bez_sf$`przeciętne miesięczne wynagrodzenia brutto w relacji do średniej krajowej (Polska=100).x` = 
  as.numeric(as.character(dane_bez_sf$`przeciętne miesięczne wynagrodzenia brutto w relacji do średniej krajowej (Polska=100).x` ))
dane_bez_sf$`rolnictwo, leśnictwo, łowiectwo i rybactwo` = as.numeric(as.character(dane_bez_sf$`rolnictwo, leśnictwo, łowiectwo i rybactwo`))
dane_bez_sf$`przemysł i budownictwo` = as.numeric(as.character(dane_bez_sf$`przemysł i budownictwo` ))
dane_bez_sf$`handel; naprawa pojazdów samochodowych; transport i gospodarka magazynowa; zakwaterowanie i gastronomia; informacja i komunikacja` = 
  as.numeric(as.character(dane_bez_sf$`handel; naprawa pojazdów samochodowych; transport i gospodarka magazynowa; zakwaterowanie i gastronomia; informacja i komunikacja`))
dane_bez_sf$`działalność finansowa i ubezpieczeniowa; obsługa rynku nieruchomości` = 
  as.numeric(as.character(dane_bez_sf$`działalność finansowa i ubezpieczeniowa; obsługa rynku nieruchomości`)) 
dane_bez_sf$`pozostałe usługi` = as.numeric(as.character(dane_bez_sf$`pozostałe usługi`)) 
dane_bez_sf$`absolwenci ogółem` = as.numeric(as.character(dane_bez_sf$`absolwenci ogółem`))


#zastąpienie braków w danych mediana z kolumny 

#mediany <- lapply(dane_bez_sf, 0 , na.rm = TRUE)
dane_bez_sf[is.na(dane_bez_sf)] <-0

dane_bez_sf$`przeciętne miesięczne wynagrodzenia brutto w relacji do średniej krajowej (Polska=100).y` <- NULL

View(dane_bez_sf)

dane_bez_sf <- dane_bez_sf %>%
  rename(rozwody = `ogółem`,
         wynag =`przeciętne miesięczne wynagrodzenia brutto w relacji do średniej krajowej (Polska=100).x`,
         rolnic = `rolnictwo, leśnictwo, łowiectwo i rybactwo`,
         przem = `przemysł i budownictwo`,
         handel = `handel; naprawa pojazdów samochodowych; transport i gospodarka magazynowa; zakwaterowanie i gastronomia; informacja i komunikacja`,
         finans = `działalność finansowa i ubezpieczeniowa; obsługa rynku nieruchomości`,
         poz_usl = `pozostałe usługi`,
         absol =  `absolwenci ogółem`)

View(dane_bez_sf)

# macierz korelacji 
macierz_korelacji <-cor(dane_bez_sf[sapply(dane_bez_sf,is.numeric)])


corrplot(macierz_korelacji, method = "circle" )
dev.off

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


# ogarniamy rozwody w wojewodztwach
wojewodztwa <- dane_bez_sf %>% select(Nazwa.x,Kod) %>% filter(Nazwa.x == toupper(Nazwa.x))
wojewodztwa <- wojewodztwa[-1,]
View(wojewodztwa)
wojewodztwa$woj <- (substr(wojewodztwa$Kod,1,2))

  dane_bez_sf$woj <- 'text'
  dane_bez_sf$woj <- (substr(dane_bez_sf$Kod,1,2))

 

dane_bez_sf<-merge(dane_bez_sf,wojewodztwa,by = "woj",all.x = TRUE)
View(dane_bez_sf)


dane_bez_sf <- dane_bez_sf %>%
  rename(wojewodztwo = `Nazwa.x.y`,
         )
dane_bez_sf$Kod.y <- NULL



ggplot(dane_bez_sf,aes(wojewodztwo))+geom_bar() + ylab("Rozwody")+
  geom_line(wynag)+
  theme(axis.text.x=element_text(angle =- 90, vjust = 0.5))



# juz moze byc :) dodamy jeszcze podział na grupy zawodowe 

# srednie zarobki w wojewodztwie

srednie_zarobki_wojewodztwa <- dane_bez_sf %>% group_by(wojewodztwo) %>%
  summarise(srednie_zarobki = mean(wynag))

View(srednie_zarobki_wojewodztwa)

ggplot(dane_bez_sf,aes(wojewodztwo))+
  geom_bar() + 
  ylab("Rozwody")+
  theme(axis.text.x=element_text(angle =- 90, vjust = 0.5))

ggplot(srednie_zarobki_wojewodztwa,aes(wojewodztwo,srednie_zarobki,group =1)) +
  geom_point(aes(y = srednie_zarobki_wojewodztwa$srednie_zarobki))+
  geom_line()+
  ylab("Średnie zarobki")+
  theme(axis.text.x=element_text(angle =- 90, vjust = 0.5))





