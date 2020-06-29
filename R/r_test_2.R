library(dplyr)
library(tidyverse)
library(ggplot2)
library(readxl)
library(sf)
library(sqldf)
library(corrplot)

library("Hmisc")

powiaty <- st_read( dsn = 'Data/jednostki_administracyjne/PRG_jednostki_administracyjne_v40_SZPRG/Powiaty.shp' )

y_rozwody <- readxl::read_xlsx('Data/rozwody.xlsx', sheet = 2)
x_ludnosc <- readxl::read_xlsx('Data/ludnosc.xlsx', sheet = 2 )
x_wynagrodzenie <- readxl::read_xlsx('Data/wynagrodzenie.xlsx', sheet = 2)


powiaty$JPT_KOD_JE <- paste0(powiaty$JPT_KOD_JE,'000')

#head(powiaty$JPT_KOD_JE)
#x_wynagrodzenie$`przeciętne miesięczne wynagrodzenia brutto w relacji do średniej krajowej (Polska=100)`

#łączenie
first_df <- left_join(y_rozwody,select(x_wynagrodzenie,Kod,`przeciętne miesięczne wynagrodzenia brutto w relacji do średniej krajowej (Polska=100)`) , by = "Kod")
first_df <- rename(first_df,liczba_rozwodow = ogółem)
first_df <- left_join(first_df,select(x_ludnosc,Kod,`ogółem`) , by = "Kod")
first_df <- rename(first_df,liczba_ludnosci = ogółem)
first_df <- first_df[!is.na(first_df$Kod),]



# na numeryczne 
first_df$liczba_rozwodow = as.numeric(as.character(first_df$liczba_rozwodow ))
first_df$liczba_ludnosci = as.numeric(as.character(first_df$liczba_ludnosci ))
first_df$`przeciętne miesięczne wynagrodzenia brutto w relacji do średniej krajowej (Polska=100)` = as.numeric(as.character(first_df$`przeciętne miesięczne wynagrodzenia brutto w relacji do średniej krajowej (Polska=100)` ))

# scalowanie

first_df <- first_df %>% mutate(liczba_rozwodow = (liczba_rozwodow/liczba_ludnosci))
View(first_df)

# zmiana nazwy zeby mialo sens
first_df <-rename(first_df,liczba_rozwodow_w_stosunku_do_liczby_ludnosci = liczba_rozwodow)

dane_z_sf <-  powiaty %>% left_join(first_df, by = c ( 'JPT_KOD_JE' = 'Kod'))
#przyklad sprawdzam czy działa
ggplot(dane_z_sf,aes(fill = liczba_rozwodow_w_stosunku_do_liczby_ludnosci )) + geom_sf()+
  scale_fill_gradient( low = 'white' ,  high = 'red')

# ogarniamy rozwody w wojewodztwach
wojewodztwa <- first_df %>% select(Nazwa,Kod) %>% filter(Nazwa == toupper(Nazwa))
wojewodztwa <- wojewodztwa[-1,]
View(wojewodztwa)
wojewodztwa$woj <- (substr(wojewodztwa$Kod,1,2))

first_dff$woj <- 'text'
first_df$woj <- (substr(first_df$Kod,1,2))

first_df<-merge(first_df,wojewodztwa,by = "woj",all.x = TRUE)

first_df$Kod.y <- NULL
first_df <- rename(first_df,wojewodztwo = Nazwa.y)


srednie_zarobki_wojewodztwa <- first_df %>% group_by(wojewodztwo) %>%
  summarise(srednie_zarobki = mean(`przeciętne miesięczne wynagrodzenia brutto w relacji do średniej krajowej (Polska=100)`))

srednie_zarobki_wojewodztwa <- na.omit(srednie_zarobki_wojewodztwa)

srednia_liczba_rozwodow <- first_df %>% group_by(wojewodztwo) %>%summarise( srednia_liczba_rozwodow_w_stosunku_do_ll = mean(liczba_rozwodow_w_stosunku_do_liczby_ludnosci))

srednia_liczba_rozwodow <- na.omit(srednia_liczba_rozwodow)

View(srednie_zarobki_wojewodztwa)

#zarobki w powiatach
ggplot(dane_z_sf,aes(fill = `przeciętne miesięczne wynagrodzenia brutto w relacji do średniej krajowej (Polska=100)` )) + geom_sf()+
  scale_fill_gradient( low = 'white' ,  high = 'dark green')

ggplot(srednie_zarobki_wojewodztwa,aes(x = wojewodztwo,y = srednie_zarobki))+
  geom_bar(stat="identity", fill = 'green') + 
  ylab("średnie miesięczne zarobki")+
  ggtitle("Średnia wysokość zarobków w województwach")+
  theme(axis.text.x=element_text(angle =- 90, vjust = 0.5))

ggplot(srednia_liczba_rozwodow,aes(x = wojewodztwo,y = srednia_liczba_rozwodow_w_stosunku_do_ll))+
  geom_bar(stat="identity", fill = 'red') + 
  ylab("Średnia liczba rozwodów")+
  theme(axis.text.x=element_text(angle =- 90, vjust = 0.5))

posortowane_rozwody <- first_df[order(first_df$liczba_rozwodow_w_stosunku_do_liczby_ludnosci),]

ggplot(first_df,aes(x = first_df$`przeciętne miesięczne wynagrodzenia brutto w relacji do średniej krajowej (Polska=100)`,
                    y = first_df$liczba_rozwodow_w_stosunku_do_liczby_ludnosci)) +
      geom_point() +
    geom_smooth(method = "lm")

#denisty

 ggplot(first_df, aes(x=first_df$liczba_rozwodow_w_stosunku_do_liczby_ludnosci)) + 
  geom_density(fill="lightblue") +
   geom_vline(aes(xintercept=mean(first_df$liczba_rozwodow_w_stosunku_do_liczby_ludnosci)),
              color="blue", linetype="dashed", size=1)
 
 ggplot(first_df, aes(x=first_df$`przeciętne miesięczne wynagrodzenia brutto w relacji do średniej krajowej (Polska=100)`)) + 
   geom_density(fill="lightgreen") +
   geom_vline(aes(xintercept=mean(first_df$`przeciętne miesięczne wynagrodzenia brutto w relacji do średniej krajowej (Polska=100)`)),
              color="blue", linetype="dashed", size=1)
 
 
 simple.fit = lm(liczba_rozwodow_w_stosunku_do_liczby_ludnosci~`przeciętne miesięczne wynagrodzenia brutto w relacji do średniej krajowej (Polska=100)`, data=first_df)
 summary(simple.fit)





