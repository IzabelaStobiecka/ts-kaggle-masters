
# Skupimy się teraz na narysowaniu wykresów danych, znalezieniu potencjalnej
# sezonowości i stworzeniu szeregów czasowych. Po wykonaniu tych czynności
# będziemy mogli zająć się szukaniem odpowiednich modeli.

# Rozpoczniemy od wczytania danych z pliku (danych, które zostały uproszczonego
# poprzez usunięcie sklepów z podobną sprzedażą danego produktu).

library(dplyr)
library(RColorBrewer)
library(forecast)
library(tseries)
library(DescTools)

sciezka_pliku <- paste0("dane/dane_do_analizy.rds")

dane <- readRDS(sciezka_pliku)
tail(dane)


# Kolejnym krokiem będzie rozdzielenie danych ze względu na produkt i sklep
# (każdy kombinacja produktu i sklepu będzie zachowana w liście). Ustalmy dodatkowo
# zmienną frequency czyli liczbę obserwacji na jednostkę czasu i stwórzmy szeregi
# czasowe z naszych danych.


f <- 365
ts_dane <- split(dane, list(dane$store, dane$item)) %>%
    lapply(function(x) ts(x$sales, frequency = f))

# Dodatkowo zapiszmy stworzone szeregi w oddzielnym pliku, aby móc korzystać z
# nich w innych raportach.


saveRDS(ts_dane, file = "dane/szeregi.rds")

# Aby zobaczyć jak wyglądają nasze dane i wnioskować o sezonowości wykonamy teraz
# wykresy za pomocą funckji `tsdisplay()` aby zobaczyć również wykresy `acf` i
# `pacf`. Dodatkowo dla każdego szeregu przeprowadzimy test rozstrzygający o
# stacjonarności (`adf.test()`), wypiszemy p-value oraz stworzymy wektor `d`, który
# będzie przechowywał informację czy szereg powinien być różnicowany przy tworzneiu
# modelu czy nie. Dla wyżej wymienionego testu hipotezą zerową jest, że szereg nie
# jest stacjonarny. Ustalimy również poziom `alfa` na którym bedziemy sprawdzali
# p-value.

d <- rep(0, length(ts_dane))
alfa <- 0.05

for (i in 1:length(ts_dane)) {
    tsdisplay(ts_dane[[i]], main = paste("Dane dla produkt.sklep:", names(ts_dane[i])))

    a <- adf.test(ts_dane[[i]], alternative = "stationary")
    print(a$p.value)

    if (a$p.value >= alfa) {
        d[i] = 1
    }
}

#Zapiszmy również do pliku nasz wektor z informacją o konieczności różnicowania.

saveRDS(d, file = "dane/wektor_d.rds")

# Z powyższych wykresów jesteśmy w stanie zaobserwować wyraźną sezonowość
# dla wszystkich produktów (roczną) oraz trend rosnący. Warto również
# zauważyć, że nasze dane całkiem nieźle zachowują się na wykresie `pacf`
# co może sugerować użycie modelu `AR(p)`.
