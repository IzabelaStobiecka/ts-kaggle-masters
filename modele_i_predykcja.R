library(dplyr)
library(forecast)

# Poprzednie analizy pozwoliły nam zredukować dane oraz wysnuć wnioski o
# stacjonarnosci. Kolejnym i ostatnim krokiem będzie stworzenie modeli i dokonanie
# predykcji. Wczytajmy zatem nasze szeregi czasowe oraz wektor odpowiadający za
# różnicowanie szergów.

ts_dane <- readRDS("dane/szeregi.rds")
d <- readRDS("dane/wektor_d.rds")
testowy <- read.csv(paste0("dane/test.csv")) %>%
    mutate(sales = 0)


#Przejdźmy do stworzenia odpowieniego modelu oraz predykcji.

nazwy <- names(ts_dane)
sklepy <- substr(nazwy, 1, 1)
produkty <- substr(nazwy, 3, 6)
powtorzenia <- c(3, 4, 5)

p <- 5
q <- 1

for (i in seq_along(ts_dane)) {
    a <- Arima(ts_dane[[i]], order = c(p, d[i], q))

    n <- nrow(testowy[testowy$store == sklepy[i] & testowy$item == produkty[i], ])

    testowy[testowy$store == sklepy[i] & testowy$item == produkty[i], 5] <- forecast(a, h = n)$mean
}

#Auto arima - nie puszczona, wiec trzeba puscic na jakims kompie co ogarnie
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
for (i in seq_along(ts_dane)) {
    a <- auto.arima(ts_dane[[i]],
                    d = d[i],
                    stepwise = TRUE,
                    approximation = TRUE)

    n <- nrow(testowy[testowy$store == sklepy[i] & testowy$item == produkty[i], ])

    testowy[testowy$store == sklepy[i] & testowy$item == produkty[i], 5] <- forecast(a, h = n)$mean
}
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

#Kolejnym krokiem jest uzupełnienie pliku testowego o predykowane wartości. Mając na uwadze pominięcie predykcji dla sklepów
#6(=5) ,9(=4) ,10(=3) uzupełnijmy plik testowy odpowiednimi wartościami.


testowy[testowy$store == 6, 5] <- testowy[testowy$store == 5, 5]
testowy[testowy$store == 9, 5] <- testowy[testowy$store == 4, 5]
testowy[testowy$store == 10, 5] <- testowy[testowy$store == 3, 5]


write.csv2(testowy, file = "dane/output.csv")



