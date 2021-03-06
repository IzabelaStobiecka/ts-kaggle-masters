
library(dplyr)
library(RColorBrewer)
library(forecast)
library(tseries)
library(DescTools)
library(knitr)

source("functions.R", encoding = "UTF-8")

sciezka_pliku <- paste0("dane/train.csv")

## Wczytywanie danych

dane <- read.csv(sciezka_pliku)

sklep <- dane$store
produkt <- dane$item
sprzedaz <- dane$sales

ktoryProdukt <- factor(produkt, levels = c("1":"50"))
ktorySklep <- factor(sklep, levels = c("1":"10"))

head(dane)

## Analiza wariancji - Czy istnieje sklep którego średnia sprzedaż różni się istotnie statystycznie od sprzedaży w innych sklepach?

boxplot(sprzedaz~ktorySklep, xlab = "Numer sklepu", ylab = "Sprzedaż produktów",
        col = brewer.pal(n = 10, name = "PiYG"))
points(1:10, by(sprzedaz, ktorySklep, mean), pch = 15)
by(sprzedaz, sklep, mean)


#Sprawdzenie założeń: normalność rozkładu każdej zmiennej losowej

par(mfrow = c(2, 5), pty = "m")

for (i in as.character(1:10)) {
    qq(sprzedaz[ktorySklep == i])
}

par(mfrow = c(1,1))


#Sprawdźmy teraz za pomocą testu Shapiro normalność danych z podziałem na sklepy.
#Zrezygnujemy z mniej istotnych dla nas informacji i wypiszemy jedynie p-wartość.


dane %>%
    group_by(store) %>%
    summarize(p_value =
                  shapiro_pvalue(sales))

# Widzimy, że p-wartości dla wszystkich sklepów są bardzo niskie. Załóżmy jednak,
# na podstawie wykresów `qqnorm` + `qqline` iż mają one rozkład zbliżony do
# normalnego.

# Przy takim założeniu możemy kontynuować naszą analizę.

alfa <- 0.05
anova(lm(sprzedaz ~ ktorySklep))
TukeyHSD(aov(sprzedaz ~ ktorySklep), conf.level = 1 - alfa)
plot(TukeyHSD(aov(sprzedaz ~ ktorySklep), conf.level = 1 - alfa))



#Średnia sprzedaż produktów jest podobna dla następujących par sklepów: _9-4, 6-5_

# Analiza wariancji - Czy istnieje produkt którego średnia sprzedaż różni się
# istotnie statystycznie od sprzedaży w innych sklepach? Aby rozpocząć taką
# analizę podzielmy nasze dane ze względu na produkty i sklepy:


dane %>%
    split(dane$item) -> dane_podzielone_p


# Uzyskaliśmy w ten sposób 50 list, z czego każda opisuje sprzedaż innego produktu.
# W następnym kroku wykorzystamy stworzoną funkcję `analiza_produktu()` aby narysować
# boxploty dla danego produktu z podziałem na sklepy, dokonać sprawdzenia
# normalności rozkładu oraz wykonać ANOVE i test Tukey'a.


lapply(dane_podzielone_p, function(x) analiza_produktu(x, alfa))

# Powyższa analiza pokazała, że średnia sprzedaż dla wszystkich produktów była
# nieistotnie rózna statystycznie dla par `10-3`, `9-4`, `6-5`. Zastosujmy zatem
# do uproszczenia danych podejście, w którym dane ze sklepów 10, 9 i 6 potraktujemy
# jako dane ze sklepów odpowiednio 3, 4, 5. Mając wtedy po dwie wartości sprzedaży
# dla każdego dnia, jako wartość, która będziemy modelować weźmiemy ich średnią.

dane %>%
    zamiana_sklepow(3, 10) %>%
    zamiana_sklepow(4, 9) %>%
    zamiana_sklepow(5, 6) -> dane_uproszczone

#Aby móc łatwo korzystać z nowych danych zapiszemy je do pliku RDS.

saveRDS(dane_uproszczone, "dane/dane_do_analizy.rds")


