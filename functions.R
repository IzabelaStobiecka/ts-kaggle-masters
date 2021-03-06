

##Funkcja rysująca qqnorm i qqline


qq <- function(x) {
    qqnorm(x)
    qqline(x, col = "red")
}


# Funkcja wykonująca Shapiro test i printująca jedynie p-value
# bo tylko to nas interesuje, dodatkowo, dla zbyt dużych zbiorów
# danych, pobiera nam próbkę o długości maksymalnej dla tego
# testu i na niej wykonuje test. Nie zminiejsza to ogólności.


shapiro_pvalue <- function(x) {
    if (length(x) > 5e3) {
        o <- sample(length(x), 5e3)
        x <- x[o]
    }
    shapiro.test(x)$p.value
}


# Funkcja, która dla listy wykonuje boxplot z podziałem na sklepy,
# weryfikację normalności rozkładu oraz ANOVE i test Tukey'a oraz
# wypisanie par, dla ktorych roznice sredniej nie są istotne
# statystycznie


analiza_produktu <- function(x, alfa = 0.05) {

    produkt <- x[["item"]][[1]]
    print(paste("Analiza produktu", produkt))

    sprzedaz_p <- sprzedaz[ktoryProdukt == as.character(produkt)]
    sklep_p <- sklep[ktoryProdukt == as.character(produkt)]

    ktorySklep_p <- factor(sklep_p, levels = c("1":"10"))

    boxplot(sprzedaz_p ~ ktorySklep_p, xlab = "Numer sklepu",
            ylab = paste("Sprzedaż produktu", as.character(produkt)),
            col = brewer.pal(n = 10, name = "PiYG"))
    points(1:10, by(sprzedaz_p, ktorySklep_p, mean), pch = 15)
    by(sprzedaz_p,  sklep_p, mean)

    print("Sprawdzenie normalnosci dla produktu, z podzialem na sklepy.")

    x %>% group_by(store) %>%
        summarize(
            p_value = shapiro_pvalue(sales)) %>%
        print()

    print("W celu dalszych badan zalozmy jednak, że kazda proba pochodzi z rozkladu zblizonego do rozkladu normalnego. Teraz wykonamy analize wariancji i testy post hoc.")

    anova(lm(sprzedaz_p ~ ktorySklep_p))
    t <- TukeyHSD(aov(sprzedaz_p~ktorySklep_p), conf.level = 1 - alfa)
    p_values <- t[["ktorySklep_p"]][ , 4]
    istotne <- which(p_values >= alfa)
    print("Pary, dla których śrendie nie różnią się istotnie statystycznie")
    print(names(istotne))
}


#Funkcja, która dla podzielonych na produkty danych dokona zmiany
#sklepu 10 na 3, 9 na 4, 6 na 5 oraz jako sales weźmie średnią
#z sales dla obu złączonych sklepów.

zamiana_sklepow <- function(x, domyslna, zmieniana) {

    #x to wstepne dane
    x %>%
        filter(store == domyslna) %>%
        select(sales) -> kol1
    x %>%
        filter(store == zmieniana) %>%
        select(sales) -> kol2

    #srednia dla dwoch zlaczanych kolumn
    kolumna_wyjsciowa <- (kol1 + kol2) / 2

    #usuwamy dane z drugim sklepem
    x %>%
        filter(store != zmieniana) -> usuniete_zmieniane

    indeksy <- which(usuniete_zmieniane$store == domyslna)

    #wklejamy srednie w wiersze z pierwszego sklepu
    usuniete_zmieniane[indeksy, 4] <- kolumna_wyjsciowa

    return(usuniete_zmieniane)
}
