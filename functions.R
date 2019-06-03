
#funckja rysująca qqnorm i qqline

qq <- function(x) {
    qqnorm(x)
    qqline(x, col = "red")
}

#Funckja wykonująca Shapiro test i printująca jedynie p-value
#bo tylko to nas interesuje, dodatkowo, dla zbyt dużych zbiorów
#danych, pobiera nam próbkę o długości maksymalnej dla tego
#testu i na niej wykonuje test. Nie zminiejsza to ogólności.

shapiro_pvalue <- function(x) {
    if (length(x) > 5e3) {
        o <- sample(length(x), 5e3)
        x <- x[o]
    }
    shapiro.test(x)$p.value
}


# Funkcja, która dla listy wykonuje boxplot z podziałem na sklepy,
# weryfikację normalności rozkładu oraz ANOVE i test Tukey'a

analiza_produktu <- function(x) {

    print(paste("Analiza produktu", x[["item"]][[1]]))

    sprzedaz_p <- sprzedaz[ktoryProdukt == as.character(x[["item"]][[1]])]
    sklep_p <- sklep[ktoryProdukt == as.character(x[["item"]][[1]])]

    ktorySklep_p <- factor(sklep_p, levels = c("1":"10"))

    boxplot(sprzedaz_p ~ ktorySklep_p, xlab = "Numer sklepu",
            ylab = paste("Sprzedaż produktu", as.character(x[["item"]][[1]])),
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
    print(TukeyHSD(aov(sprzedaz_p~ktorySklep_p), conf.level = 0.95))
    print("Przedstawmy jeszcze powyzsze wyniki na wykresie:")
    plot(TukeyHSD(aov(sprzedaz_p~ktorySklep_p), conf.level = 0.95))
}

