library(tidyverse)
library(plotly)
library(car)
library(lmtest)
library(tseries)
library(sandwich)

#### WCZYTANIE I OBRÓBKA DANYCH ####

# Wczytanie danych
data.df <- read_csv2("data.csv")
data.df <- data.df %>%
  filter(Panel_id == 'CHAIN_3')

sum(is.na(data.df))


# Wyfiltrowanie B09 i B02 z CHAIN 3, sprawdzenie NA

data.df.1 <- data.df %>%
  select(-matches("B(?!02)\\d+", perl = TRUE))

sum(is.na(data.df.1))


data.df.2 <- data.df %>%
  select(-matches("B(?!09)\\d+", perl = TRUE))

sum(is.na(data.df.2))


# Porównanie zbiorów i wybór marki modelowanej
sort(intersect(colnames(data.df.1), colnames(data.df.2)))
sort(setdiff(colnames(data.df.1), colnames(data.df.2)))
sort(setdiff(colnames(data.df.2), colnames(data.df.1)))
# wychodzi na to, że w B09 jest więcej SKU, a w B02 więcej
#   reklam. Wobec tego modelować będę B02.
rm(data.df.2)


#### SELEKCJA SKUs MARKI ####

# Wykres wolumenu sprzedaży marki, subbrandów i SKUs
p <- data.df.1 %>%
  select(Date, starts_with("VO_B02")) %>%
  pivot_longer(cols = starts_with("VO_B02"),
               names_to = "Name",
               values_to = "VO") %>%
  plot_ly(type = "scatter", 
          mode = 'lines',
          x = ~Date, 
          y = ~VO,
          color = ~Name)
htmlwidgets::saveWidget(p, "wykres.html", selfcontained = TRUE)
browseURL("wykres.html")
# widać, że marka to praktycznie subbrand S02 - subbrand S01 został wpro-
#   wadzony dopiero we wrześniu 2011 (i i tak stanowi małą część). Oba 
#   SKU z S01 wyglądają na stałe. W przypadku S02 widać, że głównym moto-
#   rem jest czteropak puszek 500 ml, wspierany przez pojedyncze i oś-
#   miopaki puszek 500 ml i pojedyncze butelki bezzwrotne 660 ml (te 
#   4 SKU są stałe). Oprócz tego występują in-outy, z których szczegól-
#   nie interesujące są puszki 500 ml - ośmiopak z gratisem, dwunastopak
#   i dwunastopak z dwoma gratisami, i to te trzy in-outy wydają się być
#   powodem zmiany zachowania marki (o którym niżej)


# Wybór SKU do dalszych analiz - udział w wolumenie (kryterium ~5%)

sku_names <- grep("^VO_B02_S\\d{2}_", names(data.df.1), value = TRUE)

sku_shares <- map_dfr(sku_names, function(sku) {
  tmp <- data.df.1 %>%
    filter(.data[[sku]] > 0)
  
  data.frame(
    SKU = sku,
    share = sum(tmp[[sku]]) / sum(tmp$VO_B02)
  )
})

sku_shares %>%
  arrange(SKU)
sku_shares %>%
  filter(share >= 0.05) %>%
  arrange(SKU)
# wyniki są ogólnie zgodne z tym, co było widać na wykresie. Biorąc kry-
#   terium 5%, kandydatami do uwzględnienia w modelu są SKU wymienione
#   wyżej (przy analizie wykresu wolumenów), oprócz pojedynczej puszki
#   500 ml z S01 i ośmiopaku bez gratisu z S02 (SKU nieomawiane również
#   zostają pominięte). Zastanowić możnaby się głębiej nad ośmiopakiem,
#   bo wydawał się mieć potencjał, ale to najwyżej na dalszym etapie. 

# Czyli wybrane SKU to B02_S01_04XCN0500, B02_S02_01XCN0500,
#   B02_S02_01XNR0660, B02_S02_04XCN0500, B02_S02_08XCN0500_1GR,
#   B02_S02_12XCN0500 i B02_S02_12XCN0500_2GR
selected_sku <- sku_shares %>%
  filter(share >= 0.05) %>%
  pull(SKU) %>%
  sub("^VO_", "", .)


#### ANALIZA GRAFICZNA ####

options(viewer = NULL)

# Wykres wolumenu sprzedaży całej kategorii i modelowanej marki oraz udział
#   marki w kategorii
p <- plot_ly(data.df.1, 
             type = "scatter", 
             mode = 'lines',
             x = ~Date, 
             y = ~VO_TOTAL_CATEGORY,
             name = "VO_TOTAL_CATEGORY") %>%
  add_trace(
    mode = 'lines',
    x = ~Date, 
    y = ~VO_B02, 
    name = "VO_B02") %>%
  add_trace(
    mode = 'lines',
    x = ~Date, 
    y = ~VO_B02 / VO_TOTAL_CATEGORY, 
    name = "Share VO_B02")
htmlwidgets::saveWidget(p, "wykres.html", selfcontained = TRUE)
browseURL("wykres.html")
sum(data.df.1$VO_B02)/sum(data.df.1$VO_TOTAL_CATEGORY)
# B02 wolumenowo stanowi na oko jakieś 10% kategorii (dokładnie średnio 13%).
#   W kategorii widać sezonowość, w marce na oko niekoniecznie. Dodatkowo
#   od kwietnia 2011 wygląda na to, że coś się dzieje z marką - jej
#   zachowanie zaczyna mocniej odbiegać od zachowania kategorii


# Wykres wolumenu sprzedaży całej kategorii i modelowanej marki
#   oraz sezonowości (wszystko jako indeksy dla porównania zachowania)
p <- plot_ly(data.df.1, 
             type = "scatter", 
             mode = 'lines',
             x = ~Date, 
             y = ~VO_TOTAL_CATEGORY/mean(data.df.1$VO_TOTAL_CATEGORY),
             name = "VO_TOTAL_CATEGORY") %>%
  add_trace(
    mode = 'lines',
    x = ~Date, 
    y = ~TI_SEASONALITY/mean(data.df.1$TI_SEASONALITY), 
    name = "SEASONALITY") %>%
  add_trace(
    mode = 'lines',
    x = ~Date, 
    y = ~VO_B02/mean(data.df.1$VO_B02), 
    name = "VO_B02")
htmlwidgets::saveWidget(p, "wykres.html", selfcontained = TRUE)
browseURL("wykres.html")
# potwierdzenie, że kategoria jest sezonowa. Jej sprzedaż jest największa w
#   lato. W modelowanej marce również widać sezonowość. Zasadniczo zachowuje
#   się ona podobnie jak kategoria. Okresem potencjalnie szczególnie ciekawym
#   wydaje się rzeczywiście ten od kwietnia 2011 - wtedy marka zaczyna zmie-
#   niać się mocniej niż kategoria, a w sierpniu i wrześniu nawet ,,idzie pod
#   prąd''


# Wykres wolumenu i dystrybucji marki oraz wybranych SKU, ich dystrybucji 
#   i ceny (indeksy)
selected_cols_tmp <- c(paste0("VO_", selected_sku),
                       paste0("DN_", selected_sku),
                       paste0("PR_", selected_sku))
p <- data.df.1 %>%
  select(Date, VO_B02, DN_B02, all_of(selected_cols_tmp)) %>%
  pivot_longer(cols = -Date,
               names_to = "Name",
               values_to = "Value") %>%
  group_by(Name) %>%
  mutate(Value_index = Value / mean(Value)) %>%
  ungroup() %>%
  plot_ly(type = "scatter", 
          mode = 'lines',
          x = ~Date, 
          y = ~Value_index,
          color = ~Name)
htmlwidgets::saveWidget(p, "wykres.html", selfcontained = TRUE)
browseURL("wykres.html")
# dla in-outów dystrybucja bardzo dobrze oddaje wolumen. Dla B02_S01_04XCN0500
#   (nowo wprowadzony) też dobrze. Dla stałych SKU gorzej - może wyjaśniać 
#   pojedyńcze zmiany, ale w porównaniu z ogólną zmiennością wolumenów jest 
#   względnie stała. Dla DN_B02_S02_12XCN0500_2GR wpływ jest bardzo widoczny,
#   dla pozostałych in-outów nie. Cena odwrotnie - dla in-outów niewiele wno-
#   si, a dla stałych raczej więcej niż dystrybucja, ale najlpiej chyba po-
#   łączyć

# Wzór na przekształcenie dające betę jako wsp. inkrementalności:
#   [DN_INOUT * sum(VO_INOUT)]  /  [sum(DN_INOUT) * mean(VO_MARKA)]

# Dodanie kolumn z przekształconym DN
data.df <- data.df %>%
  mutate(
    across(
      all_of(paste0("DN_", selected_sku)),
      ~ .x * sum(data.df[[sub("^DN_", "VO_", cur_column())]]) /
        (sum(.x) * mean(data.df$VO_B02)),
      .names = "{sub('^DN_', 'DN_adj_', .col)}"))


# Wykresy cen 
selected_cols_tmp <- c(paste0("PR_", selected_sku))
p <- data.df.1 %>%
  select(Date, all_of(selected_cols_tmp)) %>%
  pivot_longer(cols = -Date,
               names_to = "Name",
               values_to = "PR") %>%
  plot_ly(type = "scatter", 
          mode = 'lines',
          x = ~Date, 
          y = ~PR,
          color = ~Name)
htmlwidgets::saveWidget(p, "wykres.html", selfcontained = TRUE)
browseURL("wykres.html")
# są w miarę różne, nie ma co uśredniać


# Wykres liczby sklepów i wolumenu marki
p <- plot_ly(data.df.1, 
        type = "scatter", 
        mode = 'lines',
        x = ~Date, 
        y = ~NS) %>%
  add_trace(
    mode = 'lines',
    x = ~Date, 
    y = ~VO_B02, 
    name = "vO_B02")
htmlwidgets::saveWidget(p, "wykres.html", selfcontained = TRUE)
browseURL("wykres.html")
# brak drastycznych zmian, stabliny trend wzrostowy


selected_sku


model <- lm(data = data.df,
           I(VO_B02 / mean(VO_B02)) ~
             I(TI_SEASONALITY / mean(TI_SEASONALITY)) +
             DN_adj_B02_S02_12XCN0500_2GR +
             DN_adj_B02_S02_08XCN0500_1GR +
             DN_adj_B02_S02_12XCN0500 +
             log(PR_B02_S02_04XCN0500) +  
             #log(PR_B02_S02_01XCN0500) +  
             #log(PR_B02_S02_01XNR0660) +
             #DN_adj_B02_S02_01XNR0660) +
             #DN_adj_B02_S01_04XCN0500) +
             DN_B02 +
             #NS +
             #TI_H_NEW_YEAR +
             TI_H_MAY +
             #TI_H_XMAS +
             #TI_H_XMAS_BEFORE +
             #TI_H_XMAS_BEFORE2 +
             #TI_H_ASSUM_OF_MARY +
             TI_H_CORPUS_CHRISTI +
             TI_H_EASTER_MONDAY +
             TI_H_EASTER_SUNDAY +
             #TI_H_EASTER_SUNDAY_BEFORE +
             #TI_H_EPIPHANY +
             #TI_H_HALLOWEEN +
             TI_H_HALLOWEEN_BEFORE +
             #TI_H_INDEPENDENCE +  ewentualnie do dodania
             TI_H_PENTECOST
             # święta ewentualnie do sprawdzenia na łączną nieistotność
  )

summary(model)

jarque.bera.test(model$residuals)
vif(model)
bptest(model)
bgtest(model)
# Co się działo w modelu podczas dodawania kolejnych zmiennych:
#   - sama sezonowość ma nieduży wpływ, ok. 0,4%, R^2 3% i pval 7%

#   - inkrementalność DN_B02_S02_12XCN0500_2GR ładnie wchodzi i poprawia,
#       jest w przedziale <0; 1>,R^2 60%, ale heteroskedastyczność

#   - inkrementalność DN_adj_B02_S02_08XCN0500_1GR nie wchodzi, pval 67%
#       i zły znak, jako że na wykresie nie było bardzo widoczne to może
#       być do usunięcia potem, ale się zobaczy

#   - inkrementalność DN_adj_B02_S02_12XCN0500 w miarę wchodzi, ale pval 25%, 
#       trochę dziwne, że to wchodzi a DN_adj_B02_S02_08XCN0500_1GR nie, ale 
#       zobaczymy co dalej

#   - log ceny PR_B02_S02_04XCN0500 wchodzi idealnie, R^2 84%, znaki i pval w
#       w DN_adj_B02_S02_08XCN0500_1GR i DN_adj_B02_S02_12XCN0500 zmieniają
#       się na poprawne

#   - log ceny PR_B02_S02_01XCN0500 nie wchodzi, pval 70% i zły znak, ale z 
#       wykresu mógłby wejść, zobaczymy co dalej

#   - log ceny PR_B02_S02_01XNR0660 też nie wchodzi, pval 25% i zły znak, ale z 
#       wykresu bardziej dystrybucja

#   - ,,inkrementalność'' (stały SKU) PR_B02_S02_01XNR0660 też nie wchodzi, 
#       pval 50% i znacznie za duża wartość, w wykresu mogłoby wejść, zoba-
#       czymy co dalej 

#   - dystrybucja marki w miarę wchodzi, dobry znak, pval 11%, do obserwacji

#   - NS nieistotne praktycznie i statystycznie, pval 90%, drobna
#       współliniowość z PR_B02_S02_04XCN0500, raczej do wyrzucenia

#   - Nowy Rok nie wchodzi, pval 85%, trochę dziwne

#   - Majówka wchodzi (z pval 12%, ale mocno więc do zostawienia)

#   - Boże Narodzenie i przed nie wchodzą, wysokie pval >90%

#   - ze świąt zachowane na teraz w modelu oprócz tych wyżej są Boże Ciało,
#       Poniedziałek Wielkanocny, Niedziela Wielkanocna, przed Halloween, 
#       Pentecoste





p <- plot_ly(data.df, 
             type = "scatter", 
             mode = 'lines',
             x = ~Date, 
             y = ~TI_TEM_AVG,
             name = "TI_TEM_AVG") %>%
  add_trace(
    mode = 'lines',
    x = ~Date, 
    y = ~TI_TEM_AVG_NORM, 
    name = "TI_TEM_AVG_NORM")
htmlwidgets::saveWidget(p, "wykres.html", selfcontained = TRUE)
browseURL("wykres.html")



plot_ly(data.df, 
        type = "scatter", 
        mode = 'lines',
        x = ~Date, 
        y = ~EC_CPI) %>%
  layout(yaxis = list(range = c(0, max(data.df$EC_CPI))))



p <- plot_ly(data.df,
             type = "scatter",
             mode = 'lines',
             x = ~Date,
             y = ~VO_B05_S04_04XCN0500,
             name = "VO_B05_S04_04XCN0500") %>%
  add_trace(
    mode = 'lines',
    yaxis = "y2",
    x = ~Date,
    y = ~PR_B05_S04_04XCN0500,
    name = "PR_B05_S04_04XCN0500") %>%
  layout(yaxis  = list(range = c(0, max(data.df$VO_B05_S04_04XCN0500))),
         yaxis2 = list(overlaying = "y",
                       side = "right"))
htmlwidgets::saveWidget(p, "wykres.html", selfcontained = TRUE)
browseURL("wykres.html")


















