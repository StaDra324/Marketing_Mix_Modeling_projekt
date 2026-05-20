library(tidyverse)
library(plotly)
library(car)
library(lmtest)
library(tseries)
library(sandwich)
library(stargazer)
library(scales)

## Spis treści ##
#                                                    linijki
# - WCZYTANIE I WSTĘPNE OGARNIĘCIE DANYCH       ||  28   - 60
# - ANALIZA EKSPLORACYJNO - GRAFICZNA           ||  61   - 678
#     - WYKRESY                                 ||    65   - 120
#     - SELEKCJA SKUs MARKI                     ||    121  - 181
#     - WYKRESY CD.                             ||    182  - 303
#     - ANALIZA SKUs KONKURENCJI                ||    304  - 385
#     - ANALIZA MEDIÓW MARKI                    ||    386  - 657
#     - MAKROEKONOMIA                           ||    658  - 678
# - MODELOWANIE                                 ||  679  - 830
# - WALIDACJA MODELU                            ||  831  - 1118
# - DEKOMPOZYCJA MODELU                         ||  1119 - 1723
#     - WYBÓR POZIOMÓW BAZOWYCH                 ||    1139 - 1259
#     - ANALIZY GRAFICZNE                       ||    1250 - 1723
# - SYNTETYCZNA ANALIZA MEDIÓW (TV)             ||  1721 - 1774


#### WCZYTANIE I WSTĘPNE OGARNIĘCIE DANYCH ####

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


#### ANALIZA EKSPLORACYJNO - GRAFICZNA ####

options(viewer = NULL)

## WYKRESY ##

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

## SELEKCJA SKUs MARKI ##

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
#   powodem zmiany zachowania marki


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
#   bo wydawał się mieć potencjał, ale to najwyżej na dalszym etapie
rm(sku_names)

# Czyli wybrane SKU to B02_S01_04XCN0500, B02_S02_01XCN0500,
#   B02_S02_01XNR0660, B02_S02_04XCN0500, B02_S02_08XCN0500_1GR,
#   B02_S02_12XCN0500 i B02_S02_12XCN0500_2GR
selected_sku <- sku_shares %>%
  filter(share >= 0.05) %>%
  pull(SKU) %>%
  sub("^VO_", "", .)

## WYKRESY CD. ##

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
rm(selected_cols_tmp)

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
rm(selected_cols_tmp)


# Wykres liczby sklepów i wolumenu marki
p <- plot_ly(data.df.1, 
        type = "scatter", 
        mode = 'lines',
        x = ~Date, 
        y = ~NS,
        name = "NS") %>%
  add_trace(
    mode = 'lines',
    x = ~Date, 
    y = ~VO_B02, 
    name = "vO_B02")
htmlwidgets::saveWidget(p, "wykres.html", selfcontained = TRUE)
browseURL("wykres.html")
# brak drastycznych zmian, stabliny trend wzrostowy


# Wykres średniej temperatury, jej normy i odchylenia
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
    name = "TI_TEM_AVG_NORM") %>%
  add_trace(
    mode = 'lines',
    x = ~Date, 
    y = ~(TI_TEM_AVG - TI_TEM_AVG_NORM), 
    name = "TI_TEM_DEV")
htmlwidgets::saveWidget(p, "wykres.html", selfcontained = TRUE)
browseURL("wykres.html")


# Wykres ekspozycji, udziału w ekspozycji i wolumenu marki (indeksy)
p <- plot_ly(data.df, 
             type = "scatter", 
             mode = 'lines',
             x = ~Date, 
             y = ~VO_B02 / mean(VO_B02),
             name = "VO_B02") %>%
  add_trace(
    mode = 'lines',
    x = ~Date, 
    y = ~EX_NU_B02 / mean(EX_NU_B02), 
    name = "EX_NU_B02") %>%
  add_trace(
    mode = 'lines',
    x = ~Date, 
    y = ~EX_SH_B02 / mean(EX_SH_B02), 
    name = "EX_SH_B02")
htmlwidgets::saveWidget(p, "wykres.html", selfcontained = TRUE)
browseURL("wykres.html")
# zarówno wersja numeryczna i udział eskpozycji dobrze przekładają się na
#   wolumen, numerczna chyba minimalnie lepiej

## ANALIZA SKUs KONKURENCJI ##

# Wybór SKU konkurencji do dalszych analiz - marki konkurencji średnio
#   mające wolumen >= 15% naszej marki, z tego subbrandy >= 10% i z nich
#   SKU >= 5%

# Wybranie marek

brand_names_comp <- grep("^VO_B\\d{2}$", names(data.df), value = TRUE) %>%
  setdiff("VO_B02")

brand_shares_comp <- map_dfr(brand_names_comp, function(brand) {
  tmp <- data.df %>%
    filter(.data[[brand]] > 0)
  
  data.frame(
    brand = brand,
    share = sum(tmp[[brand]]) / sum(tmp$VO_B02)
  )
}) %>%
  filter(share >= 0.15)

brand_shares_comp %>%
  arrange(share)

selected_brands_comp <- brand_shares_comp$brand

# Wybranie subbrandów

subbrand_names_comp <- grep("^VO_B\\d{2}_S\\d{2}$", names(data.df), value = TRUE)

subbrand_names_comp <- subbrand_names_comp[
  str_extract(subbrand_names_comp, "^VO_B\\d{2}") %in% selected_brands_comp]

subbrand_shares_comp <- map_dfr(subbrand_names_comp, function(subbrand) {
  tmp <- data.df %>%
    filter(.data[[subbrand]] > 0)
  
  data.frame(
    subbrand = subbrand,
    share = sum(tmp[[subbrand]]) / sum(tmp$VO_B02)
  )
}) %>%
  filter(share >= 0.10)

subbrand_shares_comp %>%
  arrange(share)

selected_subbrands_comp <- subbrand_shares_comp$subbrand

# Wybranie SKU

sku_names_comp <- grep("^VO_B\\d{2}_S\\d{2}_", names(data.df), value = TRUE)

sku_names_comp <- sku_names_comp[
  str_extract(sku_names_comp, "^VO_B\\d{2}_S\\d{2}") %in% selected_subbrands_comp]

sku_shares_comp <- map_dfr(sku_names_comp, function(sku) {
  tmp <- data.df %>%
    filter(.data[[sku]] > 0)
  
  data.frame(
    sku = sku,
    share = sum(tmp[[sku]]) / sum(tmp$VO_B02)
  )
}) %>%
  filter(share >= 0.05)

sku_shares_comp %>%
  arrange(share)

selected_sku_comp <- sku_shares_comp$sku
# ogólnie według kryteriów jest potencjalnie 38 SKU konkurencji do sprawdzenia,
#   jednak ze względu na to, że czas jest ograniczony, a to nie są kluczowe 
#   zmienne dla analizy, i R^2 i tak jest wysokie, to pomijam ich dokładną a-
#   analizę i sprawdzę tylko jak dystrybucja (bo to będzie in-out) SKU z naj-
#   wyższym share wpłynie na model. 
# Podobnie jeśli chodzi o ekspozycje konkurencji, sprawdzę tylko B01 w modelu
#   bo było największe
rm(brand_names_comp, selected_brands_comp, subbrand_names_comp, sku_names_comp,
   selected_subbrands_comp, brand_shares_comp, subbrand_shares_comp)

## ANALIZA MEDIÓW MARKI ##

# TV

# Wykres TV i wolumenu
p <- data.df.1 %>%
  select(Date, VO_B02, starts_with("TV")) %>%
  pivot_longer(cols = c(VO_B02, starts_with("TV")),
               names_to = "Name",
               values_to = "Value") %>%
  plot_ly(type = "scatter", 
          mode = 'lines',
          x = ~Date, 
          y = ~Value,
          color = ~Name)
htmlwidgets::saveWidget(p, "wykres.html", selfcontained = TRUE)
browseURL("wykres.html")
# zasadniczo na oko to w sumie w nie wygląda żeby TV miało wpływ na sprze-
#   daż. Wobec tego ciężko też ocenić poziom AdStocku. W modelu żaden nie
#   wchodzi, więc kierując się teorią (że dla TV jest względnie wysoki) 
#   oraz artykułem ,,Jak budować długookresową sprzedaż marki mediami?''
#   wybieram TV50 i spróbuje jeszcze to rozbić (od razu na półrocza, bo 
#   o ile patrząc na całość TV nie wygląda jakby miało wpływ, to patrząc 
#   tylko na drugie półrocza 2010 i 2011 już bardziej, co w sumie mogłoby
#   nawet mieć sens w kontekście sezonowości

# Wykres TV i wolumenu w pierwszym półroczu 2010
p <- data.df.1 %>%
  select(Date, VO_B02, starts_with("TV")) %>%
  filter(Date >= as.Date("2010-01-01"),
         Date <= as.Date("2010-06-30")) %>%
  pivot_longer(cols = c(VO_B02, starts_with("TV")),
               names_to = "Name",
               values_to = "Value") %>%
  plot_ly(type = "scatter", 
          mode = 'lines',
          x = ~Date, 
          y = ~Value,
          color = ~Name)
htmlwidgets::saveWidget(p, "wykres.html", selfcontained = TRUE)
browseURL("wykres.html")
# nie widać związku

# Wykres TV i wolumenu w drugim półroczu 2010
p <- data.df.1 %>%
  select(Date, VO_B02, starts_with("TV")) %>%
  filter(Date >= as.Date("2010-07-01"),
         Date <= as.Date("2010-12-31")) %>%
  pivot_longer(cols = c(VO_B02, starts_with("TV")),
               names_to = "Name",
               values_to = "Value") %>%
  plot_ly(type = "scatter", 
          mode = 'lines',
          x = ~Date, 
          y = ~Value,
          color = ~Name)
htmlwidgets::saveWidget(p, "wykres.html", selfcontained = TRUE)
browseURL("wykres.html")
# może trochę widać związek

# Wykres TV i wolumenu w pierwszym półroczu 2011
p <- data.df.1 %>%
  select(Date, VO_B02, starts_with("TV")) %>%
  filter(Date >= as.Date("2011-01-01"),
         Date <= as.Date("2011-06-30")) %>%
  pivot_longer(cols = c(VO_B02, starts_with("TV")),
               names_to = "Name",
               values_to = "Value") %>%
  plot_ly(type = "scatter", 
          mode = 'lines',
          x = ~Date, 
          y = ~Value,
          color = ~Name)
htmlwidgets::saveWidget(p, "wykres.html", selfcontained = TRUE)
browseURL("wykres.html")
# jakby bardzo się postarać to może by jakiś związek sie dało zobaczć,
#   raczej mimo wszystko nie widać związku

# Wykres TV i wolumenu w drugim półroczu 2011
p <- data.df.1 %>%
  select(Date, VO_B02, starts_with("TV")) %>%
  filter(Date >= as.Date("2011-07-01"),
         Date <= as.Date("2011-12-31")) %>%
  pivot_longer(cols = c(VO_B02, starts_with("TV")),
               names_to = "Name",
               values_to = "Value") %>%
  plot_ly(type = "scatter", 
          mode = 'lines',
          x = ~Date, 
          y = ~Value,
          color = ~Name)
htmlwidgets::saveWidget(p, "wykres.html", selfcontained = TRUE)
browseURL("wykres.html")
# jakby bardzo się postarać to może by jakiś związek sie dało zobaczć,
#   raczej mimo wszystko nie widać związku

# Ogólnie po analizie graficznej rzeczywiście najlepiej dopasowany wydaje
#   się AdStock 50%, aczkolwiek i tak raczej nie spodziewam się żeby zmienne
#   były istotne, ewentualnie dla drugiego półrocza 2010. Ale żeby to spraw-
#   dzić rozbijam po półroczach

# Rozbicie TV na półrocza

data.df$TI_Y_2010_H1
data.df$TI_Y_2010_H2
data.df$TI_Y_2011_H1
data.df$TI_Y_2011_H2

# Dodanie zmiennych rozbitych TV

# Rozbicie zmiennej bez adstocku
data.df <- data.df %>%
  mutate(TV00_B02_2010_H1 = TI_Y_2010_H1 * TV00_B02,
         TV00_B02_2010_H2 = TI_Y_2010_H2 * TV00_B02,
         TV00_B02_2011_H1 = TI_Y_2011_H1 * TV00_B02,
         TV00_B02_2011_H2 = TI_Y_2011_H2 * TV00_B02)
data.df.1 <- data.df.1 %>%
  mutate(TV00_B02_2010_H1 = TI_Y_2010_H1 * TV00_B02,
         TV00_B02_2010_H2 = TI_Y_2010_H2 * TV00_B02,
         TV00_B02_2011_H1 = TI_Y_2011_H1 * TV00_B02,
         TV00_B02_2011_H2 = TI_Y_2011_H2 * TV00_B02)

# Funkcja adstockująca - 
adstock_fun <- function(column, lvl) {
  y <- numeric(length(column))
  y[1] <- column[1]
  
  for (i in 2:length(column)) {
    y[i] <- (1 - lvl) * column[i] + lvl * y[i - 1]
  }
  
  return(y)
}

# Zadstockowanie levelami 10 - 90
periods <- c("2010_H1", "2010_H2", "2011_H1", "2011_H2")
for (lvl in seq(10, 90, 10)) {
  n <- lvl / 100
  
  for (p in periods) {
    raw_col <- paste0("TV00_B02_", p)
    ads_col <- paste0("TV", lvl, "_B02_", p)
    
    data.df[[ads_col]] <- adstock_fun(data.df[[raw_col]], n)
    data.df.1[[ads_col]] <- adstock_fun(data.df[[raw_col]], n)
  }
}

data.df.1 %>%
  select(starts_with("TV"))

# Wykres noworozbitych TV
p <- data.df.1 %>%
  select(Date, VO_B02, starts_with("TV")) %>%
  pivot_longer(cols = c(VO_B02, starts_with("TV")),
               names_to = "Name",
               values_to = "Value") %>%
  plot_ly(type = "scatter", 
          mode = 'lines',
          x = ~Date, 
          y = ~Value,
          color = ~Name)
htmlwidgets::saveWidget(p, "wykres.html", selfcontained = TRUE)
browseURL("wykres.html")
# rozbice w oczywisty sposób zachowuje ogólny kształt całości, więc
#   dalej nie spodziewam się związku

# Outdoor

# Wykres OH i wolumenu i jako indeksy
p <- data.df.1 %>%
  select(Date, VO_B02, starts_with("OH")) %>%
  pivot_longer(cols = c(VO_B02, starts_with("OH")),
               names_to = "Name",
               values_to = "Value") %>%
  group_by(Name) %>%
  mutate(Value_index = Value / mean(Value)) %>%
  ungroup() %>%
  plot_ly(type = "scatter", 
          mode = 'lines',
          x = ~Date, 
          y = ~Value,
          color = ~Name) %>%
  add_trace(type = "scatter", 
            mode = 'lines',
            x = ~Date, 
            y = ~Value_index,
            color = ~Name)
htmlwidgets::saveWidget(p, "wykres.html", selfcontained = TRUE)
browseURL("wykres.html")
# tutaj jest ciekawie, bo są tylko krótkie strzały, i przy największym 
#   wzroście OH jest też akurat najwyższy wzrost wolumenu, z drugiej stro-
#   pozostałe strzały OH nie wydają się wpływać, a dodatkowo dwa z trzech 
#   strzałów , w tym ten wyglądający na mający wpływ pokrywają się z 
#   SKU B02_S02_12XCN0500_2GR, więc coś tam może być pozorne. AdStock
#   z wykresu wybrałbym chyba OH00, na podstawie teorii co wcześniej można
#   by wziąć 50%, zobaczymy co się stanie w modelu. Po zachowaniu w modelu
#   (opisane w części modelowej) można jeszcze spróbować rozbić

# Dodanie zmiennych rozbitych OH
data.df <- data.df %>%
  mutate(OH00_B02_2010_H1 = TI_Y_2010_H1 * OH00_B02,
         OH00_B02_2010_H2 = TI_Y_2010_H2 * OH00_B02,
         OH00_B02_2011_H1 = TI_Y_2011_H1 * OH00_B02,
         OH00_B02_2011_H2 = TI_Y_2011_H2 * OH00_B02)
data.df.1 <- data.df.1 %>%
  mutate(OH00_B02_2010_H1 = TI_Y_2010_H1 * OH00_B02,
         OH00_B02_2010_H2 = TI_Y_2010_H2 * OH00_B02,
         OH00_B02_2011_H1 = TI_Y_2011_H1 * OH00_B02,
         OH00_B02_2011_H2 = TI_Y_2011_H2 * OH00_B02)

# Zadstockowanie levelami 10 - 90
periods <- c("2010_H1", "2010_H2", "2011_H1", "2011_H2")
for (lvl in seq(10, 90, 10)) {
  n <- lvl / 100
  
  for (p in periods) {
    raw_col <- paste0("OH00_B02_", p)
    ads_col <- paste0("OH", lvl, "_B02_", p)
    
    data.df[[ads_col]] <- adstock_fun(data.df[[raw_col]], n)
    data.df.1[[ads_col]] <- adstock_fun(data.df[[raw_col]], n)
  }
}

data.df.1 %>%
  select(starts_with("OH"))

# Wykres noworozbitych OH
p <- data.df.1 %>%
  select(Date, VO_B02, starts_with("OH")) %>%
  pivot_longer(cols = c(VO_B02, starts_with("OH")),
               names_to = "Name",
               values_to = "Value") %>%
  plot_ly(type = "scatter", 
          mode = 'lines',
          x = ~Date, 
          y = ~Value,
          color = ~Name)
htmlwidgets::saveWidget(p, "wykres.html", selfcontained = TRUE)
browseURL("wykres.html")
# jak w TV, tu też nie spodziewam się związku

# Radio

# Wykres RA i wolumenu i jako indeksy
p <- data.df.1 %>%
  select(Date, VO_B02, starts_with("RA")) %>%
  pivot_longer(cols = c(VO_B02, starts_with("RA")),
               names_to = "Name",
               values_to = "Value") %>%
  group_by(Name) %>%
  mutate(Value_index = Value / mean(Value)) %>%
  ungroup() %>%
  plot_ly(type = "scatter", 
          mode = 'lines',
          x = ~Date, 
          y = ~Value,
          color = ~Name) %>%
  add_trace(type = "scatter", 
            mode = 'lines',
            x = ~Date, 
            y = ~Value_index,
            color = ~Name)
htmlwidgets::saveWidget(p, "wykres.html", selfcontained = TRUE)
browseURL("wykres.html")
# radio w ogóle nie wygląda jakby miało jakikolwiek wpływ, dla formalności
#   można by wstawić i rozbić ale nie powinno wejść. AdStock zobaczymy jak 
#   będzie w modelu, z teorii 50% jak TV. Nawet nie rozbijam

# Kina nie ma

## MAKROEKONOMIA ##

# Wykres zmiennych makroekonomicznych i wolumenu
p <- data.df.1 %>%
  select(Date, VO_B02, starts_with("EC")) %>%
  pivot_longer(cols = c(VO_B02, starts_with("EC")),
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
# nie powinny być istotne


#### MODELOWANIE ####

# Model +/- droga od początku 
model <- lm(data = data.df,
            I(VO_B02 / mean(VO_B02)) ~
              I(TI_SEASONALITY / mean(TI_SEASONALITY)) +
              DN_adj_B02_S02_12XCN0500_2GR +
              DN_adj_B02_S02_08XCN0500_1GR +
              DN_adj_B02_S02_12XCN0500 +
              log(PR_B02_S02_04XCN0500) +  
              #log(PR_B02_S02_01XCN0500) +  
              #log(PR_B02_S02_01XNR0660) +
              #DN_adj_B02_S02_01XNR0660 +
              #DN_adj_B02_S01_04XCN0500 +
              #DN_B02 +
              #NS +
              #TI_H_NEW_YEAR +
              TI_H_MAY +
              #TI_H_XMAS +
              #TI_H_XMAS_BEFORE +
              #TI_H_XMAS_BEFORE2 +
              #TI_H_ASSUM_OF_MARY +
              #TI_H_CORPUS_CHRISTI +
              #TI_H_EASTER_MONDAY +
              TI_H_EASTER_SUNDAY +
              #TI_H_EASTER_SUNDAY_BEFORE +
              #TI_H_EPIPHANY +
              #TI_H_HALLOWEEN +
              TI_H_HALLOWEEN_BEFORE +
              TI_H_PENTECOST +
              I(TI_TEM_AVG - TI_TEM_AVG_NORM) +
              I(EX_NU_B02 / mean(EX_NU_B02)) +
              #DN_B01_S01_12XCN0500_2GR +
              #I(EX_NU_B01 / mean(EX_NU_B01)) +
              #EV_FOOTBALL_WORLD_CUP +
              #EV_WINTER_OLYMPIC +
              #TV00_B02 +
              #TV90_B02 +
              #TV50_B02 +
              #TV50_B02_2010_H2 +
              #TV50_B02_2011_H2 +
              #TV50_B02_2010_H1 +
              TV50_B02_2011_H1
              #OH00_B02 +
              #OH90_B02 +
              #OH90_B02_2011_H2 +         
              #OH20_B02 +
              #RA00_B02 +
              #TV50_B01 +
              #EC_CPI_MA5 +
              #EC_PCE_MA5 +
              #EC_UNE_MA5 +
)

summary(model)
vif(model)
 
# jarque.bera.test(model$residuals)
# bptest(model)
# bgtest(model)

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

#   - odchylenie temperatury w miarę wchodzi, dobry znak ale pval 20%

#   - ekspozycja dobrze wchodzi zarówno jako numeryczna jak i udział, w obu 
#       przypadkach dobry znak, ładnie koryguje pval dla świąt, odchylenia
#       temperatury i dystrybucji. Na podstawie analizy graficznej zostawiam
#       wersję numeryczną, i na podstawie jej wpływu na model do usunięcia 
#       będą DN_B02, Poniedziałek Wielkanocny i Boże Ciało. Ogólnie weszło
#       na tyle dobrze, że aż sprawdziłem czy nie poprawiło tych zmiennych
#       które nie wchodziły wcześniej, zwłaszcza cen i dystrybucji, ale nie.
#       R^2 91,5%

#   - SKU konkurecji DN_B01_S01_12XCN0500_2GR nie wchodzi, dobry znak ale pval
#       76%, jako że ogólnie SKU konkurencji pomijam to wyrzucam z modelu

#   - ekspozycja konkurencji EX_NU_B01 ma pval 21% i i tak bardzo mały para-
#       metr, wyrzucam jak wyżej

#   - Mundial nie wchodzi, dobry znak ale pval 50%

#   - Igrzyska Zimowe też nie wchodzą, zły znak i pval 90%

#   - całe TV nie wchodzi, pval bardzo wysokie i parametr praktycznie 0,
#     z rozbitych na półrocza wszystkie oprócz pierwszego półrocza 2011
#     TV50_B02_2011_H1 też, a ono też ma pval 15%, więc biorąc pod uwagę
#     wykresy może być do wyrzucenia

# - przy outdorze jest ciekawa, a w zasadzie to nieciekawa sytuacja, bo
#     wraz ze wzrostem AdStocku zmniejsza się pval, do tego stopnia, że
#     przy OH00 jest 93%, przy OH70 34%, a przy oH90 10%, więc pod kątem
#     pval można by już rozważyć zostawienie. Tylko że jest zły znak, bo 
#     jest ujemny, a spodziewać by się można raczej, że wzrost wydatków
#     będzie przekładał się na wzrost sprzedaży. Dodatkowo wprowadzenie
#     OH90 poprawia zachowanie TV50_B02_2011_H1. Rozbicie OH90 nie pomaga,
#     jeśli znak się nie zmieni to trzeba będzie wyrzucić

# - radio trochę podobnie i odwrotnie jak outdoor. Ogólnie do AdStocka 60%
#     ma niskie pval i robi OH90 nieistotne, ale ma zły znak (ujemny). Z
#     drugiej strony na AdStocku 0% zmienia znak OH20 na poprawny, ale pval
#     jest 33%, a z kolei zwiększa się pval na TV50_B02_2011_H1. Więc z 
#     jednej strony można by zostawić radio bo poprawia OH, ale to by było 
#     na siłę według mnie, więc jeśli makro czegoś bardzo nie zmieni to z 
#     mediów zostawiłbym tylko TV50_B02_2011_H1 

# - TV konkurencji nie wchodzi, pval 67%

# - makroekonomia nieistotna
rm(model)

#### WALIDACJA MODELU ####

# Specyfikacja po modelowaniu
model_bef_wal <- lm(data = data.df,
            I(VO_B02 / mean(VO_B02)) ~
              I(TI_SEASONALITY / mean(TI_SEASONALITY)) +
              DN_adj_B02_S02_12XCN0500_2GR +
              DN_adj_B02_S02_08XCN0500_1GR +
              DN_adj_B02_S02_12XCN0500 +
              log(PR_B02_S02_04XCN0500) +  
              TI_H_MAY +
              TI_H_EASTER_SUNDAY +
              TI_H_HALLOWEEN_BEFORE +
              TI_H_PENTECOST +
              I(TI_TEM_AVG - TI_TEM_AVG_NORM) +
              I(EX_NU_B02 / mean(EX_NU_B02)) +
              TV50_B02_2011_H1)

summary(model_bef_wal)
vif(model_bef_wal)

# Postać i podstawowe statystyki modelu

# Uzyskany model osiągnął R^2 91,7%, skorygowane R^2 90,6%, brak współlinio-
#   wości. Zmienną objaśnianą był wolumen sprzedaży marki jako indeks (podzie-
#   lony przez średnią), a jako zmienne objaśniające uwzględnione zostały:
#   sezonowość (indeks), dystrybucje (jako inkrementalność) in-outów
#   B02_S02_12XCN0500_2GR, B02_S02_08XCN0500_1GR i B02_S02_12XCN0500, logarytm
#   ceny stałego SKU B02_S02_04XCN0500, majówka, Niedziela Wielkanocna, ty-
#   dzień przed Halloween, Zielone Świątki, odchylenie temperatury od normy,
#   ekspozycja numeryczna marki (indeks) oraz intensywność reklamy w TV w 
#   pierwszym półroczu 2011 z AdStockiem na poziomie 50%.

# Walidacja biznesowa

# Wszystkie parametry mają sens biznesowy, zarówno pod kątem znaku jak i wiel-
#   kości, z wyjątkiem ceny, dla której skorygowana elastyczność wynosi ok. 
#   -7,6%, jednak jest to odchylenie na tyle małe, że jest akceptowalne, a poza
#   tym może wynikać z danych

# Walidacja statystyczna

# Reszty

jarque.bera.test(model_bef_wal$residuals)
# brak normalności reszt

# Wykres reszt
p <-  plot_ly(data.df,
              type = "scatter",
              mode = 'lines',
              x = ~Date,
              y = ~model_bef_wal$residuals,
              name = "Residuals")
htmlwidgets::saveWidget(p, "wykres.html", selfcontained = TRUE)
browseURL("wykres.html")
# widać kilka większych outlierów. Wymieniam od największych: 02.05.2011,
#   28.03.2011, 03.10.2011, 27.06.2011, 29.08.2011, 05.04.2010

# Kandydatem na 02.05.2011 jest B02_S02_12XCN0500_2GR, jest wtedy w trakcie
#   piku (widać na wykresie z cenami, wolumenami i dystrybucją z analizy graf-
#   icznej). Można tam też zobaczyć, że pewnie chodzi o cenę

# Wykres wolumenu i ceny B02_S02_12XCN0500_2GR
p <-  plot_ly(data.df,
        type = "scatter",
        mode = 'lines',
        x = ~Date,
        y = ~VO_B02_S02_12XCN0500_2GR,
        name = "VO_B02_S02_12XCN0500_2GR") %>%
  add_trace(
    mode = 'lines',
    yaxis = "y2",
    x = ~Date,
    y = ~PR_B02_S02_12XCN0500_2GR,
    name = "PR_B02_S02_12XCN0500_2GR") %>%
  layout(yaxis  = list(range = c(0, max(data.df$VO_B02_S02_12XCN0500_2GR))),
         yaxis2 = list(overlaying = "y",
                       side = "right"))
htmlwidgets::saveWidget(p, "wykres.html", selfcontained = TRUE)
browseURL("wykres.html")
# widać, że faktycznie była wtedy mocna obniżka ceny

# Wstawiamy wzmocnienie działania

model <- lm(data = data.df,
            I(VO_B02 / mean(VO_B02)) ~
              I(TI_SEASONALITY / mean(TI_SEASONALITY)) +
              DN_adj_B02_S02_12XCN0500_2GR +
              DN_adj_B02_S02_08XCN0500_1GR +
              DN_adj_B02_S02_12XCN0500 +
              log(PR_B02_S02_04XCN0500) +  
              TI_H_MAY +
              TI_H_EASTER_SUNDAY +
              TI_H_HALLOWEEN_BEFORE +
              TI_H_PENTECOST +
              I(TI_TEM_AVG - TI_TEM_AVG_NORM) +
              I(EX_NU_B02 / mean(EX_NU_B02)) +
              #TV50_B02_2011_H1 +
              #log(PR_B02_S02_12XCN0500_2GR)
              I(TI_X_2011_05_02 * DN_adj_B02_S02_12XCN0500_2GR)
)

summary(model)
vif(model)

# - na początku spóbowałem wstawić po prostu cenę w logu bo nie była uwzglę-
#     dniona w modelu, ale zaburzyła ona inne oszacowania, i wyszła też współ-
#     liniowość między nią a dystrybucją, więc zamiast tego wstawiam samą in-
#     terakcję, nie jestem pewien w sumie z czym przemnożyć dzień, bo i dys-
#     trybucja w obu formach i cena działają, a w zasadzie cokolwiek działa bo
#     chodzi przecież o efekt w jednym dniu. Na razie zostawiam inkremental-
#     ność bo raczej się nie wstawia interakcji jak zmiennej nie ma samej i do
#     doprecyzowania najwyżej. Niezależnie od tego co się wstawi, bardzo ład-
#     nie wchodzi, jedyne co to podwyższa pval TV50_B02_2011_H1 na 93%, więc
#     nie ma już za bardzo wyboru i to wyrzucam

jarque.bera.test(model$residuals)
# normalności reszt bardzo poprawiona, pval 9%, więc zależnie od poziomu
#   istotności mogłoby nawet przejść, kwestia teraz czy lepiej dalej pop-
#   rawiać i oddawać stopnie swobody czy to jest wystarczające

# Wykres reszt
p <-  plot_ly(data.df,
              type = "scatter",
              mode = 'lines',
              x = ~Date,
              y = ~model$residuals,
              name = "Residuals")
htmlwidgets::saveWidget(p, "wykres.html", selfcontained = TRUE)
browseURL("wykres.html")
# zostały od największych: 28.03.2011, 03.10.2011, 27.06.2011, 29.08.2011,
#   05.04.2010

# Kandydata na 28.03.2011 na szybko nie widzę, ale nawinął się na 03.10.2011
#   a mają podobną wielkość. Kandydatem na 03.10.2011 jest B02_S02_04XCN0500
#   przez dystrybucję

# Wykres wolumenu i dystrybucji B02_S02_04XCN0500
p <-  plot_ly(data.df,
              type = "scatter",
              mode = 'lines',
              x = ~Date,
              y = ~VO_B02_S02_04XCN0500,
              name = "VO_B02_S02_04XCN0500") %>%
  add_trace(
    mode = 'lines',
    yaxis = "y2",
    x = ~Date,
    y = ~DN_B02_S02_04XCN0500,
    name = "DN_B02_S02_04XCN0500") %>%
  layout(yaxis  = list(range = c(0, max(data.df$VO_B02_S02_12XCN0500_2GR))),
         yaxis2 = list(overlaying = "y",
                       side = "right"))
htmlwidgets::saveWidget(p, "wykres.html", selfcontained = TRUE)
browseURL("wykres.html")
# widać, że faktycznie był wtedy mocny spadek dystrybucji

# Wstawiamy wzmocnienie działania

model <- lm(data = data.df,
            I(VO_B02 / mean(VO_B02)) ~
              I(TI_SEASONALITY / mean(TI_SEASONALITY)) +
              DN_adj_B02_S02_12XCN0500_2GR +
              DN_adj_B02_S02_08XCN0500_1GR +
              DN_adj_B02_S02_12XCN0500 +
              log(PR_B02_S02_04XCN0500) +  
              TI_H_MAY +
              TI_H_EASTER_SUNDAY +
              TI_H_HALLOWEEN_BEFORE +
              TI_H_PENTECOST +
              I(TI_TEM_AVG - TI_TEM_AVG_NORM) +
              I(EX_NU_B02 / mean(EX_NU_B02)) +
              #TV50_B02_2011_H1 +
              #log(PR_B02_S02_12XCN0500_2GR)
              I(TI_X_2011_05_02 * DN_adj_B02_S02_12XCN0500_2GR) +
              I(TI_X_2011_10_03 * DN_B02_S02_04XCN0500)
)

summary(model)
vif(model)

# - ładnie wchodzi

jarque.bera.test(model$residuals)
# normalności reszt dalej przechodzi, pval 6%, ale niższe niż wcześniej,
#   R^2 poprawia trochę ale może już podchodzić pod overfitting, więc nie
#   ma co tego uwzględniać

# Wykres reszt
p <-  plot_ly(data.df,
              type = "scatter",
              mode = 'lines',
              x = ~Date,
              y = ~model$residuals,
              name = "Residuals")
htmlwidgets::saveWidget(p, "wykres.html", selfcontained = TRUE)
browseURL("wykres.html")
# zostały od największych: 28.03.2011, 27.06.2011, 29.08.2011, 05.04.2010
#   wygląda chyba faktycznie mniej normalnie niż poprzedni
rm(model)

# Specyfikacja modelu z normalnymi resztami
model_norm <- lm(data = data.df,
            I(VO_B02 / mean(VO_B02)) ~
              I(TI_SEASONALITY / mean(TI_SEASONALITY)) +
              DN_adj_B02_S02_12XCN0500_2GR +
              DN_adj_B02_S02_08XCN0500_1GR +
              DN_adj_B02_S02_12XCN0500 +
              log(PR_B02_S02_04XCN0500) +  
              TI_H_MAY +
              TI_H_EASTER_SUNDAY +
              TI_H_HALLOWEEN_BEFORE +
              TI_H_PENTECOST +
              I(TI_TEM_AVG - TI_TEM_AVG_NORM) +
              I(EX_NU_B02 / mean(EX_NU_B02)) +
              I(TI_X_2011_05_02 * DN_adj_B02_S02_12XCN0500_2GR)
)

summary(model_norm)
vif(model_norm)
jarque.bera.test(model_norm$residuals)

# - aby uzyskać normalność reszt usunięty został ostatecznie 1 outlier,  
#     02.05.2011. Przyczyną odchylenia była najpewniej promocja cenowa
#     na in-oucie B02_S02_12XCN0500_2GR. Test Jarque-Bera daje dla modelu
#     pval 9%. R^2 wynosi 93,4%, skorygowane 92,5%

# Liniowość formy funkcyjnej

resettest(model_norm, power = 2:3, type = "fitted")
# RESET nie przechodzi, więc występują jakieś nieliniowości, ale się tym nie
#   przejmujemy

# Homoskedastyczność reszt i autokorelacja

bptest(model_norm)
# występuje heteroskedastyczność

bgtest(model_norm)
# występuje autokorelacja

# Wobec tego używamy macierzy odpornej
coeftest(model_norm, vcov. = vcovHAC(model_norm))
model_hac <- coeftest(model_norm, vcov. = vcovHAC(model_norm))

# Porównanie modeli przed walidacją, z normalnymi resztami i z macierza odporną
stargazer(model_bef_wal, model_norm, model_hac,
          type = "text", align = TRUE, style = "default", df = FALSE)

# Model końcowy (ostatecznie)
model_hac

# - model końcowy nie rózni się mocno od modelu uzyskanego podczas procesu mo-
#     delowania, więc praktycznie wszystkie wnioski zostają. Zaszły warte od-
#     notowania zmiany: 
#     - inkrementalność B02_S02_12XCN0500 wyniosła 120%, czyli więcej niż kla-
#         syczny przedział <0; 100%>, ale patrząc na wykres wolumenów to mo-
#         nawet tak być, bo to był jeden bardzo mocny strzał w momencie jak
#         jak akurat większość spadała. A nawet jeśli nie, to może to wynikać
#         danych czy coś, więc ogólnie wydaje się i tak jak najbardziej ok
#     - majówka wyszła poza klasyczny próg 5%, jej pval wynosi 18%. Jednak
#         oszacowanie parametru jest ok, i ma sens marketingowy, więc zostawiam
#     - TV50_B02_2011_H1 weszło z pval na 80%, więc było nie do uratowania i 
#         zostało wyrzucone
#   R^2 w modelu końcowym wyniosło 93,4%, a skorygowane R^2 92,6%


# Dopiero przy ponownym oglądaniu wykresu sezonowości w ramach dekompozycji
#   jakoś sobie uświadomiłem, że majówka i halloween są stałe w czasie, więc
#   powinny być uwzględnione już w sezonowości. To by też mogło tłumaczyć, 
#   dlaczego przy modelu końcowym z macierzą odporną majówka wchodzi na w 
#   miarę wysokie pval 18%. Z kolei po usunięciu majówki halloween też staje 
#   się nieistotne. Można by więc rozważyć ich usunięcie z modelu. Usunięcie
#   to praktycznie nie wpływa jednak  na pozostałe parametry, ani nie sprawia,
#   żeby zaczęły wchodzić media (co by mogło oddać). Dodatkowo po ich usu-
#   nięciu reszty przestają być normalne. Zatem, biorąc pod uwagę, że usunię-
#   cie majówki i halloween wiązałoby się z koniecznością powrotu do poprzed-
#   nich etapów (głównie resztowych, ale też ewentualnie ponownego sprawdze-
#   nia części zmiennych), a ich obecność w modelu ma sens biznesowy, a także
#   biorąc pod uwagę, że inne stałe święta (Boże Narodzenie, Nowy Rok) nie 
#   były istotne, to zostawiam model w takiej postaci w jakiej jest (ogólnie
#   wydaje mi się, że generalnie nie powinny być istotne będąc uchwycone w
#   sezonowości, więc może to sezonowość jest nieprecyzyjna, ale nie ma czasu
#   na takie rozkminy)
# Czyli pomimo chwili zawahania model zostaje bez zmian


#### DEKOMPOZYCJA MODELU ####

# Jako, że coeftest nie tworzy obiektu, do dekompozycji używam model_norm

options('max.print' = 10000)
getOption('max.print')

# Wybranie zmiennych i parametrów

model_norm$model

variables.df <- model_norm$model %>%
  as_tibble() %>%
  select(-1) %>%
  mutate(Date = data.df$Date, '(Intercept)' = 1) %>%
  select(Date, '(Intercept)', everything())

# Przemnożenie parametrów przez średnią sprzedaż
coeffs <- model_norm$coefficients * mean(data.df$VO_B02)

## WYBÓR POZIOMÓW BAZOWYCH ##

ref.lev <- rep(0, 13)
names(ref.lev) <- colnames(variables.df[-1])
ref.lev

# Czynniki bazowe

# Wykres indeksu sezonowości I(TI_SEASONALITY / mean(TI_SEASONALITY))
plot(x = variables.df$Date,
     y = variables.df$`I(TI_SEASONALITY/mean(TI_SEASONALITY))`,
     type = "l")
# nie ma nic co było by szczególnie warte uchwycenia

# Po prostu średnia, a jako że mamy zmienną już jako indeks, to jako bazę naj-
#   lepiej wziąć 1, i wtedy to będzie właśnie średni poziom sezonowości (śre-
#   dnia z tej zmiennej policzona ręcznie też wyniesie zawsze właśnie 1)
ref.lev['I(TI_SEASONALITY/mean(TI_SEASONALITY))'] <- 1
#ref.lev['I(TI_SEASONALITY/mean(TI_SEASONALITY))'] <- mean(
#  variables.df$`I(TI_SEASONALITY/mean(TI_SEASONALITY))`)
#ref.lev['I(TI_SEASONALITY/mean(TI_SEASONALITY))']

# Wykres ceny log(PR_B02_S02_04XCN0500) 
plot(x = variables.df$Date,
     y = variables.df$`log(PR_B02_S02_04XCN0500)`,
     type = "l")
# na początku była w miarę stała, potem zwiększyła poziom, potem znów, potem
#   bardzo mocna chwilowa promocja i potem powrót na jeszcze wyższy poziom, a 
#   w międzyczasie pomniejsze promocje. Warte wyłapania są na pewno zmiany
#   poziomów i ta największa promocja, ale trochę się to wyklucza bo będzie 
#   mniejsza z perspektywy początkowej ceny a tej po dwóch podwyżkach poziomu.
#   Jako że to jest stały SKU, stanowiący absolutną podstawę sprzedaży, to 
#   myślę, że z tych dwóch rzeczy istotniejsze są jednak zmiany poziomów

# Zgodnie z rozumowaniem powyżej, najlepszym poziomem będzie średnia z początku
#   okresu, tam gdzie cena była w miarę stała, czyli przed pierwszą zmianą
#   poziomu, czyli powiedzmy od początku do 05.04.2010
ref.lev['log(PR_B02_S02_04XCN0500)'] <- mean(
  variables.df$`log(PR_B02_S02_04XCN0500)`[1:14])

# Wykres odchylenia temperatury I(TI_TEM_AVG - TI_TEM_AVG_NORM)
plot(x = variables.df$Date,
     y = variables.df$`I(TI_TEM_AVG - TI_TEM_AVG_NORM)`,
     type = "l")
# nie ma nic szczególnie wartego uchwycenia

# Najlepiej chyba dobrać poziom 0, czyli normę temperatury, ewentualnie średnią
#   ale ona będzie raczej gorzej interpretowalna (a i tak co do zasady powin-
#   też wyjść w okolicy 0)
ref.lev['I(TI_TEM_AVG - TI_TEM_AVG_NORM)'] <- 0
#ref.lev['I(TI_TEM_AVG - TI_TEM_AVG_NORM)'] <- mean(
#  variables.df$`I(TI_TEM_AVG - TI_TEM_AVG_NORM)`)
#ref.lev['I(TI_TEM_AVG - TI_TEM_AVG_NORM)']

# Dla świąt kalendarzowych poziom bazowy to 0
ref.lev['TI_H_MAY'] <- 0
ref.lev['TI_H_EASTER_SUNDAY'] <- 0
ref.lev['TI_H_HALLOWEEN_BEFORE'] <- 0
ref.lev['TI_H_PENTECOST'] <- 0

# Czynniki inkrementalne 

# Dla czynników inkrementalnych poziom bazowy to 0
ref.lev['DN_adj_B02_S02_12XCN0500_2GR'] <- 0
ref.lev['DN_adj_B02_S02_08XCN0500_1GR'] <- 0
ref.lev['DN_adj_B02_S02_12XCN0500'] <- 0
ref.lev['I(EX_NU_B02/mean(EX_NU_B02))'] <- 0
ref.lev['I(TI_X_2011_05_02 * DN_adj_B02_S02_12XCN0500_2GR)'] <- 0

# Przejście na data frame
ref.lev.df <- data.frame(variables = names(ref.lev),
                         ref.lev = ref.lev)

coeffs.df <- data.frame(variables = names(coeffs),
                        coeffs = coeffs)

# Odjęcie poziomów bazowych i policzenie wpływów inkrementalnych
variables.debased.df <- variables.df %>%
  pivot_longer(-Date, names_to = 'variables', values_to = 'value') %>%
  left_join(ref.lev.df) %>%
  mutate(value.debased = value - ref.lev) %>%
  left_join(coeffs.df) %>%
  mutate(value.debased = value.debased * coeffs)

# Dodanie poziomów bazowych do stałej

base.levels.df <- variables.debased.df %>%
  mutate(base.levels.sum = coeffs * ref.lev) %>%
  group_by(Date) %>%
  summarise('(Intercept)' = sum(base.levels.sum)) %>%
  pivot_longer(-Date, names_to = 'variables', values_to = 'base.levels')

decomp.final.df <- variables.debased.df %>%
  left_join(base.levels.df) %>%
  mutate(base.levels = ifelse(is.na(base.levels), 0, base.levels)) %>%
  mutate(value.final = value.debased + base.levels) %>%
  select(Date, variables, value.final)

# Sprawdzenie czy suma czynników zgadza się z wartością fitted z modelu
check.df <- decomp.final.df %>%
  group_by(Date) %>%
  summarise(value.final = sum(value.final)) %>%
  bind_cols(
    fitted = model_norm$fitted.values * mean(data.df$VO_B02)) %>%
  mutate(check = value.final - fitted)
sum(check.df$check)

# Przygotowanie sobie ramki danych do analizy

decomp.final.df <- decomp.final.df %>%
  pivot_wider(names_from = variables, values_from = value.final) %>%
  mutate(fitted_decomp = rowSums(select(., -Date)),
         fitted_model = model_norm$fitted.values * mean(data.df$VO_B02))

data.final.df <- data.df %>%
  select(Date, VO_B02) %>%
  left_join(decomp.final.df) %>% 
  mutate(error_model = VO_B02 - fitted_model)

write.csv2(data.final.df, 'decomp_data_final.csv', row.names = F)

## ANALIZY GRAFICZNE ##

# Wykres wartości rzeczywistych vs fitted (policzonych na dwa sposoby żeby
#   upewnić się co do dekompozycji) i reszt
p <-  plot_ly(data.final.df,
              type = "scatter",
              mode = 'lines',
              x = ~Date,
              y = ~VO_B02,
              name = "VO_B02") %>%
  add_trace(
    type = "scatter",
    mode = 'lines',
    x = ~Date,
    y = ~fitted_decomp,
    name = "fitted_decomp"
  ) %>%
  add_trace(
    type = "scatter",
    mode = 'lines',
    x = ~Date,
    y = ~fitted_model,
    name = "fitted_model"
  ) %>%
  add_trace(
    type = "scatter",
    mode = 'lines',
    x = ~Date,
    y = ~error_model,
    name = "error_model"
  )
htmlwidgets::saveWidget(p, "wykres.html", selfcontained = TRUE)
browseURL("wykres.html")

# Wykres dekompozycyjny

decomp.long.df <- data.final.df %>%
  select(-c(VO_B02, fitted_decomp, fitted_model)) %>%
  pivot_longer(
    cols = -Date,
    names_to = "variable",
    values_to = "contribution"
  )

decomp.long.df$variable <- dplyr::recode(
  decomp.long.df$variable,
  "(Intercept)" = "Sprzedaż bazowa",
  "I(TI_SEASONALITY/mean(TI_SEASONALITY))" = "Sezonowość",
  "DN_adj_B02_S02_12XCN0500_2GR" = "Dystrybucja B02_S02_12XCN0500_2GR",
  "DN_adj_B02_S02_08XCN0500_1GR" = "Dystrybucja B02_S02_08XCN0500_1GR",
  "DN_adj_B02_S02_12XCN0500" = "Dystrybucja B02_S02_12XCN0500",
  "log(PR_B02_S02_04XCN0500)" = "Cena B02_S02_04XCN0500",
  "TI_H_MAY" = "Majówka",
  "TI_H_EASTER_SUNDAY" = "Niedziela Wielkanocna",
  "TI_H_HALLOWEEN_BEFORE" = "tydzień przed Halloween",
  "TI_H_PENTECOST" = "Pentecoste",
  "I(TI_TEM_AVG - TI_TEM_AVG_NORM)" = "Odchylenie temperatury",
  "I(EX_NU_B02/mean(EX_NU_B02))" = "Ekspozycja",
  "I(TI_X_2011_05_02 * DN_adj_B02_S02_12XCN0500_2GR)" = "02.05.2011",
  "error_model" = "Błąd modelu"
)

decomp.long.df$variable <- factor(
  decomp.long.df$variable,
  levels = c(
    "Sprzedaż bazowa",
    "Cena B02_S02_04XCN0500",
    "Ekspozycja",
    "Sezonowość",
    "Odchylenie temperatury",    
    "Dystrybucja B02_S02_12XCN0500_2GR",
    "Dystrybucja B02_S02_08XCN0500_1GR",
    "Dystrybucja B02_S02_12XCN0500",
    "Majówka",
    "Niedziela Wielkanocna",
    "tydzień przed Halloween",
    "Pentecoste",
    "02.05.2011",
    "Błąd modelu"
  )
)

vars <- unique(decomp.long.df$variable)

cols <- c(
  "Sprzedaż oszacowana"                               = "#9467BD",  
  "Sprzedaż rzeczywista"                              = "#f55ff5",
  "Błąd modelu"                                       = "#D62728",
  "Sprzedaż bazowa"                                   = "#889094",
  "Sezonowość"                                        = "#051b4f",
  "Dystrybucja B02_S02_12XCN0500_2GR"                 = "#1F77B4",
  "Dystrybucja B02_S02_08XCN0500_1GR"                 = "#42c8f5",
  "Dystrybucja B02_S02_12XCN0500"                     = "#7fadc9",
  "Cena B02_S02_04XCN0500"                            = "#F28E2B",
  "Majówka"                                           = "#f0e007",
  "Niedziela Wielkanocna"                             = "#c7bb14",
  "tydzień przed Halloween"                           = "#9c942a",
  "Pentecoste"                                        = "#706b26",
  "Odchylenie temperatury"                            = "#17594a",
  "Ekspozycja"                                        = "#42f5c5",
  "02.05.2011"                                        = "#FFBB78"
)
setdiff(vars, names(cols))
scales::show_col(cols)

p <-  plot_ly(colors = cols) %>%
  add_trace(
    data = data.final.df,
    type = "scatter",
    mode = 'lines',
    x = ~Date,
    y = ~fitted_model,
    name = "Sprzedaż oszacowana",
    line = list(color = cols["Sprzedaż oszacowana"],
                width = 4,
                dash = "dashdot")
  ) %>%
  add_trace(
    data = data.final.df,
    type = "scatter",
    mode = 'lines',
    x = ~Date,
    y = ~VO_B02,
    name = "Sprzedaż rzeczywista",
    line = list(color = cols["Sprzedaż rzeczywista"],
                width = 6)
  ) %>%
  add_bars(
    data = decomp.long.df,
    x = ~Date,
    y = ~contribution,
    color = ~variable,
    hovertemplate = paste(
      "Date: %{x}<br>",
      "Variable: %{fullData.name}<br>",
      "Contribution: %{y}<extra></extra>"
    )
  ) %>%
  layout(barmode = "relative",
         xaxis = list(
           title = "",
           showgrid = FALSE
         ),
         yaxis = list(
           title = "Wolumen",
           showgrid = FALSE,
           zeroline = TRUE,
           zerolinecolor = "black"
         ))
htmlwidgets::saveWidget(p, "wykres.html", selfcontained = TRUE)
browseURL("wykres.html")

# Stackbary / kaskadowe statyczne

# Przygotowanie ramki danych

# Wartości bezwzględne zagregowane po latach
stackbars.abs.df <- data.final.df %>%
  mutate(Date = as.character(year(Date))) %>%
  select(-c(fitted_decomp, fitted_model)) %>%
  group_by(Date) %>%
  summarise(
    across(where(is.numeric), ~ sum(.x)
  )) %>%
  ungroup()

# Dołączenie sumy w całym okresie
total_sum <- stackbars.abs.df %>%
  summarise(
    Date = "Total",
    across(-Date, ~ sum(.x))
  )
stackbars.abs.df <- stackbars.abs.df %>%
  bind_rows(total_sum)

# Policzenie wartości procentowych
stackbars.pct.df <- stackbars.abs.df %>%
  mutate(
    across(
      -Date,
      ~ .x / VO_B02
    ),
    Date = paste0(Date, "_pct")
  )

# Ramka z wartościami bezwzględnymi i procentowymi
stackbars.all.df <- bind_rows(
  stackbars.abs.df,
  stackbars.pct.df
)

stackbars.all.df

# Ostateczne sprawdzenie czy się dodaje
stackbars.all.df %>%
  mutate(
    check = rowSums(across(-c(Date, VO_B02))) - VO_B02
  ) %>%
  select(Date, VO_B02, check)
# jest ok

# Do wykresów potrzeba tylko wartości procentowych, i w sumie jest trochę wpły-
#   wów ujemnych, więc zwykły stackbar może nie pójść, czyli trzeba zrobić wa-
#   terfall

# Ramki do wykresów kaskadowych statycznych
waterfall.static.df <- stackbars.pct.df %>%
  select(-VO_B02) %>%
  pivot_longer(
    cols = -Date
  )
# widać, że duża część ma kontrybucję praktycznie 0, więc nie ma sensu pokazy-
#   wać wszystkich (a chciałem na początku od tego zacząć), tylko lepiej od 
#   razu pogrupować, wyrzucić grupy nieistotne grupy, pokazać wpływy grup i 
#   ewentualnie potem rozbijać grupy

# Grupuję razem dystrybucję in-outów i święta kalendarzowe.
#   Po przejrzeniu od razu grupuje też czynniki mało istotne do grupy ,,inne''
#   (znajdą się w niej sezonowość, temperatura i interakcja)
waterfall.static.groups.df <- waterfall.static.df %>%
  mutate(
    group = case_when(
      grepl("^DN", name) ~ "Dystrybucja in-outów",
      grepl("^TI_H", name) ~ "Święta kalendarzowe",
      grepl("TI_SEA|TI_TEM|TI_X_", name) ~ "Inne czynniki",
      grepl("Intercept", name) ~ "Sprzedaż bazowa",
      grepl("EX_NU", name) ~ "Ekspozycje",
      grepl("PR_", name) ~ "Cena 04XCN0500",
      TRUE ~ name
    )
  ) %>%
  group_by(Date, group) %>%
  summarise(
    value = sum(value),
    .groups = "drop"
  ) %>%
  arrange(desc(value))

# Zaczynam od totala i ciekawsze rzeczy się najwyżej rozbije
#   Reszty w totalu do wyrzucenia, z lat sie sumują do 0
waterfall.static.total.df <- waterfall.static.groups.df %>%
  filter(Date == "Total_pct",
         group != "error_model") %>%
  bind_rows(data.frame(Date = "Total_pct", 
               group = "Sprzedaż całkowita",
               value = 1))

# Wykresy kaskadowe statyczne

# Wykres statyczny total

waterfall.static.total.plot.df <- waterfall.static.total.df %>%
  mutate(
    end = cumsum(value),
    start = lag(end, default = 0),
    
    start = if_else(group == "Sprzedaż całkowita", 0, start),
    end = if_else(group == "Sprzedaż całkowita", value, end),
    
    ymin = pmin(start, end),
    ymax = pmax(start, end),
    
    id = row_number(),
    
    fill = case_when(
      group %in% c("Sprzedaż bazowa", "Sprzedaż całkowita") ~ "bases",
      value >= 0 ~ "positive",
      value < 0 ~ "negative")
  )

waterfall.static.total.plot.df <- waterfall.static.total.plot.df %>%
  mutate(
    label = percent(value, accuracy = 0.1),
    label_color = if_else(group %in% c("Sprzedaż całkowita"), "base", "main")
  )

ggplot(waterfall.static.total.plot.df, aes(x = id)) +
  geom_rect(aes(
    xmin = id - 0.43,
    xmax = id + 0.43,
    ymin = ymin,
    ymax = ymax,
    fill = fill
  )) +
  geom_text(
    aes(
      y = ymax + 0.05,
      label = label,
      color = label_color
    ),
    size = 4.1,
    fontface = "bold"
  ) +
  scale_fill_manual(values = c(
    positive = "#42bcf5",
    negative = "#F28E2B",
    bases = "#889094"
  )) +
  scale_color_manual(values = c(
    main = "#222222",
    base = "#889094"
  )) +
  scale_x_continuous(
    breaks = waterfall.static.total.plot.df$id,
    labels = str_wrap(waterfall.static.total.plot.df$group, width = 11),
    expand = expansion(mult = c(0.04, 0.04))
  ) +
  scale_y_continuous(
    labels = percent,
    expand = expansion(mult = c(0.02, 0.09))
  ) +
  labs(x = NULL, y = NULL) +
  theme_minimal(base_size = 13) +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    axis.text.x = element_text(
      size = 11,
      color = "#4a4a4a",
      lineheight = 0.95,
      margin = margin(t = 8)
    ),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    legend.position = "none",
    plot.margin = margin(t = 12, r = 12, b = 12, l = 12)
  )
# najciekawsze są ekspozycje, dystrybucje i cena

# Ramki do wykresów dynamicznych

# Policzenie zmian procentowych z roku na rok dla zmiennych bezpośrednio jest
#   niemożliwe w niektórych przypadkach, bo niektóre zmienne w 2010 były 0

waterfall.dynamic.df <- stackbars.abs.df %>%
  filter(Date != "Total") %>%
  pivot_longer(
    cols = -Date,
    names_to = "name",
    values_to = "value"
  ) %>%
  pivot_wider(
    names_from = Date,
    values_from = value
  ) %>%
  mutate(
    change = `2011` - `2010`,
    change_y2y_pct = change / `2010`,
    change_share = change / (`2011`[name == "VO_B02"] -
                             `2010`[name == "VO_B02"]),
    change_share_pct = change_share * change_y2y_pct[1],
  ) 

sum(waterfall.dynamic.df$change_share_pct)-
  waterfall.dynamic.df$change_share_pct[1]

total_change <- waterfall.dynamic.df %>%
  filter(name == "VO_B02")
total_change$change_share_pct = total_change$change_share_pct + 1

waterfall.dynamic.df <- waterfall.dynamic.df %>%
  filter(name != "VO_B02") %>%
  bind_rows(total_change)

# Stała, sezonowość i święta kalendarzowe nic nie wnoszą
waterfall.dynamic.df <- waterfall.dynamic.df %>%
  filter(!grepl("Inter", name),
         !grepl("SEA", name),
         !grepl("^TI_H", name))

# Grupujemy
waterfall.dynamic.groups.df <- waterfall.dynamic.df %>%
  mutate(
    group = case_when(
      name == "VO_B02" ~ "2011",
      grepl("^DN", name) ~ "Dystrybucja in-outów",
      grepl("^TI_H", name) ~ "Święta kalendarzowe",
      grepl("TI_SEA|TI_TEM|err|05_02", name) ~ "Inne czynniki",
      grepl("Intercept", name) ~ "Sprzedaż bazowa",
      grepl("EX_NU", name) ~ "Ekspozycje",
      grepl("PR_", name) ~ "Cena 04XCN0500",
      TRUE ~ name
    )
  ) %>%
  group_by(group) %>%
  summarise(
    value = sum(change_share_pct),
    .groups = "drop"
  ) %>%
  arrange(value)

waterfall.dynamic.groups.df <- bind_rows(
  tibble(
    group = "2010",
    value = 1
  ),
  waterfall.dynamic.groups.df
)

# Wykres dynamiczny

waterfall.dynamic.plot.df <- waterfall.dynamic.groups.df %>%
  mutate(
    end = cumsum(value),
    start = lag(end, default = 0),
    
    start = if_else(group %in% c("2010", "2011"), 0, start),
    end = if_else(group %in% c("2010", "2011"), value, end),
    
    ymin = pmin(start, end),
    ymax = pmax(start, end),
    
    id = row_number(),
    
    fill = case_when(
      group %in% c("2010", "2011") ~ "bases",
      grepl("^Dyst", group) ~ "distribution",
      grepl("^Świę", group) ~ "holidays",
      grepl("^Inn", group) ~ "other",
      grepl("^Eks", group) ~ "exposition",
      grepl("^Ce", group) ~ "price"
    )
  )

waterfall.dynamic.plot.df <- waterfall.dynamic.plot.df %>%
  mutate(
    label = percent(value, accuracy = 0.1),
    label_color = if_else(group %in% c("2010"), "base", "main")
  )

ggplot(waterfall.dynamic.plot.df, aes(x = id)) +
  geom_rect(aes(
    xmin = id - 0.43,
    xmax = id + 0.43,
    ymin = ymin,
    ymax = ymax,
    fill = fill
  )) +
  geom_text(
    aes(
      y = ymax + 0.05,
      label = label,
      color = label_color
    ),
    size = 4.1,
    fontface = "bold"
  ) +
  scale_fill_manual(values = c(
    distribution = "#42bcf5",
    exposition = "#42f5f5",
    price = "#F28E2B",
    other = "#254aba",
    bases = "#889094"
  )) +
  scale_color_manual(values = c(
    main = "#222222",
    base = "#4f575a"
  )) +
  scale_x_continuous(
    breaks = waterfall.dynamic.plot.df$id,
    labels = str_wrap(waterfall.dynamic.plot.df$group, width = 11),
    expand = expansion(mult = c(0.04, 0.04))
  ) +
  scale_y_continuous(
    labels = percent,
    expand = expansion(mult = c(0.02, 0.10))
  ) +
  labs(x = NULL, y = NULL) +
  theme_minimal(base_size = 13) +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    axis.text.x = element_text(
      size = 11,
      color = "#4a4a4a",
      lineheight = 0.95,
      margin = margin(t = 8)
    ),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    legend.position = "none",
    plot.margin = margin(t = 12, r = 12, b = 12, l = 12)
  )

# Rozgrupowane
waterfall.dynamic.ungrouped.df <- waterfall.dynamic.df %>%
  mutate(
    group = case_when(
      name == "VO_B02" ~ "2011",
      name == "DN_adj_B02_S02_12XCN0500_2GR" ~ "Dystrybucja 12XCN0500 2GR",
      name == "DN_adj_B02_S02_08XCN0500_1GR" ~ "Dystrybucja 08XCN0500 1GR",
      name == "DN_adj_B02_S02_12XCN0500" ~ "Dystrybucja 12XCN0500",
      grepl("PR_", name) ~ "Cena 04XCN0500",
      grepl("EX_NU", name) ~ "Ekspozycje",
      grepl("TI_TEM", name) ~ "Odchylenie temperatury",
      grepl("05_02", name) ~ "Interakcja 02.05",
      name == "error_model" ~ "Błąd modelu",
      TRUE ~ name
    )
  ) %>%
  transmute(
    group,
    value = change_share_pct
  ) %>%
  arrange(value)

waterfall.dynamic.ungrouped.df <- bind_rows(
  tibble(group = "2010", value = 1),
  waterfall.dynamic.ungrouped.df
)

# Wykres dynamiczny

waterfall.dynamic.plot.df <- waterfall.dynamic.ungrouped.df %>%
  mutate(
    end = cumsum(value),
    start = lag(end, default = 0),
    start = if_else(group %in% c("2010", "2011"), 0, start),
    end = if_else(group %in% c("2010", "2011"), value, end),
    ymin = pmin(start, end),
    ymax = pmax(start, end),
    id = row_number(),
    fill = case_when(
      group %in% c("2010", "2011") ~ "base",
      grepl("Dystrybucja", group) ~ "distribution",
      grepl("Ekspozyc", group) ~ "exposition",
      grepl("Cena", group) ~ "price",
      grepl("temperatury|Interakcja|Błąd", group) ~ "other"
    )
  )

waterfall.dynamic.plot.df <- waterfall.dynamic.plot.df %>%
  mutate(
    label = percent(value, accuracy = 0.1),
    label_color = if_else(group %in% c("2010"), "base", "main")
  )

ggplot(waterfall.dynamic.plot.df, aes(x = id)) +
  geom_rect(aes(
    xmin = id - 0.40,
    xmax = id + 0.40,
    ymin = ymin,
    ymax = ymax,
    fill = fill
  )) +
  geom_text(
    aes(
      y = ymax + 0.05,
      label = label,
      color = label_color
    ),
    size = 3.8,
    fontface = "bold"
  ) +
  scale_fill_manual(values = c(
    base = "#889094",
    distribution = "#42bcf5",
    exposition = "#42f5f5",
    price = "#F28E2B",
    other = "#254aba"
  )) +
  scale_color_manual(values = c(
    main = "#222222",
    base = "#4f575a"
  )) +
  scale_x_continuous(
    breaks = waterfall.dynamic.plot.df$id,
    labels = stringr::str_wrap(waterfall.dynamic.plot.df$group, width = 10),
    expand = expansion(mult = c(0.035, 0.035))
  ) +
  scale_y_continuous(
    labels = percent,
    expand = expansion(mult = c(0.02, 0.10))
  ) +
  labs(x = NULL, y = NULL) +
  theme_minimal(base_size = 13) +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    axis.text.x = element_text(
      size = 10,
      color = "#4a4a4a",
      lineheight = 0.92,
      margin = margin(t = 8)
    ),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    legend.position = "none",
    plot.margin = margin(t = 12, r = 12, b = 12, l = 12)
  )


#### SYNTETYCZNA ANALIZA MEDIÓW (TV) #### 

# Jako że zmienne mediowe nie weszły do modelu, to zgodnie z zaleceniami przep-
#   rowadzę analizę syntetyczną według instrukcji:
#     1. Proszę wybrać jedno medium dla marki (np. TV) i wybrać do niego jakiś 
#          poziom adstocku (np. 80%) oraz jakiś denominator. Wybór poziomu ad-
#          stocku i denominatora będzie mał w tym przypadku charakter arbitral-
#          ny (ich wartości nie będą pochodziły z modelu)
#     2. Dla tego medium proszę policzyć współczynnik beta (coefficient), tak 
#          aby ROI danego kanału mediowego (przy założonym wyżej adstocku i de-
#          nominatorze) wynosił 1.00. Taki parametr beta można policzyć ręcz-
#          nie – jest to rozwiązanie równania z jedną niewiadomą (a niewiadomą 
#          jest ten coefficient).
#     3. Do optymalizacji mediów proszę użyć tego cofficienta dla tego medium
#     4. Dla pozostałych mediów coffecienty będą wynosić 0 i w konsekwencji ROI
#          również 0
#     5. Pomimo, że zmiennych mediowych nie będzie w modelu, pozwoli to zopty-
#          malizować budżet mediowy (pomimo, że w finalnym modelu nie będzie
#          zmiennych mediowych)
#
# Wybrane medium to TV, a poziom adstocku 50%

# Denominator powinien być w przedziale od max(adstock.variable) / tan(1.5)
#   max(adstock.variable) / tan(0.6) do
max(data.df$TV50_B02) / tan(1.5)
max(data.df$TV50_B02) / tan(0.6)
# czyli +/- od 19 do 393, wybieram 100

# Parametr wyznaczamy tak, żeby ROI z całego okresu było równe 1, czyli po 
# przekształceniu wzoru dostajemy:
#                                   koszt netto medium 
#     beta =  ----------------------------------------------------------------
#              mean(VO_B02) * sum(atan(X / den)) * marża * (1 / st. pokrycia)

# Marżę, stopień pokrycia i koszty bierzemy z briefu
marza <- 1
st_pokrycia <- 0.05
TV_cost <- sum(data.df$TV00_B02) * 1000

# Obliczenie bety
beta_tv <- TV_cost /
  (mean(data.df$VO_B02) * 
     sum(atan(data.df$TV50_B02 / 100)) * marza * (1 / st_pokrycia))

# Sprawdzenie ROI
beta_tv * sum(atan(data.df$TV50_B02 / 100)) * 
  mean(data.df$VO_B02) * marza * (1 / st_pokrycia) / TV_cost

# Obliczenie wpływów tygodniowych mediów na sprzedaż (dla TV używamy wyliczonej
#   bety, dla radio i outdoru beta będzie 0 i w konsekswencji wpływ również)
data.df <- data.df %>%
  mutate(TV = beta_tv * mean(data.df$VO_B02) * atan(data.df$TV50_B02 / 100),
         RA = 0,
         OH = 0)

# Optymalizuję budżet z 2011

# Obliczenie wpływów rocznych 2011
data.2011.df <- data.df %>%
  filter(Date > "2010-12-31") 
impact.annual.2011.tv = sum(data.2011.df$TV)
impact.annual.2011.ra = sum(data.2011.df$RA)
impact.annual.2011.oh = sum(data.2011.df$OH)

# Obliczenie rocznych przychodów 2011
reve.tv <- impact.annual.2011.tv * marza / st_pokrycia
reve.ra <- impact.annual.2011.ra * marza / st_pokrycia
reve.oh <- impact.annual.2011.oh * marza / st_pokrycia

# Obliczenie kosztów rocznych 2011 (TV trzeba przeliczyć z GRP, reszta jest 
#   podana w wartościach kosztów)
cost.tv <- sum(data.2011.df$TV00_B02) * 1000
cost.ra <- sum(data.2011.df$RA00_B02)
cost.oh <- sum(data.2011.df$OH00_B02)

# Obliczenie ROI 2011 (dla RA i OH będą 0)
reve.tv / cost.tv
reve.ra / cost.ra
reve.oh / cost.oh
# radio nie było w budżecie 2011, a nie ma też wpływu,  więc nie będzie go w 
#   optymalizacji / realokacji budżetu

# Określenie liczby tygodni w których media były kupowane
weeks.tv <- nrow(data.2011.df %>% filter(TV00_B02 > 0))    
weeks.oh <- nrow(data.2011.df %>% filter(OH00_B02 > 0)) 

# Przeskalowanie denominatorów (tylko TV, bo OH i tak 0)
den.tv <- 100 * (max(data.df$TV00_B02) / max(data.df$TV50_B02))

# Rozwiązanie równania na wyznaczenie krzywej rocznej:
#
#                                      PRZYCHOD.ROCZNY
#   X = ----------------------------------------------------------------
#                              KOSZTY ROCZNE / (CPU * LICZBA TYGODNI)
#         LICZBA.TYG * ATAN( ---------------------------------------- )
#                                       DENOMINATOR_2

x.tv <- reve.tv / (weeks.tv * atan(cost.tv / (weeks.tv * 1000 * den.tv)))
x.oh <- 0

# Narysowanie krzywych dla kosztóW od 0 do 150% historycznych
cost.min <- 0
cost.max <- 1.5 * cost.tv

resp.curve <- seq(cost.min, cost.max, by = 5000)

resp.curves.df <- data.frame(Cost = resp.curve) %>%
  mutate(TV = x.tv * weeks.tv * atan(Cost / (1000 * weeks.tv * den.tv)),
         OH = 0,
         ROI.TV = TV / Cost,
         ROI.OH = OH / Cost,
         marginal.TV = TV - lag(TV),
         marginal.OH = OH - lag(OH))

resp.curves.long.df <- resp.curves.df %>%
  pivot_longer(-Cost, names_to = "Channel")

options(scipen = 8)

# Wykres krzywych rocznych
p <- ggplotly(
  ggplot(resp.curves.long.df %>% filter(Channel %in% c("TV", "OH")),
         aes(x = Cost, y = value, col = Channel)) +
    geom_line() + 
    ggtitle("Response curves: revenue vs. investment")
)
htmlwidgets::saveWidget(p, "wykres.html", selfcontained = TRUE)
browseURL("wykres.html")

# Wykres zysków krańcowych
p <- ggplotly(
  ggplot(resp.curves.long.df %>% filter(Channel %in% c("marginal.TV", 
                                                       "marginal.OH")),
         aes(x = Cost, y = value, col = Channel)) +
    geom_line() + 
    ggtitle("Marginal revenue vs. investment")
)
htmlwidgets::saveWidget(p, "wykres.html", selfcontained = TRUE)
browseURL("wykres.html")

## OPTYMALIZACJA HISTORYCZNEGO BUDŻETU ##

# Ogólnie jako że jest tylko radio i tv, a radio nic nie daje, to bez oficjal-
#   nej optymalizacji wiadmomo że trzeba po prostu władować wszystko w TV, ale
#   gdyby były ,,normalne'' wartości, to trzeba by było to policzyć

# Przy optymalizacji przyjmujemy następujące założenia:
#   - budżet całkowity jest taki jaki był
#   - liczba emitowanych tygodni jest taka, jak była
#   - na każde medium trzeba wydać min. 50% tego co było
#   - na każde medium można wydac max. 200% tego co było

# Obliczenie całkowitego budżetu
budget <- cost.tv + cost.oh

# Ustawienie ograniczeń min i max
min.cost.tv <- 0.5 * cost.tv
min.cost.oh <- 0.5 * cost.oh
max.cost.tv <- 2 * cost.tv
max.cost.oh <- 2 * cost.oh

df.plt.df <- resp.curves.df %>%
  select(Cost_TV = Cost, TV) %>%
  cross_join(resp.curves.df %>%
               select(Cost_OH = Cost, OH)) %>%
  mutate(total_reve = TV + OH)

# Mamy tylko dwa kanały, więc można zrobić taki wykres (przy >2 ciężko)
ggplot(df.plt.df,
       aes(x = Cost_TV, y = Cost_OH, z = total_reve)) +
  geom_contour_filled() + 
  geom_abline(slope = -1, intercept = cost.tv + cost.oh, 
              col = 'red', linewidth = 1) +
  annotate("text", x = 600000, y = cost.tv + cost.oh, 
           label = "ograniczenie budżetowe", col = "red") +
  ggtitle("Total revenue vs. TV and OH investment") +
  theme_minimal()

# Przy dwóch kanałach można też po prostu zrobić tabelkę i posortować po reve
df.plt.df %>%
  mutate(totcost = Cost_TV + Cost_OH) %>%
  filter(totcost >= budget - 2500,
         totcost <= budget + 2500,
         Cost_TV >= min.cost.tv,
         Cost_TV <= max.cost.tv,
         Cost_OH >= min.cost.oh,
         Cost_OH <= max.cost.oh) %>%
  View()

# Przypadku ogólnym chodzi o to żeby zrównać przychody krańcowe przy danym og-
#   raniczeniu budżetowym

# Maksymalny krańcowy przychód możliwy do osiągnięcia
max.marginal.revenue.all.media <- max(max(
  resp.curves.df$marginal.TV[2:nrow(resp.curves.df)]),
                                      max(
  resp.curves.df$marginal.OH[2:nrow(resp.curves.df)]))

# Pomocniczy data.frame
optimization.df <- resp.curves.df %>%
  filter(is.na(marginal.TV) == F) %>%
  mutate(marginal.TV.modified = ifelse(Cost < min.cost.tv, 
                                       max(max.marginal.revenue.all.media),
                                       ifelse(Cost > max.cost.tv, 0, 
                                              marginal.TV))) %>%
  mutate(marginal.OH.modified = ifelse(Cost < min.cost.oh, 
                                       max(max.marginal.revenue.all.media),
                                       ifelse(Cost > max.cost.oh, 0, 
                                              marginal.OH)))

# Wykres przychodów krańcowych uwzględniających ograniczenia
p <- ggplotly(
  ggplot(optimization.df %>%
           select(Cost, starts_with('marginal')) %>%
           pivot_longer(-Cost),
         aes(x = Cost, y = value, col = name)) +
    geom_line() +
    ggtitle("Marginal revenue vs. investment")
)
htmlwidgets::saveWidget(p, "wykres.html", selfcontained = TRUE)
browseURL("wykres.html")
# widać, że coś trzeba wrzucić w OH ale wszystko inne co się da idzie w TV

# Czyli w ramach realokacji historycznego budżetu



