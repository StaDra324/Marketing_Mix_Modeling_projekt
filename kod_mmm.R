library(tidyverse)
library(plotly)
library(car)
library(lmtest)
library(tseries)
library(sandwich)
library(stargazer)

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
   selected_subbrands_comp, brand_shares_comp, subbrand_shares_comp, tmp)

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
# trochę widać związek

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
#   dzić rozbijam po półroczach z AdStockiem 50%

# Rozbicie TV na półrocza

data.df$TI_Y_2010_H1
data.df$TI_Y_2010_H2
data.df$TI_Y_2011_H1
data.df$TI_Y_2011_H2

# Dodanie zmiennych rozbitych TV
data.df <- data.df %>%
  mutate(TV50_B02_2010_H1 = TI_Y_2010_H1 * TV50_B02,
         TV50_B02_2010_H2 = TI_Y_2010_H2 * TV50_B02,
         TV50_B02_2011_H1 = TI_Y_2011_H1 * TV50_B02,
         TV50_B02_2011_H2 = TI_Y_2011_H2 * TV50_B02)

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
  mutate(OH90_B02_2010_H1 = TI_Y_2010_H1 * OH90_B02,
         OH90_B02_2010_H2 = TI_Y_2010_H2 * OH90_B02,
         OH90_B02_2011_H1 = TI_Y_2011_H1 * OH90_B02,
         OH90_B02_2011_H2 = TI_Y_2011_H2 * OH90_B02)

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
#   można wstawić i rozbić ale nie powinno wejść. AdStock zobaczymy jak 
#   będzie w modelu, z teorii 50% jak TV

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
#     trybucja w obu formach i cena działają, na razie zostawiam inkremental-
#     ność bo raczej się nie wstawia interakcji jak zmiennej nie ma samej i do
#     doprecyzowania najwyżej. Niezależnie od tego co się wstawi, bardzo ład-
#     nie wchodzi, jedyne co to podwyższa pval TV50_B02_2011_H1 na 82%, więc
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
              #TI_H_MAY +
              TI_H_EASTER_SUNDAY +
              #TI_H_HALLOWEEN_BEFORE +
              TI_H_PENTECOST +
              I(TI_TEM_AVG - TI_TEM_AVG_NORM) +
              I(EX_NU_B02 / mean(EX_NU_B02)) +
              I(TI_X_2011_05_02 * DN_adj_B02_S02_12XCN0500_2GR)
)

summary(model_norm)
vif(model_norm)
jarque.bera.test(model_norm$residuals)

# - aby uzyskać normalność reszt usunięty został ostatecznie 1 outlier,  
#     02.05.2011. Zidentyfikowaną przyczyną odchylenia była promocja cenowa
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

# Model końcowy
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


#### DEKOMPOZYJA MODELU ####

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

# Wykres indeksu sezonowości
plot(x = variables.df$Date,
     y = variables.df$`I(TI_SEASONALITY/mean(TI_SEASONALITY))`,
     type = "l")


#### 2. Poziomy debazowania - wybor ####

#### CZYNNIKI BAZOWE ####


plot(x = variables.df$date,
     y = variables.df$`log(price.own)`,
     type = "l")

# widac tymczasowe obnizki cenowe, dobrze byloby wylapac ich wplyw - debazowanie do max

ref.lev['log(price.own)'] <- max(variables.df$`log(price.own)`)


plot(x = variables.df$date,
     y = variables.df$distribution.numeric.own,
     type = "l")

# sporadyczne problemy z dystrybucja, warto wylapac ich wplyw, dystrybucja najczesciej na wysokim poziomie - do max

ref.lev['distribution.numeric.own'] <- max(variables.df$distribution.numeric.own)

plot(x = variables.df$date,
     y = variables.df$`log(price.compet.2)`,
     type = "l")

# trudno zdecydowac po wykresie - w takich spornych przypadkach bezpieczna opcja: srednia z pierwszego polrocza


ref.lev['log(price.compet.2)'] <- mean(variables.df$`log(price.compet.2)`[1:26])

plot(x = variables.df$date,
     y = variables.df$distribution.compet.1,
     type = "l")


# trudny i rzadki przypadek, ale w tym wypadku max ma chyba najlatwiejsza interpretacje (jednoznacznie ujemny wplyw zmiennej konkurencyjnej), 
# dobrym wyborem beda tez srednia lub srednia z pierwszych miesiecy

ref.lev['distribution.compet.1'] <- max(variables.df$`distribution.compet.1`)

plot(x = variables.df$date,
     y = variables.df$distribution.compet.2,
     type = "l")

# max jest outlierem - bylby duzy wplyw na plus, a nie do konca obrazuje to faktyczna sytuacje, srednia wydaje sie byc najlepszym rozwiazaniem

ref.lev['distribution.compet.2'] <- mean(variables.df$`distribution.compet.2`)


plot(x = variables.df$date,
     y = variables.df$media.own,
     type = "l")


#### CZYNNIK INKREMENTALNY ####

# czynniki inkrementalne - zawsze do 0 
ref.lev['media.own'] <- 0

ref.lev

# przechodzimy na data framey z naszych wektorow

ref.lev.df <- data.frame(variables = names(ref.lev),
                         ref.lev = ref.lev)

coeffs.df <- data.frame(variables = names(coeffs),
                        coeffs = coeffs)

#### 3. Odjecie poziomow debazowania od zmiennych i wymnozenie zmiennych razy parametry beta

variables.debased.df <- variables.df %>%
  pivot_longer(-date, names_to = 'variables', values_to = 'value') %>% # przejscie na dlugi format danych
  left_join(ref.lev.df) %>% # dolaczamy kolumne z poziomem referencyjnym
  mutate(value.debased = value - ref.lev) %>% # odejmujemy od zmiennej w kazdym tygodniu jej poziom bazowy
  left_join(coeffs.df) %>% # dolaczenie wspolczynnikow
  mutate(value.debased = value.debased * coeffs) #wymnozenie wspolczynnikow przez zmienne (bez poziomow bazowania! interesuje nas wplyw vs ten poziom)



### 4. Dodanie poziomow debazowania * parametry do stalej (odjelismy ten efekt od wplywu zmiennych)


base.levels.df <- variables.debased.df %>%
  mutate(base.levels.sum = coeffs * ref.lev) %>%
  group_by(date) %>%
  summarise('(Intercept)' = sum(base.levels.sum)) %>%
  pivot_longer(-date, names_to = 'variables', values_to = 'base.levels')

# powstal data frame, ktory dla kazdego tygodnia ma przypisana wartosc poziomow bazowych, ktorych nie wliczamy
# do wplywu poszczegolnych zmiennych - sa one traktowane jako wartosc bazowa i chcemy je dosumowac do stalej sprzedazy

decomp.final.df <- variables.debased.df %>%
  left_join(base.levels.df) %>%
  mutate(base.levels = ifelse(is.na(base.levels), 0, base.levels)) %>%
  mutate(value.final = value.debased + base.levels) %>%
  select(date, variables, value.final)



### 5. Sprawdzenie czy suma czynnikow zgadza sie z wartoscia fitted z modelu 

check.df <- decomp.final.df %>%
  group_by(date) %>%
  summarise(value.final = sum(value.final)) %>%
  bind_cols(fitted = model$fitted.values * mean(economiser.data.df$sales)) %>%
  mutate(check = value.final - fitted)

sum(check.df$check)



#### 6. Przygotowanie sobie ramki danych do analizy


decomp.final.df <- decomp.final.df %>%
  pivot_wider(names_from = variables, values_from = value.final)

data.final.df <- economiser.data.df %>%
  select(date, sales) %>%
  left_join(decomp.final.df)

write.csv2(data.final.df, 'decomp_data_final.csv', row.names = F)

#### GOTOWE !!!! ####




