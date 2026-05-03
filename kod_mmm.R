library(tidyverse)
library(plotly)

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


# Analiza SKUs

p <- data.df.1 %>%
  select(Date, VO_B02, matches("VO_B02_S\\d{2}_")) %>%
  pivot_longer(cols = c(VO_B02, matches("VO_B02_S\\d{2}_")),
               names_to = "Name",
               values_to = "VO") %>%
  plot_ly(type = "scatter", 
          mode = 'lines',
          x = ~Date, 
          y = ~VO,
          color = ~Name)
htmlwidgets::saveWidget(p, "wykres.html", selfcontained = TRUE)
browseURL("wykres.html")




#### ANALIZA GRAFICZNA ####

options(viewer = NULL)

# Wykres wolumenu sprzedaży całej kategorii i modelowanej marki
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
    name = "VO_B02")
htmlwidgets::saveWidget(p, "wykres.html", selfcontained = TRUE)
browseURL("wykres.html")
sum(data.df.1$VO_B02)/sum(data.df.1$VO_TOTAL_CATEGORY)
# B02 wolumenowo stanowi na oko jakieś 10% kategorii (dokładnie 13%). W
#   kategorii widać sezonowość, w marce na oko niekoniecznie. Dodatkowo
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










p <- plot_ly(data.df, 
             type = "scatter", 
             mode = 'lines',
             x = ~Date, 
             y = ~TI_SEASONALITY) %>%
  layout(yaxis = list(range = c(0, max(data.df$TI_SEASONALITY))))
htmlwidgets::saveWidget(p, "wykres.html", selfcontained = TRUE)
browseURL("wykres.html")



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
        y = ~NS) %>%
  layout(yaxis = list(range = c(0, max(data.df$NS))))




plot_ly(data.df, 
        type = "scatter", 
        mode = 'lines',
        x = ~Date, 
        y = ~EC_CPI) %>%
  layout(yaxis = list(range = c(0, max(data.df$EC_CPI))))




plot_ly(data.df, 
        type = "scatter", 
        mode = 'lines',
        x = ~Date, 
        y = ~VO_B05) %>%
  layout(yaxis = list(range = c(0, max(data.df$VO_B05))))





p <- plot_ly(data.df, 
             type = "scatter", 
             mode = 'lines',
             x = ~Date, 
             y = ~VO_B05 / VO_TOTAL_CATEGORY) %>%
  layout(yaxis = list(range = c(0, max(data.df$VO_B05 / data.df$VO_TOTAL_CATEGORY))))
htmlwidgets::saveWidget(p, "wykres.html", selfcontained = TRUE)
browseURL("wykres.html")



plot_ly(data.df,
        type = "scatter",
        mode = 'lines',
        x = ~Date,
        y = ~VO_B05,
        name = "VO_B05") %>%
  add_trace(
    mode = 'lines',
    x = ~Date,
    y = ~VO_B05_S01,
    name = "VO_B05_S01") %>%
  add_trace(
    mode = 'lines',
    x = ~Date,
    y = ~VO_B05_S02,
    name = "VO_B05_S02") %>%
  add_trace(
    mode = 'lines',
    x = ~Date,
    y = ~VO_B05_S03,
    name = "VO_B05_S03") %>%
  add_trace(
    mode = 'lines',
    x = ~Date,
    y = ~VO_B05_S04,
    name = "VO_B05_S04") 




plot_ly(data.df,
        type = "scatter",
        mode = 'lines',
        x = ~Date,
        y = ~VO_B05,
        name = "VO_B05") %>%
  add_trace(
    mode = 'lines',
    x = ~Date,
    y = ~VO_B05_S04_01XCN0500,
    name = "VO_B05_S04_01XCN0500") %>%
  add_trace(
    mode = 'lines',
    x = ~Date,
    y = ~VO_B05_S04_01XNR0330,
    name = "VO_B05_S04_01XNR0330") %>%
  add_trace(
    mode = 'lines',
    x = ~Date,
    y = ~VO_B05_S04_01XNR0660,
    name = "VO_B05_S04_01XNR0660") %>%
  add_trace(
    mode = 'lines',
    x = ~Date,
    y = ~VO_B05_S04_01XRB0500,
    name = "VO_B05_S04_01XRB0500") %>%
  add_trace(
    mode = 'lines',
    x = ~Date,
    y = ~VO_B05_S04_04XCN0500,
    name = "VO_B05_S04_04XCN0500") %>%
  add_trace(
    mode = 'lines',
    x = ~Date,
    y = ~VO_B05_S04_04XCN0500_OKU,
    name = "VO_B05_S04_04XCN0500_OKU") %>%
  add_trace(
    mode = 'lines',
    x = ~Date,
    y = ~VO_B05_S04_06XCN0500_1GR,
    name = "VO_B05_S04_06XCN0500_1GR") %>%
  add_trace(
    mode = 'lines',
    x = ~Date,
    y = ~VO_B05_S04_06XCN0500,
    name = "VO_B05_S04_06XCN0500")  %>%
  add_trace(
    mode = 'lines',
    x = ~Date,
    y = ~VO_B05_S04_06XNR0330,
    name = "VO_B05_S04_06XNR0330") %>%
  add_trace(
    mode = 'lines',
    x = ~Date,
    y = ~VO_B05_S04_06XCN0500_O3D,
    name = "VO_B05_S04_06XCN0500_O3D") %>%
  add_trace(
    mode = 'lines',
    x = ~Date,
    y = ~VO_B05_S04_07XCN0500_SZK,
    name = "VO_B05_S04_07XCN0500_SZK") %>%
  add_trace(
    mode = 'lines',
    x = ~Date,
    y = ~VO_B05_S04_08XCN0330,
    name = "VO_B05_S04_08XCN0330") %>%
  add_trace(
    mode = 'lines',
    x = ~Date,
    y = ~VO_B05_S04_08XCN0500,
    name = "VO_B05_S04_08XCN0500") %>%
  add_trace(
    mode = 'lines',
    x = ~Date,
    y = ~VO_B05_S04_08XCN0500_ZAW,
    name = "VO_B05_S04_08XCN0500_ZAW") %>%
  add_trace(
    mode = 'lines',
    x = ~Date,
    y = ~VO_B05_S04_08XNR0330_LOD,
    name = "VO_B05_S04_08XNR0330_LOD") %>%
  add_trace(
    mode = 'lines',
    x = ~Date,
    y = ~VO_B05_S04_08XCN0500_1GR,
    name = "VO_B05_S04_08XCN0500_1GR") %>%
  add_trace(
    mode = 'lines',
    x = ~Date,
    y = ~VO_B05_S04_08XCN0500_2GR,
    name = "VO_B05_S04_08XCN0500_2GR") %>%
  layout(yaxis = list(range = c(0, max(data.df$VO_B05))))



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



plot_ly(data.df, 
        type = "scatter", 
        mode = 'lines',
        x = ~Date, 
        y = ~DN_B05) %>%
  layout(yaxis = list(range = c(0, max(data.df$DN_B05))))

















