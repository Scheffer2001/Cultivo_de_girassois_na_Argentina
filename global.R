library(tidyverse)
library(plotly)
library(ggplot2)
library(corrplot)
library(leaflet)
library(readxl)
library(RColorBrewer)
library(leafletCN)

dados_md <- read_excel("dados_metadados.xlsx")
dados_db <- read_excel("dados_db.xlsx")
dados_stat <- read_excel("dados_stat.xlsx")

extrair_latitude <- function(local) {
  latitude <- str_extract(local, "\\(.*?[NS]")
  if (is.na(latitude)) {
    return(NA)
  }
  latitude <- gsub("[^0-9.-]", "", latitude)
  return(latitude)
}
extrair_longitude <- function(local) {
  longitude <- str_extract(local, "[NS].*?O")
  if (is.na(longitude)) {
    return(NA)
  }
  longitude <- gsub("[^0-9.-]", "", longitude)
  return(longitude)
}
dados_md$latitude <- sapply(dados_md$local, extrair_latitude)
dados_md$longitude <- sapply(dados_md$local, extrair_longitude)
converter_latitude <- function(latitude) {
  latitude_str <- as.character(latitude)
  latitude_final <-
    paste0(substr(latitude_str, 1, 2), ".", substr(latitude_str, 3, 6))
  return(as.numeric(latitude_final))
}
converter_longitude <- function(longitude) {
  longitude_str <- as.character(longitude)
  longitude_final <-
    paste0(substr(longitude_str, 1, 2), ".", substr(longitude_str, 3, 6))
  return(as.numeric(longitude_final))
}
latitude_convertida <- sapply(dados_md$latitude, converter_latitude)
longitude_convertida <-
  sapply(dados_md$longitude, converter_longitude)
dados_md <- dados_md %>%
  mutate(
    latitude = -1 * latitude_convertida,
    longitude = -1 * longitude_convertida
  )
dados_md$longitude <-
  ifelse(dados_md$longitude > 0,
         -1 * dados_md$longitude,
         dados_md$longitude)
dados_md[dados_md$aba == "Gral Pico", "longitude"] <- -63.7355
dados_md[dados_md$aba == "Gral Pico", "latitude"] <- -35.3236
dados_md[dados_md$aba == "Carhue", "longitude"] <- -62.7631
dados_md[dados_md$aba == "Carhue", "latitude"] <- -37.1782
dados_md[dados_md$aba == "Bonifacio", "longitude"] <- -62.2462
dados_md[dados_md$aba == "Bonifacio", "latitude"] <- -36.8098
dados_md[dados_md$aba == "Anguil", "longitude"] <- -64.0181
dados_md[dados_md$aba == "Anguil", "latitude"] <- -36.5261
dados_md[dados_md$aba == "Goyena", "longitude"] <- -62.6138
dados_md[dados_md$aba == "Goyena", "latitude"] <- -37.7217
dados_md[dados_md$aba == "Loberia", "longitude"] <- -58.7879
dados_md[dados_md$aba == "Loberia", "latitude"] <- -38.1627
dados_md[dados_md$aba == "Miramar", "longitude"] <- -57.9327
dados_md[dados_md$aba == "Miramar", "latitude"] <- -38.2013
dados_md[dados_md$aba == "Suarez", "longitude"] <- -61.9276

dados_md <- dados_md[, -3]
# head(dados_md)

tb <- merge(dados_db, dados_md, by = 'aba', all.y = TRUE)
tb <- tb %>%
  drop_na()
# str(tb)
tb_cv <- dados_stat %>%
  filter(atributo %in% c("CV (%)", "Promedio")) %>%
  select(atributo, ano, aba, rendimento_de_graos)
tb_cv_wider <- tb_cv %>%
  pivot_wider(names_from = atributo,
              values_from = rendimento_de_graos) %>%
  distinct()
tb_cv_wider <- tb_cv_wider %>%
  mutate(CV = `CV (%)` / 100) %>%
  select(ano, aba, Promedio, CV) %>%
  mutate(
    sigma = Promedio * CV,
    lower = Promedio - (1.96 * (sigma)),
    upper = Promedio + (1.96 * (sigma))
  )

tb_cv_wider <- tb_cv_wider %>%
  select(!ano) %>%
  group_by(aba) %>%
  summarise(
    mean = mean(Promedio),
    cv = mean(CV),
    sigma = mean(sigma),
    lower = mean(lower),
    upper = mean(upper)
  )

tb <- distinct(tb)

tb <- merge(tb,
            tb_cv_wider,
            by.x = 'aba',
            by.y = 'aba',
            all.y = TRUE)

local_empresa <- tb %>%
  distinct(aba, empresa) %>%
  filter(aba != "", empresa != "")
tb <- distinct(tb)
# Filter datas
filter_data <- function(tb, LOCAL, EMPRESA) {
  tb <- tb[tb$aba %in% c(LOCAL) & tb$empresa %in% c(EMPRESA),]
  tb
}

filter_data2 <- function(tb, LOCAL) {
  tb <- tb[tb$aba %in% c(LOCAL),]
}
filter_data3 <- function(tb, EMPRESA) {
  tb <- tb[tb$empresa %in% c(EMPRESA),]
}

# Make Plots
## Boxplot
plot_boxplot <- function(tb_sel, plotly = FALSE) {
  gg <- tb_sel %>%
    ggplot(aes(x = ano, y = altura, fill = ano)) +
    geom_boxplot() +
    theme(legend.position = "") +
    xlab("") +
    ylab("") +
    ggtitle("Altura por Ano") +
    scale_fill_brewer(palette = "PuOr")
  if (plotly) {
    p <- plotly::ggplotly(gg, tooltip = "altura")
    p <- p %>% plotly::layout(showlegend = FALSE)
    return(p)
  } else{
    return(gg)
  }
}

## Barplot
plot_barplot <- function(tb_sel, plotly = FALSE) {
  tb_bp <- tb_sel %>% group_by(empresa) %>%
    summarise(rendimento = sum(rendimento_de_graos)) %>%
    arrange(rendimento) %>%
    mutate(empresa = factor(empresa, levels = empresa))
  gg <- tb_bp %>% ggplot(aes(y = empresa,
                             x = rendimento,
                             fill = rendimento)) +
    geom_col() +
    labs(x = "Rendimento Total", y = "Empresa") +
    ggtitle("Rendimento Total por Empresa") +
    scale_fill_gradient(low = "#988dc2", high = "#f2a641")
  if (plotly) {
    return(plotly::ggplotly(gg, tooltip = "rendimento"))
  } else{
    return(gg)
  }
}

## Corrplot
plot_corrplot <- function(tb_sel, plotly = FALSE) {
  tb_cp <- tb_sel %>%
    select(altura,
           rendimento_de_graos,
           densidade,
           dias_ate_floracao) %>%
    drop_na() #%>%
  # scale()
  gg <-
    ggcorrplot::ggcorrplot(
      corr = cor(tb_cp),
      hc.order = TRUE,
      type = "lower",
      lab = TRUE,
      ggtheme = ggplot2::theme_gray,
      colors = c("#988dc2", "white", "#f2a641"),
      outline.col = "lightgray"
    )
  if (plotly) {
    return(plotly::ggplotly(gg))
  } else{
    return(gg)
  }
}

## Leaflet
plot_country_map <- function(tb_sel) {
  tb_map <- tb_sel %>%
    group_by(latitude, longitude, aba) %>%
    summarise(
      Altura_media = mean(altura),
      dias_medio = mean(dias_ate_floracao),
      rendimento_total = sum(rendimento_de_graos),
      densidade_media = mean(densidade)
    ) %>%
    drop_na()
  
  # Ajuste do cálculo do raio
  max_density <- max(tb_map$densidade_media, na.rm = TRUE)
  min_density <- min(tb_map$densidade_media, na.rm = TRUE)
  tb_map <- tb_map %>%
    mutate(
      scaled_density = (densidade_media - min_density) / (max_density - min_density),
      marker_radius = 5 + 20 * scaled_density
    )
  
  
  leaflet::leaflet(tb_map) %>%
    leaflet::addTiles() %>%
    leaflet::addCircleMarkers(
      lng = ~ longitude,
      lat =  ~ latitude,
      radius = ~ marker_radius,
      color = "#988dc2",
      fillOpacity = 0.5,
      popup = ~ paste(
        "<div class='custom-popup'>",
        "<strong>Local:</strong> ",
        aba,
        "<br>",
        "<strong>Altura Média:</strong> ",
        sprintf("%.2f", Altura_media),
        " cm<br>",
        "<strong>Dias Médios até Floração:</strong> ",
        sprintf("%.2f", dias_medio),
        "<br>",
        "<strong>Rendimento Total:</strong> ",
        sprintf("%.2f", rendimento_total),
        " kg<br>",
        "<strong>Densidade Média:</strong> ",
        sprintf("%.2f", densidade_media),
        " plantas/m²",
        "</div>"
      )
    ) %>%
    leaflet::addControl(html = "<h3>Densidade média por Local</h3>",
                        position = "topright",
                        className = "map-title")
}

## Caterpillar
### para o catterpillar plot teremos o gráfico do rendimento de graos
plot_catterpillar <- function(tb_sel, plotly = FALSE) {
  tb_catter <- tb %>%
    select(aba,
           mean,
           lower,
           upper) %>%
    distinct() %>%
    drop_na() %>%
    arrange(mean)
  gg <- tb_catter %>%
    ggplot(aes(
      y = factor(aba, levels = aba),
      x = mean,
      colour = mean
    )) +
    geom_linerange(aes(xmin = lower, xmax = upper)) +
    geom_point(aes(
      y = aba,
      x = mean,
      text = paste(
        "Média:",
        round(mean, 2),
        "<br>Limite Inferior:",
        round(lower, 2),
        "<br>Limite Superior:",
        round(upper, 2)
      )
    )) +
    theme(legend.position = "none") +
    ylab("Local") + xlab("Média") +
    ggtitle("Rendimento médio de grãos por Local") +
    scale_color_gradient(low = "#988dc2", high = "#f2a641")
  if (plotly) {
    p <- plotly::ggplotly(gg, tooltip = "text")
    p <- p %>% plotly::layout(showlegend = FALSE)
    return(p)
  } else{
    return(gg)
  }
}
