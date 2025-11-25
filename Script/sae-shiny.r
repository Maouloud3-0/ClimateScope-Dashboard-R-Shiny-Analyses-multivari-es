# 1. Application shiny qui représente une carte avec un indicateur
# Chargement des packages
library(shiny)
library(dplyr)
library(leaflet)

# Chargement des données
indicateur=data.frame(pays=c("France", "Espagne","Italie","Suisse","Allemagne"),
                      menf=c(9.8, 9.7, 12.1, 8.5, 12.7),
                      long=c(1.8883,-2.64,12.5674, 8.2275, 10.4515),
                      latit=c(46.6031,40.4637,41.8719,46.8182,51.1657))

#######################################################################################################################
# Création de l'interface shiny
ui <- fluidPage(
  titlePanel("Taux de Mortalité Infantile en Europe"),
  mainPanel(
    leafletOutput("carte")
  )
)
server <- function(input, output) {
  output$carte <- renderLeaflet({
    # Création de la carte avec plot_ly
    leaflet(indicateur) %>% # pour fixer les limites de la carte
      fitBounds(-20,65,20,40) %>%
      addProviderTiles("OpenStreetMap.Mapnik") %>%  # Choix du fond de carte
# autres addProviderTiles possibles (tester) : providers$CartoDB.Positron, 
#        providers$Stamen.Toner, providers$Esri.NatGeoWorldMap, “Stamen.Watercolor”, “Stamen.TonerHybrid”
            addMarkers(
        lng = 2.3522, lat = 48.8566,  # Coordonnées centrales pour centrer la carte positionnées sur Paris
        label = "Paris"
      ) %>%
      addCircles(
        lng = ~long, lat = ~latit, 
        weight = 1,
        radius = ~sqrt(menf) * 40000,  # les cercles sont de rayon proportionnel à sqrt(taux de mortalité)
        popup = ~paste("Pays : ", pays, "<br>Taux de Mortalité Infantile : ", menf)
      )
  })
}

# Lancement de l'application Shiny
shinyApp(ui, server)

#######################################################################################################################
# Chargement des données
indicateur=data.frame(pays=c("France", "Espagne","Italie","Suisse","Allemagne"),
                      menf=c(9.8, 9.7, 12.1, 8.5, 12.7),
                      long=c(1.8883,-2.64,12.5674, 8.2275, 10.4515),
                      latit=c(46.6031,40.4637,41.8719,46.8182,51.1657))
# 2. Ajout de contours venant d'une carte au format shapefile
# bibliothèque supplémentaire nécessaire
library(sf)
# Chargement de la carte
monde <- st_read("C:/Users/gaous/Desktop/BUT 3/S6/big data/BUT3 - SAÉ 6.EMS.01 - Big Data/Données/monde/monde.shp")
head(monde)
# Création de la colonne dans la table indicateur qui contient les ISO3 des pays
indicateur$ISO3=c("FRA", "ESP","ITA","CHE","DEU")
# fusion des données avec la carte
monde_data <- left_join(monde, indicateur, by = c("ISO3" = "ISO3"))
head(monde_data)
extrait=monde_data[monde_data$ISO3 %in% c("FRA", "ESP","ITA","CHE","DEU"),]
head(extrait)
# autre méthode avec dplyr pour la sélection des données shapefile pour les pays d'interet
Europe_Ouest <- monde_data %>% 
  dplyr::filter(ISO3 %in% c("FRA", "ESP","ITA","CHE","DEU")) 
head(Europe_Ouest)

ui <- fluidPage(
  titlePanel("Quelques pays d'Europe de l'Ouest"),
  mainPanel(
    leafletOutput("carte")
  )
)
server <- function(input, output) {
  output$carte <- renderLeaflet({
    leaflet(monde_data) %>%
      addProviderTiles("OpenStreetMap.Mapnik") %>%
      addPolygons(data=Europe_Ouest,weight = 2, color="orange",fillOpacity=0.35) 
  })
}

shinyApp(ui, server)

# 3. AJout des données sur cette carte avec fonds colorés
server <- function(input, output) {
  output$carte <- renderLeaflet({
    leaflet(monde_data) %>%
      fitBounds(-20,65,20,40) %>%
      addProviderTiles("OpenStreetMap.Mapnik") %>%
      addPolygons(data=Europe_Ouest,weight = 2, color="orange",fillOpacity=0.35) %>%
      addCircles(
        lng = ~long, lat = ~latit, 
        weight = 1,
        radius = ~sqrt(menf) * 40000,  # les cercles sont de rayon proportionnel à sqrt(taux de mortalité)
        popup = ~paste("Pays : ", pays, "<br>Taux de Mortalité Infantile : ", menf)
      )
  })
}
shinyApp(ui, server)


##############################################################################☺

library(shiny)
library(ggplot2)
library(dplyr)

indicateur <- data.frame(
  pays = rep(c("Nigéria", "Australie", "France", "Brésil", "Chine"), each = 6),
  année = rep(2015:2021, times = 6),
  ISO = rep(c("NGA", "AUS", "FRA", "BRA", "CHN"), each = 6),
  pib_par_habitant = rep(c(63216.149, 17827.641, 58347.670, 21482.562, 74896.996), each = 6),
  croissance_pib_habitant = rep(c(2.985, 2.428, 1.943, 3.002, 1.925), each = 6),
  mortalite_infantile = rep(c(5.0, 14.4, 5.0, 6.9, 3.6), each = 6),
  indice_gini = rep(c(34.3, 52.9, 31.7, 37.1, 27.5), each = 6),
  investissement_etranger = rep(c(310.0817, 2610.9993, 15588.4871, 3905.3176, 5858.8015), each = 6),
  long = rep(c(8.6753, 133.7751, 2.2137, -51.9253, 104.1954), each = 6),
  latit = rep(c(9.0820, -25.2744, 46.2276, -14.2350, 35.8617), each = 6)
)
# UI
ui <- fluidPage(
  titlePanel("Comparaison des Indicateurs Économiques"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("indicator", "Choisissez l'indicateur:",
                  choices = colnames(dataframe_eco_updated)[4:8]),
      sliderInput("yearRange", "Sélectionnez la période:",
                  min = 2014, max = 2022, value = c(2014, 2022), step = 1),
      selectInput("graphType", "Choisissez le type de graphique:",
                  choices = c("Line Plot" = "line",
                              "Bar Plot" = "bar",
                              "Bubble Plot" = "bubble",
                              "Histogram" = "histogram"))
    ),
    
    mainPanel(
      plotOutput("indicatorPlot")
    )
  )
)

# Server
server <- function(input, output) {
  
  output$indicatorPlot <- renderPlot({
    # Filtrer les données en fonction des entrées de l'utilisateur
    data_filtered <- dataframe_eco_updated %>%
      filter(Année >= input$yearRange[1], Année <= input$yearRange[2]) %>%
      select(Pays, ISO, Année, all_of(input$indicator))
    
    # Créer le graphique en fonction du type sélectionné
    if (input$graphType == "line") {
      gg <- ggplot(data_filtered, aes(x = Année, y = !!sym(input$indicator), group = Pays, color = Pays)) +
        geom_line()
      
    } else if (input$graphType == "bar") {
      gg <- ggplot(data_filtered, aes(x = factor(Année), y = !!sym(input$indicator), fill = Pays)) +
        geom_bar(stat = "identity", position = position_dodge())
      
    } else if (input$graphType == "bubble") {
      gg <- ggplot(data_filtered, aes(x = factor(Année), y = !!sym(input$indicator), color = Pays)) +
        geom_point(size = 4) +
        facet_wrap(~Pays, scales = "free_y")  # Separate plots by country
      
    } else if (input$graphType == "histogram") {
      gg <- ggplot(data_filtered, aes(x = !!sym(input$indicator), fill = Pays)) +
        geom_histogram(bins = 30, alpha = 0.7) +
        facet_wrap(~Pays)  # Separate plots by country
    }
    
    # Appliquer les thèmes et labels communs ici
    gg + theme(legend.position = "bottom", strip.text.x = element_text(angle = 0)) +
      labs(color = "Pays") # Légende pour les couleurs des pays
  })
}

# Lancer l'application
shinyApp(ui = ui, server = server)

##############################################################################################################
library(shiny)
library(leaflet)
library(dplyr)
library(sf)

# Création d'un dataframe avec une ligne pour chaque combinaison pays-année
# S'assurer que la longueur de chaque vecteur correspond
indicateur <- data.frame(
  pays = rep(c("Nigéria", "Australie", "France", "Brésil", "Chine"), each = 6),
  année = rep(2015:2021, times = 6),
  ISO = rep(c("NGA", "AUS", "FRA", "BRA", "CHN"), each = 6),
  pib_par_habitant = rep(c(63216.149, 17827.641, 58347.670, 21482.562, 74896.996), each = 6),
  croissance_pib_habitant = rep(c(2.985, 2.428, 1.943, 3.002, 1.925), each = 6),
  mortalite_infantile = rep(c(5.0, 14.4, 5.0, 6.9, 3.6), each = 6),
  indice_gini = rep(c(34.3, 52.9, 31.7, 37.1, 27.5), each = 6),
  investissement_etranger = rep(c(310.0817, 2610.9993, 15588.4871, 3905.3176, 5858.8015), each = 6),
  long = rep(c(8.6753, 133.7751, 2.2137, -51.9253, 104.1954), each = 6),
  latit = rep(c(9.0820, -25.2744, 46.2276, -14.2350, 35.8617), each = 6)
)

# Chargement de la carte - ajustez le chemin selon votre dossier local
# monde <- st_read("path/to/your/shapefile.shp")

# Fusion des données avec la carte
# Assurez-vous que 'ISO3' est le bon nom de la colonne et qu'il est présent dans `monde`
# monde_data <- left_join(monde, indicateur, by = c("ISO3" = "ISO"))

# UI
ui <- fluidPage(
  titlePanel("Comparaison des Indicateurs par Pays et Année"),
  sidebarLayout(
    sidebarPanel(
      selectInput("selectedYear", "Choisissez l'année:", choices = unique(indicateur$année)),
      selectInput("selectedIndicator", "Choisissez l'indicateur:", choices = names(indicateur)[4:8])
    ),
    mainPanel(
      leafletOutput("carte")
    )
  )
)

# Server
server <- function(input, output) {
  output$carte <- renderLeaflet({
    # Filtrer les données selon les choix de l'utilisateur
    data <- indicateur %>%
      filter(année == as.numeric(input$selectedYear)) %>%
      select(pays, long, latit, everything())
    
    # Créer la carte avec leaflet
    leaflet(data) %>%
      addTiles() %>%
      addCircles(
        lng = ~long, lat = ~latit, color = "orange", fillColor = "turquoise", fillOpacity = 0.5,
        weight = 1, radius = 50000,
        popup = ~paste(pays, "<br/>Année:", année, "<br/>", input$selectedIndicator, ":", .[[input$selectedIndicator]])
      ) %>%
      fitBounds(~min(long), ~min(latit), ~max(long), ~max(latit))
  })
}

# Lancer l'application
shinyApp(ui, server)



######################################################################################################"

library(shiny)
library(FactoMineR)
library(factoextra)
library(dplyr)

#  données économiques pour l'exemple
dataframe_eco <- data.frame(
  pib_hbts.Country.Name = c("France", "États-Unis", "Nigeria","Brésil","Russie"),
  pib_hbts.Country.Code = c("FRA", "USA","NGA","BRA","RUS"),
  pib_hbts.2022 = c(16955.81,16404.27, 20000,24443.46,19598.25 ),
  croispibhab.2022 = c(1.927257,2.417186, 2.5,1.650798,1.509911),
  tmortinf.2022 = c(4.2577966,3.4442360,6,2.8907131,3.1087591),
  Gini.2022 = c(29.14675, 33.03285, 33,28.05205,25.62341),
  consoelechab.2022 = c(7991.525, 4841.662, 5500,3014.871,4683.462),  
  expominmet.2022 = c(323.3997, 352.7535, 350,353.7866,414.0824),
  transautomonde.2022 = c(218.9961, 200.6756,225,219.5805,183.0667),
  populatactive.2022 = c(41.45683, 46.33633, 60,55.57039,44.54500),
  tauxferti.2022 = c(1.5, 1.6944011, 1.6,1.9880211,1.4592959),
  inflation.2022 = c(1.4,2.4492884, 2.2,2.6071575,1.3633186),
  capitaentrepeise.2022 = c(122, 142.63243,180,102.06313,143.46701),
  emiGES.2022 = c(334.0528, 445.8548, 425,427.7372,462.5078)
)

# Définir l'interface utilisateur
library(shiny)
library(FactoMineR)
library(factoextra)
library(dplyr)

# Supposons que dataframe_eco est déjà chargé avec des données réelles
# dataframe_eco <- read.csv("chemin/vers/votre/fichier.csv")


# UI
ui <- fluidPage(
  titlePanel("Analyse en Composantes Principales (ACP) des Données Économiques"),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("selected_vars", "Choisissez les variables à inclure dans l'ACP:",
                         choices = names(dataframe_eco)[-1:-2], selected = names(dataframe_eco)[-1:-2])
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Individus", plotOutput("plot_ind")),
        tabPanel("Variables", plotOutput("plot_var")),
        tabPanel("Valeurs propres", plotOutput("plot_eig"))
      )
    )
  )
)

# Server
server <- function(input, output) {
  output$plot_ind <- renderPlot({
    data_ec <- select(dataframe_eco, all_of(input$selected_vars)) %>% na.omit()
    res_pca <- PCA(data_ec, scale.unit = TRUE, ncp = 4, graph = FALSE)
    fviz_pca_ind(res_pca, col.ind = "cos2", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE)
  })
  
  output$plot_var <- renderPlot({
    data_ec <- select(dataframe_eco, all_of(input$selected_vars)) %>% na.omit()
    res_pca <- PCA(data_ec, scale.unit = TRUE, ncp = 4, graph = FALSE)
    fviz_pca_var(res_pca, col.var = "contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE)
  })
  
  output$plot_eig <- renderPlot({
    data_ec <- select(dataframe_eco, all_of(input$selected_vars)) %>% na.omit()
    res_pca <- PCA(data_ec, scale.unit = TRUE, ncp = 4, graph = FALSE)
    fviz_eig(res_pca, addlabels = TRUE, ylim = c(0, 100))
  })
}

# Lancer l'application
shinyApp(ui, server)




