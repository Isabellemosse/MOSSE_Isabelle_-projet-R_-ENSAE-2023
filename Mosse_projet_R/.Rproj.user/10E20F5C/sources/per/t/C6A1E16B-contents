library(shiny)
library(sp)
library(dplyr)
library(shiny)
library(leaflet)
library(rnaturalearth)
library(rnaturalearthdata)
library(rnaturalearthhires)

# Chargement des données géographiques de l'Afrique de l'Ouest
# afrique_ouest <- subset(ne_countries(scale = "medium", continent = "Africa"), subregion == "Western Africa")
base <- read.csv("ACLED-Western_Africa.csv")

evenement_filtre <- subset(base, pays == "Mali" & type == "Protests" & annee == "2022")

# Création de l'interface utilisateur
ui <- fluidPage(
  # Titre de l'application
  titlePanel("Carte interactive des evenements"),
  
  # Sidebar avec les options de filtrage
  sidebarLayout(
    sidebarPanel(
      selectizeInput(
        inputId = "pays",
        label = "Sélectionnez un ou plusieurs pays",
        choices = c(unique(base$pays)),
        selected = c(unique(base$pays))[sample(1:length(unique(base$pays)), 1)],
        multiple = TRUE
      ),
      selectizeInput(
        inputId = "type_evenement",
        label = "Sélectionnez un ou plusieurs types d'événements",
        choices = c(unique(base$type)),
        selected = "Protests",
        multiple = TRUE
      ),
      selectizeInput(
        inputId = "annee",
        label = "Sélectionnez une ou plusieurs années",
        choices = c(unique(base$annee)),
        selected = "2023",
        multiple = TRUE
      )
    ),
    
    # Affichage de la carte Leaflet
    mainPanel(
      leafletOutput(outputId = "carte",
                    width = "100%",
                    height = "720px")
    )
  )
)

# Logique du serveur
server <- function(input, output, session) {
  base_filtrees <- reactive({
    base_filtrage <- base %>%
      filter(pays %in% input$pays &
               type %in% input$type_evenement & 
               annee %in% input$annee)
    base_filtrage
  })
  
  output$carte <- renderLeaflet({
    base_filtrage <- base_filtrees()
    
    # Création de la carte Leaflet
    carte_leaflet <- leaflet() %>% #initialistion d'une carte
      setView(lng = 0, lat = 8, zoom = 4) %>% # coordonnées concernant la vue de la carte
      addProviderTiles("Esri.WorldTopoMap") %>% # ajout d'un fond de carte
      addPolygons(data = ne_countries(scale="medium",type = "countries", country = input$pays),# ajout et configuration des polygones
                  fillColor = "seagreen", color = "grey0", fillOpacity = 0.6) %>%
      addCircleMarkers(data = base_filtrage,# ajout et configuration des marqueurs de cercles de la carte
                       lng = ~longitude, lat = ~latitude,
                       radius = 3, fillOpacity = 0.7,
                       popup= ~type,
                       color = "#261348")
    # Créer une colonne dans le dataframe base_filtrage pour les info-bulles au survol
    base_filtrage$popup_info <- paste("type d'évenement:", base_filtrage$type, "<br>",
                                      "Année:", base_filtrage$annee, "<br>",
                                      "Localisation:", base_filtrage$localisation)
    
    # Ajouter des marqueurs avec des info-bulles au survol
    carte_leaflet <- carte_leaflet %>%
      addMarkers(data = base_filtrage,
                 lng = ~longitude, lat = ~latitude,
                 popup = ~popup_info,
                 label = ~type,
                 labelOptions = labelOptions(noHide = TRUE)  # Afficher l'étiquette (label) au survol
      )
      
    carte_leaflet
  })
}


# Lancement de l'application Shiny
shinyApp(ui = ui, server = server)