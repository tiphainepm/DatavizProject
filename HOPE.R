# Packages
library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(leaflet)
library(dplyr)
library(data.table)
library(ggplot2)
library(ggrepel)
library(stringi)
library(RColorBrewer)
library(sunburstR)
library(readxl)
library(leaflet.extras)
library(future.apply)
library(future)
library(scales)
library(DT)
library(sf)
library(flexdashboard)




##### Importation de la base #####

#chemin <- "/Users/amanda/Desktop/Data visualisation/DatavizProjet"
chemin <- "/Users/salmalaarech/Desktop/DATA_VIZ"

setwd(chemin)


airbnb_data <- read_excel(paste(chemin,"/DataBaseCoupé.xlsx", sep=""))
airbnb_data$Latitude <- as.numeric(airbnb_data$Latitude)
airbnb_data$Longitude <- as.numeric(airbnb_data$Longitude)

JO <- read_excel(paste(chemin,"/paris-2024-sites-olympiques-et-paralympiques-franciliens.xlsx",sep=""))
JO$Latitude <- as.numeric(JO$Latitude)
JO$Longitude <- as.numeric(JO$Longitude)

########### Mise en forme de la base #############

# Mise en forme des arrondissements
zipcode_a_arrondissement <- function(zipcode) {
  Arrondissement <- as.character(zipcode)
  if (Arrondissement == "75001") {
    return("1er")
  } else if (substr(Arrondissement, nchar(Arrondissement) - 1, nchar(Arrondissement) - 1) == "0") {
    return(paste0(substr(Arrondissement, nchar(Arrondissement), nchar(Arrondissement)), "ème"))
  } else {
    return(paste0(substr(Arrondissement, nchar(Arrondissement) - 1, nchar(Arrondissement)), "ème"))
  }
}

airbnb_data <- airbnb_data %>% 
  mutate(Arrondissement = sapply(Zipcode, zipcode_a_arrondissement))

# Mise en forme des coordonnées
airbnb_data$Longitude <- as.numeric(airbnb_data$Longitude)
airbnb_data$Latitude <- as.numeric(airbnb_data$Latitude)

# Convertir Square Feet à Square Meters
airbnb_data$SquareMeters <- airbnb_data$`Square Feet` / 10.764

# Valeurs initiales pour le curseur de prix
min_price <- min(airbnb_data$Price, na.rm = TRUE)
max_price <- max(airbnb_data$Price, na.rm = TRUE)

# Mise e forme de la variable note
airbnb_data$Note <- round(airbnb_data$`Review Scores Rating`/20)

# Mise en forme des URL images

for (i in 1:length(airbnb_data$`XL Picture Url`)){
  if (is.na(airbnb_data$`XL Picture Url`[i])){
    airbnb_data$`XL Picture Url`[i]="https://www.thetrainline.com/cms/media/1360/france-eiffel-tower-paris.jpg?mode=crop&width=1080&height=1080&quality=70"
    }
}

# Ajouter une colonne 'popup' 
airbnb_data$Popup <- paste0(
  "<b>", airbnb_data$Name, "</b>", "<br><br>",
  "<div style='text-align: center; max-width: 200px; max-height: 300px; overflow-y: auto;'>", # Ajouter des styles CSS ici
  "<img src='", airbnb_data$`XL Picture Url`, "' style='max-width: 100%;'>", "<br><br>",
  airbnb_data$Space, "</div>"
)

# Ajouter une colonne 'label'

airbnb_data$Label <- paste0(
  '<div style="text-align: left;">',
  "<u>Informations :</u>",
  "<ul>", 
  "<li>Prix: ", airbnb_data$Price, "€ la nuit </li>",
  "<li>Note: ", airbnb_data$Note, " étoile</li>",
  "<li>Nombre de chambres: ", airbnb_data$Bedrooms, "</li>",
  "<li>Nombre de lits: ", airbnb_data$Beds, "</li>",
  "<li>Nombre de salle de bains: ", airbnb_data$Bathrooms, "</li>",
  "<li>Politique d'annulation: ", airbnb_data$`Cancellation Policy`, "</li>",
  "<li>Surface: ", round(airbnb_data$SquareMeters), " m²</li>",
  "<li>Arrondissement: ", airbnb_data$Arrondissement, "</li>",
  "</ul>", 
  '</div>'
)

# Icônes 

icons <-awesomeIcons(icon='home', iconColor ='black', library='glyphicon', marker='white')
icons_green <-awesomeIcons(icon='home', iconColor ='black', library='glyphicon', marker='green')
icons_orange <-awesomeIcons(icon='home', iconColor ='black', library='glyphicon', marker='orange')
icons_red <-awesomeIcons(icon='home', iconColor ='black', library='glyphicon', marker='red')

# Fonction calcul de distance

calculateDistancesToPoint <- function(lat_points1, long_points1, lat_point2, long_point2) {
  # Vérification des longueurs des vecteurs
  if (length(lat_points1) != length(long_points1)) {
    stop("Les vecteurs de latitude et de longitude ne sont pas de même longueur.")
  }
  
  haversine <- function(lat1, lon1, lat2, lon2) {
    R <- 6371  # Rayon moyen de la Terre en km
    dlat <- (lat2 - lat1) * pi / 180
    dlon <- (lon2 - lon1) * pi / 180
    a <- sin(dlat/2)^2 + cos(lat1 * pi / 180) * cos(lat2 * pi / 180) * sin(dlon/2)^2
    c <- 2 * atan2(sqrt(a), sqrt(1 - a))
    distance <- R * c
    return(distance)
  }
  
  vecteurDist <- numeric(length(lat_points1))
 
  for (i in seq_along(lat_points1)) {
    vecteurDist[i] <- haversine(lat_points1[i], long_points1[i], lat_point2, long_point2)
  }
  
  return(vecteurDist)
}

### Sunburst ###

# Assuming airbnb_dt is already defined and has the 'label' column
airbnb_dt <- as.data.table(airbnb_data)
airbnb_dt$Bedrooms <- paste0(airbnb_dt$Bedrooms, " Bedrooms")
airbnb_dt$Beds <- paste0(airbnb_dt$Beds, " Beds")


# Create the hierarchy for the sunburst chart
airbnb_dt[, label := paste(`Property Type`, `Room Type`, Bedrooms, Beds, sep = "-")]

# Aggregate the data to count the number of occurrences of each combination
agg <- airbnb_dt[, .(Count = .N), by = .(label)]



# Define UI
ui <- dashboardPage(
  dashboardHeader(
    title = "Airbnb",
    titleWidth = 250
    ),
    
    dashboardSidebar(
      width = 250,
      tags$head(
        tags$style(HTML('
        /* CSS to hide sidebar content when collapsed */
        #sidebar.collapsed .sidebar-menu, 
        #sidebar.collapsed .control-sidebar-tabs,
        #sidebar.collapsed .control-sidebar-content {
          display: none;
        }
        /* Ensure the overflow content in the sidebar is not visible */
        #sidebar {
          overflow: hidden;
        }
        
        .irs-bar, .irs-bar-edge, .irs-slider {
        background: #FF8733 !important;
        border-top-color: #FF8733 !important;
        border-bottom-color: #FF8733 !important;
      }
      .irs-from, .irs-to, .irs-single {
        background: #FF8733 !important;
        border-top-color: #FF8733 !important;
        border-bottom-color: #FF8733 !important;
      }
      '))
      ),
      sidebarMenu(
        menuItem(" À propos", tabName = "about", icon = icon("info-sign", lib = "glyphicon")),
        menuItem(" Découverte des Données", tabName = "data_discovery", icon = icon("stats", lib="glyphicon")),
        menuItem(" Carte", tabName = "map", icon = icon("globe", lib = "glyphicon")),

      tags$hr(),
      tags$h3("Filtres", class = "text-center"),
      
      pickerInput(
        "selectedArrondissement",
        "Sélectionnez l'Arrondissement",
        choices = c("1er", "2ème", "3ème", "4ème", "5ème", "6ème", "7ème", "8ème", "9ème", "10ème", "11ème", "12ème", "13ème", "14ème", "15ème", "16ème", "17ème", "18ème", "19ème", "20ème"),
        options = list(
          `actions-box` = TRUE,
          pickerOptions(
            selectedTextFormat = "count > 3",
            actionsBox = TRUE,
            style = "btn-warning",  # Couleur de fond des éléments sélectionnés
            size = 10
          )
        ),
        selected = c("1er", "2ème", "3ème", "4ème", "5ème", "6ème", "7ème", "8ème", "9ème", "10ème", "11ème", "12ème", "13ème", "14ème", "15ème", "16ème", "17ème", "18ème", "19ème", "20ème"),
        multiple = TRUE
      ),
      
      sliderInput("selectedPriceRange", "Sélectionnez une tranche de prix",
                  min = min_price,
                  max = max_price,
                  value = c(min_price, max_price),
                  step = 1
      ),
      fluidRow(
        column(5, numericInput("minSurface", "Surface minimale (m²)", value = 0, step = 1)),
        column(5, numericInput("minAccommodates", "Nombre de personnes", value = 1, min = 1))
      ),
      fluidRow(
        column(
          width = 12,
          selectizeInput(
            inputId = "selectedRating",
            label = "Sélectionnez la note",
            choices = c("1", "2", "3", "4", "5"),
            multiple = TRUE,
            selected = c("1", "2", "3", "4", "5"),
            options = list('plugins' = list('remove_button'))
          )
        )
      ),
      checkboxGroupInput(
        "selectedRoomType",
        "Sélectionnez le type de propriété",
        choices = c("Maison/Appart" = "Entire home/apt", "Chambre privée" = "Private room", "Chambre partagée" = "Shared room"),
        selected = c("Entire home/apt", "Private room", "Shared room")
      )
    )),
    
    dashboardBody(
      tags$style(HTML(".sidebar { position: fixed; width: 250px}")),
      
      tabItems(
        tabItem(
          tabName = "about",
          fluidRow(
            box(
              width = 12,
              align = "center", status="warning",
              tags$img(src = "JO.png", width = "7%", class = "img-fluid")
            )),
          fluidRow(
            box(
              width = 12,
              status="warning",
              HTML("<p>À l'heure où le monde se réunit pour célébrer l'excellence sportive lors des Jeux Olympiques, une question cruciale s'impose à tous les participants, les supporters, et les voyageurs : où déposer ses bagages afin de vivre pleinement cet événement ? Notre projet de visualisation de données se plonge au cœur de cette interrogation. Effectivement, naviguer à travers la diversité des options d'hébergement peut s'avérer aussi délicat que de maîtriser les épreuves sportives les plus exigeantes. Notre tableau de bord vous permettra de naviguer à travers les annonces Airbnb dans la ville de Paris de façon simple. </p>")
            )),
            fluidRow(width=12,
            column(width=6,
            box( width = NULL,status="warning",solidHeader = TRUE, title= "Description des Onglets",
                 HTML("<b> <h3>Onglet 'Découverte des Données'</h3> </b>
  <p>Dans l'onglet 'Découverte des Données', plongez dans l'univers des données Airbnb parisiennes grâce à une série de graphiques informatifs. Explorez la répartition des types de logements, les tarifs moyens et bien plus encore. Ces graphiques  fournissent un aperçu visuel des caractéristiques clés de notre base de données Airbnb, vous permettant de mieux comprendre les dynamiques du marché.</p>
  <b><h3>Onglet 'Carte'</h3></b>
  <p>L'onglet 'Carte' vous emmène dans un voyage visuel à travers la ville lumière. Explorez la distribution spatiale des annonces Airbnb sur une carte interactive de Paris. La carte fournit une perspective géographique unique pour vous aider à prendre des décisions éclairées.</p>
  <br>
  <p>Nous espérons que notre tableau de bord Shiny améliorera votre compréhension des annonces Airbnb à Paris et vous aidera à tirer des conclusions significatives à partir de nos données. N'hésitez pas à explorer les différentes fonctionnalités et à interagir avec les graphiques pour une expérience utilisateur enrichissante. Bonne exploration ! <br><br><br><br><br><br><br><p>"),
                 
            )),
            column(width=6,
              box(width=NULL, align = "center", status="warning",
                  tags$img(src = "Paris.png", width = "104%", class = "img-fluid")
              ))),
          fluidRow(
            box(
              width = 12,
              align = "center", status="warning",
              tags$img(src = "Airbnb.png", width = "8%", class = "img-fluid")
            ))
        ),
        tabItem(
          tabName = "data_discovery",
          fluidRow(width=12,
                 box(width=12, solidHeader=TRUE, status="warning",
                h2(HTML("<b>Présentation des données </b>"), align = "center"))),
          
          fluidRow(width=12,
                column(width=6,
                       box(width=NULL,  title="Description de la base", solidHeader = TRUE, status="warning",
                    HTML("<p>Notre base de données est un extrait des annonces Airbnb à Paris en 2020. <br> Elle détaille une multitude d'informations sur les hébergements proposés sur la plateforme. </p>"),
                         column(width=6,valueBoxOutput("obsBox")),
                         column(width=6,valueBoxOutput("varsBox")),
                    HTML("<br><br><br><br><br><br><p> Voici les informations contenues pour chaque enregistrement :</p>
    <ul>
        <li><strong>ID :</strong> Un numéro d'identification unique attribué à chaque annonce.</li>
        <li><strong>Nom :</strong> Le nom ou le titre de l'annonce, tel qu'il est présenté aux potentiels locataires.</li>
        <li><strong>Espace :</strong> Une description de l'espace offert par l'annonce.</li>
        <li><strong>XL Picture :</strong> Un lien vers une image en grande taille de l'annonce.</li>
        <li><strong>Host ID :</strong> Un numéro d'identification unique pour chaque hôte.</li>
        <li><strong>Host Response Time :</strong> Le temps que prend l'hôte à répondre aux demandes des clients.</li>
        <li><strong>Host Response Rate :</strong> Le pourcentage de réponse de l'hôte aux demandes des clients.</li>
        <li><strong>Host Verification :</strong> Les vérification qu'à effectué l'hôte sur Airbnb.</li>
        <li><strong>Zipcode :</strong> L'arrondissement où se situe l'Airbnb.</li>
        <li><strong>Host Listings Count :</strong> Le nombre total d'annonces publiées par l'hôte.</li>
        <li><strong>Latitude</strong> et <strong>Longitude :</strong> Les coordonnées géographiques de l'annonce.</li>
        <li><strong>Type de Propriété :</strong> La catégorie de propriété disponible, comme un appartement ou une maison.</li>
        <li><strong>Type de Chambre :</strong> Le type de chambre proposée, comme une chambre privée ou un logement entier.</li>
        <li><strong>Capacité d'Accueil :</strong> Le nombre maximum de personnes que l'annonce peut accueillir.</li>
        <li><strong>Nombre de Salle de Bain</strong>, <strong>Chambres</strong>, <strong>Lits :</strong> La quantité de salles de bains, chambres à coucher, et lits disponibles.</li>
        <li><strong>Type de Lit :</strong> La variété de lit proposé, comme un lit classique ou un canapé-lit.</li>
        <li><strong>Amenities :</strong> Les équipements offerts avec l'annonce, tels que l'accès Internet ou la télévision par câble.</li>
        <li><strong>Square Feet :</strong> La superficie de l'espace loué en pieds carrés.</li>
        <li><strong>Prix :</strong> Le coût par nuitée.</li>
        <li><strong>Caution</strong>, <strong>Frais de Nettoyage :</strong> Les frais supplémentaires pour la caution et le service de nettoyage.</li>
        <li><strong>Number of Reviews</strong>, <strong>Review Scores Rating :</strong> Le nombre de commentaires laissés par les visiteurs et la note moyenne de l'annonce.</li>
        <li><strong>Politique d'Annulation :</strong> Le type de politique d'annulation appliquée par l'hôte.</li>
        <li><strong>Reviews per Month :</strong> La moyenne mensuelle des commentaires reçus.</li>
    </ul>
    <p>Cette base de données nous permettra d'analyser le marché locatif de courte durée à Paris, en examinant les tendances des prix, la popularité des quartiers, et les préférences des clients.<br><br><br><br><br><br><br></p>")
          )),
          column( width= 6,
                  box(width = NULL, status = "warning", solidHeader = FALSE,title="SunBurst",
                      sunburstOutput("sunburstPlot", width="120%", height ="480px"), HTML("<i><u> Commentaire</u> :Le sunburst graphique illustre de manière visuelle la répartition des annonces de logements en fonction de leur type, du nombre de chambres et du nombre de lits. Cette représentation permet une analyse rapide des tendances de diversité dans les offres d'hébergement. </i>")),
                  box(width=NULL, height = "421px", title = "Proportion des différents types de lits", status = "warning", solidHeader = TRUE,
                      plotOutput("BedType", height = "313px"), HTML("<i><u> Commentaire</u> :La majorité des annonces proposent des lits classiques. </i>"))
                  
          )
        ),
        fluidRow(width=12,
                 column(width=12, height="400px",
                        box(
                          width = NULL,
                          title = "Cartographie du nombre d'Airbnb à Paris",
                          status = "warning",
                          solidHeader = TRUE,
                          plotOutput("cartographie"),
                          HTML("<i><u> Commentaire</u> :La carte illustre la distribution du nombre d'annonces Airbnb dans chaque arrondissement de Paris. </i>"))
                        ,)),
        fluidRow(width=12,
                 column(width=6,
                        box(
                          width = NULL,
                          style = "height: 510px;",
                          title = "Distribution en fonction des prix",
                          status = "warning",
                          solidHeader = TRUE,
                          plotOutput("histogramme"),
                          HTML("<i><u> Note</u> : Vous pouvez sélectionner l'arrondissement de votre choix, par le filtre mis en place à gauche, pour avoir une idée sur sa propre distribution en fonction des prix. </i>"),
                        )),
                 column(width=6, 
                        box(
                          width = NULL,
                          style = "height: 510px;",
                          status="warning",
                          title = "HeatMap",
                          solidHeader = FALSE,
                          leafletOutput("plot5", height="428px"),
                          HTML("<i><u> Commentaire</u> : La heatmap représente la moyenne des prix des annonces Airbnb pour chaque arrondissement de Paris, offrant une visualisation rapide des variations de tarifs à travers la ville. </i>"),
                          
                        )) 
        ),
        fluidRow(width=12,
                 column(width=3, height="200px",
                        box(width=NULL, height="200px", title="Note Moyenne", status="warning", solidHeader = TRUE,
                            gaugeOutput("noteGauge"))),
                 column(width=3, height="200px",
                        box(width=NULL, height="200px", title="Prix Moyen", status="warning", solidHeader = TRUE,
                           gaugeOutput("priceGauge"))),
                 column(width=3, height="200px",
                        box(width=NULL, height="200px", title="Taux de Réponse Moyen", status="warning", solidHeader = TRUE,
                            gaugeOutput("responseGauge"))),
                 column(width=3, height="200px",
                        box(width=NULL, height="200px", title="Nombre de Commentaire Moyen", status="warning", solidHeader = TRUE,
                           gaugeOutput("reviewGauge")))
                 ),
        fluidRow(width = 12,
                 
                 column(width = 6,
                        box(width = NULL, height = "510px", title = "Meilleurs Arrondissements", status = "warning", solidHeader = TRUE,
                            tabsetPanel(
                              tabPanel("Note", dataTableOutput("noteTable")),
                              tabPanel("Commentaire par mois", dataTableOutput("reviewsTable")),
                              tabPanel("Pourcentage de Réponse", dataTableOutput("responseRateTable"))
                            ),
                            HTML("<i><u> Commentaire</u> : Classement des arrondissements selon la moyenne des notes, le nombre moyen de commentaires mensuels, et le pourcentage de réponses du propriétaire. </i>")
                            
                        )
                 ),
                 column(width = 6,
                        box(width = NULL, height = "510px", title = "Boxplot pour les frais de nettoyage par politique d'annulation", status = "warning", solidHeader = TRUE,
                            plotOutput("Annulation"),                        
                            HTML("<i><u> Commentaire</u> : Les frais de nettoyage semblent augmenter à mesure que la politique d'annulation devient plus stricte.  </i>")
                            
                        )
                 )
        ),
        fluidRow(width = 12,
                 column(width = 6, 
                        box(
                          width = NULL,
                          height = "550px",
                          title = "Comparaison du taux de réponse en fonction des commentaires par mois",
                          solidHeader = TRUE,
                          status = "warning",
                          plotOutput("ResponseReviews"),
                          HTML("<i><u> Commentaire</u> : Nous constatons que les hôtes qui répondent plus rapidement reçoivent davantage de commentaires de la part des visiteurs, ce qui est conforme à nos attentes.  </i>")
                          
                        )),
                 column(width = 6,
                        box(
                          width = NULL,
                          height = "550px",
                          title = "BarChart nombre de chambre avec facteur nombre de toilettes",
                          solidHeader = TRUE,
                          status = "warning",
                          plotOutput("ChambreAccommodates"),
                          HTML("<i><u> Commentaire</u> : Nous observons qu'un Airbnb proposant une seule chambre peut accueillir jusqu'à 6 personnes, ce qui est surprenant compte tenu de la superfice typique des appartements parisiens.  </i>")
                          
                        )),
        ),
        fluidRow(width=12,
                 column(width=6, height="650px",
                        box(
                          width = NULL,
                          height="550px",
                          title = "Distribution de la surface en fonction du type de propriété",
                          solidHeader = TRUE,
                          status="warning",
                          plotOutput("Surface1"),
                          HTML("<i><u> Commentaire</u> : Le graphique met en évidence une tendance selon laquelle les maisons  et les lofts ont en moyenne des superficies plus importantes que les appartements, ce qui suggère une diversité significative dans les tailles de propriétés en fonction du type de bien immobilier. En outre, la variance des superficies des appartements est plus faible que celui des maisons et des lofts. </i>")
                          
                        )),
                 column(width=6, height="650px",
                        box(
                          width = NULL,
                          height="550px",
                          status="warning",
                          title = "Distribution de la surface en fonction du nombre de chambre",
                          solidHeader = TRUE,
                          plotOutput("Surface2"),
                          HTML("<i><u> Commentaire</u> : Le graphique illustre de manière évidente que la moyenne de la superficie croît à mesure que le nombre de chambres augmente, ce qui est conforme à nos attentes. </i>")
                          
                        )))
        ),
        tabItem(
          tabName = "map",
          fluidRow(
            column(width = 12,
                   box(width=NULL, solidHeader=TRUE, status="warning",
                       h2(HTML("<b>Découverte des Airbnbs à Paris</b>"), align = "center")),
                   box(width=NULL, title = "Description de la carte", status="warning", solidHeader = TRUE,
                       HTML("Explorez la localisation des Airbnbs à Paris avec notre carte interactive. </p><p> Personnalisez votre recherche en utilisant les filtres situés dans la bar latéral.</p><p> Le curseur de prix s'ajuste automatiquement pour refléter la fourchette de prix des Airbnbs sélectionnés, vous aidant ainsi à rester dans votre budget tout en choisissant parmi une variété d'options.</p> <p>Au survole des icônes sur la carte, un label informatif s'affiche, fournissant des détails essentiels sur chaque hébergement.</p> <p>Au clic des icônes sur la carte, un popup s'affiche, fournissant la description et une photo pour chaque hébergement. </p><p>De plus, notre carte propose une fonctionnalité unique. En sélectionnant un ou plusieurs sports olympiques, les Airbnb à proximité des sites olympiques associés à ces sports s'illuminent en couleurs distinctes. Les plus proches apparaissent en vert, les suivants en orange, et les plus éloignés en rouge, offrant ainsi une visualisation intuitive des options d'hébergement en fonction de la proximité aux événements sportifs.</p><p>Naviguez avec aisance à travers notre carte interactive pour découvrir les meilleurs hébergements correspondant à vos préférences et profitez pleinement de votre expérience olympique.</p>")
                   ),
                   box(width = NULL, solidHeader = TRUE,
                       leafletOutput("airbnbMap", height = 650)
                   ),
                 box(
                   width = NULL,
                   status = "warning",
                   selectInput("selectedSports", "Trouver l'airbnb le plus proche des sites olympiques de votre choix ",
                               choices = c("", "Football","Athlétisme, Para athlétisme, Rugby","Sports équestres, Para équitation, Pentathlon moderne","Para athlétisme", "VTT",
                                           "Escrime, Escrime fauteuil, Taekwondo, Para taekwondo","Tir à l'arc, Para tir à l'arc, Athlétisme, Cyclisme sur route","Escalade sportive",
                                           "Cyclisme sur route, Athlétisme", "Basketball, Basket fauteuil, Gymnastique artistique, trampoline", "Boxe, Escrime, Pentathlon moderne, Volley-ball assis",
                                           "Haltérophilie, Handball, Goalball", "Basketball 3x3, BMX freestyle, Breaking, Skateboard", "Hockey", "Judo, Para judo, Lutte, Rugby fauteuil",
                                           "Volleyball de plage, Cécifoot", "BMX race", "Badminton, Para Badminton, Gymnastique rythmique, Para powerlifting", "Goalball", "Natation artistique, Plongeon, Water-polo",
                                           "Volleyball, Boccia", "Tennis de table, Para tennis de table", "Athlétisme", "Natation, Para natation, Water-polo", "Canoë, Para canoë, Aviron, Para aviron",
                                           "Golf", "Cyclisme sur piste, Para cyclisme sur piste", "Tennis, Tennis fauteuil, Boxe", "Para cyclisme sur route"),
                               selected = "", multiple = TRUE
                   ))
                 )
        ))
      )
    ),
    skin="yellow"
    )
  
  server <- function(input, output, session) {
    
    ############# GRAPHIQUE ###############
    
    output$obsBox <- shinydashboard::renderValueBox({
      shinydashboard::valueBox(
        value = nrow(airbnb_data),
        "Observation",
        icon = icon("list-alt"),
        color = "purple"
      )
    })
    
    output$varsBox <- shinydashboard::renderValueBox({
      shinydashboard::valueBox(
        value = ncol(airbnb_data),
        "Variables",
        icon = icon("th"),
        color = "green"
      )
    })
    
    output$BedType <- renderPlot({
      
      bed_type_counts <- table(airbnb_data$`Bed Type`)
      bed_type_percentages <- prop.table(bed_type_counts)
      
      # Créer la pie chart avec ggplot2
      pie_data <- as.data.frame(bed_type_percentages)
      
      
      ggplot(data = pie_data,
             aes(x = "", y = Freq, fill = Var1)) +
        geom_bar(stat = "identity", width = 1) +
        geom_text_repel(aes(y = cumsum(sort(Freq, decreasing = TRUE)) - sort(Freq, decreasing = TRUE) / 2,
                            label = percent(sort(Freq, decreasing = TRUE)), size = 1),
                        show.legend = FALSE, hjust=0.3,nudge_x=0.3) +
        theme(axis.text.x = element_blank()) +
        coord_polar("y") +
        theme_void() +
        guides(fill = guide_legend(title = "Types de lits", reverse = TRUE))
      
      
    })
    
    output$sunburstPlot <- renderSunburst({
      sunburst(agg, count = TRUE, legend=FALSE)
    }) 
    
    output$cartographie <- renderPlot({
      
      paris <- st_read("75-paris/75-.shp")
      
      INSEE_COM_a_arrondissement <- function(zipcode) {
        Arrondissement <- as.character(zipcode)
        if (Arrondissement == "75101") {
          return("1er")
        } else if (substr(Arrondissement, nchar(Arrondissement) - 1, nchar(Arrondissement) - 1) == "0") {
          return(paste0(substr(Arrondissement, nchar(Arrondissement), nchar(Arrondissement)), "ème"))
        } else {
          return(paste0(substr(Arrondissement, nchar(Arrondissement) - 1, nchar(Arrondissement)), "ème"))
        }
      }
      
      paris <- paris %>% 
        mutate(Arrondissement = sapply(INSEE_COM, INSEE_COM_a_arrondissement))
      
      airbnb_counts <- airbnb_data %>%
        count(Arrondissement) 
      
      paris <- paris %>%
        left_join(airbnb_counts, by = "Arrondissement")
      
      paris$n <- as.numeric(paris$n)
    
      
      ggplot(data = paris) +
        geom_sf(aes(fill = n), color = "black") +  
        scale_fill_gradientn(colors = c("wheat", "red"), 
                             values = scales::rescale(c(min(paris$n), max(paris$n))),
                             name = "Nombre d'Airbnb")  +  
        ggplot2::geom_sf_text(aes(label = Arrondissement), color = "black", size = 2) +
        theme_minimal() +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(),
              axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank()) 
    })
    
    output$histogramme <- renderPlot({
      
      # Dessiner l'histogramme
      ggplot(airbnb_data, aes(x = Price, fill = "#FFA500")) +
        geom_histogram(binwidth = 50, color = "white", fill = "#FFA500") +
        theme_minimal() +
        labs(x = "Prix", y = "Fréquence")
    })
    
    output$plot5 <- renderLeaflet({
      zipcode_prices <- aggregate(cbind(Price, Longitude, Latitude) ~ Zipcode, data = airbnb_data, FUN = mean)
      leaflet(zipcode_prices) %>%
        addTiles() %>%  # Add background tiles
        addHeatmap(
          data = zipcode_prices,
          lng = ~Longitude,  # Use the mean 'Longitude' as the longitude for the heatmap
          lat = ~Latitude,   # Use the mean 'Latitude' as the latitude for the heatmap
          intensity = ~Price,  # Use mean 'Price' as the intensity for the heatmap
          blur = 20,  # Adjust the blur radius
          max = max(zipcode_prices$Price, na.rm = TRUE)   # Adjust the maximum intensity value
        ) %>%
        addLegend( # Add a legend for the heatmap
          position = "bottomright",
          pal = colorNumeric(palette = "viridis", domain = zipcode_prices$Price),
          values = ~Price,
          title = "Average Price"
        ) %>%
        setView(lng = mean(zipcode_prices$Longitude), lat = mean(zipcode_prices$Latitude), zoom = 12)
    })
    
    
    
    output$priceGauge <- renderGauge ({
      
      mean_price <- mean(airbnb_data$Price)
      max_price <- max(airbnb_data$Price)
      min_price <- min(airbnb_data$Price)
      price_3 <- min_price+2*(min_price+max_price)/3
      price_2 <- min_price+(min_price+max_price)/3
      
      
      gauge(mean_price, min = min_price, max = max_price, gaugeSectors(
        success = c(price_3, max_price), warning = c(price_2, price_3), danger = c(min_price, price_2))) })
    
    output$noteGauge <- renderGauge ({
      
      mean_note <- mean(airbnb_data$Note)
      
      gauge(mean_note, min = 0, max = 5, gaugeSectors(
        success = c(4, 5), warning = c(3, 4), danger = c(0, 3))) })
    
    output$responseGauge <- renderGauge ({
      
      NONA_data <- airbnb_data[!is.na(airbnb_data$`Host Response Rate`), ]
      mean_response <- mean(NONA_data$`Host Response Rate`)
      
      gauge(mean_response, min = 0, max = 100, symbol="%", gaugeSectors(
        success = c(70, 100), warning = c(40, 50), danger = c(0, 40))) })
    
    output$reviewGauge <- renderGauge ({
      
      mean_review <- mean(airbnb_data$`Number of Reviews`)
      max_review <- max(airbnb_data$`Number of Reviews`)
      min_review <- min(airbnb_data$`Number of Reviews`)
      review_3 <- min_review+2*(min_review+max_review)/3
      review_2 <- min_review+(min_review+max_review)/3
      
      gauge(mean_review, min = min_review, max = max_review, gaugeSectors(
        success = c(review_3, max_review), warning = c(review_2, review_3), danger = c(min_review, review_2))) })
    
    
    output$noteTable <- renderDataTable({
      
      airbnb_data %>%
        group_by(Arrondissement) %>%
        summarize(Note_moyenne = mean(Note, na.rm = TRUE)) %>%
        arrange(desc(Note_moyenne)) %>%
        DT::datatable(colnames = c('Arrondissement', 'Note Moyenne'), options = list(pageLength = 5))
    })

   
   
   output$reviewsTable <- renderDataTable({
  
    airbnb_data %>%
    group_by(Arrondissement) %>%
    summarize(Nombre_moyen_de_commentaire_par_mois = mean(`Reviews per Month`, na.rm = TRUE)) %>%
    arrange(desc(Nombre_moyen_de_commentaire_par_mois))%>%
    DT::datatable(colnames = c('Arrondissement', 'Nombre moyen de commentaires par mois'), options = list(pageLength = 5))
})
    
    output$responseRateTable <- renderDataTable({
      
      airbnb_data %>%
        group_by(Arrondissement) %>%
        summarize(Taux_de_réponse_moyen = mean(`Host Response Rate`, na.rm = TRUE)) %>%
        arrange(desc(Taux_de_réponse_moyen)) %>%
        DT::datatable(colnames = c('Arrondissement', 'Taux de réponse Moyen'), options = list(pageLength = 5))
    })
    
    output$Annulation <- renderPlot({
     
      
      ggplot(airbnb_data, aes(x = `Cancellation Policy`, y = `Cleaning Fee`, fill=`Cancellation Policy`)) +
        geom_boxplot() +
        labs(x = "Politique d'annulation",
             y = "Frais de nettoyage") +
        scale_fill_brewer(palette = "Set3") + 
        theme_minimal()
    })
    
    
    output$ResponseReviews <- renderPlot({
      
    NONA_data <- airbnb_data[!is.na(airbnb_data$`Host Response Time`), ]
    NONA_data$`Host Response Time` <- factor(NONA_data$`Host Response Time`, levels = c("within an hour", "within a few hours", "within a day", "a few days or more"))
    
    NONA_data$Reviews_per_month <-0
    
    for (i in 1:length(NONA_data$`Reviews per Month`)){
      if (NONA_data$`Reviews per Month`[i]<=1){
        NONA_data$Reviews_per_month[i]="<=1"
      } else if (NONA_data$`Reviews per Month`[i]<=2){
        NONA_data$Reviews_per_month[i]="<=2"
      } else if (NONA_data$`Reviews per Month`[i]<=3){
        NONA_data$Reviews_per_month[i]="<=3"
      } else if (NONA_data$`Reviews per Month`[i]<=4){
        NONA_data$Reviews_per_month[i]="<=4"
      } else {
        NONA_data$Reviews_per_month[i]=">=5"}
    }
    
    NONA_data$Reviews_per_month <- factor(NONA_data$Reviews_per_month)
    
    # Calculate proportions for each Note within each Host Response Time
    proportions <- NONA_data %>%
      group_by(`Host Response Time`, Reviews_per_month) %>%
      summarise(Count = n()) %>%
      mutate(Total = sum(Count), Proportion = Count / Total) %>%
      ungroup()
    
    
    # Plot
    ggplot(proportions, aes(x = `Host Response Time`, y = Proportion, fill = Reviews_per_month)) +
      geom_bar(stat = "identity", position = "fill") +
      coord_flip() +
      scale_y_continuous(labels = scales::percent_format()) +
      labs(x = "Taux de réponse des hôtes", y = "", fill = "Commentaires par mois") +
      scale_fill_brewer(palette = "Set2") +
      theme_minimal()
    
    })
    
    output$ChambreAccommodates <-renderPlot({
      NONA_data <- airbnb_data[!is.na(airbnb_data$Accommodates), ]
      table(NONA_data$Accommodates)
      
      NONA_data$Accommodates <- as.factor(NONA_data$Accommodates)
      NONA_data$Bedrooms <- as.factor(NONA_data$Bedrooms)
      
      ggplot(NONA_data)+
        geom_bar(aes(x=Bedrooms, fill=Accommodates)) +
        coord_flip()+
        labs(x = "Nombre de Chambres", y = "", fill = "Nombre max de personnes") +
        scale_fill_brewer(palette = "Set3") +
        theme_minimal()
    })
    
    output$Surface1 <- renderPlot({
    
      ggplot(airbnb_data, aes(x = SquareMeters, color = `Property Type`)) +
      geom_density() + 
      labs(x = "Surface", y = "Densité de distribution", color = "Type de propriété") +
      theme_minimal() +
      theme(legend.position = "right") 
    })
    
    output$Surface2 <- renderPlot({
      airbnb_data$Bedrooms <- as.factor(airbnb_data$Bedrooms)
      ggplot(airbnb_data, aes(x = SquareMeters, color = Bedrooms)) +
        geom_density() +  # Create the density plot
        labs(x = "Surface", y = "Densité de distribution", color = "Chambres") +
        theme_minimal() +
        theme(legend.position = "right") 
    })
    
    ############### CARTE #################
    output$airbnbMap <- renderLeaflet({
      leaflet() %>%
        addTiles() %>%
        addAwesomeMarkers(
          data = airbnb_data,
          lat = ~Latitude,
          lng = ~Longitude,
          popup = ~Popup,
          label = lapply(airbnb_data$Label,HTML),
          icon = icons
        ) %>%
        fitBounds(
          min(airbnb_data$Longitude),
          min(airbnb_data$Latitude),
          max(airbnb_data$Longitude),
          max(airbnb_data$Latitude)
        )
    })
    
    ############# FILTRES ################
    
    previous_selectedPriceRange <- reactiveVal(c(min_price, max_price))
    filtre_manuel <-reactiveVal(FALSE)
    
    observeEvent(c(input$selectedArrondissement, input$minSurface, input$minAccommodates, input$selectedRoomType,input$selectedRating, input$selectedPriceRange, input$selectedSports), {
      
      selected_airbnb <- airbnb_data
      
      if (!is.null(input$selectedArrondissement) && length(input$selectedArrondissement) > 0 && any(input$selectedArrondissement != "")){
        selected_airbnb <- selected_airbnb[selected_airbnb$Arrondissement %in% input$selectedArrondissement, ]
      }
      
      if (!is.null(input$minSurface) && !is.na(input$minSurface) && input$minSurface > 0) {
        selected_airbnb <- selected_airbnb[selected_airbnb$SquareMeters >= input$minSurface, ]
      }
      
      if ((!is.null(input$minAccommodates) && !is.na(input$minAccommodates) && input$minAccommodates > 0)) {
        selected_airbnb <- selected_airbnb[selected_airbnb$Accommodates >= input$minAccommodates, ]
      }
      
      if (!is.null(input$selectedRoomType) && length(input$selectedRoomType) > 0) {
        selected_airbnb <- selected_airbnb[selected_airbnb$`Room Type` %in% input$selectedRoomType, ]
      }
      
      if (!is.null(input$selectedRating) && length(input$selectedRating) > 0 && any(input$selectedRating != "")){
        selected_airbnb <- selected_airbnb[selected_airbnb$Note %in% input$selectedRating, ]
      }
      
      if ((filtre_manuel()) && (!is.null(input$selectedPriceRange) && !is.null(input$selectedPriceRange[1]) && !is.null(input$selectedPriceRange[2]))) {
        selected_airbnb <- selected_airbnb[selected_airbnb$Price >= input$selectedPriceRange[1] & selected_airbnb$Price <= input$selectedPriceRange[2], ]
      }
      
      
      # Vérifier si le curseur de prix a été modifié manuellement
      
      if ((!is.null(input$selectedPriceRange)) && (length(input$selectedPriceRange) > 0) && (input$selectedPriceRange[1]==previous_selectedPriceRange()[1]) && (input$selectedPriceRange[2]==previous_selectedPriceRange()[2])) {
        
        updateSliderInput(session, "selectedPriceRange", value = c(min(selected_airbnb$Price), max(selected_airbnb$Price)))
        previous_selectedPriceRange(c(min(selected_airbnb$Price), max(selected_airbnb$Price)))
        
        selected_airbnb <- selected_airbnb[selected_airbnb$Price >= input$selectedPriceRange[1] & selected_airbnb$Price <= input$selectedPriceRange[2], ]
        
      }
      else{
        # Filtrer manuellement par rapport au curseur de prix
        if (!is.null(input$selectedPriceRange) && !is.null(input$selectedPriceRange[1]) && !is.null(input$selectedPriceRange[2])) {
          selected_airbnb <- selected_airbnb[selected_airbnb$Price >= input$selectedPriceRange[1] & selected_airbnb$Price <= input$selectedPriceRange[2], ]
        }
        
        filtre_manuel(TRUE)
        previous_selectedPriceRange(input$selectedPriceRange)
        
      }
      
      
      # Mettre à jour la carte 
      
      if (!is.null(input$selectedSports) && input$selectedSports != ""){
        selected_JO <-JO
        selected_JO <- selected_JO[selected_JO$sports %in% input$selectedSports, ]
        
        selected_airbnb$distance <- 0
        
        for (i in 1:length(input$selectedSports)){
          distance_i <- calculateDistancesToPoint(selected_airbnb$Latitude, selected_airbnb$Longitude, selected_JO$Latitude[i], selected_JO$Longitude[i])
          selected_airbnb$distance <- selected_airbnb$distance + distance_i
        }
        
        
        quantiles <- quantile(selected_airbnb$distance, probs = c(1/3, 2/3))
        
        green <- c()
        orange <- c()
        red <-c()
        
        for (i in 1:length(selected_airbnb$distance)){
          if (selected_airbnb$distance[i] <= quantiles[1]) {
            green<-c(green,i)
          } else if (selected_airbnb$distance[i] <= quantiles[2]) {
            orange <- c(orange,i)
          } else {
            red <- c(red,i) 
          }}
        
        selected_airbnb_green <- selected_airbnb[green,]
        selected_airbnb_orange <- selected_airbnb[orange,]
        selected_airbnb_red <- selected_airbnb[red,]
        
        
        if ((nrow(selected_airbnb_green) > 0) && (nrow(selected_airbnb_orange) > 0) && (nrow(selected_airbnb_red) > 0)) {
          selected_airbnb_green <- selected_airbnb_green[complete.cases(selected_airbnb_green$Longitude, selected_airbnb_green$Latitude), ]
          selected_airbnb_orange <- selected_airbnb_orange[complete.cases(selected_airbnb_orange$Longitude, selected_airbnb_orange$Latitude), ]
          selected_airbnb_red <- selected_airbnb_red[complete.cases(selected_airbnb_red$Longitude, selected_airbnb_red$Latitude), ]
          
          leafletProxy("airbnbMap") %>%
            clearMarkers() %>%
            addAwesomeMarkers(
              data = selected_airbnb_green,
              lat = ~Latitude,
              lng = ~Longitude,
              popup = ~Popup,
              label = ~Label,
              icon = icons_green
            ) %>%
            addAwesomeMarkers(
              data = selected_airbnb_orange,
              lat = ~Latitude,
              lng = ~Longitude,
              popup = ~Popup,
              label = ~Label,
              icon = icons_orange
            ) %>%
            addAwesomeMarkers(
              data = selected_airbnb_red,
              lat = ~Latitude,
              lng = ~Longitude,
              popup = ~Popup,
              label = ~Label,
              icon = icons_red
            ) %>%
            fitBounds(
              min(selected_airbnb$Longitude),
              min(selected_airbnb$Latitude),
              max(selected_airbnb$Longitude),
              max(selected_airbnb$Latitude)
            )
        } else {
          leafletProxy("airbnbMap") %>%
            clearMarkers()
        }
      }else{
        if (nrow(selected_airbnb) > 0) {
          selected_airbnb <- selected_airbnb[complete.cases(selected_airbnb$Longitude, selected_airbnb$Latitude), ]
          
          leafletProxy("airbnbMap") %>%
            clearMarkers() %>%
            addAwesomeMarkers(
              data = selected_airbnb,
              lat = ~Latitude,
              lng = ~Longitude,
              popup = ~Popup,
              label = ~Label,
              icon = icons
            ) %>%
            fitBounds(
              min(selected_airbnb$Longitude),
              min(selected_airbnb$Latitude),
              max(selected_airbnb$Longitude),
              max(selected_airbnb$Latitude)
            )
        } else {
          leafletProxy("airbnbMap") %>%
            clearMarkers()
        }
      }
      
      output$plot4 <- renderPlot({
        # Filtrer les données en fonction de la sélection d'arrondissements
        set3_colors <- brewer.pal(12, "Set3")
        
        
        
        # Dessiner l'histogramme
        ggplot(selected_airbnb, aes(x = Price, fill = "#FFA500")) +
          geom_histogram(binwidth = 50, color = "white", fill = "#FFA500") +
          theme_minimal() +
          labs(x = "Prix", y = "Fréquence")
      })
    })
  }
  
  # Exécuter l'application
  shinyApp(ui, server)