library(shiny)
library(leaflet)
library(dplyr)
library(sf)
library(rgbif)
library(geobr)
library(shinydashboard)
library(shinyWidgets)
library(plotly)
library(DT)
library(shinycssloaders)
library(shinyjs)

# Lista de estados brasileiros
estados_br <- c(
  "Todos",
  "Acre" = "AC", "Alagoas" = "AL", "Amapá" = "AP", "Amazonas" = "AM",
  "Bahia" = "BA", "Ceará" = "CE", "Distrito Federal" = "DF",
  "Espírito Santo" = "ES", "Goiás" = "GO", "Maranhão" = "MA",
  "Mato Grosso" = "MT", "Mato Grosso do Sul" = "MS", "Minas Gerais" = "MG",
  "Pará" = "PA", "Paraíba" = "PB", "Paraná" = "PR", "Pernambuco" = "PE",
  "Piauí" = "PI", "Rio de Janeiro" = "RJ", "Rio Grande do Norte" = "RN",
  "Rio Grande do Sul" = "RS", "Rondônia" = "RO", "Roraima" = "RR",
  "Santa Catarina" = "SC", "São Paulo" = "SP", "Sergipe" = "SE",
  "Tocantins" = "TO"
)

# UI
ui <- dashboardPage(
  dashboardHeader(
    title = span("Biodiversidade Brasileira", 
                 style = "font-size: 24px; font-weight: 300; letter-spacing: 0.5px;"),
    titleWidth = 300
  ),
  dashboardSidebar(
    width = 300,
    tags$head(
      tags$style(HTML("
        /* Estilo geral */
        body, .main-header .logo, .main-header .navbar {
          font-family: 'Roboto', 'Helvetica Neue', Helvetica, Arial, sans-serif;
        }
        .skin-blue .main-sidebar {
          background-color: #2c3e50;
          box-shadow: 2px 0 10px rgba(0,0,0,0.1);
        }
        .skin-blue .sidebar a {
          color: #ecf0f1;
        }
        /* Estilo dos inputs */
        .form-control {
          border-radius: 4px;
          border: 1px solid #bdc3c7;
          transition: all 0.3s;
        }
        .form-control:focus {
          border-color: #3498db;
          box-shadow: 0 0 5px rgba(52,152,219,0.3);
        }
        /* Estilo dos botões */
        .btn {
          border-radius: 4px;
          font-weight: 500;
          letter-spacing: 0.5px;
          transition: all 0.3s;
          text-transform: none;
        }
        #addSpecies {
          background-color: #27ae60;
          border: none;
        }
        #addSpecies:hover {
          background-color: #2ecc71;
          transform: translateY(-2px);
        }
        #clearSelection {
          background-color: #c0392b;
          border: none;
        }
        #clearSelection:hover {
          background-color: #e74c3c;
          transform: translateY(-2px);
        }
        /* Estilo dos value boxes */
        .info-box {
          border-radius: 4px;
          overflow: hidden;
        }
        .info-box-icon {
          height: 90px;
          width: 90px;
          font-size: 45px;
        }
        .info-box-content {
          padding-left: 90px;
        }
        /* Estilo das caixas */
        .box {
          border-radius: 4px;
          box-shadow: 0 2px 10px rgba(0,0,0,0.1);
        }
        .box-header {
          border-bottom: 1px solid #f4f4f4;
        }
        /* Ajuste para o container dos inputs e botões */
        .sidebar-form {
          padding: 0 15px;
        }
        /* Responsividade */
        @media (max-width: 768px) {
          .sidebar {
            padding-top: 60px;
          }
        }
        
        /* Ajuste específico para os botões */
        #addSpecies, #clearSelection, .download-button {
          width: 100% !important;
          margin-left: 0 !important;
          margin-right: 0 !important;
        }
        
        /* Container para os botões */
        .button-container {
          padding: 10px 0;
        }
        
        /* Ajuste do hr */
        .sidebar-form hr {
          margin: 15px 0;
        }
      "))
    ),
    
    sidebarMenu(
      menuItem("Mapa", tabName = "mapa", icon = icon("map")),
      menuItem("Estatísticas", tabName = "stats", icon = icon("chart-bar")),
      menuItem("Sobre", tabName = "sobre", icon = icon("info-circle")),
      br(),
      div(
        style = "padding: 0 15px;",
        selectInput("estado", "Estado:",
                    choices = estados_br,
                    selected = "Todos"),
        selectizeInput("biomas", "Biomas:",
                       choices = NULL,
                       multiple = TRUE,
                       options = list(
                         placeholder = "Selecione os biomas...",
                         plugins = list("remove_button")
                       )),
        textInput("especie", "Espécie:", 
                  placeholder = "Digite o nome científico..."),
        dateRangeInput("dateRange", "Período:",
                       start = "2000-01-01",
                       end = Sys.Date(),
                       separator = " até ",
                       format = "dd/mm/yyyy"),
        actionButton("addSpecies", "Adicionar espécie",
                     icon = icon("plus"),
                     style = "width: 100%; margin-bottom: 10px;"),
        actionButton("clearSelection", "Limpar seleção",
                     icon = icon("trash"),
                     style = "width: 100%;"),
        hr(),
        downloadButton("downloadData", "Baixar dados",
                       style = "width: 100%; background-color: #3498db; color: white; border: none;")
      )
    )
  ),
  dashboardBody(
    useShinyjs(),
    tags$head(
      tags$style(HTML("
        body {
          font-family: 'Roboto', sans-serif;
        }
        .content-wrapper {
          background-color: #f5f7fa;
        }
        .info-box {transition: all 0.3s;}
        .info-box:hover {
          transform: translateY(-5px);
          box-shadow: 0 5px 15px rgba(0,0,0,0.3);
        }
        .leaflet-container {
          border-radius: 4px;
          box-shadow: 0 0 10px rgba(0,0,0,0.1);
        }
        .box {
          border-top: 3px solid #3c8dbc;
          transition: all 0.3s;
        }
        .box:hover {
          box-shadow: 0 5px 15px rgba(0,0,0,0.1);
        }
        /* Estilo da tabela */
        .dataTables_wrapper {
          padding: 15px;
          background: white;
          border-radius: 4px;
          box-shadow: 0 2px 10px rgba(0,0,0,0.05);
        }
        .dataTable th {
          background-color: #f8f9fa;
        }
        /* Estilo do gráfico */
        .plotly {
          border-radius: 4px;
          box-shadow: 0 2px 10px rgba(0,0,0,0.05);
        }
      "))
    ),
    tabItems(
      tabItem(tabName = "mapa",
              fluidRow(
                box(
                  width = 9,
                  withSpinner(leafletOutput("mapa", height = 600)),
                  footer = "Clique nos pontos para ver detalhes das observações"
                ),
                column(
                  width = 3,
                  valueBoxOutput("especiesBox", width = NULL),
                  valueBoxOutput("obsBox", width = NULL),
                  valueBoxOutput("biomasBox", width = NULL)
                )
              )
      ),
      tabItem(tabName = "stats",
              fluidRow(
                box(
                  width = 12,
                  plotlyOutput("especiesPerBioma")
                )
              ),
              fluidRow(
                tabBox(
                  width = 12,
                  tabPanel("Estatísticas por Bioma",
                           dataTableOutput("biomesStats")),
                  tabPanel("Lista de Espécies",
                           dataTableOutput("speciesTable"))
                )
              )
      ),
      tabItem(tabName = "sobre",
              fluidRow(
                box(
                  width = 12,
                  title = "Sobre o Projeto",
                  status = "primary",
                  solidHeader = TRUE,
                  HTML("
        <h3>Biodiversidade Brasileira</h3>
        <p>Este aplicativo visa apresentar dados sobre a biodiversidade nos diferentes biomas do Brasil.
        Os dados são obtidos através da API do GBIF (Global Biodiversity Information Facility).</p>
        
        <h3>Como Usar o Aplicativo</h3>
        <ul>
          <li><strong>Selecione um estado:</strong> Use o menu suspenso para escolher um estado específico ou 'Todos' para ver dados de todo o Brasil.</li>
          <li><strong>Filtre por biomas:</strong> Selecione um ou mais biomas para focar sua pesquisa.</li>
          <li><strong>Busque por espécies:</strong> Digite o nome científico de uma espécie no campo de busca.</li>
          <li><strong>Defina o período:</strong> Use o seletor de datas para especificar o intervalo de tempo das observações.</li>
          <li><strong>Adicione espécies:</strong> Clique em 'Adicionar espécie' para incluir a espécie buscada na visualização.</li>
          <li><strong>Explore o mapa:</strong> Observe a distribuição geográfica das espécies selecionadas.</li>
          <li><strong>Analise estatísticas:</strong> Veja gráficos e tabelas na aba 'Estatísticas' para uma visão mais detalhada.</li>
          <li><strong>Baixe os dados:</strong> Use o botão 'Baixar dados' para obter um arquivo com as informações visualizadas.</li>
        </ul>

        <h3>Biomas Brasileiros</h3>
        <p>O Brasil possui seis grandes biomas, cada um com características únicas e representado por uma cor específica no mapa:</p>
        <div style='display: flex; flex-wrap: wrap; gap: 20px;'>
          <div style='display: flex; width: 100%; justify-content: space-between; margin-bottom: 20px;'>
            <div style='flex: 1; min-width: 30%; max-width: 30%;'>
              <h4 style='color: #1B5E20;'>Amazônia</h4>
              <p>Maior floresta tropical do mundo, abrigando uma imensa biodiversidade.</p>
            </div>
            <div style='flex: 1; min-width: 30%; max-width: 30%;'>
              <h4 style='color: #2E7D32;'>Mata Atlântica</h4>
              <p>Floresta costeira com alta taxa de endemismo, muito afetada pela urbanização.</p>
            </div>
            <div style='flex: 1; min-width: 30%; max-width: 30%;'>
              <h4 style='color: #F9A825;'>Cerrado</h4>
              <p>Savana tropical com grande variedade de plantas e animais adaptados.</p>
            </div>
          </div>
          <div style='display: flex; width: 100%; justify-content: space-between;'>
            <div style='flex: 1; min-width: 30%; max-width: 30%;'>
              <h4 style='color: #FF8F00;'>Caatinga</h4>
              <p>Único bioma exclusivamente brasileiro, caracterizado por vegetação adaptada à seca.</p>
            </div>
            <div style='flex: 1; min-width: 30%; max-width: 30%;'>
              <h4 style='color: #A1887F;'>Pampa</h4>
              <p>Campos do sul do Brasil, com rica fauna e flora campestres.</p>
            </div>
            <div style='flex: 1; min-width: 30%; max-width: 30%;'>
              <h4 style='color: #0277BD;'>Pantanal</h4>
              <p>Maior planície alagável do mundo, com ecossistema único e diversificado.</p>
            </div>
          </div>
        </div>

        <h3>Espécies Populares no Brasil</h3>
        <p>Aqui estão alguns exemplos de espécies bem conhecidas que você pode buscar no aplicativo:</p>
        
        <div style='display: flex; flex-wrap: wrap; gap: 20px;'>
          <div style='flex: 1; min-width: 300px;'>
            <h4>Animais</h4>
            <ul style='list-style-type: none; padding-left: 0;'>
              <li>Sabiá-laranjeira (<em>Turdus rufiventris</em>)</li>
              <li>Quero-quero (<em>Vanellus chilensis</em>)</li>
              <li>Bem-te-vi (<em>Pitangus sulphuratus</em>)</li>
              <li>Tucano-toco (<em>Ramphastos toco</em>)</li>
              <li>Arara-azul-grande (<em>Anodorhynchus hyacinthinus</em>)</li>
              <li>Onça-pintada (<em>Panthera onca</em>)</li>
              <li>Mico-leão-dourado (<em>Leontopithecus rosalia</em>)</li>
              <li>Lobo-guará (<em>Chrysocyon brachyurus</em>)</li>
              <li>Tamanduá-bandeira (<em>Myrmecophaga tridactyla</em>)</li>
              <li>Boto-cor-de-rosa (<em>Inia geoffrensis</em>)</li>
              <li>Jabuti-piranga (<em>Chelonoidis carbonaria</em>)</li>
              <li>Jararaca (<em>Bothrops jararaca</em>)</li>
              <li>Sucuri-verde (<em>Eunectes murinus</em>)</li>
              <li>Sapo-cururu (<em>Rhinella marina</em>)</li>
              <li>Perereca-de-capacete (<em>Trachycephalus typhonius</em>)</li>
              <li>Rã-pimenta (<em>Leptodactylus labyrinthicus</em>)</li>
              <li>Tambaqui (<em>Colossoma macropomum</em>)</li>
              <li>Dourado (<em>Salminus brasiliensis</em>)</li>
              <li>Pirarucu (<em>Arapaima gigas</em>)</li>
              <li>Borboleta-azul (<em>Morpho menelaus</em>)</li>
              <li>Besouro-hércules (<em>Dynastes hercules</em>)</li>
              <li>Formiga-saúva (<em>Atta cephalotes</em>)</li>
            </ul>
          </div>
          
          <div style='flex: 1; min-width: 300px;'>
            <h4>Plantas</h4>
            <ul style='list-style-type: none; padding-left: 0;'>
              <li>Ipê-amarelo (<em>Handroanthus albus</em>)</li>
              <li>Pau-brasil (<em>Paubrasilia echinata</em>)</li>
              <li>Jatobá (<em>Hymenaea courbaril</em>)</li>
              <li>Araucária (<em>Araucaria angustifolia</em>)</li>
              <li>Açaí (<em>Euterpe oleracea</em>)</li>
              <li>Buriti (<em>Mauritia flexuosa</em>)</li>
              <li>Orquídea-da-mata (<em>Cattleya labiata</em>)</li>
              <li>Bromélias (<em>Aechmea blanchetiana</em>)</li>
              <li>Vitória-régia (<em>Victoria amazonica</em>)</li>
              <li>Mandioca (<em>Manihot esculenta</em>)</li>
            </ul>
          </div>
          
          <div style='flex: 1; min-width: 300px;'>
            <h4>Fungos</h4>
            <ul style='list-style-type: none; padding-left: 0;'>
              <li>Orelha-de-pau (<em>Pycnoporus sanguineus</em>)</li>
              <li>Cogumelo-do-sol (<em>Agaricus blazei</em>)</li>
              <li>Fungos bioluminescentes (<em>Neonothopanus gardneri</em>)</li>
              <li>Estrela-da-terra (<em>Geastrum saccatum</em>)</li>
            </ul>
          </div>
        </div>

        <h3>Sobre os Dados</h3>
        <p>Os dados apresentados neste aplicativo são fornecidos pelo GBIF (<em>Global Biodiversity Information Facility</em>), 
        uma rede internacional de dados de biodiversidade. É importante notar que estes dados representam observações 
        registradas e podem não refletir a distribuição completa das espécies. Fatores como esforço de amostragem, 
        acessibilidade de áreas e métodos de coleta podem influenciar os dados disponíveis.</p>

        <h3>Contribua</h3>
        <p>Você pode contribuir para o conhecimento da biodiversidade brasileira! Se você é um pesquisador, 
        naturalista amador ou entusiasta da natureza, considere compartilhar suas observações com plataformas 
        como o iNaturalist ou diretamente com o GBIF. Cada observação conta para entendermos melhor a distribuição 
        e o estado de conservação das espécies em nosso país.</p>
      ")
                )
              )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  especies <- reactiveVal(list())
  especies_cache <- reactiveVal(list())
  biomas_data <- reactiveVal(NULL)
  species_colors <- reactiveVal(list())
  
  encontrar_bioma <- function(lat, lon, biomas) {
    tryCatch({
      if (is.null(biomas) || nrow(biomas) == 0) return(rep(NA, length(lat)))
      pts <- st_as_sf(data.frame(lon = lon, lat = lat), coords = c("lon", "lat"), crs = 4326)
      intersects <- st_intersects(pts, biomas)
      biome_names <- biomas$name_biome[unlist(lapply(intersects, function(x) if(length(x) > 0) x[1] else NA))]
      return(biome_names)
    }, error = function(e) {
      return(rep(NA, length(lat)))
    })
  }
  
  # Cores dos biomas
  biome_colors <- c(
    "Amazônia" = "#1B5E20",
    "Mata Atlântica" = "#2E7D32",
    "Cerrado" = "#F9A825",
    "Caatinga" = "#FF8F00",
    "Pampa" = "#A1887F",
    "Pantanal" = "#0277BD",
    "Sistema Costeiro" = "#00BCD4"
  )
  
  get_species_color <- function(species_name) {
    colors <- species_colors()
    if (!species_name %in% names(colors)) {
      existing_colors <- unlist(colors)
      all_colors <- viridis::viridis(20)  # Using viridis for better color distinction
      available_colors <- setdiff(all_colors, existing_colors)
      
      if (length(available_colors) == 0) {
        # If we run out of colors, start recycling them
        new_color <- all_colors[length(colors) %% length(all_colors) + 1]
      } else {
        new_color <- available_colors[1]
      }
      
      colors[[species_name]] <- new_color
      species_colors(colors)
    }
    return(colors[[species_name]])
  }
  
  observeEvent(input$clearSelection, {
    especies(list())
    species_colors(list())
    showNotification("Todas as espécies foram removidas.", type = "message")
  })
  
  observeEvent(input$addSpecies, {
    req(input$especie)
    
    withProgress(message = 'Buscando dados da espécie...', {
      search_params <- list(
        scientificName = input$especie,
        country = "BR",
        hasCoordinate = TRUE,
        limit = 1000
      )
      
      if (input$estado != "Todos") {
        search_params$stateProvince <- input$estado
      }
      
      tryCatch({
        species_data <- do.call(occ_search, search_params)
        
        if (!is.null(species_data$data) && nrow(species_data$data) > 0) {
          especies_list <- especies()
          especies_list[[input$especie]] <- species_data$data
          especies(especies_list)
          
          get_species_color(input$especie)
          
          showNotification(
            sprintf("Adicionada espécie: %s (%d observações)", 
                    input$especie, nrow(species_data$data)),
            type = "message"
          )
        } else {
          showNotification("Nenhuma observação encontrada para esta espécie.", 
                           type = "warning")
        }
      }, error = function(e) {
        showNotification(
          sprintf("Erro ao buscar dados: %s", e$message),
          type = "error"
        )
      })
    })
  })
  
  observeEvent(input$addSpecies, {
    shinyjs::runjs("
      $('.leaflet-marker-icon').addClass('pulse');
      setTimeout(function() {
        $('.leaflet-marker-icon').removeClass('pulse');
      }, 1000);
    ")
  })
  
  output$timelinePlot <- renderPlotly({
    all_obs <- especies_filtradas()
    if (is.null(all_obs) || nrow(all_obs) == 0) return(NULL)
    
    timeline_data <- all_obs %>%
      mutate(date = as.Date(eventDate)) %>%
      group_by(date, especie) %>%
      summarise(count = n(), .groups = 'drop')
    
    species_list <- unique(timeline_data$especie)
    plot_data <- list()
    
    for (sp in species_list) {
      sp_data <- timeline_data %>% filter(especie == sp)
      plot_data[[sp]] <- list(
        x = sp_data$date,
        y = sp_data$count,
        name = sp,
        type = 'scatter',
        mode = 'lines',
        line = list(color = get_species_color(sp))
      )
    }
    
    plot_ly() %>%
      add_traces(plot_data) %>%
      layout(
        title = "Observações ao Longo do Tempo",
        xaxis = list(title = "Data"),
        yaxis = list(title = "Número de Observações"),
        hovermode = "x unified"
      )
  })
  
  # Update species per bioma plot to use consistent colors
  output$especiesPerBioma <- renderPlotly({
    all_obs <- especies_filtradas()
    if (is.null(all_obs) || nrow(all_obs) == 0) return(NULL)
    
    biome_counts <- all_obs %>%
      group_by(bioma, especie) %>%
      summarise(n_observacoes = n(), .groups = 'drop') %>%
      filter(!is.na(bioma))
    
    num_especies <- length(unique(biome_counts$especie))
    
    if (num_especies == 1) {
      p <- plot_ly(biome_counts, x = ~bioma, y = ~n_observacoes, type = "bar",
                   marker = list(color = biome_colors[biome_counts$bioma])) %>%
        layout(
          title = paste("Número de Observações por Bioma -", unique(biome_counts$especie)),
          xaxis = list(title = "Bioma"),
          yaxis = list(title = "Número de Observações")
        )
    } else {
      colors <- sapply(unique(biome_counts$especie), get_species_color)
      
      p <- plot_ly(biome_counts, x = ~bioma, y = ~n_observacoes, 
                   color = ~especie, colors = colors, type = "bar") %>%
        layout(
          title = "Número de Observações por Bioma e Espécie",
          xaxis = list(title = "Bioma"),
          yaxis = list(title = "Número de Observações"),
          barmode = "stack"
        )
    }
    
    p
  })
  
  # Definir quais são os biomas continentais e quais são sistemas
  biome_groups <- list(
    continental = c("Amazônia", "Mata Atlântica", "Cerrado", "Caatinga", "Pampa", "Pantanal"),
    sistema = c("Sistema Costeiro")
  )
  
  observe({
    biomas <- read_biomes(year = 2019, showProgress = FALSE)
    biomas <- st_transform(biomas, 4326)
    
    if (input$estado != "Todos") {
      estado_data <- read_state(code_state = input$estado, year = 2020, showProgress = FALSE)
      estado_data <- st_transform(estado_data, 4326)
      biomas <- st_intersection(biomas, estado_data)
    }
    
    biomas <- st_make_valid(biomas)
    biomas <- st_collection_extract(biomas, "POLYGON")
    
    biomas_data(biomas)
    
    updateSelectizeInput(session, "biomas",
                         choices = unique(biomas$name_biome),
                         selected = character(0))
  })
  
  # Adicionar espécie
  observeEvent(input$addSpecies, {
    req(input$especie)
    
    withProgress(message = 'Buscando dados da espécie...', {
      # Sempre buscar para todo o Brasil
      search_params <- list(
        scientificName = input$especie,
        country = "BR",
        hasCoordinate = TRUE,
        limit = 1000
      )
      
      tryCatch({
        species_data <- do.call(occ_search, search_params)
        
        if (!is.null(species_data$data) && nrow(species_data$data) > 0) {
          especies_list <- especies()
          especies_list[[input$especie]] <- species_data$data
          especies(especies_list)
          
          get_species_color(input$especie)
          
          # Filtrar os dados apenas para o estado atual para a notificação
          filtered_data <- if (input$estado != "Todos") {
            estado_full_name <- names(estados_br)[estados_br == input$estado]
            species_data$data %>% filter(stateProvince == estado_full_name)
          } else {
            species_data$data
          }
          
          if (nrow(filtered_data) > 0) {
            showNotification(
              sprintf("Adicionada espécie: %s (%d observações %s)", 
                      input$especie, 
                      nrow(filtered_data),
                      if(input$estado != "Todos") sprintf("em %s", input$estado) else "no total"),
              type = "message"
            )
          } else {
            showNotification(
              sprintf("Espécie adicionada, mas sem observações %s.", 
                      if(input$estado != "Todos") sprintf("em %s", input$estado) else ""),
              type = "warning"
            )
          }
        } else {
          showNotification("Nenhuma observação encontrada para esta espécie.", 
                           type = "warning")
        }
      }, error = function(e) {
        showNotification(
          sprintf("Erro ao buscar dados: %s", e$message),
          type = "error"
        )
      })
    })
  })
  
  # Dados filtrados
  especies_filtradas <- reactive({
    especies_list <- especies()
    biomas <- biomas_data()
    
    if (length(especies_list) == 0 || is.null(biomas)) return(NULL)
    
    # Combinar todas as observações
    all_obs <- bind_rows(especies_list, .id = "especie")
    
    # Filtrar por estado se necessário
    if (input$estado != "Todos") {
      estado_full_name <- names(estados_br)[estados_br == input$estado]
      all_obs <- all_obs %>% 
        filter(stateProvince == estado_full_name)
    }
    
    if (nrow(all_obs) == 0) return(NULL)
    
    all_obs <- all_obs %>%
      mutate(bioma = encontrar_bioma(decimalLatitude, decimalLongitude, biomas))
    
    if (!is.null(input$biomas) && length(input$biomas) > 0) {
      all_obs <- all_obs %>% filter(bioma %in% input$biomas)
    }
    
    # Filtrar por data
    all_obs <- all_obs %>% 
      filter(as.Date(eventDate) >= input$dateRange[1] & 
               as.Date(eventDate) <= input$dateRange[2])
    
    all_obs
  })
  
  observeEvent(input$estado, {
    if (length(especies()) > 0) {
      filtered_data <- especies_filtradas()
      
      if (is.null(filtered_data)) {
        showNotification(
          if (input$estado == "Todos") {
            "Não há observações para o período selecionado."
          } else {
            sprintf("Não há observações para %s no estado %s.", 
                    paste(names(especies()), collapse = ", "),
                    input$estado)
          },
          type = "warning"
        )
      } else {
        especies_por_estado <- filtered_data %>%
          group_by(especie) %>%
          summarise(n = n()) %>%
          arrange(desc(n))
        
        notification_text <- if (input$estado == "Todos") {
          sprintf("Mostrando %d observações de %d espécies em todo o Brasil.", 
                  nrow(filtered_data), nrow(especies_por_estado))
        } else {
          sprintf("Mostrando %d observações de %d espécies em %s.", 
                  nrow(filtered_data), nrow(especies_por_estado), input$estado)
        }
        
        showNotification(notification_text, type = "message")
      }
    }
  })
  
  output$mapa <- renderLeaflet({
    biomas <- biomas_data()
    all_obs <- especies_filtradas()
    
    req(biomas)
    
    # Filtrar biomas se selecionados
    if (!is.null(input$biomas) && length(input$biomas) > 0) {
      biomas <- biomas[biomas$name_biome %in% input$biomas, ]
    }
    
    # Adicionar coluna de cor e tipo (bioma continental ou sistema) diretamente aos dados
    biomas$cor <- biome_colors[biomas$name_biome]
    biomas$tipo <- ifelse(biomas$name_biome %in% biome_groups$continental, "Bioma Continental", "Sistema")
    
    # Mapa
    m <- leaflet() %>%
      addProviderTiles(providers$CartoDB.Voyager)
    
    # Adicionar polígonos dos biomas
    m <- m %>%
      addPolygons(
        data = biomas,
        fillColor = ~cor,
        fillOpacity = 0.6,
        weight = 1,
        color = "#404040",
        group = ~tipo,
        popup = ~paste0("<b>", name_biome, "</b>")
      )
    
    # Adicionar espécies, se houver
    if (!is.null(all_obs) && nrow(all_obs) > 0) {
      especies_cores <- colorFactor(
        palette = rainbow(length(unique(all_obs$especie))),
        domain = unique(all_obs$especie)
      )
      
      m <- m %>%
        addCircleMarkers(
          data = all_obs,
          lng = ~decimalLongitude,
          lat = ~decimalLatitude,
          radius = 4,
          color = ~especies_cores(especie),
          group = ~especie,
          popup = ~paste0(
            "<strong>", scientificName, "</strong><br>",
            "Data: ", format(as.Date(eventDate), "%d/%m/%Y"), "<br>",
            "Coletor: ", recordedBy, "<br>",
            "Instituição: ", institutionCode
          )
        )
    }
    
    create_custom_legend <- function(biome_colors, biome_groups) {
      legend_colors <- c(
        unname(biome_colors[biome_groups$continental]),
        unname(biome_colors[biome_groups$sistema])
      )
      legend_labels <- c(
        biome_groups$continental,
        biome_groups$sistema
      )
      
      legend_html <- paste0(
        "<div style='padding: 6px; font-size: 12px; background: white; background: rgba(255,255,255,0.8);",
        "box-shadow: 0 0 15px rgba(0,0,0,0.2); border-radius: 5px;'>",
        "<div style='margin-bottom:3px;'><strong>Biomas Continentais</strong></div>"
      )
      
      for (i in seq_along(biome_groups$continental)) {
        legend_html <- paste0(
          legend_html,
          "<div><i style='background:", legend_colors[i], "; width:10px; height:10px; ",
          "display:inline-block; margin-right:5px;'></i>",
          legend_labels[i], "</div>"
        )
      }
      
      legend_html <- paste0(
        legend_html,
        "<div style='margin-top:5px; margin-bottom:3px;'><strong>Sistemas</strong></div>"
      )
      
      for (i in (length(biome_groups$continental) + 1):length(legend_labels)) {
        legend_html <- paste0(
          legend_html,
          "<div><i style='background:", legend_colors[i], "; width:10px; height:10px; ",
          "display:inline-block; margin-right:5px;'></i>",
          legend_labels[i], "</div>"
        )
      }
      
      legend_html <- paste0(legend_html, "</div>")
      return(legend_html)
    }
    
    # Adicionar legendas separadas para biomas continentais e sistemas
    m <- m %>%
      addControl(
        html = create_custom_legend(biome_colors, biome_groups),
        position = "bottomright"
      )
    
    # Se houver espécies, adicionar legenda para elas também
    if (!is.null(all_obs) && nrow(all_obs) > 0) {
      m <- m %>%
        addLegend(
          position = "bottomright",
          colors = especies_cores(unique(all_obs$especie)),
          labels = unique(all_obs$especie),
          title = "Espécies",
          opacity = 0.7
        )
    }
    
    # Adicionar controle de camadas
    grupos <- c("Bioma Continental", "Sistema")
    if (!is.null(all_obs) && nrow(all_obs) > 0) {
      grupos <- c(grupos, unique(all_obs$especie))
    }
    
    m %>%
      addLayersControl(
        overlayGroups = grupos,
        options = layersControlOptions(collapsed = FALSE)
      )
  })
  
  # Gráfico de espécies por bioma
  output$especiesPerBioma <- renderPlotly({
    all_obs <- especies_filtradas()
    if (is.null(all_obs) || nrow(all_obs) == 0) return(NULL)
    
    biome_counts <- all_obs %>%
      group_by(bioma, especie) %>%
      summarise(n_observacoes = n(), .groups = 'drop') %>%
      filter(!is.na(bioma))
    
    num_especies <- length(unique(biome_counts$especie))
    
    if (num_especies == 1) {
      # Para uma única espécie
      p <- plot_ly(biome_counts, x = ~bioma, y = ~n_observacoes, type = "bar",
                   marker = list(color = biome_colors[biome_counts$bioma])) %>%
        layout(
          title = paste("Número de Observações por Bioma -", unique(biome_counts$especie)),
          xaxis = list(title = "Bioma"),
          yaxis = list(title = "Número de Observações")
        )
    } else {
      # Para múltiplas espécies
      p <- plot_ly(biome_counts, x = ~bioma, y = ~n_observacoes, type = "bar", color = ~especie) %>%
        layout(
          title = "Número de Observações por Bioma e Espécie",
          xaxis = list(title = "Bioma"),
          yaxis = list(title = "Número de Observações"),
          barmode = "stack"
        )
    }
    
    p
  })
  
  # Caixas laterais
  output$especiesBox <- renderValueBox({
    all_obs <- especies_filtradas()
    n_especies <- if (!is.null(all_obs)) length(unique(all_obs$especie)) else 0
    valueBox(
      value = n_especies,
      subtitle = "Espécies",
      icon = icon("leaf"),
      color = "green"
    )
  })
  
  output$obsBox <- renderValueBox({
    all_obs <- especies_filtradas()
    total_obs <- if (!is.null(all_obs)) nrow(all_obs) else 0
    valueBox(
      value = total_obs,
      subtitle = "Observações",
      icon = icon("eye"),
      color = "yellow"
    )
  })
  
  output$biomasBox <- renderValueBox({
    biomas <- biomas_data()
    n_biomas <- if (!is.null(biomas)) {
      sum(unique(biomas$name_biome) %in% biome_groups$continental)
    } else {
      0
    }
    valueBox(
      value = n_biomas,
      subtitle = "Biomas Continentais",
      icon = icon("globe-americas"),
      color = "blue"
    )
  })
  
  # Estatísticas por bioma
  output$biomesStats <- renderDataTable({
    all_obs <- especies_filtradas()
    if (is.null(all_obs)) return(NULL)
    
    biome_counts <- all_obs %>%
      mutate(
        tipo = case_when(
          bioma %in% biome_groups$continental ~ "Bioma Continental",
          bioma %in% biome_groups$sistema ~ "Sistema",
          TRUE ~ NA_character_
        )
      ) %>%
      group_by(tipo, bioma) %>%
      summarise(
        n_especies = n_distinct(especie),
        n_observacoes = n(),
        .groups = 'drop'
      ) %>%
      arrange(tipo, desc(n_observacoes)) %>%
      filter(!is.na(bioma))
    
    datatable(biome_counts,
              colnames = c("Tipo", "Nome", "Nº de Espécies", "Nº de Observações"),
              options = list(pageLength = 7))
  })
  
  # Tabela de espécies
  output$speciesTable <- renderDataTable({
    all_obs <- especies_filtradas()
    if (is.null(all_obs)) return(NULL)
    
    species_summary <- all_obs %>%
      group_by(especie) %>%
      summarise(
        observacoes = n(),
        primeira_obs = min(as.Date(eventDate)),
        ultima_obs = max(as.Date(eventDate))
      )
    
    datatable(species_summary,
              colnames = c("Espécie", "Observações", "Primeira Observação", "Última Observação"),
              options = list(pageLength = 5))
  })
  
  # Informações sobre biomas na aba "Sobre"
  observe({
    biomas <- biomas_data()
    if (is.null(biomas)) return()
    
    biome_info <- lapply(unique(biomas$name_biome), function(biome) {
      HTML(sprintf('
        <div class="biome-info" style="margin-bottom: 15px;">
          <h5>%s</h5>
          <div style="height: 10px; width: 100px; background-color: %s; margin-bottom: 5px;"></div>
          <p>%s</p>
        </div>
      ', biome, biome_colors[biome], get_biome_description(biome)))
    })
    
    removeUI(selector = "#biomasInfo *")
    insertUI(
      selector = "#biomasInfo",
      where = "beforeEnd",
      ui = div(biome_info)
    )
  })
  
  # Descrição dos biomas
  get_biome_description <- function(biome) {
    switch(biome,
           "Amazônia" = "Maior floresta tropical do mundo, com alta biodiversidade e importante papel no clima global.",
           "Mata Atlântica" = "Floresta tropical que se estende ao longo da costa brasileira, com alto endemismo.",
           "Cerrado" = "Savana tropical com grande variedade de plantas e animais adaptados.",
           "Caatinga" = "Único bioma exclusivamente brasileiro, adaptado a condições semiáridas.",
           "Pampa" = "Região de campos nativos com rica biodiversidade de gramíneas.",
           "Pantanal" = "Maior planície alagável do mundo, com fauna e flora adaptadas ao ciclo das águas.",
           "Sistema Costeiro" = "Região que abrange diversos ecossistemas costeiros e marinhos. O Sistema Costeiro Oceânico sempre faz parte de um bioma continental quando não está no oceano (por exemplo, em rios).",
           "Descrição não disponível")
  }
  
  # Download
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("biodiversidade-brasil-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      all_obs <- especies_filtradas()
      if (is.null(all_obs)) return(NULL)
      write.csv(all_obs, file, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)