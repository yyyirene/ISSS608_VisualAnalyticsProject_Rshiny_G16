# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#
# shinydashboard
library(shiny)

# Add resource path for PDF file
addResourcePath("pdfs", "www")
library(shinydashboard)
library(dplyr)
library(DT)
library(visNetwork)
library(jsonlite)
library(shinyWidgets)
library(lubridate)
library(ggplot2)
library(plotly)
library(jsonlite)
library(dplyr)
library(networkD3)
library(igraph)
library(jsonlite)
library(dplyr)
library(tidygraph)
library(tibble)
library(webshot2)
library(htmlwidgets)
library(circlize)
library(scales)
library(reshape2)
#——————————————————————————————————influence graph data preparation————————————————————————————————————

kg <- fromJSON("data/MC1_graph.json")

nodes_tbl <- as_tibble(kg$nodes)
edges_tbl <- as_tibble(kg$links)

nodes_df <- as.data.frame(kg$nodes)
edges_df <- as.data.frame(kg$links)
all_nodes <- nodes_df

id_map <- tibble(id = nodes_tbl$id,  #Retrieve the ID column of each row node
                 index = seq_len(
                   nrow(nodes_tbl)))  #Generate a line number sequence from 1 to n


edges_tbl <- edges_tbl %>%
  left_join(id_map, by = c("source" = "id")) %>%  # source id → from index
  rename(from = index) %>% 
  left_join(id_map, by = c("target" = "id")) %>%  # target id → to index
  rename(to = index)

edges_tbl <- edges_tbl %>%
  filter(!is.na(from), !is.na(to))

graph <- tbl_graph(nodes = nodes_tbl, 
                   edges = edges_tbl, 
                   directed = kg$directed)


#————————————————————————————————————————————————————————————————————————————————————

extract_subnetwork <- function(graph, node_name, 
                               distance = NULL, 
                               direction = c("all", "in", "out"),
                               edge_types = NULL,
                               node_types = NULL) {
  direction <- match.arg(direction)
  node <- which(V(graph)$name == node_name)
  if (length(node) == 0) stop("Node name not found in graph.")
  distance <- ifelse(is.null(distance), length(graph), distance)
  
  mode <- switch(direction,
                 all = "all",
                 `in` = "in",
                 out = "out")
  
  igraph_subgraph <- induced_subgraph(graph, vids = ego(graph, node, order = distance, mode = mode)[[1]])
  
  nodes_df <- igraph::as_data_frame(igraph_subgraph, what = "vertices")
  edges_df <- igraph::as_data_frame(igraph_subgraph, what = "edges")
  
  if (!is.null(edge_types)) {
    edges_df <- edges_df %>% dplyr::filter(`Edge Type` %in% edge_types) %>%
      mutate(color = case_when(
        `Edge Type` == "CoverOf" ~ "#e76f51",
        `Edge Type` == "ComposerOf" ~ "#457b9d",
        `Edge Type` == "DirectlySamples" ~ "#2a9d8f",
        `Edge Type` == "InStyleOf" ~ "#f4a261",
        `Edge Type` == "InterpolatesFrom" ~ "#9d4edd",
        `Edge Type` == "LyricalReferenceTo" ~ "#ffb703",
        `Edge Type` == "LyricistOf" ~ "#219ebc",
        `Edge Type` == "MemberOf" ~ "#8ecae6",
        `Edge Type` == "PerformerOf" ~ "#e63946",
        `Edge Type` == "ProducerOf" ~ "#6a994e",
        TRUE ~ "#888"
      ),
      width = 2,
      arrows = "to"
    )
  }
  
  if (!is.null(node_types)) {
    nodes_df <- nodes_df %>% dplyr::filter(`Node Type` %in% node_types)
  }
  
  used_node_ids <- unique(c(edges_df$from, edges_df$to))
  nodes_df <- nodes_df %>% dplyr::filter(name %in% used_node_ids)
  
  tidygraph::tbl_graph(nodes = nodes_df, edges = edges_df, directed = igraph::is_directed(graph))
}

#————————————————————————————————————————————————————————————————————————————————————————
nodes_tbl <- nodes_tbl %>% mutate(index = row_number())

sailor_index <- nodes_tbl %>%
  filter(name == "Sailor Shift") %>%
  pull(index)

edges_from_sailor <- edges_tbl %>%
  filter(from == sailor_index)

first_layer_info <- edges_from_sailor %>%
  inner_join(nodes_tbl, by = c("to" = "index")) %>%
  filter(`Node Type` %in% c("Song", "Album", "MusicalGroup", "RecordLabel"))

first_layer_targets <- first_layer_info$to

influence_edges <- c("InStyleOf", "LyricalReferenceTo", "InterpolatesFrom", "CoverOf", "DirectlySamples")

edges_2nd <- edges_tbl %>%
  filter(from %in% first_layer_targets & `Edge Type` %in% influence_edges)

influenced_works <- edges_2nd$to
songs_with_outgoing <- unique(edges_2nd$from)

# Define person edge types for talent radar
person_edge_types <- c("PerformerOf", "ComposerOf", "LyricistOf", "ProducerOf")

edges_people_to_2nd <- edges_tbl %>%
  filter(
    to %in% influenced_works,
    `Edge Type` %in% person_edge_types
  ) %>%
  left_join(nodes_tbl %>% select(index, `Node Type`), by = c("from" = "index")) %>%
  rename(`From Node Type` = `Node Type`) %>%
  filter(`From Node Type` %in% c("Person", "RecordLabel")) %>%
  left_join(nodes_tbl %>% select(index, `Node Type`, release_date), by = c("to" = "index")) %>%
  rename(`To Node Type` = `Node Type`, release_date = release_date)

first_layer_filtered <- first_layer_info %>%
  filter(
    (`Node Type` %in% c("Song", "Album") & to %in% songs_with_outgoing) |
      (`Node Type` %in% c("MusicalGroup", "RecordLabel"))
  )

edges_from_sailor_filtered <- edges_from_sailor %>%
  semi_join(first_layer_filtered, by = c("to" = "to"))

all_edges <- bind_rows(
  edges_from_sailor_filtered,
  edges_2nd,
  edges_people_to_2nd %>% rename(from = from, to = to)
)

node_ids <- unique(c(all_edges$from, all_edges$to))

nodes_subgraph <- nodes_tbl %>%
  filter(index %in% node_ids) %>%
  mutate(
    id = index,
    label = name,
    group = `Node Type`,
    release_year = as.numeric(substr(release_date, 1, 4)),
    written_year = as.numeric(substr(written_date, 1, 4)),
    notoriety_year = as.numeric(substr(notoriety_date, 1, 4)),
    
    
    title = paste0(
      "<b>", name, "</b><br>",
      "Type: ", `Node Type`, "<br>",
      "Release Year: ", release_year, "<br>",
      "Written Year: ", written_year, "<br>",
      "Notoriety Year: ", notoriety_year, "<br>",
      "Genre: ", genre, "<br>",
      "Single: ", single, "<br>",
      "Notable: ", notable
    )
  ) %>%
  select(id, label, group, release_year, notable, single, genre,
         written_year, notoriety_year, title)


edges_subgraph <- all_edges %>%
  
  left_join(
    nodes_tbl %>%
      select(index, from_node_type = `Node Type`, from_name = name),
    by = c("from" = "index")
  ) %>%
  
  
  left_join(
    nodes_tbl %>%
      select(index, to_node_type = `Node Type`, to_name = name,
             single, genre, release_date, notable, written_date, notoriety_date),
    by = c("to" = "index")
  ) %>%
  
  
  mutate(
    release_date = release_date.y
  ) %>%
  
  
  transmute(
    from,
    to,
    from_name,
    from_node_type,
    edge_type = `Edge Type`,
    label = `Edge Type`,
    to_name,
    to_node_type,
    single,
    genre,
    release_date,
    notable,
    written_date,
    notoriety_date
  )



min_year <- 1983
max_year <- 2038
# —————————————————————————————————— her impact on others   ——————————————————————————————————

# ———————————————————————————————————————————————————————————————————————————————
# Create subgraph_in centered on Sailor Shift (incoming influence within 3 steps)
# ———————————————————————————————————————————————————————————————————————————————
subgraph_in <- extract_subnetwork(
  graph, 
  node_name = "Sailor Shift", 
  distance = 3, 
  direction = "in"
)

edges_vn <- igraph::as_data_frame(subgraph_in, what = "edges") %>%
  filter(`Edge Type` != "ProducerOf") %>%
  rename(from = from, to = to, label = `Edge Type`)

nodes_vn <- igraph::as_data_frame(subgraph_in, what = "vertices") %>%
  mutate(id = name,
         label = name,
         group = `Node Type`)  # 可视化用 group 表示类别

used_nodes <- unique(c(edges_vn$from, edges_vn$to))

nodes_vn <- nodes_vn %>% filter(id %in% used_nodes)

node_colors <- c(
  "Person" = "#457b9d",
  "Song" = "#e76f51",
  "Album" = "#f4a261",
  "MusicalGroup" = "#2a9d8f",
  "RecordLabel" = "#9d4edd"
)
nodes_vn <- nodes_vn %>%
  mutate(color = node_colors[group])

# —————————————————————————————————— tab 3 community influence   ——————————————————————————————————


subgraph_igraph <- as.igraph(subgraph_in)

nodes_all <- igraph::as_data_frame(subgraph_igraph, what = "vertices")
edges_all <- igraph::as_data_frame(subgraph_igraph, what = "edges")


nodes_tbl <- nodes_tbl %>% mutate(index = row_number())

creator_names <- c(
  "Zara Quinn", "Milo Knight", "Cassette Future", "Eliza Brooks",
  "Jasper Reed", "Silver Veil", "Juno Ray", "Savannah Teal",
  "The Phantom Operators", "Chloe Montgomery", "Nathaniel Brooks", "Clara Davis",
  "The Hollow Monarchs", "Cassian Storm", "Claire Holmes", "Beatrice Albright",
  "Daniel O'Connell", "Copper Canyon Ghosts"
)

creator_ids <- nodes_tbl %>%
  filter(name %in% creator_names) %>%
  pull(index)

target_edge_types <- c("PerformerOf", "ComposerOf", "LyricistOf", "ProducerOf")

edges_out <- edges_tbl %>%
  filter(from %in% creator_ids, `Edge Type` %in% target_edge_types)

edges_out_full <- edges_out %>%
  left_join(nodes_tbl %>% mutate(index = row_number()), by = c("to" = "index")) %>%
  select(from, to, `Edge Type`, name, `Node Type`, release_date,genre,notable,notoriety_date)

edges_out_full <- edges_out_full %>%
  left_join(nodes_tbl %>% select(index, creator_name = name), by = c("from" = "index")) %>%
  select(creator_name, `Edge Type`, name, `Node Type`, release_date,genre,notable,notoriety_date)

edges_oceanus <- edges_out_full %>%
  filter(genre == "Oceanus Folk")

knitr::kable(head(edges_oceanus, 6))


edges_sailor <- edges_all %>%
  filter(to == "Sailor Shift") %>%
  filter(`Edge Type` %in% c("InStyleOf", "LyricalReferenceTo", "CoverOf", "InterpolatesFrom", "DirectlySamples")) %>%
  select(from, to, `Edge Type`)

creator_work_nodes <- unique(c(edges_oceanus$creator_name, edges_oceanus$name))

all_node_names <- union(
  creator_work_nodes,
  unique(c(edges_sailor$from, edges_sailor$to, "Sailor Shift", "Copper Canyon Ghosts"))  
)

nodes_vn <- nodes_tbl %>%
  filter(name %in% all_node_names) %>%
  mutate(
    id = index,
    label = name,
    group = `Node Type`,
    color.background = ifelse(name == "Sailor Shift", "yellow", NA), 
    shape = ifelse(name == "Sailor Shift", "star", "dot")
  )

edges_vn_creator <- edges_oceanus %>%
  rename(from = creator_name, to = name, label = `Edge Type`) %>%
  left_join(nodes_vn %>% select(name, id), by = c("from" = "name")) %>%
  rename(from_id = id) %>%
  left_join(nodes_vn %>% select(name, id), by = c("to" = "name")) %>%
  rename(to_id = id) %>%
  mutate(length = NA) %>%
  select(from = from_id, to = to_id, label, length) %>%
  filter(!is.na(from) & !is.na(to))

edges_vn_sailor <- edges_sailor %>%
  left_join(nodes_vn %>% select(name, id), by = c("from" = "name")) %>%
  rename(from_id = id) %>%
  left_join(nodes_vn %>% select(name, id), by = c("to" = "name")) %>%
  rename(to_id = id) %>%
  mutate(length = NA) %>%
  select(from = from_id, to = to_id, label = `Edge Type`, length) %>%
  filter(!is.na(from) & !is.na(to))

forced_edge <- tibble(
  from = "Copper Canyon Ghosts",
  to = "Sailor Shift",
  label = "DirectlySamples",
  length = 400
)

forced_edge_ids <- forced_edge %>%
  left_join(nodes_vn %>% select(name, id), by = c("from" = "name")) %>%
  rename(from_id = id) %>%
  left_join(nodes_vn %>% select(name, id), by = c("to" = "name")) %>%
  rename(to_id = id) %>%
  select(from = from_id, to = to_id, label, length) %>%
  filter(!is.na(from) & !is.na(to))

edges_final <- bind_rows(
  edges_vn_creator,
  edges_vn_sailor,
  forced_edge_ids
)

# —————————————————————————————————— Genre Diffusion Tracker: Data Preparation ——————————————————————————————————

processedData <- reactive({
  # 原始数据读取
  graph_data <- jsonlite::fromJSON("data/MC1_graph.json")
  nodes_df <- as.data.frame(graph_data$nodes)
  edges_df <- as.data.frame(graph_data$links)
  
  # 过滤合法节点
  nodes <- nodes_df %>%
    dplyr::filter(`Node Type` %in% c("Song", "Album"), !is.na(genre))
  
  # 加入年份过滤（可选：假设你定义了 input$yearRange）
  if (!is.null(input$yearRange)) {
    nodes <- nodes %>%
      dplyr::filter(
        !is.na(release_date),
        as.numeric(release_date) >= input$yearRange[1],
        as.numeric(release_date) <= input$yearRange[2]
      )
  }
  
  # enrich edge with genre info
  edges <- edges_df %>%
    dplyr::left_join(nodes %>% dplyr::select(id, source_genre = genre), by = c("source" = "id")) %>%
    dplyr::left_join(nodes %>% dplyr::select(id, target_genre = genre), by = c("target" = "id"))
  
  # genre-genre frequency for chord diagram
  chord_df <- edges %>%
    dplyr::filter(!is.na(source_genre), !is.na(target_genre)) %>%
    dplyr::group_by(source_genre, target_genre) %>%
    dplyr::summarize(value = n(), .groups = "drop")
  
  genre_matrix <- reshape2::acast(chord_df, source_genre ~ target_genre, value.var = "value", fill = 0)
  
  list(
    genre_matrix = genre_matrix,
    nodes = nodes,
    edges = edges
  )
})

# —————————————————————————————————— Talent Radar Module 3: Data Preparation ——————————————————————————————————


# Load and prepare data
data_path <- "data/MC1_graph.json"
kg <- fromJSON(data_path)

nodes_tbl <- as_tibble(kg$nodes) %>%
  rename(node_name = name) %>%
  mutate(index = row_number())

edges_tbl <- as_tibble(kg$links)

id_map <- nodes_tbl %>% select(id, index)

edges_tbl_graph <- edges_tbl %>%
  left_join(id_map, by = c("source" = "id")) %>% rename(from = index) %>%
  left_join(id_map, by = c("target" = "id")) %>% rename(to = index) %>%
  filter(!is.na(from), !is.na(to))

# Create tidygraph object
g_tbl <- tbl_graph(
  nodes = nodes_tbl,
  edges = edges_tbl_graph,
  directed = TRUE
)

# Talent scoring function
prepare_talent_score_from_graph <- function(g_tbl) {
  nodes <- as_tibble(g_tbl, active = "nodes")
  edges <- as_tibble(g_tbl, active = "edges")
  
  # Identify notable works
  notable_work_ids <- nodes %>%
    filter(`Node Type` %in% c("Song", "Album"), notable == TRUE) %>%
    pull(index)
  
  # Identify contributing persons
  contributing_persons <- edges %>%
    filter(to %in% notable_work_ids,
           `Edge Type` %in% c("PerformerOf", "ComposerOf", "ProducerOf", "LyricistOf")) %>%
    pull(from) %>% unique()
  
  # Prepare person dataframe with genre and recency
  person_df <- nodes %>%
    filter(`Node Type` == "Person") %>%
    select(index, label = node_name, notoriety_date, written_date, genre) %>%
    mutate(
      notoriety_year = as.numeric(substr(notoriety_date, 1, 4)),
      notoriety_recency = pmax(0, 1 - (2025 - notoriety_year) / 20),
      notable_label = ifelse(index %in% contributing_persons, 1, 0)
    )
  
  # Add graph features
  graph_with_features <- g_tbl %>%
    activate(nodes) %>%
    mutate(
      degree = centrality_degree(),
      pagerank = centrality_pagerank()
    )
  
  graph_features <- as_tibble(graph_with_features, active = "nodes") %>%
    filter(`Node Type` == "Person") %>%
    select(index, degree, pagerank)
  
  # Merge all features
  features <- person_df %>%
    left_join(graph_features, by = "index") %>%
    mutate(across(c(degree, pagerank, notoriety_recency), ~replace_na(., 0)))
  
  # Train logistic regression model
  if (nrow(features) < 10 || length(unique(features$notable_label)) < 2) {
    stop("❌ Training data insufficient or lacks positive/negative samples.")
  }
  
  model <- glm(notable_label ~ degree + pagerank + notoriety_recency,
               data = features, family = binomial)
  
  # Predict and format results
  features$predicted_prob <- predict(model, newdata = features, type = "response")
  features <- features %>%
    arrange(desc(predicted_prob)) %>%
    mutate(
      recommendation = paste0(
        "🎧 ", label, " shows ",
        ifelse(pagerank > 0.5, "high influence, ", "moderate impact, "),
        ifelse(notoriety_recency > 0.6, "and recent notoriety. ", "with steady activity. "),
        "Potential score: ", round(predicted_prob * 100, 1), "%"
      )
    ) %>%
    mutate(id = index) %>%  # For visNetwork
    select(id, label, genre, degree, pagerank, notoriety_year, notoriety_recency,
           predicted_prob, notable_label, recommendation)
  
  return(list(model = model, scored = features))
}

# Generate result
talent_model_result <- prepare_talent_score_from_graph(g_tbl)
talent_score_df <- talent_model_result$scored

#————————————————————————————————————————————————————————————————————————————————————
ui <- dashboardPage( 
  
  dashboardHeader(     #start
    title = tagList(
      div(
        style = "display: flex; align-items: center;",
        span("EchoScope™", style = "font-weight: bold; font-size: 22px; color: white; margin-right: 20px;"),
        span("Discovering Music Influence Through Visual Analytics",
             style = "font-size: 16px; color: white;")
      )
    ),
    titleWidth = 600
  ),      # end title
  
        dashboardSidebar(   #start
        sidebarMenu(
          menuItem("Home", tabName = "home", icon = icon("home")),
          menuItem("Influence Analysis", tabName = "influenced", icon = icon("project-diagram")),
          menuItem("Genre Diffusion", tabName = "genre", icon = icon("fire")),
          menuItem("Talent Radar", tabName = "talent", icon = icon("satellite-dish")),
          menuItem("Trend Dashboard", tabName = "trend", icon = icon("chart-bar"))
        )
      ),  #end sidebar
  
  
  dashboardBody(   #start
    
    tabItems(       #start
      
      # --- Home Page ---
      tabItem(
        tabName = "home",
        fluidRow(
          box(
            width = 12,
            status = "primary",
            solidHeader = FALSE,
            collapsible = FALSE,
            HTML('
            <div style="padding: 10px; text-align: left;">
              <h1 style="font-size: 36px; font-weight: bold; margin-bottom: 20px; color: #2c3e50;">Welcome!</h1>
              <p style="font-size: 16px; line-height: 1.8;">
                <b>EchoScope™</b> is a web-based visual analytics prototype built for the <b>ISSS608 Visual Analytics</b> course project.
                It uses <b>VAST 2025 Mini-Challenge 1</b> data to explore the artistic evolution of <b>Sailor Shift</b> and the influence of <b>Oceanus Folk</b> on global music.
              </p>
              <p style="font-size: 17px; line-height: 1.8;">
                Our dashboard simulates a modern SaaS platform, offering interactive graphs and dashboards similar in experience to <b>Apple Music</b>.
                It allows <i>music analysts</i>, <i>A&amp;R managers</i>, and <i>cultural researchers</i> to explore trends and talent.
              </p>
              <p style="font-size: 16px; color: #2980b9; margin-top: 30px;">
                Use the navigation menu on the left to explore different modules.
              </p>
            </div>
          ')
          )
        ),
        fluidRow(
          tags$iframe(
            src = "pdfs/EchoScope_R_Shiny_App_User_Guide.pdf",
            width = "100%",
            height = "800px",
            style = "border: none;"
          )
        )
      ),  #end home info
      
      tabItem(
        tabName = "influenced",
        fluidPage(
          fluidRow(
            box(
              title = "Sailor Shift Influence Analysis",
              width = 12,
              solidHeader = TRUE,
              status = "primary",
              collapsible = TRUE,
              tabsetPanel(
                
                # ===== Tab 1: Influenced by =====
                tabPanel(
                  "Influenced by",
                  
                  br(),
                  
                  fluidRow(
                    column(
                      width = 4,
                      wellPanel(
                        pickerInput(
                          inputId = "node_type",
                          label = "Select Node Type",
                          choices = sort(unique(nodes_subgraph$group)),
                          selected = unique(nodes_subgraph$group),
                          multiple = TRUE,
                          options = list(`actions-box` = TRUE, `live-search` = TRUE)
                        ),
                        pickerInput(
                          inputId = "node_name",
                          label = "Search Artists Name",
                          choices = sort(unique(nodes_subgraph$label)),
                          selected = NULL,
                          multiple = TRUE,
                          options = list(
                            `actions-box` = TRUE,
                            `live-search` = TRUE,
                            `none-selected-text` = "Type or select a node name",
                            `style` = "btn-default"
                          )
                        ),
                        helpText(tagList(
                          "Note: Selecting a node will zoom in and highlight it in the network graph & Only apply in network graph exploration.
                        Tip: Click on a node to reveal more detailed information.",
                          
                        )),
                        pickerInput(
                          inputId = "edge_type",
                          label = "Select Edge Type",
                          choices = sort(unique(edges_subgraph$label)),
                          selected = unique(edges_subgraph$label),
                          multiple = TRUE,
                          options = list(`actions-box` = TRUE, `live-search` = TRUE)
                        ),
                        radioButtons(
                          inputId = "notable_filter",
                          label = "Is Notable?",
                          choices = c("All", "TRUE", "FALSE"),
                          selected = "All",
                          inline = TRUE
                        ),
                        pickerInput(
                          inputId = "genre_filter",
                          label = "Select Genre(s)",
                          choices = sort(unique(na.omit(nodes_subgraph$genre))),
                          selected = unique(na.omit(nodes_subgraph$genre)),
                          multiple = TRUE,
                          options = list(`actions-box` = TRUE)
                        ),
                        sliderInput("release_range", "Release Year Range",
                                    min = 1983, max = 2038,
                                    value = c(min_year, max_year), step = 1, sep = ""
                        ),
                        actionButton("release_range_btn", "Select All Years"),
                        helpText("Note: Selecting all years might take a moment. Thanks for your patience."),
                        sliderInput("network_depth", "Select Network Depth (Layers from Sailor Shift)",
                                    min = 1, max = 3, value = 2, step = 1,
                                    ticks = TRUE, animate = TRUE
                        ),
                        actionButton("network_depth_btn", "Select All Network"),
                        helpText("Note: Selecting all Network Depths might take a moment. Thanks for your patience.")
                      )
                    ),
                    
                    column(
                      width = 8,
                      tabsetPanel(
                        id = "graph_tabs",
                        type = "tabs",
                        tabPanel("Influence Network",
                                 visNetworkOutput("directGraph", height = "725px")),
                        tabPanel("Summary Statistics",
                                 fluidRow(
                                   column(
                                     width = 12,
                                     div(
                                       style = "margin-top: 30px;",
                                       plotlyOutput("groupEdgeBarPlot", height = "600px"),
                                       verbatimTextOutput("barInfo")
                                     )
                                   )
                                 ))
                      )
                    )
                  ),
                  
                  br(),
                  
                  fluidRow(
                    column(
                      width = 12,
                      div(
                        style = "padding-left: 30px; padding-right: 30px;",
                        DTOutput("directTable", width = "100%")
                      )
                    )
                  )
                ), # End Tab 1
                
                # ===== Tab 2: Her Impact & Collaborators =====
                tabPanel(
                  "Her Impact & Collaborators",
                  
                  br(),
                  
                  fluidRow(
                    column(
                      width = 4,
                      wellPanel(
                        h4("Impact Analysis Controls"),
                        pickerInput(
                          inputId = "impact_node_type",
                          label = "Select Node Type",
                          choices = sort(unique(nodes_subgraph$group)),
                          selected = unique(nodes_subgraph$group),
                          multiple = TRUE,
                          options = list(`actions-box` = TRUE, `live-search` = TRUE)
                        ),
                        pickerInput(
                          inputId = "impact_edge_type",
                          label = "Select Edge Type",
                          choices = sort(unique(edges_subgraph$edge_type)),
                          selected = unique(edges_subgraph$edge_type),
                          multiple = TRUE,
                          options = list(`actions-box` = TRUE, `live-search` = TRUE)
                        ),
                        radioButtons(
                          inputId = "impact_direction",
                          label = "Impact Direction",
                          choices = c("Outgoing (Her Impact)" = "out", "Incoming (Influenced by)" = "in"),
                          selected = "out",
                          inline = TRUE
                        ),
                        sliderInput(
                          inputId = "impact_depth",
                          label = "Network Depth",
                          min = 1, max = 3, value = 2, step = 1,
                          ticks = TRUE, animate = TRUE
                        ),
                        hr(),
                        h4("Collaboration Filters"),
                        pickerInput(
                          inputId = "collaborator_type",
                          label = "Collaborator Type",
                          choices = c("PerformerOf", "ComposerOf", "LyricistOf", "ProducerOf", "MemberOf"),
                          selected = c("PerformerOf", "ComposerOf", "LyricistOf", "ProducerOf"),
                          multiple = TRUE,
                          options = list(`actions-box` = TRUE, `live-search` = TRUE)
                        ),
                        sliderInput(
                          inputId = "collab_year_range",
                          label = "Collaboration Year Range",
                          min = 1983, max = 2038,
                          value = c(2000, 2025), step = 1, sep = ""
                        ),
                       
                    
                      )
                    ),
                    
                    column(
                      width = 8,
                      tabsetPanel(
                        id = "impact_tabs",
                        type = "tabs",
                        tabPanel("Impact Network",
                                 visNetworkOutput("impactNetwork", height = "600px")),
                        tabPanel("Collaboration Timeline",
                                 plotlyOutput("collabTimeline", height = "600px")),
                        tabPanel("Genre Impact Analysis",
                          fluidRow(
                            column(width = 6,
                              h4("Impact by Genre", style = "margin-top:10px;"),
                              plotlyOutput("genreImpactChart", height = "320px")
                            ),
                            column(width = 6,
                              h4("Influence Types", style = "margin-top:10px;"),
                              plotlyOutput("influenceStrengthChart", height = "320px")
                            )
                          ),
                          fluidRow(
                            column(width = 12,
                              h4("Collaboration Heatmap", style = "margin-top:10px;"),
                              plotlyOutput("collaborationHeatmap", height = "320px")
                            )
                          )),
                        tabPanel("Impact Statistics",
                          fluidRow(
                            column(width = 12,
                              h4("Impact Metrics", style = "margin-top:10px;"),
                              plotlyOutput("impactMetrics", height = "300px")
                            )
                          ),
                          fluidRow(
                            column(width = 12,
                              h4("Collaborator Network", style = "margin-top:10px;"),
                              plotlyOutput("collaboratorNetwork", height = "300px")
                            )
                          ))
                     
                      )
                    )
                  ),
                  
                  br(),
                  
                  fluidRow(
                    column(
                      width = 12,
                      div(
                        style = "padding-left: 30px; padding-right: 30px;",
                       
                        DTOutput("impactTable", width = "100%")
                      )
                    )
                  )
                ), # End Tab 2
                
                # ===== Tab 3: Community Influence =====
                tabPanel(
                  "Community Influence",
                  br(),
                  fluidRow(
                    column(
                      width = 4,
                      wellPanel(
                        h4("Community Influence Controls"),
                        pickerInput(
                          inputId = "comm_from_node_type",
                          label = "Select From Node Type",
                          choices = sort(unique(nodes_vn$group)),
                          selected = unique(nodes_vn$group),
                          multiple = TRUE,
                          options = list(`actions-box` = TRUE, `live-search` = TRUE)
                        ),
                        pickerInput(
                          inputId = "comm_to_node_type",
                          label = "Select To Node Type",
                          choices = sort(unique(nodes_vn$group)),
                          selected = unique(nodes_vn$group),
                          multiple = TRUE,
                          options = list(`actions-box` = TRUE, `live-search` = TRUE)
                        ),
                        pickerInput(
                          inputId = "comm_edge_type",
                          label = "Select Edge Type",
                          choices = sort(unique(edges_final$label)),
                          selected = unique(edges_final$label),
                          multiple = TRUE,
                          options = list(`actions-box` = TRUE, `live-search` = TRUE)
                        ),
                      
                      )
                    ),
                    column(
                      width = 8,
                      tabsetPanel(
                        id = "comm_tabs",
                        type = "tabs",
                        tabPanel("Community Network",
                          visNetworkOutput("commNetwork", height = "600px")
                        ),
                        tabPanel("Edge Table",
                          fluidRow(
                            column(width = 12,
                              h4("Community Influence Edge Table", style = "margin-top:10px;"),
                              DTOutput("commEdgeTable")
                            )
                          )
                        )
                        # 你可以加更多tab，比如统计图等
                      )
                    )
                  )
                ) # End Tab 3
                
                
                
              ) # End fluidRow
            ) #BOX
          )#tabsetpanel
        ) #FLUIDPAGE
      ), # End tabItem "influenced"
      
              # ----------- Genre Diffusion Tracker ---------------
      tabItem(tabName = "genre",
        fluidPage(
          fluidRow(
            box(
              title = "Genre Diffusion Tracker",
              width = 12,
              solidHeader = TRUE,
              status = "primary",
              collapsible = TRUE,
              tabsetPanel(
                tabPanel("Genre Impact Overview",
                         br(),
                  fluidRow(
                    column(width = 3,
                    wellPanel(status = "info", solidHeader = TRUE, width = 12, style = "background-color: #f9f9f9; border: none; box-shadow: none;",
                        selectInput("mainGenre", "Main Genre", choices = sort(unique(na.omit(all_nodes$genre))), selected = "Oceanus Folk"),
                        sliderInput("yearRange", "Year Range:", min = 1983, max = 2038, value = c(1990, 2025), sep = ""),
                        selectInput("nodeType", "Node Type:", choices = c("Song (Track)" = "Song", "Album" = "Album"), selected = "Song"),
                        radioButtons("hopDepth", "Influence Path Depth:", choices = c("1-hop" = 1, "2-hop" = 2), selected = 1),
                        sliderInput("fameYear", "Sailor Shift Fame Year:", min = 1983, max = 2038, value = 2012, sep = "")
                      ),
                      box(title = "Detail Panel", solidHeader = FALSE, width = 12,
                        uiOutput("detailPanel")
                      )
                    ),
                    column(width = 9,
                      fluidRow(
                        column(width = 6,
                          box(title = "Pre-Fame Influence Network", solidHeader = FALSE, width = 12,
                            visNetworkOutput("genreNetPre", height = "300px")
                          )
                        ),
                        column(width = 6,
                          box(title = "Post-Fame Influence Network", solidHeader = FALSE, width = 12,
                            visNetworkOutput("genreNetPost", height = "300px")
                          )
                        )
                      ),
                      fluidRow(
                        column(width = 12,
                          box(title = "Timeline Trend", solidHeader = FALSE, width = 12,
                            plotlyOutput("trendPlot", height = "250px")
                          )
                        )
                      )
                    )
                  )
                ),
                tabPanel("Top Influenced Artists",
                         br(),
                  fluidRow(
                    column(width = 3,
                           wellPanel(status = "info", solidHeader = TRUE, width = 12, style = "background-color: #f9f9f9; border: none; box-shadow: none;",
                        selectInput("top_artist_genre", "Focus Genre:", choices = sort(unique(na.omit(all_nodes$genre))), selected = "Oceanus Folk"),
                        numericInput("top_n", "Top N Influenced Artists:", value = 10, min = 1, max = 50)
                      )
                    ),
                    column(width = 9,
                      box(title = "Top Influenced Artists by Genre", solidHeader = FALSE, width = 12,
                        plotlyOutput("topInfluencedPlot", height = "200px")
                      )
                    )
                  ),
                  fluidRow(
                    column(width = 12,
                      box(title = "Layered Genre-Artist Sankey", solidHeader = FALSE, width = 12,
                        plotlyOutput("topGenreSankeyPlotly", height = "300px")
                      )
                    )
                  )
                )
              )
            )
          )
        )
      ), #end Genre Diffusion Tracker
      
      # --- Talent Radar UI ---
      tabItem(
        tabName = "talent",
        fluidPage(
          fluidRow(
            box(
              title = "Talent Scoring & Emerging Artist Radar",
              width = 12,
              solidHeader = TRUE,
              status = "primary",
              collapsible = TRUE,
              tabsetPanel(
                id = "talent_tabs",
                type = "tabs",
                selected = "Score Explorer",
                
                # --- Score Explorer Panel ---
                tabPanel(
                  "Score Explorer",
                  fluidRow(
                    column(
                      width = 4,
                      pickerInput(
                        inputId = "talent_genre",
                        label = "Filter by Genre",
                        choices = unique(na.omit(nodes_tbl$genre)),
                        selected = unique(na.omit(nodes_tbl$genre))[1],
                        multiple = TRUE,
                        options = list(`actions-box` = TRUE, `live-search` = TRUE)
                      ),
                      selectInput(
                        inputId = "talent_topN",
                        label = "Show Top N Artists",
                        choices = c("Top 5" = 5, "Top 10" = 10, "Top 15" = 15, "Top 20" = 20),
                        selected = 5
                      ),
                      uiOutput("select_compare_artists"),
                      hr(),
                      h4("Customize Score Weights"),
                      sliderInput("weight_pagerank", "PageRank", 0, 1, 0.3, 0.1),
                      helpText("PageRank indicates the global influence of an artist within the network."),
                      sliderInput("weight_degree", "Degree Centrality", 0, 1, 0.2, 0.1),
                      helpText("Degree Centrality measures the number of direct connections an artist has."),
                      sliderInput("weight_similarity", "Style Similarity", 0, 1, 0.3, 0.1),
                      helpText("Style Similarity reflects contributions to selected-genre works."),
                      sliderInput("weight_notable_count", "Notable Works Count", 0, 1, 0.2, 0.1),
                      helpText("Notable Works Count is the normalized count of an artist's works marked as notable."),
                      hr(),
                      sliderInput(
                        inputId = "talent_year_range",
                        label = "Year Range",
                        min = 2025,
                        max = max(as.numeric(na.omit(nodes_tbl$release_date)), na.rm = TRUE),
                        value = c(2025, max(as.numeric(na.omit(nodes_tbl$release_date)), na.rm = TRUE)),
                        step = 1,
                        sep = ""
                      ),
                      downloadButton("download_weighted_scores", "📥 Download CSV")
                    ),
                    column(
                      width = 8,
                      tabsetPanel(
                        selected = "Scoreboard",
                        tabPanel("Scoreboard", DTOutput("talent_score_table")),
                        tabPanel("Radar Comparison", plotlyOutput("talent_radar_plot", height = "550px"))
                      )
                    )
                  )
                ), # end Score Explorer
                
                # --- Artist Snapshots Panel ---
              
                
              ) # end tabsetPanel
            ) # end box
          ) # end fluidRow
        ) # end fluidPage
      ),             #end tabItem("talent"),
      
      
      # --- Trend Dashboard UI ---
      tabItem(tabName = "trend",
              fluidPage(
                fluidRow(
                  box(
                    title = "Genre Diffusion & Artist Trend Explorer",
                    width = 12,
                    solidHeader = TRUE,
                    status = "primary",
                    collapsible = TRUE,
                    tabPanel("Trend Overview",
                             fluidRow(
                               column(
                                 width = 4,
                                 pickerInput("trend_genre", "Select Genre(s)",
                                             choices = unique(na.omit(nodes_tbl$genre)),
                                             selected = unique(na.omit(nodes_tbl$genre))[1], multiple = TRUE,
                                             options = list(`actions-box` = TRUE)),
                                 sliderInput("trend_year_range", "Year Range",
                                             min = 1983, max = 2038, value = c(2005, 2025), sep = ""),
                                 checkboxGroupInput("trend_layers", "Show Layers",
                                                    choices = c("Artist Count", "Song Count", "Newcomer Count"),
                                                    selected = c("Song Count")),
                                 hr(),
                                 downloadButton("download_trend_data", "📥 Export Trend Data")
                               ),
                               column(
                                 width = 8,
                                 tabsetPanel(
                                   tabPanel("Yearly Heatmap", plotlyOutput("trend_heatmap", height = "500px")),
                                   tabPanel("Cumulative Curve", plotlyOutput("trend_cumulative_plot", height = "500px"))
                                 )
                               )
                             )
                    )
                  )
                )
              )
      )   #Trend Dashboard UI
      
      
    )   # End of tabItems
  )   # End of dashboardBody
)   #dashboard page





#—————————————————————————————————————————————————————————————————————————————————————————— 
server <- function(input, output, session) {
  
  
  filtered_edges <- reactive({
    req(input$network_depth, input$edge_type)
    
    selected_edges_raw <- if (input$network_depth == 1) {
      edges_from_sailor_filtered
    } else if (input$network_depth == 2) {
      bind_rows(edges_from_sailor_filtered, edges_2nd)
    } else if (input$network_depth == 3) {
      bind_rows(edges_from_sailor_filtered, edges_2nd, edges_people_to_2nd)
    } else {
      all_edges
    }
    
    
    edges_subgraph %>%
      semi_join(selected_edges_raw, by = c("from", "to")) %>%
      filter(edge_type %in% input$edge_type)
  })
  
  
  filtered_nodes <- reactive({
    req(filtered_edges())
    valid_ids <- unique(c(filtered_edges()$from, filtered_edges()$to))
    
    df <- nodes_subgraph %>%
      filter(
        id %in% valid_ids,
        group %in% input$node_type,
        is.na(release_year) |
          (release_year >= input$release_range[1] & release_year <= input$release_range[2])
      )
    
    
    if (!is.null(input$genre_filter)) {
      df <- df %>%
        filter(is.na(genre) | genre %in% input$genre_filter)
    }
    
    df
  })
  
  
  
  observe({
    req(filtered_nodes())
    updatePickerInput(session, "node_name",
                      choices = sort(unique(filtered_nodes()$label)))
  })
  
  output$directGraph <- renderVisNetwork({
    req(filtered_nodes(), filtered_edges())
    
    # 自定义每种 edge_type 的颜色
    edge_colors <- c(
      "CoverOf"             = "#e76f51",
      "ComposerOf"          = "#457b9d",
      "DirectlySamples"     = "#2a9d8f",
      "InStyleOf"           = "#f4a261",
      "InterpolatesFrom"    = "#9d4edd",
      "LyricalReferenceTo"  = "#ffb703",
      "LyricistOf"          = "#219ebc",
      "MemberOf"            = "#8ecae6",
      "PerformerOf"         = "#e63946",
      "ProducerOf"          = "#6a994e"
    )
    
    valid_ids <- filtered_nodes()$id
    
    
    
    edges_all <- filtered_edges() %>%
      filter(from %in% valid_ids, to %in% valid_ids) %>%
      mutate(
        color = edge_colors[edge_type],
        width = 2,
        arrows = "to",
        label = edge_type
      )
    
    
    visNetwork(filtered_nodes(), edges_all, width = "100%", height = "700px") %>%
      visEdges(arrows = "to", color = list(color = edges_all$color)) %>%
      visOptions(highlightNearest = TRUE) %>%
      visLegend(
        position = "right",
        addEdges = data.frame(
          label = c(
            "CoverOf\n\n", "ComposerOf\n\n", "DirectlySamples\n\n", "InStyleOf\n\n",
            "InterpolatesFrom\n\n", "LyricalReferenceTo\n\n", "LyricistOf\n\n",
            "MemberOf\n\n", "PerformerOf\n\n", "ProducerOf\n\n"
          ),
          color = unname(edge_colors)
        )
      )%>%
      visPhysics(solver = "forceAtlas2Based") %>%
      visLayout(randomSeed = 123)
  })
  
  
  observeEvent(input$node_name, {
    req(filtered_nodes())
    node_ids <- filtered_nodes()$id[filtered_nodes()$label %in% input$node_name]
    if (length(node_ids) > 0) {
      visNetworkProxy("directGraph") %>%
        visFocus(id = node_ids[1], scale = 0.7) %>%
        visSelectNodes(id = node_ids)
    }
  })
  
  
  observeEvent(input$notable_filter, {
    req(filtered_nodes())
    
    
    visNetworkProxy("directGraph") %>%
      visSelectNodes(id = character(0))  
    
    if (input$notable_filter == "TRUE") {
      selected_nodes <- filtered_nodes() %>%
        filter(notable == TRUE)
      
    } else if (input$notable_filter == "FALSE") {
      selected_nodes <- filtered_nodes() %>%
        filter(notable == FALSE)
      
    } else {
      return()  
    }
    
    if (nrow(selected_nodes) > 0) {
      visNetworkProxy("directGraph") %>%
        visSelectNodes(id = selected_nodes$id)
    }
  })
  
  
  observeEvent(input$release_range_btn, {
    updateSliderInput(session, "release_range", value = c(1983, 2038))
  })
  
  
  observeEvent(input$network_depth_btn, {
    updateSliderInput(session, "network_depth", value = 3)
  })
  
  
  
  output$directTable <- renderDT({
    edges_df <- filtered_edges()
    
    if ("release_date" %in% names(edges_df)) {
      edges_df <- edges_df %>%
        mutate(release_year = as.numeric(substr(as.character(release_date), 1, 4))) %>%
        filter(release_year >= input$release_range[1],
               release_year <= input$release_range[2])
    }
    
    
    if (all(c("from_name", "to_name") %in% names(edges_df))) {
      datatable(
        edges_df %>%
          select(from_name, from_node_type, edge_type,
                 to_name, to_node_type, genre,
                 release_date, notable, written_date, notoriety_date),
        options = list(pageLength = 5, scrollX = TRUE),
        rownames = FALSE
      )
    } else {
      datatable(data.frame(Message = "No data to display"), options = list(dom = 't'))
    }
  })
  output$groupEdgeBarPlot <- renderPlotly({
    req(filtered_edges(), filtered_nodes())
    
    edge_df <- filtered_edges()
    node_df <- filtered_nodes()
    
    node_df <- node_df %>%
      filter(
        input$notable_filter == "All" |
          (input$notable_filter == "TRUE" & notable == TRUE) |
          (input$notable_filter == "FALSE" & (is.na(notable) | notable == FALSE))
      )
    
    edge_from <- edge_df %>%
      left_join(node_df, by = c("from" = "id")) %>%
      rename(node_type = group) %>%
      mutate(direction = "from")
    
    edge_to <- edge_df %>%
      left_join(node_df, by = c("to" = "id")) %>%
      rename(node_type = group) %>%
      mutate(direction = "to")
    
    edge_with_nodes <- bind_rows(edge_from, edge_to) %>%
      filter(!is.na(node_type))
    
    summary_df <- edge_with_nodes %>%
      count(node_type, edge_type)
    
    summary_df$label <- paste0(
      "Node Type: ", summary_df$node_type, "<br>",
      "Edge Type: ", summary_df$edge_type, "<br>",
      "Count: ", summary_df$n
    )
    
    # 对 edge_type 排序
    edge_order <- summary_df %>%
      group_by(edge_type) %>%
      summarise(total = sum(n)) %>%
      arrange(desc(total)) %>%
      pull(edge_type)
    summary_df$edge_type <- factor(summary_df$edge_type, levels = edge_order)
    
    # 对 node_type 排序
    node_order <- summary_df %>%
      group_by(node_type) %>%
      summarise(total = sum(n)) %>%
      arrange(desc(total)) %>%
      pull(node_type)
    summary_df$node_type <- factor(summary_df$node_type, levels = node_order)
    
    # 绘图
    p <- ggplot(summary_df, aes(x = node_type, y = n, fill = edge_type, text = label)) +
      geom_bar(stat = "identity") +
      labs(
        title = "Influences on Sailor Shift by Node Category and Relationship",
        x = "Node Type",
        y = "Count",
        fill = "Edge Type"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(hjust = 1),
        legend.position = "bottom"
      )
    
    ggplotly(p, tooltip = "text") %>%
      layout(
        hoverlabel = list(font = list(color = "white")),
        legend = list(orientation = "h", x = 0.5, y = -0.3, xanchor = "center")
      )
  })
  
  
  output$barInfo <- renderPrint({
    click_data <- event_data("plotly_click")
    
  })

  # ------------- Tab 2: Her Impact & Collaborators Server Logic -------------
  
  # Prepare impact data based on direction
  impact_data <- reactive({
    req(input$impact_direction, input$impact_depth, input$impact_edge_type, input$impact_node_type)
    
    direction <- input$impact_direction
    depth <- input$impact_depth
    
    # Extract subnetwork based on direction
    if (direction == "out") {
      subg <- extract_subnetwork(graph, "Sailor Shift", distance = depth, direction = "out")
    } else if (direction == "in") {
      subg <- extract_subnetwork(graph, "Sailor Shift", distance = depth, direction = "in")
    } else {
      subg <- extract_subnetwork(graph, "Sailor Shift", distance = depth, direction = "all")
    }
    
    # Convert to data frames
    impact_nodes <- igraph::as_data_frame(subg, what = "vertices") %>%
      mutate(id = name, label = name, group = `Node Type`)
    
    impact_edges <- igraph::as_data_frame(subg, what = "edges") %>%
      filter(`Edge Type` %in% input$impact_edge_type) %>%
      rename(from = from, to = to, label = `Edge Type`)
    
    # Filter nodes based on selected types
    impact_nodes <- impact_nodes %>%
      filter(group %in% input$impact_node_type)
    
    # Get used nodes
    used_nodes <- unique(c(impact_edges$from, impact_edges$to))
    impact_nodes <- impact_nodes %>% filter(id %in% used_nodes)
    
    list(nodes = impact_nodes, edges = impact_edges)
  })
  
  # Collaboration data
  collaboration_data <- reactive({
    req(input$collaborator_type, input$collab_year_range)
    
    # Get Sailor Shift's works
    sailor_works <- nodes_tbl %>%
      filter(`Node Type` %in% c("Song", "Album")) %>%
      left_join(edges_tbl_graph, by = c("index" = "to")) %>%
      filter(`Edge Type` == "PerformerOf", from %in% 
             (nodes_tbl %>% filter(node_name == "Sailor Shift") %>% pull(index)))
    
    # Get collaborators
    collaborators <- edges_tbl_graph %>%
      filter(to %in% sailor_works$index, 
             `Edge Type` %in% input$collaborator_type) %>%
      left_join(nodes_tbl %>% select(index, node_name, `Node Type`), by = c("from" = "index")) %>%
      left_join(nodes_tbl %>% select(index, release_date, genre), by = c("to" = "index")) %>%
      filter(!is.na(release_date),
             as.numeric(release_date) >= input$collab_year_range[1],
             as.numeric(release_date) <= input$collab_year_range[2])
    
    collaborators
  })
  
  # Impact Network
  output$impactNetwork <- renderVisNetwork({
    req(impact_data())
    
    data <- impact_data()
    nodes <- data$nodes
    edges <- data$edges
    
    if (nrow(nodes) == 0 || nrow(edges) == 0) {
      return(visNetwork(nodes = data.frame(id = 1, label = "No data"), 
                       edges = data.frame(from = integer(), to = integer())) %>%
               visLayout(randomSeed = 123))
    }
    
    # Edge colors
    edge_colors <- c(
      "CoverOf" = "#e76f51", "ComposerOf" = "#457b9d", "DirectlySamples" = "#2a9d8f",
      "InStyleOf" = "#f4a261", "InterpolatesFrom" = "#9d4edd", "LyricalReferenceTo" = "#ffb703",
      "LyricistOf" = "#219ebc", "MemberOf" = "#8ecae6", "PerformerOf" = "#e63946", "ProducerOf" = "#6a994e"
    )
    
    edges_styled <- edges %>%
      mutate(
        color = edge_colors[label],
        width = 2,
        arrows = "to"
      )
    
    visNetwork(nodes, edges_styled, width = "100%", height = "600px") %>%
      visEdges(arrows = "to", color = list(color = edges_styled$color)) %>%
      visOptions(highlightNearest = TRUE) %>%
      visLegend(position = "right", addEdges = data.frame(
        label = names(edge_colors), color = unname(edge_colors)
      )) %>%
      visPhysics(solver = "forceAtlas2Based") %>%
      visLayout(randomSeed = 123)
  })
  
  # Collaboration Timeline
  output$collabTimeline <- renderPlotly({
    req(collaboration_data())
    
    collab_data <- collaboration_data() %>%
      mutate(
        year = as.numeric(substr(release_date, 1, 4)),
        collaborator_name = node_name
      ) %>%
      group_by(year, `Edge Type`, collaborator_name) %>%
      summarise(count = n(), .groups = 'drop')
    
    if (nrow(collab_data) == 0) return(plotly_empty())
    
    p <- ggplot(collab_data, aes(x = year, y = count, color = `Edge Type`, 
                                 text = paste("Collaborator:", collaborator_name, "<br>Type:", `Edge Type`, "<br>Count:", count))) +
      geom_line(size = 1.2) +
      geom_point(size = 3) +
      labs(title = "Sailor Shift Collaboration Timeline", x = "Year", y = "Collaboration Count") +
      theme_minimal()
    
    ggplotly(p, tooltip = "text")
  })
  
  # Genre Impact Chart
  output$genreImpactChart <- renderPlotly({
    req(impact_data())
    
    data <- impact_data()
    nodes <- data$nodes
    
    genre_summary <- nodes %>%
      filter(!is.na(genre)) %>%
      count(genre) %>%
      arrange(desc(n))
    
    if (nrow(genre_summary) == 0) return(plotly_empty())
    
    plot_ly(genre_summary, x = ~genre, y = ~n, type = "bar", 
            text = ~n, textposition = 'auto') %>%
      layout(
        xaxis = list(title = "Genre"),
        yaxis = list(title = "Count")
      )
  })
  
  # Influence Strength Chart
  output$influenceStrengthChart <- renderPlotly({
    req(impact_data())
    
    data <- impact_data()
    edges <- data$edges
    
    edge_summary <- edges %>%
      count(label) %>%
      arrange(desc(n))
    
    if (nrow(edge_summary) == 0) return(plotly_empty())
    
    plot_ly(edge_summary, x = ~label, y = ~n, type = "bar", 
            text = ~n, textposition = 'auto') %>%
      layout(
        xaxis = list(title = "Edge Type"),
        yaxis = list(title = "Count")
      )
  })
  
  # Collaboration Heatmap
  output$collaborationHeatmap <- renderPlotly({
    req(collaboration_data())
    
    collab_data <- collaboration_data() %>%
      mutate(year = as.numeric(substr(release_date, 1, 4))) %>%
      group_by(year, `Edge Type`) %>%
      summarise(count = n(), .groups = 'drop')
    
    if (nrow(collab_data) == 0) return(plotly_empty())
    
    # Create heatmap
    p <- ggplot(collab_data, aes(x = factor(year), y = `Edge Type`, fill = count)) +
      geom_tile() +
      scale_fill_viridis_c() +
      labs(x = "Year", y = "Collaboration Type") +
      theme_minimal()
    
    ggplotly(p) %>%
      layout(
        xaxis = list(title = "Year"),
        yaxis = list(title = "Collaboration Type")
      )
  })
  
  # Impact Metrics
  output$impactMetrics <- renderPlotly({
    req(impact_data())
    
    data <- impact_data()
    nodes <- data$nodes
    
    # Calculate metrics
    total_nodes <- nrow(nodes)
    node_types <- nodes %>% count(group)
    notable_count <- sum(nodes$notable == TRUE, na.rm = TRUE)
    
    metrics_df <- data.frame(
      Metric = c("Total Connected Nodes", "Node Types", "Notable Works"),
      Count = c(total_nodes, nrow(node_types), notable_count)
    )
    
    plot_ly(metrics_df, x = ~Metric, y = ~Count, type = "bar", 
            text = ~Count, textposition = 'auto') %>%
      layout(
        xaxis = list(title = "Metric"),
        yaxis = list(title = "Count")
      )
  })
  
  # Collaborator Network
  output$collaboratorNetwork <- renderPlotly({
    req(collaboration_data())
    
    collab_data <- collaboration_data() %>%
      count(node_name, `Edge Type`) %>%
      arrange(desc(n))
    
    if (nrow(collab_data) == 0) return(plotly_empty())
    
    plot_ly(collab_data, x = ~node_name, y = ~n, color = ~`Edge Type`, type = "bar") %>%
      layout(
        xaxis = list(title = "Collaborator"),
        yaxis = list(title = "Collaboration Count")
      )
  })
  
  # Impact Table
  output$impactTable <- renderDT({
    req(impact_data())
    
    data <- impact_data()
    nodes <- data$nodes
    edges <- data$edges
    
    # Create summary table
    summary_data <- edges %>%
      left_join(nodes %>% select(id, from_name = label, from_type = group), by = c("from" = "id")) %>%
      left_join(nodes %>% select(id, to_name = label, to_type = group, genre, notable), by = c("to" = "id")) %>%
      select(from_name, from_type, label, to_name, to_type, genre, notable) %>%
      rename("From" = from_name, "From Type" = from_type, "Relationship" = label, 
             "To" = to_name, "To Type" = to_type, "Genre" = genre, "Notable" = notable)
    
    datatable(summary_data, 
              options = list(pageLength = 10, scrollX = TRUE),
              rownames = FALSE)
  })
  

  # ------------- Genre Diffusion Tracker Sever Part -----------------
  graph_data <- fromJSON("data/MC1_graph.json")
  nodes_df <- as.data.frame(graph_data$nodes)
  edges_df <- as.data.frame(graph_data$links)
  
  all_nodes <- nodes_df
  
  # Reactive filtering
  filtered <- reactive({
    nodes <- all_nodes
    edges <- edges_df
    
    nodes <- nodes %>%
      filter(`Node Type` %in% c("Song", "Album"), !is.na(genre))
    
    yr <- input$yearRange
    if (!is.null(yr)) {
      nodes <- nodes %>%
        filter(!is.na(release_date) & as.numeric(release_date) >= yr[1] & as.numeric(release_date) <= yr[2])
    }
    
    if (!is.null(input$nodeType) && input$nodeType != "") {
      nodes <- nodes %>% filter(`Node Type` == input$nodeType)
    }
    
    edges <- edges %>%
      left_join(nodes %>% select(id, genre), by = c("source" = "id")) %>%
      rename(source_genre = genre) %>%
      left_join(nodes %>% select(id, genre), by = c("target" = "id")) %>%
      rename(target_genre = genre)
    
    list(nodes = nodes, edges = edges)
  })
  
  observe({
    updateSelectInput(session, "mainGenre",
                      choices = sort(unique(na.omit(nodes_df$genre))),
                      selected = "Oceanus Folk")
  })
  
  output$trendPlot <- renderPlotly({
    data <- filtered()$nodes
    if (nrow(data) == 0) return(NULL)
    
    df <- data %>% 
      filter(!is.na(release_date)) %>%
      mutate(Year = as.numeric(release_date)) %>%
      group_by(Year, genre) %>%
      summarize(Count = n(), .groups = 'drop')
    
    if (nrow(df) == 0) return(NULL)
    
    p <- ggplot(df, aes(x = Year, y = Count, fill = genre)) +
      geom_area(alpha = 0.6) +
      labs(x = "Year", y = "Affected Count", fill = "Genre") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$genreNetPre <- renderVisNetwork({
    data <- filtered()
    nodes <- data$nodes
    edges <- data$edges
    if (nrow(nodes) == 0 || nrow(edges) == 0) return(NULL)
    
    main_genre <- input$mainGenre
    fame_cut <- input$fameYear
    
    pre_ids <- nodes %>% filter(genre == main_genre, as.numeric(release_date) < fame_cut) %>% pull(id)
    if (length(pre_ids) == 0) return(NULL)
    
    if (input$hopDepth == 1) {
      edges_sub <- edges %>% filter(source %in% pre_ids | target %in% pre_ids)
    } else {
      one_hop <- edges %>% filter(source %in% pre_ids | target %in% pre_ids) %>% pull(source, target) %>% unlist() %>% unique()
      edges_sub <- edges %>% filter(source %in% c(pre_ids, one_hop) | target %in% c(pre_ids, one_hop))
    }
    nodes_sub <- nodes %>% filter(id %in% unique(c(edges_sub$source, edges_sub$target)))
    
    vis_nodes <- data.frame(id = nodes_sub$id, label = nodes_sub$name, group = nodes_sub$genre)
    vis_edges <- data.frame(from = edges_sub$source, to = edges_sub$target, arrows = "to")
    
    visNetwork(vis_nodes, vis_edges) %>%
      visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
      visLegend(useGroups = TRUE) %>%
      visEvents(select = "function(nodes) {
                Shiny.setInputValue('genreNetPre_selected', nodes.nodes[0], {priority: 'event'});
                }")
  })
  
  output$genreNetPost <- renderVisNetwork({
    data <- filtered()
    nodes <- data$nodes
    edges <- data$edges
    if (nrow(nodes) == 0 || nrow(edges) == 0) return(NULL)
    
    main_genre <- input$mainGenre
    fame_cut <- input$fameYear
    
    post_ids <- nodes %>% filter(genre == main_genre, as.numeric(release_date) >= fame_cut) %>% pull(id)
    if (length(post_ids) == 0) return(NULL)
    
    if (input$hopDepth == 1) {
      edges_sub <- edges %>% filter(source %in% post_ids | target %in% post_ids)
    } else {
      one_hop <- edges %>% filter(source %in% post_ids | target %in% post_ids) %>% pull(source, target) %>% unlist() %>% unique()
      edges_sub <- edges %>% filter(source %in% c(post_ids, one_hop) | target %in% c(post_ids, one_hop))
    }
    nodes_sub <- nodes %>% filter(id %in% unique(c(edges_sub$source, edges_sub$target)))
    
    vis_nodes <- data.frame(id = nodes_sub$id, label = nodes_sub$name, group = nodes_sub$genre)
    vis_edges <- data.frame(from = edges_sub$source, to = edges_sub$target, arrows = "to")
    
    visNetwork(vis_nodes, vis_edges) %>%
      visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
      visLegend(useGroups = TRUE) %>%
      visEvents(select = "function(nodes) {
                      Shiny.setInputValue('genreNetPost_selected', nodes.nodes[0], {priority: 'event'});
      }")
    
  })
  
  output$topInfluencedPlot <- renderPlotly({
    data <- filtered()
    nodes <- data$nodes
    edges <- data$edges
    genre_selected <- input$top_artist_genre
    top_n <- input$top_n
    
    if (nrow(nodes) == 0 || nrow(edges) == 0) return(NULL)
    
    # Count how often each artist is the target of an edge where source is mainGenre
    main_ids <- nodes %>% filter(genre == genre_selected) %>% pull(id)
    filtered_edges <- edges %>% filter(source %in% main_ids, !is.na(target))
    
    target_counts <- filtered_edges %>%
      group_by(target) %>%
      summarize(InfluenceCount = n(), .groups = "drop") %>%
      arrange(desc(InfluenceCount)) %>%
      head(top_n)
    
    top_nodes <- nodes %>% filter(id %in% target_counts$target) %>% select(id, name, genre)
    plot_data <- target_counts %>% left_join(top_nodes, by = c("target" = "id"))
    
    p <- ggplot(plot_data, aes(x = reorder(name, InfluenceCount), y = InfluenceCount, fill = genre)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      labs(x = "Artist", y = "Influence Count", title = "Top Influenced Artists") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  
  
  selectedNodeId <- reactiveVal(NULL)
  
  observeEvent(input$genreNetPre_selected, {
    selectedNodeId(input$genreNetPre_selected)
  })
  observeEvent(input$genreNetPost_selected, {
    selectedNodeId(input$genreNetPost_selected)
  })
  
  observeEvent(event_data("plotly_click", source = "sankeyGenre"), {
    clicked <- event_data("plotly_click", source = "sankeyGenre")
    if (!is.null(clicked)) {
      node_label <- clicked[["label"]]
      
      if (!is.null(node_label) && node_label %in% nodes_df$name) {
        node_row <- nodes_df %>% filter(name == node_label)
        if (nrow(node_row) > 0) {
          selectedNodeId(node_row$id[1])
        }
      } else {
        cat("⚠️ Node label not found in nodes_df$name: ", node_label, "\n")
      }
    }
  })
  
  
  
  output$detailPanel <- renderUI({
    sel_id <- selectedNodeId()
    if (is.null(sel_id)) {
      return(tags$p("Click a node in the network or Sankey diagram to view details."))
    }
    
    node_row <- nodes_df %>% filter(id == sel_id)
    if (nrow(node_row) == 0) return(NULL)
    
    name    <- node_row$name
    works   <- ifelse(!is.null(node_row$representative_works), node_row$representative_works, "N/A")
    activeY <- ifelse(!is.null(node_row$release_date), node_row$release_date, "Unknown")
    genre   <- ifelse(!is.null(node_row$genre), node_row$genre, "Unknown")
    type    <- ifelse(!is.null(node_row$`Node Type`), node_row$`Node Type`, "Unknown")
    notable <- ifelse(!is.null(node_row$notable), as.character(node_row$notable), "N/A")
    
    sshift_id <- nodes_df$id[nodes_df$name == "Sailor Shift"]
    related <- any(edges_df$Edge.Type %in% c("MemberOf", "InStyleOf", "LyricistOf", "LyricalReferenceTo") &
                     ((edges_df$source == sel_id & edges_df$target == sshift_id) |
                        (edges_df$source == sshift_id & edges_df$target == sel_id)))
    
    tagList(
      h4(paste0("Name: ", name)),
      p(paste0("Genre: ", genre)),
      p(paste0("Type: ", type)),
      p(paste0("Representative Works: ", works)),
      p(paste0("Active Year: ", activeY)),
      p(paste0("Notable: ", notable)),
      p(paste0("Collaboration / Style Similarity with Sailor Shift: ", ifelse(related, "Yes", "No")))
    )
  })
  
  
  
  output$topGenreSankeyPlotly <- renderPlotly({
    data <- filtered()
    edges <- data$edges
    nodes <- data$nodes
    if (nrow(edges) == 0 || nrow(nodes) == 0) return(NULL)
    
    genre_selected <- input$top_artist_genre
    top_n <- input$top_n
    
    main_ids <- nodes %>% filter(genre == genre_selected) %>% pull(id)
    sankey_edges <- edges %>% filter(source %in% main_ids)
    
    top_targets <- sankey_edges %>%
      group_by(target) %>%
      summarize(value = n(), .groups = 'drop') %>%
      top_n(top_n, wt = value)
    
    top_artists <- nodes %>%
      filter(id %in% top_targets$target) %>%
      select(id, artist_name = name, genre) %>%
      mutate(label = "Unknown Label")
    
    # build 3-layer sankey: Genre → Artist → Label (if label exists)
    sankey_df <- top_artists %>%
      mutate(genre_node = genre_selected,
             artist_node = artist_name,
             label_node = ifelse(is.na(label), "Unknown Label", label))
    
    # node list
    nodes_list <- unique(c(sankey_df$genre_node, sankey_df$artist_node, sankey_df$label_node))
    sankey_nodes_df <- data.frame(name = nodes_list, stringsAsFactors = FALSE)
    
    # links: Genre → Artist
    link1 <- sankey_df %>%
      count(source = genre_node, target = artist_node, wt = 1)
    
    # links: Artist → Label
    link2 <- sankey_df %>%
      count(source = artist_node, target = label_node, wt = 1)
    
    links_df <- bind_rows(link1, link2) %>%
      mutate(source_id = match(source, nodes_list) - 1,
             target_id = match(target, nodes_list) - 1)
    
    # Color
    base_colors <- c("#007bff", "#ff9999", "#66c2a5", "#e78ac3", "#f9c74f","#8da0cb","#a6d854","#fc8d62")
    node_colors <- alpha(colorRampPalette(
      c("#007bff", "#ff9999", "#66c2a5", "#e78ac3", "#f9c74f","#8da0cb","#a6d854","#fc8d62"))(length(nodes_list)), 0.6)
    link_colors <- scales::alpha(
      col_factor(
        palette = colorRampPalette(c("#007bff", "#ff9999", "#66c2a5", "#e78ac3", "#f9c74f","#8da0cb","#a6d854","#fc8d62"))(length(unique(links_df$source))),
        domain = links_df$source
      )(links_df$source),
      alpha = 0.4
    )
    
    # Build Sankey
    plot_ly(
      type = "sankey",
      source = "sankeyGenre",
      domain = list(x = c(0, 1), y = c(0, 1)),
      orientation = "h",
      node = list(
        label = sankey_nodes_df$name,
        color = node_colors,
        pad = 15,
        thickness = 20,
        line = list(color = "gray30", width = 0.5),
        hovertemplate = paste("Node: %{label}<extra></extra>")
      ),
      link = list(
        source = links_df$source_id,
        target = links_df$target_id,
        value = links_df$n,
        color = link_colors,
        hoverinfo = "all",
        hoverlabel = list(bgcolor = "white")
      )
    ) %>%
      layout(
        font = list(size = 12),
        margin = list(l = 20, r = 20, b = 20, t = 40)
      )
  })
  
  
  observe({
    updateSelectInput(session, "top_artist_genre",
                      choices = sort(unique(na.omit(nodes_df$genre))),
                      selected = "Oceanus Folk")
  })
  
  output$chordDiagram <- renderPlot({
    library(circlize)
    circos.clear()
    
    genre_matrix <- processedData()$genre_matrix
    
    cat("Matrix size: ", dim(genre_matrix), "\n")
    print(genre_matrix[1:5, 1:5])  # 打印前几行
    
    if (is.null(genre_matrix) || nrow(genre_matrix) == 0) {
      cat("⚠️ genre_matrix is empty.\n")
      return(NULL)
    }
    
    genre_list <- union(rownames(genre_matrix), colnames(genre_matrix))
    genre_colors <- setNames(
      colorRampPalette(c("#007bff", "#ff9999", "#66c2a5", "#e78ac3", "#f9c74f", "#8da0cb", "#a6d854", "#fc8d62"))(length(genre_list)),
      genre_list
    )
    
    cat("✅ chordDiagram starting...\n")
    
    chordDiagram(
      genre_matrix,
      transparency = 0.3,
      grid.col = genre_colors,
      annotationTrack = "grid",
      preAllocateTracks = 1
    )
  })
  
  
  # --- Server: Talent Radar & Snapshot Logic ---
  # 1) Ensure g_tbl has 'name' attribute for extract_subnetwork()
  library(tidygraph)
  # Prepare igraph for extract_subnetwork
  library(tidygraph)
  g_tbl <- tbl_graph(nodes = nodes_tbl, edges = edges_tbl_graph, directed = TRUE) %>%
    activate(nodes) %>%
    mutate(name = node_name)
  # convert to igraph object
  g_igraph <- as.igraph(g_tbl)
  
  
  # 2) Dynamic artist list by genre
  available_artists <- reactive({
    req(input$talent_genre, input$talent_year_range)
    year_range <- input$talent_year_range
    song_ids <- nodes_tbl %>%
      filter(
        genre %in% input$talent_genre,
        `Node Type` %in% c("Song", "Album"),
        !is.na(release_date),
        as.numeric(release_date) >= year_range[1],
        as.numeric(release_date) <= year_range[2]
      ) %>% pull(index)
    artist_ids <- edges_tbl_graph %>%
      filter(to %in% song_ids, `Edge Type` %in% person_edge_types) %>% pull(from) %>% unique()
    topN <- as.numeric(input$talent_topN)
    top_artists <- weighted_scores() %>%
      filter(label %in% (nodes_tbl %>% filter(index %in% artist_ids, `Node Type` == "Person") %>% pull(node_name))) %>%
      filter(label != "Sailor Shift") %>%  # 排除Sailor Shift
      arrange(desc(weighted_score)) %>%
      pull(label) %>%
      head(topN)
    if (length(top_artists) == 0) return("无艺术家")
    top_artists
  })
  
  # 3) Compare artists picker
  output$select_compare_artists <- renderUI({
    choices <- available_artists()
    pickerInput("compare_artists", "🎯 Select Artists to Compare",
                choices = choices,
                selected = if(length(choices) >= 2) head(choices, 2) else choices,
                multiple = TRUE,
                options = list(`actions-box` = TRUE, `max-options` = as.numeric(input$talent_topN))
    )
  })
  
  # 4) Sync snapshot dropdown
  observe({
    updatePickerInput(
      session, "snapshot_artist_detail",
      choices = input$compare_artists,
      selected = input$compare_artists[1]
    )
  })
  
  # 5) Compute weighted scores with Notable Works Count metric
  weighted_scores <- reactive({
    year_range <- input$talent_year_range
    df <- talent_score_df %>%
      left_join(nodes_tbl %>% distinct(node_name,index), by = c("label" = "node_name"), relationship = "many-to-many")
    song_ids <- nodes_tbl %>%
      filter(
        genre %in% input$talent_genre,
        `Node Type` %in% c("Song", "Album"),
        !is.na(release_date),
        as.numeric(release_date) >= year_range[1],
        as.numeric(release_date) <= year_range[2]
      ) %>% pull(index)
    sim_counts <- edges_tbl_graph %>%
      filter(to %in% song_ids, `Edge Type` %in% person_edge_types) %>%
      count(from, name = "sim_count")
    notable_ids <- nodes_tbl %>%
      filter(`Node Type` %in% c("Song", "Album"), notable == TRUE, !is.na(release_date), as.numeric(release_date) >= year_range[1], as.numeric(release_date) <= year_range[2]) %>% pull(index)
    not_counts <- edges_tbl_graph %>%
      filter(to %in% notable_ids, `Edge Type` %in% person_edge_types) %>%
      count(from, name = "notable_count")
    df <- df %>%
      left_join(sim_counts, by = c("index" = "from")) %>%
      left_join(not_counts, by = c("index" = "from")) %>%
      mutate(
        sim_count = replace_na(sim_count, 0),
        style_similarity = sim_count / (max(sim_count, 1) + 1),
        notable_count = replace_na(notable_count, 0),
        notable_count_norm = notable_count / (max(notable_count, 1))
      )
    df <- df %>%
      mutate(
        degree_norm = scales::rescale(degree),
        pagerank_norm = scales::rescale(pagerank)
      )
    w_pr <- input$weight_pagerank
    w_deg <- input$weight_degree
    w_sim <- input$weight_similarity
    w_not <- input$weight_notable_count
    df <- df %>%
      mutate(
        weighted_score = pagerank_norm * w_pr +
          degree_norm   * w_deg +
          style_similarity * w_sim +
          notable_count_norm * w_not
      ) %>%
      distinct(label, .keep_all = TRUE)
    df
  })
  
  # 6) Scoreboard and Radar outputs
  output$talent_score_table <- renderDT({
    df <- weighted_scores() %>%
      filter(label %in% available_artists()) %>%
      arrange(desc(weighted_score)) %>%
      mutate(
        PageRank = round(pagerank_norm, 2),
        Degree = round(degree_norm, 2),
        StyleSim = round(style_similarity, 2),
        NotableCountNorm = round(notable_count_norm, 2),
        Score = round(weighted_score, 2)
      ) %>%
      select(label, PageRank, Degree, StyleSim, NotableCountNorm, Score)
    if(nrow(df) == 0 || (nrow(df) == 1 && df$label[1] == "无艺术家")) {
      datatable(data.frame(提示 = "无数据可显示"), options = list(dom = 't'))
    } else {
      datatable(
        df,
        options = list(
          pageLength = 25,
          scrollX = TRUE,
          rowCallback = JS(
            'function(row, data) {',
            'if(data[0] == "Sailor Shift"){',
            '  $(row).css("background-color", "#FFFACD");',
            '}',
            '}'
          )
        ),
        rownames = FALSE
      )
    }
  })
  
  output$talent_radar_plot <- renderPlotly({
    selected <- input$compare_artists
    selected <- selected[!is.na(selected) & selected != ""]
    all_labels <- unique(c("Sailor Shift", selected))
    df <- weighted_scores() %>% filter(label %in% all_labels, !is.na(label), label != "")
    if(nrow(df) == 0 || (nrow(df) == 1 && df$label[1] == "无艺术家")) return(plotly_empty())
    metrics <- c("degree_norm", "pagerank_norm", "notable_count_norm", "style_similarity")
    labels  <- c("Degree",      "PageRank",      "NotableCount",      "StyleSim")
    p <- plot_ly(type = 'scatterpolar', mode = 'lines+markers')
    for(i in seq_len(nrow(df))) {
      vals <- as.numeric(df[i, metrics])
      closed_vals  <- c(vals, vals[1])
      closed_theta <- c(labels, labels[1])
      is_sailor <- df$label[i] == "Sailor Shift"
      if (is_sailor) {
        p <- p %>%
          add_trace(
            r     = closed_vals,
            theta = closed_theta,
            name  = df$label[i],
            fill  = 'toself',
            line  = list(color = "#FF9999", width = 4),
            marker = list(color = "#FF9999", size = 10),
            legendgroup = "Sailor Shift",
            showlegend = TRUE
          )
      } else {
        p <- p %>%
          add_trace(
            r     = closed_vals,
            theta = closed_theta,
            name  = df$label[i],
            fill  = 'toself',
            line  = list(width = 2),
            marker = list(size = 6),
            showlegend = TRUE
          )
      }
    }
    p %>%
      layout(
        polar = list(
          radialaxis = list(visible = TRUE, range = c(0, 1))
        )
      )
  })
  
  # 7) Download handler unchanged
  output$download_weighted_scores <- downloadHandler(
    filename=function(){paste0("talent_scores_",Sys.Date(),".csv")},
    content=function(file){write.csv(weighted_scores(), file, row.names=FALSE)}
  )
  
  # 8) Snapshot subgraph rendering with styled network (like Influence Analysis)
  observeEvent(input$snapshot_artist_detail, {
    req(input$snapshot_artist_detail)
    # 1. Extract subnetwork using igraph
    subg <- extract_subnetwork(
      graph      = g_igraph,
      node_name  = input$snapshot_artist_detail,
      distance   = input$snap_network_depth,
      direction  = "all",
      edge_types = input$snap_edge_type,
      node_types = input$snap_node_type
    )
    
    vs_nodes <- as_tibble(subg, active = "nodes") %>%
      mutate(
        id = index,
        label = node_name,
        group = `Node Type`,
        color = case_when(
          node_name == input$snapshot_artist_detail ~ "#27ae60",
          group == "Person" ~ "#457b9d",
          group == "Song" ~ "#e76f51",
          group == "Album" ~ "#f4a261",
          group == "MusicalGroup" ~ "#2a9d8f",
          group == "RecordLabel" ~ "#9d4edd",
          TRUE ~ "#cccccc"
        ),
        shape = ifelse(node_name == input$snapshot_artist_detail, "star", "dot")
      )
    vs_edges <- as_tibble(subg, active = "edges") %>%
      transmute(
        from,
        to,
        edge_type = `Edge Type`,
        label = `Edge Type`,
        color = case_when(
          edge_type == "CoverOf" ~ "#e76f51",
          edge_type == "ComposerOf" ~ "#457b9d",
          edge_type == "DirectlySamples" ~ "#2a9d8f",
          edge_type == "InStyleOf" ~ "#f4a261",
          edge_type == "InterpolatesFrom" ~ "#9d4edd",
          edge_type == "LyricalReferenceTo" ~ "#ffb703",
          edge_type == "LyricistOf" ~ "#219ebc",
          edge_type == "MemberOf" ~ "#8ecae6",
          edge_type == "PerformerOf" ~ "#e63946",
          edge_type == "ProducerOf" ~ "#6a994e",
          TRUE ~ "#888"
        ),
        width = 2,
        arrows = "to"
      )

    # 检查是否为空
    if (nrow(vs_nodes) == 0 || nrow(vs_edges) == 0) {
      output$snapshot_influence_graph <- renderVisNetwork({
        visNetwork(nodes = data.frame(id = 1, label = "No data"), edges = data.frame(from = integer(), to = integer()))
      })
      return()
    }

    output$snapshot_influence_graph <- renderVisNetwork({
      visNetwork(vs_nodes, vs_edges, width = "100%", height = "600px") %>%
        visNodes(color = list(background = vs_nodes$color), shape = vs_nodes$shape) %>%
        visEdges(arrows = 'to', color = list(color = vs_edges$color), width = vs_edges$width) %>%
        visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
        visLegend(
          position = 'right',
          useGroups = TRUE,
          addNodes = data.frame(
            label = c("Person", "Song", "Album", "MusicalGroup", "RecordLabel", "Selected Artist"),
            color = c("#457b9d", "#e76f51", "#f4a261", "#2a9d8f", "#9d4edd", "#27ae60")
          ),
          addEdges = data.frame(
            label = c("CoverOf", "ComposerOf", "DirectlySamples", "InStyleOf", "InterpolatesFrom", "LyricalReferenceTo", "LyricistOf", "MemberOf", "PerformerOf", "ProducerOf"),
            color = c("#e76f51", "#457b9d", "#2a9d8f", "#f4a261", "#9d4edd", "#ffb703", "#219ebc", "#8ecae6", "#e63946", "#6a994e")
          )
        ) %>%
        visPhysics(solver = 'forceAtlas2Based') %>%
        visLayout(randomSeed = 123)
    })
  })
  
  # --- Trend Dashboard Server ---
  filtered_trend <- reactive({
    req(input$trend_genre, input$trend_year_range)
    nodes <- nodes_tbl %>%
      filter(
        genre %in% input$trend_genre,
        `Node Type` %in% c("Song", "Album"),
        !is.na(release_date),
        as.numeric(release_date) >= input$trend_year_range[1],
        as.numeric(release_date) <= input$trend_year_range[2]
      )
    nodes
  })
  
  output$trend_heatmap <- renderPlotly({
    data <- filtered_trend()
    if (nrow(data) == 0) return(plotly_empty())
    req(input$trend_layers)
    layer <- input$trend_layers[1]
    df <- data %>%
      mutate(Year = as.numeric(release_date))
    if (layer == "Song Count") {
      df_sum <- df %>%
        group_by(Year, genre) %>%
        summarize(Count = n(), .groups = 'drop')
      fill_label <- "Count"
    } else if (layer == "Artist Count") {
      df_sum <- df %>%
        filter(group == "Person") %>%
        group_by(Year, genre) %>%
        summarize(Count = n(), .groups = 'drop')
      fill_label <- "Artist Count"
    } else if (layer == "Newcomer Count") {
      df_person <- df %>%
        filter(group == "Person") %>%
        select(label, Year, genre)  # label 是人名，或用 name 字段

      # 找到每个人首次出现的年份
      first_appear <- df_person %>%
        group_by(label, genre) %>%
        summarize(first_year = min(Year, na.rm = TRUE), .groups = 'drop')

      # 统计每年每 genre 新人数量
      df_sum <- first_appear %>%
        group_by(first_year, genre) %>%
        summarize(Count = n(), .groups = 'drop') %>%
        rename(Year = first_year)
      fill_label <- "Newcomer Count"
    }
    years <- sort(unique(df_sum$Year))
    breaks <- years[seq(1, length(years), by = 2)]
    p <- ggplot(df_sum, aes(x = factor(Year), y = genre, fill = Count)) +
      geom_tile() +
      scale_fill_viridis_c() +
      labs(x = "Year", y = "Genre", fill = fill_label) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_x_discrete(breaks = breaks)
    ggplotly(p)
  })
  
  output$trend_cumulative_plot <- renderPlotly({
    data <- filtered_trend()
    if (nrow(data) == 0) return(plotly_empty())
    req(input$trend_layers)
    layer <- input$trend_layers[1]
    df <- data %>%
      mutate(Year = as.numeric(release_date))
    if (layer == "Song Count") {
      df_sum <- df %>%
        group_by(Year, genre) %>%
        summarize(Count = n(), .groups = 'drop') %>%
        arrange(Year) %>%
        group_by(genre) %>%
        mutate(Cumulative = cumsum(Count))
      p <- ggplot(df_sum, aes(x = Year, y = Cumulative, color = genre)) +
        geom_line(size = 1.2) +
        geom_point(size = 2) +
        labs(x = "Year", y = "Cumulative Works", title = "Cumulative Number of Works") +
        theme_minimal()
    } else if (layer == "Artist Count") {
      df_sum <- df %>%
        filter(group == "Person") %>%
        group_by(Year, genre) %>%
        summarize(Count = n(), .groups = 'drop') %>%
        group_by(Year, genre) %>%
        summarize(NewArtists = n_distinct(artist), .groups = 'drop') %>%
        arrange(Year) %>%
        group_by(genre) %>%
        mutate(Cumulative = cumsum(NewArtists))
      p <- ggplot(df_sum, aes(x = Year, y = Cumulative, color = genre)) +
        geom_line(size = 1.2) +
        geom_point(size = 2) +
        labs(x = "Year", y = "Cumulative Artists", title = "Cumulative Number of Artists") +
        theme_minimal()
    } else if (layer == "Newcomer Count") {
      df_sum <- df %>%
        filter(!is.na(artist)) %>%
        group_by(artist, genre) %>%
        summarize(FirstYear = min(Year), .groups = 'drop') %>%
        group_by(FirstYear, genre) %>%
        summarize(Newcomers = n(), .groups = 'drop') %>%
        arrange(FirstYear) %>%
        group_by(genre) %>%
        mutate(Cumulative = cumsum(Newcomers)) %>%
        rename(Year = FirstYear)
      p <- ggplot(df_sum, aes(x = Year, y = Cumulative, color = genre)) +
        geom_line(size = 1.2) +
        geom_point(size = 2) +
        labs(x = "Year", y = "Cumulative Newcomers", title = "Cumulative Number of Newcomers") +
        theme_minimal()
    }
    ggplotly(p)
  })
  
  output$download_trend_data <- downloadHandler(
    filename=function(){paste0("trend_data_",Sys.Date(),".csv")},
    content=function(file){write.csv(filtered_trend(), file, row.names=FALSE)}
  )
  
  comm_filtered_nodes <- reactive({
    req(input$comm_from_node_type, input$comm_to_node_type)
    nodes_vn %>% filter(group %in% input$comm_from_node_type | group %in% input$comm_to_node_type)
  })
  
  comm_filtered_edges <- reactive({
    req(input$comm_edge_type, input$comm_from_node_type, input$comm_to_node_type)
    # 找到所有from/to节点类型符合要求的id
    from_ids <- nodes_vn$id[nodes_vn$group %in% input$comm_from_node_type]
    to_ids <- nodes_vn$id[nodes_vn$group %in% input$comm_to_node_type]
    edges_final %>%
      filter(
        label %in% input$comm_edge_type,
        from %in% from_ids,
        to %in% to_ids
      )
  })
  
  output$commNetwork <- renderVisNetwork({
    nodes <- comm_filtered_nodes()
    edges <- comm_filtered_edges() %>%
      filter(from %in% nodes$id, to %in% nodes$id) %>%
      mutate(
        color = case_when(
          label == "CoverOf" ~ "#e76f51",
          label == "ComposerOf" ~ "#457b9d",
          label == "DirectlySamples" ~ "#2a9d8f",
          label == "InStyleOf" ~ "#f4a261",
          label == "InterpolatesFrom" ~ "#9d4edd",
          label == "LyricalReferenceTo" ~ "#ffb703",
          label == "LyricistOf" ~ "#219ebc",
          label == "MemberOf" ~ "#8ecae6",
          label == "PerformerOf" ~ "#e63946",
          label == "ProducerOf" ~ "#6a994e",
          TRUE ~ "#888"
        ),
        width = 2,
        arrows = "to"
      )
    if (nrow(nodes) == 0 || nrow(edges) == 0) {
      return(visNetwork(nodes = data.frame(id = 1, label = "No data"), edges = data.frame(from = integer(), to = integer())))
    }
    visNetwork(nodes, edges, width = "100%", height = "600px") %>%
      visNodes(color = list(background = nodes$color), shape = nodes$shape) %>%
      visEdges(arrows = "to", color = list(color = edges$color)) %>%
      visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
      visLegend(position = "right", addEdges = data.frame(
        label = unique(edges$label), color = unique(edges$color)
      )) %>%
      visPhysics(solver = "forceAtlas2Based") %>%
      visLayout(randomSeed = 123)
  })
  
  output$commEdgeTable <- renderDT({
    edges <- comm_filtered_edges() %>%
      left_join(nodes_vn %>% select(id, from_name = label, from_type = group, from_genre = genre), by = c("from" = "id")) %>%
      left_join(nodes_vn %>% select(id, to_name = label, to_type = group, to_genre = genre), by = c("to" = "id")) %>%
      select(
        from_name, from_type, from_genre,
        to_name, to_type, to_genre,
        label,   # edge type
        length
      ) %>%
      rename(
        "From" = from_name,
        "From Type" = from_type,
        "From Genre" = from_genre,
        "To" = to_name,
        "To Type" = to_type,
        "To Genre" = to_genre,
        "Edge Type" = label,
        "Edge Length" = length
      )
    datatable(edges, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
  })
  
  output$commEdgeSummary <- renderPrint({
    edges <- comm_filtered_edges()
    total_edges <- nrow(edges)
    edge_type_count <- edges %>% count(label, name = "Count")
    genre_count <- edges %>%
      left_join(nodes_vn %>% select(id, genre), by = c("from" = "id")) %>%
      count(genre, name = "Count")
    cat("Total Edges:", total_edges, "\n")
    print(edge_type_count)
    cat("\nFrom Node Genre Distribution:\n")
    print(genre_count)
  })
}

# Run the app
shinyApp(ui = ui, server = server)
