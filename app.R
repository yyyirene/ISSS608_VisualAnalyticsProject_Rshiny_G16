# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#
# shinydashboard
library(shiny)
library(shinydashboard)
library(dplyr)
library(DT)
library(visNetwork)
library(jsonlite)
library(shinyWidgets)
library(lubridate)
library(ggplot2)
library(plotly)
library(networkD3)
library(igraph)
library(tidygraph)
library(tibble)
library(webshot2)
library(htmlwidgets)
library(circlize)
library(scales)
library(reshape2)
library(tidyverse)  # 如果用 tidyverse，可以省略 dplyr/ggplot2/tibble/purrr 等单独加载
library(rlang)
library(purrr)
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



min_year <- min(as.numeric(all_nodes$release_date), na.rm = TRUE)
max_year <- max(as.numeric(all_nodes$release_date), na.rm = TRUE)
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

# 统一 network 节点颜色（含填充和边框）
node_colors <- list(
  Person = list(fill = "#A5C9FF", border = "#1F75FE"),
  Album = list(fill = "#F26C6C", border = "#FF0000"),
  Song = list(fill = "#FFFF00", border = "#FFA500"),
  MusicalGroup = list(fill = "#6BDC37", border = "#008000"),
  RecordLabel = list(fill = "#F279F2", border = "#C000C0")
)
nodes_vn <- nodes_vn %>%
  mutate(color.background = unname(sapply(group, function(g) node_colors[[g]]$fill)),
         color.border = unname(sapply(group, function(g) node_colors[[g]]$border)))

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
                      width = 3,
                      
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
                          "Note: Selecting a node will zoom in and highlight it in the network graph.
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
                                    min = 1983, max = 2040,
                                    value = c(1983, 2040), step = 1, sep = ""
                        ),
                        
                        sliderInput("network_depth", "Select Network Depth (Layers from Sailor Shift)",
                                    min = 1, max = 3, value = 2, step = 1,
                                    ticks = TRUE, animate = TRUE
                        )
                        
                      
                    ),
                    
                    column(
                      width = 9,
                      tabsetPanel(
                        id = "graph_tabs",
                        type = "tabs",
                        tabPanel("Influence Network",
                          fluidRow(
                            column(
                              width = 10,
                              visNetworkOutput("directGraph", height = "650px")
                            ),
                            column(
                              width = 2,
                              tags$div(
                                style = "padding: 0px; margin-top: 20px; font-size: 10px;",  # 去掉背景和边框，字体更小
                                h4("Node Legend", style = "font-size: 12px; font-weight: bold; margin-bottom: 8px;"),
                                tags$div(style="margin-bottom:4px;",
                                  tags$span(style="display:inline-block;width:16px;height:16px;background:#A5C9FF;border:2px solid #1F75FE;margin-right:6px;border-radius:50%;"), "Person"
                                ),
                                tags$div(style="margin-bottom:4px;",
                                  tags$span(style="display:inline-block;width:16px;height:16px;background:#FFFF00;border:2px solid #FFA500;margin-right:6px;border-radius:50%;"), "Song"
                                ),
                                tags$div(style="margin-bottom:4px;",
                                  tags$span(style="display:inline-block;width:16px;height:16px;background:#F26C6C;border:2px solid #FF0000;margin-right:6px;border-radius:50%;"), "Album"
                                ),
                                tags$div(style="margin-bottom:4px;",
                                  tags$span(style="display:inline-block;width:16px;height:16px;background:#6BDC37;border:2px solid #008000;margin-right:6px;border-radius:50%;"), "MusicalGroup"
                                ),
                                tags$div(style="margin-bottom:4px;",
                                  tags$span(style="display:inline-block;width:16px;height:16px;background:#F279F2;border:2px solid #C000C0;margin-right:6px;border-radius:50%;"), "RecordLabel"
                                ),
                                h4("Edge Legend", style = "font-size: 12px; font-weight: bold; margin-bottom: 8px; margin-top: 10px;"),
                                tags$div(style="margin-bottom:4px;",
                                  tags$span(style="display:inline-block;width:20px;height:3px;background:#e76f51;margin-right:6px;vertical-align:middle;"), "CoverOf"
                                ),
                                tags$div(style="margin-bottom:4px;",
                                  tags$span(style="display:inline-block;width:20px;height:3px;background:#457b9d;margin-right:6px;vertical-align:middle;"), "ComposerOf"
                                ),
                                tags$div(style="margin-bottom:4px;",
                                  tags$span(style="display:inline-block;width:20px;height:3px;background:#2a9d8f;margin-right:6px;vertical-align:middle;"), "DirectlySamples"
                                ),
                                tags$div(style="margin-bottom:4px;",
                                  tags$span(style="display:inline-block;width:20px;height:3px;background:#f4a261;margin-right:6px;vertical-align:middle;"), "InStyleOf"
                                ),
                                tags$div(style="margin-bottom:4px;",
                                  tags$span(style="display:inline-block;width:20px;height:3px;background:#9d4edd;margin-right:6px;vertical-align:middle;"), "InterpolatesFrom"
                                ),
                                tags$div(style="margin-bottom:4px;",
                                  tags$span(style="display:inline-block;width:20px;height:3px;background:#ffb703;margin-right:6px;vertical-align:middle;"), "LyricalReferenceTo"
                                ),
                                tags$div(style="margin-bottom:4px;",
                                  tags$span(style="display:inline-block;width:20px;height:3px;background:#219ebc;margin-right:6px;vertical-align:middle;"), "LyricistOf"
                                ),
                                tags$div(style="margin-bottom:4px;",
                                  tags$span(style="display:inline-block;width:20px;height:3px;background:#8ecae6;margin-right:6px;vertical-align:middle;"), "MemberOf"
                                ),
                                tags$div(style="margin-bottom:4px;",
                                  tags$span(style="display:inline-block;width:20px;height:3px;background:#e63946;margin-right:6px;vertical-align:middle;"), "PerformerOf"
                                ),
                                tags$div(style="margin-bottom:4px;",
                                  tags$span(style="display:inline-block;width:20px;height:3px;background:#6a994e;margin-right:6px;vertical-align:middle;"), "ProducerOf"
                                )
                              )
                            )
                          )
                        ),
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
                  "Her Impact",
                  
                  br(),
                
                  fluidRow(
                    column(
                      width = 3,
                     
                  
                      
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
                        sliderInput(
                          inputId = "impact_depth",
                          label = "Network Depth",
                          min = 1, max = 2, value = 1, step = 1,
                          ticks = TRUE, animate = TRUE
                        )
                       
                       
                    
                      
                    ),
                    
                    column(
                      width = 9,
                      tabsetPanel(
                        id = "impact_tabs",
                        type = "tabs",
                        tabPanel("Impact Network",
                          fluidRow(
                            column(
                              width = 10,
                              visNetworkOutput("impactNetwork", height = "600px")
                            ),
                            column(
                              width = 2,
                              tags$div(
                                style = "padding: 0px; margin-top: 20px; font-size: 10px;",  # 去掉背景和边框，字体更小
                                h4("Node Legend", style = "font-size: 12px; font-weight: bold; margin-bottom: 8px;"),
                                tags$div(style="margin-bottom:4px;",
                                  tags$span(style="display:inline-block;width:16px;height:16px;background:#A5C9FF;border:2px solid #1F75FE;margin-right:6px;border-radius:50%;"), "Person"
                                ),
                                tags$div(style="margin-bottom:4px;",
                                  tags$span(style="display:inline-block;width:16px;height:16px;background:#FFFF00;border:2px solid #FFA500;margin-right:6px;border-radius:50%;"), "Song"
                                ),
                                tags$div(style="margin-bottom:4px;",
                                  tags$span(style="display:inline-block;width:16px;height:16px;background:#F26C6C;border:2px solid #FF0000;margin-right:6px;border-radius:50%;"), "Album"
                                ),
                                tags$div(style="margin-bottom:4px;",
                                  tags$span(style="display:inline-block;width:16px;height:16px;background:#6BDC37;border:2px solid #008000;margin-right:6px;border-radius:50%;"), "MusicalGroup"
                                ),
                                tags$div(style="margin-bottom:4px;",
                                  tags$span(style="display:inline-block;width:16px;height:16px;background:#F279F2;border:2px solid #C000C0;margin-right:6px;border-radius:50%;"), "RecordLabel"
                                ),
                                h4("Edge Legend", style = "font-size: 12px; font-weight: bold; margin-bottom: 8px; margin-top: 10px;"),
                                tags$div(style="margin-bottom:4px;",
                                  tags$span(style="display:inline-block;width:20px;height:3px;background:#e76f51;margin-right:6px;vertical-align:middle;"), "CoverOf"
                                ),
                                tags$div(style="margin-bottom:4px;",
                                  tags$span(style="display:inline-block;width:20px;height:3px;background:#457b9d;margin-right:6px;vertical-align:middle;"), "ComposerOf"
                                ),
                                tags$div(style="margin-bottom:4px;",
                                  tags$span(style="display:inline-block;width:20px;height:3px;background:#2a9d8f;margin-right:6px;vertical-align:middle;"), "DirectlySamples"
                                ),
                                tags$div(style="margin-bottom:4px;",
                                  tags$span(style="display:inline-block;width:20px;height:3px;background:#f4a261;margin-right:6px;vertical-align:middle;"), "InStyleOf"
                                ),
                                tags$div(style="margin-bottom:4px;",
                                  tags$span(style="display:inline-block;width:20px;height:3px;background:#9d4edd;margin-right:6px;vertical-align:middle;"), "InterpolatesFrom"
                                ),
                                tags$div(style="margin-bottom:4px;",
                                  tags$span(style="display:inline-block;width:20px;height:3px;background:#ffb703;margin-right:6px;vertical-align:middle;"), "LyricalReferenceTo"
                                ),
                                tags$div(style="margin-bottom:4px;",
                                  tags$span(style="display:inline-block;width:20px;height:3px;background:#219ebc;margin-right:6px;vertical-align:middle;"), "LyricistOf"
                                ),
                                tags$div(style="margin-bottom:4px;",
                                  tags$span(style="display:inline-block;width:20px;height:3px;background:#8ecae6;margin-right:6px;vertical-align:middle;"), "MemberOf"
                                ),
                                tags$div(style="margin-bottom:4px;",
                                  tags$span(style="display:inline-block;width:20px;height:3px;background:#e63946;margin-right:6px;vertical-align:middle;"), "PerformerOf"
                                ),
                                tags$div(style="margin-bottom:4px;",
                                  tags$span(style="display:inline-block;width:20px;height:3px;background:#6a994e;margin-right:6px;vertical-align:middle;"), "ProducerOf"
                                )
                              )
                            )
                          )
                        )
        
           
                     
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
                      width = 3,
                      
                       
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
                      
                      
                    ),
                    column(
                      width = 9,
                      tabsetPanel(
                        id = "comm_tabs",
                        type = "tabs",
                        tabPanel("Community Network",
                          fluidRow(
                            column(
                              width = 10,
                              visNetworkOutput("commNetwork", height = "600px")
                            ),
                            column(
                              width = 2,
                              tags$div(
                                style = "padding: 0px; margin-top: 20px; font-size: 10px;",  # 去掉背景和边框，字体更小
                                h4("Node Legend", style = "font-size: 12px; font-weight: bold; margin-bottom: 8px;"),
                                tags$div(style="margin-bottom:4px;",
                                         tags$span(style="display:inline-block;width:16px;height:16px;background:#A5C9FF;border:2px solid #1F75FE;margin-right:6px;border-radius:50%;"), "Person"
                                ),
                                tags$div(style="margin-bottom:4px;",
                                         tags$span(style="display:inline-block;width:16px;height:16px;background:#FFFF00;border:2px solid #FFA500;margin-right:6px;border-radius:50%;"), "Song"
                                ),
                                tags$div(style="margin-bottom:4px;",
                                         tags$span(style="display:inline-block;width:16px;height:16px;background:#F26C6C;border:2px solid #FF0000;margin-right:6px;border-radius:50%;"), "Album"
                                ),
                                tags$div(style="margin-bottom:4px;",
                                         tags$span(style="display:inline-block;width:16px;height:16px;background:#6BDC37;border:2px solid #008000;margin-right:6px;border-radius:50%;"), "MusicalGroup"
                                ),
                                tags$div(style="margin-bottom:4px;",
                                         tags$span(style="display:inline-block;width:16px;height:16px;background:#F279F2;border:2px solid #C000C0;margin-right:6px;border-radius:50%;"), "RecordLabel"
                                ),
                                h4("Edge Legend", style = "font-size: 12px; font-weight: bold; margin-bottom: 8px; margin-top: 10px;"),
                                tags$div(style="margin-bottom:4px;",
                                         tags$span(style="display:inline-block;width:20px;height:3px;background:#e76f51;margin-right:6px;vertical-align:middle;"), "CoverOf"
                                ),
                                tags$div(style="margin-bottom:4px;",
                                         tags$span(style="display:inline-block;width:20px;height:3px;background:#457b9d;margin-right:6px;vertical-align:middle;"), "ComposerOf"
                                ),
                                tags$div(style="margin-bottom:4px;",
                                         tags$span(style="display:inline-block;width:20px;height:3px;background:#2a9d8f;margin-right:6px;vertical-align:middle;"), "DirectlySamples"
                                ),
                                tags$div(style="margin-bottom:4px;",
                                         tags$span(style="display:inline-block;width:20px;height:3px;background:#f4a261;margin-right:6px;vertical-align:middle;"), "InStyleOf"
                                ),
                                tags$div(style="margin-bottom:4px;",
                                         tags$span(style="display:inline-block;width:20px;height:3px;background:#9d4edd;margin-right:6px;vertical-align:middle;"), "InterpolatesFrom"
                                ),
                                tags$div(style="margin-bottom:4px;",
                                         tags$span(style="display:inline-block;width:20px;height:3px;background:#ffb703;margin-right:6px;vertical-align:middle;"), "LyricalReferenceTo"
                                ),
                                tags$div(style="margin-bottom:4px;",
                                         tags$span(style="display:inline-block;width:20px;height:3px;background:#219ebc;margin-right:6px;vertical-align:middle;"), "LyricistOf"
                                ),
                                tags$div(style="margin-bottom:4px;",
                                         tags$span(style="display:inline-block;width:20px;height:3px;background:#8ecae6;margin-right:6px;vertical-align:middle;"), "MemberOf"
                                ),
                                tags$div(style="margin-bottom:4px;",
                                         tags$span(style="display:inline-block;width:20px;height:3px;background:#e63946;margin-right:6px;vertical-align:middle;"), "PerformerOf"
                                ),
                                tags$div(style="margin-bottom:4px;",
                                         tags$span(style="display:inline-block;width:20px;height:3px;background:#6a994e;margin-right:6px;vertical-align:middle;"), "ProducerOf"
                                )
                              )
                            )
                          )
                   
                        )
      
                      )# <- 只保留这一个，结束 tabsetPanel
                      
                    ) # <- 结束 column(width = 9)
                  
                  ),# <- 结束 fluidRow
                  
                  fluidRow(
                    column(
                      width = 12,
                      div(
                        style = "padding-left: 15px; padding-right: 15px;",  # 加左右空隙
                        DTOutput("commEdgeTable", width = "100%")
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
                tabPanel("Influence Trend",
                  br(),
                  selectInput(
                    "trend_target_genre",
                    "Select Influence Genre:",
                    choices = sort(unique(na.omit(nodes_tbl$genre))),
                    selected = "Oceanus Folk"
                  ),
                  fluidRow(
                    column(width = 12,
                      selectInput("trend_from_type", "From Node Type", choices = c("Both", "Song", "Album"), selected = "Both"),
                      plotlyOutput("trendPlot", height = "400px")
                    )
                  ),
                  fluidRow(
                    column(width = 12,
                      DTOutput("influenceTrendTable")
                    )
                  )
                ),
                tabPanel("Genre to Genre",
                  br(),
                  fluidRow(
                    column(width = 3,
                      sliderInput(
                        inputId = "pre_sailor_year",
                        label = "Pre Sailor Shift Fame (to 2028)",
                        min = min_year,
                        max = 2028,
                        value = c(min_year, 2028),
                        step = 1,
                        sep = ""
                      ),
                      sliderInput(
                        inputId = "post_sailor_year",
                        label = "Post Sailor Shift Fame (from 2028)",
                        min = 2028,
                        max = max_year,
                        value = c(2028, max_year),
                        step = 1,
                        sep = ""
                      ),
                      pickerInput(
                        inputId = "genre_chord_from_node_type",
                        label = "Filter by From Node Type",
                        choices = c("Song", "Album"),
                        selected = c("Song", "Album"),
                        multiple = TRUE,
                        options = list(`actions-box` = TRUE, `live-search` = TRUE)
                      ),
                      pickerInput(
                        inputId = "genre_chord_to_node_type",
                        label = "Filter by To Node Type",
                        choices = c("Song", "Album"),
                        selected = c("Song", "Album"),
                        multiple = TRUE,
                        options = list(`actions-box` = TRUE, `live-search` = TRUE)
                      ),
                      pickerInput(
                        inputId = "genre_chord_edge_type",
                        label = "Filter by Edge Type",
                        choices = c("InStyleOf", "CoverOf", "InterpolatesFrom", "LyricalReferenceTo", "DirectlySamples"),
                        selected = c("InStyleOf", "CoverOf", "InterpolatesFrom", "LyricalReferenceTo", "DirectlySamples"),
                        multiple = TRUE,
                        options = list(`actions-box` = TRUE, `live-search` = TRUE)
                      ),
                      pickerInput(
                        inputId = "chord_from_genre",
                        label = "Filter by From Genre",
                        choices = sort(unique(all_nodes$genre)),
                        selected = sort(unique(all_nodes$genre)),
                        multiple = TRUE,
                        options = list(`actions-box` = TRUE, `live-search` = TRUE)
                      ),
                      pickerInput(
                        inputId = "chord_to_genre",
                        label = "Filter by To Genre",
                        choices = sort(unique(all_nodes$genre)),
                        selected = sort(unique(all_nodes$genre)),
                        multiple = TRUE,
                        options = list(`actions-box` = TRUE, `live-search` = TRUE)
                      )
                    ),
                    column(width = 9,
                      div(
                        style = "display: flex; justify-content: center; width: 100%;",
                        chorddiag::chorddiagOutput("interactiveChorddiag", height = "600px")
                      )
              
                    ),
                    
                    column(
                      width = 12,
                      style = "width: 100%; margin-top: 20px;",
                      DT::dataTableOutput("genreChordTable")
                    )  
                    
                  )
                ),
                tabPanel("Top Influenced Artists",
                  br(),
                  selectInput(
                    "top_influenced_genre",
                    "Select Influence Genre:",
                    choices = sort(unique(na.omit(nodes_tbl$genre))),
                    selected = "Oceanus Folk"
                  ),
                  selectInput("top_influenced_n", "Top N Influencers:",
                    choices = c("Top 5" = 5, "Top 10" = 10, "Top 25" = 25, "Top 50" = 50, "Top 75" = 75, "Top 100" = 100),
                    selected = 25
                  ),
                  pickerInput(
                    inputId = "top_influenced_edge_type",
                    label = "Edge Type (Artists Associated with Influencing Genres):",
                    choices = c("PerformerOf", "ComposerOf", "LyricistOf", "ProducerOf"),
                    selected = c("PerformerOf", "ComposerOf", "LyricistOf", "ProducerOf"),
                    multiple = TRUE,
                    options = list(`actions-box` = TRUE, `live-search` = TRUE)
                  ),
                  fluidRow(
                    column(width = 12,
                      plotlyOutput("topInfluencedSankeyPlotly", height = "600px")
                    )
                  ),
                  fluidRow(
                    column(width = 12,
                      DT::dataTableOutput("topInfluencedSankeyTable")
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
                  br(),
                  fluidRow(
                    column(
                      width = 3,
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
                      width = 9,
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
                                 width = 3,
                                 br(),
                                 pickerInput("trend_genre", "Select Genre(s)",
                                             choices = unique(na.omit(nodes_tbl$genre)),
                                             selected = unique(na.omit(nodes_tbl$genre))[1], multiple = TRUE,
                                             options = list(`actions-box` = TRUE)),
                                 sliderInput("trend_year_range", "Year Range",
                                             min = 1983, max = 2040, value = c(2005, 2025), sep = ""),
                                 radioButtons(
                                   inputId = "trend_layers",
                                   label = "Show Layer",
                                   choices = c("Song Count"),
                                   selected = "Song Count",
                                   inline = TRUE
                                 ),
                                 hr(),
                                 downloadButton("download_trend_data", "\U0001F4E5 Export Trend Data")
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
    
    # 计算 degree
    sub_ids <- df$id
    subgraph <- igraph::induced_subgraph(as.igraph(graph), vids = sub_ids)
    degs <- igraph::degree(subgraph, mode = "all")
    df <- df %>%
      mutate(
        degree = unname(degs[as.character(id)]),
        size = ifelse(label == "Sailor Shift", 50, 15 + degree * 2),
        color.background = unname(sapply(group, function(g) node_colors[[g]]$fill)),
        color.border = unname(sapply(group, function(g) node_colors[[g]]$border)),
        color.highlight.background = unname(color.background),
        color.highlight.border = unname(color.border),
        shape = ifelse(label == "Sailor Shift", "star", "dot")
      )
    df
  })
  
  
  
  observe({
    req(filtered_nodes())
    updatePickerInput(session, "node_name",
                      choices = sort(unique(filtered_nodes()$label)))
  })
  
  output$directGraph <- renderVisNetwork({
    print('renderVisNetwork: directGraph')
    nodes <- as.data.frame(filtered_nodes())
    edges <- as.data.frame(filtered_edges())
    nodes[] <- lapply(nodes, unname)
    edges[] <- lapply(edges, unname)
    rownames(nodes) <- NULL
    rownames(edges) <- NULL
    print(str(nodes))
    print(str(edges))
    req(nodes, edges)
    
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
    
    valid_ids <- nodes$id
    
    
    
    edges_all <- edges %>%
      filter(from %in% valid_ids, to %in% valid_ids) %>%
      mutate(
        color = edge_colors[edge_type],
        width = 2,
        arrows = "to",
        label = edge_type
      )
    
    
    visNetwork(nodes, edges_all, width = "100%", height = "700px") %>%
      visNodes(color = list(
        background = nodes$color.background,
        border = nodes$color.border,
        highlight = list(
          background = nodes$color.highlight.background,
          border = nodes$color.highlight.border,
          borderWidth = 6
        )
      )) %>%
      visEdges(arrows = "to", color = list(color = unname(edges_all$color))) %>%
      visOptions(highlightNearest = TRUE) %>%
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
    print('renderPlotly: groupEdgeBarPlot')
    print(str(filtered_edges()))
    print(str(filtered_nodes()))
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
    req(input$impact_depth, input$impact_edge_type, input$impact_node_type)
    # Extract subnetwork based on direction
    if (input$impact_depth == 1) {
      subg <- extract_subnetwork(graph, "Sailor Shift", distance = 1, direction = "in")
    } else if (input$impact_depth == 2) {
      subg <- extract_subnetwork(graph, "Sailor Shift", distance = 2, direction = "in")
    } else {
      subg <- extract_subnetwork(graph, "Sailor Shift", distance = 3, direction = "in")
    }
    # Convert to data frames
    impact_nodes <- igraph::as_data_frame(subg, what = "vertices") %>%
      mutate(id = name, label = name, group = `Node Type`,
             color.background = unname(sapply(`Node Type`, function(g) node_colors[[g]]$fill)),
             color.border = unname(sapply(`Node Type`, function(g) node_colors[[g]]$border)),
             shape = ifelse(label == "Sailor Shift", "star", "dot"),
             size = ifelse(label == "Sailor Shift", 50, 25))
    impact_edges <- igraph::as_data_frame(subg, what = "edges") %>%
      filter(`Edge Type` %in% input$impact_edge_type) %>%
      rename(from = from, to = to, label = `Edge Type`)
    # Filter nodes based on selected types
    impact_nodes <- impact_nodes %>%
      filter(group %in% input$impact_node_type)
    # Get used nodes
    used_nodes <- unique(c(impact_edges$from, impact_edges$to))
    impact_nodes <- impact_nodes %>% filter(id %in% used_nodes)
    # 计算 degree
    sub_ids <- impact_nodes$id
    subgraph <- igraph::induced_subgraph(as.igraph(graph), vids = sub_ids)
    degs <- igraph::degree(subgraph, mode = "all")
    impact_nodes <- impact_nodes %>%
      mutate(
        degree = unname(degs[as.character(id)]),
        size = ifelse(label == "Sailor Shift", 50, 15 + degree * 2),
        color.background = unname(sapply(group, function(g) node_colors[[g]]$fill)),
        color.border = unname(sapply(group, function(g) node_colors[[g]]$border)),
        shape = ifelse(label == "Sailor Shift", "star", "dot")
      )
    list(nodes = impact_nodes, edges = impact_edges)
  })
  
  
  # Impact Network
  output$impactNetwork <- renderVisNetwork({
    print('renderVisNetwork: impactNetwork')
    data <- impact_data()
    nodes <- as.data.frame(data$nodes)
    edges <- as.data.frame(data$edges)
    nodes[] <- lapply(nodes, unname)
    edges[] <- lapply(edges, unname)
    rownames(nodes) <- NULL
    rownames(edges) <- NULL
    print(str(nodes))
    print(str(edges))
    req(nodes, edges)
    
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
      visEdges(arrows = "to", color = list(color = unname(edges_styled$color))) %>%
      visOptions(highlightNearest = TRUE) %>%
      visPhysics(solver = "forceAtlas2Based") %>%
      visLayout(randomSeed = 123)
    
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
    nodes <- all_nodes
    edges <- edges_df
    target_genre <- input$trend_target_genre
    # 只保留 to 节点 genre 为 target_genre 的边
    detail_df <- edges %>%
      left_join(nodes %>% select(id, from_name = name, from_type = `Node Type`, from_release_date = release_date, from_genre = genre), by = c("source" = "id")) %>%
      left_join(nodes %>% select(id, to_name = name, to_type = `Node Type`, to_release_date = release_date, to_genre = genre), by = c("target" = "id")) %>%
      filter(!is.na(to_genre) & tolower(trimws(to_genre)) == tolower(trimws(target_genre))) %>%
      filter(to_type %in% c("Song", "Album"), from_type %in% c("Song", "Album")) %>%
      mutate(From_Year = as.numeric(substr(from_release_date, 1, 4))) %>%
      filter(!is.na(From_Year) & From_Year >= 1900 & From_Year <= 2100)

    # 新增：根据 input$trend_from_type 过滤 from_type
    if (!is.null(input$trend_from_type) && input$trend_from_type != "Both") {
      detail_df <- detail_df %>% filter(from_type == input$trend_from_type)
    }
    # genre 多选筛选
    if (!is.null(input$trend_from_genre)) {
      detail_df <- detail_df %>% filter(from_genre %in% input$trend_from_genre)
    }

    # 统计每年每 genre 有多少 unique from 节点
    df <- detail_df %>%
      group_by(From_Year, from_genre) %>%
      summarise(Count = n_distinct(source), .groups = 'drop') %>%
      rename(Year = From_Year, Genre = from_genre)

    if (nrow(df) == 0) return(plotly_empty())

    # 自动补全 genre_colors
    base_colors <- c(
      "Alternative Rock"        = "#f78e84",
      "Americana"               = "#d56d30",
      "Avant-Garde Folk"        = "#d99000",
      "Blues Rock"              = "#daa520",
      "Darkwave"                = "#827400",
      "Desert Rock"             = "#7b8b19",
      "Doom Metal"              = "#5d9200",
      "Dream Pop"               = "#3cb100",
      "Emo/Pop Punk"            = "#00b15a",
      "Indie Folk"              = "#00b983",
      "Indie Pop"               = "#00c4a1",
      "Indie Rock"              = "#00ccc2",
      "Jazz Surf Rock"          = "#00d3d4",
      "Lo-Fi Electronica"       = "#00b3ed",
      "Oceanus Folk"            = "#2095f2",
      "Post-Apocalyptic Folk"   = "#339dff",
      "Psychedelic Rock"        = "#8f95ff",
      "Southern Gothic Rock"    = "#a08fff",
      "Space Rock"              = "#cc84ff",
      "Speed Metal"             = "#e374e6",
      "Symphonic Metal"         = "#f270d0",
      "Synthpop"                = "#f36ebe",
      "Synthwave"               = "#f57caa",
      "Acoustic Folk"           = "#a3d977",  # 新增
      "Celtic Folk"             = "#80cbc4",  # 新增
      "Sea Shanties"            = "#ba68c8"  # 新增
    )
    genres_in_data <- unique(df$Genre)
    missing_genres <- setdiff(genres_in_data, names(base_colors))
    # 自动为缺失的 genre 分配颜色
    if (length(missing_genres) > 0) {
      extra_colors <- grDevices::rainbow(length(missing_genres), start = 0.1, end = 0.9)
      names(extra_colors) <- missing_genres
      genre_colors <- c(base_colors, extra_colors)
    } else {
      genre_colors <- base_colors
    }

    # 确保 factor levels 顺序
    df$Genre <- factor(df$Genre, levels = names(genre_colors))

    p <- ggplot(df, aes(x = Year, y = Count, color = Genre)) +
      geom_line(size = 1) +
      geom_point(size = 2) +
      scale_color_manual(values = genre_colors, na.value = "#cccccc") +
      labs(
        x = "Year",
        y = "Count",
        title = paste("Number of Unique Influencing Works by", input$trend_target_genre)
      ) +
      theme_minimal()

    ggplotly(p)
  })
  
  

  
  
  # --- Server: Talent Radar & Snapshot Logic ---
  # 1) Ensure g_tbl has 'name' attribute for extract_subnetwork()
 
  # Prepare igraph for extract_subnetwork
 
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
        degree = unname(degs[as.character(id)]),
        color.background = ifelse(node_name == input$snapshot_artist_detail, "#27ae60", unname(sapply(group, function(g) node_colors[[g]]$fill))),
        color.border = ifelse(node_name == input$snapshot_artist_detail, "#27ae60", unname(sapply(group, function(g) node_colors[[g]]$border))),
        shape = ifelse(node_name == input$snapshot_artist_detail, "star", "dot"),
        size = ifelse(node_name == input$snapshot_artist_detail, 50, 25)
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


  })
  
  # --- Trend Dashboard Server ---
  # 1. Reactive 歌曲过滤
  filtered_trend <- reactive({
    req(input$trend_genre, input$trend_year_range)
    
    nodes_tbl %>%
      mutate(index = row_number()) %>%
      filter(
        `Node Type` %in% c("Song", "Album"),
        genre %in% input$trend_genre,
        !is.na(release_date),
        as.numeric(substr(release_date, 1, 4)) >= input$trend_year_range[1],
        as.numeric(substr(release_date, 1, 4)) <= input$trend_year_range[2]
      )
  })
  
  # 2. 热力图输出
  output$trend_heatmap <- renderPlotly({
    songs_df <- filtered_trend()
    if (nrow(songs_df) == 0) return(plotly_empty())
    req(input$trend_layers)
    layer <- input$trend_layers[1]
    songs_df <- songs_df %>% mutate(Year = as.numeric(substr(release_date, 1, 4)))
    person_edge_types <- c("PerformerOf", "ComposerOf", "LyricistOf", "ProducerOf")

    if (layer == "Song Count") {
      df_sum <- songs_df %>%
        group_by(Year, genre) %>%
        summarise(Count = n(), .groups = "drop")
      fill_label <- "Count"
    } else if (layer == "Artist Count") {
      artist_edges <- edges_tbl %>%
        filter(target %in% songs_df$index, `Edge Type` %in% person_edge_types)
      artists_df <- artist_edges %>%
        left_join(nodes_tbl %>% mutate(index = row_number()) %>% select(index, artist_name = name), 
                  by = c("from" = "index")) %>%
        left_join(songs_df %>% select(index, genre, release_date), by = c("to" = "index")) %>%
        mutate(Year = as.numeric(substr(release_date, 1, 4))) %>%
        filter(!is.na(Year), !is.na(artist_name))
      df_sum <- artists_df %>%
        group_by(Year, genre) %>%
        summarise(Count = n_distinct(artist_name), .groups = "drop")
      fill_label <- "Artist Count"
    } else if (layer == "Newcomer Count") {
      artist_edges <- edges_tbl %>%
        filter(target %in% songs_df$index, `Edge Type` %in% person_edge_types)
      artists_df <- artist_edges %>%
        left_join(nodes_tbl %>% mutate(index = row_number()) %>% select(index, artist_name = name), 
                  by = c("from" = "index")) %>%
        left_join(songs_df %>% select(index, genre, release_date), by = c("to" = "index")) %>%
        mutate(Year = as.numeric(substr(release_date, 1, 4))) %>%
        filter(!is.na(Year), !is.na(artist_name))
      first_year <- artists_df %>%
        group_by(artist_name, genre) %>%
        summarise(FirstYear = min(Year, na.rm = TRUE), .groups = "drop")
      df_sum <- first_year %>%
        group_by(Year = FirstYear, genre) %>%
        summarise(Count = n(), .groups = "drop")
      fill_label <- "Newcomer Count"
    } else {
      return(plotly_empty())
    }

    if (nrow(df_sum) == 0) return(plotly_empty())
    breaks <- sort(unique(df_sum$Year))
    breaks <- breaks[seq(1, length(breaks), by = 2)]

    p <- ggplot(df_sum, aes(x = factor(Year), y = genre, fill = Count)) +
      geom_tile(color = "white") +
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
    nodes_vn %>% filter(group %in% input$comm_from_node_type | group %in% input$comm_to_node_type) %>%
      mutate(color.background = unname(sapply(group, function(g) node_colors[[g]]$fill)),
             color.border = unname(sapply(group, function(g) node_colors[[g]]$border)))
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
    print('renderVisNetwork: commNetwork')
    nodes <- as.data.frame(comm_filtered_nodes())
    edges <- as.data.frame(comm_filtered_edges())
    nodes[] <- lapply(nodes, unname)
    edges[] <- lapply(edges, unname)
    rownames(nodes) <- NULL
    rownames(edges) <- NULL
    print(str(nodes))
    print(str(edges))
    req(nodes, edges)
    
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
      visNodes(
        color = list(background = unname(nodes$color.background)),
        shape = unname(nodes$shape)
      ) %>%
      visEdges(
        arrows = "to",
        color = list(color = unname(edges$color))  # 如果 edges 没有 color 这一列，可去掉
      ) %>%
      visOptions(highlightNearest = TRUE) %>%
      visPhysics(solver = "forceAtlas2Based") %>%
      visLayout(randomSeed = 123)
    
  })
  
  output$commEdgeTable <- renderDT({
    edges <- comm_filtered_edges() %>%
      left_join(nodes_vn %>% select(id, from_name = label, from_type = group, from_genre = genre), by = c("from" = "id")) %>%
      left_join(nodes_vn %>% select(id, to_name = label, to_type = group, to_genre = genre, release_date), by = c("to" = "id")) %>%
      select(
        from_name, from_type, 
        to_name, to_type, to_genre,
        label,   # edge type
        
        release_date
      ) %>%
      rename(
        "From" = from_name,
        "From Type" = from_type,
        "To" = to_name,
        "To Type" = to_type,
        "To Genre" = to_genre,
        "Edge Type" = label,
        "Work Release Time" = release_date
      )
    datatable(
      edges,  # 你要展示的数据框
      options = list(
        pageLength = 10,
        scrollX = TRUE,
            
        autoWidth = TRUE,
        columnDefs = list(list(width = '150px', targets = "_all")),
        fixedHeader = TRUE
      ),
      rownames = FALSE,
      
    )
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

  output$influenceTrendTable <- renderDT({
    nodes <- all_nodes
    edges <- edges_df
    target_genre <- input$trend_target_genre
    detail_df <- edges %>%
      left_join(nodes %>% select(id, from_name = name, from_type = `Node Type`), by = c("source" = "id")) %>%
      left_join(nodes %>% select(id, to_name = name, to_type = `Node Type`, to_release_date = release_date, to_genre = genre), by = c("target" = "id")) %>%
      mutate(Year = as.numeric(substr(to_release_date, 1, 4))) %>%
      filter(!is.na(Year) & Year >= 1900 & Year <= 2100) %>%
      filter(!is.na(to_genre) & tolower(trimws(to_genre)) == tolower(trimws(target_genre))) %>%
      filter(to_type %in% c("Song", "Album"), from_type %in% c("Song", "Album"))

    # 联动 From Node Type
    if (!is.null(input$trend_from_type) && input$trend_from_type != "Both") {
      detail_df <- detail_df %>% filter(from_type == input$trend_from_type)
    }

    detail_df <- detail_df %>%
      select(
        Year,
        From_Node = from_name,
        From_Type = from_type,
        To_Node = to_name,
        To_Type = to_type,
        Edge_Type = `Edge Type`,
        To_Release_Date = to_release_date,
        To_Genre = to_genre
      ) %>%
      arrange(Year, To_Node)

    if (nrow(detail_df) == 0) {
      datatable(data.frame(Message = "No data to display"), options = list(dom = 't'))
    } else {
      datatable(detail_df, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
    }
  })


  library(dplyr)
  library(circlize)
  library(jsonlite)
  
  output$genreChordDiagram <- renderPlot({
    circos.clear()
    
    # 你需要提前读入的数据
    # nodes_df <- as.data.frame(fromJSON("data/MC1_graph.json")$nodes)
    # edges_df <- as.data.frame(fromJSON("data/MC1_graph.json")$links)
    # 如果已经有 all_nodes 和 edges_df 则无需重新读入
    
    edge_types <- c("InStyleOf", "CoverOf", "InterpolatesFrom", "LyricalReferenceTo", "DirectlySamples")
    
    # enrich edges with genre info
    chord_edges <- edges_df %>%
      left_join(all_nodes %>% select(id, from_genre = genre, from_node_type = `Node Type`), by = c("source" = "id")) %>%
      left_join(all_nodes %>% select(id, to_genre = genre, to_node_type = `Node Type`), by = c("target" = "id")) %>%
      filter(
        !is.na(from_genre), !is.na(to_genre),
        from_genre == "Oceanus Folk",
        to_genre != "Oceanus Folk",
        `Edge Type` %in% edge_types
      )
    
    chord_df <- chord_edges %>%
      group_by(to_genre 
                ,from_genre ) %>%
      summarise(value = n(), .groups = "drop")
    
    # 如无数据则直接退出
    if (nrow(chord_df) == 0) {
      plot.new()
      text(0.5, 0.5, "No data to display for chord diagram.")
      return()
    }
    
    # 定义颜色（可以替换成你想要的 palette）
    base_colors <- c(
      "Alternative Rock"        = "#f78e84",
      "Americana"               = "#d56d30",
      "Avant-Garde Folk"        = "#d99000",
      "Blues Rock"              = "#daa520",
      "Darkwave"                = "#827400",
      "Desert Rock"             = "#7b8b19",
      "Doom Metal"              = "#5d9200",
      "Dream Pop"               = "#3cb100",
      "Emo/Pop Punk"            = "#00b15a",
      "Indie Folk"              = "#00b983",
      "Indie Pop"               = "#00c4a1",
      "Indie Rock"              = "#00ccc2",
      "Jazz Surf Rock"          = "#00d3d4",
      "Lo-Fi Electronica"       = "#00b3ed",
      "Oceanus Folk"            = "#2095f2",
      "Post-Apocalyptic Folk"   = "#339dff",
      "Psychedelic Rock"        = "#8f95ff",
      "Southern Gothic Rock"    = "#a08fff",
      "Space Rock"              = "#cc84ff",
      "Speed Metal"             = "#e374e6",
      "Symphonic Metal"         = "#f270d0",
      "Synthpop"                = "#f36ebe",
      "Synthwave"               = "#f57caa",
      "Acoustic Folk"           = "#a3d977",  # 新增
      "Celtic Folk"             = "#80cbc4",  # 新增
      "Sea Shanties"            = "#ba68c8"  # 新增
    )
    
    # 添加缺失 genre 的默认颜色
    genre_list <- union(chord_df$to_genre, chord_df$from_genre)
    missing_genres <- setdiff(genre_list, names(base_colors))
    default_colors <- rep("#d3d3d3", length(missing_genres))
    genre_colors <- c(base_colors, setNames(default_colors, missing_genres))
    
    # 画图
    chordDiagram(
      x = chord_df,
      grid.col = genre_colors,
      transparency = 0.2,
      directional = 1,
      direction.type = c("diffHeight"),
      annotationTrack = "grid",
      preAllocateTracks = 1
    )
    
    circos.trackPlotRegion(
      track.index = 1, 
      panel.fun = function(x, y) {
        sector.name <- get.cell.meta.data("sector.index")
        xlim <- get.cell.meta.data("xlim")
        ylim <- get.cell.meta.data("ylim")
        circos.text(
          x = mean(xlim), 
          y = ylim[1] + .1, 
          labels = sector.name,
          facing = "clockwise", 
          niceFacing = TRUE, 
          adj = c(0, 0.5),
          cex = 0.7  # 可以调整字号大小
        )
      }, 
      bg.border = NA
    )
    
    
    
  })
  
  output$genreChordTable <- DT::renderDataTable({
    req(input$genre_chord_edge_type, input$genre_chord_from_node_type, input$genre_chord_to_node_type, input$chord_from_genre, input$chord_to_genre)
    edge_types <- input$genre_chord_edge_type
    from_types <- input$genre_chord_from_node_type
    to_types <- input$genre_chord_to_node_type
    from_genres <- input$chord_from_genre
    to_genres <- input$chord_to_genre
    nodes <- all_nodes
    edges <- edges_df

    chord_edges <- edges %>%
      left_join(nodes %>% select(id, from_genre = genre, from_node_type = `Node Type`), by = c("source" = "id")) %>%
      left_join(nodes %>% select(id, to_genre = genre, to_node_type = `Node Type`), by = c("target" = "id")) %>%
      filter(
        !is.na(from_genre), !is.na(to_genre),
        from_genre %in% from_genres,
        to_genre %in% to_genres,
        `Edge Type` %in% edge_types,
        from_node_type %in% from_types,
        to_node_type %in% to_types
      )

    chord_df <- chord_edges %>%
      group_by(from_genre, from_node_type, to_genre, to_node_type, `Edge Type`) %>%
      summarise(value = n(), .groups = "drop") %>%
      arrange(desc(value))

    if (nrow(chord_df) == 0) {
      return(data.frame(Message = "No data to display"))
    } else {
      DT::datatable(
        chord_df,
        colnames = c("From Genre", "From Node Type", "To Genre", "To Node Type", "Edge Type", "Count"),
        options = list(pageLength = 10, scrollX = TRUE),
        rownames = FALSE
      )
    }
  })

  output$interactiveChorddiag <- chorddiag::renderChorddiag({
    edge_types <- input$genre_chord_edge_type
    from_types <- input$genre_chord_from_node_type
    to_types <- input$genre_chord_to_node_type
    from_genres <- input$chord_from_genre
    to_genres <- input$chord_to_genre
    # 新增：获取时间滑块
    pre_year_range <- input$pre_sailor_year
    post_year_range <- input$post_sailor_year
    # 新增：过滤节点
    nodes <- all_nodes %>%
      mutate(release_year = as.numeric(release_date)) %>%
      filter(
        (release_year >= pre_year_range[1] & release_year <= pre_year_range[2]) |
        (release_year >= post_year_range[1] & release_year <= post_year_range[2])
      )
    edges <- edges_df
    chord_edges <- edges %>%
      left_join(nodes %>% select(id, from_genre = genre, from_node_type = `Node Type`, from_release_year = release_year), by = c("source" = "id")) %>%
      left_join(nodes %>% select(id, to_genre = genre, to_node_type = `Node Type`, to_release_year = release_year), by = c("target" = "id")) %>%
      filter(
        !is.na(from_genre), !is.na(to_genre),
        from_genre %in% from_genres,
        to_genre %in% to_genres,
        `Edge Type` %in% edge_types,
        from_node_type %in% from_types,
        to_node_type %in% to_types,
        # 新增：from/to节点release_year都在所选区间
        (
          (from_release_year >= pre_year_range[1] & from_release_year <= pre_year_range[2]) |
          (from_release_year >= post_year_range[1] & from_release_year <= post_year_range[2])
        ) &
        (
          (to_release_year >= pre_year_range[1] & to_release_year <= pre_year_range[2]) |
          (to_release_year >= post_year_range[1] & to_release_year <= post_year_range[2])
        )
      )

    chord_df <- chord_edges %>%
      group_by(from_genre, to_genre) %>%
      summarise(value = n(), .groups = "drop")

    if (nrow(chord_df) == 0) {
      return(NULL)
    }

    genre_list <- unique(c(chord_df$from_genre, chord_df$to_genre))
    genre_matrix <- matrix(0, nrow = length(genre_list), ncol = length(genre_list),
                           dimnames = list(genre_list, genre_list))
    for (i in seq_len(nrow(chord_df))) {
      genre_matrix[chord_df$from_genre[i], chord_df$to_genre[i]] <- chord_df$value[i]
    }
    genre_matrix <- t(genre_matrix)

    
    base_colors <- c(
      "Alternative Rock"        = "#f78e84",
      "Americana"               = "#d56d30",
      "Avant-Garde Folk"        = "#d99000",
      "Blues Rock"              = "#daa520",
      "Darkwave"                = "#827400",
      "Desert Rock"             = "#7b8b19",
      "Doom Metal"              = "#5d9200",
      "Dream Pop"               = "#3cb100",
      "Emo/Pop Punk"            = "#00b15a",
      "Indie Folk"              = "#00b983",
      "Indie Pop"               = "#00c4a1",
      "Indie Rock"              = "#00ccc2",
      "Jazz Surf Rock"          = "#00d3d4",
      "Lo-Fi Electronica"       = "#00b3ed",
      "Oceanus Folk"            = "#2095f2",
      "Post-Apocalyptic Folk"   = "#339dff",
      "Psychedelic Rock"        = "#8f95ff",
      "Southern Gothic Rock"    = "#a08fff",
      "Space Rock"              = "#cc84ff",
      "Speed Metal"             = "#e374e6",
      "Symphonic Metal"         = "#f270d0",
      "Synthpop"                = "#f36ebe",
      "Synthwave"               = "#f57caa",
      "Acoustic Folk"           = "#a3d977",  # 新增
      "Celtic Folk"             = "#80cbc4",  # 新增
      "Sea Shanties"            = "#ba68c8"  # 新增
    )
    
    
    missing_genres <- setdiff(genre_list, names(base_colors))
    default_colors <- rep("#d3d3d3", length(missing_genres))
    genre_colors <- c(base_colors, setNames(default_colors, missing_genres))
    genre_colors <- unname(genre_colors[genre_list])

    chorddiag::chorddiag(
      genre_matrix,
      groupnamePadding = 20,
      showTicks = FALSE,
      groupColors = genre_colors,
      margin = 130,
      tooltipGroupConnector = " → ",
      groupnameFontsize = 13
    )
  })

  output$topInfluencedSankeyPlotly <- renderPlotly({
 
    selected_genre <- input$top_influenced_genre
    top_k <- as.numeric(input$top_influenced_n)
    target_nodes <- nodes_tbl %>%
      filter(`Node Type` %in% c("Song", "Album"), genre == selected_genre)
    influence_types <- c("InStyleOf", "CoverOf", "DirectlySamples", "InterpolatesFrom", "LyricalReferenceTo")
    influences_to_oceanus <- edges_tbl %>%
      filter(`Edge Type` %in% influence_types, target %in% target_nodes$id)
    source_nodes <- nodes_tbl %>%
      filter(id %in% influences_to_oceanus$source, !is.na(genre))
    mid_genre_df <- influences_to_oceanus %>%
      left_join(source_nodes, by = c("source" = "id")) %>%
      filter(`Node Type` %in% c("Song", "Album"), !is.na(genre), genre != selected_genre ) %>%
      select(source_genre = genre, source_id = source, target_id = target, edge_type_to_ocean = `Edge Type`)
    people_roles <- input$top_influenced_edge_type
    people_edges <- edges_tbl %>%
      filter(`Edge Type` %in% people_roles, target %in% mid_genre_df$source_id) %>%
      rename(edge_type_from_person = `Edge Type`)
    people <- nodes_tbl %>%
      filter(id %in% people_edges$source, `Node Type` %in% c("Person", "MusicalGroup")) %>%
      select(id, person_name = node_name)
    sankey_df <- people_edges %>%
      left_join(mid_genre_df, by = c("target" = "source_id"), relationship = "many-to-many") %>%
      left_join(people, by = c("source" = "id")) %>%
      mutate(target_genre = selected_genre) %>%
      select(person_name, source_genre, target_genre, edge_type_from_person, edge_type_to_ocean)
    top_people <- sankey_df %>%
      count(person_name, sort = TRUE) %>%
      slice_max(n, n = top_k) %>%
      pull(person_name)
    sankey_df <- sankey_df %>%
      filter(person_name %in% top_people)
    node_list <- unique(c(sankey_df$person_name, sankey_df$source_genre, selected_genre))
    sankey_nodes_df <- data.frame(name = node_list, stringsAsFactors = FALSE)
    link1 <- sankey_df %>%
      count(source = person_name, target = source_genre, edge_type = edge_type_from_person) %>%
      mutate(
        source_id = match(source, node_list) - 1,
        target_id = match(target, node_list) - 1,
        hover = paste0(source, " → ", target, "<br>Type: ", edge_type, "<br>Count: ", n)
      ) %>%
      filter(!is.na(source_id), !is.na(target_id))

    link2 <- sankey_df %>%
      count(source = source_genre, target = target_genre, edge_type = edge_type_to_ocean) %>%
      mutate(
        source_id = match(source, node_list) - 1,
        target_id = match(target, node_list) - 1,
        hover = paste0(source, " → ", target, "<br>Type: ", edge_type, "<br>Count: ", n)
      ) %>%
      filter(!is.na(source_id), !is.na(target_id))

    links_df <- bind_rows(link1, link2)
    base_colors <- c(
      "Alternative Rock"        = "#f78e84",
      "Americana"               = "#d56d30",
      "Avant-Garde Folk"        = "#d99000",
      "Blues Rock"              = "#daa520",
      "Darkwave"                = "#827400",
      "Desert Rock"             = "#7b8b19",
      "Doom Metal"              = "#5d9200",
      "Dream Pop"               = "#3cb100",
      "Emo/Pop Punk"            = "#00b15a",
      "Indie Folk"              = "#00b983",
      "Indie Pop"               = "#00c4a1",
      "Indie Rock"              = "#00ccc2",
      "Jazz Surf Rock"          = "#00d3d4",
      "Lo-Fi Electronica"       = "#00b3ed",
      "Oceanus Folk"            = "#2095f2",
      "Post-Apocalyptic Folk"   = "#339dff",
      "Psychedelic Rock"        = "#8f95ff",
      "Southern Gothic Rock"    = "#a08fff",
      "Space Rock"              = "#cc84ff",
      "Speed Metal"             = "#e374e6",
      "Symphonic Metal"         = "#f270d0",
      "Synthpop"                = "#f36ebe",
      "Synthwave"               = "#f57caa",
      "Acoustic Folk"           = "#a3d977",  # 新增
      "Celtic Folk"             = "#80cbc4",  # 新增
      "Sea Shanties"            = "#ba68c8"  # 新增
    )
    # 修复 node_colors 的写法（避免 %||% 出错）
    node_colors <- sapply(sankey_nodes_df$name, function(name) {
      if (name %in% sankey_df$person_name) {
        "#cccccc"
      } else {
        if (!is.null(base_colors[[name]])) base_colors[[name]] else "#cccccc"
      }
    }) %>% alpha(0.7)
    
    # 修复可能造成崩溃的 link_colors 写法（不变）
    link_colors <- links_df$target %>%
      map_chr(~ {
        if (.x %in% names(base_colors)) {
          base_colors[[.x]]
        } else {
          "#bbbbbb"
        }
      }) %>%
      alpha(0.4)
     
    p <- plot_ly(
      type = "sankey",
      domain = list(x = c(0, 1), y = c(0, 1)),
      orientation = "h",
      source = "sankeyGenre",
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
        customdata = links_df$hover,
        hovertemplate = "%{customdata}<extra></extra>"
      )
    ) %>%
      layout(
        font = list(size = 12),
        margin = list(l = 20, r = 20, b = 20, t = 40),
        title = paste("Sankey Flow to", selected_genre, "→ Genres → Artists ")
      )
    p <- event_register(p, 'plotly_click')
    p
  })

  output$topInfluencedSankeyTable <- DT::renderDataTable({
    selected_genre <- input$top_influenced_genre
    top_k <- as.numeric(input$top_influenced_n)
    influence_types <- c("InStyleOf", "CoverOf", "DirectlySamples", "InterpolatesFrom", "LyricalReferenceTo")
    target_nodes <- nodes_tbl %>%
      filter(`Node Type` %in% c("Song", "Album"), genre == selected_genre)
    influences_to_oceanus <- edges_tbl %>%
      filter(`Edge Type` %in% influence_types, target %in% target_nodes$id)
    source_nodes <- nodes_tbl %>%
      filter(id %in% influences_to_oceanus$source, !is.na(genre))
    mid_genre_df <- influences_to_oceanus %>%
      left_join(source_nodes, by = c("source" = "id")) %>%
      filter(`Node Type` %in% c("Song", "Album"), !is.na(genre), genre != selected_genre ) %>%
      select(source_genre = genre, source_id = source, target_id = target, edge_type_to_ocean = `Edge Type`)
    people_roles <- input$top_influenced_edge_type
    people_edges <- edges_tbl %>%
      filter(`Edge Type` %in% people_roles, target %in% mid_genre_df$source_id) %>%
      rename(edge_type_from_person = `Edge Type`)
    people <- nodes_tbl %>%
      filter(id %in% people_edges$source, `Node Type` %in% c("Person", "MusicalGroup")) %>%
      select(id, person_name = node_name)
    # 对 mid_genre_df 按 source_id 去重，只保留一条记录
    mid_genre_df_one <- mid_genre_df %>% distinct(source_id, .keep_all = TRUE)
    sankey_df <- people_edges %>%
      left_join(mid_genre_df_one, by = c("target" = "source_id")) %>%
      left_join(people, by = c("source" = "id")) %>%
      mutate(target_genre = selected_genre) %>%
      select(person_name, source_genre, target_genre, edge_type_from_person, edge_type_to_ocean)
    top_people <- sankey_df %>%
      count(person_name, sort = TRUE) %>%
      slice_max(n, n = top_k) %>%
      pull(person_name)
    sankey_df <- sankey_df %>%
      filter(person_name %in% top_people)
    # 统计每组出现次数
    sankey_table <- sankey_df %>%
      group_by(person_name, source_genre, target_genre, edge_type_from_person, edge_type_to_ocean) %>%
      summarise(n = n(), .groups = "drop")
    DT::datatable(
      sankey_table,
      colnames = c("Artist", "From Genre", "To Genre", "Person-Work Edge Type", "Genre-Work Edge Type", "Person-Work Count"),
      options = list(pageLength = 10, scrollX = TRUE),
      rownames = FALSE
    )
  })

}



# Run the app
shinyApp(ui = ui, server = server)



