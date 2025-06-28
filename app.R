# —————————————————————————————————— Talent Radar Module 3: Unified Implementation ——————————————————————————————————

# Load packages
library(jsonlite)
library(dplyr)
library(tidygraph)
library(tibble)
library(DT)
library(plotly)
library(visNetwork)
library(shiny)
library(shinyWidgets)

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

g_tbl <- tbl_graph(
  nodes = nodes_tbl,
  edges = edges_tbl_graph,
  directed = TRUE
)

prepare_talent_score_from_graph <- function(g_tbl) {
  nodes <- as_tibble(g_tbl, active = "nodes")
  edges <- as_tibble(g_tbl, active = "edges")
  
  notable_work_ids <- nodes %>%
    filter(`Node Type` %in% c("Song", "Album"), notable == TRUE) %>%
    pull(index)
  
  contributing_persons <- edges %>%
    filter(to %in% notable_work_ids,
           `Edge Type` %in% c("PerformerOf", "ComposerOf", "ProducerOf", "LyricistOf")) %>%
    pull(from) %>% unique()
  
  person_df <- nodes %>%
    filter(`Node Type` == "Person") %>%
    select(index, label = node_name, notoriety_date, written_date, genre) %>%
    mutate(
      notoriety_year = as.numeric(substr(notoriety_date, 1, 4)),
      notoriety_recency = pmax(0, 1 - (2025 - notoriety_year) / 20),
      notable_label = ifelse(index %in% contributing_persons, 1, 0)
    )
  
  graph_with_features <- g_tbl %>%
    activate(nodes) %>%
    mutate(
      degree = centrality_degree(),
      pagerank = centrality_pagerank()
    )
  
  graph_features <- as_tibble(graph_with_features, active = "nodes") %>%
    filter(`Node Type` == "Person") %>%
    select(index, degree, pagerank)
  
  features <- person_df %>%
    left_join(graph_features, by = "index") %>%
    mutate(across(c(degree, pagerank, notoriety_recency), ~replace_na(., 0)))
  
  model <- glm(notable_label ~ degree + pagerank + notoriety_recency,
               data = features, family = binomial)
  
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
    mutate(id = index) %>%
    select(id, label, genre, degree, pagerank, notoriety_year, notoriety_recency,
           predicted_prob, notable_label, recommendation)
  
  return(list(model = model, scored = features))
}

talent_model_result <- prepare_talent_score_from_graph(g_tbl)
talent_score_df <- talent_model_result$scored

# —————————————————————————————————— UI and Server Logic ——————————————————————————————————

ui <- dashboardPage(
  dashboardHeader(title = "EchoScope™"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Talent Radar", tabName = "talent", icon = icon("satellite-dish"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "talent",
              fluidPage(
                fluidRow(
                  box(title = "Talent Scoring & Emerging Artist Radar", width = 12, solidHeader = TRUE, status = "primary",
                      tabsetPanel(
                        tabPanel("Score Explorer",
                                 fluidRow(
                                   column(4,
                                          pickerInput("talent_genre", "Filter by Genre",
                                                      choices = unique(na.omit(talent_score_df$genre)),
                                                      selected = head(unique(na.omit(talent_score_df$genre)), 1),
                                                      multiple = TRUE, options = list(`actions-box` = TRUE)),
                                          uiOutput("select_top_artists")
                                   ),
                                   column(8,
                                          tabsetPanel(
                                            tabPanel("Radar Comparison", plotlyOutput("talent_radar_plot", height = "550px")),
                                            tabPanel("Scoreboard", DTOutput("talent_score_table"))
                                          )
                                   )
                                 )
                        ),
                        tabPanel("Artist Snapshots",
                                 fluidRow(
                                   column(4,
                                          uiOutput("select_snapshot_genre"),
                                          uiOutput("select_snapshot_artist")
                                   ),
                                   column(8,
                                          DTOutput("top5_talent_by_genre"),
                                          visNetworkOutput("snapshot_graph", height = "500px")
                                   )
                                 ),
                                 verbatimTextOutput("artist_recommendation_text")
                        )
                      )
                  )
                )
              )
      )
    )
  )
)

server <- function(input, output, session) {
  
  output$select_top_artists <- renderUI({
    req(talent_score_df)
    pickerInput("compare_artists", "Select Artists to Compare",
                choices = talent_score_df$label,
                selected = head(talent_score_df$label, 2),
                multiple = TRUE,
                options = list(`actions-box` = TRUE, `max-options` = 5))
  })
  
  output$select_snapshot_genre <- renderUI({
    selectInput("snapshot_genre", "Choose Genre",
                choices = sort(unique(talent_score_df$genre)),
                selected = head(unique(talent_score_df$genre), 1))
  })
  
  output$select_snapshot_artist <- renderUI({
    req(input$snapshot_genre)
    top5 <- talent_score_df %>%
      filter(genre == input$snapshot_genre) %>%
      arrange(desc(predicted_prob)) %>%
      slice(1:5)
    selectInput("snapshot_artist", "Select Artist to Explore Graph", choices = top5$label)
  })
  
  output$talent_score_table <- renderDT({
    req(talent_score_df, input$talent_genre)
    filtered <- talent_score_df %>% filter(genre %in% input$talent_genre)
    datatable(filtered, options = list(pageLength = 10, scrollX = TRUE))
  })
  
  output$talent_radar_plot <- renderPlotly({
    req(input$compare_artists)
    radar_df <- talent_score_df %>%
      filter(label %in% input$compare_artists) %>%
      select(label, degree, pagerank, notoriety_recency)
    radar_df_scaled <- radar_df
    radar_df_scaled[,-1] <- lapply(radar_df[,-1], scales::rescale)
    p <- plot_ly(type = 'scatterpolar', mode = 'lines+markers')
    for (i in 1:nrow(radar_df_scaled)) {
      p <- p %>% add_trace(r = as.numeric(radar_df_scaled[i, -1]),
                           theta = colnames(radar_df_scaled)[-1],
                           name = radar_df_scaled$label[i])
    }
    p %>% layout(polar = list(radialaxis = list(visible = TRUE, range = c(0,1))))
  })
  
  output$artist_recommendation_text <- renderText({
    req(input$snapshot_genre)
    top_artist <- talent_score_df %>%
      filter(genre == input$snapshot_genre) %>%
      arrange(desc(predicted_prob)) %>%
      slice(1)
    top_artist$recommendation
  })
  
  output$top5_talent_by_genre <- renderDT({
    req(input$snapshot_genre)
    df <- talent_score_df %>%
      filter(genre == input$snapshot_genre) %>%
      arrange(desc(predicted_prob)) %>%
      slice(1:5)
    datatable(df)
  })
  
  output$snapshot_graph <- renderVisNetwork({
    req(input$snapshot_artist)
    artist <- input$snapshot_artist
    graph_df <- edges_tbl %>%
      left_join(nodes_tbl %>% select(index, from_node = node_name), by = c("source" = "index")) %>%
      left_join(nodes_tbl %>% select(index, to_node = node_name), by = c("target" = "index")) %>%
      filter(from_node == artist | to_node == artist)
    nodes <- unique(c(graph_df$from_node, graph_df$to_node))
    nodes_df <- data.frame(id = nodes, label = nodes)
    edges_df <- data.frame(from = graph_df$from_node, to = graph_df$to_node, label = graph_df$`Edge Type`)
    visNetwork(nodes_df, edges_df, width = "100%", height = "500px") %>%
      visOptions(highlightNearest = TRUE) %>%
      visPhysics(solver = "forceAtlas2Based")
  })
}

shinyApp(ui, server)