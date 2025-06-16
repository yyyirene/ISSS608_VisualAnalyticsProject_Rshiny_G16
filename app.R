#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#
# 加载 shinydashboard
library(shiny)
library(shinydashboard)
library(dplyr)
library(DT)
library(visNetwork)
library(jsonlite)


#——————————————————————————————————influence graph data preparation————————————————————————————————————

kg <- fromJSON("data/MC1_graph.json")

nodes_tbl <- as_tibble(kg$nodes)
edges_tbl <- as_tibble(kg$links) 

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
    edges_df <- edges_df %>% dplyr::filter(`Edge Type` %in% edge_types)
  }
  
  if (!is.null(node_types)) {
    nodes_df <- nodes_df %>% dplyr::filter(`Node Type` %in% node_types)
  }
  
  used_node_ids <- unique(c(edges_df$from, edges_df$to))
  nodes_df <- nodes_df %>% dplyr::filter(name %in% used_node_ids)
  
  tidygraph::tbl_graph(nodes = nodes_df, edges = edges_df, directed = igraph::is_directed(graph))
}

##########
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
  transmute(
    id = index,
    label = name,
    group = `Node Type`
  )

edges_subgraph <- all_edges %>%
  transmute(
    from = from,
    to = to,
    label = `Edge Type`,
    title = paste0("Edge Type: ", `Edge Type`, "<br>Release: ", release_date)
  )

#############################3
ui <- dashboardPage(
  dashboardHeader(
    title = tagList(
      div(
        style = "display: flex; align-items: center;",
        span("EchoScope™", style = "font-weight: bold; font-size: 22px; color: white; margin-right: 20px;"),
        span("Discovering Music Influence Through Visual Analytics",
             style = "font-size: 16px; color: white;")
      )
    ),
    titleWidth = 600
  ),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      
      #-------------------------Influence Graph-------------------------#
      menuItem("Influence Analysis", icon = icon("project-diagram"),
               menuSubItem("Influenced By", tabName = "influenced"),
               menuSubItem("Her Impact & Collaborator", tabName = "impact"),
               menuSubItem("Community Influence", tabName = "community")
      ),
      #------------------------------------------------------------------#
      
      menuItem("Genre Diffusion", tabName = "genre", icon = icon("fire")),
      menuItem("Talent Radar", tabName = "talent", icon = icon("satellite-dish")),
      menuItem("Trend Dashboard", tabName = "trend", icon = icon("chart-bar"))
    )
  ),
  
  dashboardBody(
    tabItems(
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
      ),
      
      # --- Influence Graph Tabs ---#
      # tabItem for "Influenced By"
      # tabItem for "Influenced By"
      tabItem(
        tabName = "influenced",
        h2("Who has Sailor Shift been influenced by?"),
        br(),
        p("To visualize the influence on Sailor Shift, we explored both direct and indirect connections,
          along with how these evolved over time. The visual analysis process followed three key steps:"),
        tags$ol(
          tags$li("Identify the individuals who directly influenced her."),
          tags$li("Examine the works created by Sailor Shift, and trace any indirect influences on these works from others."),
          tags$li("Apply a timeline to analyze how these influences evolved over time and observe any trends in their impact.")
        ),
        br(),
        tabsetPanel(
          tabPanel("Directly influenced",
                   h4("This tab shows direct influences."),
                   fluidRow(
                     column(
                       width = 4,
                       wellPanel(  # 包裹控件使其视觉更集中
                         selectInput("node_type", "Select Node Type",
                                     choices = unique(nodes_subgraph$group),
                                     selected = "Person"),
                         checkboxGroupInput("edge_type", "Edge Types",
                                            choices = unique(edges_subgraph$label),
                                            selected = unique(edges_subgraph$label)),
                         sliderInput("release_range", "Release Year",
                                     min = 1990, max = 2025, value = c(2000, 2025))
                       )
                     ),
                     column(
                       width = 8,
                       visNetworkOutput("directGraph", height = "500px"),
                       br(),
                       DTOutput("directTable")
                     )
                   )
          )
        )
         
        ),
      
      # tabItem for "Her Impact & Collaborator"
      tabItem(
        tabName = "impact",
        h2("Her Impact & Collaborator"),
        h3("Who has she collaborated with and directly or indirectly influenced?")
        # 添加图表或内容输出
      ),
      
      # tabItem for "Community Influence"
      tabItem(
        tabName = "community",
        h2("Community Influence"),
        h3("How has she influenced collaborators of the broader Oceanus Folk community?")
        # 添加图表或内容输出
      ),
      
      
      # --- Other Modules ---
      tabItem(tabName = "genre", h2("Genre Diffusion Module")),
      tabItem(tabName = "talent", h2("Talent Radar Module")),
      tabItem(tabName = "trend", h2("Trend Dashboard Module"))
    )
  )
)

# Server logic
server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    x <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    hist(x, breaks = bins, col = 'darkgray', border = 'white',
         xlab = 'Waiting time to next eruption (in mins)',
         main = 'Histogram of waiting times')
  })
  
  output$directGraph <- renderVisNetwork({
    visNetwork(nodes_subgraph, edges_subgraph, width = "100%", height = "700px") %>%
      visEdges(arrows = "to") %>%
      visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
      visLegend(useGroups = TRUE, position = "right") %>%
      visPhysics(solver = "forceAtlas2Based",
                 forceAtlas2Based = list(gravitationalConstant = -80),
                 stabilization = list(enabled = TRUE, iterations = 100)) %>%
      visLayout(randomSeed = 123)
  })
  
  output$directTable <- renderDT({
    datatable(edges_subgraph, options = list(pageLength = 10), rownames = FALSE)
  })
}


# Run the app
shinyApp(ui = ui, server = server)
