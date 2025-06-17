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
library(shinyWidgets)
library(lubridate)


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
    edges_df <- edges_df %>% dplyr::filter(`Edge Type` %in% edge_types)
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
    
    # Tooltip 显示内容
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
  # 补充 from 节点信息
  left_join(
    nodes_tbl %>%
      select(index, from_node_type = `Node Type`, from_name = name),
    by = c("from" = "index")
  ) %>%
  
  # 补充 to 节点信息
  left_join(
    nodes_tbl %>%
      select(index, to_node_type = `Node Type`, to_name = name,
             single, genre, release_date, notable, written_date, notoriety_date),
    by = c("to" = "index")
  ) %>%
  
  # 重命名 release_date.y 为统一字段
  mutate(
    release_date = release_date.y
  ) %>%
  
  # 最终保留字段
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



#————————————————————————————————————————————————————————————————————————————————————
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
      
      # --- Influenced Tab ---
      tabItem(
        tabName = "influenced",
        fluidRow(
          column(
            width = 12,
            box(
              width = 12,
              title = NULL,
              solidHeader = FALSE,
              status = "primary",
              collapsible = FALSE,
              div(
                style = "padding: 20px; background-color: #f8f9fa; border-radius: 5px;",
                h2("Who has Sailor Shift been influenced by?"),
                br(),
                p("To visualize the influence on Sailor Shift, we explored both direct and indirect connections,
                along with how these evolved over time. The visual analysis process followed three key steps:"),
                tags$ol(
                  tags$li("Identify the individuals who directly influenced her."),
                  tags$li("Examine the works created by Sailor Shift, and trace any indirect influences on these works from others."),
                  tags$li("Apply a timeline to analyze how these influences evolved over time and observe any trends in their impact.")
                )
              )
            )
          )
        ),
        
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
                options = list(
                  `actions-box` = TRUE,
                  `live-search` = TRUE
                )
              ),
              pickerInput(
                inputId = "node_name",
                label = "Search Node Name",
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
              
              helpText(
                tagList(
                  "Note: Selecting a node will zoom in and highlight it in the network graph.",
                  tags$br(),
                  "Tip: Click on a node to reveal more detailed information."
                )
              ),
              
              pickerInput(
                inputId = "edge_type",
                label = "Select Edge Type",
                choices = sort(unique(edges_subgraph$label)),
                selected = unique(edges_subgraph$label),
                multiple = TRUE,
                options = list(
                  `actions-box` = TRUE,
                  `live-search` = TRUE
                )
              ),
              
              sliderInput("release_range", "Release Year Range",
                          min = 1983,
                          max = 2038,
                          value = c(min_year, max_year),
                          step = 1,
                          sep = ""),
              
              actionButton("release_range_btn", "Select All Years"),
              
              sliderInput(
                inputId = "network_depth",
                label = "Select Network Depth (Layers from Sailor Shift)",
                min = 1,
                max = 3,
                value = 2,
                step = 1,
                ticks = TRUE,
                animate = TRUE
              ),
              actionButton("network_depth_btn", "Select All network")
            )
          ),
          column(
            width = 8,
            visNetworkOutput("directGraph", height = "650px")
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
      ),
      
      # --- Other Tabs ---
      tabItem(
        tabName = "impact",
        h2("Her Impact & Collaborator"),
        h3("Who has she collaborated with and directly or indirectly influenced?")
      ),
      
      tabItem(
        tabName = "community",
        h2("Community Influence"),
        h3("How has she influenced collaborators of the broader Oceanus Folk community?")
      ),
      
      tabItem(tabName = "genre", h2("Genre Diffusion Module")),
      tabItem(tabName = "talent", h2("Talent Radar Module")),
      tabItem(tabName = "trend", h2("Trend Dashboard Module"))
      
    ) # End of tabItems
  )) # End of dashboardBody

#—————————————————————————————————————————————————————————————————————————————————————————— 
server <- function(input, output, session) {
    
    # 更新筛选后的边（确保字段完整）
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
      
      # 精确匹配 from + to + edge_type
      edges_subgraph %>%
        semi_join(selected_edges_raw, by = c("from", "to")) %>%
        filter(edge_type %in% input$edge_type)
    })
    
    # 更新筛选后的节点（release_year & node type）
    filtered_nodes <- reactive({
      req(filtered_edges())
      valid_ids <- unique(c(filtered_edges()$from, filtered_edges()$to))
      
      nodes_subgraph %>%
        filter(
          id %in% valid_ids,
          group %in% input$node_type,
          is.na(release_year) |
            (release_year >= input$release_range[1] & release_year <= input$release_range[2])
        )
    })
    
    # 更新节点名称下拉选项
    observe({
      req(filtered_nodes())
      updatePickerInput(session, "node_name",
                        choices = sort(unique(filtered_nodes()$label)))
    })
    
    # 渲染网络图
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
      
      # 补充颜色与样式
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
    
    # 节点聚焦功能
    observeEvent(input$node_name, {
      req(filtered_nodes())
      node_ids <- filtered_nodes()$id[filtered_nodes()$label %in% input$node_name]
      if (length(node_ids) > 0) {
        visNetworkProxy("directGraph") %>%
          visFocus(id = node_ids[1], scale = 0.7) %>%
          visSelectNodes(id = node_ids)
      }
    })
    
    # 设置年份范围（假设为 1983 - 2038）
    observeEvent(input$release_range_btn, {
      updateSliderInput(session, "release_range", value = c(1983, 2038))
    })
    
    # 设置网络层数范围（假设最大为 3）
    observeEvent(input$network_depth_btn, {
      updateSliderInput(session, "network_depth", value = 3)
    })
    
    
    # 表格输出
    output$directTable <- renderDT({
      edges_df <- filtered_edges()
      
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
  }
  
# Run the app
shinyApp(ui = ui, server = server)
