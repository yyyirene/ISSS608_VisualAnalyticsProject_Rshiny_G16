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
library(ggplot2)
library(plotly)
library(jsonlite)
library(dplyr)
library(networkD3)


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
                          min = 1983,
                          max = 2038,
                          value = c(min_year, max_year),
                          step = 1,
                          sep = "",
              ),
                          
                          
                          
                         
              
              actionButton("release_range_btn", "Select All Years"),
              helpText(
                  "Note: Selecting all years might take a moment. Thanks for your patience."),
              
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
              actionButton("network_depth_btn", "Select All network"),
              helpText(
                "Note: Selecting all Network Depths might take a moment. Thanks for your patience.")
            )
          ),
          
          
          column(
            width = 8,  # 占据整行比较好
            tabsetPanel(
              id = "graph_tabs",
              type = "tabs",
              
              tabPanel("Influence Network",
                       visNetworkOutput("directGraph", height = "725px")
              ),
              
              tabPanel("Summary Statistics",
                       fluidRow(
                         column(
                           width = 12,
                           div(
                             style = "margin-top: 30px;",  # 给条形图添加顶部空隙
                             plotlyOutput("groupEdgeBarPlot", height = "600px"),
                             verbatimTextOutput("barInfo")
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
              DTOutput("directTable", width = "100%")
            )
          )
        )
      ),
      # --- Other Tabs ---------------------------------------------------------------------------
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
    # ----------- Genre Diffusion Tracker ---------------
    tabItem(tabName = "genre",
            fluidRow(
              
              column(width = 3,
                     box(title = "Control Panel", status = "info", solidHeader = TRUE, width = 13,
                         selectInput("mainGenre", "Main Genre",
                                     choices = sort(unique(na.omit(all_nodes$genre))),
                                     selected = "Oceanus Folk"
                         ),
                         sliderInput("yearRange", "Year Range:",
                                     min = 1983, max = 2038, value = c(1990, 2025), sep = ""
                         ),
                         selectInput("nodeType", "Node Type:",
                                     choices = c("Song (Track)" = "Song", "Album" = "Album"),
                                     selected = "Song"
                         ),
                         radioButtons("hopDepth", "Influence Path Depth:",
                                      choices = c("1-hop" = 1, "2-hop" = 2), selected = 1
                         )
                     )
              ),
              
              # 中间图表区域 + Detail panel 并排显示
              column(width = 9,
                     fluidRow(
                       column(width = 8,
                              box(title = "Timeline Trend", status = "primary", solidHeader = TRUE, width = 14,
                                  plotlyOutput("trendPlot", height = "200px")
                              ),
                              box(title = "Genre Influence Network", status = "primary", solidHeader = TRUE, width = 14,
                                  visNetworkOutput("genreNetwork", height = "250px")
                              ),
                              box(title = "Genre Influence Backflow", status = "primary", solidHeader = TRUE, width = 14,
                                  sankeyNetworkOutput("genreSankey", height = "250px")
                              )
                       ),
                       column(width = 4,
                              box(title = "Detail Panel", status = "primary", solidHeader = TRUE, width = 12,
                                  uiOutput("detailPanel")
                              )
                       )
                     )
              )
            )
    ),
      
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
      
      df <- nodes_subgraph %>%
        filter(
          id %in% valid_ids,
          group %in% input$node_type,
          is.na(release_year) |
            (release_year >= input$release_range[1] & release_year <= input$release_range[2])
        )
      
      # ➕ Genre 筛选
      if (!is.null(input$genre_filter)) {
        df <- df %>%
          filter(is.na(genre) | genre %in% input$genre_filter)
      }
      
      df
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
    
    
    observeEvent(input$notable_filter, {
      req(filtered_nodes())
      
      # 清除所有当前选中的节点（无论选什么都先清空）
      visNetworkProxy("directGraph") %>%
        visSelectNodes(id = character(0))  
      
      if (input$notable_filter == "TRUE") {
        selected_nodes <- filtered_nodes() %>%
          filter(notable == TRUE)
        
      } else if (input$notable_filter == "FALSE") {
        selected_nodes <- filtered_nodes() %>%
          filter(notable == FALSE)
        
      } else {
        return()  # All，什么也不选
      }
      
      if (nrow(selected_nodes) > 0) {
        visNetworkProxy("directGraph") %>%
          visSelectNodes(id = selected_nodes$id)
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
      
      # 如果包含 release_date，则提取 release_year 并过滤
      if ("release_date" %in% names(edges_df)) {
        edges_df <- edges_df %>%
          mutate(release_year = as.numeric(substr(as.character(release_date), 1, 4))) %>%
          filter(release_year >= input$release_range[1],
                 release_year <= input$release_range[2])
      }
      
      # 如果字段存在，就展示表格
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
      
      # 添加 text 信息用于 hover 或点击显示
      summary_df$label <- paste0(
        "Node Type: ", summary_df$node_type, "<br>",
        "Edge Type: ", summary_df$edge_type, "<br>",
        "Count: ", summary_df$n
      )
      
      p <- ggplot(summary_df, aes(x = node_type, y = n, fill = edge_type, text = label)) +
        geom_bar(stat = "identity") +
        labs(
          title = "Influences on Sailor Shift by Node Category and Relationship",
          x = "Node Type",
          y = "Count",
          fill = "Edge Type"
        ) +
        theme_minimal() +
        theme(axis.text.x = element_text(hjust = 1))
      
      ggplotly(p, tooltip = "text") %>%
        layout(hoverlabel = list(
          font = list(color = "white") # 设置文字为白色
                       # 可选：设置背景为黑色
        ))
      # 转换为交互图
    })
    
    output$barInfo <- renderPrint({
      click_data <- event_data("plotly_click")
      if (is.null(click_data)) {
        "Tips : Click on a bar segment to see details"
      } else {
        paste0("You clicked on:\nNode Type: ", click_data$x, 
               "\nCount: ", click_data$y, 
               "\nEdge Type: ", click_data$curveNumber + 1)  # 仅近似展示
      }
    })
    
    # ------------- Genre Diffusion Tracker Sever Part -----------------
    graph_data <- fromJSON("data/MC1_graph.json")
    nodes_df <- as.data.frame(graph_data$nodes)
    edges_df <- as.data.frame(graph_data$links)
    
    all_nodes <- nodes_df
    
    # Reactive filtering: filter nodes and edges based on user input
    filtered <- reactive({
      
      nodes <- all_nodes 
      edges <- edges_df
      
      # Only keep nodes that are Song or Album and have genre
      nodes <- nodes %>%
        filter(`Node Type` %in% c("Song", "Album"), !is.na(genre))
      
      # Filter by year (assumes release_date is year string)
      yr <- input$yearRange
      if (!is.null(yr)) {
        nodes <- nodes %>%
          filter(!is.na(release_date) & as.numeric(release_date) >= yr[1] & 
                   as.numeric(release_date) <= yr[2])
      }
      
      # Filter by node type
      if (!is.null(input$nodeType) && input$nodeType != "") {
        nodes <- nodes %>% filter(`Node Type` == input$nodeType)
      }
      
      # Join genre info
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
    
    # Timeline trend plot: count genre nodes influenced by Oceanus Folk per year
    output$trendPlot <- renderPlotly({
      data <- filtered()$nodes
      if (nrow(data) == 0) return(NULL)
      # Extract year and count number of nodes per genre per year
      df <- data %>% 
        filter(!is.na(release_date)) %>%
        mutate(Year = as.numeric(release_date)) %>%
        group_by(Year, genre) %>%
        summarize(Count = n(), .groups = 'drop')
      if (nrow(df) == 0) return(NULL)
      # Plot stacked area chart
      p <- ggplot(df, aes(x = Year, y = Count, fill = genre)) +
        geom_area(alpha = 0.6) +
        labs(x = "Year", y = "Affected Count", fill = "Genre") +
        theme_minimal()
      ggplotly(p)
    })
    
    # Genre influence network: centered on Oceanus Folk showing influence connections
    output$genreNetwork <- renderVisNetwork({
      data <- filtered()
      nodes <- data$nodes
      edges <- data$edges
      if (nrow(nodes) == 0 || nrow(edges) == 0) return(NULL)
      
      main_genre <- input$mainGenre
      
      # 用 all_nodes 获取主 genre 节点 ID
      main_ids <- all_nodes %>% filter(genre == main_genre) %>% pull(id)
      if (length(main_ids) == 0) return(NULL)
      
      if (input$hopDepth == 1) {
        edges_sub <- edges %>% filter(source %in% main_ids | target %in% main_ids)
        nodes_sub <- nodes %>% filter(id %in% unique(c(edges_sub$source, edges_sub$target)))
      } else {
        one_hop <- edges %>% filter(source %in% main_ids | target %in% main_ids) %>%
          pull(source, target) %>% unlist() %>% unique()
        edges_sub <- edges %>% filter(source %in% c(main_ids, one_hop) | target %in% c(main_ids, one_hop))
        nodes_sub <- nodes %>% filter(id %in% unique(c(edges_sub$source, edges_sub$target)))
      }
      
      vis_nodes <- data.frame(id = nodes_sub$id, label = nodes_sub$name,
                              value = 10, group = nodes_sub$genre)
      vis_edges <- data.frame(from = edges_sub$source, to = edges_sub$target, arrows = "to")
      
      visNetwork(vis_nodes, vis_edges) %>%
        visOptions(highlightNearest = list(enabled = TRUE, degree = 1),
                   nodesIdSelection = TRUE)
    })
    
    # Sankey diagram: backflow influence from other genres to Oceanus Folk
    output$genreSankey <- renderSankeyNetwork({
      data <- filtered()$edges
      nodes <- filtered()$nodes
      if (nrow(data) == 0 || nrow(nodes) == 0) return(NULL)
      
      main_genre <- input$mainGenre
      
      # Filter for edges where only one side is Oceanus Folk
      sankey_links <- data %>%
        filter(
          !is.na(source_genre) & !is.na(target_genre),
          (target_genre != main_genre & source_genre == main_genre) |
            (source_genre != main_genre & target_genre == main_genre)
        ) %>%
        mutate(
          FromGenre = ifelse(target_genre == main_genre, source_genre, target_genre),
          ToGenre = main_genre
        ) %>%
        group_by(FromGenre, ToGenre) %>%
        summarize(Value = n(), .groups = 'drop')
    
      
      if (nrow(sankey_links) == 0) return(NULL)
      
      # Create nodes and links
      sankey_nodes <- data.frame(name = unique(c(sankey_links$FromGenre, sankey_links$ToGenre)))
      sankey_links <- sankey_links %>%
        mutate(
          source = match(FromGenre, sankey_nodes$name) - 1,
          target = match(ToGenre, sankey_nodes$name) - 1
        )
      
      sankeyNetwork(
        Links = sankey_links,
        Nodes = sankey_nodes,
        Source = "source",
        Target = "target",
        Value = "Value",
        NodeID = "name",
        fontSize = 12,
        nodeWidth = 30
      )
      
    })
    
    # Detail panel: show node details upon selection
    output$detailPanel <- renderUI({
      # Assume visNetwork selection triggers input$genreNetwork_selected
      sel_id <- input$genreNetwork_selected
      if (is.null(sel_id)) {
        return(tags$p("Click a node in the network or Sankey diagram to view details."))
      }
      # Look up selected node information
      node_row <- nodes_df %>% filter(id == sel_id)
      if (nrow(node_row) == 0) return(NULL)
      # Sample field extraction (adjust to your schema)
      name    <- node_row$name
      works   <- ifelse(!is.null(node_row$representative_works), node_row$representative_works, "N/A")
      activeY <- ifelse(!is.null(node_row$release_date), node_row$release_date, "Unknown")
      # Check whether there is collaboration or similarity with Sailor Shift
      sshift_id <- nodes_df$id[nodes_df$name == "Sailor Shift"]
      related <- any(edges_df$Edge.Type %in% c("MemberOf", "InStyleOf", "LyricistOf", "LyricalReferenceTo") &
                       ((edges_df$source == sel_id & edges_df$target == sshift_id) |
                          (edges_df$source == sshift_id & edges_df$target == sel_id)))
      tagList(
        h4(paste0("Name: ", name)),
        p(paste0("Representative Works: ", works)),
        p(paste0("Active Year: ", activeY)),
        p(paste0("Collaboration / Style Similarity with Sailor Shift: ", ifelse(related, "Yes", "No")))
      )
    })
  }
  
# Run the app
shinyApp(ui = ui, server = server)
