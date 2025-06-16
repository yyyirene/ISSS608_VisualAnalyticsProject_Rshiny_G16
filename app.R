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

# Define UI using dashboardPage
ui <- dashboardPage(
  dashboardHeader(
    title = tagList(
      div(
        style = "display: flex; align-items: center;",
        # EchoScope title
        span("EchoScope™", style = "font-weight: bold; font-size: 22px; color: white; margin-right: 20px;"),
        # Discovering subtitle
        span("Discovering Music Influence Through Visual Analytics",
             style = "font-size: 16px; color: white;")
      )
    ),
    titleWidth = 600
  ),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Influence Graph", tabName = "influence", icon = icon("project-diagram")),
      menuItem("Genre Diffusion", tabName = "genre", icon = icon("fire")),
      menuItem("Talent Radar", tabName = "talent", icon = icon("satellite-dish")),
      menuItem(" Trend Dashboard", tabName = "trend", icon = icon("chart-bar"))
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
      
      # --- Other Tabs ---
      tabItem(tabName = "influence", h2("Influence Graph Module")),
      tabItem(tabName = "genre", h2("Genre Diffusion Module")),
      tabItem(tabName = "talent", h2("Talent Radar Module")),
      tabItem(tabName = "trend", h2("Trend Dashboard Module"))
    )
  ),
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
}


# Run the app
shinyApp(ui = ui, server = server)
