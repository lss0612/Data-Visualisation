# Shiny visualize relationships with RGDP

library(shiny)


# Define UI for real gdp app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Real GDP per capita"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Selector for variable to plot against mpg ----
      selectInput("variable", "Variable:",
                  c("Polity IV" = "PolityIV",
                    "Resource Cursed" = "ResourceCursed",
                    "Region" = "Region",
                    "Oil Revenue / Total Tax Revenue" = "H1",
                    "Oil Revenue / Investment" = "H2",
                    "Oil Tax Revenue / Nonresource Tax Revenue" = "H3")),
      
      # Input: Checkbox for whether outliers should be included ----
      checkboxInput("outliers", "Show outliers", FALSE)
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Formatted text for caption ----
      h3(textOutput("caption")),
      
      # Output: Plot of the requested variable against mpg ----
      plotOutput("RGDPPlot")
      
    )
  )
)

# Define server logic to plot various variables against hp ----
server <- function(input, output) {
  
  # Compute the formula text ----
  # This is in a reactive expression since it is shared by the
  # output$caption and output$hpPlot functions
  formulaText <- reactive({
    paste("Real.GDP.capita ~", input$variable)
  })
  
  # Return the formula text for printing as a caption ----
  output$caption <- renderText({
    formulaText()
  })
  
  # Generate a plot of the requested variable against gdp ----
  # and only exclude outliers if requested
  output$RGDPPlot <- renderPlot({
    boxplot(as.formula(formulaText()),
            data = fulldata,
            outline = input$outliers,
            col = "#75AADB", pch = 19)
  })
  
}

# Create Shiny app ----
shinyApp(ui, server)
