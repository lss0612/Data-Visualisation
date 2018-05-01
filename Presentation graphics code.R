##Data Visualization Project
##code for graphics used in presentation and paper
##Les Stanaland
## 25 April 2018
## R version 3.4.4

##necessary libraries
library(interplot)
library(shiny)
library(ggplot2)
library(maps)
library(ggthemes)


##Marginal effects plot

interplot(m=m10, var1="H1", var2="ResourceCursed")+
  aes(color = "blue") + theme(legend.position="none") +
  geom_hline(yintercept = 0, linetype = "dashed") + xlab("Resource Cursed Countries") +
  ylab("Marginal Effect of Rent-Seeking") + labs(title="Marginal Effect of Resource Cursed Countries \n on Rent Seeking") +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14),
        title=element_text(size=14))

##world map


data <- data.frame(fulldata$country, fulldata$oilrents, fulldata$Lat, fulldata$Long)
world <- ggplot() +
  borders("world", colour = "gray85", fill = "gray80") +
  theme_map() 

map <- world +
  geom_point(aes(x = fulldata.Long, y = fulldata.Lat, size = fulldata.oilrents),
             data = data, 
             colour = 'blue', alpha = .5) +
  scale_size_continuous(range = c(1, 10), 
                        breaks = c(2,7,14, 23)) +
  labs(size = 'Oil Rents')
ggsave("plot.png")


# Shiny visualize relationships with RGDP
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

