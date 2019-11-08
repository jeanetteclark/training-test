library(shiny)
library(ggplot2)

# load data from ADC
data_url <- "https://arcticdata.io/metacat/d1/mn/v2/object/urn%3Auuid%3A35ad7624-b159-4e29-a700-0c0770419941"
bg_chem <- read.csv(data_url, stringsAsFactors = FALSE)
var_names <- names(bg_chem)

var_names_input <- var_names[-which(var_names %in% c("Time", "Date", "Station"))]

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Biochemistry Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("x_var",
                        "X axis",
                        choices = var_names_input),
            selectInput("y_var",
                        "Y axis",
                        choices = var_names_input),
            selectInput("color_var",
                        "Color",
                        choices = var_names_input)),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        
       ggplot(bg_chem, mapping = aes_string(x = input$x_var,
                                            y = input$y_var,
                                            color = input$color_var)) +
            geom_point(size = 5) +
            theme_bw() +
            scale_color_gradient2(low="blue", mid="white", high="red", midpoint = mean(bg_chem[,input$color_var])) +
            xlab(gsub("CTD_", " ", input$x_var)) +
            ylab(gsub("CTD_", " ", input$y_var)) +
            labs(color = gsub("CTD_", " ", input$color_var))
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
