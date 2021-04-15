#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

##### Load libraries #####
library(shiny)
library(tidyverse)
library(stringr)
library(shinythemes)


##### Read and clean up data ######
diabetes <- read_csv("/cloud/project/diabetes_app/diabetes.csv")
diabetes_clean <- diabetes %>%
    mutate(height = round(height * 2.54, 1), weight = round(weight / 2.205, 1)) %>%
    mutate(BMI = weight/((height/100)^2)) %>%
    mutate(BMI_class = case_when(BMI<18.5 ~ 'Underweight', 
                                 18.5<=BMI & BMI<25 ~ 'Normal', 
                                 25<=BMI & BMI<30 ~ 'Overweight', 
                                 30<=BMI & BMI<35 ~ 'Severe Obesity', 
                                 35<=BMI & BMI<40 ~ 'Morbid Obesity', 
                                 BMI>=40 ~ 'Super Obesity')) %>%
    mutate(BMI_class = factor(BMI_class,
                              levels =  c('Underweight', 'Normal', 'Overweight', 'Severe Obesity', 'Morbid Obesity', 'Super Obesity')))





# User Interface
ui <- fluidPage(theme=shinytheme("cerulean"),

    # Application title
    titlePanel("Diabetes Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            # Input for the location - drop-down
            selectInput("location", 
                        label = h3("Filter by location"),
                        choices = c("Buckingham", 
                                    "Louisa"),
                        selected = "Buckingham"),
            
            # Input for the age - slider
            sliderInput("age",
                        label = h3("Choose the age of the subjects"),
                        min = 19, max = 92, value = c(19, 92)),
            
            # Variables to plot
            selectInput("variablex", 
                        label = h3("Select variable for X-axis"), 
                        choices = list("height" = "height", "weight" = "weight", "waist" = "waist", "hip" = "hip", "BMI" = "BMI"), 
                        selected = "height"),
            selectInput("variabley", 
                        label = h3("Select variable for Y-axis"), 
                        choices = list("height" = "height", "weight" = "weight", "waist" = "waist", "hip" = "hip", "BMI" = "BMI"), 
                        selected = "weight")
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
            h2("Chosen settings:"),
            textOutput("ageRange"),
            textOutput("countNumber"),
            
            h2("Plot of the selected variables:"),
            plotOutput("plot"),
            
            h2("Filtered table:"),
            tableOutput("testTable")
        )
    )
)



# Server
server <- function(input, output) {
    
    # This renders some text specifying what settings you have chosen.
    output$ageRange <- renderText({ 
        paste("You have chosen an age range that goes from",
              input$age[1], "to", input$age[2], "and are only visualizing", input$gender, "subjects.")
    })

    output$countNumber <- renderText({
        patients <- diabetes_clean %>%
            filter(location == input$location) %>%
            filter(age > input$age[1],
                   age < input$age[2]) %>%
            count() %>%
            pull()
        paste("Total number of patients selected:", patients)
    })
    
    
    output$testTable <- renderTable({
        diabetes_clean %>%
            filter(location == input$location) %>%
            filter(age > input$age[1],
                   age < input$age[2])
    })
    
    # This renders a plot of the chosen variables
    output$plot <- renderPlot({
        data <- diabetes_clean %>%
            filter(location == input$location) %>%
            filter(age > input$age[1],
                   age < input$age[2]) %>%
            select(input$variablex, input$variabley)
        
        ggplot(data, aes(data[[1]], data[[2]])) +
            geom_point() + 
            labs(x=input$variablex, y=input$variabley)
    })
    
}



# Run the application 
shinyApp(ui = ui, server = server)