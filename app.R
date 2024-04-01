if(!require(shiny)){install.packages('shiny', dependencies = TRUE)}
if(!require(DT)){install.packages('DT', dependencies = TRUE)}
if(!require(ggplot2)){install.packages('ggplot2', dependencies = TRUE)}
if(!require(dplyr)){install.packages('dplyr', dependencies = TRUE)}
library(shiny)
library(DT)
library(ggplot2)
library(dplyr)


data <- read.csv("Auschwitz_Death_Certificates.csv", stringsAsFactors = FALSE)

data$Birthplace_First_Letter <- substr(data$Birthplace, 1, 1)
data$Residence_First_Letter <- substr(data$Residence, 1, 1)


ui <- fluidPage(
  titlePanel("Auschwitz Death Certificates Analysis"),
  sidebarLayout(
    sidebarPanel(
      selectInput("dataSelection",
                  "Select Data for Graph:",
                  choices = c("Birthplace", "Residence", "Religion")),
      uiOutput("letterOrReligionInput")
    ),
    mainPanel(
      plotOutput("murderPlot"),
      DTOutput("murderTable")
    )
  )
)


server <- function(input, output, session) {
  
  
  output$letterOrReligionInput <- renderUI({
    if (input$dataSelection %in% c("Birthplace", "Residence")) {
      checkboxGroupInput("selectedLetters",
                         "Select Letters for Graph:",
                         choices = sort(unique(c(data$Birthplace_First_Letter, data$Residence_First_Letter))),
                         selected = sort(unique(c(data$Birthplace_First_Letter, data$Residence_First_Letter)))[1])
    } else {
      checkboxGroupInput("selectedReligions",
                         "Select Religions:",
                         choices = unique(data$Religion),
                         selected = unique(data$Religion)[1])
    }
  })
  
  
  output$murderPlot <- renderPlot({
    if (input$dataSelection %in% c("Birthplace", "Residence")) {
      selected <- if (input$dataSelection == "Birthplace") input$selectedLetters else input$selectedLetters
      data_to_plot <- data %>%
        filter((input$dataSelection == "Birthplace" & Birthplace_First_Letter %in% selected) |
                 (input$dataSelection == "Residence" & Residence_First_Letter %in% selected)) %>%
        group_by(fct = if(input$dataSelection == "Birthplace") Birthplace_First_Letter else Residence_First_Letter) %>%
        summarise(Count = n())
      
      ggplot(data_to_plot, aes(x = fct, y = Count, fill = fct)) +
        geom_bar(stat = "identity") +
        labs(x = "First Letter", y = "Count", title = paste("Number of People Murdered by First Letter of", input$dataSelection)) +
        theme_minimal()
      
    } else {
      selected <- input$selectedReligions
      data_to_plot <- data %>%
        filter(Religion %in% selected) %>%
        group_by(Religion) %>%
        summarise(Count = n())
      
      ggplot(data_to_plot, aes(x = Religion, y = Count, fill = Religion)) +
        geom_bar(stat = "identity") +
        labs(x = "Religion", y = "Count", title = "Number of People Murdered by Religion") +
        theme_minimal()
    }
  })
  
  
  output$murderTable <- renderDT({
    if (input$dataSelection %in% c("Birthplace", "Residence")) {
      selected <- if (input$dataSelection == "Birthplace") input$selectedLetters else input$selectedLetters
      data_to_display <- data %>%
        filter((input$dataSelection == "Birthplace" & grepl(paste0("^", selected, collapse = "|"), Birthplace, ignore.case = TRUE)) |
                 (input$dataSelection == "Residence" & grepl(paste0("^", selected, collapse = "|"), Residence, ignore.case = TRUE)))
    } else {
      selected <- input$selectedReligions
      data_to_display <- data %>%
        filter(Religion %in% selected)
    }
    datatable(data_to_display, options = list(pageLength = 10, scrollX = TRUE))
  })
}


shinyApp(ui = ui, server = server)
