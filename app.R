#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

admissions <- as.data.frame(UCBAdmissions)

# Define UI for UCBAdmissions app ----
ui <- pageWithSidebar(
  
  # App title ----
  headerPanel("UCB Admissions Explorer"),
  
  # Sidebar panel for inputs ----
  sidebarPanel(
    selectInput("department",
                "Choose a Department:",
                choices = c("A", "B", "C", "D", "E", "F"))
  ),
  
  # Main panel for displaying outputs ----
  mainPanel(
    h3(textOutput("title")),
    plotOutput("admissionsPlot")
  )
)

# Define server logic ----
server <- function(input, output) {
  
  output$title <- renderText({
    paste("Admission Results for Department", input$department)
  })
  
  output$admissionsPlot <- renderPlot({
    
    dept_data <- subset(admissions, Dept == input$department)
    
    counts <- xtabs(Freq ~ Gender + Admit, data = dept_data)
    
    barplot(counts,
            beside = TRUE,
            col = c("darkseagreen3", "steelblue"),
            legend.text = TRUE,
            args.legend = list(x = "topright", title = "Gender"),
            xlab = "Admission Status",
            ylab = "Number of Applicants")
  })
}

# Run the application
shinyApp(ui, server)
