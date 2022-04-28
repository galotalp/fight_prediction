#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
    fluidRow(style = 'padding:30px',
        titlePanel("Markowitz Portfolio Calculator for Sports Betting"),
        textInput("total", "Total $", 1000),
        numericInput('matches', 'Number of Matches', 8, min = 2, max = 50),
        # Application title
        column(12,style = 'padding:30px',
               fluidRow(
                       column(6,
                          fluidRow(
                              column(4, 
                                     uiOutput("boxes1")),
                              column(4,
                                     uiOutput("boxes2")),
                              column(4,
                                     uiOutput("boxes3"))
                          )
                   ),
                   column(width = 6),
                   tableOutput("table")
                   
               )
              
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    

    output$boxes1 <- renderUI({
        n <- as.integer(input$matches)
        lapply(1:n, function(i) {
            textInput(paste0("caption",i), paste("Match",i), paste("match identifier",i))
        })
    })
    output$boxes2 <- renderUI({
        n <- as.integer(input$matches)
        lapply(1:n, function(i) {
            textInput(paste0("odds",i), paste("Odds",i), 1.1)
        })
    })
    output$boxes3 <- renderUI({
        n <- as.integer(input$matches)
        
        odds_list <- c(lapply(1:n, function(i) {
            input[[paste0("odds", i)]]
        }))
        
        odds_list <- as.numeric(as.character(odds_list))
      
        win_probabilities1 <- 1.0/odds_list
        
        lapply(1:n, function(i) {
            sliderInput(paste0("perc",i), "",
                        min = round(-99*win_probabilities1[i]), max = round(99 - 100*win_probabilities1[i]), value = 0, ticks = FALSE
            )
        })
    })
    
    output$value <- renderText({ input$caption })
    
    
    
    output$table <- renderTable({
        n <- input$matches
        total <- input$total
        total <- as.numeric(as.character(total))
        match_names <- c(lapply(1:n, function(i) {
            input[[paste0("caption", i)]]
        }))
        
        odds_list <- c(lapply(1:n, function(i) {
            input[[paste0("odds", i)]]
        }))
        
        perc_list <- c(lapply(1:n, function(i) {
            input[[paste0("perc", i)]]
        }))
        
        output_data <- data.frame(matrix(data = NA,nrow =n, ncol = 5))
        colnames(output_data) <- c("Match Names","Implied Probability","Adjusted Probability", "Proportion of Bet","Amount")
        output_data$`Match Names` <- match_names
        
        odds_list <- as.numeric(as.character(odds_list))
        perc_list <- as.numeric(as.character(perc_list))
        
        win_probabilities1 <- 1.0/odds_list
        
        output_data$`Implied Probability` <- win_probabilities1
        
        win_probabilities2 <- win_probabilities1 + perc_list/100
        win_var <- win_probabilities2*(1-win_probabilities2)
        
        output_data$`Adjusted Probability` <- win_probabilities2
        
        
        covariance_matrix <- diag(n)*win_var
        covariance_matrix_inverse <- solve(covariance_matrix)
        expected_returns <- win_probabilities2*odds_list
        
    
        ## calculate weights and normalize
        weights <- covariance_matrix_inverse%*%expected_returns
        weights <- weights/(sum(weights))
        

        output_data$`Proportion of Bet` <- weights
        output_data$`Amount` <- weights*total
        output_data
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
