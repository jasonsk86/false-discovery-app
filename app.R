
library(shiny)
library(shinydashboard)
library(tidyverse)
library(ggplot2)
library(glue)
library(scales)

ui <- dashboardPage(skin = "black",
                   
        dashboardHeader(title = "False Discovery Simulation", titleWidth = 450),
        
        dashboardSidebar(disable = TRUE), 
        
        dashboardBody(
            
            
           fluidRow(
               
               box(
                   title = "Inputs", solidHeader = TRUE, status = "warning", width = 4, height = 500, 
                   
                   p("Generate a completely random data set and see how even random data can generate statistically significant results"),
                   br(),
                   p("Play around with different numbers of predictors to see how it affects the chance of seeing some random but significant effects."),
 
                   numericInput("p", "Number of Predictor Variables", value = 20),
                   
                   p("You can also choose to make an adjustment to the p-value by applying the Bonferonni correction"),
                   
                   checkboxInput("checkbox", label = "Apply bonferonni correction", value = FALSE),
                   
                   actionButton("simulate", label = "Simulate Random Data and Test", icon = icon("power-off"))
               ),
               
               box(
                   title = "Features and their p Values", solidHeader = TRUE, status = "primary", width = 8, height = 500,
                   
                   plotOutput("sig_plot"))
           ), 
           
           fluidRow(
               
               box(
                   title = "Glimpse of randomly generated data", solidHeader = TRUE, status = "primary",
                   
                   width = 12,
                   
                   tableOutput("data"))
           ), 
    )
)


server <- function(input, output, session) {
    

    values <- reactiveValues(df = NULL, raw = NULL)
    
    observeEvent(input$simulate, {
        
        outcome <- runif(100)
        
        rng_data <- data.frame(replicate(input$p, runif(100)))
        
        # run sequential t-tests using all random features with random outcome
        sig_results <- rng_data %>% 
            map_dbl(~t.test(.x , outcome)$p.value) %>% 
            as.data.frame() %>% 
            rownames_to_column(var = "feature") %>% 
            rename("p_value" = ".") %>% 
            mutate(significant = ifelse(p_value <= 0.05, "Y", "N"))
        
        #temp <- sig_results
        
        values$df <- sig_results
        
        values$raw <- cbind(rng_data, outcome)
        
    })
    
    
    output$data <- renderTable({
        
        head(values$raw)
    })
    
    output$sig_plot <- renderPlot({
        
        
        ggplot(values$df, aes(x = feature, y = p_value, fill = significant)) +
            geom_col() +
            scale_fill_manual(values = c("grey", muted("green"))) +
            scale_y_continuous(breaks = seq(0, 1, 0.05)) +
            geom_hline(yintercept = 0.05, alpha = 0.5, linetype = "dashed") +
            labs(title = glue("p Values of {input$p} Randomly Generated Features with a Randomly Generated Response")) +
            coord_flip() +  
            theme_minimal() +
            theme(
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                plot.title = element_text(face = "bold", size = 20, margin = margin(b = 20)),
                axis.title.x = element_text(margin = margin(t = 15))
            )
        
    })
  
}

shinyApp(ui, server)


