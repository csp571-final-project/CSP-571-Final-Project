library(ggplot2)
library(Cairo)   # For nicer ggplot2 output when deployed on Linux
# dependencies
require('Rtsne')
library(dplyr)
library(splitstackshape)
library(cluster)
library(ggplot2)
library(plotly)

set.seed(1)
dfs <- read.csv('TransfromedData_1_sample.csv', header = T)

# calculate distance for variables of all types
gower_dist <- dfs %>% select(-c(loan_status, addr_state)) %>% daisy(
  metric = "gower")
summary(gower_dist)

tsne_obj <- Rtsne(gower_dist, is_distance = TRUE, perplexity=50)

tsne_data <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("x_coord", "y_coord")) %>%
  cbind(dfs)

tsne_obj_3d <- Rtsne(gower_dist, is_distance = TRUE, perplexity=50, dims=3)

tsne_data_3d <- tsne_obj_3d$Y %>%
  data.frame() %>%
  setNames(c("x_coord", "y_coord", "z_coord")) %>%
  cbind(dfs)


ui <- fluidPage(
  fluidRow(
    column(width = 12,
           plotlyOutput("plot2", height = 600
                      # Equivalent to: click = clickOpts(id = "plot_click")
                      #click = "plot1_click",
                      #brush = brushOpts(
                      #  id = "plot1_brush"
                      #)
           )
    )
  ),
  fluidRow(
    column(width = 12,
           plotOutput("plot1", height = 600,
                      # Equivalent to: click = clickOpts(id = "plot_click")
                      click = "plot1_click",
                      brush = brushOpts(
                        id = "plot1_brush"
                      )
           )
    )
  ),
  fluidRow(
    column(width = 12,
           h4("Brushed points"),
           dataTableOutput("brush_info")
    )
  )
)

server <- function(input, output) {
  output$plot1 <- renderPlot({
    ggplot(aes(x = x_coord, y = y_coord), data = tsne_data) + 
      geom_point(aes(color = loan_status, shape=purpose), alpha=0.5)
  })
  
  output$plot2 <- renderPlotly({
    plot_ly(tsne_data_3d, x = ~x_coord, y = ~y_coord, z = ~z_coord, 
            color = ~loan_status, 
            shape = ~purpose,
            mode='markers',
            hoverinfo = 'text',
            text = ~paste('Purpose', dfs$purpose) # TODO display all relevant data
    )
  })
  
  output$brush_info <- renderDataTable({
    brushedPoints(tsne_data, input$plot1_brush)
  })
}

shinyApp(ui, server)