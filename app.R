library(ggplot2)
library(Cairo)   # For nicer ggplot2 output when deployed on Linux
# dependencies
require('Rtsne')
library(dplyr)
library(splitstackshape)
library(cluster)
library(ggplot2)

dfs <- read.csv('TransfromedData_1_sample.csv', header = T)

# calculate distance for variables of all types
gower_dist <- dfs %>% select(-loan_status) %>% daisy(
  metric = "gower")
summary(gower_dist)

tsne_obj <- Rtsne(gower_dist, is_distance = TRUE, perplexity=30)

tsne_data <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("x_coord", "y_coord")) %>%
  cbind(dfs)


ui <- fluidPage(
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
      geom_point(aes(color = addr_state, shape=loan_status), alpha=0.5)
    #+ geom_text(aes(label=purpose), check_overlap = T)
  })
  
  output$brush_info <- renderDataTable({
    brushedPoints(tsne_data, input$plot1_brush)
  })
}

shinyApp(ui, server)