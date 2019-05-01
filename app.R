library(ggplot2)
library(Cairo)   # For nicer ggplot2 output when deployed on Linux
# dependencies
require('Rtsne')
library(dplyr)
library(splitstackshape)
library(cluster)
library(ggplot2)
library(plotly)


tsne_data <- read.csv('space_2d.csv', header = T)
space <- read.csv('space_3d.csv', header = T)


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
    p <- plot_ly() %>% add_trace(x = space$x_coord, y = space$y_coord, z = space$z_coord, 
                                 mode='markers',
                                 hoverinfo = 'text',
                                 marker=list(
                                   color = space$cluster_default_rate, 
                                   colorscale='RdYlBu',
                                   colorbar=list(title='Default rate')
                                 ),
                                 text = ~paste('</br>Cluster', space$cluster, 
                                               '</br>Cluster default rate:', space$cluster_default_rate, 
                                               '</br>Purpose', space$purpose), # TODO display all relevant data,
                                 type = "scatter3d"
    ) 
    
    new_data_point <- space[space$test == T, ][5,]
    
    p %>% layout(scene = list(
      aspectratio = list(
        x = 1,
        y = 1,
        z = 1
      ),
      camera = list(
        center = list(
          x = 0,
          y = 0,
          z = 0
        ),
        eye = list(
          x = 1.96903462608,
          y = -1.09022831971,
          z = 0.405345349304
        ),
        up = list(
          x = 0,
          y = 0,
          z = 1
        )
      ), annotations = list(list(
        showarrow = T,
        # datapoint coordinates:
        x = new_data_point$x_coord,
        y = new_data_point$y_coord,
        z = new_data_point$z_coord,
        text = "New loan application",
        xshift = 0,
        opacity = 1
      ))))
  })
  
  output$brush_info <- renderDataTable({
    brushedPoints(tsne_data, input$plot1_brush)
  })
}

shinyApp(ui, server)