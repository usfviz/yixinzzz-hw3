rm(list = ls())
cat("\014") 

if (!("plotly" %in% rownames(installed.packages()))) {
  install.packages("plotly")
}
if (!("GGally" %in% rownames(installed.packages()))) {
  install.packages("GGally")
}

library("shiny")
library("ggplot2")
library("GGally")
library("plotly")

data <- read.csv("dataset_Facebook.csv", sep = ";")

ui <- fluidPage(
  mainPanel(
    tabsetPanel(
      tabPanel(title = "Bubble Plot", plotlyOutput("plot1", width= 900, height=600)),
      tabPanel(title = "Scatterplot Matrix", plotlyOutput("plot2", width= 800, height=500)),
      tabPanel(title = "Parallel Coordinates Plot", plotlyOutput("plot3", width= 800, height=500))
    )
  )
)

server <- function(input, output)  {
  output$plot1 <- renderPlotly({
    df <- data[c("Lifetime.Post.Total.Reach", "Type", "share", 
                 "like")]
    df <- df[order(-df[, "Lifetime.Post.Total.Reach"]),]
    p <- ggplot(df, 
                aes(like, share, 
                        size = Lifetime.Post.Total.Reach, fill = Type,
                        text = paste('Type: ', Type, '</br> Number of Likes: ', like, 
                                     '</br> Number of Shares: ', 
                                     share, '</br> Total Reach: ', Lifetime.Post.Total.Reach)))
    p <- p + geom_point(shape=21, stroke = 0.2, alpha = 0.9) + theme_bw()
    p <- p + scale_y_continuous("Number of Shares", limits = c(0, 100))
    p <- p + scale_x_continuous("Number of Likes", limits = c(0, 600))
    p <- p + scale_size(guide="none") + guides(fill=guide_legend(title="Type(Click)"))
    ggplotly(p, tooltip = 'text')
  })
  
  output$plot2 <- renderPlotly({
    df <- data[c("Lifetime.Post.Consumers", "Lifetime.Post.Total.Reach",
                 "Lifetime.Engaged.Users", "Lifetime.Post.Consumptions",
                 "Type")]
    p <- ggpairs(df, 1:4, mapping = ggplot2::aes(color = Type, alpha = 0.6))
    p <- p + theme_bw() + theme(legend.position = "none")
    ggplotly(p)
  })
  
  output$plot3 <- renderPlotly({
    df <- data[c("Post.Month", "Post.Weekday",
                 "Post.Hour", 
                 "Type")]
    p <- ggparcoord(df, 1:3, groupColumn= "Type", showPoints = T) + theme_bw()
    p <- p + guides(fill=guide_legend(title="Type(Click)")) 
    p <- p + scale_x_discrete("Post Time")  + scale_y_discrete("Posts")
    ggplotly(p)
  })
}

shinyApp(ui = ui, server = server)

