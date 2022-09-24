#Librerias
library(MASS)
library(shiny)
library(ggplot2)
library(gtable)
library(grid)

#-------------------
?sidebarLayout
plot_ui <- fluidPage(
    titlePanel( "Una reducción de dimensión" ),

    sidebarLayout(
      sidebarPanel(
        selectInput(inputId = "dataset",
                                   label = "Ejemplo:",
                                   choices = c("Comercio exterior", "Boston", "Data 3")),
                       sliderInput(
                         "tita",
                         "Ángulo de dirección:",
                         min = 0,
                         max = 360,
                         value = 0,
                         step = 1,
                         ticks = FALSE),
                       checkboxInput("scale", "Estandarizar marginalmente",FALSE)),
      mainPanel(plotOutput("distPlot")),
      position = 'left')
  )
  
plot_server <- function( input, output ) {
    output$distPlot <- renderPlot( {
      
    datasetInput <- reactive({
        switch(input$dataset,
               "Comercio exterior" = read.csv("country_profile_variables.csv")[,c(11,12)],
               "Boston" = MASS::Boston[,c(5,8)],
               "Data 3" = NULL)
      })
      
    col <- reactive({
        switch(input$dataset,
               "Comercio exterior" = 'darkgreen',
               "Boston" = 'firebrick',
               "Data 3" = NULL)
      }
      )
      
    default <- reactive({
        switch(input$scale,
               "Comercio exterior" = FALSE,
               "Boston" = TRUE,
               "Data 3" = TRUE)
      }
      )
      
      project <-function(x,a){
        z <- as.numeric(crossprod(x,a))
        coord <- z*a
        return(c(coord,z))
      }
      
      X <- data.frame(scale(datasetInput(),center=TRUE,scale=input$scale))
      vt <- round(sum(diag(cov(X))),2)
        
      main_plot <- ggplot(X,aes(x=X[,1],y=X[,2]))  +
        geom_hline(yintercept=0, linetype='dotted', color='grey50') +
        geom_vline(xintercept=0, linetype='dotted', color='grey50') + 
        geom_point(size=2,colour='grey40') +
        coord_cartesian(xlim = c(-max(X),max(X)), ylim=c(-max(X),max(X))) +
        labs(title = 'Dos dimensiones') +
        annotate("text", x=0, y=-0.8*max(X),
                 label= paste('Variabilidad total : ',vt), fontface='bold') +
        theme(axis.title.y=element_blank(),axis.title.x=element_blank(),
              panel.background=element_rect(fill='white',colour='grey50')) 
      
      tita <- input$tita*pi/180
      a <- c(cos(tita),sin(tita))
      
      Z <- data.frame(t(apply(X,1,project,a)))
      a <- round(a,2)
      eff <- round(100*var(Z[,3])/sum(diag(cov(X))),1)
      v <- round(var(Z[,3]),2)
      
      main_plot <- main_plot + geom_abline(intercept=0, slope=tan(tita),colour='grey30') +
        geom_point(data = Z, mapping = aes(x=Z[,1],y=Z[,2]), color=col(),
                   size = 2) +
        annotate("text", x=0, y=-0.9*max(X),
                 label= paste('Dirección : [',a[1],' ',a[2],']'), fontface='bold')           
      z_plot <- ggplot(Z,aes(x=Z[,3],y=0)) + geom_point(colour=col()) +
        coord_cartesian(xlim = c(-max(X),max(X)), ylim=c(-0.04,0.01)) +
        labs(title = 'Una dimensión') + 
        annotate("text", x=0, y=-0.02,
                 label= paste('Variabilidad explicada :',v,' (',eff,' %)'),
                 fontface='bold') +
        theme(axis.title.y=element_blank(),axis.title.x=element_blank(),
              axis.text.y=element_blank(),
              panel.background=element_rect(fill='white',colour='grey50'),
              aspect.ratio = 1/5)
      
      g1 <- ggplotGrob(main_plot)
      g2 <- ggplotGrob(z_plot)
      g <- rbind(g1, g2, size = "first")
      g$widths <- unit.pmax(g1$widths, g2$widths)
      grid.newpage()
      grid.draw(g)
      
    }, width = 500, height = 500 )
  }
  
shinyApp( ui = plot_ui, server = plot_server )





