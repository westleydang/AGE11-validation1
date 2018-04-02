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
   
   # Application title
   titlePanel("AGE 11 - preliminary look at subROI per group/age"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         selectInput("ch",
                     "What channel:",
                     factor(channels.all)),
        selectInput("roi",
                    "What roi:",
                    factor(interesting.ROI))
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  library(ggplot2)
  library(reshape2)
  library(dplyr)
  library(plotrix)
  
  
  # Load the data
  rawkk = read.csv(file="All AGE 10 series counts-03232018/Wes_SUBROI_CLN_STACKED.csv", header=TRUE, sep=",")
  rawkk$idBysubroi = as.factor(rawkk$idBysubroi)
  colnames(rawkk)[2] = "GROUP"
  
  # Merge animal details
  animal_details = read.csv("animal_details.csv")
  animal_details$Mouse.ID. = as.factor(animal_details$ID)
  colnames(animal_details)[9] = "idBysubroi"
  rawkk = merge(animal_details, rawkk, by = "idBysubroi")
  
  attach(rawkk)
  rawkk$OLmm2.norm = rawkk$OLmm2
  rawkk$OLmm2.norm = rawkk$OLmm2/(rawkk$Ch2mm2 + rawkk$Ch4mm2 - rawkk$OLmm2)
  
  # What is the mean count per brain, normalized to area?
  channels.all = names(rawkk[13:18])
  
  
  
  
   
   output$distPlot <- renderPlot({

      # draw
      
      df = rawkk[rawkk$SUBROI==input$roi,]
      ggplot(df) +
        aes(x=GROUP, y=df[, input$ch], fill=EXPT, shape=EXPT, color=EXPT) +
        stat_summary(fun.y = mean, 
                     geom="point") +
        stat_summary(fun.data=mean_se, 
                     geom="pointrange") +
        facet_wrap(~EXPT) + 
        labs(y = as.character(input$ch))
      
      
      
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

