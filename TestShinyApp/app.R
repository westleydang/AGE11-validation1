#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)  
library(ggplot2)
library(reshape2)
library(dplyr)
library(plotrix)
# Load the data
kk = read.csv(file="All AGE 10 series counts-03232018/Wes_SUBROI_CLN_STACKED.csv", header=TRUE, sep=",")
kk$idBysubroi = as.factor(kk$idBysubroi)


# Merge animal details
animal_details = read.csv("animal_details.csv")
animal_details$Mouse.ID. = as.factor(animal_details$ID)
colnames(animal_details)[9] = "idBysubroi"
kk = merge(animal_details, kk, by = "idBysubroi")
colnames(kk)[2] = "GROUP"


# Merge group details
group_details = read.csv("group_details.csv")
names(group_details)[1] = "GROUP"
kk = merge(group_details, kk, by="GROUP")

replace = read.csv("replacements.csv")
names(replace) = c("pre", "post")

# Rename the age groups
kk$EXPT = as.factor(as.character(replace$post[match(kk$EXPT, replace$pre)]))

# normalize the OL counts
kk$OLmm2.norm = kk$OLmm2
kk$OLmm2.norm = kk$OLmm2/(kk$Ch2mm2 + kk$Ch4mm2 - kk$OLmm2)

# Rename the channels
names(kk)[15:20] = as.character(replace[1:6,2])


attach(kk)

# Transform rawkk to a long format
kk.long = melt(kk, id.vars=names(kk[1:14]), variable.name = "CHANNEL", value.name="DENSITY")





############################################

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("AGE 11 - preliminary look at subROI per group/age"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        selectInput("roi",
                    "What ROI:",
                    factor(kk$SUBROI),
                    multiple=F, 
                    selected = "LA"
                    )
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot"),
         plotOutput("distPlot2"),
         plotOutput("distPlot3")
      )
   )
)

############################################

# Define server logic required to draw a histogram
server <- function(input, output) {

  
   
   output$distPlot <- renderPlot({
     
     kkl.collapseROI = kk.long %>% filter(SUBROI==input$roi) %>%
       group_by(CHANNEL, GROUP, VALENCE, CONTEXT, EXPT, ID) %>%
       summarize(sem = std.error(DENSITY), mean_density = mean(DENSITY), N = length(DENSITY),
                 ymin = mean_density-sem, ymax = mean_density+sem)
     kkl.collapseROI

      ggplot(kkl.collapseROI) + aes(GROUP, mean_density, fill=EXPT, color = EXPT) + facet_wrap(~CHANNEL, scales="free") +
        stat_summary(fun.y = mean, geom="point") +
        stat_summary(fun.data = mean_se, geom="linerange") +
        ggtitle("Comparing densities per channel ~ group/age, where n = # animals ") +
        theme(text=element_text(size=12),
              axis.text.x = element_text(angle = 90, hjust = 1))
        
      

   })   
   
   
   output$distPlot2 <- renderPlot({
     kkl.collapseROI = kk.long %>% filter(SUBROI==input$roi) %>%
       group_by(CHANNEL, GROUP, VALENCE, CONTEXT, EXPT, ID) %>%
       summarize(sem = std.error(DENSITY), mean_density = mean(DENSITY), N = length(DENSITY),
                 ymin = mean_density-sem, ymax = mean_density+sem)
     kkl.collapseROI

     ggplot(kkl.collapseROI) + aes(VALENCE, mean_density, fill=EXPT, color = EXPT) + facet_wrap(~CHANNEL, scales="free") +
       stat_summary(fun.y = mean, geom="point") +
       stat_summary(fun.data = mean_se, geom="linerange") +
       ggtitle("... comparing context vs shock") +
       theme(text=element_text(size=16),
             axis.text.x = element_text(angle = 90, hjust = 1))

   })
   
   
   output$distPlot3 <- renderPlot({
     kkl.collapseROI = kk.long %>% filter(SUBROI==input$roi) %>%
       group_by(CHANNEL, GROUP, VALENCE, CONTEXT, EXPT, ID) %>%
       summarize(sem = std.error(DENSITY), mean_density = mean(DENSITY), N = length(DENSITY),
                 ymin = mean_density-sem, ymax = mean_density+sem)
     kkl.collapseROI
     
     ggplot(kkl.collapseROI) + aes(CONTEXT, mean_density, fill=EXPT, color = EXPT) + facet_wrap(~CHANNEL, scales="free") +
       stat_summary(fun.y = mean, geom="point") +
       stat_summary(fun.data = mean_se, geom="linerange") +
       ggtitle("... comparing AA vs AB") +
       theme(text=element_text(size=12),
             axis.text.x = element_text(angle = 90, hjust = 1))
     
   })
   
   
   
} # end server

# Run the application 
shinyApp(ui = ui, server = server)

