#
# This Shiny web application display the probability of each 3 card hand for the board game
# Secret Hitler. The game uses a 17 deck made up of 6 Liberal and 11 Facist cards with a discard 
# pile. Hence, the without replacement probabilities are calculated conditional on the cards that 
# have been used.  The cards on the board are visible to all players, while the cards in the discard
# were only visilbe to the players that used them.
#

library(shiny)
library(dplyr)
library(ggplot2)
library(ggthemes)


# Define UI for application
ui <- fluidPage(
   
   # Application title
   titlePanel("Secret Hitler Probability Calculator"),
   helpText("Secret Hitler is a board game (www.secrethitler.com) and has nothing to do with actual Hitler or Trump."),
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        h4("Cards on the Board"),
        sliderInput("libs",
                     "Number of Liberal Cards",
                     0,
                     min = 0,
                     max = 4,
                     step = 1),
        sliderInput("facists",
                  "Number of Facist Cards",
                  0,
                  min = 0,
                  max = 5,
                  step = 1),
        h4('Cards you "know" to be in the discard'),
        helpText('Are you sure?'),
        sliderInput("libs_disc",
                    "Number of Liberal Cards",
                    0,
                    min = 0,
                    max = 6,
                    step = 1),
        sliderInput("facists_disc",
                    "Number of Facist Cards",
                    0,
                    min = 0,
                    max = 8,
                    step = 1)
      ),
      
      # Show results in the main panel.
      mainPanel(
         tableOutput("table"),
         h5("Chance that the President's 3 card draw is:"),
         plotOutput("plot",width = 300),
         helpText('Intructions: User the sliders to show the cards that are on the board. 
                  Additionally, you can add cards that you "know" are in the discard. But, unless 
                  you discarded it yourself, be careful. The chart shows the chance that the predsident
                  selects each 3 card combination."LLL" is three liberal cards and so forth.')
      )
   )
)

# Define server logic
server <- function(input, output) {
  #The cards object gets the collection of cards in the deck based on the user input. 
  cards <- reactive({
   nlib = 6 - (input$libs + input$libs_disc)
   nf = 11 - (input$facists + input$facists_disc)
   nlib = max(nlib,0)
   nf = max(nf,0)
   ncard = nlib + nf
   return(data.frame(nlib,nf,ncard))
   })
  #The probs object caculates the probabilities based on the cards and includes formatting helpers for ggplot.
   probs=reactive({
     nlib=cards()$nlib
     nf=cards()$nf
     ncard=cards()$ncard
     probs = data.frame(
       draw = factor(c('LLL','LLF','LFF','FFF'),levels=c('LLL','LLF','LFF','FFF')),
       prob = c(
              choose(nlib,3)*choose(nf,0)/choose(ncard,3),
              choose(nlib,2)*choose(nf,1)/choose(ncard,3),
              choose(nlib,1)*choose(nf,2)/choose(ncard,3),
              choose(nlib,0)*choose(nf,3)/choose(ncard,3)
              )
       )
     probs = probs %>% 
             arrange(desc(draw)) %>%
             mutate(pos=cumsum(prob)-prob*.5,
                    lab=paste0(round(prob*100,1),'%'))
   })
   #The output table just shows the collection of crds baed on the input.
   output$table <-renderTable({data.frame(Liberal_Cards=sprintf('%1.0f',cards()$nlib),
                                          Facicst_Cards=sprintf('%1.0f',cards()$nf))
   })
   #The output plot shows the probability of each 3 card combination. Minimalist theme. 300px for mobile freindly.
   output$plot <- renderPlot({
                  ggplot(data=probs(),aes(x=1,y=prob,fill=draw)) +
                  geom_bar(stat="identity") + 
                  geom_text(aes(y=probs()$pos),label=probs()$lab,vjust=.75,fontface="bold") +
                  geom_text(aes(y=probs()$pos),label=probs()$draw,vjust=-.75,fontface="bold") +
                  theme_few() +
                  theme(axis.text.x = element_blank(),
                        axis.title.x = element_blank(),
                        axis.ticks.x = element_blank()) + 
                  scale_y_continuous(labels = scales::percent) +
                  scale_fill_manual(values=c("#0099CC","#FFCC33","#999966","#FF6600"))
   })
}
# Run the application 
shinyApp(ui = ui, server = server)