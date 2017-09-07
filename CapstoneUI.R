## Capstone UI

library(shiny)

CapstoneUI <- shinyUI(fluidPage(
        
                # Name Of My Project
                titlePanel("Finishing Each Other's... DINNER(Sentences)!"),
        
                # Get the Words To Predict With.
                sidebarLayout(
                        sidebarPanel(
                                helpText("With this app you can input up to three words and receive a prediction for what the next word will be."),
                                textInput("text","Text",value = "Type words here!",placeholder = "Input up to three words"))
                                
                ,

                # Show a plot of the generated distribution
                mainPanel(
                        textOutput("text1"),
                        textOutput("wp"),
                        textOutput("p")
                ))
        
))

# shinyUI(fluidPage(
#         
#         # Application title
#         titlePanel("Old Faithful Geyser Data"),
#         
#         # Sidebar with a slider input for number of bins
#         sidebarLayout(
#                 sidebarPanel(
#                         sliderInput("bins",
#                                     "Number of bins:",
#                                     min = 1,
#                                     max = 50,
#                                     value = 30)
#                 ),
#                 
#                 # Show a plot of the generated distribution
#                 mainPanel(
#                         plotOutput("distPlot")
#                 )
#         )
# ))