#Test File!
shinyUI(fluidPage(
  titlePanel("[Some dumb a** title here]"),
  
  #headerPanel("What's the difference between a title and a header?", windowTitle = "windowtitle?"),
    #headerPanel feels redundant. 
  
  sidebarLayout(
    sidebarPanel( 'This is the stupid sidebar',
                  selectInput('element_ID',label = 'Choose one:', choices = c("Cat","Dog","none of the above"))),
    mainPanel("All the important sh**t in the main panel",
              h3("Here's it's dumb title"),
              p("And the bunch o'crap description that follows it"),
              tabsetPanel(
                tabPanel("Data", "[Data tables and stuff here]"),
                tabPanel("Summary", "[Summary explanation here]"),
                tabPanel("Plots", "[Pretty plots here]"))
              )
)