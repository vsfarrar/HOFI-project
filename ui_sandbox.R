#Test File!
shinyUI(fluidPage(
  titlePanel("[Some dumb a** title here]"),
  
  #headerPanel("What's the difference between a title and a header?", windowTitle = "windowtitle?"),
    #headerPanel feels redundant. 
  
  sidebarLayout(
    sidebarPanel( 'This is the stupid sidebar',
                  selectInput('element_ID',label = 'Choose one:', choices = c("Cat","Dog","none of the above")),
                  br(),
                  checkboxGroupInput("checkGroup", label = h3("Select one:"), choices = list("Choice 1" = 1, "Choice 2" = 2, "Choice 3" = 3),selected = 1)),
    mainPanel("All the important sh**t in the main panel",
              h3("Here's it's dumb title"),
              p("And the bunch o'crap description that follows it"),
              tabsetPanel(
                tabPanel("Data", "[Data tables and stuff here]",
                         (DT::dataTableOutput("myTable"))),
                tabPanel("Summary", "[Summary explanation here]"),
                tabPanel("Plots", "[Pretty plots here]"))
              )
)))