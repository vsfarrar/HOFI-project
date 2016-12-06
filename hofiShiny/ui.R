fluidPage(
    theme=shinytheme("simplex"),
    list(tags$head(HTML('<link rel="icon", href="hofi.png", 
                        type="image/png" />'))),
    div(style="padding: 1px 0px; width: '100%'",
        titlePanel(
          title="", windowTitle="HoFI Feeder project"
        )
    ),#div
navbarPage(
      title=div(img(src="hofi.png", height=50, width=80), "House Finch Feeder project"),
  tabPanel("About",
    sidebarPanel(
      selectInput("info","Read more about...", 
                  c("Study goals" = "goals", "Experimental design" = "design", "Future Analyses" = "follup")),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br()
      ),#side
    conditionalPanel(
        condition="input.info=='goals'",
        h2("Study goals:"),
        h3("We aimed to answer the question",em("Do bird feeders affect the health of wild birds?")),
        p("Urban birds are known to have higher disease loads, including intestinal parasites
          like coccidia and external infections, like avian pox (McGraw et al 2014) when compared to rural
          birds. Could this be due to bird feeders - artificial food sources that attract birds in large
          flocks and act as fomites for disease? Most people, intending to help wild birds, do not clean
          their feeders, and could be contributing to disease in dense, urban populations. We wanted to explore
          how rural and urban house finches respond differently to dirty feeders versus feeder cleaning, 
          especially when feeders are their only source of food.")
        ), #main
   conditionalPanel(
     condition="input.info=='design'",
     h2("Experimental design:"),
     p("Rural birds (23) from South Mountain Preserve were kept in captivity during the study period,
        while urban birds were trapped on Arizona state University campus each week.Urban, wild birds 
        used and dirtied feeders for two week periods, during which
        rural birds used clean feeders that were bleached daily. Every 2 week cycle, 
        urban bird feeders were given to rural birds without cleaning, while urban birds
        recieved feeder cleaning daily. Every week we measured mass, pox, took microbiome
        swabs, and collected fecal samples for coccidians.")
   ),
   conditionalPanel(
     condition="input.info=='follup'",
     h2("Future analyses:"),
     p("We hope to conduct microbiome analysis on the swabs in the future.")
   )
  ),#tab
  tabPanel("Data",
    sidebarPanel(
      checkboxGroupInput('popsub', 'Population to show:',
        c("Urban (wild birds from ASU)"=pops[1],"Rural (captive birds from South Mtn)"=pops[2]),selected=pops),
      checkboxGroupInput('sexsub', 'Sexes to show:',
        c("Males"=sexes[2],"Females"=sexes[1]))
    ),#side
    mainPanel("Wide Data-table of all birds.",
              DT::dataTableOutput("myTable")
              )#main
    ),#tab
  tabPanel("Graphs",
           sidebarPanel(
            checkboxGroupInput('plotpop', 'Population to show:',
                               c("Urban (wild birds from ASU)"=pops[1],"Rural (captive birds from South Mtn)"=pops[2])),
            checkboxGroupInput('plotsex', 'Sexes to show:',
                               c("Males"=sexes[2],"Females"=sexes[1])),
            checkboxInput("show.avg", "Show average line", TRUE)
           ),
           mainPanel(
             uiOutput("plot"),#depends on input
             br(),
             p("Key to cycle period colors:"),
             p(span("Clean feeders (bleached daily)", style="color:blue")),
             p(span("Dirty feeders", style="color:red"))
           )
) #tab
) #navbarpage
) #fluidpage

