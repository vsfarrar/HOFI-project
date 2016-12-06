
  fluidPage(
    list(tags$head(HTML('<link rel="icon", href="hofi.png", 
                        type="image/png" />'))),
    div(style="padding: 1px 0px; width: '100%'",
        titlePanel(
          title="", windowTitle="HoFI Feeder project"
        )
    ),
navbarPage(
      title=div(img(src="hofi.png", height=50, width=80), "House Finch Feeder project"),
  tabPanel("About",
    sidebarPanel(
      checkboxGroupInput('testDel','Test and delete:',
                         pops,selected=pops)
    ),
    mainPanel(h3("The test text is:")
      )
  ),

  tabPanel("Data",
    sidebarPanel(
      checkboxGroupInput('popsub', 'Population to show:',
        c("Urban (wild birds from ASU)"=pops[1],"Rural (captive birds from South Mtn)"=pops[2]),selected=pops),
      checkboxGroupInput('sexsub', 'Sexes to show:',
        c("Males"=sexes[2],"Females"=sexes[1]))
    ),
    mainPanel("Wide Data-table of all birds.",
              DT::dataTableOutput("myTable"))
    ),
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
)
)
)
