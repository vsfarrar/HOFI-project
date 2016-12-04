navbarPage("House Finch Feeder project",
  tabPanel("About",
    sidebarPanel(
      checkboxGroupInput('plotpop', 'Population to show:',
                         c("Urban (wild birds from ASU)"=pops[1],"Rural (captive birds from South Mtn)"=pops[2]),selected=pops),
      checkboxGroupInput('plotsex', 'Sexes to show:',
                         c("Males"=sexes[2],"Females"=sexes[1]))
    ),
    mainPanel(h3("The test text is:"),
              textOutput("myText"), 
              plotOutput("myPlot")
      )
  )

  tabPanel("Data",
    sidebarPanel(
      checkboxGroupInput('popsub', 'Population to show:',
        c("Urban (wild birds from ASU)"=pops[1],"Rural (captive birds from South Mtn)"=pops[2]),selected=pops),
      checkboxGroupInput('sexsub', 'Sexes to show:',
        c("Males"=sexes[2],"Females"=sexes[1]))
    ),
    mainPanel("Wide Data-table of all birds.",
              DT::dataTableOutput("myTable"))
    )
)