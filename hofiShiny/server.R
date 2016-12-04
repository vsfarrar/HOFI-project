function(input, output) {
  currentGraphz<- reactive({graphz(input$plotpop)})
  output$myText<-renderText({currentGraphz()})
  
  # output$myPlot<-renderPlot({
  #   ggplot(data = (birdmass.l[(birdmass.l$pop %in% input$plotpop)&(birdmass.l$sex %in% input$plotsex),]), aes(x=week,y=mass)) +
  #            list(
  #              annotate("rect",xmin=-Inf,xmax=3,ymin=-Inf,ymax=Inf, fill ="red", alpha = 0.1),
  #              annotate("rect",xmin=3,xmax=5,ymin=-Inf,ymax=Inf, fill ="blue", alpha = 0.1),
  #              annotate("rect",xmin=5,xmax=7,ymin=-Inf,ymax=Inf, fill ="red", alpha = 0.1),
  #              annotate("rect",xmin=7,xmax=9,ymin=-Inf,ymax=Inf, fill ="blue", alpha = 0.1),
  #              annotate("rect",xmin=9,xmax=Inf,ymin=-Inf,ymax=Inf, fill ="red", alpha = 0.1),
  #              geom_line(aes(colour=ID)),
  #              geom_point(aes(colour=ID)),
  #              scale_x_continuous(breaks=c(1:10), labels=c("Pre-study","1","2","3","4","5","6","7","8","9")),
  #              ylab("Mass(g)"),
  #              xlab("Week of the study"),
  #              theme(legend.position = "none"),
  #              NULL
  #            )

  #})

  output$myTable <- DT::renderDataTable(
    datatable(birdmass.w[(birdmass.w$pop %in% input$popsub)&(birdmass.w$sex %in% input$sexsub),]) 
  )
}

    #Attempting to color code cells conditionally by dirty or clean cycle
    
#      %>%
#       formatStyle('wk0','pop', backgroundColor= styleEqual(pops,c('moccasin','lightskyblue','lightskyblue'))%>%
#       formatStyle('wk1','pop', backgroundColor= styleEqual(pops,c('moccasin','lightskyblue','lightskyblue')))
#       )
#   )
# }
      #%>%
      # formatStyle('wk2','pop', backgroundColor= styleEqual(pops,c('lightskyblue','moccasin','moccasin'))%>%
      # formatStyle('wk3','pop', backgroundColor= styleEqual(pops,c('lightskyblue','moccasin','moccasin'))%>%
      # formatStyle('wk4','pop', backgroundColor= styleEqual(pops,c('moccasin','lightskyblue','lightskyblue'))%>%
      # formatStyle('wk5','pop', backgroundColor= styleEqual(pops,c('moccasin','lightskyblue','lightskyblue'))%>%
      # formatStyle('wk6','pop', backgroundColor= styleEqual(pops,c('lightskyblue','moccasin','moccasin'))%>%
      # formatStyle('wk7','pop', backgroundColor= styleEqual(pops,c('lightskyblue','moccasin','moccasin'))%>%
      # formatStyle('wk8','pop', backgroundColor= styleEqual(pops,c('moccasin','lightskyblue','lightskyblue'))%>%
      # formatStyle('wk9','pop', backgroundColor= styleEqual(pops,c('moccasin','lightskyblue','lightskyblue')))
      # )))))))))
  
  
