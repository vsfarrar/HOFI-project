##old server as of 12/5/16 11:40PM
function(input, output) {
  
  output$plot <- renderUI({
    plotOutput("p")
  })
  
  #plotting function using ggplot2
  output$p <- renderPlot({
    
    #panels set up separately
    urb.panels<-list(
      annotate("rect",xmin=-Inf,xmax=3,ymin=-Inf,ymax=Inf, fill ="red", alpha = 0.1),
      annotate("rect",xmin=3,xmax=5,ymin=-Inf,ymax=Inf, fill ="blue", alpha = 0.1),
      annotate("rect",xmin=5,xmax=7,ymin=-Inf,ymax=Inf, fill ="red", alpha = 0.1),
      annotate("rect",xmin=7,xmax=9,ymin=-Inf,ymax=Inf, fill ="blue", alpha = 0.1),
      annotate("rect",xmin=9,xmax=Inf,ymin=-Inf,ymax=Inf, fill ="red", alpha = 0.1),
      NULL)
    rur.panels<-list(
      annotate("rect",xmin=-Inf,xmax=3,ymin=-Inf,ymax=Inf, fill ="blue", alpha = 0.1),
      annotate("rect",xmin=3,xmax=5,ymin=-Inf,ymax=Inf, fill ="red", alpha = 0.1),
      annotate("rect",xmin=5,xmax=7,ymin=-Inf,ymax=Inf, fill ="blue", alpha = 0.1),
      annotate("rect",xmin=7,xmax=9,ymin=-Inf,ymax=Inf, fill ="red", alpha = 0.1),
      annotate("rect",xmin=9,xmax=Inf,ymin=-Inf,ymax=Inf, fill ="blue", alpha = 0.1),
      NULL)
    
 
    
    print(input$plotpop)
  
    
    if(length(input$plotpop) == 1 && input$plotpop=="Urban"){
      p<-ggplot(data = (birdmass.l[(birdmass.l$pop %in% input$plotpop)&(birdmass.l$sex %in% input$plotsex),]), aes(x=week,y=mass)) +
        list(
          # annotate("rect",xmin=-Inf,xmax=3,ymin=-Inf,ymax=Inf, fill ="red", alpha = 0.1),
          # annotate("rect",xmin=3,xmax=5,ymin=-Inf,ymax=Inf, fill ="blue", alpha = 0.1),
          # annotate("rect",xmin=5,xmax=7,ymin=-Inf,ymax=Inf, fill ="red", alpha = 0.1),
          # annotate("rect",xmin=7,xmax=9,ymin=-Inf,ymax=Inf, fill ="blue", alpha = 0.1),
          # annotate("rect",xmin=9,xmax=Inf,ymin=-Inf,ymax=Inf, fill ="red", alpha = 0.1),
          geom_line(aes(colour=ID)),
          geom_point(aes(colour=ID)),
          scale_x_continuous(breaks=c(0:9), labels=c("Pre-study","1","2","3","4","5","6","7","8","9")),
          ylab("Mass(g)"),
          xlab("Week of the study"),
          theme(legend.position = "none"),
          NULL
        )+urb.panels
        
    }
    else if(input$plotpop=="Rural"){
      p<-ggplot(data = (birdmass.l[(birdmass.l$pop %in% input$plotpop)&(birdmass.l$sex %in% input$plotsex),]), aes(x=week,y=mass)) +
        list(
          geom_line(aes(colour=ID)),
          geom_point(aes(colour=ID)),
          scale_x_continuous(breaks=c(1:9), labels=c("1","2","3","4","5","6","7","8","9")),
          ylab("Mass(g)"),
          xlab("Week of the study"),
          theme(legend.position = "none"),
          NULL
        )+rur.panels
    }
    else{
      p<-ggplot(data = (birdmass.l[(birdmass.l$sex %in% input$plotsex),]), aes(x=week,y=mass)) +
        list(
          # annotate("rect",xmin=-Inf,xmax=3,ymin=-Inf,ymax=Inf, fill ="red", alpha = 0.1),
          # annotate("rect",xmin=3,xmax=5,ymin=-Inf,ymax=Inf, fill ="blue", alpha = 0.1),
          # annotate("rect",xmin=5,xmax=7,ymin=-Inf,ymax=Inf, fill ="red", alpha = 0.1),
          # annotate("rect",xmin=7,xmax=9,ymin=-Inf,ymax=Inf, fill ="blue", alpha = 0.1),
          # annotate("rect",xmin=9,xmax=Inf,ymin=-Inf,ymax=Inf, fill ="red", alpha = 0.1),
          geom_line(aes(colour=ID)),
          geom_point(aes(colour=ID)),
          scale_x_continuous(breaks=c(0:9), labels=c("Pre-study","1","2","3","4","5","6","7","8","9")),
          ylab("Mass(g)"),
          xlab("Week of the study"),
          theme(legend.position = "none"),
          NULL
        )
    }
    
    if(input$show.avg==TRUE){ 
      p<-p+ stat_summary(fun.y = mean, geom="line", size=1, linetype=5)
    }
    
    print(p)
  })
  #   output$myTestPlot<- renderPlot({
  #     ggplot(data = (birdmass.l[(birdmass.l$pop %in% input$plotpop)&(birdmass.l$sex %in% input$plotsex),]), aes(x=week,y=mass)) +
  #            list(
  #              annotate("rect",xmin=-Inf,xmax=3,ymin=-Inf,ymax=Inf, fill ="red", alpha = 0.1),
  #              annotate("rect",xmin=3,xmax=5,ymin=-Inf,ymax=Inf, fill ="blue", alpha = 0.1),
  #              annotate("rect",xmin=5,xmax=7,ymin=-Inf,ymax=Inf, fill ="red", alpha = 0.1),
  #              annotate("rect",xmin=7,xmax=9,ymin=-Inf,ymax=Inf, fill ="blue", alpha = 0.1),
  #              annotate("rect",xmin=9,xmax=Inf,ymin=-Inf,ymax=Inf, fill ="red", alpha = 0.1),
  #              geom_line(aes(colour=ID)),
  #              geom_point(aes(colour=ID)),
  #              scale_x_continuous(breaks=c(0:9), labels=c("Pre-study","1","2","3","4","5","6","7","8","9")),
  #              ylab("Mass(g)"),
  #              xlab("Week of the study"),
  #              theme(legend.position = "none"),
  #              NULL
  #            )
  # 
  # })
  # 
  output$myTable <- DT::renderDataTable(
    datatable(birdmass.w[(birdmass.w$pop %in% input$popsub)&(birdmass.w$sex %in% input$sexsub),]) 
  )
}