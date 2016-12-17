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
    
 
    
    print(input$plotpop) #checks length of vector for loop function...computer science checking stuff
  
    
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

#RESIDUAL PLOTS
  output$residPlot <- renderUI({
    plotOutput("resP")
  })
  
  output$resP <- renderPlot({
   if(input$graph=="reg"){
     resP<-ggplot(tarmass,aes(x=avg_tarsus,y=mass))+
       list(
         ylab("Mass(g)"),
         xlab("Average tarsus length(mm)"),
         ggtitle("Regression of mass vs tarsus"),
         NULL
       )
   }else{
     resP<-ggplot(tarmass, aes(x=population, y=residual)) 
   }
#factors   
  if(input$factor=="pop" & input$graph=="reg"){
    resP<-resP+
      list(
        geom_point(aes(color=population),size=3),
        geom_smooth(method=lm,se=FALSE, color="black"),
        scale_color_manual(values=c("limegreen","gray40")),
        NULL
      )
  }
  if(input$factor=="sex"& input$graph=="reg"){
    resP<-resP+
      list(
        geom_point(aes(color=sex),size=3),
        geom_smooth(method=lm,se=FALSE, color="black"),
        scale_color_manual(values=c("magenta","blue")),
        NULL
      )
  }
  if(input$factor=="cyc"& input$graph=="reg"){
      resP<-resP+
        list(
          geom_point(aes(color=cycle),size=3),
          geom_smooth(method=lm,se=FALSE, color="black"),
          scale_color_manual(values=c("turquoise","tan2")),
          NULL
        )
    }
  if(input$factor=="pop"& input$graph=="viol"){
      resP<-resP+
        list(
          geom_violin(aes(fill=population),position=position_dodge(1)),
          geom_dotplot(aes(color=population),binaxis='y', stackdir='center', dotsize=0.5, position=position_dodge(1), fill="black"),
          geom_boxplot(aes(fill=population),width=0.1,position=position_dodge(1)),
          scale_fill_manual(values=c("limegreen","gray40")),
          xlab("Population"),
          ylab("Condition index (Residuals)"),
          ggtitle("Condition by population"),
          NULL
        )
  }   
  if(input$factor=="sex"& input$graph=="viol"){
      resP<-resP+
        list(
          geom_violin(aes(fill=sex),position=position_dodge(1)),
          geom_dotplot(aes(color=sex),binaxis='y', stackdir='center', dotsize=0.5, position=position_dodge(1), fill="black"),
          geom_boxplot(aes(fill=sex),width=0.1,position=position_dodge(1)),
          scale_fill_manual(values=c("purple","lightblue2")),
          xlab("Population"),
          ylab("Condition index (Residuals)"),
          ggtitle("Condition by population and sex"),
          NULL
        )
  }       
    
  if(input$factor=="cyc"& input$graph=="viol"){
      resP<-resP+
        list(
          geom_violin(aes(fill=cycle),position=position_dodge(1)),
          geom_dotplot(aes(color=cycle),binaxis='y', stackdir='center', dotsize=0.5, position=position_dodge(1), fill="black"),
          geom_boxplot(aes(fill=cycle),width=0.1,position=position_dodge(1)),
          scale_fill_manual(values=c("turquoise","tan2")),
          xlab("Population"),
          ylab("Condition index (Residuals)"),
          ggtitle("Condition by population and cycle"),
          NULL
        )
    }       
    
    
    #coordflip
    print(resP)
  })
  
  output$myTable <- DT::renderDataTable(
    datatable(birdmass.w[(birdmass.w$pop %in% input$popsub)&(birdmass.w$sex %in% input$sexsub),]) 
  
  )
}