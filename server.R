

# Define server logic required to plot various variables against mpg
shinyServer(function(input, output, session) {
  
  
  fitcch<- function(age, cirs, region) {
    region_number<- regions[which(region == regions$Region),2]
    cumbase<-subset(cumbase, AREA==region_number )
    newrow<-cumbase[1,]
    newrow<-transform(newrow,RISK=0, RS_EXIT=0, surv0=1) 
    cumbase<-(rbind(newrow,cumbase))
    
    xbeta<-beta_age*age+beta_cirs*cirs
    exbeta<-exp(xbeta)
    surv<-(cumbase$surv0)^exbeta
    time<-cumbase$RS_EXIT
    survdata<-data.frame(time,surv)
    
    #find survival prob for input age
    surv_row<-which(abs(survdata$time-age)==min(abs(survdata$time-age)))
    if (survdata[max(surv_row),1]<age) {
      surv_base<-survdata[max(surv_row)+1,2] 
    } else { surv_base<-survdata[max(surv_row),2]
    }
    
    #calculate conditional probabilities
    survdata$new_surv<-sapply(survdata$surv,function(x) min(1,x/surv_base))
    
    #calculate median
    if(survdata[which.min(survdata$new_surv),3]>0.5)
    {
      remain<-"N/A"
      median<-"N/A"
    } else{
      your.number=0.5
      medrow<-which(abs(survdata$new_surv-your.number)==min(abs(survdata$new_surv-your.number)))
      if (survdata[medrow,3]<0.5) {
        median<-survdata[medrow-1,1] 
      } else median<-survdata[medrow,1]
      remain<-round(median-age,4)
    }
    
    survdata$type<-paste("CCO ",region," region")
    
    #ontario lifetable 
    #find survival prob for input age
    surv_row_life<-which(abs(lifedata$time-age)==min(abs(lifedata$time-age)))
    #the max function is used in the even that any age with decimal 0.5 is input
    #if not used this will return two survival probs. instead of 1
    if (lifedata[max(surv_row_life),1]<age) {
      surv_base_life<-lifedata[max(surv_row_life)+1,2] 
    } else { surv_base_life<-lifedata[max(surv_row_life),2]
    }
    
    #calculate conditional probabilities
    lifedata$new_surv<-sapply(lifedata$surv,function(x) min(1,x/surv_base_life))
    #get rid of 1st year anomaly
    lifedata<-lifedata[-1,]
    lifedata$type<-"Ontario Life Table"
    
    combined_data<-rbind(lifedata,survdata)
    
    # make more presentable
    df <- survdata[,c("time","new_surv")]
    df <- subset(df,time>=age)
    names(df) <- c("Age Exit",paste("Conditional Survival Prob. given survival to age ",age))
    
    df2 <- lifedata[,c("time","new_surv")]
    df2 <- subset(df2,time>=age)
    names(df2) <- c("Age Exit",paste("Conditional Survival Prob. given survival to age ",age))
    
    
    info <- list(survdata=survdata, lifedata=lifedata, combined_data=combined_data,
                 median=median, le=remain, df=df, df2=df2)
    return(info)
  }
  
  Data <-reactive({
    
    fitcch(input$age, input$cirs, input$ccor)
  
  })
  
  Data_ce <-reactive({
    
    fitcch(input$age, input$cirs, 'Central East')
       
  })
  
  Data_cw <-reactive({
    
    fitcch(input$age, input$cirs, 'Central West')
    
  })
   
  Data_e <-reactive({
    
    fitcch(input$age, input$cirs, 'East')
    
  })
  
  Data_ne <-reactive({
    
    fitcch(input$age, input$cirs, 'North East')
    
  })
  
  Data_nw <-reactive({
    
    fitcch(input$age, input$cirs, 'North West')
    
  })
  
  Data_s <-reactive({
    
    fitcch(input$age, input$cirs, 'South')
    
  })
  
  Data_se <-reactive({
    
    fitcch(input$age, input$cirs, 'South East')
    
  })
  
  Data_sw <-reactive({
    
    fitcch(input$age, input$cirs, 'South West')
    
  })
  
  Data_All<- reactive ({
    
    rbind(Data_ce()$survdata,Data_cw()$survdata,
                   Data_e()$survdata,Data_ne()$survdata,
                   Data_nw()$survdata,Data_s()$survdata,
                   Data_se()$survdata,Data_sw()$combined_data)
    
  })
  
  Data_Region<- reactive ({
    
    rbind(Data_ce()$survdata,Data_cw()$survdata,
          Data_e()$survdata,Data_ne()$survdata,
          Data_nw()$survdata,Data_s()$survdata,
          Data_se()$survdata,Data_sw()$survdata)
    
  })
  
    
  # enable paging on gvisTable
  myOptions <- reactive({  
    list(
      
      page='enable',
      pageSize=15
      
    )
  })
  
  
  # Use gvisTable to enable paging and sorting
  output$gvisTable <- renderGvis( {
    
    # make more presentable
    df <- Data()$survdata[,c("time","new_surv")]
    df <- subset(df,time>=input$age)
    names(df) <- c("Age Exit",paste("Conditional Survival Prob. given survival to age ",input$age))
    
    
    gvisTable(df, options=myOptions())
  })
  
  # Use gvisTable to enable paging and sorting
  output$gvisTable2 <- renderGvis( {
    
    # make more presentable
    df2 <- Data()$lifedata[,c("time","new_surv")]
    df2 <- subset(df2,time>=input$age)
    names(df2) <- c("Age Exit",paste("Conditional Survival Prob. given survival to age ",input$age))
    
    
    gvisTable(df2, options=myOptions())
  })
  
  
  output$downloadData <- downloadHandler(
    filename = function() { paste("Age=",input$age," CIRS=",input$cirs," ",input$ccor," Region", '.csv', sep='') },
    content = function(file) {
      write.csv(Data()$df, file, row.names=F)
    }
  )
  
  output$downloadData2 <- downloadHandler(
    filename = function() { paste("Age=",input$age," Ontario Life Table", '.csv', sep='') },
    content = function(file) {
      write.csv(Data()$df2, file, row.names=F)
    }
  )
  
  
  #Compute the forumla text in a reactive expression since it is 
  #shared by the output$caption and output$mpgPlot expressions
  formulaText <- reactive({
    
    if (Data()$median=="N/A"){
   paste("The median survival time cannot be computed for",input$ccor,"Region, Age", input$age, " and 
         CIRS score ", input$cirs, "from the data used to derive the above survival curve") 
    } else {
      paste("A survival probability of 0.50 was used as a measure of median survival time")
    }
  })
  
  #Return the formula text for printing as a caption
  output$caption <- renderText({
   formulaText()
  })
  
  # Generate a plot of the requested variable against mpg and only 
  # include outliers if requested
  output$plot <- renderPlot({
    #boxplot(as.formula(formulaText()), 
     #       data = mpgData,
      #      outline = input$outliers)
  
    if (input$ontlife){  
      p <- ggplot(Data()$combined_data, aes(x=time, y=new_surv))
      surv.plot<- p+geom_step(size = 1.1, aes(colour=type))+xlab("age (years)")+ylab("survival probability")+
        labs(title=paste("Survival Curves for Ontario Life Table, Men (2000-2002)","and All Cause Mortality for",input$ccor,"Region (1999)"), colour="")+
        geom_hline(yintercept = 0.50, linetype = "longdash")+
        scale_x_continuous(breaks=seq(0,105,5))+ theme(legend.position="bottom")+scale_y_continuous(breaks=seq(0,1,0.1))
    } else { p <- ggplot(Data()$survdata, aes(x=time, y=new_surv))
             surv.plot<- p+geom_step(size = 1.1, aes(colour=type))+xlab("age (years)")+ylab("survival probability")+
               labs(title=paste("Survival Curve from All Cause Mortality for",input$ccor,"Region (1999)"), colour="")+
               geom_hline(yintercept = 0.50, linetype = "longdash")+
               scale_x_continuous(breaks=seq(0,105,5))+ theme(legend.position="bottom")
    }
    
    print(surv.plot)
    
    })
  
  output$plotall <- renderPlot({
    #boxplot(as.formula(formulaText()), 
    #       data = mpgData,
    #      outline = input$outliers)
    
    if (input$ontlife){  
      p <- ggplot(Data_All(), aes(x=time, y=new_surv))
      surv.plot2<- p+geom_step(size = 1.1, aes(colour=type))+xlab("age (years)")+ylab("survival probability")+
        labs(title=paste("Survival Curves for Ontario Life Table, Men (2000-2002) and All Cause Mortality by Region (1999)"), colour="")+
        geom_hline(yintercept = 0.50, linetype = "longdash")+
        scale_x_continuous(breaks=seq(0,105,5))+ scale_y_continuous(breaks=seq(0,1,0.1))
    } else {
      p <- ggplot(Data_Region(), aes(x=time, y=new_surv))
      surv.plot2<- p+geom_step(size = 1.1, aes(colour=type))+xlab("age (years)")+ylab("survival probability")+
        labs(title=paste("Survival Curves from All Cause Mortality by CCO Region (1999)"), colour="")+
        geom_hline(yintercept = 0.50, linetype = "longdash")+
        scale_x_continuous(breaks=seq(0,105,5))+ scale_y_continuous(breaks=seq(0,1,0.1))
    }
    
    print(surv.plot2)
    
  })
  
  
    # Reactive expression to compose a data frame containing all of the values
  sliderValues <- reactive({
    
    # Compose data frame
    data.frame(
      Result = c("Median Survival Time (years)",
               "Remaining Lifetime (years)"
                ),
      Value = as.character(c(Data()$median,
                             Data()$le
                             )), 
      stringsAsFactors=FALSE)
  }) 
  
  # Show the values using an HTML table
  output$values <- renderTable({
    sliderValues()
  })

  
  ageValues <- reactive({
    
    # Compose data frame
    data.frame(
      Input = c("Age (years)", 
               "CIRS-G_pros"
      ),
      Value = as.character(c(input$age, 
                             input$cirs
      )), 
      stringsAsFactors=FALSE)
  }) 
  
  # Show the values using an HTML table
  output$agevalues <- renderTable({
    ageValues()
  }) 
  
  output$agevalues2 <- renderTable({
    ageValues()
  }) 
  
#   ageValues2 <- reactive({
#     
#     # Compose data frame
#     data.frame(
#       Input = c("Age (years)", 
#                 "CIRS-G_pros"
#       ),
#       Value = as.character(c(input$age, 
#                              input$cirs
#       )), 
#       stringsAsFactors=FALSE)
#   }) 
  
  
  


  
  medianValues <- reactive({
    
    # Compose data frame
    data.frame(
      Region = c("Median Survival Time (years)",
               "Remaining Lifetime (years)"
      ),
      Central_East = as.character(c(Data_ce()$median,
                             Data_ce()$le
      )),
      Central_West = as.character(c(Data_cw()$median,
                                    Data_cw()$le
      )), 
      East = as.character(c(Data_e()$median,
                                    Data_e()$le
      )), 
      North_East = as.character(c(Data_ne()$median,
                                    Data_ne()$le
      )), 
      North_West = as.character(c(Data_nw()$median,
                                    Data_nw()$le
      )), 
      South = as.character(c(Data_s()$median,
                                    Data_s()$le
      )), 
      South_East = as.character(c(Data_se()$median,
                                    Data_se()$le
      )), 
      South_West = as.character(c(Data_sw()$median,
                                    Data_sw()$le
      )), 
      
      stringsAsFactors=FALSE)
  }) 
  
# Show the values using an HTML table
output$medianvalues <- renderTable({
  medianValues()
})

})


#add comment here