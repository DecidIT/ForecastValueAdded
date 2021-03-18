#---------------------------------------------#
#-------------- DEMO DecidIT -----------------#
#---------------------------------------------#
library(shiny)
library(shinythemes)
library(forecast)
library(smooth)
library(scales)
library(MLmetrics)
library(magrittr)
library(plyr)
library(dplyr)
library(ggplot2)
library(xts)
library(reshape2)
library(lubridate)
library(DT)
library(dygraphs)
library(markdown)
library(DescTools)
library(yardstick)  #See package "greybox"
library(pwr)


function(input, output) {
####------------------------------------####  
#----- Constants
####------------------------------------####  

  DefaultFile    <- "Data/BioProdLoc.csv"  
  
  c_DisplayType <- list('Histogram'  = "Hist",                          #Display Types
                        'Box plot'   = "Bplot", 
                        'Line'       = "Line")  
  
  c_ErrorMetrics <- list(Percentage = c('MAPE'  = "MAPE", 'Forecast Accuracy' = 'Accuracy'),
                         Relative   = c('GMRAE' = 'GMRAE'),   # Geometric Mean Relative Absolute Error
                         Scaled     = c('MASE'  = 'MASE')) #,
 #$$$ Temporary commemt# Other      = c('MAD/MEAN ratio'  = 'MAD/MEAN'))
  
  c_NaiveFcst    <- list("Random Walk" = "naive",                      #Naive Forecasts
                         "Seasonal Random Walk" = "snaive", 
                         "Moving Average" = "ma")
  c_Frequency   <- list("Quarter"  = 4,
                        "Annual"   = 12 )
  
####------------------------------------####  
#----- FUNCTIONS
####------------------------------------####  

#-------------------------------------------
# List with file description and information
#-------------------------------------------  
   FileInfo <- reactive({
#    Read file
     file         <- read.csv(rv$file, header = TRUE, sep = input$sep, stringsAsFactors = FALSE, strip.white = TRUE)
     cat(file=stderr(), "File name is", DefaultFile, "\n")
#    Set file info     
     col1         <- as.character(file[,1])
     col2         <- as.character(file[,2])
     titlecol1    <- as.character(colnames(file)[1])
     titlecol2    <- colnames(file)[2]
     nbcol        <- length(file) 
     cols         <- colnames(file)
     colnames(file)[3:4]   <- c("Bucket","Actuals")
     subfile      <- file[,3:nbcol]
     buckcolnr    <- which(colnames(subfile) == "Bucket")
     actcolnr     <- which(colnames(subfile) == "Actuals")
#    Output Result List - FileInfo                                  
     result    <- list(Criter1    = col1,                        # Criteria 1 - Data
                       Criter2    = col2,                        # Criteria 2 - Data
                       NameCrit1  = titlecol1,                   # Criteria 1 - Name
                       NameCrit2  = titlecol2,                   # Criteria 2 - Name
                       BuckColNr  = buckcolnr,                   # "Bucket"  column number (=1)
                       ActColNr   = actcolnr,                    # "Actuals" column number (=2)
                       Steps      = cols[5:nbcol],               # Forecast Steps 
                       Content    = file,                        # Full content
                       Data       = subfile                      # Content for calculation
                      ) 
     return(result)
   })  
  
  #------------------------------------------- 
  FileData <- reactive({
  #-------------------------------------------
  file     <- read.csv(rv$file, header = TRUE, sep = input$sep)
  colnames(file)[3:4]      <- c("Bucket","Actuals")       # Rename "hearts" columns
  
  subfile  <- file
  if (!is.null(input$Filter1))  {
    subfile  <- dplyr::filter(file, FileInfo()$Criter1 %in% (input$Filter1))
  }
  if (!is.null(input$Filter2)) {
    subfile  <- dplyr::filter(file, FileInfo()$Criter2 %in% input$Filter2)
  }
    nbcol    <- length(file) 
    tsset    <- subfile[,3:nbcol]
    result   <- list(Content = file,        # Full content
                     SelData = subfile,     # Filtered subset
                     TsSet   = tsset)       # Time Series Subset"
  }) 
  #-------------------------------------------
  MeltTabError <- reactive({
  #-------------------------------------------  
       t.error  <- group_by(FileData()$TsSet, Bucket)
       Naive    <- NaiveForecast()
       Naive    <- data.frame(fitted(Naive))
       Naives   <- bind_cols(Naive,Naive)
       colnames(Naives) <- c("Naive1", "Naive")
       t.error  <- bind_cols(t.error[,1:2], Naives, t.error[,3:ncol(t.error)])
    
    t.error <- melt(t.error, id= c("Bucket", "Actuals", "Naive1"), variable = "steps", value.name = "Fcst")
    
    line <- seq(1:nrow(t.error))
    switch(input$ErrorMetric,
#---       MAPE            
           MAPE = {error <- sapply(line, function(i){
             MAPE(t.error$Fcst[i], t.error$Actuals[i])
                  })
           },
#---      Forecast Accuracy 
          Accuracy = {error <- sapply(line, function(i){
            1 - MAPE(t.error$Actuals[i], t.error$Fcst[i])
                  })
          },
#---      GMRAE
          GMRAE = {error <- sapply(line, function(i){
             MAE(t.error$Fcst[i], t.error$Naive[i], t.error$Actuals[i])
          })
          }, 
#---      MASE - Mean Absolute Scaled Error (Hyndman & Koehler, 2006)
          MASE = {error <- unlist(MaseTabError()$mase_results[,4])}
)
         
    TabError <- bind_cols(t.error,data.frame(error))
    TabError$Naive1  <- NULL
    TabError$Bucket  <- ymd(TabError$Bucket) 
    return(TabError)
    
  })

#-------------------------------------------
  MaseTabError <- reactive({
#-------------------------------------------   
# Table contenant le calcul de MASE groupees par product/Loc
#    Prod/Loc/Actuals/Bucket/Mase-Naive/Mase-Others
# Table d'entree pour le renseignement de MeltTabError
#   Bucket/Steps/Naive/Actuals/Value?/Error    
    
    t.SelData  <- group_by(FileData()$SelData, Bucket)
    Naive      <- NaiveForecast()
    Naives     <- bind_cols(Naive,Naive)
    colnames(Naives) <- c("Naive1", "Naive")   
    t.SelData  <- bind_cols(t.SelData[,1:3], Naives, t.SelData[,4:ncol(t.SelData)])
    t.SelData  <- melt(t.SelData, id= c(colnames(t.SelData)[1], colnames(t.SelData)[2],"Bucket", "Actuals", "Naive1"), variable = "steps", value.name = "Fcst")

    mase_results <- t.SelData %>%
           group_by(steps)    %>%
           yardstick::mase(Actuals, Fcst)
    
    mase_reslev1 <- t.SelData        %>%
      group_by(t.SelData[,2], steps) %>%
      yardstick::mase(Actuals, Fcst)
    colnames(mase_reslev1)[1] <- 'Level1'
    
    mase_reslev2 <- t.SelData        %>%
      group_by(t.SelData[,1], steps) %>%
      yardstick::mase(Actuals, Fcst)
    colnames(mase_reslev2)[1] <- 'Level2'    
    
    mase_reslev1 <- subset(mase_reslev1, (steps %in% c(input$StepA, input$StepB)))
    mase_reslev2 <- subset(mase_reslev2, (steps %in% c(input$StepA, input$StepB)))
    
    return (  list(  mase_results = mase_results,
                     mase_reslev1 = mase_reslev1,
                     mase_reslev2 = mase_reslev2))
    
  })
#-------------------------------------------
  NaiveForecast <- reactive({
#-------------------------------------------  
    t.error <- group_by(FileData()$TsSet, Bucket)
    switch(input$NaiveModel,
           naive  =                 # Random walk
             { fcst <- naive(t.error$Actuals)
               fcst <- fcst["fitted"]
             },
           snaive =                  # Seasonal random walk
            # { fit <- snaive(ts(forecast,freq=2), h=7)
              { ts.actuals <- ts(t.error$Actuals, frequency = as.integer(input$SnFrequ))
                fcst <- snaive(ts.actuals)
                fcst <- fcst["fitted"]
             },
           ma    =                   # Moving average
            { fcst <- sma(t.error$Actuals, order = input$MaOrder)  } 
    )
   
    return(fcst)
  })
#-------------------------------------------
  Accuracy  <- function(fcst,act) {
     result  <- (1 - MAPE(fcst,act)) * 100
   return(result)
  }
#-------------------------------------------
  TrackSignal  <- function(fcst,act) {
# In [-1;1]    Threeeshold = 4,5 for 12 periods  
    result  <- (act-fcst)/abs(act-fcst)
    return(result)
  }
#------------------------------------------- 
  NormBias  <- function(fcst,act) {
# In [-1;1]   >2 for 12 periods    
    result  <- (fcst-act)/(fcst+act)
    return(result)
  }
#-------------------------------------------
# Mean Relative Absolute Error - Observation level
  MAE <- function(fcst, naive, act) { 
    #if (act != naive){
    result = abs((act - fcst) / (act - naive))
    return(result)
   # }

  } 
#-------------------------------------------  
  GMRAE <- DescTools::Gmean
 
####------------------------------------####  
#----- Reactive Values
####------------------------------------#### 
  rv <- reactiveValues(
    file      = DefaultFile
  )  
  
####------------------------------------####  
  
# Set Logo
  output$ImageLogo1 <- renderImage({
    return(list(
      src = "Images/Logo.png",
      contentType = "image/png",
      alt = "Help1"
    ))
  }, deleteFile = FALSE)
  
  # Set Logo
  output$ImageLogo2 <- renderImage({
    return(list(
      src = "Images/Logo.png",
      contentType = "image/png",
      alt = "Help1"
    ))
  }, deleteFile = FALSE)
  
  # Set Logo
  output$FileFormat <- renderImage({
    return(list(
      src = "Images/FileFormat.jpg",
      contentType = "image/jpeg",
      alt = "Help1"
    ))
  }, deleteFile = FALSE)
  
####------------------------------------####
#- Forecast Value Added - SIDE BAR
####------------------------------------####
#---- Reading file
#------------------------------------------#

  # Load sample dataset
  observeEvent(input$SampleDataLink, {
    rv$file  <- DefaultFile
  })  
  
  # Load user data
  observeEvent(input$UserFile, {
    rv$file <- input$UserFile$datapath
  })

  # filter dataset
  observeEvent(c(input$Filter1,input$Filter2), {
      #file <- filter(FileInfo()$Content, Product %in% input$Filter1, Location %in% input$Filter2)
     # FileInfo()
  })
  
#------------------------------------------#
#---------- SIDE BAR
#------------------------------------------#
#------------------------------------------#
#----- Session - SIDE BAR
#------------------------------------------#
  
  output$DisplayType <- renderUI({
    selectInput("DisplayType", "Display type:",
                c_DisplayType, selected = c_DisplayType[1])
  }) 
  
  # output$NbBins <- renderUI({
  #   if (input$DisplayType != "Hist") return(NULL)
  #   sliderInput("NbBins", "Bins:", min= 0, max= 100, value= 10, step= 5)
  # })
#------------------------------------------#
#----- Inputs - SIDE BAR
#------------------------------------------#
  
  
output$ErrorMetric <- renderUI({
   selectInput("ErrorMetric", "Error metric:",
               c_ErrorMetrics, selected = c_ErrorMetrics[1])
 })

output$NaiveModel <- renderUI({
  if (input$ErrorMetric == "MASE") { #$$$ Temporary if
  selectInput("NaiveModel", "Naive forecast:",
                c_NaiveFcst[1])#, selected = c_NaiveFcst[1])
  } else {
  selectInput("NaiveModel", "Naive forecast:",
              c_NaiveFcst, selected = c_NaiveFcst[1])
}
})

output$SnFrequ <- renderUI({
  if (input$NaiveModel != "snaive") return(NULL)
  selectInput("SnFrequ", "Frequency:",
              c_Frequency, selected = c_Frequency[1])
})

output$MaOrder <- renderUI({
  if (input$NaiveModel != "ma") return(NULL)
  sliderInput("MaOrder", "Order:", min= 2, max= 12, value= 3, step= 1)
})

output$ReadWiki01 <- renderUI({
  
  switch(input$ErrorMetric,
  MAPE     =
   More <- p("Read more about this metric here → ", a(href="https://en.wikipedia.org/wiki/Mean_absolute_percentage_error", icon("wikipedia-w"),target="_blank"),style="color:black;text-align:center"),
  Accuracy =
   More <- p("Read more about this metric here → ", a(href="https://en.wikipedia.org/wiki/Mean_absolute_percentage_error", icon("wikipedia-w"),target="_blank"),style="color:black;text-align:center"),
  GMRAE    = 
   More <- p("Read more about this metric here → ", a(href="https://support.numxl.com/hc/en-us/articles/115001223403", icon("internet-explorer"),target="_blank"),style="color:black;text-align:center"),
  MASE     =        
   More <- p("Read more about this metric here → ", a(href="https://en.wikipedia.org/wiki/Mean_absolute_scaled_error", icon("wikipedia-w"),target="_blank"),style="color:black;text-align:center")
  ) # EndSwitch
  
})

#------------------------------------------
#----- Filters - SIDE BAR
#------------------------------------------
output$Filter1 <- renderUI({
#... Set placeholder
  Criter1  <- FileInfo()$Criter1
  SelTitle <- paste(FileInfo()$NameCrit1, ":")
  SelInfo  <- paste("Select", FileInfo()$NameCrit1)
  selectizeInput("Filter1", SelTitle, 
                 choices = Criter1,
                 multiple= TRUE,
                 options = list(placeholder = SelInfo))  
})

output$Filter2 <- renderUI({
  Criter2 <- FileInfo()$Criter2
  SelTitle <- paste(FileInfo()$NameCrit2, ":")
  SelInfo <- paste("Select", FileInfo()$NameCrit2) 
  selectizeInput("Filter2",  SelTitle,
                 choices= Criter2,
                 multiple= TRUE,
                 options = list(placeholder = SelInfo))
})

 ####------------------------------------####
 #- SESSION - Main Panel
 ####------------------------------------####
 #--- SESSION - table output
#------------------------------------------
 output$ContentTable <- renderDataTable({
    datatable(FileData()$SelData, options = list( lengthMenu = c(5,10,25,50,100),pageLength = 5))
 }) 
#------------------------------------------
#--- SESSION - Business Data
#------------------------------------------

    output$BusinessDataPlot <- renderPlot({
           TsSet     <- group_by(FileData()$TsSet, Bucket)
           MeltTsSet <- melt(TsSet, id= "Bucket", variable= "Steps", value.name= "Fcst")
 
       switch(input$DisplayType,
           Line = {
             ggplot(MeltTsSet, aes(x = ymd(Bucket), y = Fcst)) +
               geom_line(aes(color = Steps), size = 1) +
               labs(title = "Time series") +
               #theme(legend.position = "top") 
               theme_bw()
           },    
           Hist = {          
             ggplot(MeltTsSet,aes(x=Fcst, fill = Steps)) +
               geom_histogram(bins=nclass.Sturges(MeltTsSet),color="white",aes(y=..density..),lwd=0.8) +
               geom_density(color="seagreen4", alpha=0.3,fill="seagreen4",lty=1)+
               geom_vline(aes(xintercept=mean(Fcst)), linetype="dashed") +
               labs(title = "Histogram") +
               facet_grid(Steps~.)  +
               #theme(legend.position = "top") 
               theme_bw()

           },
           Bplot = { 
               ggplot(MeltTsSet, aes(x= Fcst, y= Steps, fill= Steps)) +
                 geom_boxplot()       +
                 stat_summary(fun.y=mean, colour="darkred", geom="point",shape=18, size=3)+
                 labs(title = "Box plots by steps") +
                 #theme(legend.position = "top") 
                 theme_bw()
           },
           )
         })

#------------------------------------------
#--- METRICS - Error table
#------------------------------------------
#------------------------------------ 
#--- METRICS - FVA Table
#------------------------------------
output$ResultFVA <- renderTable({

# SWITCH    
  switch(input$ErrorMetric,
    MAPE     = { v.CastTabError <- dcast(MeltTabError(), Bucket~steps, value.var = "error")
                 errValues      <- apply(v.CastTabError[,-1], 2, mean, na.rm = TRUE)
    },
    Accuracy = { v.CastTabError <- dcast(MeltTabError(), Bucket~steps, value.var = "error")
                 errValues      <- apply(v.CastTabError[,-1], 2, mean, na.rm = TRUE)
    },
    GMRAE    = { v.CastTabError <- dcast(MeltTabError(), Bucket~steps, value.var = "error")
                 errValues      <- apply(v.CastTabError[,-1], 2, GMRAE, na.rm = TRUE)
    },
    MASE     = { errValues        <- unlist(MaseTabError()$mase_results[,4])
                 names(errValues) <- MaseTabError()$mase_results$steps
    #https://stackoverflow.com/questions/28680994/converting-rows-into-columns-and-columns-into-rows-using-r
    },
  )
# EndSWITCH
    
  DimErrTab      <- length(errValues)

  MatFva <- matrix(0, nrow= DimErrTab, ncol= DimErrTab)
  MatFva[,1] <- errValues
  for (i in 2:DimErrTab){
    for (j in 2:DimErrTab){
      if (i >= j) {
        switch(input$ErrorMetric,
               Accuracy  = { MatFva[i,j] <- MatFva[i,1] - MatFva[j-1,1]
               },
               MAPE      = { MatFva[i,j] <- MatFva[j-1,1] - MatFva[i,1]
               },
               GMRAE     = { MatFva[i,j] <- MatFva[j-1,1] - MatFva[i,1]
               },
               MASE     = { MatFva[i,j] <- (MatFva[j-1,1] - MatFva[i,1]) / max(MatFva[j-1,1],MatFva[i,1])
               }
        ) #EndSwitch
            } #EndIf
      }
  }
  
  MatFva[MatFva != 0]  <- paste0(round((100*MatFva[MatFva != 0]),1),"%")
  MatFva[MatFva == 0]  <- "-"
  if (input$ErrorMetric == "MASE"){
    MatFva[,1] <- format(errValues, digits=2, nsmall=3)
  }
  
  TabFva   <- data.frame(as.data.frame(names(errValues)), as.data.frame(MatFva))
  names(TabFva) <- c("Forecast Steps",  input$ErrorMetric,paste("FVA vs", names(errValues[-DimErrTab])))
  return(TabFva)
})

#------------------------------------
 output$ExplainFVA <- renderText({
    treated <- FALSE
    switch( input$ErrorMetric,
           MASE = {  errVal    <- MaseTabError()$mase_results[,4]
                    names(errVal) <- MaseTabError()$mase_results$steps
           },
           
           GMRAE    = {
             v.CastTabError <- dcast(MeltTabError(), Bucket~steps, value.var = "error")
             errVal      <- apply(v.CastTabError[,-1], 2, mean, na.rm = TRUE)
           },
           
           Accuracy = {
             v.CastTabError <- dcast(MeltTabError(), Bucket~steps, value.var = "error")
             errVal      <- apply(v.CastTabError[,-1], 2, mean, na.rm = TRUE)
           },
          
           MAPE    = {
           v.CastTabError <- dcast(MeltTabError(), Bucket~steps, value.var = "error")
           errVal      <- apply(v.CastTabError[,-1], 2, mean, na.rm = TRUE)
                    },
           )
    
   if(input$ErrorMetric == "Accuracy"){
     
     #  Naive forecast is the worst
     if (abs(min(errVal)) == abs(errVal["Naive"]))
     {
       treated <- TRUE
       outText     <- "You run an efficient forecast process!
              Any step adds value compare to the naive forecast."
     }
     
     #  Naive forecast is the best
     if (abs(max(errVal)) == abs(errVal["Naive"]))
     {
       treated <- TRUE
       outText     <- "You might have to review your process!
              As the naive forecast outperforms your process."
     }
     # 
     # #   Else...
     if (treated == FALSE)
     {
       badSteps  <- names(errVal[abs(errVal) < abs(errVal["Naive"])])
       outSteps  <- apply(data.frame(badSteps), 2 , paste, collapse = ", ")  
       outText   <- paste(outSteps, " forecast(s) should be analysed and improved to add value to the process. " )
     }     
     
     
   } else {  # Error metric is not forecast accuracy
        
    
#  Naive forecast is the worst
   if (abs(max(errVal)) == abs(errVal["Naive"]))
   {
   treated <- TRUE
   outText     <- "You run an efficient forecast process!
              Any step adds value compare to the naive forecast."
   }

#  Naive forecast is the best
   if (abs(min(errVal)) == abs(errVal["Naive"]))
   {
   treated <- TRUE
   outText     <- "You might have to review your process!
              As the naive forecast outperforms your process."
   }
# 
# #   Else...
  if (treated == FALSE)
   {
   badSteps  <- names(errVal[abs(errVal) > abs(errVal["Naive"])])
   outSteps  <- apply(data.frame(badSteps), 2 , paste, collapse = ", ")  
   outText   <- paste(outSteps, " forecast(s) should be analysed and improved to add value to the process. " )
  }
     
   } #EndElse
     
result <- outText
   })
#------------------------------------ 
#--- METRICS - Error Content - Graphic
#------------------------------------

 output$ContentError <- renderDygraph ({
  v.CastTabError <- dcast(MeltTabError(),  Bucket~steps, value.var = "error")
  TabError <- xts(v.CastTabError[,-1],
                      order.by = ymd(v.CastTabError[,1]))

    dygraph(TabError, main = paste(input$ErrorMetric)) %>%
    dyHighlight(highlightCircleSize = 2,
                highlightSeriesBackgroundAlpha = 0.4,
                hideOnMouseOut = TRUE) %>%
    dyLegend(show = "follow", width = 200, labelsSeparateLines= TRUE)
})

#--------------
#--- MASE
#-------------
output$ContentMASE <- renderPlot ({
    ggplot(MaseTabError()$mase_results, aes(x=steps,y=.estimate, fill=steps)) +
      geom_bar(stat = "identity", position = position_dodge())   +
      geom_text(aes(label=format(.estimate,digits=2, nsmall=3 )), vjust=1.6, color="white", size=3.5) +
      labs(title ="MASE" , x = "Steps", y = "MASE" ) +
      theme(plot.title   = element_text(size=18, face="bold" , hjust=0.5),
            axis.title.x = element_text(size = 14),
            axis.title.y = element_text(size = 14),
            legend.position="top")
})

#$$$ Test
output$MaseResults <- renderTable({
  #--- MaseTabError  
  # MaseTabLev1  <- MaseTabError()$mase_results
  # NewMase  <- reshape2::dcast(MaseTabError()$mase_reslev1, Level1~steps, value.var = ".estimate")
  # NewMase  <- bind_cols(NewMase, diff = (NewMase[input$StepB] - NewMase[input$StepA]))
  # return(NewMase)
  
  #--- MeltTabError    
  #return(MeltTabError())
  
  #--- dcast(MeltTabError)  
   v.MeltTabError <- subset(MeltTabError(), (steps %in% c("Naive", input$StepC)))
  # v.CastTabError <- na.omit(dcast(v.MeltTabError, Bucket~steps, value.var = "error"))
  # pwr <- t.test( v.CastTabError[["Naive"]], v.CastTabError[[input$StepC]])
  # data_frame(pwr$statistic, pwr$p.value, pwr$conf.int)
  # pwr.d      <- (mean(v.CastTabError[[input$StepC]]) - mean(v.CastTabError[["Naive"]])) / sqrt((var(v.CastTabError[["Naive"]],na.rm=TRUE) + var(v.CastTabError[[input$StepC]],na.rm=TRUE)) / 2)
  # power  <- pwr.t.test(n= length(v.CastTabError), d = pwr.d , sig.level = 0.05)
  # res    <- data.frame(t =pwr$statistic, pval =pwr$p.value, cohen = as.double(pwr.d), power = power$power, int = pwr$conf.int)
  
  #v.CastTabError <- dcast(MeltTabError(), Bucket~steps, value.var = "error")
  #return(v.CastTabError)
  
})
#$$$ End Test

####------------------------------------####
#- FVA - ANALYSIS - Main Panel 
####------------------------------------####


 #------------------------------------ 
 #--- ANALYSIS - Steps selection
 #------------------------------------ 
output$StepA <- renderUI ({
  FcstSteps <- FileInfo()$Steps
  selectInput("StepA", "Select step:",
              c("Naive", FcstSteps), selected = "Naive")
})
output$StepB <- renderUI ({
  FcstSteps  <- FileInfo()$Steps
  selectInput("StepB", "Select step:",
              c("Naive", FcstSteps), selected = "Naive") #FcstSteps()[1])
})

output$StepC <- renderUI ({
  FcstSteps <-  FileInfo()$Steps
  selectInput("StepC", "Select step:",
               c("Naive", FcstSteps), selected = "Naive")
})

output$StepD <- renderUI ({
  FcstSteps <-  FileInfo()$Steps
  selectInput("StepD", "Select step:",
              c("Naive", FcstSteps), selected = "Naive")
})

#------------------------------------ 
#--- ANALYSIS - Error Graphs
#------------------------------------ 
output$CompareError <- renderDygraph ({
   v.MeltTabError <- subset(MeltTabError(), (steps %in% c(input$StepA, input$StepB)))
   v.CastTabError <- dcast(v.MeltTabError,  Bucket~steps, value.var = "error")
   TabError <- xts(v.CastTabError[,-1],
                   order.by = ymd(v.CastTabError[,1]))

   #--- V2: Manage title trough Observe with user Title 
   if (input$StepA != input$StepB){
     title <- paste(input$ErrorMetric,":", input$StepA ,"vs", input$StepB)
   } else {
     title <- paste(input$ErrorMetric,":", input$StepA)
   }
   dygraph(TabError, main = title, group = "AnalysisFVA") %>%
     dyHighlight(highlightCircleSize = 2,
                 highlightSeriesBackgroundAlpha = 0.4,
                 hideOnMouseOut = TRUE) %>%
     dyLegend(width = 200, labelsSeparateLines= TRUE) %>%
     dyRangeSelector(height = 20, strokeColor = "")   %>%
     dyCrosshair(direction = "vertical")
 })

#--- MASE vs Location
output$CompareMASE1 <- renderPlot ({
    ggplot(MaseTabError()$mase_reslev1, aes(x=steps,y=.estimate, fill= Level1)) +
      geom_bar(stat = "identity", position = position_dodge())   +
      scale_fill_discrete(name= FileInfo()$NameCrit2, guide = guide_legend(reverse = FALSE)) +
      geom_text(aes(label=format(.estimate,digits=2, nsmall=3 )), vjust=1.6, color="white",
                position = position_dodge(0.9),size=3.5)         +
      labs(title ="MASE", x = "Forecast Steps", y = "MASE" ) +
      theme(plot.title   = element_text(size=18, face="bold" , hjust=0.5),
            axis.title.x = element_text(size = 14),
            axis.title.y = element_text(size = 14),
            legend.position="right")
})

#--- Explain Error
output$ExplainError <- renderUI({
 if (input$StepA != input$StepB){
   if (input$ErrorMetric == "MASE"){
     MaseTabLev1  <- MaseTabError()$mase_reslev1
     MaseTabLev1  <- reshape2::dcast(MaseTabError()$mase_reslev1, Level1~steps, value.var = ".estimate")
     MaseTabLev1  <- bind_cols(MaseTabLev1, diff = (MaseTabLev1[input$StepB] - MaseTabLev1[input$StepA]))
     nbin1        <- sum(MaseTabLev1[length(MaseTabLev1)] < 0)
     outof1       <- length(unique(MaseTabLev1$Level1))
     percent1     <- (nbin1 / outof1) * 100
     percent1     <- format(percent1, digits=2, nsmall=1)
     
     MaseTabLev2  <- MaseTabError()$mase_reslev2
     MaseTabLev2  <- reshape2::dcast(MaseTabError()$mase_reslev2, Level2~steps, value.var = ".estimate")
     MaseTabLev2  <- bind_cols(MaseTabLev2, diff = (MaseTabLev2[input$StepB] - MaseTabLev2[input$StepA]))
     nbin2        <- sum(MaseTabLev2[length(MaseTabLev2)] < 0)
     outof2       <- length(unique(MaseTabLev2$Level2))
     percent2     <- (nbin2 / outof2) * 100
     percent2     <- format(percent2, digits=2, nsmall=1)
     
     res1  <- paste("The", input$StepB, "forecast adds more value than the ",
                      input$StepA,"forecast for", nbin1, " out of ", outof1, FileInfo()$NameCrit2,
                      "(", percent1, "%", ").") 
     res2 <-  paste("The", input$StepB, "forecast adds more value than the ",
                      input$StepA,"forecast for", nbin2, " out of ", outof2, FileInfo()$NameCrit1,
                      "(", percent2, "%", ").")
     result <- HTML(paste(res1,res2, sep ='<br/>'))
     
   } else {
     
  v.MeltTabError <- subset(MeltTabError(), (steps %in% c(input$StepA, input$StepB)))
  v.CastTabError <- dcast(v.MeltTabError,  Bucket~steps, value.var = "error")
  fva     <- v.CastTabError[input$StepB] - v.CastTabError[input$StepA]
  BvsA    <- length(fva[fva > 0]) / nrow(fva)
  percent <- paste0(round((100*BvsA),1),"%")
  result  <- paste("The", input$StepA, "forecast adds more value than the ",
                   input$StepB,"forecast",percent, "of the time.")
   } # EndIf (input$ErrorMetric)
 }

})


#--- RESULT - FVA Graph
output$AnalysFVA <- renderDygraph ({
  
  v.MeltTabError <- subset(MeltTabError(), (steps %in% c(input$StepA, input$StepB)))
  v.CastTabError <- dcast(v.MeltTabError,  Bucket ~ steps, value.var = "error")
  v.TabFVA       <- bind_cols(v.CastTabError["Bucket"],
                              FVA = v.CastTabError[input$StepB] - v.CastTabError[input$StepA])
  TabFVA         <- xts(v.TabFVA[,-1],
                  order.by = ymd(v.TabFVA[,1]))

  #--- V2: Manage title trough Observe with user Title 
  if (input$StepA != input$StepB){
    title <- paste("FVA:", input$StepA ,"vs", input$StepB)
  } else {
    title <- paste("FVA:", input$StepA)
  }  
  dygraph(TabFVA, main = title, group = "AnalysisFVA") %>%
    dyHighlight(highlightCircleSize = 2,
                highlightSeriesBackgroundAlpha = 0.4,
                hideOnMouseOut = TRUE) %>%
    dyLimit(0, color = "black") %>%
    dyLegend(width = 200, labelsSeparateLines= TRUE) %>%
    dyRangeSelector(height = 20, strokeColor = "") %>%
    dyCrosshair(direction = "vertical")

}) 

#--- MASE vs Product
output$CompareMASE2 <- renderPlot ({
  ggplot(MaseTabError()$mase_reslev2, aes(x=steps,y=.estimate, fill= Level2)) +
      geom_bar(stat = "identity", position = position_dodge())   +
      scale_fill_discrete(name= FileInfo()$NameCrit1, guide = guide_legend(reverse = FALSE)) +
      geom_text(aes(label=format(.estimate,digits=2, nsmall=3 )), vjust=1.6, color="white",
                position = position_dodge(0.9),size=3.5)         +
      labs(title ="MASE", x = "Forecast Steps", y = "MASE" ) +
      theme(plot.title   = element_text(size=18, face="bold" , hjust=0.5),
            axis.title.x = element_text(size = 14),
            axis.title.y = element_text(size = 14),
            legend.position="right")
})


####------------------------------------####
#- FVA - INFERENCES - Main Panel 
####------------------------------------####

output$InfHist <- renderPlot({
  v.MeltTabError <- subset(MeltTabError(), (steps == input$StepC))
  
 ggplot(v.MeltTabError,aes(x=error)) +
   geom_histogram(bins=nclass.Sturges(v.MeltTabError$error),color="white",fill="seagreen1",aes(y=..density..),lwd=0.8) +
   geom_density(color="seagreen4", alpha=0.3,fill="seagreen4",lty=1)+
   labs(title = paste(input$StepC, "\n histogram"),x="Error",y="Density")   +
   theme(plot.title = element_text(color="navy", size=15, face="bold.italic",hjust=0.5),
       axis.title.x = element_text(color="navy", size=13, face="bold"),
       axis.title.y = element_text(color="navy", size=13, face="bold"))
})

output$InfBplot <- renderPlot({
  v.MeltTabError <- subset(MeltTabError(), (steps == input$StepC))
  ggplot(NULL,aes(x=0,y=v.MeltTabError$error))+geom_boxplot(color="black",fill="skyblue",alpha=0.5)+ 
    stat_summary(fun.y=mean, colour="darkred", geom="point",shape=18, size=3)+
    labs(title = paste(input$StepC, "\n boxplot"),x="",y="Error")+
    theme(plot.title = element_text(color="navy", size=15, face="bold.italic",hjust=0.5),
          axis.title.x = element_text(),
          axis.text.x  = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.y = element_text(color="navy", size=13, face="bold"))
})


 output$InfQqplot <- renderPlot({
   v.MeltTabError <- subset(MeltTabError(), (steps == input$StepC))
   par(font.main=4,font.lab=2,col.main="navy",col.lab="navy",cex.lab=1.1)
   u <-par("usr")
   rect(u[1], u[3], u[2], u[4], col="#EBE9E9", border=TRUE)
   grid(NULL,NULL,col="white",lty=1)
   par(new=TRUE)
   qqnorm(v.MeltTabError$error,col="coral",pch=16,id=T,lwd=1.9,col.lines = "black",grid = F, main = paste(input$StepC, "\n Q-Q plot"),xlab="Normal quantiles",ylab="Error")
   qqline(v.MeltTabError$error)
 #box(col="white")
 })
 
 
 output$ExplainInference <- renderUI({
   
   v.MeltTabError <- subset(MeltTabError(), (steps %in% c("Naive", input$StepC)))
   v.CastTabError <- na.omit(dcast(v.MeltTabError, Bucket~steps, value.var = "error"))
   pwr.t  <- t.test( v.CastTabError[["Naive"]], v.CastTabError[[input$StepC]], conf.level = 0.95)
   pwr.d  <- (mean(v.CastTabError[[input$StepC]]) - mean(v.CastTabError[["Naive"]])) / sqrt((var(v.CastTabError[["Naive"]],na.rm=TRUE) + var(v.CastTabError[[input$StepC]],na.rm=TRUE)) / 2)
   power  <- pwr.t.test(n= length(v.CastTabError), d = pwr.d , sig.level = 0.05)
   #power  <- pwr.t.test(n= length(v.CastTabError), d = pwr.d , sig.level = c(0.05,0.1))
   
   res1 <- paste("Tstat = ", format(pwr.t$statistic, digits=2, nsmall = 3))
   res2 <- paste("pval  = ", format(pwr.t$p.value,   digits=2, nsmall = 3 ))
   
   ConfLevel <- paste0(round(100*attributes(pwr.t$conf.int)$conf.level,1),"%")
   res3 <- paste("Confidence interval (", ConfLevel , ")"," = ", 
                 format(pwr.t$conf.int[[1]],  digits=2, nsmall = 3), " to ", format(pwr.t$conf.int[[2]],  digits=2, nsmall = 3))
   res4 <- paste ("Power = ", paste0(round(100*power$power,1),"%"))
   HTML(paste(res1, res2, res3, res4, sep ='<br/>'))

})
   
 ####------------------------------------####
 #- FVA - Application                                     mainPanel
 ####------------------------------------####
 
  output$ImageHelp1 <- renderImage({
    return(list(
      src = "Images/FVAProcess.jpg",
      contentType = "image/jpg",
      alt = "Help1"
    ))
  }, deleteFile = FALSE)

  output$ImageHelp2 <- renderImage({
    return(list(
      src = "Images/OutputFVA.png",
      contentType = "image/png",
      alt = "Help2"
    ))
  }, deleteFile = FALSE)
  
  
}

####------------------------------------####
#- Forecast -                                     sideBar
####------------------------------------####
#---- Inputs
#------------------------------------------#