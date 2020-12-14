library(shiny)
library(mcr)
library(shinydashboard)
library(rhandsontable)
library(rmarkdown)
library(readxl)




shinyServer(function(input, output, session) {
  
  
  vals <- reactiveValues(hot_data = data.frame('X'= round(c(rep(NA, 10)), digits = 2),
                                               'Y'= round(c(rep(NA, 10)), digits = 2)), 
                         final_data = data.frame('X'= round(c(rep(NA, 10)), digits = 2),
                                                 'Y'= round(c(rep(NA, 10)), digits = 2)))
  
  observeEvent(input$hot,{
    vals$final_data <- hot_to_r(input$hot)
    }
  )
  
  
  datasetInput <- eventReactive(vals$hot_data,{
    vals$hot_data
    
  })
  
  observeEvent(input$file,{
    vals$final_data <- read_excel(input$file$datapath)

  })
  
  output$plot1 <- renderPlot({
    
    a <- vals$final_data
    if (is.null(a)) {
      return(NULL)} else {
        names(a) <- c('M1', 'M2')
        data1 <- try(mcreg(a$M1, a$M2,
                       mref.name = input$m1, mtest.name = input$m2, 
                       na.rm = TRUE), silent = TRUE)
        try(MCResult.plotDifference(data1, plot.type = input$batype,
                                add.grid = TRUE), silent = TRUE)
        
      }
    
  })
  
  output$plot2 <- renderPlot({
    
    a <- vals$final_data
    if (is.null(a)) {
      return(NULL)} else {
        names(a) <- c("M1", "M2")
        data1 <- try(mcreg(a$M1, a$M2, error.ratio = input$syx, 
                       method.reg = input$regmodel, method.ci = input$cimethod,
                       method.bootstrap.ci = input$metbootci, 
                       slope.measure = "radian", na.rm = TRUE), silent = TRUE)
        try(MCResult.plot(data1, ci.area = input$ciarea,
                      add.legend = input$legend, identity = input$identity,
                      add.cor = input$addcor, x.lab = input$m1,
                      y.lab = input$m2, cor.method = input$cormet,
                      equal.axis = TRUE, add.grid = TRUE, 
                      na.rm = TRUE), silent = TRUE)
        
      }
    
  })

  output$plot3 <- renderPlot({
    
    a <- vals$final_data
    if (is.null(a)) {
      return(NULL)} else {
        names(a) <- c("M1", "M2")
        data1 <- try(mcreg(a$M1, a$M2, error.ratio = input$syx, 
                       method.reg = input$regmodel, method.ci = input$cimethod,
                       method.bootstrap.ci = input$metbootci, slope.measure = "radian",
                       mref.name = input$m1, mtest.name = input$m2, 
                       na.rm = TRUE), silent = TRUE)
        try(compareFit(data1), silent = TRUE)
        
      }
    
  })  
  
  
  output$summary <- renderPrint({
    
    a <- vals$final_data
    if (is.null(a)) {
      return(NULL)} else {
        names(a) <- c("M1", "M2")
        data1 <- try(mcreg(a$M1, a$M2, error.ratio = input$syx, 
                       method.reg = input$regmodel, method.ci = input$cimethod,
                       method.bootstrap.ci = input$metbootci, slope.measure = "radian",
                       mref.name = input$m1, mtest.name = input$m2, 
                       na.rm = TRUE), silent = TRUE)
        try(printSummary(data1), silent = TRUE)
      }
    
  })
  
  output$hot <- renderRHandsontable({
    a <- vals$final_data
    rhandsontable(a, height = 482) %>%
       hot_col(col = colnames(a)[1], format = '0.00', type = 'numeric') %>%
       hot_col(col = colnames(a)[2], format = '0.00', type = 'numeric')  
      

  })
  
  output$downloadReport <- downloadHandler(
    filename = function() {
      paste(paste(input$m1,'vs.',input$m2, '@', Sys.Date()), sep = '.', switch(
        input$format, PDF = 'pdf', HTML = 'html'
      ))
    },
    content = function(file) {
      
      params <- list(
        data = vals$final_data,
        m1 = input$m1,
        m2 = input$m2,
        syx = input$syx,
        regmodel = input$regmodel,
        cimethod = input$cimethod,
        metbootci = input$metbootci,
        batype = input$batype,
        ciarea = input$ciarea,
        legend = input$legend,
        identity = input$identity,
        addcor = input$addcor,
        cormet = input$cormet,
        name = input$name
      )
      
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)
    
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        switch(
                          input$format,
                          PDF = pdf_document(), HTML = html_document(), Word = word_document()
                        ),
                        envir = new.env(parent = globalenv())
      )
      
      
      # src <- normalizePath('report.Rmd')
      # owd <- setwd(tempdir())
      # on.exit(setwd(owd))
      # file.copy(src, 'report.Rmd')
      # out <- rmarkdown::render('report.Rmd',
      #                          params = params,
      #                          switch(
      #   input$format,
      #   PDF = pdf_document(), HTML = html_document(), Word = word_document()
      # ))
      # file.rename(out, file)
      
    }
    
  )
  
})
