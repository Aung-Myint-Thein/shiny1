 library(shiny)
library(RMySQL)
flag<-0
MySQL.<-MySQL()
con=dbConnect(MySQL.,user='root', password='insead',dbname='insead')

dbSendQuery(con,"update scores set num=0 where id=1985")

# Define server logic for random distribution application
shinyServer(function(input, output) {
  myscore1<-reactive({
    if (input$b1==0)
      return()
    else
    {
      dbSendQuery(con,"update scores set num=num-1 where id=1985")
      dbGetQuery(con,"select num from scores where id=1985")}
    
    
  })
  myscore2<-reactive({
    if (input$b2==0)
      return()
    else
    {
      dbSendQuery(con,"update scores set num=num+1 where id=1985")
      dbGetQuery(con,"select num from scores where id=1985")}
    
    
  })
  output$score1<-renderTable({
    
    myscore1()
    
  })
  output$score2<-renderTable({
    
    myscore2()
    
  })
  
  output$fq2<-renderTable({
    
    fq2()
  })
  output$fq3<-renderTable({
    
    
    fq3()
  })
  choicesInput<-reactive({
    input$choices
    
  })  
  output$caption<-renderText({
    choicesInput()
  })
  
  output$downloadPDF <-
    downloadHandler(filename = "report.pdf",
                    content = function(file)
                    {
                      
                      #texi2pdf("report.tex")
                      #knit2pdf("report.Rnw",output="report.pdf")
                      file.copy("report.pdf", file)
                    }
                    ,
                    contentType = "application/pdf"
    )
  
  
  
  observe({
    
    ss<-dbGetQuery(con,"select num from scores where id=1985")
    if (as.integer(ss)<=1)
      dbGetQuery(con, "select results from reporting")
  })
  observe({
    ss<-dbGetQuery(con,"select num from scores where id=1985")
    if (as.integer(ss)==2)
      dbGetQuery(con, "select results2 from reporting")
  })
  observe({
    ss<-dbGetQuery(con,"select num from scores where id=1985")
    if (as.integer(ss)>2)
      dbGetQuery(con, "select result3 from reporting")
  })
  
  
  output$r3<-renderTable({
    reporting3()
  })
  
  
  output$reporting<-renderTable({
    dbGetQuery(con,"select * from reporting")
  })
  output$squestions<-renderTable({
    dbGetQuery(con,"select * from small")
    
  })
  output$mquestions<-renderTable({
    dbGetQuery(con,"select * from medium")
    
  })
  output$lquestions<-renderTable({
    dbGetQuery(con,"select * from large")
    
  })
  
  observe({
    if (input$b1 == 0)
      return()
    
    ifoption1<-reactive({
      
      id<-sample(1:3,1,replace=T) 
      query<-paste("select option1,option2 from medium where id='",id,"'",sep="")
      dbGetQuery(con, query)
    })
    output$qq1<-renderTable({
      ifoption1()
      
    })
    output$nui<-renderUI({
      
      tableOutput("qq1")
    })
    output$noption<-renderUI({
      helpText("in the previous question you have selected option1")
      
      
    })
    output$nsb<-renderUI({
      submitButton("calculate and next question")
      
      
    })
  })
  
  observe({
    if (input$b2 == 0)
      return()
    
    ifoption2<-reactive({
      id<-sample(1:3,1,replace=T) 
      query<-paste("select option1,option2 from large where id='",id,"'",sep="")
      dbGetQuery(con, query)})
    output$qq2<-renderTable({
      ifoption2()
      
    })
    output$nui<-renderUI({
      
      tableOutput("qq2")
      
    })
    output$noption<-renderUI({
      helpText("in the previous question you have selected option2")
      
      
    })
    output$nsb<-renderUI({
      submitButton("calculate and next question")
      
      
    })
  })
  



  # Reactive expression to generate the requested distribution. This is 
  # called whenever the inputs change. The output renderers defined 
  # below then all used the value computed from this expression
  data <- reactive({  
    dist <- switch(input$dist,
                   norm = rnorm,
                   unif = runif,
                   lnorm = rlnorm,
                   exp = rexp,
                   rnorm)

    dist(input$n)
  })

  # Generate a plot of the data. Also uses the inputs to build the 
  # plot label. Note that the dependencies on both the inputs and
  # the data reactive expression are both tracked, and all expressions 
  # are called in the sequence implied by the dependency graph
  output$plot <- renderPlot({
    dist <- input$dist
    n <- input$n

    hist(data(), 
         main=paste('r', dist, '(', n, ')', sep=''))
  })

  # Generate a summary of the data
  output$summary <- renderPrint({
    summary(data())
  })

  # Generate an HTML table view of the data
  output$table <- renderTable({
    data.frame(x=data())
  })
})
