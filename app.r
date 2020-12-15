library(shiny)
library(shinydashboard)
library(infoPack)

dummydata <- data.frame(
    cancerSite = rep("Lung",36)
    ,reportingMonth = seq.Date(from = as.Date("2017-01-01"), by = "1 month",length.out = 36)
    ,performancePercentage = c(
        0.84,0.86,0.874,0.791,0.691,0.756,0.808,0.82,0.822,0.861,0.905,0.911
        ,0.923,0.878,0.923,0.659,0.818,0.81,0.814,0.884,0.867,0.895,0.911,0.934
        ,0.941,0.901,0.926,0.934,0.894,0.902,0.878,0.845,0.846,0.819,0.80,0.759
    )
    ,target = rep(0.93,36)
)

optionsList <- spcOptions(
    improvementDirection = "increase"
    ,mainTitle = "Lung Cancer Two-Week-Wait Performance"
    ,yAxisLabel = "Performance %"
    ,xAxisLabel = "Reporting Month"
    ,percentageYAxis = TRUE
    ,target = target
    ,yAxisBreaks = 0.02
)

ui <- dashboardPage(
    header = dashboardHeader(title = "Example Performance Dashboard",titleWidth = "400px")
    ,sidebar = dashboardSidebar(disable = TRUE)
    ,body = dashboardBody(
        plotOutput("spc1",click = "spcInput1")
        ,actionButton("clear","Clear Comments")
    )
    ,title = "Example Performance Dashboard - Two-Way BI"
    ,skin = "black"
)

server <- function(input, output) {
    
    datasets <- reactiveValues(dummydata = dummydata
                               ,commentStack = runSQL("SELECT X x, Y y, Comment comment FROM BI_REPORTING.Admin.Example2WayBIComments")
                               )
    
    output$spc1 <- renderPlot({
        plot <- spc(datasets$dummydata,performancePercentage,reportingMonth,options = optionsList)
        
        if(nrow(datasets$commentStack) > 0){
            for(i in 1:nrow(datasets$commentStack)){
                comment <- datasets$commentStack[i,]
                plot <- plot + 
                    annotate(geom = "text",x = comment$x,y=comment$y-0.01,label = comment$comment)
            }
        }
        plot
    })
    
    observeEvent(input$spcInput1,{
        points <- spc(datasets$dummydata,performancePercentage,reportingMonth,options = spcOptions(outputChart = FALSE))
        
        selectedPoint <- nearPoints(points,input$spcInput1,xvar = "x", yvar = "y")
        
        if(nrow(selectedPoint) > 0){
            showModal(
                modalDialog(
                    title = "Add Comment"
                    ,h4(paste0("Selected point data:"))
                    ,h5(paste0("X Coordinate: ",format(selectedPoint$x,"%b %Y")))
                    ,h5(paste0("Y Coordinate: ",scales::percent(selectedPoint$y)))
                    ,textInput("newComment","Comment to add")
                    ,actionButton("addNewComment","Add New Comment")
                )
            )
        }
    })
    
    observeEvent(input$addNewComment,{
        points <- spc(datasets$dummydata,performancePercentage,reportingMonth,options = spcOptions(outputChart = FALSE))
        
        selectedPoint <- nearPoints(points,input$spcInput1,xvar = "x", yvar = "y")
        
        datasets$commentStack <- rbind(
            datasets$commentStack
            ,data.frame(
                x = selectedPoint$x
                ,y = selectedPoint$y
                ,comment = input$newComment
            )
        )
        
        runSQL.params("INSERT INTO BI_REPORTING.Admin.Example2WayBIComments(X,Y,Comment) SELECT ?x, ?y, ?comment"
                      ,params = list(x = gsub(" UTC","",selectedPoint$x), y = selectedPoint$y, comment = input$newComment)
                      )
        
        removeModal()
    })
    
    observeEvent(input$clear,{
        runSQL("TRUNCATE TABLE BI_REPORTING.Admin.Example2WayBIComments")
        datasets$commentStack <- runSQL("SELECT X x, Y y, Comment comment FROM BI_REPORTING.Admin.Example2WayBIComments")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
