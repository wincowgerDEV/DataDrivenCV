library(shiny)
library(vitae)
library(googlesheets4)
library(bs4Dash)
library(dplyr)
library(glue)
library(knitr)
library(rmarkdown)
library(tinytex)
library(digest)
library(shinyWidgets)
library(DT)

source("CV_Builder.R")

googlesheets4::gs4_deauth()

ui <- dashboardPage(
    dashboardHeader(title = "Data Driven CV"),
    dashboardSidebar(
        sidebarUserPanel(
            #image = "https://drive.google.com/file/d/13iCjC10dV3giFhCCoir_8mnbwtHM1rMA/view?usp=sharing",
            name = "Welcome!"
        ),
        
        sidebarMenu(
            id = "sidebarmenu",
            #sidebarHeader("Header 1"),
            menuItem(
                "About",
                tabName = "item1",
                icon = icon("sliders-h")
            ),
            menuItem(
                "CV Generator",
                tabName = "item2",
                icon = icon("file")
            )
        )
    ),
    dashboardBody(
        tags$head(
            #Ethical Ads
            HTML('<script async src="https://media.ethicalads.io/media/client/ethicalads.min.js"></script>')
        ),
        tabItems(
            tabItem(
                tabName = "item1",
                box(
                    title = "Overview",
                    p("Welcome to the Data Driven CV Portal. This tool allows you to use a Google Sheet to format and automatically create your professional CV. Credit to the Vitae package for doing most of the heavy lifting here. I just wrapped their package up in a web gui with a little bit of user experience and controlled logic in the sheet. To get started, go to the CV Generator tab on the left sidebar and look at the example CV."),
                    width = 12
                ),
                box(
                    title = "Contribute",
                    collapsed = T,
                    p("You can help us build this tool, it is fully open source:"),
                    HTML('<a class="btn btn-info" href = "https://github.com/wincowgerDEV/DataDrivenCV" role = "button" >Github</a>'),
                    width = 12
                ),
                box(
                    title = "Report Issue",
                    collapsed = T,
                    p("If you have any issues with a particular sheet you are using please create an issue here:"),
                    HTML('<a class="btn btn-info" href = "https://github.com/wincowgerDEV/DataDrivenCV/issues" role = "button" >Issues</a>'),
                    width = 12
                ), 
                HTML('<!-- Show a text ad -->
                         <div class = "dark raised" data-ea-publisher="openanalysisorg" data-ea-type="image" data-ea-style="stickybox"></div>')
            ),
            tabItem(
                tabName = "item2",
                fluidRow(
                    column(width = 4, 
                           box(
                               textInput("sheet_link", "Google Sheet Link", "https://docs.google.com/spreadsheets/d/1E3_Z900RAWbRnThNNu-_DXQqZN6qHkD2wN7ZYGmKt34/edit?usp=sharing"),
                               actionButton("build_report", "Build CV", icon = icon("file")),
                               downloadButton("download_report", "Download CV", style = "background-color: #28a745;"),
                               width = 4
                           ),
                           box(
                               DT::DTOutput(outputId = "table"),
                               width = 4
                           )
                           ), 
                    column(width = 8,
                           box(
                            uiOutput("pdf"),
                            width = 8
                            )
                           )
                ),
                fluidRow(HTML('<!-- Show a text ad -->
                         <div class = "dark raised" data-ea-publisher="openanalysisorg" data-ea-type="image" data-ea-style="stickybox"></div>'))
            )
        )
    )
)



server <- function(input, output) {
    
    values <- reactiveVal()
    observeEvent(input$build_report, {
        withProgress(message = 'CV Generation in Progress',
                     detail = 'This may take a minute...', min = 0, max = 4, value = 1, {
        if(!testsheet(input$sheet_link)){
            if(!all(c("Topic", "Subtopic", "StartMonth",	"StartYear", "EndMonth", "EndYear",	"SubSubtopic",	"Location",	"ShortDescription",	"LongDescription",	"Link") %in% colnames(read_sheet(input$sheet_link)))){
                show_alert(
                    title = "Format not supported!",
                    text = paste0("Column names must be formated as Topic, Subtopic, StartMonth, StartYear, EndMonth, EndYear, SubSubtopic, Location, ShortDescription, LongDescription, Link"),
                    type = "error")
            }
            else{
                
                             cv_builder(input$sheet_link)
                             incProgress(1/4)
                             file.copy("AwesomeCV/AwesomeCV.pdf", "www/CV.pdf", overwrite = T)
                             incProgress(2/4)
                             values(NULL)
                             incProgress(3/4)
                             values(tags$iframe(style="height:600px; width:100%", src="CV.pdf"))
                             incProgress(4/4)
                         
            }
        }
        else{
            show_alert(
                title = "URL not supported!",
                text = paste0("URL link is not supported, url needs to be a google sheet share link for an open google sheet, please see the example link by reloading the page."),
                type = "error")
            }
        })
        
    })
    
    output$pdf <-  renderUI({
        req(file.exists("www/CV.pdf"))
        values()
    })
    
    output$table <- renderDT({
        req(values())
        read_sheet(input$sheet_link)
        }
    )
    
    output$download_report <- downloadHandler( 
        filename = function() {"CV.pdf"},
        content = function(file){file.copy("www/CV.pdf", file)}
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
