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

source("CV_Builder.R")

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
                "Image Query",
                tabName = "item2",
                icon = icon("file")
            )
        )
    ),
    dashboardBody(
        tabItems(
            tabItem(
                tabName = "item1",
                box(
                    title = "Overview",
                    h3("Welcome to the microplastic taxonomy page, this is a place to improve your use of visual microscopy in microplastic identification. Go to the image query tab to get started querying our database of microplastic images by color, morphology, and polymer types."),
                    width = 12
                ),
                box(
                    title = "Contribute",
                    collapsed = T,
                    h3("You can help us build this database of microplastic imagery by filling out this form if you just have a few images to share:"),
                    HTML('<a class="btn btn-info" href = "https://forms.gle/kA4ynuHsbu7VWkZm7" role = "button" >Form</a>'),
                    h3("If you over 50 images to share, please contact wincowger@gmail.com to share a zip folder of images. All we need is a folder with images that have unique names and a spreadsheet that lists the name of the image and relevant metadata following the google form information."),
                    width = 12
                    
                )
            ),
            tabItem(
                tabName = "item2",
                fluidRow(
                    box(
                        textInput("sheet_link", "Google Sheet Link", "https://docs.google.com/spreadsheets/d/1E3_Z900RAWbRnThNNu-_DXQqZN6qHkD2wN7ZYGmKt34/edit?usp=sharing"),
                        actionButton("build_report", "Build CV", icon = icon("file")),
                        downloadButton("download_report", "Download Report", style = "background-color: #28a745;"),
                        width = 4
                    ), 
                    box(
                    uiOutput("pdf"),
                    width = 8
                    )
                )
            )
        )
    )
)



server <- function(input, output) {
    
    values <- reactiveVal()
    observeEvent(input$build_report, {
        if(!testsheet(input$sheet_link)){
            cv_builder(input$sheet_link)
            file.copy("AwesomeCV.pdf", "www/CV.pdf", overwrite = T)
            values(NULL)
            values(tags$iframe(style="height:600px; width:100%", src="CV.pdf"))
            
        }
        else{
            show_alert(
                title = "URL not supported!",
                text = paste0("URL link is not supported, url needs to be a google sheet share link for an open google sheet, please see the example link by reloading the page."),
                type = "error")
        }
        
    })
    
    output$pdf <-  renderUI({
        req(file.exists("www/CV.pdf"))
        values()
    })
    
    output$download_report <- downloadHandler( 
        filename = function() {"CV.pdf"},
        content = function(file){file.copy("www/CV.pdf", file)}
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
