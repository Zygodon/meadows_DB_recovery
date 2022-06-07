# MEADOWS DB RECOVERY APP version 2_1
# J B Pilkington 2022-06-1
library(shiny)
library(DT)
library(xlsx)

source ("DB_recovery_code.R")

values <- reactiveValues()
values$srvid <- 0

# Define UI ----
ui <- fluidPage(
    titlePanel("MEADOWS DATABASE RECOVERY APP v2_1"),
    sidebarLayout(
        sidebarPanel(
            helpText("Each of the original speadsheets represents a single survey of a vegetative assembly. Define
                     which one you want by selecting a survey in the lower box. You can refine the
                     search by choosing a specific year in the upper box"),
            selectInput(inputId = "year",
                label = "Choose year surveyed",
                choices = unlist(SurveyYears()),
                selected = "All"),
            selectInput(inputId = "survey",
                label = "Select the survey to recover",
                choices = survey_choices$Namestring),
            downloadButton("dl", "Export table to Excel")# ,
        ),
            
        # Main panel for displaying outputs; just has the table in it.
        mainPanel("", # "main panel"
                  fluidRow(style = "border: 2px solid mediumseagreen; background-color: grey;
                           margin-right: 10px; border-radius: 5px",
                      column(5, "Source", style = "background-color: #DCDCDC; border-radius: 5px; margin-right: 5px;
                             margin-left: 20px", 
                             textOutput("file")),
                      column(2, "Quadrat Size", style = "background-color: #DCDCDC; border-radius: 5px; margin-right: 5px;", 
                             textOutput("quadrat_size")),
                      column(2, "Grid Ref", style = "background-color: #DCDCDC; border-radius: 5px; margin-right: 5px;", 
                             textOutput("grid_ref")),
                      column(2, "NVC", style = "background-color: #DCDCDC; border-radius: 5px; margin-right: 5px;", 
                             textOutput("community"))
                  ),
                  fluidRow(tags$br()),
                  shinycssloaders::withSpinner(DT::dataTableOutput("table"))
            )
        )
    )   

# Define server logic ----
server <- function(input, output, clientData, session){
    observeEvent(input$year, {
        # Change values for input$survey
        survey_choices <- surveyList(input$year)
        updateSelectInput(session, "survey",
            choices = survey_choices$Namestring)
    })
    
    observeEvent(input$survey,{
        # Update srvid, survey_data and other outputs 
        # that don't refer directly to srvid
        values$srvid <- survey_choices$surveys_id[which(survey_choices$Namestring %in% input$survey)]
        survey_data <- surveyData(values$srvid)
        output$file <- renderText(survey_data$source_file)
        output$quadrat_size <- renderText(survey_data$quadrat_size)
        output$grid_ref <- renderText(survey_data$grid_ref)
        output$community <- renderText(survey_data$community)
    })
    output$table <- DT::renderDataTable(surveyTable(values$srvid))
    
    output$dl <- downloadHandler(
        filename = function()
        {
            # srvid <- unlist(strsplit(as.character(input$surveys), ":"))[1]
            srv_data <- surveyData(values$srvid)
            fName <- unlist(strsplit(srv_data$source_file, "\\."))[1]
            fName <- paste(fName, "_recovered.xlsx", sep = "")
        },
        content = function(file)
        {
            srv_table <- surveyTable(values$srvid)
            srv_data <- surveyData(values$srvid)

            wb<-createWorkbook(type="xlsx")
            TITLE_STYLE <- CellStyle(wb)+ Font(wb,  heightInPoints=11,
                                               color="blue", isBold=TRUE)
            sheet <- createSheet(wb, sheetName = "Recovered data")
            # addTitle(sheet, rowIndex=1, title=fName,titleStyle = TITLE_STYLE)
            addDataFrame(srv_table, sheet, startRow=3, startColumn=1)
            saveWorkbook(wb, file)
        }
    )
    
}

# Run the app ----
shinyApp(ui = ui, server = server)