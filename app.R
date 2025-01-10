library(shiny)
library(readxl)
library(writexl)
library(DT)

source("R/round_app.R")

# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("Round-app: Round Students' marks"),
  # add a banner
  tags$div(
    style = "background-color: #FFA500; color: black; padding: 10px; text-align: center; font-weight: bold; font-size: 16px; margin-bottom: 15px;",
    "Warning: For demonstration purposes only. Do not upload any personal (student) data!"
  ),
  # Add a description subpanel
  fluidRow(
    column(12, 
           h4("Function Description"),
           p("This application allows you to upload two Excel files containing students' marks."),
           p("1. The first file should contain the grades that need to be processed, with the format '<batch_n>_rep_cijferlijst.xlsx' "),
           p("2. The second file should contain the names and number of the students who registered for the exam"),
           p("with the format '<course_number>_<YYYYMMDD>' (for example, '500193-B-6_20241219')."),
           p("After uploading both files, the app will round the grades and provide a downloadable file."),
           p("You can download the resulting file with merged data and calculated final grades."),
           p("You can also download a check file, to directly compare the original and rounded grade.")
           
    )
  ),
  
  # First file input for uploading the Excel file
  fileInput("upload1", "file with the grades ('Resultaat') which need to be rounded.
            The name of the file usually has the format '<batch_n>_rep_cijferlijst.xlsx' (e.g., '9295_rep_cijferlijst').
            Before continuing, check in the table which will appear below if it is the right one. If not, please upload the corect one.",
            multiple = FALSE, accept = ".xlsx"),
  
  
  
  # Message shown after the first file is uploaded
  textOutput("message"),
  
  # Conditionally shown second file input (only after first file is uploaded)
  uiOutput("upload2UI"),
  
  # Conditionally render the first table or the second one
  uiOutput("tableUI"),
  
  # Conditionally render the download button
  uiOutput("downloadUI"),
  
  uiOutput("download2UI")
  
)

server <- function(input, output, session) {
  
  # Reactive expression to read the first file when uploaded
  data1 <- reactive({
    req(input$upload1)  # Make sure the first file is uploaded
    
    tryCatch({
      read_xlsx(input$upload1$datapath)  # Read the .xlsx file
    }, error = function(e) {
      NULL  # Return NULL if there's an error
    })
  })
  
  # Reactive expression for the second file (only if the first file is uploaded)
  data2 <- reactive({
    req(input$upload1)  # Ensure that the first file is uploaded before uploading the second one
    req(input$upload2)  # Ensure the second file is uploaded
    tryCatch({
      read_xlsx(input$upload2$datapath)  # Read the second .xlsx file
    }, error = function(e) {
      NULL  # Return NULL if there's an error
    })
  })
  
  # Conditionally render the second file input UI after the first file is uploaded
  output$upload2UI <- renderUI({
    req(input$upload1)  # Ensure the first file is uploaded before showing the second input
    
    # Show the second file input only after the first file is uploaded
    fileInput("upload2", "Upload the second file with the name and number of the students enrolled (e.g. '500193-B-6_20241219') \n
              check if it is fine in the table below after uploading it. If it is not the right one, please select the correct file.
              If you are sure that you uploaded the right files, scroll down and download the file with the rounded grades and the
              check file",
              multiple = FALSE, accept = ".xlsx")
    
    
  })
  
  # Render the message after uploading the first file
  output$message <- renderText({
    req(input$upload1)  # Only show the message once the first file is uploaded
    "Great! Now, upload the second file with the grades."
  })
  
  # Conditionally render the tables
  output$tableUI <- renderUI({
    req(input$upload1)  # Ensure the first file is uploaded before rendering tables
    
    if (is.null(input$upload2)) {
      # If the second file is not uploaded, show the first table
      DTOutput("table1")
    } else {
      # If the second file is uploaded, hide the first table and show the second one
      DTOutput("table2")
    }
  })
  
  # Render the preview table for the first uploaded file
  output$table1 <- renderDT({
    req(data1())  # Ensure data1 is available (first file)
    datatable(data1())  # Display the data from the first uploaded file
  })
  
  # Render the preview table for the second uploaded file (if available)
  output$table2 <- renderDT({
    req(data2())  # Ensure data2 is available (second file)
    datatable(data2())  # Display the data from the second uploaded file
  })
  
  # Conditionally render the download button
  output$downloadUI <- renderUI({
    req(input$upload1, input$upload2)  # Ensure both files are uploaded
    downloadButton("download", "Download the rounded grades ")
  })
  
  output$download2UI <- renderUI({
    req(input$upload1, input$upload2)  # Ensure both files are uploaded
    downloadButton("download2", "Download the check file")
  })
  
  
  
  # Handle the download of the processed result as CSV
  output$download <- downloadHandler(
    filename = function() {
      # Get the name of the second uploaded file (without the extension)
      file_name <- tools::file_path_sans_ext(input$upload2$name)
      
      paste(file_name, "_signed.xlsx", sep = "")  # Dynamic filename
    },
    content = function(file) {
      req(data1(), data2())  # Ensure both datasets are available
      processed_data <- round_app(data1(), data2())$file  # Process the data
      write_xlsx(processed_data, file)  # Write the processed data 
    }
  )
  
  output$download2 <- downloadHandler(
    filename = "check_file.csv",
    
    content = function(file) {
      req(data1(), data2())  # Ensure both datasets are available
      check_file <- round_app(data1(), data2())$check_file  # Process the data
      write.csv(check_file, file)  # Write the processed data 
    }
  )
}


# Run the application 
shinyApp(ui = ui, server = server)
