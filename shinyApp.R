# This is a Shiny application for bulk texting. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

## Written by Steve Scherrer on 27 Nov 2020


rm(list = ls())

library('shiny')
library('shinythemes')
library('shinydashboard')

source('src/HelperFunctions-BatchMessenging.R')
source('src/HelperFunctions-WritingCharts.R')

patient_data = importPatientData('11.19')
current_sheetdate = getCurrentSheetDate()

##### Application logic goes here

# Define UI for application that draws a histogram
## Set up navigation bar
ui <- navbarPage('Tele-Dashboard', theme = shinytheme('flatly'),

    # Tab title
    tabPanel("Messenger",

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            h3("Batch Message Patients"),
            ## Specify Sheet
            textAreaInput(inputId = "sheet_name", label = 'Date Label of Google Sheet', value = current_sheetdate, height = '40px'),
            
            ## Default Message Area
            textAreaInput(inputId = "batch_message", label = "Batch Message: %name%, %additional names%, and %all% wildcards will be replaced from patient records", value = "Hi %name%, this is Dr. Saunders from Starmed Healthcare, you reached out to be tested and I wanted to offer a brief telemedicine visit to check in on you %additional names% regarding any concerns or symptoms you %all% may have as part of this care. If it's alright, I'll send you a secure encrypted link through Doximity and can meet you %all% shortly.  Would that be ok?", height = '120px'),
            sliderInput("batch_size",
                        "Approx. how many patients do you want to see?",
                        min = 1,
                        max = 100,
                        value = 10),
            sliderInput('batch_limit', "Limit to n patients per number", min = 1, max = 20, value = 20),
            actionButton("send_batch", "Send Batch Messages"),
            
            ## Individual Responses
            h3("Send Individual Response"),
            textInput(inputId = "reply_number", label = "Reply Number", value = "+1XXXXXXXXX"),
            textAreaInput(inputId = "reply_message", label = "Reply Message", value = 'Response', height = '100px'),
            actionButton("patient_reply", "Reply to Patient"),
            
            ## Mark Messages as Read
            actionButton("mark_as_read", "Mark Messages As Read")
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
            fluidPage(DTOutput('tbl'))
        )
    )
),
tabPanel("Patient Visit", 
         h3('Exmination Records'),
         actionButton("writeExamRecords", "Write Patient Charts to PDF")
         )
)


# Define server logic required to draw a histogram
server = function(input, output) {
    
  #### Immediately Render Message Viewer
    output$tbl = renderDT(
        message_log, options = list(lengthChange = FALSE)
    )
    
    #### Logic to Refresh Message Viewer every 30 seconds
    autoInvalidate <- reactiveTimer(30000)
    observe({
        autoInvalidate()
        ## Check for an update to the message log
        message_log = updateLog(patient_data)
        output$tbl = renderDT(
            message_log, options = list(lengthChange = FALSE)
        )
    })
    
    ### Logic for batch texting
    observeEvent(input$send_batch, {
      #### Sending batch messages
      # Format sender number
      sender_number = formatPhone(twilio_number)
      ## Pull latest patient records from google sheets - To avoid possible conflicts with other doctors editing the sheet 
      patient_data = importPatientData(input$sheet_name)
      ## Prioritize numbers to text based on number of patients with that number 
      patient_priority = prioritizePatients(patient_data, limit = input$batch_limit)
      ## Define a batch size by number of patients
      n_patients = defineBatchSize(patient_priority, input$batch_size)
      ## Update the provider sheet - Block off patients for Jim and Change visit to no
      updateProviderSheet(input$sheet_name, patient_data, patient_priority, n_patients)
      ### Send Batch Messages - send formatted input$batch_message to n_patients number of patients
      ## Loop through each number
      for (number in patient_priority$phone_number[1:min(nrow(patient_priority), n_patients)]){
        # Format message and recipient phone number
        formatted_message = formatMessage(number, patient_data, input$batch_message)
        recipient_number = formatPhone(number)
        ## If recipient isn't already in the message log
        if (!recipient_number %in% message_log$Number){
          print(recipient_number)
          # try sending a text. If you get an error, restore the provider sheet
          tryCatch(
          expr = tw_send_message(from = sender_number, to = recipient_number, body = formatted_message),
          error = function(e){print('error'); restoreProviderSheet(input$sheet_name, number)},
          warning = function(e) {print('warning')},
          finally = function(e){print('finally')}
          )
        }
      }
      ## Update the message log
      message_log = updateLog(patient_data)
      ## Refresh Message Log Viewer
      output$tbl = renderDT(
          message_log, options = list(lengthChange = FALSE)
      )
    })
    
    ### Logic to handle individual responses
    observeEvent(input$patient_reply, {
        ## Format sender and recipient numbers
        sender_number = formatPhone(twilio_number)
        recipient_number = formatPhone(input$reply_number)
        ## Send response message
        tw_send_message(from = sender_number, to = recipient_number, body = input$reply_message)
        ## Update message log
        message_log = updateLog(patient_data)
        ## Refresh message log viewer
        output$tbl = renderDT(
            message_log, options = list(lengthChange = FALSE)
        )
    })
    
    ## Logic to Mark All Messages in Inbox as Read
    observeEvent(input$mark_as_read, {
      message_log = markMessagesRead()
      ## Refresh Message Log Viewer
      output$tbl = renderDT(
        message_log, options = list(lengthChange = FALSE)
      )
    })
    
    ### TAB 2 - Writing Patient Charts
    observeEvent(input$writeExamRecords, {
      chartPatientsFromSheet()
    })

    
}

# Run the application 
shinyApp(ui = ui, server = server)
