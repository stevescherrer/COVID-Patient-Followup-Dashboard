# This is a Shiny application for bulk texting. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

## Written by Steve Scherrer on 27 Nov 2020


##### TODO: 
    # Should we pre-sort patient info somehow (like by date they got covid test) before batch sending?
rm(list = ls())

library('shiny')
source('src/HelperFunctions.R')


##### Application logic goes here

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Patient Follow Up Interface"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            h3("Batch Message Patients"),
            textAreaInput(inputId = "batch_message", label = "Batch Message: %name% and %clinic% wildcards will be replaced from patient records", value = "Hi %name%, this is Dr. Saunders calling on behalf of %clinic%. I would like to discuss any symptoms of COVID-19 you may have or be currently experiencing. Are you available for a phone screen at no personal cost?", height = '120px'),
            sliderInput("batch_size",
                        "How texts do you want to send?",
                        min = 1,
                        max = 100,
                        value = 10),
            actionButton("send_batch", "Send Batch Messages"),
            
            div(style="margin-bottom:10px"),
            
            h3("Send Individual Response"),
            textInput(inputId = "reply_number", label = "Reply Number", value = "+1XXXXXXXXX"),
            textAreaInput(inputId = "reply_message", label = "Reply Message", value = 'Response', height = '100px'),
            actionButton("patient_reply", "Reply to Patient"),
            
  
        ),

        # Show a plot of the generated distribution
        mainPanel(
            fluidPage(DTOutput('tbl'))
        )
    )
)

# Define server logic required to draw a histogram
server = function(input, output) {
    
    output$tbl = renderDT(
        message_log, options = list(lengthChange = FALSE)
    )
    
    #### Check for new messages by re-rendering log every 30 seconds
    autoInvalidate <- reactiveTimer(30000)
    
    observe({
        autoInvalidate()
        ## Check for an update to the message log
        updated_log = updateLog()
        ## Compare rows in updated log file to current message log. If tehy're not the same, rerender and update message log
        if (dim(message_log)[1] != dim(updated_log)[1]){
            message_log = updated_log
            output$tbl = renderDT(
                message_log, options = list(lengthChange = FALSE)
            )
            beep(10)
        }
    })
    
    ### Logic for batch texting
    observeEvent(input$send_batch, {
        ## Format sender number
        sender_number = gsub('+', '', twilio_number)
        ## Send formatted input$batch_message to input$batch_size number of patients
        sendBatchTexts(input$batch_message, input$batch_size)
        ## update message log
        message_log = updateLog()
        ## update viewer
        output$tbl = renderDT(
            message_log, options = list(lengthChange = FALSE)
        )
        ## Make a noise
        beep(5)
    })
    
    ### Logic to handle individual responses
    observeEvent(input$patient_reply, {
        ## Format sender and recipient numbers
        sender_number = gsub('+', '', twilio_number)
        recipient_number = gsub('+', '', input$reply_number)
        ## Send response message
        tw_send_message(from = sender_number, to = recipient_number, body = input$reply_message)
        ## Update message log
        message_log = updateLog()
        ## update viewer
        output$tbl = renderDT(
            message_log, options = list(lengthChange = FALSE)
        )
        beep(1)
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)

