### This script locally ingests patient information, formats, and sends text messages. 
## Written by Steve Scherrer on 25 Nov 2020 for Jim Saunders. 
## All writes preserved all wrongs traversed



## Import file showing which records have been previously called
previous_stopping_record = as.numeric(read.csv('data/called_index.csv')[1])
## Import chat message log
message_log = read.csv('data/message_log.csv')

### Import config details
config = read.csv('src/config.csv', header = FALSE)
## Import patient data file
patient_data = read.csv(file.path('data', config[config$V1 == 'PATIENT_DATA_FILE',2]))
## Set system enviornment variables
Sys.setenv(TWILIO_SID = config[config$V1 == 'TWILIO_SID',2])
Sys.setenv(TWILIO_TOKEN = config[config$V1 == 'TWILIO_TOKEN',2])
# Assign twilio number
twilio_number = paste('+', config[config$V1 == 'TWILIO_NUMBER',2], sep = '', collapse = '')

##########################################################
#################### Workspace Setup #####################
##########################################################

### Import principle dependencies
  ## List of required package dependencies
  packages = c(
    'twilio',# tw_send_message()
    'DT',
    'beepr'
  )
  
  ## loop to check R library for each dependent package and install if they're not currently in library
  for (package in packages){
    if (!require(package, character.only = TRUE)){
      install.packages(package)
    }
    ## Load each package
    library(package, character.only = TRUE)
  }


##########################################################
#################### Helper Functions ####################
##########################################################
  
## Format patient's name
formatName = function(name){
  ## Split off just first name
  formatted_name = strsplit(name, split = " ")[[1]][1]
  ## Convert to lower case
  formatted_name = tolower(formatted_name)
  ## strip off first letter of name, caplitalize and then paste back together
  formatted_name = paste(toupper(substr(formatted_name, start = 1, stop = 1)), substr(formatted_name, start = 2, stop = nchar(formatted_name)), sep = '')
  return (formatted_name)
}
# formatName(patient_data$name[1])
  
## Format patient's phone number
formatPhone = function(number){
  formatted_number = gsub(pattern = "", replacement = "", number)
  ## add country code (1) to number
  formatted_number = paste('+1', as.character(formatted_number), sep = "")
  if (nchar(formatted_number) == 12 & 
      substr(formatted_number, start = 1, stop = 2) == "+1" & 
      !is.na(as.numeric(substr(formatted_number, start = 2, stop = nchar(formatted_number))))
      ){
  return(formatted_number)
  } else {
    print(paste('Phone number ', number, ' could not be formatted correctly.',sep = ""))
    return (number)
  }
}

formatClinic = function(clinic){
  # """
  # Helper Function to format clinic name
  # Takes string 'clinic' from patient records file, 
  # 
  # Autoformats capitalization, and adds the word clinic to the end if not already present
  # 
  # Inputs: clinic - A string containing one or more words
  # Outputs: Formatted string
  # """
  split_clinic = tolower(strsplit(clinic, split = ' ')[[1]])
  ## Capitalize anything thats not 'and' 'or' or 'the' unless it occurs first
  for (i in 1:length(split_clinic)){
    word = split_clinic[i]
    if ((! word %in% c('and', 'or', 'the')) | (i == 1)){
      formatted_word = paste(toupper(substr(word, start = 1, stop = 1)), substr(word, start = 2, nchar(word)), sep = "")
      split_clinic[i] = formatted_word
    }
  }
  if (tolower(split_clinic[length(split_clinic)]) != 'clinic'){
    split_clinic[length(split_clinic) + 1] = 'Clinic'
  }
  return(paste(split_clinic, collapse = ' '))
}

formatPatientData = function(patient_data){
  for (i in 1:length(patient_data[,1])){
    patient_data[i, phone_column_number] = formatPhone(patient_data[i, phone_column_number])
  }
  return(patient_data)
}

sendBatchTexts = function(generic_message, batch_size){
  ### Main Function to send text messages to patients
    ## Updated so batch messages will not go out to the same patient more than once.
  if (i <= nrow(patient_data)){
    for (i in (previous_stopping_record + 1):min((previous_stopping_record + 1 + batch_size), nrow(patient_data))){
      
      ### Format Messages
        formatted_message = gsub(pattern = '%name%', replacement = formatName(patient_data$patient_name[i]), x = generic_message)
        formatted_message = gsub(pattern = '%clinic%', replacement = formatClinic(patient_data$clinic_location[i]), x = formatted_message)

      ### Send Batch Texts
        ## Format recipient number
        formatted_number = gsub('+', '', patient_data$patient_number[i])
        if (substr(formatted_number, start = 1, stop = 1) != 1){
          formatted_number = paste('1', formatted_number, sep = "", collapse = "")
        }
        ### Send Message after making sure we haven't already texted this person
        if (formatted_number %in% message_log$Patient.Number == FALSE){
          tw_send_message(from = sender_number, to = formatted_number, body = formatted_message)
        }
    }
    ## Write out current index for keeping track of where in patient data we currently are
    write.csv(i, file = 'data/called_index.csv', row.names = FALSE)
  ## If we run out of records...
  } else {
    print('No more patient records in dataset.')
  }
}



updateLog = function(){
  ## Get messages from twilio server
  messages <- tw_get_messages_list(page = 0, page_size = 1000)
  ## Parse each message and add it to message log file
  for(message in messages){
    ## Format message date and convert to local time
    message_date = strsplit(message$date_updated, split = " ")[[1]][c(4, 3, 2, 5)]
    message_date_fmt = as.POSIXct(paste(c(message_date[1], '-', which(month.abb == message_date[2]), '-', message_date[3], ' ', message_date[4]), collapse = ''), tz = 'UTC')
    local_message_date = format(message_date_fmt, tz=Sys.timezone(),usetz=TRUE)
    
    ## Get Sender Name, number and message direction
    if(twilio_number == message$from){
      message_direction = 'Outbound'
      patient_number = message$to
    } else {
      message_direction = 'Inbound'
      patient_number = message$from
    }
    patient_name = patient_data$patient_name[patient_data$patient_number == patient_number][1]
    
    ## Extract message body
    message_body = message$body
    message_log = unique(rbind(c(patient_name, patient_number, local_message_date, message_direction, message_body), message_log))
  }
  message_log = message_log[order(message_log$Datetime, decreasing = TRUE), ]
  write.csv(message_log, file = 'data/message_log.csv', row.names = FALSE)
  return(message_log)
}


