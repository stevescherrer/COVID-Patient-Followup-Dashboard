### This script locally ingests patient information, formats, and sends text messages. 
## Written by Steve Scherrer on 25 Nov 2020 for Jim Saunders. 
## All writes preserved all wrongs traversed


##########################################################
#################### Workspace Setup #####################
##########################################################

### Import principle dependencies
## List of required package dependencies
packages = c(
  'twilio',# tw_send_message()
  'DT',
  'beepr',
  'data.table',
  'googlesheets4',
  'emojifont',
  'rmarkdown',
  'readxl'
)

### The first time you run this application, first run the following commands
if(!'devtools' %in% installed.packages()){install.packages("devtools")}
if(!'googlesheets4' %in% installed.packages()){devtools::install_github("tidyverse/googlesheets4")}

## loop to check R library for each dependent package and install if they're not currently in library
for (package in packages){
  if (!require(package, character.only = TRUE)){
    install.packages(package)
  }
  ## Load each package
  library(package, character.only = TRUE)
}


#### Tab 1: Messenging
### Import config details
config = read.csv('src/config.csv', header = FALSE)
## Set system enviornment variables
Sys.setenv(TWILIO_SID = config[config$V1 == 'TWILIO_SID',2])
Sys.setenv(TWILIO_TOKEN = config[config$V1 == 'TWILIO_TOKEN',2])

## Import chat message log
message_log = read.csv('data/message_log.csv')

# Assign twilio number
twilio_number = paste('+', config[config$V1 == 'TWILIO_NUMBER',2], sep = '', collapse = '')



##########################################################
#################### Helper Functions ####################
##########################################################

importPatientData = function(sheet_date = 1){
  ## Default sheet date is 11.19 because I know it works
  patient_data = read_sheet(file.path('data', config[config$V1 == 'PATIENT_DATA_FILE',2]), sheet = sheet_date)
  if(dim(patient_data)[1] > 0){ # don't do anything if there's no data
    colnames(patient_data)[which(grepl('Patient Name', colnames(patient_data), fixed = TRUE))] = 'patient_name'
    colnames(patient_data)[which(grepl('DOB', colnames(patient_data), fixed = TRUE))] = 'dob'
    colnames(patient_data)[which(grepl('Phone', colnames(patient_data), fixed = TRUE))] = 'number'
    
    ## Extract patient name from patient_name colum
    patient_data$name = NA
    for(i in 1:nrow(patient_data)){
      if(!is.null(patient_data$patient_name[i][[1]])){
        patient_data$name[i] = unlist(patient_data$patient_name[i])
      }
    }
    
    ## Extract patient phonenumber from number colum
    patient_data$patient_number = NA
    colnum = findPhoneColumn(patient_data)
    
    for(i in 1:nrow(patient_data)){
      if(!is.null(patient_data$number[i][[1]])){
        patient_data$patient_number[i] = unlist(patient_data[i,colnum])
      }
    }
    
    if (class(patient_data$dob[1])[1] == 'list'){
      patient_data$dob = unlist(patient_data$dob)
    }
  }
  ## Remove patients missing things
  patient_data = patient_data[!is.na(patient_data$patient_number), ]
  patient_data = patient_data[!is.na(patient_data$name), ]
  
  return(patient_data)
}

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

prioritizePatients = function(patient_data, limit = 100){
  ## Prioritize Numbers to text based on numbe of individuals in household
  patient_priority = aggregate(name~patient_number, FUN = uniqueN, data = patient_data)
  colnames(patient_priority) = c('phone_number', 'n_patients')
  patient_priority = patient_priority[order(patient_priority$n_patients, decreasing = TRUE), ]
  
  ## limit batched numbers to fewer than X patients per number
  patient_priority = patient_priority[patient_priority$n_patients <= limit, ]
  
  ### Check if any numbers have been claimed by other physicians. If they have, exclude them
  
  provider_col = which(grepl('Assigned', colnames(patient_data), fixed = TRUE) & which(grepl('Provider', colnames(patient_data), fixed = TRUE)))
  print('provider_col_fetched')
  rm_ind = c()
  for (i in 1:length(patient_priority$phone_number)){
    if(!all(is.na(patient_data[patient_data$patient_number == patient_priority$phone_number[i], provider_col]))){
      rm_ind = c(rm_ind, i)
    }
  }
  ## Remove any numbers where a patient(s) have already been assigned a provider
  if(length(rm_ind) > 0){
    patient_priority = patient_priority[-rm_ind, ]
  }
  ## Remove any patients without a phone number
  patient_priority = patient_priority[patient_priority$phone_number != '0', ]
  print('patient_prioritized')
  return(patient_priority)
}

updateProviderSheet = function(sheet_date, patient_data, patient_priority = NULL, batch_size = 1, number = NULL){
  ### Write updated provider and visit data to google sheet
  ## Find column letter in google sheet by matching known column names 
  provider_col = toupper(letters[which(grepl('Assigned', colnames(patient_data), fixed = TRUE) & which(grepl('Provider', colnames(patient_data), fixed = TRUE)))])
  visit_col = toupper(letters[which(grepl('Visit', colnames(patient_data), fixed = TRUE) & which(grepl('Complete', colnames(patient_data), fixed = TRUE)))])
  
  ## If we hand the function a single number, we'll update and mark off patients with that number. If we've given it multiple numbers, in the form of a patient priority file, we'll update using the patient priority file instead (the latter being faster) 
  if (is.null(number)){
    ## Grab a batch of patients
    indicies_to_overwrite = which(unlist(lapply(patient_data$patient_number, FUN = formatPhone)) %in% patient_priority$phone_number[1:batch_size]) + 1
  } else {
    ## Overwrite a single number
    indicies_to_overwrite = which(unlist(lapply(patient_data$patient_number, FUN = formatPhone)) %in% number[batch_size]) + 1
  }
  ## loop through indicies
  for (i in indicies_to_overwrite){
    # Update assigned provider
    range_write(file.path('data', config[config$V1 == 'PATIENT_DATA_FILE',2]), sheet = sheet_date, range = paste(provider_col, i, sep = ""), data = data.frame('Assigned Provider' = 'Jim Saunders'), col_names = FALSE)
    # Update Visit complete
    range_write(file.path('data', config[config$V1 == 'PATIENT_DATA_FILE',2]), sheet = sheet_date, range = paste(visit_col, i, sep = ""), data = data.frame('Visit Complete' = 'No'), col_names = FALSE)
  }
}

restoreProviderSheet = function(sheet_date, number){
  ### Write updated provider and visit data to google sheet
  ## Find column letter in google sheet by matching known column names 
  provider_col = toupper(letters[which(grepl('Assigned', colnames(patient_data), fixed = TRUE) & which(grepl('Provider', colnames(patient_data), fixed = TRUE)))])
  visit_col = toupper(letters[which(grepl('Visit', colnames(patient_data), fixed = TRUE) & which(grepl('Complete', colnames(patient_data), fixed = TRUE)))])
  
  ## Grab a batch of patients
  indicies_to_overwrite = which(patient_data$patient_number %in% number) + 1
  ## loop through indicies
  for (i in indicies_to_overwrite){
    # Update assigned provider
    range_write(file.path('data', config[config$V1 == 'PATIENT_DATA_FILE',2]), sheet = sheet_date, range = paste(provider_col, i, sep = ""), data = data.frame('Assigned Provider' = ' '), col_names = FALSE)
    # Update Visit complete
    range_write(file.path('data', config[config$V1 == 'PATIENT_DATA_FILE',2]), sheet = sheet_date, range = paste(visit_col, i, sep = ""), data = data.frame('Visit Complete' = ' '), col_names = FALSE)
  }
}


formatMessage = function(number, patient_data, generic_message){
  ## Subset patient data
  patients = patient_data[patient_data$patient_number == number, ]
  ## Define the oldest patient as the primary contact
  patient_names = sapply(patients$name[order(patients$dob, decreasing = TRUE)], FUN = formatName)
  patient_names = unique(patient_names[1:length(patient_names)])
  primary_name = patient_names[1]
  
  ## Format additional patient names. Note wildcard_all as well.
  if(length(patient_names) == 1){
    additional_names = ''
    wildcard_all = ''
  } else if(length(patient_names) == 2){
    additional_names = paste(' and', patient_names[2:length(patient_names)], sep = ' ')
    wildcard_all = 'all '
  } else {
    additional_names = paste(', ', paste(paste(patient_names[2:(length(patient_names)-1)], sep = ' ', collapse = ', '), patient_names[length(patient_names)], sep = ', and '), sep = '')
    wildcard_all = 'all '
  }
  
  ## Generate formatted message
  formatted_message = gsub(pattern = "%name%", replacement = primary_name, generic_message)
  formatted_message = gsub(pattern = "%additional names%", replacement = additional_names, formatted_message)
  formatted_message = gsub(pattern = "%all%", replacement = wildcard_all, formatted_message)
  formatted_message = gsub(pattern = "  ", replacement = ' ', formatted_message)
  formatted_message = gsub(pattern = " ,", replacement = ',', formatted_message)
  
  return(formatted_message)
}

formatPhone = function(number){
  formatted_number = gsub('+', '', number, fixed = T)
  formatted_number = gsub(' ', '', formatted_number, fixed = T)
  ## Make sure we have the country code
  if (substr(formatted_number, start = 1, stop = 1) != 1){
    formatted_number = paste('1', formatted_number, sep = "")
  }
  return(formatted_number)
}

sendBatchTexts = function(batch_message, patient_priority, patient_data, batch_size){
  ## Loop through a batch starting with highest priority phone numbers. For each phone number...
  for (number in patient_priority$phone_number[1:min(length(patient_priority), batch_size)]){


    ### Format phone number for messaging
    ## Remove an '+'
   
    
    ### Send Message after making sure we haven't already texted this person
    if (!formatted_number %in% message_log$Number){
      tw_send_message(from = sender_number, to = formatted_number, body = formatted_message)
    }
  }
}

updateLog = function(patient_data){
  message_log = read.csv('data/message_log.csv')
  message_log$Read[message_log$Read == "<U+2705>"] = "\u2705"
  previous_log = message_log
  previous_log$message_id = ''
  for (i in 1:nrow(previous_log)){
    previous_log$message_id[i] = paste(previous_log$Number[i], 
                                       paste(strsplit(previous_log$Date[i], split = ':')[[1]][1:2], collapse = ':'),
                                       previous_log$Body[i], sep = '')
  }
  ## Get messages from twilio server
  messages <- tw_get_messages_list(page = 0, page_size = 1000)
  ## Parse each message and add it to message log file
  for(message in messages){
    ## Format message date and convert to local time
    message_date = strsplit(message$date_updated, split = " ")[[1]][c(4, 3, 2, 5)]
    message_date_fmt = as.POSIXct(paste(c(message_date[1], '-', which(month.abb == message_date[2]), '-', message_date[3], ' ', message_date[4]), collapse = ''), tz = 'UTC')
    local_message_date = format(message_date_fmt, tz=Sys.timezone(),usetz=TRUE)
    
    ## Get Sender number and message direction
    if(twilio_number == message$from){
      message_direction = 'Outbound'
      patient_number = message$to
      if(substr(patient_number, start = 1, stop = 1) == '+'){patient_number = substr(patient_number, start = 2, stop = nchar(patient_number))}
    } else {
      message_direction = 'Inbound'
      patient_number = message$from
      if(substr(patient_number, start = 1, stop = 1) == '+'){patient_number = substr(patient_number, start = 2, stop = nchar(patient_number))}
    }
    
    ## Get patient_name
    patient_name = patient_data$name[patient_data$patient_number == patient_number][1]
    if (length(patient_name)){
      patient_name = 'Unknown'
    }
    
    ## Extract message body
    message_body = message$body
    
    ### Assign Read Status
    ## Default to square emoji indicating message is unseen
    read_status = " "
    ## If we sent the message, mark it as read
    if(message_direction == 'Outbound'){read_status = "\u2705"}
    
    ## Check if message is already in the log, if not, add it
    message_id = paste(patient_number, paste(strsplit(local_message_date, split = ':')[[1]][1:2], collapse = ':'), message_body, sep = '')
    ## if message id is not in previous log, we'll add it
    if(!message_id %in% previous_log$message_id){
      ## Write out new message log
      message_log = data.frame(unique(rbind(c(read_status, patient_name, patient_number, local_message_date, message_direction, message_body), message_log)))
        colnames(message_log) = c("Read", "Name", "Number", "Date", "Direction", "Body")
    }
  }
  
  ## Backfill names already in the log
  for (i in 1:length(message_log$Name)){
    # Match this message to all other messages by phone number
    message_subset = message_log[message_log$Number == message_log$Number[i], ]
    # if matching numbers have a name, update this log file
    if(any(message_subset$Name != 'Unknown')){
      message_log$Name[i] = unique(message_subset$Name[message_subset$Name != 'Unknown'])[1]
    }
  }
  
  ## Reorder message log
  message_log = message_log[order(message_log$Date, decreasing = TRUE), ]
  ## Write log out
  write.csv(message_log, file = 'data/message_log.csv', row.names = FALSE)
  return(message_log)
}

markMessagesRead = function(){
  message_log = read.csv('data/message_log.csv')
  message_log$Read = "\u2705"
  write.csv(message_log, file = 'data/message_log.csv', row.names = FALSE)
  return(message_log)
}

### The following function has been pulled straight from the gargle package in case gargle isn't working
request_retry = function (..., max_tries_total = 5, max_total_wait_time_in_seconds = 100) 
{
  resp <- request_make(...)
  tries_made <- 1
  b <- calculate_base_wait(n_waits = max_tries_total - 1, total_wait_time = max_total_wait_time_in_seconds)
  while (we_should_retry(tries_made, max_tries_total, resp)) {
    wait_time <- backoff(tries_made, resp, base = b)
    Sys.sleep(wait_time)
    resp <- request_make(...)
    tries_made <- tries_made + 1
  }
  invisible(resp)
}

getCurrentSheetDate = function(){
  current_sheetdate = strsplit(as.character(Sys.Date()), split = '-')[[1]][c(2,3)]
  ## format month
  if(substr(current_sheetdate[1], start = 1, stop = 1) == 0){
    # current_sheetdate[1] = substr(current_sheetdate[1], start = 2, stop = 2)
  }
  ## format day
  if(substr(current_sheetdate[2], start = 1, stop = 1) == 0){
    # current_sheetdate[2] = substr(current_sheetdate[2], start = 2, stop = 2)
  }
  ## paste together
  current_sheetdate = paste(current_sheetdate, collapse = '.')
  return(current_sheetdate)
}

defineBatchSize = function(patient_priority, batch_size){
  n_patients = 0
  i = 0
  while(n_patients < batch_size & i <= nrow(patient_priority)){
    i = i+1
    n_patients = n_patients + patient_priority$n_patients[i]
  }
  return(i)
}


importPositivePatients = function(xlsx_file, day_range = c(NULL, NULL)){
  ## Helper function for importing and cleaning positive patient files
  positive_patients = as.data.frame(read_excel(xlsx_file))
  ## Drop patients without names and/or phone numbers
  positive_patients = positive_patients[!is.na(positive_patients$`First Name`), ]
  positive_patients = positive_patients[!is.na(positive_patients$`Last Name`), ]
  positive_patients = positive_patients[!is.na(positive_patients$Mobile), ]
  ## Format a column 'name'
  positive_patients$name = ''
  for (i in 1:length(positive_patients$`First Name`)){
    positive_patients$name[i] = paste(positive_patients$`First Name`[i], positive_patients$`Last Name`[i], sep = ' ')
  }
  ## Format patient phone numbers
  positive_patients$patient_number = ''
  for (i in 1:length(positive_patients$Mobile)){
    positive_patients$patient_number[i] = formatPhone(positive_patients$Mobile[i])
  }
 
  ## Make every colname lowercase
  for (i in 1:length(colnames(positive_patients))){
    colnames(positive_patients)[i] = tolower(colnames(positive_patients)[i])
  }
  
  ## remove patients outside of the date range
  if(!class(positive_patients$date)[1] == "POSIXct"){
    for(i in positive_patients$date){
      positive_patients$date[i] = as.POSIXct(positive_patients$date[i], format = '%m/%d/%y')
    }
  }
  
  current_date = Sys.time()
  value_of_a_day = 60 * 60 * 24
  if(!is.null(day_range[1])){
    positive_patients = positive_patients[positive_patients$date <= current_date - (value_of_a_day * day_range[1]), ]
  }
  if(!is.null(day_range[2])){
    positive_patients = positive_patients[positive_patients$date <= current_date - (value_of_a_day * day_range[2]), ]
  }
  return(positive_patients)
}

### Get all sheet names
getSheetNames = function(url){
  ## Helper function for extracting all daily case sheets from google doc
  sheet_meta = gs4_get(url)
  ## Loop through each sheet name and check if it is a date (will start with a number), if it is a date, return it
  sheet_names = c()
  for (sheet_name in sheet_meta$sheets$name){
    if(substr(sheet_name, 1, 1) %in% as.character(0:9)){
      sheet_names = c(sheet_names, sheet_name)
    }
  }
  return(sheet_names)
}

### Search through sheets for a number
findPatientInSheets = function(number, url = config[config$V1 == 'PATIENT_DATA_FILE',2]){
  ### Search through sheets for a number, then mark them off with jim's provider info
  ## Get all sheet names
  sheet_names = getSheetNames(url) 
  ## Format the phone number
  number = formatPhone(number)
  ## create a variable that we will update if we find a match
  matching_sheet = list()
  matching_sheet$data = NULL
  matching_sheet$sheet_name = NULL
  
  ## Loop through all sheets in reverse looking for a match
  for (sheet in rev(sheet_names)){
    ## Import sheet 
    sheet_to_check = importPatientData(sheet_date = sheet)
      if(dim(sheet_to_check)[1] > 0){ # Don't bother checking if there's nothing in this sheet
        ## Check for corrosponding number
        if(number %in% unlist(lapply(sheet_to_check$patient_number, FUN = formatPhone))){
          ## if we find it, update matching_sheet variable and Stop looking for matches
          matching_sheet$sheet_name = sheet
          matching_sheet$data = sheet_to_check
          return(matching_sheet)
        }
      }
    }
  return(matching_sheet)
}


findPhoneColumn = function(patient_data){
  cols = lapply(colnames(patient_data), FUN = strsplit, split = " ")
  for(i in 1:length(cols)){
    col = cols[i]
    if ('phone' %in% tolower(unlist(col[[1]])) | 'number' %in% tolower(unlist(col[[1]]))){
      return (i)
    }
  }
  return(NULL)
}


updateProviderNotes = function(sheet_date, patient_data, patient_priority = NULL, batch_size = 1, number = NULL){
  ### Write updated provider and visit data to google sheet
  ## Find column letter in google sheet by matching known column names 
  notes_col = toupper(letters[which(grepl('Notes', colnames(patient_data), fixed = TRUE))])

  ## If we hand the function a single number, we'll update and mark off patients with that number. If we've given it multiple numbers, in the form of a patient priority file, we'll update using the patient priority file instead (the latter being faster) 
  if (is.null(number)){
    ## Grab a batch of patients
    indicies_to_overwrite = which(unlist(lapply(patient_data$patient_number, FUN = formatPhone)) %in% patient_priority$phone_number[1:batch_size]) + 1
  } else {
    ## Overwrite a single number
    indicies_to_overwrite = which(unlist(lapply(patient_data$patient_number, FUN = formatPhone)) %in% number[batch_size]) + 1
  }
  
  ## loop through indicies
  for (i in indicies_to_overwrite){
    if(is.na(patient_data$Notes[indicies_to_overwrite[i]])){
      new_note = 'Followup following positive diagnosis by Jim Saunders'
    } else {
      new_note = paste(patient_data$Notes[indicies_to_overwrite[i]], 'Followup following positive diagnosis peformed by Jim Saunders', sep = ' -- ')
    }
    # Update Notes provider
    range_write(file.path('data', config[config$V1 == 'PATIENT_DATA_FILE',2]), sheet = sheet_date, range = paste(notes_col, i, sep = ""), data = data.frame('Notes' = new_note), col_names = FALSE)
   }
}




