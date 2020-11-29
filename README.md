# COVID-Patient-Followup-Dashboard
 A dashboard for soliciting telemedicine followups with persons tested for COVID 19


This project consists of a shiny app (to be run locally on the user's computer) and supporting R scripts for running a SMS based messaging platform to communictate with a database consisting of  names, phone numbers, and clinic names.

After downloading the repository, users will need to create a twilio account (https://www.twilio.com) and create a twilio phone number. 

To run the web application, a user will need to create an csv file called 'config.csv' and place it in the project's 'src' folder. This script contains two columns with twilio account details in the following format: 

TWILIO_SID, aStringConsistingOfTheUsersTwilioSID
TWILIO_TOKEN, aStringConsistingOfTheUsersTwilioToken
TWILIO_NUMBER, 1XXXXXXXXX 
PATIENT_DATA_FILE, nameOfCSVFileContainingPatientDataKeptInDataFolder.csv


