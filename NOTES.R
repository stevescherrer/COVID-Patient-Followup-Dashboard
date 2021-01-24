## Col 1 - NL. (Normal)
## Col 2 - Date of service date patient was seen
## col 3Name - First Last
## 4 DOB - Date of Birth DD/MM/YYYY
## 5 Phone number - Doesnt need to be in chart
## 6 Chief Complaint
## 7 C.C. if other - Details
## 8. Is it possible you've had exposure to COVID-19 - ALWAYS YES
## 9. If possible pelase give context - If NULL, don't put question
## 10. Have you been tested prior...
## 11. If yes to previous, fill in, otherwise no
## 12. Are you wearning a mask and do you feel like its helping? Yes as default
## 13. Have you traveled recently? If so where?
## 14. If yes to the question above: - Will be removed and use 13 as default
## 15. Have you been quarentining - Yes No
## 16-32 Symptoms: Yes/No
## 33: Other HPI notes
## 34: How was this patient evaluated?
## 35: Patient consents - Yes
## 36: ROS: Yes
## 37: 

[x] If first column is NULL, then skip and don't make a pdf'
  # First column is jim's coding 

### None means print none on patient chart
### N/A means don't print on patient chart
### Anything else - print on patient chart

Date -> Date of service
[x] Move How was this patient evaluated during this clinical encounter to very bottom of the form
[x] Move ROS: A complete ROS was performed and negative except as noted above. [x] Yes, [ ] No to the bottom of symptoms
[x] Replace Height/Weight to Height/Weight/BMI
[x] Vital signs are as reported by the patient using devices designed for home/personal use are normal: Yes


[x] Add provider coding, lab orders E&M levels()

[x] Provider coding subsection
Line by Line details included if not NA
Ex: 
  *proider coding* 
    - Z20.828 Suspected/confirmed covid exposure
    - R09.81 Nasal sinus congestion 

[x] Same thing with lab orders - TAdd lab ordrers if present. If not, print None
  
[x]E&M level always will be present. - Listed by text
  
[x] Discharge instructions provided? If 99203, then Yes, otherwise don't add this subsection
  
[x] Provider Signature: James Saunders MD

CHANGE visit footer to LASTFIRSTDOB
File name INITIALSDOBDATEOFVISIT

Progam in visited column to update with chartname



MESSANGER APP
IS IT POSSIBLE TO DO REVERSE?
MAKE A NEW BUTTON FOR SENDING TO AGGREGATED NUMBERS OF 2 OR LESS
WITH A SLIDER TO SELECT HOW MANY TEXTS TO SEND

Note on DOB - Olivia Blockmon 15/10/2017

Need to make 'N/A' into 'NA'






############### NOTES FROM 28 DECEMBER #########
Why do some patients appear twice? Ex: Kelly Johnson
Why are so many patients missing past medical? columns AP and onward

