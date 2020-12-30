#### Tab 2: Examination
formatStatement = function(x){
  if(tolower(x) == 'yes'){
    return ('[x] Due to COVID pandemic if suspicion of exposure is possible providers are required to only initiate contact during physical exam if absolutely medically necessary. Please perform the physical exam below and include commentary in section below. When possible record vitals.')
  } else if (tolower(x) == 'no'){
    return ('[ ] Due to COVID pandemic if suspicion of exposure is possible providers are required to only initiate contact during physical exam if absolutely medically necessary. Please perform the physical exam below and include commentary in section below. When possible record vitals.')
  }
}

formatDOB = function(x){
  split_date = strsplit(x, split = '/')[[1]]
  if (length(split_date) > 1){
    return(paste(split_date[3], '-', split_date[1], '-', split_date[2], sep = ""))
  } else {
    return(x)
  }
}

writePatientChart = function(record){
  record_name = paste(substr(record$first_name, 1, 1), substr(record$last_name,1,1), gsub("/", "", record$dob), gsub("-", "", record$exam_date), sep = "")
  
  sink(file.path('exam records', paste(record_name, '.txt', sep = "")))
  {
    cat("PATIENT CHART \n============================================== \n")
    if(!is.null(record$dos)){
      cat(paste('Date: ', formatDOB(record$dos), ' \n', sep = ''))
    }
    cat(paste('Patient Name: ', formatName(record$last_name), ', ', formatName(record$first_name), ' \n', sep = ''))
    cat(paste('Date of Birth:', formatDOB(record$dob), " \n", sep = ' '))
    cat(" \n")
    cat(paste("Chief Complaint: ", record$chief_complaint, '\n', sep = ""))
    cat(" \n")
    cat(" \n")
    
    cat("HISTORY OF PRESENT ILLNESS / SYMPTOMS \n============================================== \n")
    if(is.null(record$exposure_context)){
      cat(paste('Is it possible you have had exposure to COVID-19?', record$possible_exposure))
    }else{
      cat(paste('Is it possible you have had exposure to COVID-19? ', record$possible_exposure, '. ', record$exposure_context, sep = ''))
    }
    cat(' \n')
    
    if(is.null(record$testing_context)){
      cat(paste('Have you been tested prior? ', record$prior_testing, sep = ''))
    } else {
      cat(paste('Have you been tested prior? ', record$prior_testing, ', ', record$testing_context, sep = ''))
    }
    cat(' \n')
    
    if(!is.null(record$mask_use)){
      if(tolower(record$mask_use) == 'yes'){
        cat(paste('Mask Use: Patient wears a mask and feels it is helping limit community spread of COVID-19 infections'))
      } else {
        cat(paste('Mask Use:', record$mask_use, sep = ' '))
      }
    }
    cat(' \n')
    
    if(is.null(record$travel)){
      cat('Have you traveled recently? No')
    } else {
      cat(paste('Have you traveled recently? ', record$travel, sep = ''))
    }
    cat(' \n')
    
    if(is.null(record$quarantine)){
      cat('Have you had to quarantine or been quarantining? No')
    } else {
      cat(paste('Have you had to quarantine or been quarantining? ', record$quarantine, sep = ''))
    }
    cat(' \n')
    cat(" \n")
    
    cat("SYMPTOMS \n============================================== \n")
    cat(paste('Have you been experiencing any symptoms?', record$symptoms))
    cat(' \n')
    cat(paste('Fever/Chills:           ', record$fever_chills, ' \n'))
    cat(paste('Fatigue:                ', record$fatigue, ' \n'))
    cat(paste('Cough:                  ', record$cough, ' \n'))
    cat(paste('Shortness of Breath:    ', record$shortness_of_breath, ' \n'))
    cat(paste('Congestion / Runny Nose:', record$congestion, ' \n'))
    cat(paste('Cough:                  ', record$cough, ' \n'))
    cat(paste('Sore Throat:            ', record$sore_throat, ' \n'))
    cat(paste('Headache:               ', record$headache, ' \n'))
    cat(paste('Body Aches:             ', record$bodyache, '\n'))
    cat(paste('Nausea/Vomiting:        ', record$nausea, ' \n'))
    cat(paste('Diarrhea:               ', record$diarrhea, ' \n'))
    cat(paste('Loss of Taste/Smell:    ', record$loss_of_taste, ' \n'))
    cat(paste('Trouble Breathing:      ', record$trouble_breathing, ' \n'))
    cat(paste('Chest Pain/Pressure:    ', record$chest_pain, ' \n'))
    cat(paste('New Confusion:          ', record$new_confusion, ' \n'))
    cat(paste('Inability to Wake:      ', record$inability_to_wake, ' \n'))
    cat(paste('Bluish Lips or Face:    ', record$bluish_lips_or_face, ' \n'))
    cat(paste('Other HPI & ROS Notes:  ', record$other_hpi, ' \n'))
    cat(paste('ROS: A complete ROS was performed and negative except as noted above?', record$ros, ' \n'))
    cat(" \n")
    cat(" \n")
    
    cat("PAST MEDICAL / SOCIAL / FAMILY HISTORY \n============================================== \n")
    cat(paste('Asthma:                              ', record$asthma, ' \n'))
    cat(paste('Diabetes:                            ', record$diabetes, ' \n'))
    cat(paste('Hyptertension:                       ', record$hypertension, ' \n'))
    cat(paste('Heart Disease:                       ', record$heart_disease, ' \n'))
    cat(paste('Liver Disease:                       ', record$liver_disease, ' \n'))
    cat(paste('Immunocompromised:                   ', record$immunocompromised, ' \n'))
    cat(paste('Hypercholesterolemia:                ', record$hypercholesterolemia, ' \n'))
    cat(paste('Chronic Lung Disease:                ', record$lung_disease, ' \n'))
    cat(paste('Chronic Kidney Disease with Dialysis:', record$dialysis, ' \n'))
    if(!is.null(record$other_family_hist)){
      cat(paste('Other:                               ', record$other_family_hist, ' \n'))
    }
    cat(' \n')
    cat(paste('Medications:', record$medications, ' \n'))
    cat(paste('Allergies:', record$allergies, ' \n'))
    cat(paste('Surgeries:', record$surgeries, ' \n'))
    
    if(!is.null(record$smoking)){
      cat(paste('Smoking:', record$smoking, ' \n'))
    }
    
    if(!is.null(record$alcohol)){
      cat(paste('Alcohol:', record$alcohol, ' \n'))
    }
    
    if(!is.null(record$family_hx)){
      cat(paste('Family Hx:', record$family_hx, ' \n'))
    } 
    cat(" \n")
    cat(" \n")
    
    cat("PHYSICAL EXAMINATION \n============================================== \n")
    if(!is.null(record$bmi)){cat(paste('Aproxmiate BMI:', record$bmi, ' \n'))}
    if(!is.null(record$temperature)){cat(paste('Temperature (°F):', record$temperature, ' \n'))}
    cat(paste('Vital signs are as reported by the patient using devices designed for home/personal use:', record$self_reporting, ' \n', ' \n', sep = " "))
    
    if(!is.null(record$gen)){
      cat(paste('General - Appears well, in no acute distress and not ill appearing:', record$gen, ' \n'))
      cat(paste('If the above is abnormal:', record$gen_other,' \n' ,' \n'))
    }
    
    if(!is.null(record$eyes)){
      cat(paste('Eyes - No discharge, injection; pupils equal and round; non-icteric:', record$eyes, ' \n'))
      cat(paste('If the above is abnormal:', record$eyes_other,' \n',' \n'))
    }
    
    if(!is.null(record$ears)){
      cat(paste('Ears - No external lesions noted:', record$ears, ' \n'))
      cat(paste('If the above is abnormal:', record$ears_other, ' \n', ' \n'))
    }
    
    if(!is.null(record$nose)){
      cat(paste('Nose - No external lesions or discharge noted:', record$nose, ' \n'))
      cat(paste('If the above is abnormal:', record$nose_other, ' \n', ' \n'))
    }
    
    if(!is.null(record$respiratory)){
      cat(paste('Respiratory - No conversational dyspnea, no central cyanosis:', record$respiratory, ' \n'))
      cat(paste('If the above is abnormal:', record$respiratory_other, ' \n', ' \n'))
    }
    
    if(!is.null(record$back)){
      cat(paste('Back - Good posture and station:', record$back, ' \n'))
      cat(paste('If the above is abnormal:', record$back_other, ' \n', ' \n'))
    }
    
    if(!is.null(record$msk)){
      cat(paste('MSK - Motor strength and tone grossly normal; gross movement of extremities WNL:', record$msk, ' \n'))
      cat(paste('If the above is abnormal:', record$msk_other, ' \n', ' \n'))
    }
    
    if(!is.null(record$neuro)){
      cat(paste('Neuro - A&O x 3, CN grossly intact, cognitive exam grossly normal:', record$neuro, ' \n'))
      cat(paste('If the above is abnormal:', record$neuro_other, ' \n', ' \n'))
    }
    
    if(!is.null(record$psych)){
      cat(paste('Psych - Judgement/insight good, mood/affect full range/pleasant, recent/remote memory normal:', record$psych, ' \n'))
      cat(paste('If the above is abnormal:', record$psych_other, ' \n', ' \n'))
    }
    
    cat("============================================== \n")
    cat("\n")
    cat(formatStatement(record$statement_issued))
    cat(" \n")
    cat(" \n")
    
    cat("REVIEWED WITH THE PATIENT \n============================================== \n
  [x] Patient consents to telehealth visit as a substitute for an in-clinic visit due to possible COVID-19 exposure during the pandemic per CDC recommendations. Patient was seen via Telehealth and assessed remotely to the best of this provider's ability. \n\n
  [x] Patient was advised to report to the ER or call 911 for severe chest pain, shortness of breath or emergency not relieved with home medication. Patient was advised to stay at home and avoid unnecessary contacts, optimize nutrition, hydration and rest. Remain calm and treat manageable symptoms at home. Contact telehealth with additional concerns. \n\n
  [x] Discussed CDC guidelines (per update July 20, 2020) for discontinuation of isolation:")
    cat(" \n
  1. A test-based strategy is no longer recommended to determine when to discontinue home isolation, except in certain circumstances. \n 
  2. For asymptomatic illness, may discontinue isolation after at least 10 days have passed since the first day of positive RT-PCR test for SARS-CoV-2 RNA. \n \n
  3. For mild to moderate illness, may discontinue isolation after at least 10 days have passed since symptoms onset AND at least 24 hours afebrile (without use of fever reducing medication) AND other symptoms improved.\n \n
  4. For severe to critical illness or immunocompromised patients, may discontinue isolation after at least 20 days have passed since symptoms onset AND at least 24 hours afebrile (without use of fever reducing medication) AND other symptoms improved. \n \n")
    cat(" \n")
    cat("[x] Patient is advised neither antibodies test (IgM and IgG) nor antigen test are FDA approved but were both granted FDA Emergency Use Authorization (EUA) under their Policy for Coronavirus Disease-2019 Tests During the Public Health Emergency, published May 11, 2020, following the federal declaration of the Public Health Emergency (PHE) declared March 13, 2020. All questions answered. Patient is advised if antibodies test is performed and positive, s/he will be recommended COVID-19 swab PCR test. Patient is also advised if antigen test is performed and negative, s/he will be recommended COVID-19 swab PCR test. If swab PCR test is performed, recommend quarantine until results are received.")
    cat(" \n")
    cat(" \n")
    
    cat("PROVIDER CODING \n============================================== \n")
    cat(paste(record$provider_coding, ' \n'))
    if(record$`z71.89_other_specified_counseling` == 'CONFIRMED'){cat('Z71.89 Other specified counseling')}
    if(!is.null(record$other_diagnosis)){cat(paste(record$other_diagnosis, ' /n'))}
    if(!is.null(record$`r51_headache`)){cat("R51 Headache \n")}
    if(!is.null(record$`r50.9_fever,_unspecified`)){cat("R50.9 Fever, Unspecified \n")}
    if(!is.null(record$`r68.83_chills_(without_fever)`)){cat("R68.83 Chills (Without fever) \n")}
    if(!is.null(record$`r52_body_aches`)){cat("R52 Body aches \n")}
    if(!is.null(record$`r53.1_weakness`)){cat("R53.1 Weakness \n")}
    if(!is.null(record$`r53.83_fatigue,_unspecified_type`)){cat("R53.83 Fatigue, unspecified type \n")}
    if(!is.null(record$`j01.90_acute_sinusitis,_unspecified`)){cat("J01.90 Acute sinusitis, unspecified \n")}
    if(!is.null(record$`r09.81_nasal_sinus_congestion`)){cat("R09.81 Nasal sinus congestion \n")}
    if(!is.null(record$`r09.82_post-nasal_drip`)){cat("R09.82 Post-nasal drip \n")}
    if(!is.null(record$`j00_rhinorrhea`)){cat("J00 Rhinorrhea \n")}
    if(!is.null(record$`r06.7_sneezing`)){cat("R06.7 Sneezing \n")}
    if(!is.null(record$`r43.0_loss_of_smell`)){cat("R43.0 Loss of smell \n")}
    if(!is.null(record$`r43.2_loss_of_taste`)){cat("R43.2 Loss of taste \n")}
    if(!is.null(record$`r43.9_smell_or_taste_sensation_disturbance`)){cat("R43.9 Smell or taste sensation disturbance \n")}
    if(!is.null(record$`r05_cough`)){cat("R05 Cough \n")}
    if(!is.null(record$`j02.9_acute_pharyngitis,_unspecified`)){cat("J02.9 Acute pharyngitis, unspecified \n")}
    if(!is.null(record$`j39.2_irritated_throat`)){cat("J39.2 Irritated throat \n")}
    if(!is.null(record$`r07.0_throat_pain`)){cat("R07.0 Throat pain \n")}
    if(!is.null(record$`r09.89_phlegm_in_throat`)){cat("R09.89 Phlegm in throat \n")}
    if(!is.null(record$`j37.0_congestion_of_larynx`)){cat("J37.0 Congestion of larynx \n")}
    if(!is.null(record$`j98.8_congestion_of_respiratory_tract`)){cat("J98.8 Congestion of respiratory tract \n")}
    if(!is.null(record$`r06.02_shortness_of_breath`)){cat("R06.02 Shortness of breath \n")}
    if(!is.null(record$`r06.2_wheezing`)){cat("R06.2 Wheezing \n")}
    if(!is.null(record$`r06.4_labored_breathing`)){cat("R06.4 Labored breathing \n")}
    if(!is.null(record$`r06.03_respiratory_distress`)){cat("R06.03 Respiratory distress \n")}
    if(!is.null(record$`r06.89_other_abnormalities_of_breathing`)){cat("R06.89 Other abnormalities of breathing \n")}
    if(!is.null(record$`r07.89_tightness_in_chest`)){cat("R07.89 Tightness in chest \n")}
    if(!is.null(record$`r07.1_chest_pain_on_breathing`)){cat("R07.1 Chest pain on breathing \n")}
    if(!is.null(record$`r07.9_chest_pain,_unspecified_type`)){cat("R07.9 Chest pain, unspecified type \n")}
    if(!is.null(record$`r11.0_nausea_alone`)){cat("R11.0 Nausea alone \n")}
    if(!is.null(record$`r11.2_nausea_with_vomiting,_unspecified`)){cat("R11.2 Nausea with vomiting, unspecified \n")}
    if(!is.null(record$`r11.10_vomiting,_unspecified`)){cat("R11.10 Vomiting, unspecified \n")}
    if(!is.null(record$`r10.9_abdominal_pain,_unspecified_abdominal_location`)){cat("R10.9 Abdominal pain, unspecified abdominal location \n")}
    if(!is.null(record$`r19.7_diarrhea,_unspecified_type`)){cat("R19.7 Diarrhea, unspecified type \n")}
    if(!is.null(record$`j22_acute_respiratory_infection`)){cat("J22 Acute respiratory infection  \n")}
    if(!is.null(record$`b34.9_viral_illness`)){cat("B34.9 Viral illness \n")}
    if(!is.null(record$`j10_influenza_due_to_other_identified_influenza_virus`)){cat("J10 Influenza due to other identified influenza virus \n")}
    cat(" \n")
    cat(" \n")
    
    cat("LAB ORDERS \n============================================== \n")   
    if(!is.null(record$`covid-19_pcr_(u0003)`)){cat("COVID-19 PCR (U0003) \n")}
    if(!is.null(record$`covid-19_antigen,_rapid_poc_(87811)`)){cat("COVID-19 Antigen, Rapid POC (87811) \n")}
    if(!is.null(record$`covid-19_antibody,_rapid_poc_(86328)`)){cat("COVID-19 Antibody, Rapid POC (86328) \n")}
    if(!is.null(record$`influenza_a/b,_rapid_poc_(87804_x2)`)){cat("Influenza A/B, Rapid POC (87804 x2) \n")}
    if(!is.null(record$`strep_a,_rapid_poc_(87880)`)){cat("Strep A, Rapid POC (87880) \n")}
    
    if(is.null(record$`covid-19_pcr_(u0003)`) & 
       is.null(record$`covid-19_antigen,_rapid_poc_(87811)`) & 
       is.null(record$`covid-19_antibody,_rapid_poc_(86328)`) & 
       is.null(record$`influenza_a/b,_rapid_poc_(87804_x2)`) &
       is.null(record$`strep_a,_rapid_poc_(87880)`)
    ){
      cat('None \n')
    }
    cat(" \n")
    
    cat("E&M LEVEL \n============================================== \n")    
    cat(paste(record$`e_&_m_level`, ' \n'))
    
    if(record$`e_&_m_level` == 99203){
      cat("Discharge instructions provided? Yes \n")    
    }
    cat('\n')
    
    cat('============================================== \n')
    cat(paste('How was this patient evaluated during this clinical encounter?', record$evaluation_type, ' \n'))
    cat(paste('Patient consents to video visit per CDC guidelines during COVID-19 pandemic.', record$video_consent, ' \n'))
    cat('============================================== \n')
    cat('\n')
    
    cat('Provider Signature: James Saunders MD')
    cat('\n')
  }
  sink()   # Stop writing to the file
  
  ## Write PDF footer
  writeLines(sprintf(
    '\\usepackage{fancyhdr}
      \\fancyhead[R]{}
\\fancyhead[C]{}
\\fancyhead[L]{}
  \\pagestyle{fancy}
  \\fancyfoot[LE,RO]{%s}
  \\fancyfoot[CO,CE]{\\thepage}
  ', paste(record$last_name, record$first_name, gsub('/', '', record$dob), sep = "")), "exam records/header.tex")
  
  text = readLines(file.path('exam records', paste(record_name, '.txt', sep = "")))
  cat(text, sep = " \n", file = file.path('exam records', paste(record_name, '.Rmd', sep = "")))
  render(file.path('exam records', paste(record_name, '.Rmd', sep = "")),clean = TRUE, pdf_document(includes = includes(in_header = 'header.tex')))
  ## Removing support files
  file.remove(file.path('exam records', paste(record_name, '.Rmd', sep = "")))
  file.remove(file.path('exam records', paste(record_name, '.txt', sep = "")))
  file.remove(file.path('exam records', paste(record_name, '.tex', sep = "")))
  file.remove(file.path('exam records',  paste(record_name, '.log', sep = "")))
  file.remove(file.path('exam records',  paste(record_name, '.aux', sep = "")))
  file.remove(file.path('exam records',  paste(record_name, '.out', sep = "")))
  file.remove(file.path('exam records', 'header.tex'))
  return(record_name)
}


formatRecordColumns = function(record){
  ## Take each column out of a list format
  for (col in colnames(record)){
    if(is.list(record[,col])){
      record[,col] = unlist(record[,col])
    }
  }
  
  ## Change 'NULL' values to NULL values, Change 'NA' and 'N/A' to NA
  if(any(tolower(record) == 'null')){
    record[tolower(record) == 'null'] = NULL
  }
  if(any(tolower(record) == 'na')){
    record[tolower(record) == 'na'] = NA
  }
  if(any(tolower(record) == 'n/a')){
    record[tolower(record) == 'n/a'] = NA
  }
  
  ## Rename columns
  record$first_name = strsplit(record$name, split = ' ')[[1]][1]
  record$last_name =  strsplit(record$name, split = ' ')[[1]][2]
  colnames(record)[which(colnames(record) == "is_it_possible_you_had_exposure_to_covid-19?")] = 'possible_exposure'
  colnames(record)[which(colnames(record) == "if_possible_please_give_context-_when,_where,_etc?")] = 'exposure_context'
  colnames(record)[which(colnames(record) == "have_you_been_tested_prior?")] = 'prior_testing'
  colnames(record)[which(colnames(record) == "if_yes,_to_the_question_above,_when_was_the_last_test,_results,_any_other_relevant_details")] = 'testing_details'
  colnames(record)[which(colnames(record) == "do_you_wear_a_mask_and_feel_it_is_helping_limit_community_spread_of_covid-19_infections?")] = 'mask_use'
  colnames(record)[which(colnames(record) == "have_you_traveled_recently?_if_so_where?")] = 'travel'
  colnames(record)[which(colnames(record) == "have_you_had_to_quarantine_or_been_quarantining?")] = 'quarantine'
  colnames(record)[which(colnames(record) == "any_symptoms?")] = 'symptoms'
  colnames(record)[which(colnames(record) == "fever/chills")] = 'fever_chills'
  colnames(record)[which(colnames(record) == "congested/runny_nose")] = 'congestion'
  colnames(record)[which(colnames(record) == "body_aches")] = 'bodyache'
  colnames(record)[which(colnames(record) == "nausea/vomiting")] = 'nausea'
  colnames(record)[which(colnames(record) == "loss_of_taste/smell")] = 'loss_of_taste'
  colnames(record)[which(colnames(record) == "chest_pain/pressure")] = 'chest_pain'
  colnames(record)[which(colnames(record) == "other_hpi_&_ros_notes:")] = 'other_hpi'
  colnames(record)[which(colnames(record) == "how_was_this_patient_evaluated_during_this_clinical_encounter?")] = 'evaluation_type'
  colnames(record)[which(colnames(record) == "patient_consents_to_video_visit_per_cdc_guidelines_during_covid-19_pandemic.")] = 'video_consent'
  colnames(record)[which(colnames(record) == "ros:_a_complete_ros_was_performed_and_negative_except_as_noted_above.")] = 'ros'
  colnames(record)[which(colnames(record) == "chronic_lung_dz")] = 'lung_disease'
  colnames(record)[which(colnames(record) ==  "chronic_kidney_dz_w/_dialysis")] = 'dialysis'
  colnames(record)[which(colnames(record) ==  "approximate_bmi")] = 'bmi'
  colnames(record)[which(colnames(record) ==  "vital_signs_are_as_reported_by_the_patient_using_devices_designed_for_home/personal_use.")] = 'self_reporting'
  colnames(record)[which(colnames(record) ==  "if_no_to_the_statement_above:")] = 'self_reporting_other'
  colnames(record)[which(colnames(record) ==  "gen:_appears_well,_in_no_acute_distress_and_not_ill_appearing")] = 'gen'
  colnames(record)[which(colnames(record) ==  "if_above_is_abnormal:")] = 'gen_other'
  colnames(record)[which(colnames(record) ==  "eyes:_no_discharge,_injection;_pupils_equal_and_round;_non-icteric")] = 'eyes'
  colnames(record)[which(colnames(record) ==  "if_above_is_abnormal:.1")] = 'eyes_other'
  colnames(record)[which(colnames(record) ==  "ears:_no_external_lesions_noted")] = 'ears'
  colnames(record)[which(colnames(record) ==  "if_above_is_abnormal:.2")] = 'ears_other'
  colnames(record)[which(colnames(record) ==  "nose:_no_external_lesions_or_discharge_noted")] = 'nose'
  colnames(record)[which(colnames(record) ==  "if_above_is_abnormal:.3")] = 'nose_other'
  colnames(record)[which(colnames(record) ==  "respiratory:_no_conversational_dyspnea,_no_central_cyanosis")] = 'respiratory'
  colnames(record)[which(colnames(record) ==  "if_above_is_abnormal:.4")] = 'respiratory_other'
  colnames(record)[which(colnames(record) ==  "back:_good_posture_and_station")] = 'back'
  colnames(record)[which(colnames(record) ==  "if_above_is_abnormal:.5")] = 'back_other'
  colnames(record)[which(colnames(record) ==  "msk:_motor_strength_and_tone_grossly_normal;_gross_movement_of_extremities_wnl")] = 'msk'
  colnames(record)[which(colnames(record) ==  "if_above_is_abnormal:.6")] = 'msk_other'
  colnames(record)[which(colnames(record) ==  "neuro:_a&o_x_3,_cn_grossly_intact,_cognitive_exam_grossly_normal")] = 'neuro'
  colnames(record)[which(colnames(record) ==  "if_above_is_abnormal:.7")] = 'neuro_other'
  colnames(record)[which(colnames(record) ==  "psych:_judgement/insight_good,_mood/affect_full_range/pleasant,_recent/remote_memory_normal")] = 'psych'
  colnames(record)[which(colnames(record) ==  "if_above_is_abnormal:.1")] = 'psych_other'
  colnames(record)[which(colnames(record) ==  "statement_issued\r\n_due_to_covid_pandemic_if_suspicion_of_exposure_is_possible_providers_are_required_to_only_initiate_contact_during_physical_exam_if_absolutely_medically_necessary._please_perform_the_physical_exam_below_and_include_commentary_in_section_below._when_possible_record_vitals.")] = 'statement_issued'
  return(record)
}

loadExamData = function(url = config[config$V1 == 'PATIENT_CHARTS_SHEET',2]){
  ## Load exam data
  exam_data = read_sheet(url, sheet = 'Sheet1', skip = 1, col_names = FALSE)
  
  ## Format exam data
  exam_data = as.data.frame(t(exam_data))
  exam_data = exam_data[-(1:3), ]
  col_names = gsub(' ', '_', unlist(lapply(exam_data[1, ], FUN = tolower)))
  exam_data = exam_data[-(1:4), ]
  colnames(exam_data) = col_names
  colnames(exam_data)[1] = 'symptomatic_type'
  exam_data = exam_data[ ,-(6:30)]
  row.names(exam_data) = NULL
  
  return(exam_data)
}

##### Updated function for reading in patient logs from Jim's spreadsheet
chartPatientsFromSheet = function(){
  ## Read in google sheet containing exam data
  exam_data = loadExamData()
  
  ## Loop through all exam data
  for (i in 1:nrow(exam_data)){
    ## Check if chart has been been printed for a patient by checking for a patient chart id
    if(exam_data$patient_chart_id[i] == 'NULL' & !is.na(exam_data$name[i]) & !exam_data$name[i] == 'NULL'){
      ## Pull individual patient records
      record = exam_data[i, ]
      ## Check if patient was seen
      if(!is.na(record$symptomatic_type) & record$symptomatic_type != 'NULL'){
        ## Reformat record
        record = formatRecordColumns(record)
        ## Get record location for writing chart name to google sheet 
        cell_pos = cell_limits(ul = c(156,i+7), lr = c(156,i+7)) # get cell position to write
        ## Write chart
        writeChart = function(rec, pos){
          record_name = writePatientChart(rec)
          print(record_name)
          range_write(config[config$V1 == 'PATIENT_CHARTS_SHEET',2], sheet = 'Sheet1', range = pos, data = data.frame(record_name), col_names = FALSE)
        }
        tryCatch(
          expr = writeChart(record, cell_pos),
          error = function(e){print(paste('Error writing record for', record$name))}
        )
      }
    }
  }
  ## Reset sink()
  for(i in seq_len(sink.number())){
    sink(NULL)
  }
}

