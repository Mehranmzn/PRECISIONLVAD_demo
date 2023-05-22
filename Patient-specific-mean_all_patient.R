#' Calculate residuals for each patient for a specific variable
#'
#' This function takes a variable name and a data frame containing all patient data
#' and returns a data frame containing residuals for each patient.
#'
#' @param variable_name The name of the variable for which residuals are to be calculated.
#' @param df_all_patient The data frame containing all patient data.
#'
#' @return A data frame containing residuals for each patient.

patient_specific_residual<-  function(variable_name, df_all_patient){
  
  
  # This function takes a variable name and a data frame containing all patient data 
  # and returns a data frame containing residuals for each patient. 
  
  # Arguments:
  # variable_name: the name of the variable for which residuals are to be calculated.
  # df_all_patient: the data frame containing all patient data.
  
  
  
  # Find all unique patient IDs in the data frame.
  u= sort(unique(df_all_patient$ID))
  
  # Create an empty data frame to store residuals for all patients.
  df_contain_resid_all_patient<-data.frame()
  
  # Loop through all patients.
  for(k in 1:length(u)){
    
    # Subset the data frame to contain only data for the current patient.
    df_patient<- df_all_patient[df_all_patient$ID==u[k],]
    
    # Subset the admission file to contain only admission data for the current patient.
    patient_admission<- admission_file[admission_file$ID==u[k] ,]
    
    # Find all dates on which the stored speed value changed for the current patient.
    dates_speed_changed<- sort(df[which(df_patient$Stored_speed != lag(df_patient$Stored_speed)),]$Datetime)
    
    # If the stored speed value never changed, use only admission dates as events.
    if (length(dates_speed_changed)!=0) {
      events<- sort(unique(patient_admission$admission_date), 
                    unique(dates_speed_changed))
    }else{
      events<- unique(patient_admission$admission_date)
    }
    
    # Create an empty data frame to store residuals for the current patient.
    residuals<- data.frame()
    
    # Get the total number of observations for the current patient.
    number_of_obs<-nrow(df_patient)
    
    # If there are no events for the current patient, skip to the next patient.
    if (length(events)==0) {
      next
    }else{
      
      # If there are multiple events for the current patient, calculate residuals 
      # for each event.
      if (length(events)>1) {
        
        # Find the index of the last observation before the first event.
        calibration_index<- max(which(df_patient$Datetime < events[1]))
        
        # Subset the data frame to contain only observations before the first event.
        calibration_obs<- df_patient[1:calibration_index,]
        
        # Calculate residuals for the first event.
        residuals<- mean_estimator_ind_patient(df = calibration_obs, 
                                               df_normal = pws_patient, 
                                               variable_name = variable_name)
        
        # Create an empty data frame to store residuals for the current event.
        residuals_temporal<- residuals_temporal[(1:(nrow(calibration_obs))),]
        
        # Loop through all events except for the first one.
        for (t in 2:length(events)) {
          
          # Find the indices of the observations between the current and previous event.
          i<- max(which(df_patient$Datetime <= events[t]))
          j<- max(which(df_patient$Datetime <= events[t-1]))
          
          # Subset the data frame to contain only observations between the current and previous event.
          calibration_obs<- df_patient[(j+1):i,]
          
          # Calculate residuals for the current event.
          residuals_temporal<- mean_estimator_ind_patient(df = calibration_obs, 
                                                          df_normal = pws_patient, 
                                                          variable_name = variable_name)
          
          # Create an empty data frame to store residuals for the current event.
          residuals_temporal<- residuals_temporal[(1:(nrow(calibration_obs))),]
          
          # Add the residuals for the current event to the residuals
          
          residuals<- rbind(residuals, residuals_temporal)
        }  
        
        
        
        number_of_resid<- nrow(residuals)
        
        
        if (number_of_resid < number_of_obs) {
          
          calibration_obs<- df_patient[(number_of_res+1):number_of_obs,]
          
          residuals_temporal<-  mean_estimator_ind_patient(df = calibration_obs, 
                                                           df_normal = pws_patient, 
                                                           variable_name = variable_name)
          
          residuals_temporal<- residuals_temporal[(1:(nrow(calibration_obs))),]
          residuals<- rbind(residuals, residuals_temporal)
          
          
        }
        
        
        
      }else{
        calibration_index<- max(which(df_patient$Datetime < events[1]))
        
        calibration_obs<- df_patient[1:calibration_index,]
        
        residuals_temporal<- mean_estimator_ind_patient(df = calibration_obs, 
                                                        df_normal = pws_patient, 
                                                        variable_name = variable_name)
        
        
        
        residuals<- rbind(residuals, residuals_temporal)
        
        
        number_of_res<- nrow(residuals)
        
        if (number_of_res < number_of_obs) {
          
          
          calibration_obs<- df_patient[(number_of_res+1):number_of_obs,]
          
          
          residuals_temporal<- mean_estimator_ind_patient(df = calibration_obs, 
                                                          df_normal = pws_patient, 
                                                          variable_name = variable_name)
          
          
          residuals_temporal<- residuals_temporal[(1:(nrow(calibration_obs))),]
          
          residuals<- rbind(residuals, residuals_temporal)
          
          
          
          
        }
        
        
        
      }
    }
    
    
    
    
    
    df_patient<- cbind(df_patient, residuals)
    
    
    
    
    df_contain_resid_all_patient<-rbind(df_contain_resid_all_patient, df_patient)
    
  }
  
  
  return(df_contain_resid_all_patient)
  
  
}
