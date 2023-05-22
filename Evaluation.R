#' This function takes in a data frame containing out-of-control (OOC) events for all patients and a variable, and returns 
#' a list of data frames containing information about daily alarms for each patient, true alarms, and weeks with alarms.
#' @param df_contain_ooc_all_patients A data frame containing OOC events for all patients
#' @param variable A variable
#' @return A list of data frames containing information about daily alarms for each patient, true alarms, and weeks with alarms.
evaluation_function<- function( df_contain_ooc_all_patients,
                                variable){
  
  patients_ID = sort(unique(df_contain_ooc_all_patients$ID))
  
  number_of_daily_alarms<- data.frame()
  
  alarms_14_days<- data.frame()
  info_alarm_14<-data.frame()
  
  for(k in 1:length(patients_ID)){
    
    df_patient<- df_contain_ooc_all_patients[df_contain_ooc_all_patients$ID == patients_ID[k],]
    
    number_of_obs<- nrow(df_patient)
    
    patient_admission<- admission_file[admission_file$ID==patients_ID[k] ,]$admission_date
    
    
    num_adm_before_exclusion<- length(patient_admission)
    
    
    one_week_before_adm<- as.Date(patient_admission) - 14 
    
    
    #########_____BEFORE event weeks___________######
    weekno <- as.numeric(df_patient$Datetime -df_patient$Datetime[1]) %/% 14 +1
    
    Week<- df_patient$Datetime[1] + 14* weekno
    week<- sort(unique(Week))
    week<- as.Date(week)
    
    
    alarm_14_day_bef_event<- data.frame()
    
    
    infor_alarm_14_days<- data.frame()
    
    
    FN_calculate<-data.frame()
    
    for (t in 1:length(patient_admission)) {
      
      event1<- as.Date(patient_admission[t])
      
      if (sum(df_patient[between(df_patient$Datetime, event1-14  , event1),]$ooc, na.rm = T)!=0 ) {
        
        alarm_14_day_bef_event[t,1]<-1
        alarm_14_day_bef_event[t,2]<-1
        
        infor_alarm_14_days[t,1]<- sum(df_patient[between(df_patient$Datetime, event1-14  , event1),]$ooc)
        infor_alarm_14_days[t,2]<-  patients_ID[k]
        infor_alarm_14_days[t,3]<- event1
        
        
        
        
      }else{
        alarm_14_day_bef_event[t,1]<-0
        alarm_14_day_bef_event[t,2]<-1
        
        infor_alarm_14_days[t,1]<- 0
        infor_alarm_14_days[t,2]<-  patients_ID[k]
        infor_alarm_14_days[t,3]<- event1
        
        
      }
      
    }
    
    ### Now we have to shot down alarms after 14 days of each event
    for (t in 1:length(patient_admission)) {
      
      event1<- as.Date(patient_admission[t])
      
      
      df_patient[between(df_patient$Datetime, event1, event1+14),]$ooc<- 0
      
      
    }
    
    # ##Daily measures:
    number_of_daily_alarms[k,1]<-patients_ID[k]
    
    if (nrow(infor_alarm_14_days)!=0) {
      
      #true alarms 14 days prior
      number_of_daily_alarms[k,2]<-sum(infor_alarm_14_days[,1],na.rm=T)
      
    }else{
      number_of_daily_alarms[k,2]<-0
      
    }
    
    
    # False alarms : total alarms - true alarms
    number_of_daily_alarms[k,3]<- sum(df_patient$ooc, na.rm=T) -  number_of_daily_alarms[k,2]
    
    # True-Negatives: Total Negatives - negatives in 14 days
    number_of_daily_alarms[k,4]<- sum(df_patient$ooc==0, na.rm= T) - colSums(infor_alarm_14_days==0, na.rm = T)[[1]]
    
    
    # FAR
    number_of_daily_alarms[k,5]<- ((number_of_daily_alarms[k,3])/(number_of_obs))
    
    
    
    total_weeks[k,1]<- length(week)
    
    
    
    alarms_14_days<-rbind(alarms_14_days, alarm_14_day_bef_event)
    
    info_alarm_14<-rbind(info_alarm_14, infor_alarm_14_days)
    
    
  }
  
  
  
  colnames(info_alarm_14)[1]<- "True_alarms"
  colnames(info_alarm_14)[2]<- "ID"
  colnames(info_alarm_14)[3]<- "Admission_Date"
  
  colnames(alarms_14_days)[1]<- "predicted"
  colnames(alarms_14_days)[2]<- "truth"
  
  colnames(number_of_daily_alarms)[1]<-"ID"
  colnames(number_of_daily_alarms)[2]<-"True_alarms"
  colnames(number_of_daily_alarms)[3]<-"False_alarms"
  colnames(number_of_daily_alarms)[4]<-"True_negatives"
  colnames(number_of_daily_alarms)[5]<-"FAR"
  
  return(list(alarms_14_days, number_of_daily_alarms, info_alarm_14, total_weeks))
}

