


#' ooc_for_all_patients function
#' 
#' This function calculates Upper Control Limit (UCL) and Lower Control Limit (LCL) for each patient based on EWMA smoothed residuals and checks if there are any Out Of Control (OOC) points. 
#' 
#' @param df_contain_resid A data frame containing the residuals and patient ID
#' @param L1 A numeric value for the distance between UCL and the target value
#' @param L2 A numeric value for the distance between LCL and the target value
#' @param lambda A numeric value for the smoothing constant for EWMA smoothing
#' @param Variable_name The name of the variable used in the analysis
#' 
#' @return A data frame containing residuals, UCL, LCL, and OOC points for each patient
#' 
#' @examples
#' # Call the function with appropriate arguments
#' df_ooc <- ooc_for_all_patients(df_contain_resid = df, L1 = 6, L2 = 3, lambda = 0.8, Variable_name = "Flow")
#' 
ooc_for_all_patients <- function(df_contain_resid, L1, L2, lambda, Variable_name) {
  
  # Get unique patient IDs from the input data frame
  patients_ID <- unique(df_contain_resid$ID)
  
  # Initialize an empty data frame to store results for all patients
  df_all_patients_with_ooc <- data.frame()
  
  # Loop through all the unique patient IDs
  for (j in 1:length(patients_ID)) {
    
    # Get data for a specific patient
    df_patient <- df_contain_resid[df_contain_resid$ID == patients_ID[j], ]
    
    # Get the number of observations for this patient
    number_of_obs <- nrow(df_patient)
    
    # Calculate EWMA smoothed residuals for this patient
    df_patient$ewma_resid <- ewmaSmooth(x = seq(1, number_of_obs, 1), y = df_patient$calibrated_resid, lambda = lambda)$y
    
    # Initialize LCL and UCL columns for this patient
    df_patient$lcl <- 0
    df_patient$ucl <- 0
    
    # Initialize variables for counting OOC points and storing mean residuals
    Q <- 0
    mean_resid_patient <- data.frame()
    
    # Loop through all the observations for this patient
    for (p in 1:number_of_obs) {
      
      # Check if the observation is updated
      if (df_patient$updated[p] == 1) {
        
        # Define the start and end of the window for this observation
        start <- p
        end <- p + 59
        
        # Increment the count of OOC points
        Q <- Q + 1
        
        # Initialize the sigma value to 0 for all observations
        df$sigma <- 0
        df$sigma[p] <- 1
        
        # Initialize the iterator to 1 and calculate the coefficient
        iterator <- 1
        coef <- lambda / (2 - lambda)
        
        # Check if L1 is greater than or equal to L2
        if (L1 >= L2) {
          
          # Calculate the difference between L1 and L2
          diif <- L1 - L2
          
          # Loop through the window of 60 observations
          for (k in 1:60) {
            a<- 1 - ((1- lambda)^(2*(k)))
            
            df_patient[(k+start),]$lcl<-  mean(df_patient[(start):(k+start-1),"calibrated_resid"][[1]])-
              (L1 - (diif*iterator)/60)*df_patient$sigma[(start+k-1)]*sqrt(coef* a )
            
            
            df_patient[(k+start),]$ucl<-  mean(df_patient[(start):(k+start-1),"calibrated_resid"][[1]])+
              (L1 - (diif*iterator)/60)*df_patient$sigma[(start+k-1)]*sqrt(coef* a )
            
            
            iterator<- iterator + 1
            
          }
          
          
        }else{
          
          diif<-L2 -L1
          for (k in 1:60) {
            a<- 1 - ((1- lambda)^(2*(k)))
            
            df_patient[(k+start),]$lcl<-  mean(df_patient[(start):(k+start-1),"calibrated_resid"][[1]])- 
              (L1 + (diif*iterator)/60)*df$sigma[(start+k-1)]*sqrt(coef* a )
            
            
            df_patient[(k+start),]$ucl<-  mean(df_patient[(start):(k+start-1),"calibrated_resid"][[1]])+ 
              (L1 + (diif*iterator)/60)*df_patient$sigma[(start+k-1)]*sqrt(coef* a )
            
            
            iterator<- iterator + 1
            
            
          }
          
          
        }
        
      }
    }
    
    
    df_patient$ucl[1]<-df_patient$ucl[2]
    df_patient$lcl[1]<-df_patient$lcl[2]
    
    df_patient$ooc<-0
    
    for (p in 1:number_of_obs) {
      if (df_patient$ucl[p]==0 | is.na(df_patient$ucl[p]==T)) {
        df_patient$ucl[p]<-df_patient$ucl[p-1]
        df_patient$lcl[p]<-df_patient$lcl[p-1]
      }
      
      if (df_patient$ewma_resid[p]>=df_patient$ucl[p]| df_patient$ewma_resid[p]<=df_patient$lcl[p]) {
        df_patient$ooc[p]<-1
      }
      
    }
    
    
    
    
    
    
    df_all_patients_with_ooc<-rbind(df_all_patients_with_ooc, df_patient)
    
  }
  
  
  
  return(df_all_patients_with_ooc)
  
}
