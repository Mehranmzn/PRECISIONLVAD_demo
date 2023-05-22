#' Calculates the mean estimation and calibrated residuals for individual patients
#'
#' @param df data frame containing patient data
#' @param df_normal data frame containing normal data
#' @param variable_name name of the variable for which to estimate the mean
#' 
#' @return data frame containing calibrated residuals, mean estimations, sigma, and updated columns
#'
#' @examples
#' mean_estimator_ind_patient(df, df_normal, "Flow")
#'
#'
mean_estimator_ind_patient<- function(df,
                                      df_normal,
                                      variable_name){
  
  df$calibrated_resid<-0
  iter<-180
  kl<-nrow(df)
  
  if (iter> kl) {
    iter<-kl
  }
  
  bank_data<- df_normal %>%group_by(ID)%>%slice(1:28)
  bank_data$Datetime<- as.Date(bank_data$Datetime)
  bank_data$calibrated_resid<-0
  bank_data$mean_estimation<-0
  df$mean_estimation<-0
  online_mean<-data.frame()
  
  patient_num = df$ID[1]
  if (variable_name == "Flow") {
    for (j in 1:iter) {
      a <- df[1:j,]
      new_df <- rbind(bank_data, a)
      new_model <- lmer(
        Flow ~ (1 + norm_speed | ID) + HCT, 
        data = new_df, REML = F, 
        control = lmerControl(optimizer ="bobyqa", calc.derivs = F)
      )
      the_estimated_mean <- predict(new_model, a)
      online_mean <- rbind(online_mean, the_estimated_mean)
      df[j, c("calibrated_resid", "mean_estimation")] <- c(
        df$Flow[j] - online_mean[j, 1],
        online_mean[j, 1]
      )
    }
    df[((iter+1):kl), c("calibrated_resid", "mean_estimation")] <- c(
      df[((iter+1):kl), "Flow"] - online_mean[iter, 1],
      online_mean[iter, 1]
    )
    df$sigma <- c(2, sqrt(cumsum(df$calibrated_resid[1:iter]^2)/iter))
    df$sigma[((iter+1):kl)] <- df$sigma[iter]
  } else if (variable_name == "Motor_power") {
    for (j in 1:iter) {
      a <- df[1:j,]
      new_df <- rbind(bank_data, a)
      new_model <- lmer(
        Motor_power ~ (1 + norm_speed | ID), 
        data = new_df, REML = F, 
        control = lmerControl(optimizer ="bobyqa", calc.derivs = F)
      )
      the_estimated_mean <- predict(new_model, a)
      online_mean <- rbind(online_mean, the_estimated_mean)
      df[j, c("calibrated_resid", "mean_estimation")] <- c(
        df$Motor_power[j] - online_mean[j, 1],
        online_mean[j, 1]
      )
    }
  }
  
    df[((iter+1): kl), "calibrated_resid"]<- df[((iter+1):kl), "Motor_power"] - online_mean[iter,1]
    df[((iter+1): kl), "mean_estimation"]<- online_mean[iter,1]
    
    df$sigma<-0
    
    for (s in 1:iter) {
      
      df$sigma[s]<- sqrt(var(df[1:s,"calibrated_resid"])[1])
      
      
    }
    
    # a starting value for variance/sd
    
    df$sigma[1]<-2
    df[((iter+1): kl), "sigma"]<- df[iter,"sigma"]
    
    
  }
  
  
  
  df$updated<-0
  df$updated[1]<-1
  control_limits<- data.frame(lcl = rep(0, iter), ucl = rep(0,iter), mean_estimate = rep(0, iter))
  
  
  
  return(df[, c("calibrated_resid", "mean_estimation","sigma", "updated")])
}