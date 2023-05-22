library(qcc)
library(dplyr)
library(lme4)

###########____________I/O and data cleaning_____________#############

# loading the datasets:
df_all_patient<-read.csv("Tutorial/data/syn_data_cardiacarrythmia.csv")


#or:
df_all_patient<-read.csv("Tutorial/data/syn_data_majorbleeding.csv")


#stable_patients
pws_patient<-read.csv("Tutorial/data/example_stable_patient.csv")
admission_file<-read.csv("example_admission_file.csv")




variable_name="Flow"

L1 = 5
L2 = 3
lambda = 0.2

df_all_patient$Datetime<- as.Date(df_all_patient$Datetime)
pws_patient$Datetime<- as.Date(pws_patient$Datetime)

###########____________Applying the algorithm on the patients_____________#############
# Obtaining the patient-specific mean for all patient and variable
# flow
df_residuals<-patient_specific_residual(variable_name = variable_name,
                              df_all_patient = df_all_patient)


# obtaining the out of control points

out_of_controls = ooc_for_all_patients(df_contain_resid = df_residuals,
                     L1 = L1,
                     L2 = L2,
                     lambda = lambda,
                     Variable_name = variable_name)

# Evaluated

evaluation_function(df_contain_ooc_all_patients =out_of_controls ,
                    variable =variable_name)



