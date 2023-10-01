# Load the ggplot2 library
#CITE: SOME CODE WERE PRODUCED FROM ChatGPT

library(ggplot2)
df = cdc

# Assuming you have already loaded your data into a dataframe called 'df'
library(dplyr)

#Remove all these columns
df <- df %>% select(-weight, -payer_code, -medical_specialty)


#Remove the rows with the ? mark
df <- df %>%
  filter_all(all_vars(. != "?"))


# Create the box plot to interpret time in hospital vs age group
ggplot(df, aes(x=age, y=time_in_hospital)) +
  geom_boxplot() +
  labs(title="Relationship between Age Group and Time Spent in Hospital",
       x="Age Group",
       y="Time in Hospital (days)") +
  theme_minimal()


#================

library(ggplot2)

# Bar Graph for time spent vs gender
ggplot(df, aes(x=gender, y=time_in_hospital, fill=gender)) +
  stat_summary(fun=mean, geom="bar") +
  labs(title="Average Time Spent in Hospital by Gender",
       x="Gender",
       y="Average Time in Hospital (days)") +
  theme_minimal()




#====================

# Bar Graph
ggplot(df, aes(x=race, y=time_in_hospital, fill=race)) +
  stat_summary(fun=mean, geom="bar") +
  labs(title="Average Time Spent in Hospital by Race",
       x="Race",
       y="Average Time in Hospital (days)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#=======================


library(ggplot2)

#AVERAGE TIME IN HOSPITAL VS NUMBER OF PROCEDURES
ggplot(df, aes(x=factor(num_procedures), y=time_in_hospital)) +
  geom_bar(stat="summary", fun="mean", fill="skyblue", position=position_dodge()) +
  geom_smooth(aes(group=1), method="lm", color="red", se=FALSE) +
  labs(title="Average Time in Hospital by Number of Procedures",
       x="Number of Procedures", y="Average Time in Hospital") +
  theme_minimal()


#AVERAGE TIME IN HOSPITAL VS NUMBER OF EMERGENCIES
ggplot(df, aes(x=factor(number_emergency), y=time_in_hospital)) +
  geom_bar(stat="summary", fun="mean", fill="skyblue", position=position_dodge()) +
  geom_smooth(aes(group=1), method="lm", color="red", se=FALSE) +
  labs(title="Average Time in Hospital by Number of Emergencies",
       x="Number of Emergencies", y="Average Time in Hospital") +
  theme_minimal()

#AVERAGE TIME IN HOSPITAL VS NUMBER OF DIAGNOSES
ggplot(df, aes(x=factor(number_diagnoses), y=time_in_hospital)) +
  geom_bar(stat="summary", fun="mean", fill="skyblue", position=position_dodge()) +
  geom_smooth(aes(group=1), method="lm", color="red", se=FALSE) +
  labs(title="Average Time in Hospital by Number of Diagnoses",
       x="Number of Diagnoses", y="Average Time in Hospital") +
  theme_minimal()


#==== MODEL SELECTION ===============#

#Continuous Variables:
  # number_of_procedures
  # number_of_emergencies
  # number_of_diagnoses
# Categorical Variables:
  # race
  # gender
  # age_group
  # medication_changes
  # metformin
  # repaglinide
  # nateglinide
  # chlorpropamide
  # glimepiride
  # acetohexamide
  # glipizide
  # glyburide
  # tolbutamide
  # pioglitazone
  # rosiglitazone
  # acarbose
  # miglitol
  # troglitazone
  # tolazamide
  # examide
  # citoglipton
  # insulin
  # glyburide-metformin
  # glipizide-metformin
  # glimepiride-pioglitazone
  # metformin-rosiglitazone
  # metformin-pioglitazone
  # change
  # diabetesMed
  # readmitted

# Updated Categorical predictors
cat_vars <- c("race", "gender", "age", "max_glu_serum",
              "A1Cresult", "metformin", "repaglinide", "nateglinide", "chlorpropamide",
              "glimepiride", "acetohexamide", "glipizide", "glyburide", "tolbutamide",
              "pioglitazone", "rosiglitazone", "acarbose", "miglitol", "troglitazone",
              "tolazamide", "insulin", "glyburide.metformin",
              "glipizide.metformin", "glimepiride.pioglitazone",
              "metformin.pioglitazone", "change", "diabetesMed", "readmitted")



# Continuous predictors remain unchanged
cont_vars <- c( "num_lab_procedures", "num_procedures", "num_medications", 
               "number_outpatient", "number_emergency", "number_inpatient", "number_diagnoses")


library(MASS)
df[cat_vars] <- lapply(df[cat_vars], factor)
full_model <- lm(time_in_hospital ~ ., data = df[, c("time_in_hospital", cat_vars, cont_vars)])
stepwise_model <- stepAIC(full_model, direction = "both")
summary(stepwise_model)


# Check which categorical variables are not in the dataframe
missing_cat_vars <- cat_vars[!cat_vars %in% colnames(df)]
print(missing_cat_vars)

# Check which continuous variables are not in the dataframe
missing_cont_vars <- cont_vars[!cont_vars %in% colnames(df)]
print(missing_cont_vars)

# Identify categorical variables with only one level
single_level_vars <- cat_vars[sapply(df[, cat_vars], function(col) length(unique(col)) == 1)]
print(single_level_vars)


backward_model <- step(lm(time_in_hospital ~ ., data = df[, c("time_in_hospital", cat_vars, cont_vars)]), 
                       direction = "backward")

summary(backward_model)
# 
# lm(formula = time_in_hospital ~ race + gender + age + max_glu_serum + 
#      A1Cresult + metformin + repaglinide + glimepiride + acetohexamide + 
#      glipizide + glyburide + pioglitazone + rosiglitazone + insulin + 
#      change + diabetesMed + readmitted + num_lab_procedures + 
#      num_procedures + num_medications + number_outpatient + number_emergency + 
#      number_inpatient + number_diagnoses, data = df[, c("time_in_hospital", 
#                                                         cat_vars, cont_vars)])

# 
# Residual standard error: 2.523 on 97995 degrees of freedom
# Multiple R-squared:  0.2898,	Adjusted R-squared:  0.2894 
# F-statistic: 701.5 on 57 and 97995 DF,  p-value: < 2.2e-16

print (cat_vars)


# Loop through each variable in cat_vars
for (var in cont_vars) {
  print(paste("Variable:", var))
  print(unique(df[, var]))
}

######################### SHINY APP ###########################
#CITE: HELP FROM ChatGPT

# Load necessary libraries
library(shiny)
library(shinythemes)

ui <- fluidPage(
  titlePanel("Predict Time Spent in Hospital"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("race", "Race:", 
                  choices = c("Caucasian", "AfricanAmerican", "Other", "Asian", "Hispanic"),
                  selected = "Caucasian"),
      selectInput("gender", "Gender:", 
                  choices = c("Female", "Male", "Unknown/Invalid"),
                  selected = "Female"),
      selectInput("age", "Age Group:", 
                  choices = c("[0-10)", "[10-20)", "[20-30)", "[30-40)", "[40-50)", "[50-60)", 
                              "[60-70)", "[70-80)", "[80-90)", "[90-100)"),
                  selected = "[50-60)"),
      selectInput("max_glu_serum", "Max Glucose Serum:", 
                  choices = c(">200", ">300", "None", "Norm"),
                  selected = "None"),
      selectInput("A1Cresult", "A1C Result:", 
                  choices = c(">7", ">8", "None", "Norm"),
                  selected = "None"),
      
      # Button to toggle medication variables
      actionButton("toggleMed", "Show/Hide Medication Variables"),
      
      # Conditional panel for medication variables
      conditionalPanel(
        condition = "input.toggleMed % 2 == 1",
        
        # List all medication and other variables here
        selectInput("metformin", "Metformin:", 
                    choices = c("Down", "No", "Steady", "Up"),
                    selected = "No"),
        selectInput("repaglinide", "Repaglinide:", 
                    choices = c("Down", "No", "Steady", "Up"),
                    selected = "No"),
        selectInput("nateglinide", "Nateglinide:", 
                    choices = c("Down", "No", "Steady", "Up"),
                    selected = "No"),
        selectInput("chlorpropamide", "Chlorpropamide:", 
                    choices = c("Down", "No", "Steady", "Up"),
                    selected = "No"),
        selectInput("glimepiride", "Glimepiride:", 
                    choices = c("Down", "No", "Steady", "Up"),
                    selected = "No"),
        selectInput("acetohexamide", "Acetohexamide:", 
                    choices = c("No", "Steady"),
                    selected = "No"),
        selectInput("glipizide", "Glipizide:", 
                    choices = c("Down", "No", "Steady", "Up"),
                    selected = "No"),
        selectInput("glyburide", "Glyburide:", 
                    choices = c("Down", "No", "Steady", "Up"),
                    selected = "No"),
        selectInput("tolbutamide", "Tolbutamide:", 
                    choices = c("No", "Steady"),
                    selected = "No"),
        selectInput("pioglitazone", "Pioglitazone:", 
                    choices = c("Down", "No", "Steady", "Up"),
                    selected = "No"),
        selectInput("rosiglitazone", "Rosiglitazone:", 
                    choices = c("Down", "No", "Steady", "Up"),
                    selected = "No"),
        selectInput("acarbose", "Acarbose:", 
                    choices = c("Down", "No", "Steady", "Up"),
                    selected = "No"),
        selectInput("miglitol", "Miglitol:", 
                    choices = c("Down", "No", "Steady", "Up"),
                    selected = "No"),
        selectInput("troglitazone", "Troglitazone:", 
                    choices = c("No", "Steady"),
                    selected = "No"),
        selectInput("tolazamide", "Tolazamide:", 
                    choices = c("No", "Steady", "Up"),
                    selected = "No"),
        selectInput("insulin", "Insulin:", 
                    choices = c("Down", "No", "Steady", "Up"),
                    selected = "No"),
        selectInput("glyburide_metformin", "Glyburide-Metformin:", 
                    choices = c("Down", "No", "Steady", "Up"),
                    selected = "No"),
        selectInput("glipizide_metformin", "Glipizide-Metformin:", 
                    choices = c("No", "Steady"),
                    selected = "No"),
        selectInput("glimepiride_pioglitazone", "Glimepiride-Pioglitazone:", 
                    choices = c("No", "Steady"),
                    selected = "No"),
        selectInput("metformin_pioglitazone", "Metformin-Pioglitazone:", 
                    choices = c("No", "Steady"),
                    selected = "No"),
        selectInput("change", "Change:", 
                    choices = c("Ch", "No"),
                    selected = "No"),
        selectInput("diabetesMed", "DiabetesMed:", 
                    choices = c("Yes", "No"),
                    selected = "Yes"),
        selectInput("readmitted", "Readmitted:", 
                    choices = c("<30", ">30", "NO"),
                    selected = "NO")
      ),
      
      # Continuous inputs
      numericInput("num_lab_procedures", "Number of Lab Procedures:", value = 50, min = 0),
      numericInput("num_procedures", "Number of Procedures:", value = 3, min = 0),
      numericInput("num_medications", "Number of Medications:", value = 15, min = 0),
      numericInput("number_outpatient", "Number of Outpatient Visits:", value = 2, min = 0),
      numericInput("number_emergency", "Number of Emergency Visits:", value = 1, min = 0),
      numericInput("number_inpatient", "Number of Inpatient Visits:", value = 2, min = 0),
      numericInput("number_diagnoses", "Number of Diagnoses:", value = 7, min = 0),
      actionButton("predict", "Predict")
      ),
    
    
    
    mainPanel(
      verbatimTextOutput("prediction_output")
    )
  )
)

server <- function(input, output) {
  
  # Listen for the predict button to be pressed
  observeEvent(input$predict, {
    
    # Create a data frame of the input values
    new_data <- data.frame(
      race = input$race,
      gender = input$gender,
      age = input$age,
      max_glu_serum = input$max_glu_serum,
      A1Cresult = input$A1Cresult,
      metformin = input$metformin,
      repaglinide = input$repaglinide,
      nateglinide = input$nateglinide,
      chlorpropamide = input$chlorpropamide,
      glimepiride = input$glimepiride,
      acetohexamide = input$acetohexamide,
      glipizide = input$glipizide,
      glyburide = input$glyburide,
      tolbutamide = input$tolbutamide,
      pioglitazone = input$pioglitazone,
      rosiglitazone = input$rosiglitazone,
      acarbose = input$acarbose,
      miglitol = input$miglitol,
      troglitazone = input$troglitazone,
      tolazamide = input$tolazamide,
      insulin = input$insulin,
      glyburide_metformin = input$glyburide_metformin,
      glipizide_metformin = input$glipizide_metformin,
      glimepiride_pioglitazone = input$glimepiride_pioglitazone,
      metformin_pioglitazone = input$metformin_pioglitazone,
      change = input$change,
      diabetesMed = input$diabetesMed,
      readmitted = input$readmitted,
      num_lab_procedures = input$num_lab_procedures,
      num_procedures = input$num_procedures,
      num_medications = input$num_medications,
      number_outpatient = input$number_outpatient,
      number_emergency = input$number_emergency,
      number_inpatient = input$number_inpatient,
      number_diagnoses = input$number_diagnoses
    )
    
    # Predict using the backward model
    prediction <- predict(backward_model, newdata = new_data)
    
    # Display the prediction
    output$prediction_output <- renderText({
      paste("Predicted time in hospital:", round(prediction, 2), "days")
    })
  })
}

shinyApp(ui, server)





























