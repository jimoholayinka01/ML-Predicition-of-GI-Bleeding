# ============================================================
# GI BLEEDING RISK PREDICTION
# V2 - SHINY APPLICATION
# ============================================================

library(shiny)
library(bslib)
library(DBI)
library(RSQLite)
library(dplyr)
library(xgboost)

# ============================================================
# LOAD MODEL
# ============================================================

# Load XGBoost model
final_xgb_model <- xgb.load(
  "v2_gi_bleeding_model.ubj"
)

# Load model metadata
v2_model_metadata <- readRDS(
  "v2_model_metadata.rds"
)

# Extract metadata
model_threshold <- v2_model_metadata$threshold
model_feature_names <- v2_model_metadata$feature_names
model_formula <- v2_model_metadata$formula
model_sex_levels <- v2_model_metadata$sex_levels
model_race_levels <- v2_model_metadata$race_levels


# ============================================================
# 2. MODEL-SUPPORTED AGE RANGE
# ============================================================
# Based on the V2 modelling dataset:
# Minimum age = 33
# Maximum age = 111

model_min_age <- 33
model_max_age <- 111


# ============================================================
# 3. USER-FACING RACE OPTIONS
# ============================================================
# The model was trained using:
# Asian
# Black or African American
# Unknown
# White
#
# Additional user-facing categories are mapped to "Unknown"
# internally because the model does not contain separate
# categories for them.

race_choices <- c(
  "White",
  "Black or African / Caribbean",
  "Asian",
  "Mixed / Multiple ethnic groups",
  "Other ethnic group",
  "Middle Eastern",
  "Prefer not to say"
)


# ============================================================
# 4. RACE MAPPING FUNCTION
# ============================================================

map_race_to_model <- function(user_race) {
  
  case_when(
    
    user_race == "White" ~
      "White",
    
    user_race == "Black or African / Caribbean" ~
      "Black or African American",
    
    user_race == "Asian" ~
      "Asian",
    
    TRUE ~
      "Unknown"
  )
}


# ============================================================
# 5. DATABASE
# ============================================================

dir.create(
  "data",
  showWarnings = FALSE
)

con <- dbConnect(
  SQLite(),
  "patients.sqlite"
)

dbExecute(
  con,
  "
  CREATE TABLE IF NOT EXISTS patients (
    patient_id TEXT PRIMARY KEY,
    age INTEGER,
    sex TEXT,
    race TEXT,
    prior_aspirin INTEGER,
    prior_naproxen INTEGER,
    prior_celecoxib INTEGER,
    prior_peptic_ulcer INTEGER,
    prior_ulcerative_colitis INTEGER,
    prior_esophagitis INTEGER,
    prior_diverticular_disease INTEGER
  )
  "
)


# ============================================================
# 6. LOAD PATIENTS
# ============================================================

get_patients <- function() {
  
  dbGetQuery(
    con,
    "
    SELECT *
    FROM patients
    ORDER BY patient_id
    "
  )
}


# ============================================================
# 7. UI
# ============================================================

ui <- page_navbar(
  
  title = "GI Bleeding Risk Assessment",
  
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly"
  ),
  
  # ==========================================================
  # PAGE 1 - PATIENT REGISTRATION
  # ==========================================================
  
  nav_panel(
    "Patient Registration",
    
    layout_sidebar(
      
      sidebar = sidebar(
        
        h4("Patient Information"),
        
        textInput(
          "patient_id",
          "Patient ID",
          placeholder = "e.g. PAT_ID_001"
        ),
        
        sliderInput(
          "age",
          "Age",
          min = model_min_age,
          max = model_max_age,
          value = 50,
          step = 1
        ),
        
        p(
          class = "text-muted",
          style = "font-size: 0.9rem;",
          paste0(
            "Supported model age range: ",
            model_min_age,
            "–",
            model_max_age,
            " years."
          )
        ),
        
        selectInput(
          "sex",
          "Sex",
          choices = c(
            "FEMALE",
            "MALE"
          )
        ),
        
        selectInput(
          "race",
          "Race / Ethnicity",
          choices = race_choices
        ),
        
        p(
          class = "text-muted",
          style = "font-size: 0.85rem;",
          "Race categories not separately represented in the ",
          "model are mapped to an internal 'Unknown' category."
        )
      ),
      
      card(
        
        card_header(
          "Clinical and Medication History"
        ),
        
        h5("Medication History"),
        
        layout_columns(
          
          checkboxInput(
            "prior_aspirin",
            "Prior aspirin",
            FALSE
          ),
          
          checkboxInput(
            "prior_naproxen",
            "Prior naproxen",
            FALSE
          ),
          
          checkboxInput(
            "prior_celecoxib",
            "Prior celecoxib",
            FALSE
          ),
          
          col_widths = c(
            4, 4, 4
          )
        ),
        
        hr(),
        
        h5("Clinical Conditions"),
        
        layout_columns(
          
          checkboxInput(
            "prior_peptic_ulcer",
            "Prior peptic ulcer",
            FALSE
          ),
          
          checkboxInput(
            "prior_ulcerative_colitis",
            "Prior ulcerative colitis",
            FALSE
          ),
          
          checkboxInput(
            "prior_esophagitis",
            "Prior esophagitis",
            FALSE
          ),
          
          checkboxInput(
            "prior_diverticular_disease",
            "Prior diverticular disease",
            FALSE
          ),
          
          col_widths = c(
            6, 6, 6, 6
          )
        ),
        
        hr(),
        
        actionButton(
          "save_patient",
          "Save Patient",
          class = "btn-primary btn-lg"
        ),
        
        br(),
        br(),
        
        uiOutput(
          "save_message"
        )
      )
    )
  ),
  
  
  # ==========================================================
  # PAGE 2 - RISK PREDICTION
  # ==========================================================
  
  nav_panel(
    "Risk Prediction",
    
    layout_sidebar(
      
      sidebar = sidebar(
        
        h4("Select Patient"),
        
        uiOutput(
          "patient_selector"
        ),
        
        actionButton(
          "run_prediction",
          "Run Risk Assessment",
          class = "btn-primary btn-lg"
        )
      ),
      
      uiOutput(
        "prediction_result"
      )
    )
  ),
  
  
  # ==========================================================
  # PAGE 3 - MODEL INFORMATION
  # ==========================================================
  
  # ==========================================================
  # PAGE 3 - MODEL INFORMATION
  # ==========================================================
  
  nav_panel(
    "Model Information",
    
    div(
      
      class = "container-fluid",
      
      br(),
      
      # ======================================================
      # MODEL OVERVIEW + PERFORMANCE
      # ======================================================
      
      layout_columns(
        
        # ----------------------------------------------------
        # MODEL OVERVIEW
        # ----------------------------------------------------
        
        card(
          
          card_header(
            
            div(
              class = "model-card-title",
              
              icon("brain"),
              
              span("Model")
            )
          ),
          
          div(
            class = "model-overview-content",
            
            h2(
              "XGBoost",
              class = "model-name"
            ),
            
            p(
              "The final model was selected following ",
              "5-fold cross-validation of six candidate ",
              "classification models."
            ),
            
            div(
              class = "model-list",
              
              div(
                class = "model-item",
                icon("check"),
                span("Logistic Regression")
              ),
              
              div(
                class = "model-item",
                icon("check"),
                span("Elastic Net")
              ),
              
              div(
                class = "model-item",
                icon("check"),
                span("Random Forest")
              ),
              
              div(
                class = "model-item",
                icon("check"),
                span("GBM")
              ),
              
              div(
                class = "model-item",
                icon("check"),
                span("SVM")
              ),
              
              div(
                class = "model-item model-selected",
                icon("check-circle"),
                span("XGBoost")
              )
              
            )
          )
          
        ),
        
        
        # ----------------------------------------------------
        # MODEL PERFORMANCE
        # ----------------------------------------------------
        
        card(
          
          card_header(
            
            div(
              class = "model-card-title",
              
              icon("chart-line"),
              
              span("Model Performance")
            )
          ),
          
          div(
            class = "performance-grid",
            
            # AUROC
            div(
              class = "performance-item",
              
              div(
                class = "performance-icon",
                icon("chart-line")
              ),
              
              div(
                
                div(
                  class = "performance-label",
                  "Test AUROC"
                ),
                
                div(
                  class = "performance-value",
                  "0.687"
                )
                
              )
            ),
            
            # AUPRC
            div(
              class = "performance-item",
              
              div(
                class = "performance-icon",
                icon("chart-area")
              ),
              
              div(
                
                div(
                  class = "performance-label",
                  "Test AUPRC"
                ),
                
                div(
                  class = "performance-value",
                  "0.310"
                )
                
              )
            ),
            
            # THRESHOLD
            div(
              class = "performance-item",
              
              div(
                class = "performance-icon",
                icon("sliders")
              ),
              
              div(
                
                div(
                  class = "performance-label",
                  "Classification Threshold"
                ),
                
                div(
                  class = "performance-value",
                  "20.5%"
                )
                
              )
            )
            
          )
          
        ),
        
        col_widths = c(
          6,
          6
        )
        
      ),
      
      br(),
      
      
      # ======================================================
      # STUDY POPULATION + MODEL FEATURES
      # ======================================================
      
      layout_columns(
        
        # ----------------------------------------------------
        # STUDY POPULATION
        # ----------------------------------------------------
        
        card(
          
          card_header(
            
            div(
              class = "model-card-title",
              
              icon("users"),
              
              span("Study Population")
            )
          ),
          
          div(
            class = "population-grid",
            
            div(
              class = "population-item",
              
              div(
                class = "population-value",
                "2,694"
              ),
              
              div(
                class = "population-label",
                "Total Patients"
              )
              
            ),
            
            div(
              class = "population-item",
              
              div(
                class = "population-value",
                "479"
              ),
              
              div(
                class = "population-label",
                "GI Bleeding"
              )
              
            ),
            
            div(
              class = "population-item",
              
              div(
                class = "population-value",
                "2,215"
              ),
              
              div(
                class = "population-label",
                "No GI Bleeding"
              )
              
            )
            
          ),
          
          hr(),
          
          p(
            class = "small text-muted mb-0",
            
            "The V2 modelling dataset contained 2,694 ",
            "unique patients, including 479 GI bleeding ",
            "and 2,215 non-GI bleeding cases."
          )
          
        ),
        
        
        # ----------------------------------------------------
        # MODEL FEATURES
        # ----------------------------------------------------
        
        card(
          
          card_header(
            
            div(
              class = "model-card-title",
              
              icon("list-check"),
              
              span("Model Features")
            )
          ),
          
          div(
            class = "feature-grid",
            
            # DEMOGRAPHICS
            div(
              
              div(
                class = "feature-heading",
                
                icon("user"),
                
                "Demographics"
              ),
              
              tags$ul(
                
                tags$li("Age"),
                tags$li("Sex"),
                tags$li("Race / ethnicity")
                
              )
              
            ),
            
            # MEDICATION
            div(
              
              div(
                class = "feature-heading",
                
                icon("pills"),
                
                "Medication"
              ),
              
              tags$ul(
                
                tags$li("Prior aspirin"),
                tags$li("Prior naproxen"),
                tags$li("Prior celecoxib")
                
              )
              
            ),
            
            # CONDITIONS
            div(
              
              div(
                class = "feature-heading",
                
                icon("stethoscope"),
                
                "Clinical Conditions"
              ),
              
              tags$ul(
                
                tags$li("Prior peptic ulcer"),
                tags$li("Prior ulcerative colitis"),
                tags$li("Prior esophagitis"),
                tags$li("Prior diverticular disease")
                
              )
              
            )
            
          )
          
        ),
        
        col_widths = c(
          6,
          6
        )
        
      ),
      
      br(),
      
      
      # ======================================================
      # IMPORTANT INFORMATION
      # ======================================================
      
      card(
        
        card_header(
          
          div(
            class = "model-card-title",
            
            icon("circle-info"),
            
            span("Important Information")
          )
          
        ),
        
        div(
          class = "important-info-grid",
          
          # RESEARCH PROTOTYPE
          div(
            
            div(
              class = "info-icon",
              icon("flask")
            ),
            
            div(
              
              h5(
                "Research Prototype"
              ),
              
              p(
                "This application is a research and software ",
                "demonstration prototype. The prediction ",
                "represents the output of a machine-learning ",
                "model and should not be interpreted as a ",
                "clinical diagnosis."
              )
              
            )
            
          ),
          
          # THRESHOLD
          div(
            
            div(
              class = "info-icon",
              icon("sliders")
            ),
            
            div(
              
              h5(
                "Classification Threshold"
              ),
              
              p(
                "The model uses a classification threshold ",
                "of 20.5% for the risk classification displayed ",
                "in this application."
              )
              
            )
            
          ),
          
          # AGE RANGE
          div(
            
            div(
              class = "info-icon",
              icon("calendar")
            ),
            
            div(
              
              h5(
                "Supported Age Range"
              ),
              
              p(
                "The model was developed using patients aged ",
                "33–111 years. Predictions outside this age ",
                "range are not supported."
              )
              
            )
            
          )
          
        )
        
      ),
      
      br()
      
    )
  ),
)


# ============================================================
# 8. SERVER
# ============================================================

server <- function(input, output, session) {
  
  
  # ==========================================================
  # PATIENT DATA
  # ==========================================================
  
  patients <- reactiveVal(
    get_patients()
  )
  
  
  # ==========================================================
  # SAVE PATIENT
  # ==========================================================
  
  observeEvent(
    input$save_patient,
    {
      
      # ------------------------------------------------------
      # Validate Patient ID
      # ------------------------------------------------------
      
      if (
        is.null(input$patient_id) ||
        trimws(input$patient_id) == ""
      ) {
        
        output$save_message <- renderUI({
          
          div(
            class = "alert alert-danger",
            
            strong(
              "Please enter a Patient ID."
            )
          )
          
        })
        
        return()
      }
      
      
      # ------------------------------------------------------
      # Check duplicate Patient ID
      # ------------------------------------------------------
      
      existing_patient <- dbGetQuery(
        con,
        "SELECT patient_id FROM patients WHERE patient_id = ?",
        params = list(
          input$patient_id
        )
      )
      
      
      if (
        nrow(existing_patient) > 0
      ) {
        
        output$save_message <- renderUI({
          
          div(
            class = "alert alert-warning",
            
            strong(
              "This Patient ID already exists."
            ),
            
            br(),
            
            "Please use a different Patient ID."
          )
          
        })
        
        return()
      }
      
      
      # ------------------------------------------------------
      # Map UI race to model race
      # ------------------------------------------------------
      
      model_race <- map_race_to_model(
        input$race
      )
      
      
      # ------------------------------------------------------
      # Create patient record
      # ------------------------------------------------------
      
      patient_record <- data.frame(
        
        patient_id =
          trimws(input$patient_id),
        
        age =
          as.integer(input$age),
        
        sex =
          input$sex,
        
        race =
          model_race,
        
        prior_aspirin =
          as.integer(input$prior_aspirin),
        
        prior_naproxen =
          as.integer(input$prior_naproxen),
        
        prior_celecoxib =
          as.integer(input$prior_celecoxib),
        
        prior_peptic_ulcer =
          as.integer(input$prior_peptic_ulcer),
        
        prior_ulcerative_colitis =
          as.integer(input$prior_ulcerative_colitis),
        
        prior_esophagitis =
          as.integer(input$prior_esophagitis),
        
        prior_diverticular_disease =
          as.integer(input$prior_diverticular_disease),
        
        stringsAsFactors = FALSE
      )
      
      
      # ------------------------------------------------------
      # Save to SQLite
      # ------------------------------------------------------
      
      dbWriteTable(
        con,
        "patients",
        patient_record,
        append = TRUE,
        row.names = FALSE
      )
      
      
      # ------------------------------------------------------
      # Refresh patient list
      # ------------------------------------------------------
      
      patients(
        get_patients()
      )
      
      
      # ------------------------------------------------------
      # Confirmation
      # ------------------------------------------------------
      
      output$save_message <- renderUI({
        
        div(
          class = "alert alert-success",
          
          strong(
            "Patient saved successfully."
          ),
          
          br(),
          
          paste(
            input$patient_id,
            "is now available for risk prediction."
          )
        )
        
      })
      
    }
  )
  
  
  # ==========================================================
  # PATIENT SELECTOR
  # ==========================================================
  
  output$patient_selector <- renderUI({
    
    patient_data <- patients()
    
    if (
      nrow(patient_data) == 0
    ) {
      
      return(
        p(
          class = "text-muted",
          "No patients have been registered yet."
        )
      )
      
    }
    
    selectInput(
      
      "selected_patient",
      
      "Patient ID",
      
      choices =
        patient_data$patient_id
      
    )
    
  })
  
  
  # ==========================================================
  # RUN PREDICTION
  # ==========================================================
  
  observeEvent(
    input$run_prediction,
    {
      
      req(
        input$selected_patient
      )
      
      
      # ------------------------------------------------------
      # Retrieve patient
      # ------------------------------------------------------
      
      patient_data <- patients()
      
      patient <- patient_data %>%
        filter(
          patient_id ==
            input$selected_patient
        )
      
      
      req(
        nrow(patient) == 1
      )
      
      
      # ------------------------------------------------------
      # Safety check for age
      # ------------------------------------------------------
      
      if (
        patient$age < model_min_age ||
        patient$age > model_max_age
      ) {
        
        output$prediction_result <- renderUI({
          
          div(
            class = "alert alert-danger",
            
            h4(
              "Prediction unavailable"
            ),
            
            p(
              paste0(
                "The patient's age is outside the model ",
                "development range of ",
                model_min_age,
                "–",
                model_max_age,
                " years."
              )
            ),
            
            p(
              "The model should not be used to generate ",
              "predictions outside this range."
            )
          )
          
        })
        
        return()
      }
      
      
      # ------------------------------------------------------
      # Create model input
      # ------------------------------------------------------
      
      model_input <- data.frame(
        
        sex = factor(
          patient$sex,
          levels = model_sex_levels
        ),
        
        age =
          patient$age,
        
        race = factor(
          patient$race,
          levels = model_race_levels
        ),
        
        prior_aspirin =
          patient$prior_aspirin,
        
        prior_naproxen =
          patient$prior_naproxen,
        
        prior_celecoxib =
          patient$prior_celecoxib,
        
        prior_peptic_ulcer =
          patient$prior_peptic_ulcer,
        
        prior_ulcerative_colitis =
          patient$prior_ulcerative_colitis,
        
        prior_esophagitis =
          patient$prior_esophagitis,
        
        prior_diverticular_disease =
          patient$prior_diverticular_disease
      )
      
      
      # ------------------------------------------------------
      # Create model matrix
      # ------------------------------------------------------
      
      prediction_matrix <- model.matrix(
        model_formula,
        data = model_input
      )
      
      
      # Remove intercept
      prediction_matrix <-
        prediction_matrix[
          ,
          colnames(prediction_matrix) !=
            "(Intercept)",
          drop = FALSE
        ]
      
      
      # ------------------------------------------------------
      # Ensure exact feature structure
      # ------------------------------------------------------
      
      missing_features <-
        setdiff(
          model_feature_names,
          colnames(prediction_matrix)
        )
      
      
      if (
        length(missing_features) > 0
      ) {
        
        output$prediction_result <- renderUI({
          
          div(
            class = "alert alert-danger",
            
            h4(
              "Prediction error"
            ),
            
            p(
              "The patient data could not be converted ",
              "to the feature structure expected by the model."
            )
          )
          
        })
        
        return()
      }
      
      
      prediction_matrix <-
        prediction_matrix[
          ,
          model_feature_names,
          drop = FALSE
        ]
      
      
      # ------------------------------------------------------
      # XGBoost prediction
      # ------------------------------------------------------
      
      prediction_dmatrix <- xgb.DMatrix(
        data = prediction_matrix
      )
      
      
      predicted_probability <-
        predict(
          final_xgb_model,
          prediction_dmatrix
        )
      
      
      # ------------------------------------------------------
      # Classification
      # ------------------------------------------------------
      
      risk_class <- ifelse(
        predicted_probability >=
          model_threshold,
        
        "HIGH RISK",
        
        "LOW RISK"
      )
      
      
      # ------------------------------------------------------
      # Five-level UI risk classification
      # ------------------------------------------------------
      
      predicted_percentage <-
        predicted_probability * 100
      
      
      risk_level <- case_when(
        
        predicted_percentage <= 10 ~
          "Very Low",
        
        predicted_percentage <= 20.5 ~
          "Low",
        
        predicted_percentage <= 30 ~
          "High",
        
        predicted_percentage <= 40 ~
          "Very High",
        
        TRUE ~
          "Very High / Highest"
      )
      
      
      # ------------------------------------------------------
      # Risk meter position
      # ------------------------------------------------------
      
      pointer_position <-
        min(
          max(
            predicted_percentage,
            0
          ),
          100
        )
      
      
      # ------------------------------------------------------
      # Result colours
      # ------------------------------------------------------
      
      result_class <- ifelse(
        risk_class == "HIGH RISK",
        "risk-high",
        "risk-low"
      )
      
      
      # ------------------------------------------------------
      # Render result
      # ------------------------------------------------------
      
      output$prediction_result <- renderUI({
        
        div(
          
          class = "container-fluid",
          
          br(),
          
          # ==================================================
          # MAIN RESULT
          # ==================================================
          
          card(
            
            class = "border-0 shadow-sm",
            
            div(
              class = "text-center p-4",
              
              h5(
                paste(
                  "Patient:",
                  patient$patient_id
                ),
                class = "text-muted"
              ),
              
              div(
                class = paste(
                  "risk-result",
                  result_class
                ),
                
                h1(
                  risk_class,
                  class = "risk-title"
                ),
                
                p(
                  "Model Risk Classification",
                  class = "risk-subtitle"
                )
                
              )
              
            )
          ),
          
          br(),
          
          # ==================================================
          # RISK METER
          # ==================================================
          
          card(
            
            card_header(
              "Model Risk Scale"
            ),
            
            div(
              class = "risk-meter-container",
              
              div(
                class = "risk-labels",
                
                span("VERY LOW"),
                span("LOW"),
                span("HIGH"),
                span("VERY HIGH"),
                span("HIGHEST")
                
              ),
              
              div(
                class = "risk-meter",
                
                div(
                  class = "risk-meter-segment very-low"
                ),
                
                div(
                  class = "risk-meter-segment low"
                ),
                
                div(
                  class = "risk-meter-segment high"
                ),
                
                div(
                  class = "risk-meter-segment very-high"
                ),
                
                div(
                  class = "risk-meter-segment highest"
                ),
                
                div(
                  class = "risk-pointer",
                  
                  style = paste0(
                    "left:",
                    pointer_position,
                    "%;"
                  )
                )
                
              ),
              
              div(
                class = "risk-meter-scale",
                
                span("0%"),
                span("10%"),
                span("20.5%"),
                span("30%"),
                span("40%"),
                span("100%")
                
              ),
              
              div(
                class = "text-center mt-3",
                
                strong(
                  paste(
                    "Current model classification:",
                    risk_class
                  )
                ),
                
                br(),
                
                span(
                  class = "text-muted",
                  paste(
                    "Risk level:",
                    risk_level
                  )
                )
                
              )
              
            )
          ),
          
          br(),
          
          # ==================================================
          # PATIENT SUMMARY
          # ==================================================
          
          card(
            
            card_header(
              "Patient Summary"
            ),
            
            div(
              class = "patient-summary-row",
              
              # AGE
              div(
                class = "patient-summary-item",
                
                div(
                  class = "summary-icon",
                  icon("calendar")
                ),
                
                div(
                  class = "summary-content",
                  
                  div(
                    class = "summary-label",
                    "Age"
                  ),
                  
                  div(
                    class = "summary-value",
                    patient$age
                  )
                )
              ),
              
              # SEX
              div(
                class = "patient-summary-item",
                
                div(
                  class = "summary-icon",
                  icon("venus-mars")
                ),
                
                div(
                  class = "summary-content",
                  
                  div(
                    class = "summary-label",
                    "Sex"
                  ),
                  
                  div(
                    class = "summary-value",
                    patient$sex
                  )
                )
              ),
              
              # RACE
              div(
                class = "patient-summary-item",
                
                div(
                  class = "summary-icon",
                  icon("users")
                ),
                
                div(
                  class = "summary-content",
                  
                  div(
                    class = "summary-label",
                    "Race"
                  ),
                  
                  div(
                    class = "summary-value",
                    
                    case_when(
                      
                      patient$race ==
                        "Black or African American" ~
                        "Black / African",
                      
                      TRUE ~
                        patient$race
                      
                    )
                  )
                )
              )
              
            )
          ),
          
          br(),
          
          # ==================================================
          # CLINICAL HISTORY
          # ==================================================
          
          # ==================================================
          # CLINICAL & MEDICATION HISTORY
          # ==================================================
          
          layout_columns(
            
            # ==================================================
            # MEDICATION HISTORY
            # ==================================================
            
            card(
              
              card_header(
                
                div(
                  class = "history-card-title",
                  
                  icon("pills"),
                  
                  span(
                    "Medication History"
                  )
                  
                )
                
              ),
              
              div(
                class = "history-list",
                
                div(
                  class = "history-row",
                  
                  span(
                    "Aspirin"
                  ),
                  
                  span(
                    class = ifelse(
                      patient$prior_aspirin == 1,
                      "status-yes",
                      "status-no"
                    ),
                    
                    ifelse(
                      patient$prior_aspirin == 1,
                      "✓ Yes",
                      "✕ No"
                    )
                  )
                  
                ),
                
                div(
                  class = "history-row",
                  
                  span(
                    "Naproxen"
                  ),
                  
                  span(
                    class = ifelse(
                      patient$prior_naproxen == 1,
                      "status-yes",
                      "status-no"
                    ),
                    
                    ifelse(
                      patient$prior_naproxen == 1,
                      "✓ Yes",
                      "✕ No"
                    )
                  )
                  
                ),
                
                div(
                  class = "history-row",
                  
                  span(
                    "Celecoxib"
                  ),
                  
                  span(
                    class = ifelse(
                      patient$prior_celecoxib == 1,
                      "status-yes",
                      "status-no"
                    ),
                    
                    ifelse(
                      patient$prior_celecoxib == 1,
                      "✓ Yes",
                      "✕ No"
                    )
                  )
                  
                )
                
              )
              
            ),
            
            
            # ==================================================
            # CLINICAL CONDITIONS
            # ==================================================
            
            card(
              
              card_header(
                
                div(
                  class = "history-card-title",
                  
                  icon("stethoscope"),
                  
                  span(
                    "Clinical Conditions"
                  )
                  
                )
                
              ),
              
              div(
                class = "history-list",
                
                div(
                  class = "history-row",
                  
                  span(
                    "Peptic ulcer"
                  ),
                  
                  span(
                    class = ifelse(
                      patient$prior_peptic_ulcer == 1,
                      "status-yes",
                      "status-no"
                    ),
                    
                    ifelse(
                      patient$prior_peptic_ulcer == 1,
                      "✓ Yes",
                      "✕ No"
                    )
                  )
                  
                ),
                
                div(
                  class = "history-row",
                  
                  span(
                    "Ulcerative colitis"
                  ),
                  
                  span(
                    class = ifelse(
                      patient$prior_ulcerative_colitis == 1,
                      "status-yes",
                      "status-no"
                    ),
                    
                    ifelse(
                      patient$prior_ulcerative_colitis == 1,
                      "✓ Yes",
                      "✕ No"
                    )
                  )
                  
                ),
                
                div(
                  class = "history-row",
                  
                  span(
                    "Esophagitis"
                  ),
                  
                  span(
                    class = ifelse(
                      patient$prior_esophagitis == 1,
                      "status-yes",
                      "status-no"
                    ),
                    
                    ifelse(
                      patient$prior_esophagitis == 1,
                      "✓ Yes",
                      "✕ No"
                    )
                  )
                  
                ),
                
                div(
                  class = "history-row",
                  
                  span(
                    "Diverticular disease"
                  ),
                  
                  span(
                    class = ifelse(
                      patient$prior_diverticular_disease == 1,
                      "status-yes",
                      "status-no"
                    ),
                    
                    ifelse(
                      patient$prior_diverticular_disease == 1,
                      "✓ Yes",
                      "✕ No"
                    )
                  )
                  
                )
                
              )
              
            ),
            
            col_widths = c(
              6,
              6
            )
            
          ),
          
          br(),
          
          br(),
          
          # ==================================================
          # DISCLAIMER
          # ==================================================
          
          div(
            
            class = "alert alert-warning",
            
            h5(
              "Research Prototype"
            ),
            
            p(
              "This prediction is generated by a ",
              "machine-learning model developed for ",
              "research and software demonstration. ",
              "It should not be interpreted as a ",
              "clinical diagnosis or a substitute for ",
              "professional clinical judgement."
            )
            
          )
        )
      })
      
    }
  )
}


# ============================================================
# 9. CUSTOM CSS
# ============================================================

ui <- tagList(
  
  tags$head(
    
    tags$style(
      
      HTML(
        
        
        "
        /* ==================================================
           GENERAL RESPONSIVENESS
           ================================================== */

        .container-fluid {
          max-width: 1400px;
          margin-left: auto;
          margin-right: auto;
        }

        .card {
          overflow: visible !important;
        }

        .card-body {
          overflow: visible !important;
        }


        /* ==================================================
           RISK RESULT
           ================================================== */

        .risk-result {
          border-radius: 16px;
          padding: 30px 20px;
          width: 100%;
        }

        .risk-high {
          background-color: #dc3545;
          color: white;
        }

        .risk-low {
          background-color: #198754;
          color: white;
        }

        .risk-title {
          font-size: clamp(2rem, 5vw, 3.5rem);
          font-weight: 700;
          margin-bottom: 8px;
        }

        .risk-subtitle {
          font-size: 1.1rem;
          margin-bottom: 0;
        }
                  /* ==================================================
             PATIENT SUMMARY
             ================================================== */
          
          .patient-summary-row {
            display: flex;
            width: 100%;
            gap: 12px;
            padding: 8px 0;
          }
          
          .patient-summary-item {
            flex: 1;
            min-width: 0;
            display: flex;
            align-items: center;
            gap: 12px;
            padding: 16px;
            border: 1px solid #dee2e6;
            border-radius: 10px;
            background: #ffffff;
          }
          
          .summary-icon {
            width: 42px;
            height: 42px;
            min-width: 42px;
            border-radius: 50%;
            display: flex;
            align-items: center;
            justify-content: center;
            background: #e9f2f9;
            color: #2c3e50;
            font-size: 18px;
          }
          
          .summary-content {
            min-width: 0;
          }
          
          .summary-label {
            font-size: 0.8rem;
            color: #6c757d;
            margin-bottom: 3px;
          }
          
          .summary-value {
            font-size: 1.15rem;
            font-weight: 600;
            color: #212529;
            word-break: break-word;
          }
          
          
          /* ==================================================
             HISTORY CARDS
             ================================================== */
          
          .history-card-title {
            display: flex;
            align-items: center;
            gap: 10px;
            font-weight: 600;
          }
          
          .history-card-title i {
            font-size: 18px;
          }
          
          .history-list {
            width: 100%;
          }
          
          .history-row {
            display: flex;
            justify-content: space-between;
            align-items: center;
            padding: 10px 4px;
            border-bottom: 1px solid #eeeeee;
            gap: 10px;
          }
          
          .history-row:last-child {
            border-bottom: none;
          }
          
          .status-yes {
            font-weight: 600;
            color: #198754;
          }
          
          .status-no {
            font-weight: 500;
            color: #6c757d;
          }

        /* ==================================================
           RISK METER
           ================================================== */

        .risk-meter-container {
          padding: 20px 5px 10px 5px;
        }

        .risk-labels {
          display: grid;
          grid-template-columns:
            10% 10.5% 9.5% 10% 60%;

          text-align: center;
          font-size: 0.72rem;
          font-weight: 700;
          margin-bottom: 8px;
          gap: 2px;
        }

        .risk-meter {
          position: relative;
          width: 100%;
          height: 32px;
          display: flex;
          border-radius: 16px;
          overflow: visible;
        }

        .risk-meter-segment {
          height: 100%;
        }

        .risk-meter-segment:first-child {
          border-radius: 16px 0 0 16px;
        }

        .risk-meter-segment:last-of-type {
          border-radius: 0 16px 16px 0;
        }

        .very-low {
          width: 10%;
          background: #198754;
        }

        .low {
          width: 10.5%;
          background: #75b798;
        }

        .high {
          width: 9.5%;
          background: #ffc107;
        }

        .very-high {
          width: 10%;
          background: #fd7e14;
        }

        .highest {
          width: 60%;
          background: #dc3545;
        }

        .risk-pointer {
          position: absolute;
          top: -12px;
          transform: translateX(-50%);
          width: 5px;
          height: 56px;
          background: #212529;
          border-radius: 4px;
          z-index: 5;
          box-shadow: 0 0 0 2px white;
        }

        .risk-pointer::after {
          content: '';
          position: absolute;
          bottom: -7px;
          left: 50%;
          transform: translateX(-50%);
          border-left: 7px solid transparent;
          border-right: 7px solid transparent;
          border-top: 8px solid #212529;
        }

        .risk-meter-scale {
          display: flex;
          justify-content: space-between;
          margin-top: 12px;
          font-size: 0.75rem;
          color: #6c757d;
        }

        /* ==================================================
   MODEL INFORMATION PAGE
   ================================================== */

.model-card-title {
  display: flex;
  align-items: center;
  gap: 10px;
  font-weight: 600;
}

.model-card-title i {
  font-size: 18px;
}


/* ==================================================
   MODEL OVERVIEW
   ================================================== */

.model-overview-content {
  padding: 4px 0;
}

.model-name {
  font-size: 2rem;
  margin-bottom: 12px;
}

.model-list {
  display: grid;
  grid-template-columns: 1fr 1fr;
  gap: 8px;
  margin-top: 16px;
}

.model-item {
  display: flex;
  align-items: center;
  gap: 8px;
  padding: 7px 10px;
  border-radius: 6px;
  background: #f8f9fa;
  font-size: 0.9rem;
}

.model-item i {
  color: #6c757d;
}

.model-selected {
  font-weight: 600;
  background: #e9f7ef;
}

.model-selected i {
  color: #198754;
}


/* ==================================================
   MODEL PERFORMANCE
   ================================================== */

.performance-grid {
  display: grid;
  grid-template-columns: 1fr 1fr;
  gap: 12px;
}

.performance-item {
  display: flex;
  align-items: center;
  gap: 12px;
  padding: 14px;
  border: 1px solid #dee2e6;
  border-radius: 10px;
  min-width: 0;
}

.performance-item:last-child {
  grid-column: 1 / -1;
}

.performance-icon {
  width: 40px;
  height: 40px;
  min-width: 40px;
  border-radius: 50%;
  background: #e9f2f9;
  display: flex;
  align-items: center;
  justify-content: center;
  color: #2c3e50;
}

.performance-label {
  font-size: 0.8rem;
  color: #6c757d;
}

.performance-value {
  font-size: 1.7rem;
  font-weight: 600;
  color: #212529;
}


/* ==================================================
   STUDY POPULATION
   ================================================== */

.population-grid {
  display: grid;
  grid-template-columns: repeat(3, 1fr);
  gap: 10px;
}

.population-item {
  text-align: center;
  padding: 12px 6px;
  border: 1px solid #dee2e6;
  border-radius: 10px;
}

.population-value {
  font-size: 1.5rem;
  font-weight: 600;
}

.population-label {
  font-size: 0.75rem;
  color: #6c757d;
  margin-top: 3px;
}


/* ==================================================
   MODEL FEATURES
   ================================================== */

.feature-grid {
  display: grid;
  grid-template-columns: repeat(3, 1fr);
  gap: 18px;
}

.feature-heading {
  display: flex;
  align-items: center;
  gap: 8px;
  font-weight: 600;
  margin-bottom: 8px;
}

.feature-heading i {
  color: #2c3e50;
}

.feature-grid ul {
  margin-bottom: 0;
  padding-left: 20px;
}

.feature-grid li {
  margin-bottom: 4px;
  font-size: 0.9rem;
}


/* ==================================================
   IMPORTANT INFORMATION
   ================================================== */

.important-info-grid {
  display: grid;
  grid-template-columns: repeat(3, 1fr);
  gap: 20px;
}

.important-info-grid > div {
  display: flex;
  gap: 12px;
  padding: 14px;
  border: 1px solid #dee2e6;
  border-radius: 10px;
  background: #fafafa;
}

.info-icon {
  width: 38px;
  height: 38px;
  min-width: 38px;
  border-radius: 50%;
  display: flex;
  align-items: center;
  justify-content: center;
  background: #fff3cd;
  color: #856404;
}

.important-info-grid h5 {
  margin-top: 0;
  margin-bottom: 6px;
}

.important-info-grid p {
  margin-bottom: 0;
  font-size: 0.88rem;
  color: #495057;
}


/* ==================================================
   MODEL INFORMATION - RESPONSIVE
   ================================================== */

@media (max-width: 768px) {

  .model-list {
    grid-template-columns: 1fr;
  }

  .performance-grid {
    grid-template-columns: 1fr;
  }

  .performance-item:last-child {
    grid-column: auto;
  }

  .population-grid {
    grid-template-columns: 1fr;
  }

  .feature-grid {
    grid-template-columns: 1fr;
  }

  .important-info-grid {
    grid-template-columns: 1fr;
  }

}
        /* ==================================================
           SMALL SCREEN ADJUSTMENTS
           ================================================== */

        @media (max-width: 768px) {

          .risk-labels {
            font-size: 0.55rem;
          }

          .risk-meter-scale {
            font-size: 0.65rem;
          }

          .risk-meter {
            height: 26px;
          }

          .risk-pointer {
            height: 48px;
          }

          .card {
            margin-bottom: 12px;
          }

          .navbar-brand {
            font-size: 1rem;
          }

        }


        @media (max-width: 480px) {

          .risk-labels {
            font-size: 0.45rem;
          }

          .risk-meter-scale {
            font-size: 0.55rem;
          }

          .risk-title {
            font-size: 2rem;
          }

          .risk-result {
            padding: 25px 10px;
          }

        }
        "

      )
    )
  ),
  
  ui
)


# ============================================================
# 10. RUN APPLICATION
# ============================================================

shinyApp(
  ui = ui,
  server = server
)
