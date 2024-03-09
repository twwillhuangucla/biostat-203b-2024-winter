library(shiny)
library(ggplot2)
library(bigrquery)
library(dbplyr)
library(DBI)
library(tidyverse)
library(gt)
library(gtsummary)

#Load datasets
mimic_icu_cohort <- readRDS("./mimic_icu_cohort.rds")

lab_data <- mimic_icu_cohort |>
  pivot_longer(cols = c("Glucose",
                        "Potassium",
                        "White Blood Cells",
                        "Creatinine",
                        "Sodium",
                        "Bicarbonate",
                        "Chloride",
                        "Hematocrit"),
               names_to = "lab_items",
               values_to = "value")|>
  select(c(lab_items, value))

vitals_data <- mimic_icu_cohort |>
  pivot_longer(cols = c("Heart Rate",
                        "Non Invasive Blood Pressure systolic",
                        "Temperature Fahrenheit",
                        "Respiratory Rate",
                        "Non Invasive Blood Pressure diastolic"),
               names_to = "vitals",
               values_to = "value")|>
  mutate(vitals = as.factor(vitals)) |>
  select(vitals, value)


id_list <- mimic_icu_cohort |>
  select(subject_id) |>
  distinct() |>
  pull(subject_id)

# Define UI for dataset viewer app ----
ui <- fluidPage(
  tabsetPanel(
    tabPanel("Patient Characteristics",
             sidebarLayout(
               sidebarPanel(
                 selectInput("variable", 
                             label = "Select a Variable",
                             choices = c("First care unit" = "first_careunit", 
                                         "Last care unit" = "last_careunit", 
                                         "Long stays" = "los_long",
                                         "Admission type" = "admission_type",
                                         "Admission location" = "admission_location",
                                         "Discharge location" = "discharge_location",
                                         "Race" = "race",
                                         "Insurance status" = "insurance",
                                         "Marital status" = "marital_status",
                                         "Gender" = "gender",
                                         "Age" = "age_intime",
                                         "Lab events" = "lab_events",
                                         "Vitals" = "vitals")),
                 checkboxInput("outliers", 
                               "Remove outliers in IQR method for measurements?", F),
               ),
               
               mainPanel(
                 plotOutput("varplot"),
                 gt_output("summary")
               )
             )
  
    ),
    tabPanel("Patient's ADT and ICU stay information",
             sidebarLayout(
               sidebarPanel(
                 selectizeInput("patient_ID", 
                             label = "Select a Patient", 
                             choices = id_list,
                             options = list(maxOptions = 5)),
               ),
               mainPanel(
                 plotOutput("sidplot", height = "500px")
               )
             )
    )
    
  )
)


# Define server logic to summarize and view selected dataset ----
server <- function(input, output) {
  
  #First panel
  #Plot
  output$varplot <- renderPlot({
    if (input$variable == "vitals") {
      ggplot() +
        geom_boxplot(aes(x = vitals, y = value),
                     data = vitals_data,
                     outliers = !input$outliers) +
        coord_flip() +
        labs(title = "Vitals",
             x = "",
             y = "Value")
    }
    
    else if (input$variable == "lab_events") {
      ggplot() +
        geom_boxplot(aes(x = lab_items, y = value),
                     data = lab_data,
                     outliers = !input$outliers) +
        coord_flip() +
        labs(title = "Lab Events",
             x = "",
             y = "Value")
    }
    
    else if (input$variable == "age_intime") {
      mimic_icu_cohort |>
        ggplot() +
        geom_histogram(aes(x = age_intime),
                       binwidth = 1) +
        labs(title = "Age at ICU intime",
             x = "Age",
             y = "Count")
    }
    
    else {
      mimic_icu_cohort |>
        ggplot() +
        geom_bar(aes_string(x = input$variable),
                 stat = "count") +
        coord_flip() +
        labs(title = input$variable,
             x = input$variable,
             y = "Count")
    }
  })
  
  #gtsummary
  output$summary <- render_gt({
    if (input$variable == "vitals") {
      table <- vitals_data |>
        tbl_summary(by = vitals, type = list(value ~ "continuous2"),
                    statistic = list(all_continuous() ~ c("{mean} ({sd})",
                                                          "{median} [{p25}, {p75}]",
                                                          "{min}, ({max})"))) |>
        as_gt()
    }
    
    else if (input$variable == "lab_events") {
      table <- lab_data |>
        tbl_summary(by = lab_items, type = list(value ~ "continuous2"),
                    statistic = list(all_continuous() ~ c("{mean} ({sd})",
                                                          "{median} [{p25}, {p75}]",
                                                          "{min}, ({max})"))) |>
        as_gt()
    }
    
    else if (input$variable == "age_intime") {
      table <- mimic_icu_cohort |>
        select(age_intime) |>
        tbl_summary(type = list(age_intime ~ "continuous2"),
                    statistic = list(all_continuous() ~ c("{mean} ({sd})",
                                                          "{median} [{p25}, {p75}]",
                                                          "{min}, {max}"))) |>
        as_gt()
    }
    
    else {
      table <- mimic_icu_cohort |>
        select(input$variable) |>
        tbl_summary() |>
        as_gt()
    }
  })

  #Second panel
  satoken <- "biostat-203b-2024-winter-313290ce47a6.json"
  bq_auth(path = satoken)
  con_bq <- dbConnect(
    bigrquery::bigquery(),
    project = "biostat-203b-2024-winter",
    dataset = "mimic4_v2_2",
    billing = "biostat-203b-2024-winter"
  )
  
  sid <- reactive({
    as.integer(input$patient_ID)
  })
  
  sid_adt <- reactive({
    tbl(con_bq, "transfers") |>
    filter(subject_id == !!sid(),
           eventtype != "discharge")
  })
  
  sid_adm <- reactive({
    tbl(con_bq, "admissions") |>
    filter(subject_id == !!sid())
  })
  
  sid_lab <- reactive({
    tbl(con_bq, "labevents") |>
    filter(subject_id == !!sid())
  })
  
  sid_proc <- reactive({
    tbl(con_bq, "procedures_icd") |>
    filter(subject_id == !!sid()) |>
    left_join(tbl(con_bq, "d_icd_procedures"), 
              by = c("icd_code","icd_version"))
  })
  
  sid_patients <- reactive({
    tbl(con_bq, "patients") |>
    filter(subject_id == !!sid())
  })
  
  sid_diag <- reactive({
    tbl(con_bq, "diagnoses_icd") |>
    filter(subject_id == !!sid()) |>
    left_join(tbl(con_bq, "d_icd_diagnoses"), 
              by = c("icd_code","icd_version")) |>
    arrange(seq_num)
  })
  
  output$sidplot <- renderPlot({
    ggplot() +
      geom_segment(data = sid_adt(), aes(
        x = intime, xend = outtime,
        y = "ADT", yend = "ADT",
        color = careunit,
        linewidth = str_detect(careunit, "(ICU|CCU)"))) +
      geom_point(data = sid_lab(),
                 aes(
                   x = charttime,
                   y = "Lab"),
                 shape = 3) +
      geom_point(data = sid_proc(), aes(
        x = as.POSIXct(chartdate),
        y = "Procedure",
        shape = long_title)) +
      guides(linewidth = "none", shape = 
               guide_legend(nrow = pull(sid_proc(), long_title) |> 
                              unique() |> length ())) +
      theme_minimal() +
      theme(legend.position = "bottom",
            legend.box = "vertical") +
      labs(
        title = str_c("Patient ", sid(), ", ",
                      pull(sid_patients(), gender), ", ", 
                      pull(sid_patients(), anchor_age), " years old, ",
                      pull(sid_adm(), race)),
        subtitle = str_c(unique(pull(sid_diag(), long_title))[1:3], 
                         collapse = "\n"),
        x = "",
        y = "",
        shape = "Procedure"
      ) +
      scale_y_discrete(limits = c("Procedure", "Lab", "ADT")) +
      scale_shape_manual(values = c(1:(pull(sid_proc(), long_title) |> 
                                         unique() |> length ())))

  })
  
}

# Create Shiny app ----
shinyApp(ui, server)