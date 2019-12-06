
# Setup -------------------------------------------------------------------

library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(shinydashboardPlus)
library(flexdashboard)
library(magrittr)
library(tidyverse)
library(gam)
library(c3)
library(billboarder)
library(shinyjs)

GAM <- readRDS("fitted_GAM.RDS")

# only run this code once to make the file smaller

# GAM <- GAM$imputed
# GAM$nd_dbp = NULL
# 
# GAM$nhtn$mdl$data=NULL
# GAM$nd_sbp$mdl$data=NULL
# 
# GAM$nhtn$mdl$smooth.frame=NULL
# GAM$nd_sbp$mdl$smooth.frame=NULL
# 
# GAM$nhtn$mdl$smooth=NULL
# GAM$nd_sbp$mdl$smooth=NULL
# 
# GAM$nhtn$mdl$residuals=NULL
# GAM$nd_sbp$mdl$residuals=NULL
# 
# GAM$nhtn$mdl$fitted.values=NULL
# GAM$nd_sbp$mdl$fitted.values=NULL
# 
# GAM$nhtn$mdl$effects=NULL
# GAM$nd_sbp$mdl$effects=NULL
# 
# GAM$nhtn$mdl$additive.predictors=NULL
# GAM$nd_sbp$mdl$additive.predictors=NULL
# 
# GAM$nhtn$mdl$weights=NULL
# GAM$nd_sbp$mdl$weights=NULL
# 
# GAM$nhtn$mdl$qr$qr=NULL
# GAM$nd_sbp$mdl$qr$qr=NULL
# 
# GAM$nhtn$mdl$qr$var=NULL
# GAM$nd_sbp$mdl$qr$var=NULL
# 
# GAM$nhtn$mdl$y=NULL
# GAM$nd_sbp$mdl$y=NULL
# 
# saveRDS(GAM, 'fitted_GAM.RDS')

# model_nhtn <- GAM$nhtn$mdl
# model_nd_sbp <- GAM$nd_sbp$mdl
# 
# model_nhtn$model=NULL
# model_nd_sbp$model=NULL


cpnts=readRDS("cut_points.RDS")
sds=readRDS("standard_deviations.RDS")
bounds=readRDS("shiny_variable_boundaries.RDS")

# tst=readRDS("shiny_outcomes_only.RDS")

theme_Publication<-function(base_size=16){
  
  require(ggthemes)
  
  (theme_foundation(base_size=base_size)+ 
      theme(plot.title = element_text(face = "bold",
        size = rel(1), hjust = 0.5),
        text = element_text(),
        panel.background = element_rect(colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA),
        panel.border = element_rect(colour = 'black'),
        legend.key.size = unit(3,"line"),
        legend.key.height = unit(3,"line"),
        axis.title = element_text(face = "bold",size = rel(1)),
        axis.title.y = element_text(angle=90,vjust =2),
        axis.title.x = element_text(vjust = -0.2),
        axis.text = element_text(size=rel(1)),
        axis.line = element_blank(),
        axis.ticks = element_line(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.key = element_rect(colour = NA),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.title = element_text(face="italic"),
        legend.text = element_text(size=rel(1)),
        plot.margin=unit(c(10,5,5,5),"mm"),
        strip.background=element_rect(colour="black",fill="#f0f0f0"),
        strip.text = element_text(face="bold")
      ))
  
}

ilgt<-function(x) exp(x) / (1+exp(x))

dash_sidebar<-dashboardSidebar(
  sidebarMenu(
    menuItem("About", tabName = "about", icon = icon("question")),
    menuItem(
      "Nocturnal hypertension",
      tabName="nhtn",
      icon=icon("user-md"),
      startExpanded = TRUE,
      menuSubItem("Predict my risk", tabName = "nhtn1"),
      menuSubItem("Explain my prediction", tabName = "nhtn2"),
      menuSubItem("Input external data", tabName = "nhtn3")
    ),
    menuItem(
      "Non-dipping SBP",
      tabname='nd_sbp',
      icon=icon("user-md"),
      startExpanded = TRUE,
      menuSubItem("Predict my risk", tabName = "nd_sbp1"),
      menuSubItem("Explain my prediction", tabName = "nd_sbp2"),
      menuSubItem("Input external data", tabName = "nd_sbp3")
    )
  ), 
  width = 275
)


# About tab ---------------------------------------------------------------

about_box1_body <- paste(
  "This application provides support for decisions about",
  "who should be screened for nocturnal hypertension and non-dipping",
  "systolic blood pressure (BP) using 24-hour ambulatory blood pressure",
  "monitoring. Clinicians and researchers may seek to screen adults", 
  "for nocturnal hypertension and non-dipping systolic BP. Also, researchers", 
  "may seek to enroll a cohort of participants with nocturnal hypertension", 
  "to test interventions that lower blood pressure while asleep.",
  "Ambulatory BP monitoring is the primary approach used to identify nocturnal", 
  "hypertension and non-dipping systolic BP.  However, it is not practical to", 
  "conduct ambulatory BP monitoring in all adults.",
  "A more feasible approach is to conduct ambulatory BP monitoring", 
  "screening among adults with a high probability of having these", 
  "nocturnal BP phenotypes. Therefore, we developed predictive equations", 
  "to identify adults with a high probability of having nocturnal", 
  "hypertension or non-dipping systolic BP.",
  "Although the predictive equations in this website are finalized,",
  "the user-interface may undergo future updates based on user feedback"
)

about_box2_body <- paste(
  "Nocturnal hypertension and non-dipping systolic BP",
  "each have their own respective tab that can be accessed",
  "using the controls to the left. Once you click a tab,",
  "it will unfold to show three sub-items that you can navigate to.",
  "For individual predictions, click on the 'Predict my risk'",
  "tab. This will open a menu where you can provide input values",
  "that will be used to generate your predicted probability for",
  "having nocturnal hypertension or non-dipping systolic BP.",
  "If you would like to see how each input variable contributed",
  "to your prediction, this info can be found in the", 
  "'Explain my prediction' tab. If you would like to compute",
  "predictions for an entire dataset, you may upload the data",
  "in the 'Input external data' tab. Instructions and an illustrative",
  "input data set (with fake data) are provided."
)

about_box1_title <- paste(
  "Welcome to the nocturnal blood pressure pattern risk calculator"
)

about_box2_title <- paste(
  "How to use this application"
)

b64 <- base64enc::dataURI(file="www/prof_pic.jfif")

about_tab<-tabItem(
  tabName = 'about',
  fluidRow(
    boxPlus(
      tags$p(about_box1_body, style = "font-size: 150%;"),
      title = tags$p(about_box1_title, style="font-size: 175%"),
      solidHeader = TRUE,
      width = 12,
      status = 'primary',
      closable = FALSE,
      collapsible = TRUE,
      enable_dropdown = TRUE,
      dropdown_menu = dropdownItemList(
        dropdownItem(
          url = "https://www.ahajournals.org/journal/hyp", 
          name = "Link to Hypertension"
        )
      )
    )
  ), 
  fluidRow(
    infoBox(title = "What is Nocturnal Hypertension?", 
      value = "Mean systolic blood pressure ≥ 120 mm Hg or diastolic blood pressure ≥ 70 mm Hg while asleep",
      subtitle = NULL,
      icon = shiny::icon("first-aid"), color = "light-blue", width = 6,
      href = NULL, fill = FALSE),
    infoBox(title = "What is Non-dipping Systolic Blood Pressure (BP)?", 
      value = "A decline in systolic blood pressure < 10% when asleep versus awake",
      subtitle = NULL,
      icon = shiny::icon("chart-line"), color = "light-blue", width = 6,
      href = NULL, fill = FALSE)
  ), fluidRow(
    infoBox(title = "Why should I care about nocturnal blood pressure patterns?", 
      value = "Nocturnal hypertension and non-dipping systolic BP are each associated with an increased risk for cardiovascular disease events independent of BP measured in the clinic setting.",
      subtitle = NULL,
      icon = shiny::icon("heart"), color = "light-blue", width = 12,
      href = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3172086/", fill = FALSE)
  ), fluidRow(
    box(
      tags$p(about_box2_body, style = "font-size: 150%;"),
      title = tags$p(about_box2_title, style="font-size: 175%"),
      solidHeader = TRUE,
      width = 12,
      status = 'primary',
      collapsible = TRUE
    )
  ), fluidRow(
    widgetUserBox(
      title = "Byron C. Jaeger, PhD",
      subtitle = "Lead Developer",
      width = 12,
      type = 2,
      src = b64,
      boxToolSize = 'md',
      color = "primary",
      "Assistant professor of biostatistics at the University of Alabama at Birmingham",
      footer = "Contact: bcjaeger at uab dot edu"
    )
  )
)



# Nocturnal hypertension --------------------------------------------------
# Tab 1 -------------------------------------------------------------------

nhtn_tab1<-tabItem(tabName = 'nhtn1',
  # Info boxes --------------------------------------------------------------
  
  fluidRow(
    infoBox(title = "How do I use this?", 
      value = "Fill in your information below and then click 'Compute prediction' ",
      subtitle = NULL,
      icon = shiny::icon("first-aid"), color = "light-blue", width = 6,
      href = NULL, fill = FALSE),
    infoBox(title = "What if I don't know all of this information?", 
      value = "The default values may be used as an educated guess, but this will reduce the accuracy of your prediction",
      subtitle = NULL,
      icon = shiny::icon("chart-line"), color = "light-blue", width = 6,
      href = NULL, fill = FALSE)
  ),
  
  # Inputs ------------------------------------------------------------------
  
  fluidRow(
    box(
      width=12,
      solidHeader = TRUE,
      title=tags$p(
        'Input values for your nocturnal hypertension predicted probability',
        style="font-size: 175%"),
      status='primary',
      collapsible = TRUE,
      column(
        width=4,
        numericInput('nhtn_age',"Age in years",
          min=bounds$age$lower,
          max=bounds$age$upper,
          value=round(bounds$age$middle*sds['age'])),
        
        numericInput('nhtn_height_cm',"Height in inches",
          min=round(bounds$height_cm$lower*0.393701),
          max=round(bounds$height_cm$upper*0.393701),
          value=round(bounds$height_cm$middle*sds['height_cm']*0.393701)),
        
        numericInput('nhtn_neck_cm',"Neck circumference in inches",
          min=floor(bounds$neck_cm$lower*0.393701),
          max=ceiling(bounds$neck_cm$upper*0.393701),
          value=round(bounds$neck_cm$middle*sds['neck_cm']*0.393701))
      ),
      
      column(
        width=4,
        
        numericInput('nhtn_hdl',"HDL cholesterol in mg/dL",
          min=bounds$hdl$lower,
          max=bounds$hdl$upper,
          value=round(bounds$hdl$middle*sds['hdl'])),
        
        # numericInput('glucose',"Fasting blood glucose in mg/dL",
        #   min=bounds$glucose$lower,
        #   max=bounds$glucose$upper,
        #   value=round(bounds$glucose$middle*sds['glucose'])),
        
        numericInput('nhtn_cln_sbp',"Clinic systolic blood pressure in mm Hg",
          min=bounds$cln_sbp$lower,
          max=bounds$cln_sbp$upper,
          value=round(bounds$cln_sbp$middle*sds['cln_sbp'])),
        
        numericInput('nhtn_cln_dbp',"Clinic diastolic blood pressure in mm Hg",
          min=bounds$cln_dbp$lower,
          max=bounds$cln_dbp$upper,
          value=round(bounds$cln_dbp$middle*sds['cln_dbp']))
        
      ), 
      
      column(
        width=4,
        
        selectInput(
          'nhtn_smoke',
          'Smoking status', 
          choices=c("Never","Former","Current"),
          selected='Black'
        ),
        
        selectInput(
          'nhtn_race',
          'Race', 
          choices=c("Asian","Black","Other","White"),
          selected='Black'
        ),
        
        numericInput(
          'nhtn_acr_nmr',
          "Albumin to creatinine ratio in mg/g",
          min = 0,
          max = 100,
          value = 0.50
        )
      ),
      actionButton(
        "do_nhtn", "Compute prediction", 
        width = '100%', icon = icon("cog"), 
        style="color: #fff; background-color: #337ab7; border-color: #2e6da4"
      ),
      br(),br(),
      actionButton(
        "reset_nhtn", "Reset the inputs to default values",
        width = '100%', icon = icon("cog"), 
        style="color: #fff; background-color: #337ab7; border-color: #2e6da4"
      )
    )
  ), 
  
  
  # Outputs -----------------------------------------------------------------
  
  
  
  fluidRow(
    box(
      tags$p(
        "Predictions will appear after hitting the 'Compute prediction' button", 
        style = "font-size: 150%;"
      ),
      title = tags$p(
        "Your predicted probability",
        style="font-size: 175%"
      ),
      solidHeader = TRUE,
      width = 12,
      status = 'primary',
      collapsible = TRUE,
      billboarderOutput("nhtn_prob_gauge", height='300px')
    )
    
    # box(
    #   tags$p(
    #     "Screening recommendation wil appear after hitting the 'Compute prediction' button", 
    #     style = "font-size: 150%;"
    #   ),
    #   title = tags$p(
    #     "Your screening recommendation",
    #     style="font-size: 175%"
    #   ),
    #   solidHeader = TRUE,
    #   width = 6,
    #   status = 'primary',
    #   collapsible = TRUE,
    #   h3(
    #     uiOutput("nhtn_screen_rec"), 
    #     style='font-size: 175%'
    #   )
    # )
    
  )
)



# Tab 2 -------------------------------------------------------------------

nhtn_tab2 <- tabItem(tabName = 'nhtn2',
  
  # Info boxes --------------------------------------------------------------
  
  fluidRow(
    infoBox(title = "How was predicted probability computed?", 
      value = "The figure below shows how much each variable contributed to your prediction",
      subtitle = NULL,
      icon = shiny::icon("first-aid"), color = "light-blue", width = 6,
      href = NULL, fill = FALSE),
    infoBox(title = "How do I make the figure?", 
      value = "Provide inputs on the 'Predict my risk' tab and hit the 'compute prediction' button.",
      subtitle = NULL,
      icon = shiny::icon("first-aid"), color = "light-blue", width = 6,
      href = NULL, fill = FALSE)
  ),
  fluidRow(
    box(
      width=12,
      solidHeader = TRUE,
      title=tags$p(
        'How each variable impacts your predicted probability for having nocturnal hypertension',
        style="font-size: 175%"),
      status='primary',
      collapsible = TRUE,
      plotOutput('nhtn_explained', height='100%')
    )
  )
)


# Tab 3 -------------------------------------------------------------------

nhtn_tab3 <- tabItem(tabName = 'nhtn3',
  # Input of nocturnal hypertension dataset -------------------------------  
  fluidRow(
    box(
      tags$p("STEP 1: Remove any rows that have missing values for at least one of the predictors in the predictive equation for nocturnal hypertension. Uploading a dataset with missing values will cause an error.", style='font-size: 150%'),
      tags$p("STEP 2: Check that your data are formatted using the right units.", style='font-size: 150%'),
      HTML(
        paste(
          tags$div(
            tags$ul(
              tags$li("Age should be in years",style="font-size: 150%"),
              tags$li("Race categories are Black, White, Asian, and Other",style="font-size: 150%"),
              tags$li("Smoking categories are Current, Former, and Never",style="font-size: 150%"),
              tags$li("Height should be in centimeters",style="font-size: 150%"),
              tags$li("Neck circumference should be in centimeters",style="font-size: 150%"),
              tags$li("Clinic systolic and diastolic blood pressure should be in mm Hg",style="font-size: 150%"),
              tags$li("HDL cholesterol should be in mg/dL",style="font-size: 150%"),
              #tags$li("Fasting blood glucose should be in mg/dL",style="font-size: 150%"),
              tags$li("Urinary albumin-to-creatinine ratio should be in log mg/g plus 1. For example, if the albumin-to-creatinine ratio is 0.5 mg/g, then the value in your data should be log(0.5 + 1) ",style="font-size: 150%")
            )
          )
        )
      ),
      tags$p("STEP 3: Inspect the distribution of each continuous predictor variable in your data. Compare the upper and lower quantiles of your variables with the upper and lower bounds of the slider inputs in the 'Predict my risk' subtab under the 'Nocturnal hypertension' tab. Our predictive equation is meant to be applied to data with predictor variables in these boundaries, and we have not assessed its performance on data outside of them.", style='font-size: 150%'),
      fileInput(
        "file_nhtn", 
        tags$p(
          "STEP 4: Upload your data here as a .csv file",
          style='font-size: 150%'),
        accept = c(
          "text/csv",
          "text/comma-separated-values,text/plain",
          ".csv"),
        width='100%'
      ),
      solidHeader = TRUE,
      status = 'primary',
      title=tags$p(
        'Data upload instructions',
        style="font-size: 175%"),
      width=12,
      collapsible = TRUE
    )
  ),
  fluidRow(
    box(
      DT::dataTableOutput('fake_data_nhtn'),
      solidHeader = TRUE,
      status = 'primary',
      title=tags$p(
        'This is how your data should look',
        style="font-size: 175%"),
      width=6
    ),
    box(
      DT::dataTableOutput('indata_nhtn'),
      solidHeader = TRUE,
      status = 'primary',
      title=tags$p(
        'This is your data',
        style="font-size: 175%"),
      width=6
    )
  ),
  fluidRow(
    box(
      tags$p(
        "After uploading your data, you can download a .csv file with predicted probabilities for nocturnal hypertension by clicking the button directly below this text.",
        style="font-size: 150%"),
      downloadButton(
        "download_nhtn_data", 
        label="Download predictions by clicking this button after you have uploaded a valid dataset",
        style="color: #fff; background-color: #337ab7; border-color: #2e6da4"
      ),
      solidHeader = TRUE,
      status = 'primary',
      title=tags$p(
        'How to download your data with predicted probabilities for nocturnal hypertension',
        style="font-size: 175%"),
      width=12
    )
  )
)


# Non-dipping systolic blood pressure -------------------------------------
# Tab 1 -------------------------------------------------------------------

nd_sbp_tab1 <- tabItem(tabName = 'nd_sbp1',
  # Info boxes --------------------------------------------------------------
  
  fluidRow(
    infoBox(title = "How do I use this?", 
      value = "Fill in your information below and then click 'Compute prediction' ",
      subtitle = NULL,
      icon = shiny::icon("first-aid"), color = "light-blue", width = 6,
      href = NULL, fill = FALSE),
    infoBox(title = "What if I don't know all of this information?", 
      value = "The default values may be used as an educated guess, but this will reduce the accuracy of your prediction",
      subtitle = NULL,
      icon = shiny::icon("chart-line"), color = "light-blue", width = 6,
      href = NULL, fill = FALSE)
  ),
  
  # Inputs ------------------------------------------------------------------
  
  fluidRow(
    
    box(
      width=12,
      solidHeader = TRUE,
      title=tags$p(
        'Input values for your non-dipping systolic blood pressure predicted probability',
        style="font-size: 175%"
      ),
      status='primary',
      collapsible = TRUE,
      column(
        width=4,
        numericInput('nd_sbp_age',"Age in years",
          min=bounds$age$lower,
          max=bounds$age$upper,
          value=round(bounds$age$middle*sds['age'])),
        
        numericInput('nd_sbp_height_cm',"Height in inches",
          min=round(bounds$height_cm$lower*0.393701),
          max=round(bounds$height_cm$upper*0.393701),
          value=round(bounds$height_cm$middle*sds['height_cm']*0.393701))
      ),
      
      column(
        width=4,
        
        numericInput('nd_sbp_waist_cm',"Waist circumference in inches",
          min=floor(bounds$waist_cm$lower*0.393701),
          max=ceiling(bounds$waist_cm$upper*0.393701),
          value=round(bounds$waist_cm$middle*sds['waist_cm']*0.393701)),
        
        numericInput('nd_sbp_hdl',"HDL cholesterol in mg/dL",
          min=bounds$hdl$lower,
          max=bounds$hdl$upper,
          value=round(bounds$hdl$middle*sds['hdl'])),
        
        numericInput('nd_sbp_acr_nmr',"Albumin to creatinine ratio in mg/g",
          min = 0,
          max = 100,
          value = 0.50)
        
      ), 
      
      column(
        width=4,
        
        selectInput(
          'nd_sbp_sex',
          'Sex', 
          choices=c("Female","Male"),
          selected='Female'
        ),
        
        selectInput(
          'nd_sbp_alcohol',
          'Do you drink alcohol?', 
          choices=c("No","Yes"),
          selected="No"
        ),
        
        selectInput(
          'nd_sbp_race',
          'Race', 
          choices=c("Asian","Black","Other","White"),
          selected='Black'
        )
        
      ),
      actionButton(
        "do_nd_sbp", 
        "Compute prediction", 
        width = '100%', icon = icon("cog"), 
        style="color: #fff; background-color: #337ab7; border-color: #2e6da4"
      ),
      br(),br(),
      actionButton(
        "reset_nd_sbp", "Reset the inputs to default values",
        width = '100%', icon = icon("cog"), 
        style="color: #fff; background-color: #337ab7; border-color: #2e6da4"
      )
    )
  ), 
  
  
  # Outputs -----------------------------------------------------------------
  
  fluidRow(
    box(
      tags$p(
        "Predictions will appear after hitting the 'Compute prediction' button", 
        style = "font-size: 150%;"
      ),
      title = tags$p(
        "Your predicted probability",
        style="font-size: 175%"
      ),
      solidHeader = TRUE,
      width = 12,
      status = 'primary',
      collapsible = TRUE,
      billboarderOutput("nd_sbp_prob_gauge", height='300px')
    )
    
    # box(
    #   tags$p(
    #     "Screening recommendation wil appear after hitting the 'Compute prediction' button", 
    #     style = "font-size: 150%;"
    #   ),
    #   title = tags$p(
    #     "Your screening recommendation",
    #     style="font-size: 175%"
    #   ),
    #   solidHeader = TRUE,
    #   width = 6,
    #   status = 'primary',
    #   collapsible = TRUE,
    #   h3(
    #     uiOutput("nd_sbp_screen_rec"), 
    #     style='font-size: 175%'
    #   )
    # )
    
  )
)



# Tab 2 -------------------------------------------------------------------

nd_sbp_tab2 <- tabItem(tabName = 'nd_sbp2',
  
  # Info boxes --------------------------------------------------------------
  
  fluidRow(
    infoBox(title = "How was predicted probability computed?", 
      value = "The figure below shows how much each variable contributed to your prediction",
      subtitle = NULL,
      icon = shiny::icon("first-aid"), color = "light-blue", width = 6,
      href = NULL, fill = FALSE),
    infoBox(title = "How do I make the figure?", 
      value = "Provide inputs on the 'Predict my risk' tab and hit the 'compute prediction' button.",
      subtitle = NULL,
      icon = shiny::icon("first-aid"), color = "light-blue", width = 6,
      href = NULL, fill = FALSE)
  ),
  fluidRow(
    box(
      width=12,
      solidHeader = TRUE,
      title=tags$p(
        'How each variable impacts your predicted probability for having non dipping systolic blood pressure',
        style="font-size: 175%"),
      status='primary',
      collapsible = TRUE,
      plotOutput('nd_sbp_explained', height='100%')
    )
  )
)


# Tab 3 -------------------------------------------------------------------

nd_sbp_tab3 <- tabItem(tabName = 'nd_sbp3',
  # Input of non dipping systolic blood pressure dataset -------------------------------  
  fluidRow(
    box(
      tags$p("STEP 1: Remove any rows that have missing values for at least one of the predictors in the predictive equation for non dipping systolic blood pressure. Uploading a dataset with missing values will cause an error.", style='font-size: 150%'),
      tags$p("STEP 2: Check that your data are formatted using the right units.", style='font-size: 150%'),
      HTML(
        paste(
          tags$div(
            tags$ul(
              tags$li("Age should be in years",style="font-size: 150%"),
              tags$li("Race categories are Black, White, Asian, and Other",style="font-size: 150%"),
              tags$li("Sex categories are Female and Male",style="font-size: 150%"),
              tags$li("Alcohol categories are None and Some",style="font-size: 150%"),
              tags$li("Height should be in centimeters",style="font-size: 150%"),
              tags$li("Waist circumference should be in centimeters",style="font-size: 150%"),
              tags$li("HDL cholesterol should be in mg/dL",style="font-size: 150%"),
              #tags$li("Fasting blood glucose should be in mg/dL",style="font-size: 150%"),
              tags$li("Urinary albumin-to-creatinine ratio should be in log mg/g plus 1. For example, if the albumin-to-creatinine ratio is 0.5 mg/g, then the value in your data should be log(0.5 + 1) ",style="font-size: 150%")
            )
          )
        )
      ),
      tags$p("STEP 3: Inspect the distribution of each continuous predictor variable in your data. Compare the upper and lower quantiles of your variables with the upper and lower bounds of the slider inputs in the 'Predict my risk' subtab under the 'non dipping systolic blood pressure' tab. Our predictive equation is meant to be applied to data with predictor variables in these boundaries, and we have not assessed its performance on data outside of them.", style='font-size: 150%'),
      fileInput(
        "file_nd_sbp", 
        tags$p(
          "STEP 4: Upload your data here as a .csv file",
          style='font-size: 150%'),
        accept = c(
          "text/csv",
          "text/comma-separated-values,text/plain",
          ".csv"),
        width='100%'
      ),
      solidHeader = TRUE,
      status = 'primary',
      title=tags$p(
        'Data upload instructions',
        style="font-size: 175%"),
      width=12,
      collapsible = TRUE
    )
  ),
  fluidRow(
    box(
      DT::dataTableOutput('fake_data_nd_sbp'),
      solidHeader = TRUE,
      status = 'primary',
      title=tags$p(
        'This is how your data should look',
        style="font-size: 175%"),
      width=6
    ),
    box(
      DT::dataTableOutput('indata_nd_sbp'),
      solidHeader = TRUE,
      status = 'primary',
      title=tags$p(
        'This is your data',
        style="font-size: 175%"),
      width=6
    )
  ),
  fluidRow(
    box(
      tags$p(
        "After uploading your data, you can download a .csv file with predicted probabilities for non dipping systolic blood pressure by clicking the button directly below this text.",
        style="font-size: 150%"),
      downloadButton(
        "download_nd_sbp_data", 
        label="Download predictions by clicking this button after you have uploaded a valid dataset",
        style="color: #fff; background-color: #337ab7; border-color: #2e6da4"
      ),
      solidHeader = TRUE,
      status = 'primary',
      title=tags$p(
        'How to download your data with predicted probabilities for non-dipping systolic blood pressure',
        style="font-size: 175%"),
      width=12
    )
  )
)


# UI commit ---------------------------------------------------------------

dash_body<-dashboardBody(
  tabItems(
    about_tab,
    nhtn_tab1,
    nhtn_tab2,
    nhtn_tab3,
    nd_sbp_tab1,
    nd_sbp_tab2,
    nd_sbp_tab3
  )
)


ui <- dashboardPage(
  useShinyjs(),
  header=dashboardHeader(title = 'ABPM Screener', titleWidth = 275),
  sidebar = dash_sidebar,
  body = dash_body
)

# input = list(
#   nhtn_age=bounds$age$lower,
#   nhtn_race='Black',
#   nhtn_smoke='Current',
#   nhtn_neck_cm=16,
#   nhtn_height_cm=60,
#   nhtn_cln_sbp=bounds$cln_sbp$lower,
#   nhtn_cln_dbp=bounds$cln_dbp$lower,
#   nhtn_hdl=bounds$hdl$lower,
#   nhtn_acr_nmr=0.75
# )

# input = list(
#   nd_sbp_age=bounds$age$lower,
#   nd_sbp_race='Black',
#   nd_sbp_sex = 'Male',
#   nd_sbp_alcohol='Yes',
#   nd_sbp_height_cm=60,
#   nd_sbp_waist_cm=40,
#   nd_sbp_hdl= 40,
#   nd_sbp_acr_nmr=2
# )

# Server computations -----------------------------------------------------

server <- function(input, output) { 
  
  invec_nhtn<-eventReactive(
    input$do_nhtn,{
      data.frame(
        age=input$nhtn_age/sds['age'],
        race=input$nhtn_race,
        smoke=input$nhtn_smoke,
        neck_cm=input$nhtn_neck_cm*2.54/sds['neck_cm'],
        height_cm=input$nhtn_height_cm*2.54/sds['height_cm'],
        cln_sbp=input$nhtn_cln_sbp/sds['cln_sbp'],
        cln_dbp=input$nhtn_cln_dbp/sds['cln_dbp'],
        hdl=input$nhtn_hdl/sds['hdl'],
        #glucose=input$glucose/sds['glucose'],
        acr_nmr=log(input$nhtn_acr_nmr+1)
      )
    })
  
  observeEvent(input$reset_nhtn, 
    {
      reset("nhtn_age")
      reset("nhtn_race")
      reset("nhtn_smoke")
      reset("nhtn_neck_cm")
      reset("nhtn_height_cm")
      reset("nhtn_cln_sbp")
      reset("nhtn_cln_dbp")
      reset("nhtn_hdl")
      reset("nhtn_acr_nmr")
    }
  )
  
  invec_nd_sbp<-eventReactive(
    input$do_nd_sbp,{
      data.frame(
        age=input$nd_sbp_age/sds['age'],
        race=input$nd_sbp_race,
        sex=input$nd_sbp_sex,
        alcohol=if (input$nd_sbp_alcohol=='No') "None" else "Some",
        waist_cm=input$nd_sbp_waist_cm*2.54/sds['waist_cm'],
        height_cm=input$nd_sbp_height_cm*2.54/sds['height_cm'],
        hdl=input$nd_sbp_hdl/sds['hdl'],
        #glucose=input$glucose/sds['glucose'],
        acr_nmr=log(input$nd_sbp_acr_nmr+1)
      )
    })
  
  observeEvent(input$reset_nd_sbp, 
    {
      reset("nd_sbp_age")
      reset("nd_sbp_race")
      reset("nd_sbp_sex")
      reset("nd_sbp_alcohol")
      reset("nd_sbp_waist_cm")
      reset("nd_sbp_height_cm")
      reset("nd_sbp_hdl")
      reset("nd_sbp_acr_nmr")
    }
  )
  
  nhtn_pred <- reactive({
    
    prd <- 
      predict(GAM$nhtn$mdl,newdata=invec_nhtn(),type='response') %>%
      as.numeric()  
    
    list(
      numeric=prd,
      character=prd %>% 
        multiply_by(100) %>% 
        round(1) %>% 
        paste0('%')
    )
    
  })
  
  nd_sbp_pred <- reactive({
    
    prd <- 
      predict(GAM$nd_sbp$mdl,newdata=invec_nd_sbp(),type='response') %>%
      as.numeric()  
    
    list(
      numeric = prd,
      character = prd %>% 
        multiply_by(100) %>% 
        round(1) %>% 
        paste0('%')
    )
    
  })
  
  # The order is critical here and must match the order of terms 
  # in the GAM model object
  nhtn_explained <- eventReactive(input$do_nhtn,{
    data.frame(
      age=paste("Age =", input$nhtn_age,'years'),
      neck_cm=paste("Neck Circ. =",input$nhtn_neck_cm,"inches"),
      race=paste("Race =", input$nhtn_race),
      smoke=paste("Smoking status =", input$nhtn_smoke),
      height_cm=paste("Height =", input$nhtn_height_cm,"inches"),
      cln_sbp=paste("Sys. BP =",input$nhtn_cln_sbp,'mm Hg'),
      cln_dbp=paste("Dia BP =",input$nhtn_cln_dbp,'mm Hg'),
      hdl=paste("HDL Chol. =",input$nhtn_hdl,'mg/dL'),
      acr_nmr=paste("ACR =",input$nhtn_acr_nmr,'mg/g'),
      stringsAsFactors = FALSE
    )
  })
  
  # The order is critical here and must match the order of terms 
  # in the GAM model object
  nd_sbp_explained <- eventReactive(input$do_nd_sbp,{
    data.frame(
      age=paste("Age =", input$nd_sbp_age,'years'),
      sex=paste("Sex =", input$nd_sbp_sex),
      race=paste("Race =", input$nd_sbp_race),
      alcohol=paste("Alcohol =", input$nd_sbp_alcohol),
      height_cm=paste("Height =", input$nd_sbp_height_cm,"inches"),
      waist_cm=paste("Waist Circ. =",input$nd_sbp_waist_cm,"inches"),
      hdl=paste("HDL Chol. =",input$nd_sbp_hdl,'mg/dL'),
      acr_nmr=paste("ACR =",input$nd_sbp_acr_nmr,'mg/g'),
      stringsAsFactors = FALSE
    )
  })
  
  output$nhtn_explained <- renderPlot({
    
    ans=predict(GAM$nhtn$mdl,newdata=invec_nhtn(),type='response')
    prd=predict(GAM$nhtn$mdl,newdata=invec_nhtn(),type='terms')
    cnst=attr(prd,'constant')
    prd=as.numeric(prd)
    bump=diff(range(prd))*0.075
    expl = nhtn_explained() %>% tidyr::gather(variable,value)%>%
      mutate(
        contr=prd,
        hjust=ifelse(contr>0, -bump, bump),
        fill=ifelse(contr>0, 'increase','decrease'),
        modif=case_when(
          variable %in% c(
            "neck_cm","smoke","cln_sbp","cln_dbp","hdl","acr_nmr"
          ) ~ "slateblue2",
          TRUE ~ "black"
        )
      ) %>% 
      dplyr::arrange(contr)
    
    p = ggplot(
      expl,
      aes(
        x=reorder(value,contr),
        y=contr,
        fill=fill,
        label=format(round(contr,2),nsmall=2)
      )
    ) +
      geom_bar(stat='identity',col='black')+
      #geom_text(aes(y=hjust),size=6)+
      #coord_flip()+
      theme_Publication()+
      theme(axis.text.x = element_text(angle=45,vjust=1/2))+
      scale_fill_manual(values=c("cornflowerblue","lightsalmon3"))+
      labs(
        y='Impact on log-odds of nocturnal hypertension',
        x='\nInput values (modifiable variables are purple)',
        fill='This variable causes \npredicted log-odds to...') +
      theme(
        axis.text.x = element_text(color=expl$modif),
        legend.position = 'top'
      )
    
    print(p)
    
  }, height=650)
  
  output$nd_sbp_explained <- renderPlot({
    
    ans=predict(GAM$nd_sbp$mdl,newdata=invec_nd_sbp(),type='response')
    prd=predict(GAM$nd_sbp$mdl,newdata=invec_nd_sbp(),type='terms')
    cnst=attr(prd,'constant')
    prd=as.numeric(prd)
    bump=diff(range(prd))*0.075
    
    expl = nd_sbp_explained() %>% tidyr::gather(variable,value)%>%
      mutate(
        contr=prd,
        hjust=ifelse(contr>0, -bump, bump),
        fill=ifelse(contr>0, 'increase','decrease'),
        modif=case_when(
          variable %in% c(
            "neck_cm","smoke","waist_cm","alcohol","hdl","acr_nmr"
          ) ~ "slateblue2",
          TRUE ~ "black"
        )
      ) %>% 
      dplyr::arrange(contr)
    
    p = ggplot(
      expl,
      aes(
        x=reorder(value,contr),
        y=contr,
        fill=fill,
        label=format(round(contr,2),nsmall=2)
      )
    ) +
      geom_bar(stat='identity',col='black')+
      #geom_text(aes(y=hjust),size=6)+
      #coord_flip()+
      theme_Publication()+
      theme(axis.text.x = element_text(angle=45,vjust=1/2))+
      scale_fill_manual(values=c("cornflowerblue","lightsalmon3"))+
      labs(
        y='Impact on log-odds of nocturnal hypertension',
        x='\nInput values (modifiable variables are purple)',
        fill='This variable causes \npredicted log-odds to...') +
      theme(
        axis.text.x = element_text(color=expl$modif),
        legend.position = 'top'
      )
    
    print(p)
    
  }, height=650)
  
  output$nhtn_prob_gauge <- renderBillboarder({
    billboarder() %>% 
      bb_gaugechart(
        value=nhtn_pred()$numeric*100,
        name='Predicted probability for nocturnal hypertension',
        steps_color=c("#60B044", "#F6C600", "#F97600"),
        steps=c(
          as.numeric(cpnts$nhtn['sens']*100), 
          as.numeric(cpnts$nhtn['ppv']*100), 
          100)
      )
  })
  
  output$nd_sbp_prob_gauge <- renderBillboarder({
    billboarder() %>% 
      bb_gaugechart(
        value=nd_sbp_pred()$numeric*100,
        name='Predicted probability for nocturnal hypertension',
        steps_color=c("#60B044", "#F6C600", "#F97600"),
        steps=c(
          as.numeric(cpnts$nd_sbp['sens']*100), 
          as.numeric(cpnts$nd_sbp['ppv']*100), 
          100)
      )
  })
  
  
  InData_nhtn<-reactive({
    inFile <- input$file_nhtn
    if (is.null(inFile)){
      NULL 
    } else {
      outdat<-tmpdat<-read.csv(inFile$datapath)
      cntn=names(which(sapply(outdat,is.numeric)))
      sds=readRDS("standard_deviations.RDS")
      sds=sds[cntn]
      sds['acr_nmr']=1
      
      for(f in cntn){
        tmpdat[[f]]%<>%divide_by(sds[f])
      }
      
      outdat$prob_nhtn=predict(GAM$nhtn$mdl,newdata=tmpdat,type='response')
      outdat
    }
  })
  
  InData_nd_sbp<-reactive({
    inFile <- input$file_nd_sbp
    if (is.null(inFile)){
      NULL 
    } else {
      outdat<-tmpdat<-read.csv(inFile$datapath)
      cntn=names(which(sapply(outdat,is.numeric)))
      sds=readRDS("standard_deviations.RDS")
      sds=sds[cntn]
      sds['acr_nmr']=1
      
      for(f in cntn){
        tmpdat[[f]]%<>%divide_by(sds[f])
      }
      
      outdat$prob_nd_sbp=predict(GAM$nd_sbp$mdl,newdata=tmpdat,type='response')
      outdat
    }
  })
  
  
  output$fake_data_nhtn <- DT::renderDataTable({
    read.csv("Fake_Data_NHTN.csv") %>%
      set_rownames(NULL) %>% 
      mutate_if(is.numeric,round,1)
  },
    options = list(scrollY="300px", 
      scrollX="300px", 
      pageLength = 100)
  )
  
  output$fake_data_nd_sbp <- DT::renderDataTable({
    read.csv("Fake_Data_ND_SBP.csv") %>%
      set_rownames(NULL) %>% 
      mutate_if(is.numeric,round,1)
  },
    options = list(scrollY="300px", 
      scrollX="300px", 
      pageLength = 100)
  )
  
  output$indata_nhtn <- DT::renderDataTable({
    
    if(is.null(InData_nhtn())){
      NULL
    } else {
      InData_nhtn() %>%
        set_rownames(NULL) %>% 
        mutate_if(is.numeric,round,1)
    }
  },
    options = list(
      scrollY="300px", 
      scrollX="300px", 
      pageLength = 100
    )
  )
  
  output$indata_nd_sbp <- DT::renderDataTable({
    
    if(is.null(InData_nd_sbp())){
      NULL
    } else {
      InData_nd_sbp() %>%
        set_rownames(NULL) %>% 
        mutate_if(is.numeric,round,1)
    }
  },
    options = list(
      scrollY="300px", 
      scrollX="300px", 
      pageLength = 100
    )
  )
  
  output$download_nhtn_data <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(InData_nhtn(), file, row.names = FALSE)
    },
    contentType = 'text/csv'
  )
  
  output$download_nd_sbp_data <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(InData_nd_sbp(), file, row.names = FALSE)
    },
    contentType = 'text/csv'
  )
  
}

shinyApp(ui, server)

# output$nhtn_screen_rec <- renderUI({
#   if(nhtn_pred()$numeric < cpnts$nhtn['sens']){
#     
#     HTML(
#       paste0(
#         "<ul type='square'>",
#         '<li>',
#         'Your predicted probability for nocturnal hypertension is less than ',
#         round(100*cpnts$nhtn['sens']),"%",
#         '</li>',
#         ' <li>',
#         'Our data indicate that approximately ',
#         table(tst$nhtn,GAM$nhtn$prd$test>cpnts$nhtn['sens'])%>%
#           prop.table(margin=2)%>%magrittr::extract(1,1)%>%
#           multiply_by(100)%>%round(1)%>%format(nsmall=1),
#         ' % of adults with predicted probability less than ',
#         round(100*cpnts$nhtn['sens'],1),
#         '% do not have nocturnal hypertension.',
#         '</li>',
#         '<li>',
#         ' Therefore, there is strong evidence against', 
#         ' screening for nocturnal hypertension using',
#         ' ambulatory blood pressure monitoring.',
#         '</li>',
#         '</ul>'
#         )
#     )
#     
#   } else if (nhtn_pred()$numeric >= cpnts$nhtn['sens'] & nhtn_pred()$numeric <= cpnts$nhtn['ppv']){
#     
#     
#     cmp_sens <- function(sqmat){
#       sqmat[2,2] / sum(sqmat[2,])
#     }
#     
#     HTML(
#       paste0(
#         "<ul type='square'>",
#         '<li>',
#         'Your predicted probability for nocturnal hypertension is above ',
#         round(100*cpnts$nhtn['sens']), "%",
#         '</li>',
#         ' <li>',
#         'Our data indicate that approximately ',
#         table(tst$nhtn, GAM$nhtn$prd$test >= cpnts$nhtn['sens']) %>%
#           cmp_sens %>% 
#           multiply_by(100) %>%
#           round(1) %>%
#           format(nsmall=1),
#         ' % of adults who have nocturnal hypertension also have',
#         ' a predicted probability above this cut-point.',
#         '</li>',
#         '<li>',
#         ' Therefore, if you want to detect nearly all (i.e. 75%) cases of', 
#         ' nocturnal hypertension, you may consider screening ',
#         ' with ambulatory blood pressure monitoring.',
#         '</li>',
#         '</ul>'
#       )
#     )
#     
#     
#   } else {
#     
#     HTML(
#       paste0(
#         "<ul type='square'>",
#         '<li>',
#         'Your predicted probability for nocturnal hypertension is greater than ',
#         round(100*cpnts$nhtn['ppv'],1),'%.',
#         '</li>',
#         ' <li>',
#         'Our data indicate that approximately ',
#         table(tst$nhtn, GAM$nhtn$prd$test>cpnts$nhtn['ppv'])%>%
#           prop.table(margin=2)%>%magrittr::extract(2,2)%>%
#           multiply_by(100)%>%round(1)%>%format(nsmall=1),
#         ' % of adults with predicted probability greater than ',
#         round(100*cpnts$nhtn['ppv'],1),
#         '% have nocturnal hypertension.',
#         '</li>',
#         '<li>',
#         ' Therefore, if you want almost all (i.e. 75%) of the people who',
#         ' are screened for nocturnal hypertension to be positive cases,',
#         ' you may consider screening using',
#         'ambulatory blood pressure monitoring.',
#         '</li>',
#         '</ul>'
#       )
#     )
#   } 
# })
