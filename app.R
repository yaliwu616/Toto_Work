# setwd("C:/Users/yawu/OneDrive - George Weston Limited-6469347-MTCAD/2019 Data Scientist Finance/IFRS 16")
# source("./Shiny App/ShinyApp/Bundle_Fun_IFRS16.R")
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(readxl))
library(lubridate)
library(timeDate)
options(scipen = 999)
options(digits=12)
options(launch.browser = TRUE)
library(shiny)
library(DT)
library(dplyr)
library(readr)
library(tools)
library(RODBC)
library(shinythemes)
library(shinyWidgets)
library(purrr)
library(data.table)
library(sqldf)
library(formattable)
library(DescTools)
library(shinycssloaders)
#library(shinyalert)
###########################################################################################
#################################################################################################################
#######################################################################################
###  Shiny App Stars from below: #########################################################
############################################################################################

# help javascript code for footer 

jsCode <- "function(row, data, start, end, display) {var api = this.api(), data;$( api.column(0).footer() ).html('Total');}"

jsCode2 <- "function(row, data, start, end, display) {var api = this.api(), data;
                         
                         $( api.column(0).footer()).html('Total');
 
                         $( api.column(4).footer()).html(LeaseNum)



           }"

###############################################################################################################
##########################################################################################################

b64 <- base64enc::dataURI(file="C:/Users/yawu/OneDrive - George Weston Limited-6469347-MTCAD/2019 Data Scientist Finance/IFRS 16/Shiny App/ShinyApp/www/logo-loblaws2.png", 
                          mime="image/png")


b65  <- base64enc::dataURI(file="C:/Users/yawu/OneDrive - George Weston Limited-6469347-MTCAD/2019 Data Scientist Finance/IFRS 16/Shiny App/ShinyApp/www/Loblaw.PNG", 
                           mime="image/png")


ui <- fluidPage(
  list(tags$head(HTML('<link rel="icon", href="MyIcon.png",
                      type="image/png" />'))
       ,tags$head(tags$style(HTML('#sidebar {width: 100px;}')))),
  div(style="position:relative;center:30px;padding: 1px 0px",
      titlePanel(
        title="", windowTitle="Lease Forecasting Web"
      )
  )
  ,

 navbarPage(
   
    # title =div(img(src='https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcSezCWU0UQAMpPCLrMUJrFajhA2psk_XthTHx1ir-R44MRYTQqA', width = 40, height = 38, align = "middle", style = "position: relative; top: -3px;"),
    #            "Lease Forecasting Web App", img(src="https://lcl-flip-cdn.azureedge.net/img/global/logo-footer-Loblaw_Companies_Ltd.png", width = 90, height = 38, align = "middle",
    #                                             style = "position: relative; top: -3px; right: -1000px;"))
   
   title =div( "Lease Forecasting Web App", img(src="https://lcl-flip-cdn.azureedge.net/img/global/logo-footer-Loblaw_Companies_Ltd.png", 
                                                width = 90, height = 38, align = "middle",
                                               style = "position: relative; top: -3px; right: -1000px;"))

    ,
   # title = "Lease Forecasting Web App" ,
    theme = shinytheme("flatly"),
    #######################################################
    ##### New Lease Forecasting Panel #############
    
    tabPanel( "New Lease Forecasting",
             
                sidebarLayout(
                  
                  sidebarPanel( width = 3,
                               
                                fileInput(inputId = "newleasefile",
                                          label = "Choose 1 file to upload (only accept xlsx file):",
                                          multiple = FALSE,
                                          accept =  c(".xlsx")
                                          ),
                                actionButton("newlse_submibutton", "Submit!", icon = icon("thumbs-up"))  # add @ 5/16/2019
                              )
                  
                  ,mainPanel(
                    
                    tabsetPanel(type = "tabs",
                                
                                tabPanel("Note",icon = icon("exclamation-circle"),
                                         
                                         br(), br(),
                                         
                                         p("Please download the new lease template file for reference"),
                                         
                                         downloadButton(outputId ="download_new_lease_template", "Download New Lease Template File")
                                ),
                              
                                tabPanel("New Leases",icon = icon("info"), br(),
                                           
                                           DT::dataTableOutput(outputId = "Lease_Table_new"),
                                           br(),
                                           downloadButton(outputId = "download_data_new_lease", label = "Download")
                                        ),

                                tabPanel("Summary Tables", icon = icon("table"), 
                                         fluidPage(
                                           
                                           fluidRow( 
                                             column (2,
                                                     pickerInput(
                                                       inputId = "newselectbreakdownby",
                                                       label = "Breakdown by:",
                                                       choices =  c('Lease ID',
                                                                    'Legal Entity'
                                                       ),
                                                       
                                                       options = list(
                                                         `actions-box` = TRUE),
                                                       multiple = FALSE
                                                     )  
                                             ),
                                             column (2,   
                                                     
                                                     pickerInput(
                                                       inputId = "newreporttype",
                                                       label = "Report by:",
                                                       choices =  c('Period to Date',
                                                                    'Quarter to Date',
                                                                    'Year to Date'
                                                                    
                                                       ),
                                                       
                                                       options = list(
                                                         `actions-box` = TRUE),
                                                       multiple = FALSE
                                                     )
                                             ),
                                             column (2,
                                                     pickerInput(
                                                       inputId = "newreportcutoff",
                                                       label = "Snapshot Type:",
                                                       choices =  c('Before',
                                                                    'After',
                                                                    'Delta'
                                                       ),
                                                       
                                                       options = list(
                                                         `actions-box` = TRUE),
                                                       multiple = FALSE
                                                     )  
                                             ),
                                             column (3,
                                                     pickerInput(
                                                       inputId = "newselectyear",
                                                       label = "Fiscal Year:",
                                                       choices = c(
                                                         sort(unique(as.character(lease$Fiscal_Year)))
                                                       ),
                                                       
                                                       options = list(
                                                         `actions-box` = TRUE),
                                                       multiple = TRUE
                                                     )
                                             ),
                                             column (3,
                                                     pickerInput(
                                                       inputId = "newselectperiod",
                                                       label = "Period:",
                                                       choices =  c(as.character(sort(unique(lease$Fiscal_Period)))
                                                       ),
                                                       
                                                       options = list(
                                                         `actions-box` = TRUE),
                                                       multiple = TRUE)
                                             )
                                           ),
                                           withSpinner(DT::dataTableOutput(outputId = "New_Lease_VS_Table")),
                                           downloadButton(outputId = "download_data_new", label = "Download")
                                           
                                         )
                                         
                                )
                                
                                
                    )
                  )
               
              )
    ),
    #####################################################
    navbarMenu("Existing Lease Forecasting",
               
        tabPanel("Multiple Leases Multiple Changes",
               
            sidebarLayout(
                 
                 sidebarPanel( width = 3,  
                               
                               helpText("If changes vary for leases, please upload an external file below: "),
                               
                               fileInput(inputId = "currentleasefile",
                                         label = "Choose 1 file to upload (only accept xlsx file):",
                                         multiple = FALSE,
                                         accept =  c( ".xlsx")
                                         
                               ),
                               actionButton("mullse_submibutton", "Submit!", icon = icon("thumbs-up")) 
                               
                               ),
                               
                 mainPanel(  
                   
                     tabsetPanel(type = "tabs",
                                 
                              tabPanel("Note",icon = icon("exclamation-circle"),
                                         
                                         br(), br(),
                                         
                                         p("Please download the multi lease multi change template file for reference"),
                                         
                                         downloadButton(outputId ="download_mul_lease_mul_template", "Download Multi Lease Multi change Template File")
                                       ),
                               
                               tabPanel("Leases Selected", icon = icon("info"), 
                                        
                                           br(), 
                                           #downloadButton(outputId = "MultiChange_Leases_Selected", label = "Download"),
                                          # br(), 
                                          # withSpinner(DT::dataTableOutput(outputId = "Leases_Selected"))
                                        
                                           DT::dataTableOutput(outputId = "Leases_Selected"),
                                           br(),
                                           downloadButton(outputId = "MultiChange_Leases_Selected", label = "Download")
                                           
                                        
                               ),

                               tabPanel("Summary Tables", icon = icon("table"), 
                                        fluidPage(
                                          
                                          fluidRow( 
                                           column (2,
                                                    pickerInput(
                                                      inputId = "multiselectbreakdownby",
                                                      label = "Breakdown by:",
                                                      choices =  c('Lease ID',
                                                                   'Legal Entity'
                                                      ),
                                                      
                                                      options = list(
                                                        `actions-box` = TRUE),
                                                      multiple = FALSE
                                                    )  
                                            ),
                                            column (2,   
                                                    
                                                    pickerInput(
                                                      inputId = "multireporttype",
                                                      label = "Report by:",
                                                      choices =  c('Period to Date',
                                                                   'Quarter to Date',
                                                                   'Year to Date'
                                                                   
                                                      ),
                                                      
                                                      options = list(
                                                        `actions-box` = TRUE),
                                                      multiple = FALSE
                                                    )
                                            ),
                                            column (2,
                                                    pickerInput(
                                                      inputId = "multireportcutoff",
                                                      label = "Snapshot Type:",
                                                      choices =  c('Before',
                                                                   'After',
                                                                   'Delta'
                                                      ),
                                                      
                                                      options = list(
                                                        `actions-box` = TRUE),
                                                      multiple = FALSE
                                                    )  
                                            ),
                                            column (3,
                                                    pickerInput(
                                                      inputId = "multiselectyear",
                                                      label = "Fiscal Year:",
                                                      choices = c(
                                                        sort(unique(as.character(lease$Fiscal_Year)))
                                                      ),
                                                      
                                                      options = list(
                                                        `actions-box` = TRUE),
                                                      multiple = TRUE
                                                    )
                                            ),
                                            column (3,
                                                    pickerInput(
                                                      inputId = "multiselectperiod",
                                                      label = "Period:",
                                                      choices =  c(as.character(sort(unique(lease$Fiscal_Period)))
                                                      ),
                                                      
                                                      options = list(
                                                        `actions-box` = TRUE),
                                                      multiple = TRUE)
                                            )
                                          ),
                                          
                                          
                                          withSpinner(DT::dataTableOutput(outputId = "multi_Summary_Lease_VS_Table")),
                                          downloadButton(outputId = "download_mulChange_mulLse", label = "Download")
                                          
                                          
                                 )
                                        
                               )
                            )
                         )
                      )
                ),
                 
         tabPanel("Unified Change",
                        
              sidebarLayout(
                 
                 sidebarPanel( width = 3,    
                       helpText("If apply same changes to all selected leases, please use the following steps:"),
                               # Choose lease end date
                               dateRangeInput(inputId = "Lease_end_dt",
                                              label = "Select Lease End Date between:",
                                              start = as.Date(NA),
                                              end = as.Date(NA),
                                              min = min_end_dt, #cannot change to NA
                                              max = max_end_dt, #cannot change to NA
                                              format = "yyyy-mm-dd",
                                              startview = "year"),
                               # Choose Lease Entity type
                               selectInput(inputId = "Legal_Entity",
                                           label = "Legal Entity ID",
                                           choices = c(sort(unique(as.character(lease$Legal_Entity)))),
                                           multiple = TRUE,
                                           selectize = FALSE,
                                           selected = NULL),
                               
                               
                               # Choose Lease Type
                               selectInput(inputId = "Lease_Type",
                                           label = "Lease Type",
                                           choices = c(sort(unique(as.character(lease$LSE_TY_DESC)))),
                                           
                                           multiple = TRUE,
                                           selectize = FALSE,
                                           selected = NULL),
                               
                               
                               textInput( inputId = "LP_Date",
                                          label = "Lease Payment Day is Nth day of each month:",
                                          placeholder  = "Enter a number like 1",
                                          value = '1'
                               ),
                               # Lock Year
                               numericInput( inputId = "change_year",
                                             label = "Lease Change Year:",
                                             min = lubridate::year(Sys.Date()),
                                             max = NA,
                                             value = lubridate::year(Sys.Date())),
                               # Lock Year Period
                               numericInput( inputId = "change_period",
                                             label = "Lease Change Period:",
                                             min = 1, max = 13,
                                             value = 4),
                               # Apply global lease end date 
                               dateInput(inputId = 'Lease_New_End_Date',
                                         label = 'Lease New End Date: (yyyy-mm-dd)',
                                         value = as.Date(NA),
                                         format = "yyyy-mm-dd",
                                         startview = "year"),
                               # Input for how many renew options for the lease
                               numericInput( inputId = "no_renew_options",
                                             label = "Or How Many Renew Options (0-20):",
                                             min = 0, max=20,
                                             value = 2),
                               
                               # If apply same change for renewal 
                               selectInput(inputId = "Same_rent_change",
                                           label = "Same Rent Change in all Renewals?",
                                           choices = c("Y", "N"),
                                           
                                           multiple = FALSE,
                                           selectize = FALSE,
                                           selected = "Y"),
                               
                    
                               #Interest Rate for lease payment
                               
                               textInput( inputId = "IR_for_LP",
                                          label = "% increase in lease payment for each renew option:",
                                          placeholder  = "interest rate for renew option like 5,3 if not same",
                                          value = '5'
                               ),
                               
                               # IP increase by $ amount 
                               textInput( inputId = "amt_for_LP",
                                          label = " Or $ amount increase in lease payment for each renew option:",
                                          placeholder  = "$ Amount for renew option like 500,800 if not same",
                                          value = ''
                               ),
                               
                               # Interest Rate for Lease
                               numericInput( inputId = "new_interest_rate",
                                             label = " New Interest Rate (%) for the Lease(s):",
                                             min = NA, max = 100,
                                             value = 6),
                               
                               # ARO value for calculation
                               numericInput( inputId = "ARO_value",
                                             label = "New ARO Value for calculation",
                                             min = NA, max = NA,
                                             value = NA),
                               # TI Adjustment value
                               numericInput( inputId = "TI_adj_value",
                                             label = "TI Adjustment Value for calculation",
                                             min = 0, max = NA,
                                             value = 0),
                               # TI Expected Receipt Date
                               dateInput(inputId = 'TI_Date',
                                         label = 'TI Expected Receipt Date: (yyyy-mm-dd)',
                                         value = as.Date(NA),
                                         format = "yyyy-mm-dd",
                                         startview = "year"),
                               # Impairment value
                               numericInput( inputId = "impairment_value",
                                             label = "Impairment value for calculation",
                                             min = 0, max = NA,
                                             value = 0),
                               # Impairment date
                               dateInput(inputId = 'impairment_Date',
                                         label = 'Impairment Date: (yyyy-mm-dd)',
                                         value = as.Date(NA),
                                         format = "yyyy-mm-dd",
                                         startview = "year"),
                               # Purchase Option amount
                               numericInput( inputId = "po_amt",
                                             label = "Purchase Option amount:",
                                             min = 0, max = NA,
                                             value = 0),
                               # PO future date
                               dateInput(inputId = 'po_date',
                                         label = 'PO Future Date:(yyyy-mm-dd)',
                                         value = as.Date(NA),
                                         format = "yyyy-mm-dd",
                                         startview = "year"),
                       
                           actionButton("unilse_submibutton", "Submit!", icon = icon("thumbs-up"))  # add @ 5/16/2019
                               
                 ),
                 # Output(s)
                 mainPanel(
                   
                   #textOutput(outputId = "avg_y"),
                   tabsetPanel(type = "tabs",
                               
                              tabPanel("Lease Selected", icon = icon("info"),
                                          br(), 
                                          DT::dataTableOutput(outputId = "Originaltable"),
                                          br(),
                                          downloadButton(outputId = "UniChange_Leases_Selected", label = "Download")
                                       ),

                               tabPanel("Summary Tables", icon = icon("table"), 
                                        fluidPage(
                                          
                                          fluidRow( 
                                            column (2,
                                                    pickerInput(
                                                      inputId = "selectbreakdownby",
                                                      label = "Breakdown by:",
                                                      choices =  c('Lease ID',
                                                                   'Legal Entity'
                                                      ),
                                                      
                                                      options = list(
                                                        `actions-box` = TRUE),
                                                      multiple = FALSE
                                                    )  
                                            ),
                                            column (2,   
                                                    
                                                    pickerInput(
                                                      inputId = "reporttype",
                                                      label = "Report by:",
                                                      choices =  c('Period to Date',
                                                                   'Quarter to Date',
                                                                   'Year to Date'
                                                                   
                                                      ),
                                                      
                                                      options = list(
                                                        `actions-box` = TRUE),
                                                      multiple = FALSE
                                                    )
                                            ),
                                            column (2,
                                                    pickerInput(
                                                      inputId = "reportcutoff",
                                                      label = "Snapshot Type:",
                                                      choices =  c('Before',
                                                                   'After',
                                                                   'Delta'
                                                      ),
                                                      
                                                      options = list(
                                                        `actions-box` = TRUE),
                                                      multiple = FALSE
                                                    )  
                                            ),
                                            column (3,
                                                    pickerInput(
                                                      inputId = "selectyear",
                                                      label = "Fiscal Year:",
                                                      choices = c(
                                                        sort(unique(as.character(lease$Fiscal_Year)))
                                                      ),
                                                      
                                                      options = list(
                                                        `actions-box` = TRUE),
                                                      multiple = TRUE
                                                    )
                                            ),
                                            column (3,
                                                    pickerInput(
                                                      inputId = "selectperiod",
                                                      label = "Period:",
                                                      choices =  c(as.character(sort(unique(lease$Fiscal_Period)))
                                                      ),
                                                      
                                                      options = list(
                                                        `actions-box` = TRUE),
                                                      multiple = TRUE)
                                            )
                                          ),
                                          # br(), 
                                          
                                          withSpinner(DT::dataTableOutput(outputId = "Summary_Lease_VS_Table")),
                                          downloadButton(outputId = "download_uniChange",  "Download")
                                          
                                          
                                 )
                                        
                               )
                   )
                   
                  # ,textOutput(outputId = "out")
                   
                 ) #mainpanel 
               )
            )
    ),
    ####################################################################
    # 3rd Panel: Overall forecasting results 
    #####################################################################
    tabPanel("Overall Forecasting Results",
             fluidPage(
               fluidRow(
                column (2,
                         pickerInput(
                           inputId = "ttlselectbreakdownby",
                           label = "Breakdown by:",
                           choices =  c('Lease ID',
                                        'Legal Entity'
                           ),
                           
                           options = list(
                             `actions-box` = TRUE),
                           multiple = FALSE
                         )
                    ),
                column (2,   
                         
                         pickerInput(
                           inputId = "ttlreporttype",
                           label = "Report Level:",
                           choices =  c('Period to Date',
                                        'Quarter to Date',
                                        'Year to Date'
                           ),
                           
                           options = list(
                             `actions-box` = TRUE),
                           multiple = FALSE
                         )
                      ),
                 column (2,
                         pickerInput(
                           inputId = "ttlreportcutoff",
                           label = "Snapshot Type:",
                           choices =  c('Before',
                                        'After',
                                        'Delta'
                           ),
                           
                           options = list(
                             `actions-box` = TRUE),
                           multiple = FALSE
                         )  
                 ),
                 column (3,
                         pickerInput(
                           inputId = "ttlselectyear",
                           label = "Fiscal Year:",
                           choices = c(
                             2019
                           ),
                           
                           options = list(
                             `actions-box` = TRUE),
                           multiple = TRUE
                         )
                 ),
                 column (3,
                         pickerInput(
                           inputId = "ttlselectperiod",
                           label = "Period:",
                           choices =  c(seq(1:13)
                           ),
                           
                           options = list(
                             `actions-box` = TRUE),
                           multiple = TRUE)
                 )
               ),
              
               #textOutput(outputId = "out")
               withSpinner(DT::dataTableOutput(outputId = "Overall_Lease_VS_Table")),
               downloadButton(outputId = "download_Overall_Change", label = "Download")
               
             )
    ),
    #############################################
    ### 4th tab######################################
    tabPanel("Sensitive Analysis",
             
             h3("To Be Determined")
             
             )
  )
  
)

#############################################################################################
################################## Server part below: ########################################
##############################################################################################
server <- function(input, output, session){
  
  # new lease template
  output$download_new_lease_template <- downloadHandler(
  filename = function() {
    paste0("New Lease Template File", ".", "xlsx")
  },
  content = function(file) {
    myfile <- srcpath <- 'C:/Users/yawu/Desktop/new_lease_template.xlsx'    
    file.copy(myfile, file)
  }
  )
###################################################################
  # multi change multi leases template
  output$download_mul_lease_mul_template <- downloadHandler(
  filename = function() {
    paste0("Multi Leases Multi Changes Template File", ".", "xlsx")
  },
  content = function(file) {
    myfile <- srcpath <- 'C:/Users/yawu/Desktop/Multi_Lease_Multi_Change_template.xlsx'    
    file.copy(myfile, file)
  }
  )
##############################################
  # read in new lease external file

  lease_infor_new <- reactive({
    req(input$newleasefile)
    rawdf <- input$newleasefile
    ext <- tools::file_ext(rawdf$name)
    if(ext =="xlsx"){
      lease_infor <- readxl::read_excel(rawdf$datapath, sheet = 1)
    }
    else{
      break
    }
    return(lease_infor)
  })

  lease_recur_cost <- reactive({
    req(input$newleasefile)
    rawdf <- input$newleasefile
    ext <- tools::file_ext(rawdf$name)
    if(ext =="xlsx"){

      recur_cost <- readxl::read_excel(rawdf$datapath, sheet = 2)
    }
    else{
      break
    }
    return(recur_cost)
  })


 #######################################################################
  # read in external file for current leases
 current_lease_ext <- reactive({

   req(input$currentleasefile)
   curfile <- input$currentleasefile
   ext <- tools::file_ext(curfile$name)
   if(ext == "xlsx"){

     lease_para <- readxl::read_excel(curfile$datapath, sheet = 1)
     # convert datetime to date
     lease_para$New_lease_end_date <- as.Date(lease_para$New_lease_end_date)
     lease_para$Impariment_Date <- as.Date(lease_para$Impariment_Date)
     lease_para$TI_Receipt_Date <- as.Date(lease_para$TI_Receipt_Date)
     lease_para$PO_date <- as.Date(lease_para$PO_date)

   } 
   # else if (ext == "csv"){
   # 
   #   lease_para <- read.csv(curfile$datapath, stringsAsFactors = FALSE)
   #   # convert character to date
   #   lease_para$New_lease_end_date <- as.Date(parse_date_time(lease_para$New_lease_end_date, 'mdy'))
   #   lease_para$Impariment_Date <- as.Date(parse_date_time(lease_para$Impariment_Date, 'mdy'))
   #   lease_para$TI_Receipt_Date <- as.Date(parse_date_time(lease_para$TI_Receipt_Date, 'mdy'))
   #   lease_para$PO_date <- as.Date(parse_date_time(lease_para$PO_date, 'mdy'))
   # 
   # } 
   
   else{

     break
   }

   return(lease_para)

 })

########################################################################
#######################################################################
############# Unified Change #################################################
# filter lease data frame by user input
workdf_exist <- eventReactive(input$unilse_submibutton, {


    if(!isTruthy(input$Lease_end_dt) == TRUE & !isTruthy(input$Legal_Entity) == TRUE &
       !isTruthy(input$Lease_Type) == TRUE){

      return(NULL)

    }

    else if(!isTruthy(input$Lease_end_dt) == FALSE & !isTruthy(input$Legal_Entity) == TRUE &
            !isTruthy(input$Lease_Type) == TRUE){

      df <-  lease %>% filter(IFRS_END_DT >= input$Lease_end_dt[1] &
                                IFRS_END_DT <= input$Lease_end_dt[2])}

    else if(!isTruthy(input$Lease_end_dt) == FALSE & !isTruthy(input$Legal_Entity) == FALSE &
            !isTruthy(input$Lease_Type) == TRUE){

      df <- lease %>% filter(IFRS_END_DT >= input$Lease_end_dt[1] & IFRS_END_DT <= input$Lease_end_dt[2] &
                              as.character(Legal_Entity) %in% input$Legal_Entity)}

    else if(!isTruthy(input$Lease_end_dt) == FALSE & !isTruthy(input$Legal_Entity) == TRUE &
            !isTruthy(input$Lease_Type) == FALSE){

      df <- lease %>% filter(IFRS_END_DT >= input$Lease_end_dt[1] & IFRS_END_DT <= input$Lease_end_dt[2] &
                              as.character(LSE_TY_DESC) %in% input$Lease_Type)}

    else {
      
      df <- lease %>% filter(IFRS_END_DT >= input$Lease_end_dt[1] & IFRS_END_DT <= input$Lease_end_dt[2] &
                              as.character(Legal_Entity) %in% input$Legal_Entity &
                              as.character(LSE_TY_DESC) %in% input$Lease_Type)
    }

    return(df)

  })
  #######################################################################################################
  ######################################################################################################

  # Split workdf into buntch files for each individual lease

  # out_exist not change as workdf_exist change ?
  
  # unified change part 

  out_exist <- reactive({

    #req(workdf_exist())
    
    if(isTruthy(workdf_exist()) & length(unique(workdf_exist()$Lease_ID)) > 0 ){
      
    print(paste(" workdf_exist has lease IDs is ", length(unique(workdf_exist()$Lease_ID))))
   
 
    df <- workdf_exist()
    
    outdf_exist <- split(df, f = df$Lease_ID)
    
    } else { return(NULL) }
    
    return(outdf_exist)

  })

#####################################################################################################

  # External file for current leases
  
  # multiple changes 

ext_cur_lse_work_df <- eventReactive(input$mullse_submibutton, {

      req(current_lease_ext())
    
      current_lease_ext_df <- current_lease_ext()
    
      lease_data <- subset(lease, lease$Lease_ID %in% unique(current_lease_ext_df$Lease_ID))
    
      lease_data$split_col <- paste0(lease_data$Lease_ID,'_', lease_data$Legal_Entity)
    
      split_work_data <- lease_data %>% group_by(split_col) %>% nest()
    
      current_lease_ext_df$split_col <- paste0(current_lease_ext_df$Lease_ID,'_', current_lease_ext_df$Legal_Entity)
    
      test2 <- current_lease_ext_df %>% inner_join(split_work_data, by = c("split_col" = "split_col"))
    
      test2$split_col <- NULL
    
      split_test2 <- test2 %>% group_by(Lease_ID, Legal_Entity) %>% nest()
    
      return(split_test2)

})

###########################################################################
  # for new lease

  newlease_infor <- eventReactive(input$newlse_submibutton, {

    
    
    req(lease_infor_new())
    newlease <- lease_infor_new()
    # update fields type
    newlease$IFRS_START_DT <- as.Date(newlease$IFRS_START_DT, format = "%Y-%m-%d")

    newlease$IFRS_END_DT <- as.Date(newlease$IFRS_END_DT, format = "%Y-%m-%d")

    newlease$ASSET_IN_SERV_DT <- as.Date(newlease$ASSET_IN_SERV_DT, format = "%Y-%m-%d")

    newlease$Impairment_Date <- as.Date(as.numeric(newlease$Impairment_Date), origin = '1899-12-30')
    newlease$TI_Date <- as.Date(as.numeric(newlease$TI_Date), origin = '1899-12-30')
    newlease$PO_Date  <- as.Date(as.numeric(newlease$PO_Date), origin = '1899-12-30')


    newlease2 <- sqldf("select * from newlease left join lease_pd_cal
      on newlease.IFRS_START_DT <= lease_pd_cal.CORP_PD_END_DT and
                       newlease.IFRS_END_DT >= lease_pd_cal.CORP_PD_STRT_DT")

    newlease2 <- arrange(newlease2,PRPTY_NUM,CORP_PD_STRT_DT)

    return(newlease2)

  })

  ######################################################################################
  newlease_recurr_cost <- eventReactive(input$newlse_submibutton, {

    req(lease_recur_cost())
    new_recurr_cost <- lease_recur_cost()

    # update fields type
    new_recurr_cost$lse_term_start_dt <- as.Date(new_recurr_cost$lse_term_start_dt, format = "%Y-%m-%d")

    new_recurr_cost$lse_term_end_dt <- as.Date(new_recurr_cost$lse_term_end_dt, format = "%Y-%m-%d")

    # group by PRPTY_NUM, lse_term_start_dt, lse_term_end_dt, bse_rnt_amt
    new_recurr_cost2 <- new_recurr_cost %>% select(PRPTY_NUM, lse_term_start_dt, lse_term_end_dt, bse_rnt_amt) %>%
      dplyr::group_by(PRPTY_NUM, lse_term_start_dt, lse_term_end_dt) %>%
      dplyr::summarise(monthly_pay = sum(bse_rnt_amt, na.rm=TRUE)) %>%
      dplyr::arrange(PRPTY_NUM, lse_term_start_dt, lse_term_end_dt)

    # Map with the lease_pd_cal table
    lease_recur_cost2 <- sqldf("select * from new_recurr_cost2 left join lease_pd_cal
                               on new_recurr_cost2.lse_term_start_dt <= lease_pd_cal.CORP_PD_END_DT and
                               new_recurr_cost2.lse_term_end_dt >= lease_pd_cal.CORP_PD_STRT_DT")

    return(lease_recur_cost2)

  })
  #################################################################################
  # Split by propety number
  # new lease

  split_lease_recurr_cost <- reactive({

    req(newlease_recurr_cost())

    df <- newlease_recurr_cost()
    df2 <- split(df, f = df$PRPTY_NUM)

    return(df2)

  })


  split_new_lease_info <- reactive({

    req(newlease_infor())

    df <- newlease_infor()
    df2 <- split(df, f = df$PRPTY_NUM)

    return(df2)

  })


####################################################################################################
  #  apply function to new lease
  new_lease_table <- reactive({

    req(split_new_lease_info(), split_lease_recurr_cost())

    lease_info_df_final <- split_new_lease_info()

    recurring_cost_df_final <- split_lease_recurr_cost()


    datalist_new = list()

    withProgress(message = 'Creating Table', value = 0,{

      n <- length(lease_info_df_final)

      for (i in 1:n) {

        print(paste("i is",i))

        datalist_new[[i]] <- new_lease_forecast(lease_info_df = lease_info_df_final[[i]],
                                                recurring_cost_df = recurring_cost_df_final[[i]]
        )

        incProgress(1/n, detail = paste("Running Part", i, "out of", n))

        Sys.sleep(0.1)

      }
    })
    #print(paste("i is",i))

    new_lease_big_data <- dplyr::bind_rows(datalist_new)
    
    print(paste("col names of new_lease_big_data", colnames(new_lease_big_data)))
    
    new_lease_big_data <- new_lease_big_data %>% select(Lease_ID,	PRPTY_NUM,	CO_CD,	IFRS_START_DT,	IFRS_END_DT,	ASSET_IN_SERV_DT,	INCR_BORR_RT,

                                                        CORP_YR_NUM, CORP_PD_NUM,	CORP_PD_STRT_DT,	CORP_PD_END_DT,	Remaining_Lease_Term,	count_period,
                                                        payment_date, nodaysinperiod,	no_days_in_period_before_payment,	no_days_in_period_after_payment,

                                                        Right_of_Use_Opening,	Right_of_Use_Renewal_PO_Adjustment,	Right_of_Use_Tenant_Inducement_Adjustment,
                                                        Right_of_Use_ARO_Adjustment,	Right_of_Use_Impaired_Adjustment,
                                                        Right_of_Use_Amortization_Expense, Right_of_Use_Ending,

                                                        Financial_Liability_Lease_Payment,	Financial_Liability_Tenant_Inducement_Receipt,
                                                        PV,	Financial_Liability_Renewal_PO_Adjustment,	Financial_Liability_Opening,
                                                        Financial_Liability_Interest_Expense, Financial_Liability_Ending_Detail,
                                                        ARO_Opening,
                                                        ARO_Interest_Expense,	ARO_Revaluation_Adjustment,	ARO_Ending,	Tenant_Inducement_Opening,
                                                        Tenant_Inducement_Addition,	Tenant_Inducement_Period_Amortization,	Tenant_Inducement_Ending, TI_receipt,
                                                        impair_amt,	PO_amt, PPV_Amt,
                                                        lse_term_start_dt,	lse_term_end_dt
                                                        )

    return(new_lease_big_data)


  })

###############################################################################################
#################################################################################################
# apply function to unified change for current leases#
  
  
  # Apply forecast_lease function to current lease: each individual lease data frame
  # Unified change 
lease.table <- reactive({

    req(out_exist())

    out_df <- out_exist()

    datalist = list()

    withProgress(message = 'Creating Table', value = 0, {

      m <- length(out_df)
      
      print(paste("length of out_df is", m))

      for (i in 1:m) {


         print(paste("i is",i))
      
        if (unique(out_df[[i]]$IFRS_END_DT) <= today() | last(out_df[[i]]$Fiscal_Year) < input$change_year |
            (last(out_df[[i]]$Fiscal_Year) == input$change_year &
             last(out_df[[i]]$Fiscal_Period) < input$change_period)){

          print(paste("Skip this Lease ID:", unique(out_df[[i]]$Lease_ID)))
          
          next
        }
        else{
          datalist[[i]] <- forecast_lease(originalDF = out_df[[i]], 
                                          changeYr=input$change_year, 
                                          changePeriod=input$change_period,
                                          newLeaseEndDt = input$Lease_New_End_Date,
                                          noRenewOption = input$no_renew_options,
                                          Same_rent_change_in_all_renewals = input$Same_rent_change,
                                          IR_for_LP = input$IR_for_LP ,
                                          amt_for_lp = input$amt_for_LP,
                                          inputNewIR = input$new_interest_rate,
                                          nth_payday = input$LP_Date,
                                          TI_receipt_dt = input$TI_Date,
                                          TI_Amt = input$TI_adj_value,
                                          PO_dt = input$po_date,
                                          PO_amt = input$po_amt,
                                          impair_dt = input$impairment_Date,
                                          impair_amt = input$impairment_value,
                                          AROValue = input$ARO_value)
        }

        incProgress(1/m, detail = paste("Running Part", i, "out of", m))

        Sys.sleep(0.1)
      }

    })

    print(paste("i is",i))
    big_data <- dplyr::bind_rows(datalist)
    return(big_data)



  })

###################################################################################################
  # apply function to multiple change for current leases#
  
multi.change.lease.table <- reactive({

    req(ext_cur_lse_work_df())

    mul_out_df <- ext_cur_lse_work_df()

    results_df = list()

    withProgress(message = 'Creating Table', value = 0, {

      mm <- length(mul_out_df$data)

      for (k in 1:mm) {

              print(paste("k is", k))

                 t <- mul_out_df$data[[k]]
                  
                  testpara3 <- data_frame(
                               originalDF = as.list(t$data),
                                changeYr = as.list(t[[1]]),
                                changePeriod = as.list(t[[2]]),
                                newLeaseEndDt = as.list(t[[3]]),
                                noRenewOption = as.list(t[[4]]),
                                Same_rent_change_in_all_renewals = as.list(t[[5]]),
                                IR_for_LP = as.list(t[[6]]),
                                amt_for_lp = as.list(t[[7]]),
                                inputNewIR = as.list(t[[8]]),
                                nth_payday = as.list(t[[9]]),
                                TI_Amt = as.list(t[[10]]),
                                TI_receipt_dt = as.list(t[[11]]),
                                impair_amt = as.list(t[[12]]) ,
                                impair_dt = as.list(t[[13]]) ,
                                PO_amt = as.list(t[[14]]),
                                PO_dt = as.list(t[[15]]) ,
                                AROValue = as.list(t[[16]])
                  )
                  
                  for (j in 1:nrow(t)){
                    
                    print(paste("now j is :", j))
                    
                    finaldf <- pmap(.l=testpara3[j,], .f=forecast_lease) # finaldf is a list 
                    
                        if( j < nrow(t) )
                        {
                          testpara3$originalDF[[j+1]] <-  finaldf[[1]]
                          
                        } else {break}
                    
                    
                   }
                  
                  results_df[[k]] <- finaldf[[1]]  # Save last one to results_df 
             }
                          
        incProgress(1/mm, detail = paste("Running Part", k, "out of", mm))

        Sys.sleep(0.1)
    })
    
    print(paste("k is",k))
    final <- dplyr::bind_rows(results_df)
    return(final)
})
##########################################################################################################  
########################################################################################################
######################### Lease selected part  ###########################################################
########################################################################################################
  
  # leases selected part 
  
# 1) for unified change 
  output$Originaltable <- DT::renderDataTable({

    if(!isTruthy(lease.table()) == FALSE){
      
      summary_df <- lease.table()
      
      summary_df$Fiscal_Year <- as.character(summary_df$Fiscal_Year)
      
      summary_df$Fiscal_Period <- as.character(summary_df$Fiscal_Period)
      
      summary_df$INTEREST_RATE <- as.character(summary_df$INTEREST_RATE)
      
      summary_df$UNQ_LSE_ID <- as.character(summary_df$UNQ_LSE_ID)
      
      summary_df$Remaining_Lease_Term <- as.character(summary_df$Remaining_Lease_Term)
      
      DT::datatable(data = summary_df,
                   container = htmltools::withTags(
                     table(tableHeader(colnames(summary_df)),
                           tableFooter(sapply(summary_df, function(x) 
                             if(is.numeric(x)) currency(sum(x, na.rm = TRUE), digits = 0L)))
                       )),
                    rownames = FALSE,
                    options = list(paging = TRUE,
                                   searching = TRUE,
                                   autoWidth = TRUE,
                                   ordering = TRUE,
                                   dom = 'Bfrtip',
                                   
                                   scrollY = 400,
                                   scrollX = TRUE,
                                   scroller = TRUE,
                                 
                                   pageLength = 800,
                                   footerCallback = JS(jsCode))
                    )

    } else { return(NULL)}

  })

# Download Button for unified change leases
output$UniChange_Leases_Selected <- downloadHandler(

    filename = function(){
      
      paste0("Leases with Unified Changed Report", "_", now(), ".", "xlsx")
      
    },

    if(!isTruthy(lease.table()) == FALSE){
      
      content = function(file) {
        
        writexl::write_xlsx( lease.table(),
                              file 
                           )
      }

    } else{return(NULL)}

  )
############################################################################################  
  #2 ) for new lease
output$Lease_Table_new <- DT::renderDataTable({

    if(!isTruthy(new_lease_table()) == FALSE){
      
      new_lse_df <- new_lease_table() 
      
      new_lse_df$count_period <- as.character(new_lse_df$count_period)
      
      new_lse_df$nodaysinperiod <- as.character(new_lse_df$nodaysinperiod)
      
      new_lse_df$no_days_in_period_before_payment <- as.character(new_lse_df$no_days_in_period_before_payment)
      
      new_lse_df$no_days_in_period_after_payment <- as.character(new_lse_df$no_days_in_period_after_payment)
      
      new_lse_df$Remaining_Lease_Term <- as.character(new_lse_df$Remaining_Lease_Term)
      
      new_lse_df$INTEREST_RATE <- as.character(new_lse_df$INTEREST_RATE)
      new_lse_df$Fiscal_Year <- as.character(new_lse_df$Fiscal_Year)
      new_lse_df$Fiscal_Period <- as.character(new_lse_df$Fiscal_Period)

      DT::datatable(data = new_lse_df,
                    
                    container = htmltools::withTags(
                      table(tableHeader(colnames(new_lse_df)),
                            tableFooter(sapply(new_lse_df, function(x) 
                              if(is.numeric(x)) currency(sum(x, na.rm = TRUE), digits = 0L)))
                      )),
                    rownames = FALSE,
                    
                    options = list(paging = TRUE,
                                   searching = TRUE,
                                   #fixedColumns = FALSE,
                                   autoWidth = TRUE,
                                   ordering = TRUE,
                                   dom = 'Bfrtip',
                                   
                                   scrollY = 400,
                                   scrollX = TRUE,
                                   scroller = TRUE,
                                   
                                   pageLength = 800,
                                   footerCallback = JS(jsCode)
                                   
                                )
                    )

    } else { return(NULL)}

  })
  
# Download Button for new leases
output$download_data_new_lease <- downloadHandler(

    filename = function(){
      paste0("Only New Lease Report", "_", now(), ".", "xlsx")
    },

    if(!isTruthy(new_lease_table()) == FALSE){
      content = function(file) {
        writexl::write_xlsx( new_lease_table(),
                              file 
                           )
      }

    } else{return(NULL)}

  )
############################################################################################    
  # 3) for multiple change
output$Leases_Selected <- DT::renderDataTable({

    if(isTruthy(multi.change.lease.table())){
      
      mul_change_lse_df <- multi.change.lease.table() 
      
      mul_change_lse_df$Fiscal_Year <- as.character(mul_change_lse_df$Fiscal_Year)
      
      mul_change_lse_df$Fiscal_Period <- as.character(mul_change_lse_df$Fiscal_Period)
      
      mul_change_lse_df$INTEREST_RATE <- as.character(mul_change_lse_df$INTEREST_RATE)
      
      mul_change_lse_df$UNQ_LSE_ID <- as.character(mul_change_lse_df$UNQ_LSE_ID)
      
      mul_change_lse_df$Remaining_Lease_Term <- as.character(mul_change_lse_df$Remaining_Lease_Term)
      

       DT::datatable(data = mul_change_lse_df,
                     container = htmltools::withTags(
                       table(tableHeader(colnames(mul_change_lse_df)),
                             tableFooter(sapply(mul_change_lse_df, function(x) 
                               if(is.numeric(x)) currency(sum(x, na.rm = TRUE), digits = 0L)))
                       )),
                     rownames = FALSE,
                    
                     options = list(paging = TRUE,
                                   searching = TRUE,
                                 
                                   autoWidth = TRUE,
                                   ordering = TRUE,
                                   dom = 'Bfrtip',
                                   
                                   scrollY = 400,
                                   scrollX = TRUE,
                                   scroller = TRUE,
                                   
                                   pageLength = 800,
                                   footerCallback = JS(jsCode)
                                   )
                    )

    } else { return(NULL)}

  }) 

# Download Button for multi changes multi leases
output$MultiChange_Leases_Selected <- downloadHandler(

    filename = function(){
      paste0("Leases with multiple Changes Report", "_", now(), ".", "xlsx")
    },

    if(!isTruthy(multi.change.lease.table()) == FALSE){
      content = function(file) {
        writexl::write_xlsx( multi.change.lease.table(),
                              file 
                           )
      }

    } else{return(NULL)}

  )
#####################################################################################################
############################################################################################
  #  Summary table tabs 
  # summary table for current leases
  
  # 1) for new lease 
  # add 18 periods 
  
  
new.lease.table.update <- reactive({  
  
  req(new_lease_table())
  
  newLeaseTable <- new_lease_table()
  
# rename some columns 
newLeaseTable <- data.table::setnames(newLeaseTable, old=c("CORP_PD_NUM","CORP_YR_NUM", "CO_CD",
                                     "ASSET_IN_SERV_DT",
                                     "INCR_BORR_RT",
                                     "PPV_Amt",
                                     "IFRS_START_DT"), new=c("Fiscal_Period", "Fiscal_Year", "Legal_Entity", "IN_SERV_DT",
                                                             "INTEREST_RATE", "Ppv_Adjustment_Opening",
                                                             "IFRS_STRT_DT"))
keeps <- colnames(lease)

newLeaseTable <-   newLeaseTable[, (colnames(newLeaseTable) %in% keeps), drop = FALSE]
  
#split data into pieces
  newLeaseTable_nested <- newLeaseTable %>% group_by(Lease_ID) %>% nest()

  newLeaseTable_18P <- newLeaseTable_nested %>% mutate(updateDF = map(data, extend_18P_df)) %>%
                          select(Lease_ID, updateDF) %>%
                          unnest(updateDF)
  
  # append 18Periods data to previous table
  newLeaseTable_update <- dplyr::bind_rows(newLeaseTable, newLeaseTable_18P)

  return(newLeaseTable_update)
  
}) 
  
  #2) add YTD, QTD, PTD
new.lease.table.update2 <- reactive({   
  
    # req(new.lease.table.update())
  
     if(isTruthy(new.lease.table.update()) & nrow(new.lease.table.update()) > 0 ){
     newleasetableupdate <- new.lease.table.update()
  
     newlease_add_qrtnum <- newleasetableupdate %>% mutate(Fiscal_Quarter = case_when(Fiscal_Period %between% c(1,3) ~ 1,
                                                      Fiscal_Period %between% c(4,6) ~ 2,
                                                      Fiscal_Period %between% c(7,10) ~ 3,
                                                      TRUE ~ 4
                                ))
    
    
    newlease_add_qrtnum <- newlease_add_qrtnum %>% group_by(Lease_ID, Fiscal_Year) %>%
      mutate(YTD_Depn = (cumsum(-Right_of_Use_Amortization_Expense)),
             YTD_Interest = (cumsum(Financial_Liability_Interest_Expense)),
             YTD_ARO_Interest = abs(cumsum(ARO_Interest_Expense)),
             YTD_TI_Amort = (cumsum(-Tenant_Inducement_Period_Amortization)),
             YTD_Rent_Payments = cumsum(Financial_Liability_Lease_Payment))
    
    newlease_add_qrtnum <- newlease_add_qrtnum %>% group_by(Lease_ID, Fiscal_Year, Fiscal_Quarter ) %>%
      mutate(QTD_Depn = (cumsum(-Right_of_Use_Amortization_Expense)),
             QTD_Interest = (cumsum(Financial_Liability_Interest_Expense)),
             QTD_ARO_Interest = abs(cumsum(ARO_Interest_Expense)),
             QTD_TI_Amort = (cumsum(-Tenant_Inducement_Period_Amortization)),
             QTD_Rent_Payments = cumsum(Financial_Liability_Lease_Payment)
            )

    newlease_add_qrtnum <- newlease_add_qrtnum %>% group_by(Lease_ID, Fiscal_Year, Fiscal_Period ) %>%
      mutate(PTD_Depn = (cumsum(-Right_of_Use_Amortization_Expense)),
             PTD_Interest = (cumsum(Financial_Liability_Interest_Expense)),
             PTD_ARO_Interest = abs(cumsum(ARO_Interest_Expense)),
             PTD_TI_Amort = (cumsum(-Tenant_Inducement_Period_Amortization)),
             PTD_Rent_Payments = cumsum(Financial_Liability_Lease_Payment)
      )
    
    
    DT_newlease_add_qrtnum <- data.table(newlease_add_qrtnum)
    
    # add quarter row num
    DT_newlease_add_qrtnum[, quarter_row_num := seq_len(.N), by = c( "Lease_ID"  ,'Fiscal_Year','Fiscal_Quarter') ]
    
    #split data into pieces
    DT_newlease_add_qrtnum_nested <- DT_newlease_add_qrtnum %>% group_by(Lease_ID) %>% nest()
    
    newlease_update <- DT_newlease_add_qrtnum_nested %>% mutate(retained_earnings_YTD = map(data, retained_earnings_YTD),
                                                                retained_earnings_QTD = map(data, retained_earnings_QTD),
                                                                retained_earnings_PTD = map(data, retained_earnings_PTD)) %>%
                              select(Lease_ID, data, retained_earnings_YTD, retained_earnings_QTD,retained_earnings_PTD) %>%
                              unnest(data,retained_earnings_YTD,retained_earnings_QTD,retained_earnings_PTD)
    
    return(newlease_update)
     } else { return(NULL) }
}) 
##################################################################################################################
##################################################################################################################  

  # 2) for multiple lease multiple changes
  # add 18 periods 
  
  
multi_change_lease <- reactive({  
  
  req(multi.change.lease.table())
  
  multichangelease <- multi.change.lease.table()
  
  keepscol <- colnames(lease)

  multichangelease <-   multichangelease[, (colnames(multichangelease) %in% keepscol), drop = FALSE]
  
#split data into pieces
  multichangelease_nested <- multichangelease %>% group_by(Lease_ID) %>% nest()

  multichangelease_18P <- multichangelease_nested %>% mutate(updateDF = map(data, extend_18P_df)) %>%
                          select(Lease_ID, updateDF) %>%
                          unnest(updateDF)

   # append 18Periods data to previous table
  multichangelease_update <- dplyr::bind_rows(multichangelease, multichangelease_18P)
  
  return(multichangelease_update)
  
}) 
  
  #2) add YTD, QTD, PTD

multi_change_lease2 <- reactive({   
  
     #req(multi_change_lease())
   
    if(isTruthy(multi_change_lease()) & nrow(multi_change_lease()) >0 ){
  
     mulleasetableupdate <- multi_change_lease()
  
     mulleasetable_add_qrtnum <- mulleasetableupdate %>% mutate(Fiscal_Quarter = case_when(Fiscal_Period %between% c(1,3) ~ 1,
                                                      Fiscal_Period %between% c(4,6) ~ 2,
                                                      Fiscal_Period %between% c(7,10) ~ 3,
                                                      TRUE ~ 4
                                ))
    
    
    mulleasetable_add_qrtnum <- mulleasetable_add_qrtnum %>% group_by(Lease_ID, Fiscal_Year) %>%
      mutate(YTD_Depn = (cumsum(-Right_of_Use_Amortization_Expense)),
             YTD_Interest = (cumsum(Financial_Liability_Interest_Expense)),
             YTD_ARO_Interest = abs(cumsum(ARO_Interest_Expense)),
             YTD_TI_Amort = (cumsum(-Tenant_Inducement_Period_Amortization)),
             YTD_Rent_Payments = cumsum(Financial_Liability_Lease_Payment))
    
    mulleasetable_add_qrtnum <- mulleasetable_add_qrtnum %>% group_by(Lease_ID, Fiscal_Year, Fiscal_Quarter ) %>%
      mutate(QTD_Depn = (cumsum(-Right_of_Use_Amortization_Expense)),
             QTD_Interest = (cumsum(Financial_Liability_Interest_Expense)),
             QTD_ARO_Interest = abs(cumsum(ARO_Interest_Expense)),
             QTD_TI_Amort = (cumsum(-Tenant_Inducement_Period_Amortization)),
             QTD_Rent_Payments = cumsum(Financial_Liability_Lease_Payment)
            )

    mulleasetable_add_qrtnum <- mulleasetable_add_qrtnum %>% group_by(Lease_ID, Fiscal_Year, Fiscal_Period ) %>%
      mutate(PTD_Depn = (cumsum(-Right_of_Use_Amortization_Expense)),
             PTD_Interest = (cumsum(Financial_Liability_Interest_Expense)),
             PTD_ARO_Interest = abs(cumsum(ARO_Interest_Expense)),
             PTD_TI_Amort = (cumsum(-Tenant_Inducement_Period_Amortization)),
             PTD_Rent_Payments = cumsum(Financial_Liability_Lease_Payment)
      )
    
    
    DT_mulleasetable_add_qrtnum <- data.table(mulleasetable_add_qrtnum)
    
    # add quarter row num
    DT_mulleasetable_add_qrtnum[, quarter_row_num := seq_len(.N), by = c( "Lease_ID"  ,'Fiscal_Year','Fiscal_Quarter') ]
    
    #split data into pieces
    DT_mulleasetable_add_qrtnum_nested <- DT_mulleasetable_add_qrtnum %>% group_by(Lease_ID) %>% nest()
    
    multilease_update <- DT_mulleasetable_add_qrtnum_nested %>% mutate(retained_earnings_YTD = map(data, retained_earnings_YTD),
                                                                       retained_earnings_QTD = map(data, retained_earnings_QTD),
                                                                       retained_earnings_PTD = map(data, retained_earnings_PTD)) %>%
                              select(Lease_ID, data, retained_earnings_YTD, retained_earnings_QTD,retained_earnings_PTD) %>%
                              unnest(data,retained_earnings_YTD,retained_earnings_QTD,retained_earnings_PTD)
    
    return(multilease_update)
    } else { return(NULL)}
}) 
##################################################################################################################  
##################################################################################################################  

  # 3) for unified change
  # add 18 periods 
  
  
uni_change_lease <- reactive({  
  
  req(lease.table())
  
  unichangelease <- lease.table()
  
  keepscol <- colnames(lease)

  unichangelease <-   unichangelease[, (colnames(unichangelease) %in% keepscol), drop = FALSE]
  
#split data into pieces
  unichangelease_nested <- unichangelease %>% group_by(Lease_ID) %>% nest()

  unichangelease_18P <- unichangelease_nested %>% mutate(updateDF = map(data, extend_18P_df)) %>%
                          select(Lease_ID, updateDF) %>%
                          unnest(updateDF)
  
    # append 18Periods data to previous table
  unichangelease_update <- dplyr::bind_rows(unichangelease, unichangelease_18P)
  

  return(unichangelease_update)
  
}) 
  
  #2) add YTD, QTD, PTD

uni_change_lease2 <- reactive({   
  
    # req(uni_change_lease())
  
    if(isTruthy(uni_change_lease()) & nrow(uni_change_lease()) > 0 ){
  
     unileasetableupdate <- uni_change_lease()
  
     unileasetable_add_qrtnum <- unileasetableupdate %>% mutate(Fiscal_Quarter = case_when(Fiscal_Period %between% c(1,3) ~ 1,
                                                      Fiscal_Period %between% c(4,6) ~ 2,
                                                      Fiscal_Period %between% c(7,10) ~ 3,
                                                      TRUE ~ 4
                                ))
    
    
    unileasetable_add_qrtnum <- unileasetable_add_qrtnum %>% group_by(Lease_ID, Fiscal_Year) %>%
      mutate(YTD_Depn = (cumsum(-Right_of_Use_Amortization_Expense)),
             YTD_Interest = (cumsum(Financial_Liability_Interest_Expense)),
             YTD_ARO_Interest = abs(cumsum(ARO_Interest_Expense)),
             YTD_TI_Amort = (cumsum(-Tenant_Inducement_Period_Amortization)),
             YTD_Rent_Payments = cumsum(Financial_Liability_Lease_Payment))
    
    unileasetable_add_qrtnum <- unileasetable_add_qrtnum %>% group_by(Lease_ID, Fiscal_Year, Fiscal_Quarter ) %>%
      mutate(QTD_Depn = (cumsum(-Right_of_Use_Amortization_Expense)),
             QTD_Interest = (cumsum(Financial_Liability_Interest_Expense)),
             QTD_ARO_Interest = abs(cumsum(ARO_Interest_Expense)),
             QTD_TI_Amort = (cumsum(-Tenant_Inducement_Period_Amortization)),
             QTD_Rent_Payments = cumsum(Financial_Liability_Lease_Payment)
            )

    unileasetable_add_qrtnum <- unileasetable_add_qrtnum %>% group_by(Lease_ID, Fiscal_Year, Fiscal_Period ) %>%
      mutate(PTD_Depn = (cumsum(-Right_of_Use_Amortization_Expense)),
             PTD_Interest = (cumsum(Financial_Liability_Interest_Expense)),
             PTD_ARO_Interest = abs(cumsum(ARO_Interest_Expense)),
             PTD_TI_Amort = (cumsum(-Tenant_Inducement_Period_Amortization)),
             PTD_Rent_Payments = cumsum(Financial_Liability_Lease_Payment)
      )
    
    
    DT_unileasetable_add_qrtnum <- data.table(unileasetable_add_qrtnum)
    
    # add quarter row num
    DT_unileasetable_add_qrtnum[, quarter_row_num := seq_len(.N), by = c( "Lease_ID"  ,'Fiscal_Year','Fiscal_Quarter') ]
    
    #split data into pieces
    DT_unileasetable_add_qrtnum_nested <- DT_unileasetable_add_qrtnum %>% group_by(Lease_ID) %>% nest()
    
    unilease_update <- DT_unileasetable_add_qrtnum_nested %>% mutate(retained_earnings_YTD = map(data, retained_earnings_YTD),
                                                                     retained_earnings_QTD = map(data, retained_earnings_QTD),
                                                                     retained_earnings_PTD = map(data, retained_earnings_PTD)) %>%
                              select(Lease_ID, data, retained_earnings_YTD, retained_earnings_QTD,retained_earnings_PTD) %>%
                              unnest(data,retained_earnings_YTD,retained_earnings_QTD,retained_earnings_PTD)
    
    return(unilease_update)
    
    } else { return(NULL) }
    
   # return(unilease_update)
}) 

##################################################################################################################  
##################################################################################################################  
##################################################################################################################  
#################################################################################################################
################################################################################################################## 
################################################################################################################## 
#  Summary tables for New Lease Forecasting 

# add new lease to leaseBefore 
all_after_addNew <- reactive({  
  
  
   req(new.lease.table.update2())
   
   newleasework <- new.lease.table.update2()
  
   lease_before_minus_newlse <-  subset(DT_lease_before_update, !(DT_lease_before_update$Lease_ID %in% unique(newleasework$Lease_ID)))
  
   # Append new lease to original lease table
   addNewlease_after <- dplyr::bind_rows(lease_before_minus_newlse, newleasework)
   
   addNewlease_after <- addNewlease_after %>% mutate(type = 'After')
   
   return(addNewlease_after)

})

################################################################################################################## 
################################################################################################################## 
#  Summary tables for unified change Lease Forecasting 

all_after_unichange <- reactive({  
  
   req(uni_change_lease2())
  
   all_unichange_df <-  subset(DT_lease_before_update, !(DT_lease_before_update$Lease_ID %in% unique(uni_change_lease2()$Lease_ID)))
   
   all_addunichange <- dplyr::bind_rows(all_unichange_df,uni_change_lease2())
   
   all_addunichange <- all_addunichange %>% mutate(type = 'After')
   
   return(all_addunichange)

})
################################################################################################################## 
##################################################################################################################  
#  Summary tables for unified change Lease Forecasting 

all_after_mulchange <- reactive({  
  
   req(multi_change_lease2())
  
   all_mulchange_df <-  subset(DT_lease_before_update, !(DT_lease_before_update$Lease_ID %in% unique(multi_change_lease2()$Lease_ID)))
   
   all_addmulchange <- dplyr::bind_rows(all_mulchange_df, multi_change_lease2())
   
   all_addmulchange <- all_addmulchange %>% mutate(type = 'After')
   
   return(all_addmulchange)

})


################################################################################################################
################################################################################################################## 




#   add before, after, delta to each summary #########################################
# 
# lease.before.summary <- reactive({
# 
# lease_before_summary <- DT_lease_before_update %>% select(Lease_ID, PRPTY_NUM, Legal_Entity, Fiscal_Year, Fiscal_Period,
#                                                              Fiscal_Quarter, type,
#                                                             Right_of_Use_Ending, Financial_Liability_Ending_Detail,
#                                                             ARO_Ending , Tenant_Inducement_Ending ,retained_earnings_YTD ,
#                                                             retained_earnings_QTD, retained_earnings_PTD , YTD_Depn,
#                                                             YTD_Interest, YTD_ARO_Interest, YTD_TI_Amort, YTD_Rent_Payments,
#                                                             QTD_Depn,
#                                                             QTD_Interest, QTD_ARO_Interest, QTD_TI_Amort, QTD_Rent_Payments,
#                                                             PTD_Depn,
#                                                             PTD_Interest, PTD_ARO_Interest, PTD_TI_Amort, PTD_Rent_Payments,
#                                                             )
# return(lease_before_summary)
# 
# })

############################################################################
# 1) for new leases summary tab

all_after_newleaseadd <- reactive({  
  
  req(all_after_addNew())
  
  # for new leases, there are no Lease_ID. Only PRPTY_NUM & CO_CD 
  # pick up columns in DT_lease_before_update
  # Rename co_cd to Legal_Entity
  
  afteraddnew <- all_after_addNew()
  
  # colnames(afteraddnew)[colnames(afteraddnew) == 'CO_ID'] <- "Legal_Entity"
  
  # select columns needed for summary table 

  
  lease_after_summary <- afteraddnew %>% select(Lease_ID, PRPTY_NUM, Legal_Entity, Fiscal_Year, Fiscal_Period,
                                                 Fiscal_Quarter, type,
                                                            Right_of_Use_Ending, Financial_Liability_Ending_Detail,
                                                            ARO_Ending , Tenant_Inducement_Ending ,retained_earnings_YTD ,
                                                            retained_earnings_QTD, retained_earnings_PTD , YTD_Depn,
                                                            YTD_Interest, YTD_ARO_Interest, YTD_TI_Amort, YTD_Rent_Payments,
                                                            QTD_Depn,
                                                            QTD_Interest, QTD_ARO_Interest, QTD_TI_Amort, QTD_Rent_Payments,
                                                            PTD_Depn,
                                                            PTD_Interest, PTD_ARO_Interest, PTD_TI_Amort, PTD_Rent_Payments,
                                                            )
  
  mergetable <- lease_after_summary %>% full_join(lease_before_summary, by = c('Lease_ID' = 'Lease_ID',
                                                                               "PRPTY_NUM" = "PRPTY_NUM",
                                                                               "Legal_Entity" = "Legal_Entity",
                                                                               "Fiscal_Year" = "Fiscal_Year",
                                                                               "Fiscal_Period" = "Fiscal_Period",
                                                                               'Fiscal_Quarter' = 'Fiscal_Quarter'
                                                                            )) %>% mutate_if(is.numeric, coalesce, 0)
  print(paste("col names of mergetable", colnames(mergetable)))
  
  mergetable <- mergetable[, !(names(mergetable) %in% c('type.x', 'type.y'))]
  
  diff <- mergetable[ , grepl("*\\.x$",names(mergetable))] - mergetable[,grepl("*\\.y$",names(mergetable))]
  
  diff_update <- cbind(mergetable[,1:6, drop = FALSE], diff)
  
  names(diff_update) <- fix_names(names(diff_update))
  
  #####################
  # combine before, after, delta together 
  
  diff_update <-  diff_update %>% mutate(type = 'Delta') 
  
  
  bf_after_delta_newlease <- dplyr::bind_rows(lease_before_summary, lease_after_summary, diff_update)
  
  #################################################################################################
  PTDdf <- bf_after_delta_newlease %>% select(Lease_ID, PRPTY_NUM , Legal_Entity, Fiscal_Year, Fiscal_Period, Fiscal_Quarter,
                                              type,
                                             Right_of_Use_Ending, Financial_Liability_Ending_Detail,
                                             ARO_Ending, Tenant_Inducement_Ending, retained_earnings_PTD,
                                             PTD_Depn, PTD_Interest, PTD_ARO_Interest, PTD_TI_Amort, PTD_Rent_Payments ) %>% 
            mutate(reporttype = 'Period to Date')


  QTDdf <- bf_after_delta_newlease %>% select(Lease_ID, PRPTY_NUM , Legal_Entity, Fiscal_Year, Fiscal_Period, Fiscal_Quarter,
                                            type,
                                            Right_of_Use_Ending, Financial_Liability_Ending_Detail,
                                            ARO_Ending, Tenant_Inducement_Ending, retained_earnings_QTD,
                                            QTD_Depn, QTD_Interest, QTD_ARO_Interest, QTD_TI_Amort, QTD_Rent_Payments ) %>% 
            mutate(reporttype = 'Quarter to Date')

  YTDdf <- bf_after_delta_newlease %>% select(Lease_ID, PRPTY_NUM , Legal_Entity, Fiscal_Year, Fiscal_Period, Fiscal_Quarter,
                                            type,
                                            Right_of_Use_Ending, Financial_Liability_Ending_Detail,
                                            ARO_Ending, Tenant_Inducement_Ending, retained_earnings_YTD,
                                            YTD_Depn, YTD_Interest, YTD_ARO_Interest, YTD_TI_Amort, YTD_Rent_Payments ) %>% 
            mutate(reporttype = 'Year to Date')


  bf_after_delta_newleaseadd <- dplyr::bind_rows(PTDdf, QTDdf, YTDdf)
  
  ##################################################################################################
  
  return(bf_after_delta_newleaseadd)

})

################################################################################################################## 
################################################################################################################## 
# 2) for unified change 

all_after.unichange <- reactive({  
  
  req(all_after_unichange())
  
  afterunichange <- all_after_unichange()
  
  # select columns needed for summary table 
                                                  
  
  lease_after_summary.unichange <- afterunichange %>% select(Lease_ID, PRPTY_NUM, Legal_Entity, Fiscal_Year, Fiscal_Period,
                                                              Fiscal_Quarter, type,
                                                            Right_of_Use_Ending, Financial_Liability_Ending_Detail,
                                                            ARO_Ending , Tenant_Inducement_Ending ,retained_earnings_YTD ,
                                                            retained_earnings_QTD, retained_earnings_PTD , YTD_Depn,
                                                            YTD_Interest, YTD_ARO_Interest, YTD_TI_Amort, YTD_Rent_Payments,
                                                            QTD_Depn,
                                                            QTD_Interest, QTD_ARO_Interest, QTD_TI_Amort, QTD_Rent_Payments,
                                                            PTD_Depn,
                                                            PTD_Interest, PTD_ARO_Interest, PTD_TI_Amort, PTD_Rent_Payments,
                                                            )

  mergetabe.unichange <- lease_after_summary.unichange %>% full_join(lease_before_summary, by = c( 'Lease_ID' = 'Lease_ID',
                                                                                                     "PRPTY_NUM" = "PRPTY_NUM",
                                                                                                     "Legal_Entity" = "Legal_Entity",
                                                                                                     "Fiscal_Year" = "Fiscal_Year",
                                                                                                     "Fiscal_Period" = "Fiscal_Period",
                                                                                                     'Fiscal_Quarter' = 'Fiscal_Quarter'
                                                                            )) %>% mutate_if(is.numeric, coalesce, 0) 
  
  mergetabe.unichange <- mergetabe.unichange[, !(names(mergetabe.unichange) %in% c('type.x', 'type.y'))]
  
  diff_unichange <- mergetabe.unichange[ , grepl("*\\.x$",names(mergetabe.unichange))] - mergetabe.unichange[,grepl("*\\.y$",names(mergetabe.unichange))]
  
  diff_unichange_update <- cbind(mergetabe.unichange[,1:6, drop = FALSE], diff_unichange)
  
  names(diff_unichange_update) <- fix_names(names(diff_unichange_update))
  
  #####################
  # combine before, after, delta together 
  
  diff_unichange_update <-  diff_unichange_update %>% mutate(type = 'Delta') 
  
  
  bf_after_delta_unichange <- dplyr::bind_rows(lease_before_summary, lease_after_summary.unichange, diff_unichange_update)
  

#################################################################################################
  PTDdf.unichange <- bf_after_delta_unichange %>% select(Lease_ID, PRPTY_NUM , Legal_Entity, Fiscal_Year, Fiscal_Period, Fiscal_Quarter,
                                            type,
                                             Right_of_Use_Ending, Financial_Liability_Ending_Detail,
                                             ARO_Ending, Tenant_Inducement_Ending, retained_earnings_PTD,
                                             PTD_Depn, PTD_Interest, PTD_ARO_Interest, PTD_TI_Amort, PTD_Rent_Payments ) %>% 
                      mutate(reporttype = 'Period to Date')


  QTDdf.unichange <- bf_after_delta_unichange %>% select(Lease_ID, PRPTY_NUM , Legal_Entity, Fiscal_Year, Fiscal_Period, Fiscal_Quarter,
                                            type,
                                            Right_of_Use_Ending, Financial_Liability_Ending_Detail,
                                            ARO_Ending, Tenant_Inducement_Ending, retained_earnings_QTD,
                                            QTD_Depn, QTD_Interest, QTD_ARO_Interest, QTD_TI_Amort, QTD_Rent_Payments ) %>% 
                      mutate(reporttype = 'Quarter to Date')

  YTDdf.unichange <- bf_after_delta_unichange %>% select(Lease_ID, PRPTY_NUM , Legal_Entity, Fiscal_Year, Fiscal_Period, Fiscal_Quarter,
                                            type,
                                            Right_of_Use_Ending, Financial_Liability_Ending_Detail,
                                            ARO_Ending, Tenant_Inducement_Ending, retained_earnings_YTD,
                                            YTD_Depn, YTD_Interest, YTD_ARO_Interest, YTD_TI_Amort, YTD_Rent_Payments ) %>% 
                      mutate(reporttype = 'Year to Date')


  bf_after_delta_unichange_made <- dplyr::bind_rows(PTDdf.unichange, QTDdf.unichange, YTDdf.unichange)
  
  return(bf_after_delta_unichange_made)

})
################################################################################################################## 
################################################################################################################## 
# 3) for multiple changes 

all_after.mulchange <- reactive({  
  
  req(all_after_mulchange())
  
  aftermulchange <- all_after_mulchange()
  
  # select columns needed for summary table 
  
  lease_after_summary.mulchange <- aftermulchange %>% select(Lease_ID, PRPTY_NUM, Legal_Entity, Fiscal_Year, Fiscal_Period,
                                                              Fiscal_Quarter, type,
                                                            Right_of_Use_Ending, Financial_Liability_Ending_Detail,
                                                            ARO_Ending , Tenant_Inducement_Ending ,retained_earnings_YTD ,
                                                            retained_earnings_QTD, retained_earnings_PTD , YTD_Depn,
                                                            YTD_Interest, YTD_ARO_Interest, YTD_TI_Amort, YTD_Rent_Payments,
                                                            QTD_Depn,
                                                            QTD_Interest, QTD_ARO_Interest, QTD_TI_Amort, QTD_Rent_Payments,
                                                            PTD_Depn,
                                                            PTD_Interest, PTD_ARO_Interest, PTD_TI_Amort, PTD_Rent_Payments,
                                                            )

  mergetabe.mulchange <- lease_after_summary.mulchange %>% full_join(lease_before_summary,
                                                                     by = c( 'Lease_ID' = 'Lease_ID',
                                                                             "PRPTY_NUM" = "PRPTY_NUM",
                                                                             "Legal_Entity" = "Legal_Entity",
                                                                             "Fiscal_Year" = "Fiscal_Year",
                                                                             "Fiscal_Period" = "Fiscal_Period",
                                                                             'Fiscal_Quarter' = 'Fiscal_Quarter')) %>% 
                             mutate_if(is.numeric, coalesce, 0)

  mergetabe.mulchange <- mergetabe.mulchange[, !(names(mergetabe.mulchange) %in% c('type.x', 'type.y'))]
  
  diff_mulchange <- mergetabe.mulchange[ , grepl("*\\.x$",names(mergetabe.mulchange))] - mergetabe.mulchange[,grepl("*\\.y$",names(mergetabe.mulchange))]
  
  diff_mulchange_update <- cbind(mergetabe.mulchange[,1:6, drop = FALSE], diff_mulchange)
  
  names(diff_mulchange_update) <- fix_names(names(diff_mulchange_update))
  
  #####################
  # combine before, after, delta together 
  
  diff_mulchange_update <-  diff_mulchange_update %>% mutate(type = 'Delta') 
  
  
  bf_after_delta_mulchange <- dplyr::bind_rows(lease_before_summary, lease_after_summary.mulchange, diff_mulchange_update)
  
#################################################################################################
  PTDdf.mulchange <- bf_after_delta_mulchange %>% select(Lease_ID, PRPTY_NUM , Legal_Entity, Fiscal_Year, Fiscal_Period, Fiscal_Quarter,
                                            type,
                                             Right_of_Use_Ending, Financial_Liability_Ending_Detail,
                                             ARO_Ending, Tenant_Inducement_Ending, retained_earnings_PTD,
                                             PTD_Depn, PTD_Interest, PTD_ARO_Interest, PTD_TI_Amort, PTD_Rent_Payments ) %>% 
                      mutate(reporttype = 'Period to Date')


  QTDdf.mulchange  <- bf_after_delta_mulchange %>% select(Lease_ID, PRPTY_NUM , Legal_Entity, Fiscal_Year, Fiscal_Period, Fiscal_Quarter,
                                            type,
                                            Right_of_Use_Ending, Financial_Liability_Ending_Detail,
                                            ARO_Ending, Tenant_Inducement_Ending, retained_earnings_QTD,
                                            QTD_Depn, QTD_Interest, QTD_ARO_Interest, QTD_TI_Amort, QTD_Rent_Payments ) %>% 
                      mutate(reporttype = 'Quarter to Date')

  YTDdf.mulchange  <- bf_after_delta_mulchange %>% select(Lease_ID, PRPTY_NUM , Legal_Entity, Fiscal_Year, Fiscal_Period, Fiscal_Quarter,
                                            type,
                                            Right_of_Use_Ending, Financial_Liability_Ending_Detail,
                                            ARO_Ending, Tenant_Inducement_Ending, retained_earnings_YTD,
                                            YTD_Depn, YTD_Interest, YTD_ARO_Interest, YTD_TI_Amort, YTD_Rent_Payments ) %>% 
                      mutate(reporttype = 'Year to Date')


  bf_after_delta_mulchange_made <- dplyr::bind_rows(PTDdf.mulchange , QTDdf.mulchange , YTDdf.mulchange )

  
  return(bf_after_delta_mulchange_made)

})

################################################################################################################## 
################################################################################################################## 
##################################################################################################################
##################################################################################################################
#########    output summary table ###############################################################################
##################################################################################################################
##################################################################################################################
Summary_Lease_VS_Table <- reactive({    
  
    if(!isTruthy(all_after.unichange()) == FALSE){
  
       
       unilseadd_lse <- all_after.unichange()
       
       unilseadd_lse$Fiscal_Year <- as.character(unilseadd_lse$Fiscal_Year)
       unilseadd_lse$Fiscal_Period <- as.character(unilseadd_lse$Fiscal_Period)
       unilseadd_lse$Fiscal_Quarter <- as.character(unilseadd_lse$Fiscal_Quarter)
       
       if(input$selectbreakdownby == 'Lease ID'){
         
         
         if(isTruthy(input$reporttype) == TRUE & input$reporttype == 'Period to Date'){
           
           unilseadd_lse <- unilseadd_lse %>% filter(reporttype == input$reporttype) %>% 
                            select(-ends_with("QTD"), -ends_with("YTD"), - starts_with("QTD"), -starts_with("YTD"), -Fiscal_Quarter ) %>%
                            mutate(Financial_Liability_Ending_Detail = -abs(Financial_Liability_Ending_Detail)
                                   ,PTD_Rent_Payments = -abs(PTD_Rent_Payments)
                                   ) %>% 
                            rename( ROU_Closing = Right_of_Use_Ending,
                                    Liability_Closing = Financial_Liability_Ending_Detail,
                                    ARO_Closing = ARO_Ending,
                                    TI_Closing = Tenant_Inducement_Ending,
                                     Retained_Earning_Opening_PTD = retained_earnings_PTD,
                                     ROU_Depreciation_PTD = PTD_Depn,
                                     Liability_Interest_PTD = PTD_Interest,
                                     ARO_Interest_PTD = PTD_ARO_Interest,
                                     TI_Amortization_PTD = PTD_TI_Amort,
                                     Rental_Payments_PTD = PTD_Rent_Payments
                                  ) #%>% mutate_if(is.numeric, round, digits = 0L) %>% 
                             #mutate_if(is.numeric, formatC, digits = 0L, format="d", big.mark = ",")
           
           if(isTruthy(input$reportcutoff)){
           
          
                 if(input$reportcutoff == 'Delta'){
                   
                   unilseadd_lse <- unilseadd_lse %>% filter(type == input$reportcutoff)
                   
                   unilseadd_lse <- unilseadd_lse[rowSums(abs(unilseadd_lse[sapply(unilseadd_lse,is.numeric)]), na.rm = T)>0,,drop=F]
                   
                 }else{
                   
                   unilseadd_lse <- unilseadd_lse %>% filter(type == input$reportcutoff)
                   
                 }
                 
           }
           
           if(isTruthy(input$selectyear)){
          
           unilseadd_lse <- unilseadd_lse %>% filter(Fiscal_Year %in% input$selectyear) 
             
           }
           if(isTruthy(input$selectperiod)){
             
             unilseadd_lse <- unilseadd_lse %>% filter(Fiscal_Period  %in% input$selectperiod )
           }
           
           return(unilseadd_lse)
           
         }else if(isTruthy(input$reporttype) & input$reporttype == 'Quarter to Date'){
           
           unilseadd_lse <- unilseadd_lse %>% filter(reporttype == 'Quarter to Date') %>% 
                    select(-ends_with("PTD"), -ends_with("YTD"), - starts_with("PTD"), -starts_with("YTD"), -Fiscal_Period ) %>% 
                    mutate(Financial_Liability_Ending_Detail = -abs(Financial_Liability_Ending_Detail)
                           ,QTD_Rent_Payments = -abs(QTD_Rent_Payments)
                           ) %>%
                     rename( ROU_Closing = Right_of_Use_Ending,
                            Liability_Closing = Financial_Liability_Ending_Detail,
                            ARO_Closing = ARO_Ending,
                            TI_Closing = Tenant_Inducement_Ending,
                             Retained_Earning_Opening_QTD = retained_earnings_QTD,
                             ROU_Depreciation_QTD = QTD_Depn,
                             Liability_Interest_QTD = QTD_Interest,
                             ARO_Interest_QTD = QTD_ARO_Interest,
                             TI_Amortization_QTD = QTD_TI_Amort,
                             Rental_Payments_QTD = QTD_Rent_Payments
                            ) #%>% mutate_if(is.numeric, round, digits = 0L) %>% 
                             #mutate_if(is.numeric, formatC, digits = 0L, format="d", big.mark = ",")
           
           if(isTruthy(input$reportcutoff)){
           
                 if(input$reportcutoff == 'Delta'){
                   
                   unilseadd_lse <- unilseadd_lse %>% filter(type == input$reportcutoff)
                   
                   unilseadd_lse <- unilseadd_lse[rowSums(abs(unilseadd_lse[sapply(unilseadd_lse,is.numeric)]), na.rm = T)>0,,drop=F]
                   
                 }else{
                   
                   unilseadd_lse <- unilseadd_lse %>% filter(type == input$reportcutoff)
                   
                 }
           
           }
           
           if(isTruthy(input$selectyear)){
          
           unilseadd_lse <- unilseadd_lse %>% filter(Fiscal_Year %in% input$selectyear) 
             
           }
           if(isTruthy(input$selectperiod)){
             
             unilseadd_lse <- unilseadd_lse %>% filter(Fiscal_Period  %in% input$selectperiod )
           }
           
           return(unilseadd_lse)
           
         }else {
           
           unilseadd_lse <- unilseadd_lse %>% filter(reporttype == 'Year to Date') %>% 
                    select(-ends_with("PTD"), -ends_with("QTD"), - starts_with("PTD"), -starts_with("QTD"), 
                           -Fiscal_Period, -Fiscal_Quarter) %>% mutate(Financial_Liability_Ending_Detail = -abs(Financial_Liability_Ending_Detail)
                                                                       ,YTD_Rent_Payments = -abs(YTD_Rent_Payments)
                                                                       ) %>%
                   rename( ROU_Closing = Right_of_Use_Ending,
                            Liability_Closing = Financial_Liability_Ending_Detail,
                            ARO_Closing = ARO_Ending,
                            TI_Closing = Tenant_Inducement_Ending,
                             Retained_Earning_Opening_YTD = retained_earnings_YTD,
                             ROU_Depreciation_YTD = YTD_Depn,
                             Liability_Interest_YTD = YTD_Interest,
                             ARO_Interest_YTD = YTD_ARO_Interest,
                             TI_Amortization_YTD = YTD_TI_Amort,
                             Rental_Payments_YTD = YTD_Rent_Payments
                          ) #%>% mutate_if(is.numeric, round, digits = 0L) %>% 
                            # mutate_if(is.numeric, formatC, digits = 0L, format="d", big.mark = ",")
           
           if(isTruthy(input$reportcutoff)){
           
                 if(input$reportcutoff == 'Delta'){
                   
                   unilseadd_lse <- unilseadd_lse %>% filter(type == input$reportcutoff)
                   
                   unilseadd_lse <- unilseadd_lse[rowSums(abs(unilseadd_lse[sapply(unilseadd_lse,is.numeric)]), na.rm = T)>0,,drop=F]
                   
                 }else{
                   
                   unilseadd_lse <- unilseadd_lse %>% filter(type == input$reportcutoff)
                   
                 }
             
          }
           
           if(isTruthy(input$selectyear)){
          
           unilseadd_lse <- unilseadd_lse %>% filter(Fiscal_Year %in% input$selectyear) 
             
           }
           if(isTruthy(input$selectperiod)){
             
             unilseadd_lse <- unilseadd_lse %>% filter(Fiscal_Period  %in% input$selectperiod )
           
                }
         
              }
          return(unilseadd_lse)
         
         }else{
         
         
             if(isTruthy(input$reporttype) == TRUE & input$reporttype == 'Period to Date'){
         
                 
                uniq_lse <- length(unique(subset(all_after.unichange(), all_after.unichange()$reporttype == input$reporttype)$Lease_ID))
           
                unilseadd_lse <- unilseadd_lse %>% filter(reporttype == 'Period to Date') %>% 
                                 select(type , Lease_ID, Legal_Entity, Fiscal_Year, Fiscal_Period, Fiscal_Quarter,
                                         Right_of_Use_Ending,  
                                         Financial_Liability_Ending_Detail,
                                          ARO_Ending, Tenant_Inducement_Ending,retained_earnings_PTD ,
                                           PTD_Depn, PTD_Interest  , PTD_ARO_Interest  ,PTD_TI_Amort  ,PTD_Rent_Payments
                                          ) %>% group_by(type, Legal_Entity, Fiscal_Year, Fiscal_Period) %>%
                                           summarise( Lease_ID_Count = n_distinct(Lease_ID),
                                             ROU_Closing = sum(Right_of_Use_Ending, na.rm = TRUE),
                                             Liability_Closing = -(sum(Financial_Liability_Ending_Detail, na.rm = TRUE)),
                                             ARO_Closing = sum(ARO_Ending,na.rm = TRUE),
                                             TI_Closing = sum(Tenant_Inducement_Ending,na.rm = TRUE),
                                             Retained_Earning_Opening_PTD = sum(retained_earnings_PTD, na.rm = TRUE),
                                             ROU_Depreciation_PTD = sum(PTD_Depn,na.rm = TRUE),
                                             Liability_Interest_PTD = sum(PTD_Interest,na.rm = TRUE),
                                             ARO_Interest_PTD = sum(PTD_ARO_Interest,na.rm = TRUE),
                                             TI_Amortization_PTD = sum(PTD_TI_Amort,na.rm = TRUE),
                                             Rental_Payments_PTD = (sum(PTD_Rent_Payments,na.rm = TRUE))
                                           ) %>% arrange(type, Legal_Entity, Fiscal_Year, as.numeric(Fiscal_Period)) 
                  
                
                 unilseadd_lse$Lease_ID_Count <- as.character(unilseadd_lse$Lease_ID_Count)
                 
                 unilseadd_lse <- add_column(unilseadd_lse, ttl_unique_lse = rep(uniq_lse, nrow(unilseadd_lse)), .after = 4)
             
                 unilseadd_lse$ttl_unique_lse <- as.character(unilseadd_lse$ttl_unique_lse)
                 
                 if(isTruthy(input$reportcutoff)){
                   
                     uniq_lse <- length(unique(subset(all_after.unichange(), all_after.unichange()$reporttype == input$reporttype & 
                                                                             all_after.unichange()$type == input$reportcutoff)$Lease_ID))
                   
              
                     if(input$reportcutoff == 'Delta'){
                       
                       
                       unilseadd_lse <- unilseadd_lse %>% filter(type == input$reportcutoff)
                       
                       unilseadd_lse <- unilseadd_lse[rowSums(abs(unilseadd_lse[sapply(unilseadd_lse,is.numeric)]), na.rm = T)>0,,drop=F]
                       
                       unilseadd_lse$ttl_unique_lse <- rep(uniq_lse, nrow(unilseadd_lse))
                       
                     }else{
                       
                       unilseadd_lse <- unilseadd_lse %>% filter(type == input$reportcutoff)
                       
                       unilseadd_lse$ttl_unique_lse <- rep(uniq_lse, nrow(unilseadd_lse))
                       
                     }
                     
               }
                 
                 
                 
                 if(isTruthy(input$selectyear)){
                   
                     uniq_lse <- length(unique(subset(all_after.unichange(), all_after.unichange()$reporttype == input$reporttype & 
                                                      all_after.unichange()$type == input$reportcutoff &
                                                      all_after.unichange()$Fiscal_Year %in% input$selectyear
                                                      )$Lease_ID))
                     unilseadd_lse <- unilseadd_lse %>% filter(Fiscal_Year %in% input$selectyear) 
                     
                     unilseadd_lse$ttl_unique_lse <- rep(uniq_lse, nrow(unilseadd_lse))
                     
                   
                 }
                 if(isTruthy(input$selectperiod)){
                   
                     if(!isTruthy(input$selectyear)){
                   
                            uniq_lse <- length(unique(subset(all_after.unichange(), all_after.unichange()$reporttype == input$reporttype & 
                                                      all_after.unichange()$type == input$reportcutoff &
                                                      all_after.unichange()$Fiscal_Period %in% input$selectperiod)$Lease_ID))
                     }else{
                       
                            uniq_lse <- length(unique(subset(all_after.unichange(), all_after.unichange()$reporttype == input$reporttype & 
                                                          all_after.unichange()$type == input$reportcutoff &
                                                          all_after.unichange()$Fiscal_Year %in% input$selectyear &
                                                          all_after.unichange()$Fiscal_Period %in% input$selectperiod)$Lease_ID))
                       
                     }
                   
                     unilseadd_lse <- unilseadd_lse %>% filter(Fiscal_Period  %in% input$selectperiod )
                     
                     unilseadd_lse$ttl_unique_lse <- rep(uniq_lse, nrow(unilseadd_lse))
                    
                       
                       
                       
                     
               
                 }
                
                return(unilseadd_lse)
       
         }else if(isTruthy(input$reporttype) == TRUE & input$reporttype == 'Quarter to Date'){
           
           uniq_lse <- length(unique(subset(all_after.unichange(), all_after.unichange()$reporttype == input$reporttype)$Lease_ID))
           
           unilseadd_lse <- unilseadd_lse %>% filter(reporttype == 'Quarter to Date') %>% 
                    select(type , Lease_ID, Legal_Entity, Fiscal_Year, Fiscal_Period, Fiscal_Quarter, Right_of_Use_Ending,  
                           Financial_Liability_Ending_Detail,
                           ARO_Ending, Tenant_Inducement_Ending,retained_earnings_QTD ,
                           QTD_Depn, QTD_Interest  , QTD_ARO_Interest  , QTD_TI_Amort  , QTD_Rent_Payments
                    ) %>% group_by(type, Legal_Entity, Fiscal_Year, Fiscal_Quarter) %>%
                    summarise( Lease_ID_Count = n_distinct(Lease_ID),
                               ROU_Closing = sum(Right_of_Use_Ending, na.rm=T),
                               Liability_Closing = -(sum(Financial_Liability_Ending_Detail, na.rm = TRUE)),
                               ARO_Closing = sum(ARO_Ending, na.rm=T),
                               TI_Closing = sum(Tenant_Inducement_Ending, na.rm=T),
                               Retained_Earning_Opening_QTD = sum(retained_earnings_QTD, na.rm=T),
                               ROU_Depreciation_QTD = sum(QTD_Depn, na.rm=T),
                               Liability_Interest_QTD = sum(QTD_Interest, na.rm=T),
                               ARO_Interest_QTD = sum(QTD_ARO_Interest, na.rm=T),
                               TI_Amortization_QTD = sum(QTD_TI_Amort, na.rm=T),
                               Rental_Payments_QTD = (sum(QTD_Rent_Payments, na.rm=T))
                              ) %>% arrange(type, Legal_Entity, Fiscal_Year, Fiscal_Quarter)  #%>% mutate_if(is.numeric, round, digits = 0L) %>% 
                             #mutate_if(is.numeric, formatC, digits = 0L, format="d", big.mark = ",")
           
           unilseadd_lse$Lease_ID_Count <- as.character(unilseadd_lse$Lease_ID_Count)
           
           unilseadd_lse <- add_column(unilseadd_lse, ttl_unique_lse = rep(uniq_lse, nrow(unilseadd_lse)), .after = 4)
         
           unilseadd_lse$ttl_unique_lse <- as.character(unilseadd_lse$ttl_unique_lse)
           
           if(isTruthy(input$reportcutoff)){
             
                 uniq_lse <- length(unique(subset(all_after.unichange(), all_after.unichange()$reporttype == input$reporttype & 
                                                    all_after.unichange()$type == input$reportcutoff)$Lease_ID))
                 
                 
                 if(input$reportcutoff == 'Delta'){
                   
                   
                   unilseadd_lse <- unilseadd_lse %>% filter(type == input$reportcutoff)
                   
                   unilseadd_lse <- unilseadd_lse[rowSums(abs(unilseadd_lse[sapply(unilseadd_lse,is.numeric)]), na.rm = T)>0,,drop=F]
                   
                   unilseadd_lse$ttl_unique_lse <- rep(uniq_lse, nrow(unilseadd_lse))
                   
                 }else{
                   
                   unilseadd_lse <- unilseadd_lse %>% filter(type == input$reportcutoff)
                   
                   unilseadd_lse$ttl_unique_lse <- rep(uniq_lse, nrow(unilseadd_lse))
                   
                 }
             
           }
           
           
           if(isTruthy(input$selectyear)){
          
             uniq_lse <- length(unique(subset(all_after.unichange(), all_after.unichange()$reporttype == input$reporttype & 
                                                all_after.unichange()$type == input$reportcutoff &
                                                all_after.unichange()$Fiscal_Year %in% input$selectyear)$Lease_ID))
             
             unilseadd_lse <- unilseadd_lse %>% filter(Fiscal_Year %in% input$selectyear) 
             
             unilseadd_lse$ttl_unique_lse <- rep(uniq_lse, nrow(unilseadd_lse))
             
           }
           if(isTruthy(input$selectperiod)){
             
             if(!isTruthy(input$selectyear)){
               
               uniq_lse <- length(unique(subset(all_after.unichange(), all_after.unichange()$reporttype == input$reporttype & 
                                                  all_after.unichange()$type == input$reportcutoff &
                                                  all_after.unichange()$Fiscal_Period %in% input$selectperiod)$Lease_ID))
             }else{
               
               uniq_lse <- length(unique(subset(all_after.unichange(), all_after.unichange()$reporttype == input$reporttype & 
                                                  all_after.unichange()$type == input$reportcutoff &
                                                  all_after.unichange()$Fiscal_Year %in% input$selectyear &
                                                  all_after.unichange()$Fiscal_Period %in% input$selectperiod)$Lease_ID))
               
             }
             
             unilseadd_lse <- unilseadd_lse %>% filter(Fiscal_Period  %in% input$selectperiod )
             
             unilseadd_lse$ttl_unique_lse <- rep(uniq_lse, nrow(unilseadd_lse))
         
           }
           return(unilseadd_lse)
           
          }else{
            
               uniq_lse <- length(unique(subset(all_after.unichange(), all_after.unichange()$reporttype == input$reporttype)$Lease_ID))
      
               unilseadd_lse <- unilseadd_lse %>% filter(reporttype == 'Year to Date') %>% 
                 select(type , Lease_ID, Legal_Entity, Fiscal_Year, Fiscal_Period, Fiscal_Quarter, Right_of_Use_Ending, 
                        Financial_Liability_Ending_Detail,
                         ARO_Ending, Tenant_Inducement_Ending, retained_earnings_YTD ,
                          YTD_Depn, YTD_Interest  , YTD_ARO_Interest  ,YTD_TI_Amort  ,YTD_Rent_Payments
                            ) %>% group_by(type, Legal_Entity, Fiscal_Year, Fiscal_Period) %>%
                            summarise( Lease_ID_Count = n_distinct(Lease_ID),
                                       ROU_Closing = sum(Right_of_Use_Ending, na.rm=T),
                                       Liability_Closing = -(sum(Financial_Liability_Ending_Detail, na.rm = TRUE)),
                                       ARO_Closing = sum(ARO_Ending, na.rm=T),
                                       TI_Closing = sum(Tenant_Inducement_Ending, na.rm=T),
                                       Retained_Earning_Opening_YTD = sum(retained_earnings_YTD, na.rm=T),
                                       ROU_Depreciation_YTD = sum(YTD_Depn, na.rm=T),
                                       Liability_Interest_YTD = sum(YTD_Interest, na.rm=T),
                                       ARO_Interest_YTD = sum(YTD_ARO_Interest, na.rm=T),
                                       TI_Amortization_YTD = sum(YTD_TI_Amort, na.rm=T),
                                       Rental_Payments_YTD = (sum(YTD_Rent_Payments, na.rm=T))
                            ) %>% arrange(type, Legal_Entity, Fiscal_Year, as.numeric(Fiscal_Period))
               
                 unilseadd_lse$Lease_ID_Count <- as.character(unilseadd_lse$Lease_ID_Count)
               
                 unilseadd_lse <- add_column(unilseadd_lse, ttl_unique_lse = rep(uniq_lse, nrow(unilseadd_lse)), .after = 4)
                 
                 unilseadd_lse$ttl_unique_lse <- as.character(unilseadd_lse$ttl_unique_lse)
                 
                 if(isTruthy(input$reportcutoff)){
                       
                       uniq_lse <- length(unique(subset(all_after.unichange(), all_after.unichange()$reporttype == input$reporttype & 
                                                          all_after.unichange()$type == input$reportcutoff)$Lease_ID))
                       
                       
                       if(input$reportcutoff == 'Delta'){
                         
                         
                         unilseadd_lse <- unilseadd_lse %>% filter(type == input$reportcutoff)
                         
                         unilseadd_lse <- unilseadd_lse[rowSums(abs(unilseadd_lse[sapply(unilseadd_lse,is.numeric)]), na.rm = T)>0,,drop=F]
                         
                         unilseadd_lse$ttl_unique_lse <- rep(uniq_lse, nrow(unilseadd_lse))
                         
                       }else{
                         
                         unilseadd_lse <- unilseadd_lse %>% filter(type == input$reportcutoff)
                         
                         unilseadd_lse$ttl_unique_lse <- rep(uniq_lse, nrow(unilseadd_lse))
                         
                       }
                   
                 }

           if(isTruthy(input$selectyear)){
                
                       uniq_lse <- length(unique(subset(all_after.unichange(), all_after.unichange()$reporttype == input$reporttype & 
                                                          all_after.unichange()$type == input$reportcutoff &
                                                          all_after.unichange()$Fiscal_Year %in% input$selectyear)$Lease_ID))
                       unilseadd_lse <- unilseadd_lse %>% filter(Fiscal_Year %in% input$selectyear) 
                       
                       unilseadd_lse$ttl_unique_lse <- rep(uniq_lse, nrow(unilseadd_lse)) 
                       
              }
                 
            if(isTruthy(input$selectperiod)){
                   
                       if(!isTruthy(input$selectyear)){
                         
                              uniq_lse <- length(unique(subset(all_after.unichange(), all_after.unichange()$reporttype == input$reporttype & 
                                                              all_after.unichange()$type == input$reportcutoff &
                                                              all_after.unichange()$Fiscal_Period %in% input$selectperiod)$Lease_ID))
                         }else{
                         
                             uniq_lse <- length(unique(subset(all_after.unichange(), all_after.unichange()$reporttype == input$reporttype & 
                                                                all_after.unichange()$type == input$reportcutoff &
                                                                all_after.unichange()$Fiscal_Year %in% input$selectyear &
                                                                all_after.unichange()$Fiscal_Period %in% input$selectperiod)$Lease_ID))
                         
                       }
                       
                       unilseadd_lse <- unilseadd_lse %>% filter(Fiscal_Period  %in% input$selectperiod )
                       
                       unilseadd_lse$ttl_unique_lse <- rep(uniq_lse, nrow(unilseadd_lse))
                   
               
                 }
          }
           
    return(unilseadd_lse)
 
 }
    
  return(unilseadd_lse)  
      
} else{ return(NULL)}

  }
)
#############################################################################
################  @ 2019/5/9 ###############################################
##############################################################################
####################################################################################

output$Summary_Lease_VS_Table <- DT::renderDataTable( 

     {
       
       # for breakdown by Legal ID 
       
       if(is.null(Summary_Lease_VS_Table()) == FALSE & !("Lease_ID_Count" %in% colnames(Summary_Lease_VS_Table()))){

         DT::datatable(data = Summary_Lease_VS_Table(),
                       container = htmltools::withTags(
                         table(tableHeader(colnames(Summary_Lease_VS_Table())),
                         tableFooter(sapply(Summary_Lease_VS_Table(), function(x) if(is.numeric(x)) currency(sum(x, na.rm = TRUE), digits = 0L)))
                       )),
                   
                    rownames = FALSE,
                    options = list(paging = TRUE,
                                   searching = TRUE,
                                   #fixedColumns = FALSE,
                                   autoWidth = TRUE,
                                   ordering = TRUE,
                                   dom = 'Bfrtip',
                                   #deferRender = TRUE,
                                   scrollY = 400,
                                   scrollX = TRUE,
                                   scroller = TRUE,
                                   #buttons = c( 'csv'),
                                   pageLength = 8 ,  #nrow(unichange_summary),
                                   footerCallback = JS(jsCode)
                                   
                                   )
                   
                      )
         
       }else if(is.null(Summary_Lease_VS_Table()) == FALSE & ("Lease_ID_Count" %in% colnames(Summary_Lease_VS_Table()))) {
         

         jsCode2 <- sub("LeaseNum", unique(Summary_Lease_VS_Table()$ttl_unique_lse),jsCode2)
         
         report <- Summary_Lease_VS_Table()
         
         report$ttl_unique_lse <- NULL
         
         DT::datatable(data = report,
                       container = htmltools::withTags(
                         table(tableHeader(colnames(report)),
                               tableFooter(sapply(report, function(x) 
                                                          if(is.numeric(x)) currency(sum(x, na.rm = TRUE), digits = 0L)))
                         )),
                       
                       rownames = FALSE,
                       options = list(paging = TRUE,
                                      searching = TRUE,
                                      #fixedColumns = FALSE,
                                      autoWidth = TRUE,
                                      ordering = TRUE,
                                      dom = 'Bfrtip',
                                      #deferRender = TRUE,
                                      scrollY = 400,
                                      scrollX = TRUE,
                                      scroller = TRUE,
                                      #buttons = c( 'csv'),
                                      pageLength = 8 ,  #nrow(unichange_summary),
                                      footerCallback = JS(jsCode2)
                                      
                          )
                       
                    )
         
         
         
         
         
       }
       
       
       
       
       
       
       
})
  
    # Download Button for new leases
output$download_uniChange <- downloadHandler(

    filename = function(){
      paste0("Unified Change Lease Report", "_", now(), ".", "xlsx")
    },

    if(!isTruthy(Summary_Lease_VS_Table()) == FALSE){
      content = function(file) {
        

        # Write to a file specified by the 'file' argument
        writexl::write_xlsx( Summary_Lease_VS_Table(),
                     file 
                           )
      }

    } else{return(NULL)}

  )  
################################################################################################################## 
################################################################################################################## 
####################################################################################################
  #  summary tab for new lease

# 2) new lease 
New_Lease_VS_Table_output <- reactive({    
  
    if(!isTruthy(all_after_newleaseadd()) == FALSE){
      
       newlseadd_lse <- all_after_newleaseadd()
       
       #print(paste("col names of newlseadd_lse", colnames(newlseadd_lse)))
       
       newlseadd_lse$Fiscal_Year <- as.character(newlseadd_lse$Fiscal_Year)
       newlseadd_lse$Fiscal_Period <- as.character(newlseadd_lse$Fiscal_Period)
       newlseadd_lse$Fiscal_Quarter <- as.character(newlseadd_lse$Fiscal_Quarter)
       
       if(input$newselectbreakdownby == 'Lease ID'){
         
         
         if(isTruthy(input$newreporttype) == TRUE & input$newreporttype == 'Period to Date'){
           
           newlseadd_lse <- newlseadd_lse %>% filter(reporttype == input$newreporttype) %>% 
                            select(-ends_with("QTD"), -ends_with("YTD"), - starts_with("QTD"), -starts_with("YTD"), -Fiscal_Quarter ) %>% 
                            mutate(Financial_Liability_Ending_Detail = -abs(Financial_Liability_Ending_Detail)
                                   ,PTD_Rent_Payments = -abs(PTD_Rent_Payments)
                                   ) %>% 
                            rename( ROU_Closing = Right_of_Use_Ending,
                                    Liability_Closing = Financial_Liability_Ending_Detail,
                                    ARO_Closing = ARO_Ending,
                                    TI_Closing = Tenant_Inducement_Ending,
                                     Retained_Earning_Opening_PTD = retained_earnings_PTD,
                                     ROU_Depreciation_PTD = PTD_Depn,
                                     Liability_Interest_PTD = PTD_Interest,
                                     ARO_Interest_PTD = PTD_ARO_Interest,
                                     TI_Amortization_PTD = PTD_TI_Amort,
                                     Rental_Payments_PTD = PTD_Rent_Payments
                                  ) #%>% mutate_if(is.numeric, round, digits = 0L) %>% 
                             #mutate_if(is.numeric, formatC, digits = 0L, format="d", big.mark = ",")
           
           if(isTruthy(input$newreportcutoff)){
           
                 
                  
                  if(input$newreportcutoff == 'Delta'){
                    
                    newlseadd_lse <- newlseadd_lse %>% filter(type == input$newreportcutoff)
                    
                    newlseadd_lse <- newlseadd_lse[rowSums(abs(newlseadd_lse[sapply(newlseadd_lse,is.numeric)]), na.rm = T)>0,,drop=F]
                    
                  }else{
                    
                    newlseadd_lse <- newlseadd_lse %>% filter(type == input$newreportcutoff)
                    
                  }
           
           }
           
           if(isTruthy(input$newselectyear)){
          
           newlseadd_lse <- newlseadd_lse %>% filter(Fiscal_Year %in% input$newselectyear) 
             
           }
           if(isTruthy(input$newselectperiod)){
             
             newlseadd_lse <- newlseadd_lse %>% filter(Fiscal_Period  %in% input$newselectperiod )
           }
           
           return(newlseadd_lse)
           
         }else if(isTruthy(input$newreporttype) & input$newreporttype == 'Quarter to Date'){
           
           newlseadd_lse <- newlseadd_lse %>% filter(reporttype == 'Quarter to Date') %>% 
                    select(-ends_with("PTD"), -ends_with("YTD"), - starts_with("PTD"), -starts_with("YTD"), -Fiscal_Period ) %>% 
                    mutate(Financial_Liability_Ending_Detail = -abs(Financial_Liability_Ending_Detail)
                           ,QTD_Rent_Payments = -abs(QTD_Rent_Payments)
                           ) %>% 
                     rename( ROU_Closing = Right_of_Use_Ending,
                            Liability_Closing = Financial_Liability_Ending_Detail,
                            ARO_Closing = ARO_Ending,
                            TI_Closing = Tenant_Inducement_Ending,
                             Retained_Earning_Opening_QTD = retained_earnings_QTD,
                             ROU_Depreciation_QTD = QTD_Depn,
                             Liability_Interest_QTD = QTD_Interest,
                             ARO_Interest_QTD = QTD_ARO_Interest,
                             TI_Amortization_QTD = QTD_TI_Amort,
                             Rental_Payments_QTD = QTD_Rent_Payments
                            ) #%>% mutate_if(is.numeric, round, digits = 0L) %>% 
                             #mutate_if(is.numeric, formatC, digits = 0L, format="d", big.mark = ",")
           
           if(isTruthy(input$newreportcutoff)){
           
                 if(input$newreportcutoff == 'Delta'){
                   
                   newlseadd_lse <- newlseadd_lse %>% filter(type == input$newreportcutoff)
                   
                   newlseadd_lse <- newlseadd_lse[rowSums(abs(newlseadd_lse[sapply(newlseadd_lse,is.numeric)]), na.rm = T)>0,,drop=F]
                   
                 }else{
                   
                   newlseadd_lse <- newlseadd_lse %>% filter(type == input$newreportcutoff)
                   
                 }
           
           }
           
           if(isTruthy(input$newselectyear)){
          
           newlseadd_lse <- newlseadd_lse %>% filter(Fiscal_Year %in% input$newselectyear) 
             
           }
           if(isTruthy(input$newselectperiod)){
             
             newlseadd_lse <- newlseadd_lse %>% filter(Fiscal_Period  %in% input$newselectperiod )
           }
           
           return(newlseadd_lse)
           
         }else {
           
           newlseadd_lse <- newlseadd_lse %>% filter(reporttype == 'Year to Date') %>% 
                    select(-ends_with("PTD"), -ends_with("QTD"), - starts_with("PTD"), -starts_with("QTD"), 
                           -Fiscal_Period, -Fiscal_Quarter) %>% 
                   mutate(Financial_Liability_Ending_Detail = -abs(Financial_Liability_Ending_Detail)
                          ,YTD_Rent_Payments = -abs(YTD_Rent_Payments)
                          ) %>% 
                   rename( ROU_Closing = Right_of_Use_Ending,
                            Liability_Closing = Financial_Liability_Ending_Detail,
                            ARO_Closing = ARO_Ending,
                            TI_Closing = Tenant_Inducement_Ending,
                             Retained_Earning_Opening_YTD = retained_earnings_YTD,
                             ROU_Depreciation_YTD = YTD_Depn,
                             Liability_Interest_YTD = YTD_Interest,
                             ARO_Interest_YTD = YTD_ARO_Interest,
                             TI_Amortization_YTD = YTD_TI_Amort,
                             Rental_Payments_YTD = YTD_Rent_Payments
                          )# %>% mutate_if(is.numeric, round, digits = 0L) %>% 
                            # mutate_if(is.numeric, formatC, digits = 0L, format="d", big.mark = ",")
           
           if(isTruthy(input$newreportcutoff)){
           
                 if(input$newreportcutoff == 'Delta'){
                   
                   newlseadd_lse <- newlseadd_lse %>% filter(type == input$newreportcutoff)
                   
                   newlseadd_lse <- newlseadd_lse[rowSums(abs(newlseadd_lse[sapply(newlseadd_lse,is.numeric)]), na.rm = T)>0,,drop=F]
                   
                 }else{
                   
                   newlseadd_lse <- newlseadd_lse %>% filter(type == input$newreportcutoff)
                   
                 }
           
           }
           
           if(isTruthy(input$newselectyear)){
          
           newlseadd_lse <- newlseadd_lse %>% filter(Fiscal_Year %in% input$newselectyear) 
             
           }
           if(isTruthy(input$newselectperiod)){
             
             newlseadd_lse <- newlseadd_lse %>% filter(Fiscal_Period  %in% input$newselectperiod )
           
                }
         
              }
          return(newlseadd_lse)
         
         }else{
         
         
         if(isTruthy(input$newreporttype) == TRUE & input$newreporttype == 'Period to Date'){
           
           uniq_lse_newlse <- length(unique(subset(all_after_newleaseadd(), all_after_newleaseadd()$reporttype == input$newreporttype)$Lease_ID)) 
         
           newlseadd_lse <- newlseadd_lse %>% filter(reporttype == 'Period to Date') %>% 
                      select(type , Lease_ID, Legal_Entity, Fiscal_Year, Fiscal_Period, Fiscal_Quarter,Right_of_Use_Ending,  
                             Financial_Liability_Ending_Detail,
                              ARO_Ending, Tenant_Inducement_Ending,retained_earnings_PTD ,
                               PTD_Depn, PTD_Interest  , PTD_ARO_Interest  ,PTD_TI_Amort  ,PTD_Rent_Payments
                              ) %>% group_by(type, Legal_Entity, Fiscal_Year, Fiscal_Period) %>%
                               summarise( Lease_ID_Count = n_distinct(Lease_ID),
                                 ROU_Closing = sum(Right_of_Use_Ending, na.rm = T),
                                 Liability_Closing = -(sum(Financial_Liability_Ending_Detail, na.rm = TRUE)),
                                 ARO_Closing = sum(ARO_Ending, na.rm = T),
                                 TI_Closing = sum(Tenant_Inducement_Ending, na.rm = T),
                                 Retained_Earning_Opening_PTD = sum(retained_earnings_PTD, na.rm = T),
                                 ROU_Depreciation_PTD = sum(PTD_Depn, na.rm = T),
                                 Liability_Interest_PTD = sum(PTD_Interest, na.rm = T),
                                 ARO_Interest_PTD = sum(PTD_ARO_Interest, na.rm = T),
                                 TI_Amortization_PTD = sum(PTD_TI_Amort, na.rm = T),
                                 Rental_Payments_PTD = (sum(PTD_Rent_Payments, na.rm = T))
                               ) %>% arrange(type, Legal_Entity, Fiscal_Year, as.numeric(Fiscal_Period)) #%>% mutate_if(is.numeric, round, digits = 0L) %>% 
                             #mutate_if(is.numeric, formatC, digits = 0L, format="d", big.mark = ",")
           newlseadd_lse$Lease_ID_Count <- as.character(newlseadd_lse$Lease_ID_Count)
           
           newlseadd_lse <- add_column(newlseadd_lse, ttl_unique_lse = rep(uniq_lse_newlse, nrow(newlseadd_lse)), .after = 4)
           
           newlseadd_lse$ttl_unique_lse <- as.character(newlseadd_lse$ttl_unique_lse)
                     
           if(isTruthy(input$newreportcutoff)){
             
                  uniq_lse_newlse <- length(unique(subset(all_after_newleaseadd(), all_after_newleaseadd()$reporttype == input$newreporttype & 
                                                       all_after_newleaseadd()$type == input$newreportcutoff)$Lease_ID))
                  
                  #print(paste("uniq_lse_newlse", uniq_lse_newlse))
           
                 if(input$newreportcutoff == 'Delta'){
                   
                   
                   newlseadd_lse <- newlseadd_lse %>% filter(type == input$newreportcutoff)
                   
                   newlseadd_lse <- newlseadd_lse[rowSums(abs(newlseadd_lse[sapply(newlseadd_lse,is.numeric)]), na.rm = T)>0,,drop=F]
                   
                   newlseadd_lse$ttl_unique_lse <- rep(uniq_lse_newlse, nrow(newlseadd_lse))
                   
                 }else{
                   
                   newlseadd_lse <- newlseadd_lse %>% filter(type == input$newreportcutoff)
                   
                   newlseadd_lse$ttl_unique_lse <- rep(uniq_lse_newlse, nrow(newlseadd_lse))
                   
                 }
           
           }
           
           if(isTruthy(input$newselectyear)){
             
             uniq_lse_newlse <- length(unique(subset(all_after_newleaseadd(), all_after_newleaseadd()$reporttype == input$newreporttype & 
                                                       all_after_newleaseadd()$type == input$newreportcutoff &
                                                       all_after_newleaseadd()$Fiscal_Year %in% input$newselectyear)$Lease_ID))
          
             newlseadd_lse <- newlseadd_lse %>% filter(Fiscal_Year %in% input$newselectyear) 
             
             newlseadd_lse$ttl_unique_lse <- rep(uniq_lse_newlse, nrow(newlseadd_lse))
             
           }
           if(isTruthy(input$newselectperiod)){
             
                 if(!isTruthy(input$newselectyear)){
                   
                   uniq_lse_newlse <- length(unique(subset(all_after_newleaseadd(), all_after_newleaseadd()$reporttype == input$newreporttype & 
                                                             all_after_newleaseadd()$type == input$newreportcutoff &
                                                             all_after_newleaseadd()$Fiscal_Period %in% input$newselectperiod)$Lease_ID))
                 }else{
                   
                   uniq_lse_newlse <- length(unique(subset(all_after_newleaseadd(), all_after_newleaseadd()$reporttype == input$newreporttype & 
                                                             all_after_newleaseadd()$type == input$newreportcutoff &
                                                             all_after_newleaseadd()$Fiscal_Year %in% input$newselectyear &
                                                             all_after_newleaseadd()$Fiscal_Period %in% input$newselectperiod)$Lease_ID))
                   
                 }
             
              newlseadd_lse <- newlseadd_lse %>% filter(Fiscal_Period  %in% input$newselectperiod )
              newlseadd_lse$ttl_unique_lse <- rep(uniq_lse_newlse, nrow(newlseadd_lse))
         
           }
          
          return(newlseadd_lse)
 
         }else if(isTruthy(input$newreporttype) == TRUE & input$newreporttype == 'Quarter to Date'){
           
           uniq_lse_newlse <- length(unique(subset(all_after_newleaseadd(), all_after_newleaseadd()$reporttype == input$newreporttype)$Lease_ID)) 
           
           newlseadd_lse <- newlseadd_lse %>% filter(reporttype == 'Quarter to Date') %>% 
                    select(type , Lease_ID, Legal_Entity, Fiscal_Year, Fiscal_Period, Fiscal_Quarter, Right_of_Use_Ending,  
                           Financial_Liability_Ending_Detail,
                           ARO_Ending, Tenant_Inducement_Ending,retained_earnings_QTD ,
                           QTD_Depn, QTD_Interest  , QTD_ARO_Interest  ,QTD_TI_Amort  ,QTD_Rent_Payments
                    ) %>% group_by(type, Legal_Entity, Fiscal_Year, Fiscal_Quarter) %>%
                    summarise( Lease_ID_Count = n_distinct(Lease_ID),
                               ROU_Closing = sum(Right_of_Use_Ending, na.rm = T),
                               Liability_Closing = -(sum(Financial_Liability_Ending_Detail, na.rm = TRUE)),
                               ARO_Closing = sum(ARO_Ending, na.rm = T),
                               TI_Closing = sum(Tenant_Inducement_Ending, na.rm = T),
                               Retained_Earning_Opening_QTD = sum(retained_earnings_QTD, na.rm = T),
                               ROU_Depreciation_QTD = sum(QTD_Depn, na.rm = T),
                               Liability_Interest_QTD = sum(QTD_Interest, na.rm = T),
                               ARO_Interest_QTD = sum(QTD_ARO_Interest, na.rm = T),
                               TI_Amortization_QTD = sum(QTD_TI_Amort, na.rm = T),
                               Rental_Payments_QTD = (sum(QTD_Rent_Payments, na.rm = T))
                              ) %>% arrange(type, Legal_Entity, Fiscal_Year, Fiscal_Quarter)# %>% mutate_if(is.numeric, round, digits = 0L) %>% 
                             #mutate_if(is.numeric, formatC, digits = 0L, format="d", big.mark = ",")
           
           newlseadd_lse$Lease_ID_Count <- as.character(newlseadd_lse$Lease_ID_Count)
           
           newlseadd_lse <- add_column(newlseadd_lse, ttl_unique_lse = rep(uniq_lse_newlse, nrow(newlseadd_lse)), .after = 4)
           
           newlseadd_lse$ttl_unique_lse <- as.character(newlseadd_lse$ttl_unique_lse)
           
           if(isTruthy(input$newreportcutoff)){
                 
                 uniq_lse_newlse <- length(unique(subset(all_after_newleaseadd(), all_after_newleaseadd()$reporttype == input$newreporttype & 
                                                           all_after_newleaseadd()$type == input$newreportcutoff)$Lease_ID))
                 
                 if(input$newreportcutoff == 'Delta'){
                   
                   
                   newlseadd_lse <- newlseadd_lse %>% filter(type == input$newreportcutoff)
                   
                   newlseadd_lse <- newlseadd_lse[rowSums(abs(newlseadd_lse[sapply(newlseadd_lse,is.numeric)]), na.rm = T)>0,,drop=F]
                   
                   newlseadd_lse$ttl_unique_lse <- rep(uniq_lse_newlse, nrow(newlseadd_lse))
                   
                 }else{
                   
                   newlseadd_lse <- newlseadd_lse %>% filter(type == input$newreportcutoff)
                   
                   newlseadd_lse$ttl_unique_lse <- rep(uniq_lse_newlse, nrow(newlseadd_lse))
                   
                 }
                 
           }
           
           if(isTruthy(input$newselectyear)){
             
             uniq_lse_newlse <- length(unique(subset(all_after_newleaseadd(), all_after_newleaseadd()$reporttype == input$newreporttype & 
                                                       all_after_newleaseadd()$type == input$newreportcutoff &
                                                       all_after_newleaseadd()$Fiscal_Year %in% input$newselectyear)$Lease_ID))
          
             newlseadd_lse <- newlseadd_lse %>% filter(Fiscal_Year %in% input$newselectyear) 
             
             newlseadd_lse$ttl_unique_lse <- rep(uniq_lse_newlse, nrow(newlseadd_lse))
             
           }
           if(isTruthy(input$newselectperiod)){
             
             if(!isTruthy(input$newselectyear)){
               
               uniq_lse_newlse <- length(unique(subset(all_after_newleaseadd(), all_after_newleaseadd()$reporttype == input$newreporttype & 
                                                         all_after_newleaseadd()$type == input$newreportcutoff &
                                                         all_after_newleaseadd()$Fiscal_Period %in% input$newselectperiod)$Lease_ID))
             }else{
               
               uniq_lse_newlse <- length(unique(subset(all_after_newleaseadd(), all_after_newleaseadd()$reporttype == input$newreporttype & 
                                                         all_after_newleaseadd()$type == input$newreportcutoff &
                                                         all_after_newleaseadd()$Fiscal_Year %in% input$newselectyear &
                                                         all_after_newleaseadd()$Fiscal_Period %in% input$newselectperiod)$Lease_ID))
               
             }
             
             newlseadd_lse <- newlseadd_lse %>% filter(Fiscal_Period  %in% input$newselectperiod )
             newlseadd_lse$ttl_unique_lse <- rep(uniq_lse_newlse, nrow(newlseadd_lse))
         
           }
           return(newlseadd_lse)
           
          }else{
            
             uniq_lse_newlse <- length(unique(subset(all_after_newleaseadd(), all_after_newleaseadd()$reporttype == input$newreporttype)$Lease_ID)) 
             
             newlseadd_lse <- newlseadd_lse %>% filter(reporttype == 'Year to Date') %>% 
               select(type , Lease_ID, Legal_Entity, Fiscal_Year, Fiscal_Period, Fiscal_Quarter, Right_of_Use_Ending, 
                      Financial_Liability_Ending_Detail,
                       ARO_Ending, Tenant_Inducement_Ending,retained_earnings_YTD ,
                       YTD_Depn, YTD_Interest  , YTD_ARO_Interest  ,YTD_TI_Amort  ,YTD_Rent_Payments
                          ) %>% group_by(type, Legal_Entity, Fiscal_Year, Fiscal_Period) %>%
                          summarise( Lease_ID_Count = n_distinct(Lease_ID),
                                     ROU_Closing = sum(Right_of_Use_Ending, na.rm = T),
                                     Liability_Closing = -(sum(Financial_Liability_Ending_Detail, na.rm = TRUE)),
                                     ARO_Closing = sum(ARO_Ending, na.rm = T),
                                     TI_Closing = sum(Tenant_Inducement_Ending, na.rm = T),
                                     Retained_Earning_Opening_YTD = sum(retained_earnings_YTD, na.rm = T),
                                     ROU_Depreciation_YTD = sum(YTD_Depn, na.rm = T),
                                     Liability_Interest_YTD = sum(YTD_Interest, na.rm = T),
                                     ARO_Interest_YTD = sum(YTD_ARO_Interest, na.rm = T),
                                     TI_Amortization_YTD = sum(YTD_TI_Amort, na.rm = T),
                                     Rental_Payments_YTD = (sum(YTD_Rent_Payments, na.rm = T))
                          ) %>% arrange(type, Legal_Entity, Fiscal_Year, as.numeric(Fiscal_Period)) #%>% mutate_if(is.numeric, round, digits = 0L) %>% 
                             #    mutate_if(is.numeric, formatC, digits = 0L, format="d", big.mark = ",")
                newlseadd_lse$Lease_ID_Count <- as.character(newlseadd_lse$Lease_ID_Count)
             
                newlseadd_lse <- add_column(newlseadd_lse, ttl_unique_lse = rep(uniq_lse_newlse, nrow(newlseadd_lse)), .after = 4)
                
                newlseadd_lse$ttl_unique_lse <- as.character(newlseadd_lse$ttl_unique_lse)
                
                if(isTruthy(input$newreportcutoff)){
                  
                      uniq_lse_newlse <- length(unique(subset(all_after_newleaseadd(), all_after_newleaseadd()$reporttype == input$newreporttype & 
                                                                all_after_newleaseadd()$type == input$newreportcutoff)$Lease_ID))
                      
                      if(input$newreportcutoff == 'Delta'){
                        
                        
                        newlseadd_lse <- newlseadd_lse %>% filter(type == input$newreportcutoff)
                        
                        newlseadd_lse <- newlseadd_lse[rowSums(abs(newlseadd_lse[sapply(newlseadd_lse,is.numeric)]), na.rm = T)>0,,drop=F]
                        
                        newlseadd_lse$ttl_unique_lse <- rep(uniq_lse_newlse, nrow(newlseadd_lse))
                        
                      }else{
                        
                        newlseadd_lse <- newlseadd_lse %>% filter(type == input$newreportcutoff)
                        
                        newlseadd_lse$ttl_unique_lse <- rep(uniq_lse_newlse, nrow(newlseadd_lse))
                        
                      }
                      
                }
          
               
               if(isTruthy(input$newselectyear)){
              
                 uniq_lse_newlse <- length(unique(subset(all_after_newleaseadd(), all_after_newleaseadd()$reporttype == input$newreporttype & 
                                                           all_after_newleaseadd()$type == input$newreportcutoff &
                                                           all_after_newleaseadd()$Fiscal_Year %in% input$newselectyear)$Lease_ID))
                 
                 newlseadd_lse <- newlseadd_lse %>% filter(Fiscal_Year %in% input$newselectyear) 
                 
                 newlseadd_lse$ttl_unique_lse <- rep(uniq_lse_newlse, nrow(newlseadd_lse)) 
                 
               }
               if(isTruthy(input$newselectperiod)){
                 
                     if(!isTruthy(input$newselectyear)){
                       
                       uniq_lse_newlse <- length(unique(subset(all_after_newleaseadd(), all_after_newleaseadd()$reporttype == input$newreporttype & 
                                                                 all_after_newleaseadd()$type == input$newreportcutoff &
                                                                 all_after_newleaseadd()$Fiscal_Period %in% input$newselectperiod)$Lease_ID))
                     }else{
                       
                       uniq_lse_newlse <- length(unique(subset(all_after_newleaseadd(), all_after_newleaseadd()$reporttype == input$newreporttype & 
                                                                 all_after_newleaseadd()$type == input$newreportcutoff &
                                                                 all_after_newleaseadd()$Fiscal_Year %in% input$newselectyear &
                                                                 all_after_newleaseadd()$Fiscal_Period %in% input$newselectperiod)$Lease_ID))
                       
                     }
                     
                     newlseadd_lse <- newlseadd_lse %>% filter(Fiscal_Period  %in% input$newselectperiod )
                     newlseadd_lse$ttl_unique_lse <- rep(uniq_lse_newlse, nrow(newlseadd_lse))
             
               }
           }
    return(newlseadd_lse)
 
 }
    
  return(newlseadd_lse)  

} else{ return(NULL)}

  }
)


output$New_Lease_VS_Table <- DT::renderDataTable( 

       {
         
         # for breakdown by Legal ID 
         
      if(is.null(New_Lease_VS_Table_output()) == FALSE & !("Lease_ID_Count" %in% colnames(New_Lease_VS_Table_output()))){   
         
         
                 DT::datatable(data = New_Lease_VS_Table_output(),
                               container = htmltools::withTags(
                                 table(tableHeader(colnames(New_Lease_VS_Table_output())),
                                       tableFooter(sapply(New_Lease_VS_Table_output(), function(x) if(is.numeric(x)) currency(sum(x, na.rm = TRUE), digits = 0L)))
                                 )),
                            rownames = FALSE,
                            options = list(paging = TRUE,
                                           searching = TRUE,
                                           #fixedColumns = FALSE,
                                           autoWidth = TRUE,
                                           ordering = TRUE,
                                           dom = 'Bfrtip',
                                           deferRender = TRUE,
                                           scrollY = 400,
                                           scrollX = TRUE,
                                           scroller = TRUE,
                                           #buttons = c( 'csv'),
                                           pageLength = 80,
                                           footerCallback = JS(jsCode)
                                           
                                           )
                           )
      }else if(is.null(New_Lease_VS_Table_output()) == FALSE & ("Lease_ID_Count" %in% colnames(New_Lease_VS_Table_output()))) {
        
        
        jsCode2 <- sub("LeaseNum", unique(New_Lease_VS_Table_output()$ttl_unique_lse),jsCode2)
        
        report_new <- New_Lease_VS_Table_output()
        
        report_new$ttl_unique_lse <- NULL
        
        DT::datatable(data = report_new,
                      container = htmltools::withTags(
                        table(tableHeader(colnames(report_new)),
                              tableFooter(sapply(report_new, function(x) 
                                if(is.numeric(x)) currency(sum(x, na.rm = TRUE), digits = 0L)))
                        )),
                      
                      rownames = FALSE,
                      options = list(paging = TRUE,
                                     searching = TRUE,
                                     #fixedColumns = FALSE,
                                     autoWidth = TRUE,
                                     ordering = TRUE,
                                     dom = 'Bfrtip',
                                     #deferRender = TRUE,
                                     scrollY = 400,
                                     scrollX = TRUE,
                                     scroller = TRUE,
                                     #buttons = c( 'csv'),
                                     pageLength = 80 ,  #nrow(unichange_summary),
                                     footerCallback = JS(jsCode2)
                                     
                      )
                      
        )
      }
        
         
})
  
    # Download Button for new leases
  output$download_data_new <- downloadHandler(

    filename = function(){
      paste0("New Lease Report", "_", now(), ".", "xlsx")
    },

    if(!isTruthy(New_Lease_VS_Table_output()) == FALSE){
      content = function(file) {
        #sep <- switch(input$filetype_new, "csv" = ",", "tsv" = "\t")

        # Write to a file specified by the 'file' argument
        writexl::write_xlsx( New_Lease_VS_Table_output(),
                     file #, #sep = ',',
                     #row.names = FALSE)
                           )
      }

    } else{return(NULL)}

  )
#####################################################################################################
################################################################################################################## 
####################################################################################################
#3) multi changes multi leases 
multi_Summary_Lease_VS_Table <- reactive({    
  
    if(!isTruthy(all_after.mulchange()) == FALSE){
      
       mullseadd_lse <- all_after.mulchange()
      
       mullseadd_lse$Fiscal_Year <- as.character(mullseadd_lse$Fiscal_Year)
       mullseadd_lse$Fiscal_Period <- as.character(mullseadd_lse$Fiscal_Period)
       mullseadd_lse$Fiscal_Quarter <- as.character(mullseadd_lse$Fiscal_Quarter)
       
       
       if(input$multiselectbreakdownby == 'Lease ID'){
         
         
         if(isTruthy(input$multireporttype) == TRUE & input$multireporttype == 'Period to Date'){
           
           mullseadd_lse <- mullseadd_lse %>% filter(reporttype == input$multireporttype) %>% 
                            select(-ends_with("QTD"), -ends_with("YTD"), - starts_with("QTD"), -starts_with("YTD"), -Fiscal_Quarter ) %>%
                            mutate(Financial_Liability_Ending_Detail = - Financial_Liability_Ending_Detail
                                   ,PTD_Rent_Payments = -(PTD_Rent_Payments)
                                   ) %>% 
                            rename( ROU_Closing = Right_of_Use_Ending,
                                    Liability_Closing = Financial_Liability_Ending_Detail,
                                    ARO_Closing = ARO_Ending,
                                    TI_Closing = Tenant_Inducement_Ending,
                                     Retained_Earning_Opening_PTD = retained_earnings_PTD,
                                     ROU_Depreciation_PTD = PTD_Depn,
                                     Liability_Interest_PTD = PTD_Interest,
                                     ARO_Interest_PTD = PTD_ARO_Interest,
                                     TI_Amortization_PTD = PTD_TI_Amort,
                                     Rental_Payments_PTD = PTD_Rent_Payments
                                  ) #%>% mutate_if(is.numeric, round, digits = 0L) %>% 
                             #mutate_if(is.numeric, formatC, digits = 0L, format="d", big.mark = ",")
           
           if(isTruthy(input$multireportcutoff)){
           
           
           
                 if(input$multireportcutoff == 'Delta'){
                   
                   mullseadd_lse <- mullseadd_lse %>% filter(type == input$multireportcutoff)
                   
                   mullseadd_lse <- mullseadd_lse[rowSums(abs(mullseadd_lse[sapply(mullseadd_lse,is.numeric)]), na.rm = T)>0,,drop=F]
                   
                 }else{
                   
                   mullseadd_lse <- mullseadd_lse %>% filter(type == input$multireportcutoff)
                   
                 }
           
           }
           
           if(isTruthy(input$multiselectyear)){
          
           mullseadd_lse <- mullseadd_lse %>% filter(Fiscal_Year %in% input$multiselectyear) 
             
           }
           if(isTruthy(input$multiselectperiod)){
             
             mullseadd_lse <- mullseadd_lse %>% filter(Fiscal_Period  %in% input$multiselectperiod )
           }
           
           return(mullseadd_lse)
           
         }else if(isTruthy(input$multireporttype) & input$multireporttype == 'Quarter to Date'){
           
           mullseadd_lse <- mullseadd_lse %>% filter(reporttype == 'Quarter to Date') %>% 
                    select(-ends_with("PTD"), -ends_with("YTD"), - starts_with("PTD"), -starts_with("YTD"), -Fiscal_Period ) %>% 
                    mutate(Financial_Liability_Ending_Detail = -(Financial_Liability_Ending_Detail)
                           ,QTD_Rent_Payments = -(QTD_Rent_Payments)) %>% 
                     rename( ROU_Closing = Right_of_Use_Ending,
                            Liability_Closing = Financial_Liability_Ending_Detail,
                            ARO_Closing = ARO_Ending,
                            TI_Closing = Tenant_Inducement_Ending,
                             Retained_Earning_Opening_QTD = retained_earnings_QTD,
                             ROU_Depreciation_QTD = QTD_Depn,
                             Liability_Interest_QTD = QTD_Interest,
                             ARO_Interest_QTD = QTD_ARO_Interest,
                             TI_Amortization_QTD = QTD_TI_Amort,
                             Rental_Payments_QTD = QTD_Rent_Payments
                            ) #%>% mutate_if(is.numeric, round, digits = 0L) %>% 
                             #mutate_if(is.numeric, formatC, digits = 0L, format="d", big.mark = ",")
           
           if(isTruthy(input$multireportcutoff)){
      
                 if(input$multireportcutoff == 'Delta'){
                   
                   mullseadd_lse <- mullseadd_lse %>% filter(type == input$multireportcutoff)
                   
                   mullseadd_lse <- mullseadd_lse[rowSums(abs(mullseadd_lse[sapply(mullseadd_lse,is.numeric)]), na.rm = T)>0,,drop=F]
                   
                 }else{
                   
                   mullseadd_lse <- mullseadd_lse %>% filter(type == input$multireportcutoff)
                   
                 }
           
           }
           
           if(isTruthy(input$multiselectyear)){
          
           mullseadd_lse <- mullseadd_lse %>% filter(Fiscal_Year %in% input$multiselectyear) 
             
           }
           if(isTruthy(input$multiselectperiod)){
             
             mullseadd_lse <- mullseadd_lse %>% filter(Fiscal_Period  %in% input$multiselectperiod )
           }
           
           return(mullseadd_lse)
           
         }else {
           
           mullseadd_lse <- mullseadd_lse %>% filter(reporttype == 'Year to Date') %>% 
                    select(-ends_with("PTD"), -ends_with("QTD"), - starts_with("PTD"), -starts_with("QTD"), 
                           -Fiscal_Period, -Fiscal_Quarter) %>% 
                   mutate(Financial_Liability_Ending_Detail = -(Financial_Liability_Ending_Detail)
                          ,YTD_Rent_Payments = -(YTD_Rent_Payments)
                          ) %>% 
                   rename( ROU_Closing = Right_of_Use_Ending,
                            Liability_Closing = Financial_Liability_Ending_Detail,
                            ARO_Closing = ARO_Ending,
                            TI_Closing = Tenant_Inducement_Ending,
                             Retained_Earning_Opening_YTD = retained_earnings_YTD,
                             ROU_Depreciation_YTD = YTD_Depn,
                             Liability_Interest_YTD = YTD_Interest,
                             ARO_Interest_YTD = YTD_ARO_Interest,
                             TI_Amortization_YTD = YTD_TI_Amort,
                             Rental_Payments_YTD = YTD_Rent_Payments
                          ) #%>% mutate_if(is.numeric, round, digits = 0L) %>% 
                            # mutate_if(is.numeric, formatC, digits = 0L, format="d", big.mark = ",")
           
           if(isTruthy(input$multireportcutoff)){
           
        
               if(input$multireportcutoff == 'Delta'){
                 
                 mullseadd_lse <- mullseadd_lse %>% filter(type == input$multireportcutoff)
                 
                 mullseadd_lse <- mullseadd_lse[rowSums(abs(mullseadd_lse[sapply(mullseadd_lse,is.numeric)]), na.rm = T)>0,,drop=F]
                 
               }else{
                 
                 mullseadd_lse <- mullseadd_lse %>% filter(type == input$multireportcutoff)
                 
               }
           
           }
           
           if(isTruthy(input$multiselectyear)){
          
           mullseadd_lse <- mullseadd_lse %>% filter(Fiscal_Year %in% input$multiselectyear) 
             
           }
           if(isTruthy(input$multiselectperiod)){
             
             mullseadd_lse <- mullseadd_lse %>% filter(Fiscal_Period  %in% input$multiselectperiod )
           
                }
         
              }
          return(mullseadd_lse)
         
         }else{
         
         
         if(isTruthy(input$multireporttype) == TRUE & input$multireporttype == 'Period to Date'){
           
           uniq_lse_mullse <- length(unique(subset(all_after.mulchange(), all_after.mulchange()$reporttype == input$multireporttype)$Lease_ID)) 
         
           mullseadd_lse <- mullseadd_lse %>% filter(reporttype == 'Period to Date') %>% 
                      select(type , Lease_ID, Legal_Entity, Fiscal_Year, Fiscal_Period, Fiscal_Quarter,Right_of_Use_Ending,  
                             Financial_Liability_Ending_Detail,
                              ARO_Ending, Tenant_Inducement_Ending,retained_earnings_PTD ,
                               PTD_Depn, PTD_Interest  , PTD_ARO_Interest  ,PTD_TI_Amort  ,PTD_Rent_Payments
                              ) %>% group_by(type, Legal_Entity, Fiscal_Year, Fiscal_Period) %>%
                               summarise( Lease_ID_Count = n_distinct(Lease_ID),
                                 ROU_Closing = sum(Right_of_Use_Ending, na.rm = T),
                                 Liability_Closing = -(sum(Financial_Liability_Ending_Detail, na.rm = TRUE)),
                                 ARO_Closing = sum(ARO_Ending, na.rm = T),
                                 TI_Closing = sum(Tenant_Inducement_Ending, na.rm = T),
                                 Retained_Earning_Opening_PTD = sum(retained_earnings_PTD, na.rm = T),
                                 ROU_Depreciation_PTD = sum(PTD_Depn, na.rm = T),
                                 Liability_Interest_PTD = sum(PTD_Interest, na.rm = T),
                                 ARO_Interest_PTD = sum(PTD_ARO_Interest, na.rm = T),
                                 TI_Amortization_PTD = sum(PTD_TI_Amort, na.rm = T),
                                 Rental_Payments_PTD = (sum(PTD_Rent_Payments, na.rm = T))
                               )
           
           mullseadd_lse <- mullseadd_lse %>% dplyr::arrange(type, Legal_Entity, Fiscal_Year, as.numeric(Fiscal_Period))
            
           mullseadd_lse$Lease_ID_Count <- as.character(mullseadd_lse$Lease_ID_Count)
           
           mullseadd_lse <- add_column(mullseadd_lse, ttl_unique_lse = rep(uniq_lse_mullse, nrow(mullseadd_lse)), .after = 4)
           
           mullseadd_lse$ttl_unique_lse <- as.character(mullseadd_lse$ttl_unique_lse)
           
           if(isTruthy(input$multireportcutoff)){
           
                  uniq_lse_mullse <- length(unique(subset(all_after.mulchange(), all_after.mulchange()$reporttype == input$multireporttype & 
                                                           all_after.mulchange()$type == input$multireportcutoff)$Lease_ID))
                 
                  
                  if(input$multireportcutoff == 'Delta'){
                    
                    
                    mullseadd_lse <- mullseadd_lse %>% filter(type == input$multireportcutoff)
                    
                    mullseadd_lse <- mullseadd_lse[rowSums(abs(mullseadd_lse[sapply(mullseadd_lse,is.numeric)]), na.rm = T)>0,,drop=F]
                    
                    mullseadd_lse$ttl_unique_lse <- rep(uniq_lse_mullse, nrow(mullseadd_lse))
                    
                  }else{
                    
                    mullseadd_lse <- mullseadd_lse %>% filter(type == input$multireportcutoff)
                    
                    mullseadd_lse$ttl_unique_lse <- rep(uniq_lse_mullse, nrow(mullseadd_lse))
                    
                  }
              
              
           
           }
           
           if(isTruthy(input$multiselectyear)){
             
             uniq_lse_mullse <- length(unique(subset(all_after.mulchange(), all_after.mulchange()$reporttype == input$multireporttype & 
                                                       all_after.mulchange()$type == input$multireportcutoff &
                                                       all_after.mulchange()$Fiscal_Year %in% input$multiselectyear)$Lease_ID))
          
              mullseadd_lse <- mullseadd_lse %>% filter(Fiscal_Year %in% input$multiselectyear) 
              
              mullseadd_lse$ttl_unique_lse <- rep(uniq_lse_mullse, nrow(mullseadd_lse))
             
           }
           if(isTruthy(input$multiselectperiod)){
             
               if(!isTruthy(input$multiselectyear)){
                 
                 uniq_lse_mullse <- length(unique(subset(all_after.mulchange(), all_after.mulchange()$reporttype == input$multireporttype & 
                                                           all_after.mulchange()$type == input$multireportcutoff &
                                                           all_after.mulchange()$Fiscal_Period %in% input$newselectperiod)$Lease_ID))
               }else{
                 
                 uniq_lse_mullse <- length(unique(subset(all_after.mulchange(), all_after.mulchange()$reporttype == input$multireporttype & 
                                                           all_after.mulchange()$type == input$multireportcutoff &
                                                           all_after.mulchange()$Fiscal_Year %in% input$multiselectyear &
                                                           all_after.mulchange()$Fiscal_Period %in% input$multiselectperiod)$Lease_ID))
                 
               }
               
             mullseadd_lse <- mullseadd_lse %>% filter(Fiscal_Period  %in% input$multiselectperiod )
             
             mullseadd_lse$ttl_unique_lse <- rep(uniq_lse_mullse, nrow(mullseadd_lse))
         
           }
          
          return(mullseadd_lse)
 
         }else if(isTruthy(input$multireporttype) == TRUE & input$multireporttype == 'Quarter to Date'){
           
                 uniq_lse_mullse <- length(unique(subset(all_after.mulchange(), all_after.mulchange()$reporttype == input$multireporttype)$Lease_ID)) 
            
                 mullseadd_lse <- mullseadd_lse %>% filter(reporttype == 'Quarter to Date') %>% 
                          select(type , Lease_ID, Legal_Entity, Fiscal_Year, Fiscal_Period, Fiscal_Quarter, Right_of_Use_Ending,  
                                 Financial_Liability_Ending_Detail,
                                 ARO_Ending, Tenant_Inducement_Ending,retained_earnings_QTD ,
                                 QTD_Depn, QTD_Interest  , QTD_ARO_Interest  ,QTD_TI_Amort  ,QTD_Rent_Payments
                          ) %>% group_by(type, Legal_Entity, Fiscal_Year, Fiscal_Quarter) %>%
                          summarise( Lease_ID_Count = n_distinct(Lease_ID),
                                     ROU_Closing = sum(Right_of_Use_Ending, na.rm = T),
                                     Liability_Closing = -(sum(Financial_Liability_Ending_Detail, na.rm = TRUE)),
                                     ARO_Closing = sum(ARO_Ending, na.rm = T),
                                     TI_Closing = sum(Tenant_Inducement_Ending, na.rm = T),
                                     Retained_Earning_Opening_QTD = sum(retained_earnings_QTD, na.rm = T),
                                     ROU_Depreciation_QTD = sum(QTD_Depn, na.rm = T),
                                     Liability_Interest_QTD = sum(QTD_Interest, na.rm = T),
                                     ARO_Interest_QTD = sum(QTD_ARO_Interest, na.rm = T),
                                     TI_Amortization_QTD = sum(QTD_TI_Amort, na.rm = T),
                                     Rental_Payments_QTD = (sum(QTD_Rent_Payments, na.rm = T))
                                    )  %>% arrange(type, Legal_Entity, Fiscal_Year, Fiscal_Quarter) #%>% mutate_if(is.numeric, round, digits = 0L) %>% 
                                   #mutate_if(is.numeric, formatC, digits = 0L, format="d", big.mark = ",")
                 mullseadd_lse$Lease_ID_Count <- as.character(mullseadd_lse$Lease_ID_Count)
                 
                 mullseadd_lse <- add_column(mullseadd_lse, ttl_unique_lse = rep(uniq_lse_mullse, nrow(mullseadd_lse)), .after = 4)
           
                 mullseadd_lse$ttl_unique_lse <- as.character(mullseadd_lse$ttl_unique_lse)
                 
           if(isTruthy(input$multireportcutoff)){
                   
                         uniq_lse_mullse <- length(unique(subset(all_after.mulchange(), all_after.mulchange()$reporttype == input$multireporttype & 
                                                                   all_after.mulchange()$type == input$multireportcutoff)$Lease_ID))
                         
                         
                         if(input$multireportcutoff == 'Delta'){
                           
                           
                           mullseadd_lse <- mullseadd_lse %>% filter(type == input$multireportcutoff)
                           
                           mullseadd_lse <- mullseadd_lse[rowSums(abs(mullseadd_lse[sapply(mullseadd_lse,is.numeric)]), na.rm = T)>0,,drop=F]
                           
                           mullseadd_lse$ttl_unique_lse <- rep(uniq_lse_mullse, nrow(mullseadd_lse))
                           
                         }else{
                           
                           mullseadd_lse <- mullseadd_lse %>% filter(type == input$multireportcutoff)
                           
                           mullseadd_lse$ttl_unique_lse <- rep(uniq_lse_mullse, nrow(mullseadd_lse))
                           
                         }
                   
                   
                   
                 }
           
           if(isTruthy(input$multiselectyear)){
          
                 uniq_lse_mullse <- length(unique(subset(all_after.mulchange(), all_after.mulchange()$reporttype == input$multireporttype & 
                                                           all_after.mulchange()$type == input$multireportcutoff &
                                                           all_after.mulchange()$Fiscal_Year %in% input$multiselectyear)$Lease_ID))
                 
                 mullseadd_lse <- mullseadd_lse %>% filter(Fiscal_Year %in% input$multiselectyear) 
                 
                 mullseadd_lse$ttl_unique_lse <- rep(uniq_lse_mullse, nrow(mullseadd_lse))
                 
           }
           if(isTruthy(input$multiselectperiod)){
             
                 if(!isTruthy(input$multiselectyear)){
                   
                   uniq_lse_mullse <- length(unique(subset(all_after.mulchange(), all_after.mulchange()$reporttype == input$multireporttype & 
                                                             all_after.mulchange()$type == input$multireportcutoff &
                                                             all_after.mulchange()$Fiscal_Period %in% input$newselectperiod)$Lease_ID))
                 }else{
                   
                   uniq_lse_mullse <- length(unique(subset(all_after.mulchange(), all_after.mulchange()$reporttype == input$multireporttype & 
                                                             all_after.mulchange()$type == input$multireportcutoff &
                                                             all_after.mulchange()$Fiscal_Year %in% input$multiselectyear &
                                                             all_after.mulchange()$Fiscal_Period %in% input$multiselectperiod)$Lease_ID))
                   
                 }
                 
                 mullseadd_lse <- mullseadd_lse %>% filter(Fiscal_Period  %in% input$multiselectperiod )
                 
                 mullseadd_lse$ttl_unique_lse <- rep(uniq_lse_mullse, nrow(mullseadd_lse))
         
           }
           return(mullseadd_lse)
           
          }else{
            
         uniq_lse_mullse <- length(unique(subset(all_after.mulchange(), all_after.mulchange()$reporttype == input$multireporttype)$Lease_ID)) 
         
         mullseadd_lse <- mullseadd_lse %>% filter(reporttype == 'Year to Date') %>% 
           select(type , Lease_ID, Legal_Entity, Fiscal_Year, Fiscal_Period, Fiscal_Quarter, Right_of_Use_Ending, 
                  Financial_Liability_Ending_Detail,
                   ARO_Ending, Tenant_Inducement_Ending,retained_earnings_YTD ,
                   YTD_Depn, YTD_Interest  , YTD_ARO_Interest  ,YTD_TI_Amort  ,YTD_Rent_Payments
                      ) %>% group_by(type, Legal_Entity, Fiscal_Year, Fiscal_Period) %>%
                      summarise( Lease_ID_Count = n_distinct(Lease_ID),
                                 ROU_Closing = sum(Right_of_Use_Ending, na.rm = T),
                                 Liability_Closing = -(sum(Financial_Liability_Ending_Detail, na.rm = TRUE)),
                                 ARO_Closing = sum(ARO_Ending, na.rm = T),
                                 TI_Closing = sum(Tenant_Inducement_Ending, na.rm = T),
                                 Retained_Earning_Opening_YTD = sum(retained_earnings_YTD, na.rm = T),
                                 ROU_Depreciation_YTD = sum(YTD_Depn, na.rm = T),
                                 Liability_Interest_YTD = sum(YTD_Interest, na.rm = T),
                                 ARO_Interest_YTD = sum(YTD_ARO_Interest, na.rm = T),
                                 TI_Amortization_YTD = sum(YTD_TI_Amort, na.rm = T),
                                 Rental_Payments_YTD = (sum(YTD_Rent_Payments, na.rm = T))
                      ) %>% arrange(type, Legal_Entity, Fiscal_Year, as.numeric(Fiscal_Period)) 
         
         mullseadd_lse$Lease_ID_Count <- as.character(mullseadd_lse$Lease_ID_Count)
         
         mullseadd_lse <- add_column(mullseadd_lse, ttl_unique_lse = rep(uniq_lse_mullse, nrow(mullseadd_lse)), .after = 4)
         
         mullseadd_lse$ttl_unique_lse <- as.character(mullseadd_lse$ttl_unique_lse)
                            
         if(isTruthy(input$multireportcutoff)){
           
                 uniq_lse_mullse <- length(unique(subset(all_after.mulchange(), all_after.mulchange()$reporttype == input$multireporttype & 
                                                           all_after.mulchange()$type == input$multireportcutoff)$Lease_ID))
                 
                 
                 if(input$multireportcutoff == 'Delta'){
                   
                   
                   mullseadd_lse <- mullseadd_lse %>% filter(type == input$multireportcutoff)
                   
                   mullseadd_lse <- mullseadd_lse[rowSums(abs(mullseadd_lse[sapply(mullseadd_lse,is.numeric)]), na.rm = T)>0,,drop=F]
                   
                   mullseadd_lse$ttl_unique_lse <- rep(uniq_lse_mullse, nrow(mullseadd_lse))
                   
                 }else{
                   
                   mullseadd_lse <- mullseadd_lse %>% filter(type == input$multireportcutoff)
                   
                   mullseadd_lse$ttl_unique_lse <- rep(uniq_lse_mullse, nrow(mullseadd_lse))
                   
                 }
                 
           
           
         }
           
           if(isTruthy(input$multiselectyear)){
          
           mullseadd_lse <- mullseadd_lse %>% filter(Fiscal_Year %in% input$multiselectyear) 
             
           }
           if(isTruthy(input$multiselectperiod)){
             
             mullseadd_lse <- mullseadd_lse %>% filter(Fiscal_Period  %in% input$multiselectperiod )
         
           }
       }
    return(mullseadd_lse)
 
 }
    
  return(mullseadd_lse)  
      
} else{ return(NULL)}

  }
)


output$multi_Summary_Lease_VS_Table <- DT::renderDataTable( 

       { 
         
         # for breakdown by Legal ID 
         
   if(is.null(multi_Summary_Lease_VS_Table()) == FALSE & !("Lease_ID_Count" %in% colnames(multi_Summary_Lease_VS_Table()))){   
         
         DT::datatable(data = multi_Summary_Lease_VS_Table(),
                       container = htmltools::withTags(
                         table(tableHeader(colnames(multi_Summary_Lease_VS_Table())),
                               tableFooter(sapply(multi_Summary_Lease_VS_Table(), function(x) if(is.numeric(x)) currency(sum(x, na.rm = TRUE), digits = 0L)))
                         )),
                    rownames = FALSE,
                  
                    options = list(paging = TRUE,
                                   searching = TRUE,
                                   #fixedColumns = FALSE,
                                   autoWidth = TRUE,
                                   ordering = TRUE,
                                   dom = 'Bfrtip',
                                   #deferRender = TRUE,
                                   scrollY = 400,
                                   scrollX = TRUE,
                                   scroller = TRUE,
                                   #buttons = c( 'csv'),
                                   pageLength = 80,
                                   footerCallback = JS(jsCode)
                                   )
                   )
   }else if(is.null(multi_Summary_Lease_VS_Table()) == FALSE & ("Lease_ID_Count" %in% colnames(multi_Summary_Lease_VS_Table()))) {
     
     
     jsCode2 <- sub("LeaseNum", unique(multi_Summary_Lease_VS_Table()$ttl_unique_lse),jsCode2)
     
     report_mul <- multi_Summary_Lease_VS_Table()
     
     report_mul$ttl_unique_lse <- NULL
     
     DT::datatable(data = report_mul,
                   container = htmltools::withTags(
                     table(tableHeader(colnames(report_mul)),
                           tableFooter(sapply(report_mul, function(x) 
                             if(is.numeric(x)) currency(sum(x, na.rm = TRUE), digits = 0L)))
                     )),
                   
                   rownames = FALSE,
                   options = list(paging = TRUE,
                                  searching = TRUE,
                                  #fixedColumns = FALSE,
                                  autoWidth = TRUE,
                                  ordering = TRUE,
                                  dom = 'Bfrtip',
                                  #deferRender = TRUE,
                                  scrollY = 400,
                                  scrollX = TRUE,
                                  scroller = TRUE,
                                  #buttons = c( 'csv'),
                                  pageLength = 80 ,  #nrow(unichange_summary),
                                  footerCallback = JS(jsCode2)
                                  
                   )
                   
     )
   }
})
  
    # Download Button for new leases
output$download_mulChange_mulLse <- downloadHandler(

    filename = function(){
      paste0("Multi Change Multi Lease Report", "_", now(), ".", "xlsx")
    },

    if(!isTruthy(multi_Summary_Lease_VS_Table()) == FALSE){
      content = function(file) {
        

        # Write to a file specified by the 'file' argument
        writexl::write_xlsx( multi_Summary_Lease_VS_Table(),
                     file 
                           )
      }

    } else{return(NULL)}

  )  

################################################################################################################## 
######################################################################################################################  

###########################  Overall summary results tab ################################################################

################################################################################################################## 
# Combine all after together 

# create reactiveValues to store data created from reactive in the previous steps
data_created <-  reactiveValues()

observe({ if(is.null(new.lease.table.update2()) == FALSE){

                data_created$dat1 <- new.lease.table.update2()

}

})

observe({ if(is.null(multi_change_lease2()) == FALSE){

  data_created$dat2 <- multi_change_lease2()

}

})

observe({ if(is.null(uni_change_lease2()) == FALSE){

  data_created$dat3 <- uni_change_lease2()

}

})
###############################################################################################
# 1) 3 tables all exist

all_after_allchange <- reactive({  
  
 
  print(paste("nrow(data_created$dat1)", nrow(data_created$dat1)))
  
  print(paste("nrow(data_created$dat2)", nrow(data_created$dat2)))
  
  print(paste("nrow(data_created$dat3)", nrow(data_created$dat3)))
  
  # validate(need(any(unique(data_created$dat2$Lease_ID) %in% unique(data_created$dat3$Lease_ID)) == F &     
  #                 any(unique(data_created$dat1$Lease_ID) %in% unique(data_created$dat2$Lease_ID)) == F &
  #                 any(unique(data_created$dat1$Lease_ID) %in% unique(data_created$dat3$Lease_ID)) == F
  #               ,"Please ensure no lease ID(s) double selected!" ))
  
  observeEvent(  any(unique(data_created$dat2$Lease_ID) %in% unique(data_created$dat3$Lease_ID)) == T |     
                 any(unique(data_created$dat1$Lease_ID) %in% unique(data_created$dat2$Lease_ID)) == T |
                 any(unique(data_created$dat1$Lease_ID) %in% unique(data_created$dat3$Lease_ID)) == T,{
                   
                   showModal(modalDialog(
                     title = "Error",
                     "Please ensure no lease ID(s) double selected!",
                     easyClose = TRUE
                   ))  
                   
                   
                   
                 }
               
               
               
               )
  
  
  if(any(unique(data_created$dat2$Lease_ID) %in% unique(data_created$dat3$Lease_ID)) == F &     
     any(unique(data_created$dat1$Lease_ID) %in% unique(data_created$dat2$Lease_ID)) == F &
     any(unique(data_created$dat1$Lease_ID) %in% unique(data_created$dat3$Lease_ID)) == F){
  
          all_change_df <- dplyr::bind_rows(data_created$dat1, data_created$dat2, data_created$dat3)
          
        
        
          #remove any leases got changed; then append all change df
          DT_lease_before_update2 <-  subset(DT_lease_before_update, !(DT_lease_before_update$Lease_ID %in% unique(all_change_df$Lease_ID)))
        
          DT_lease_afterallchange <- dplyr::bind_rows(DT_lease_before_update2,all_change_df)
        
          DT_lease_afterallchange <- DT_lease_afterallchange %>% mutate(type = 'After')
          
          
          
          return(DT_lease_afterallchange)
  } else{return(NULL)}
   

})


################################################################################################################## 
###################################################################################################
all_after.allchange <- reactive({  
  
   req(all_after_allchange())
  
  # if(isTruthy(all_after_allchange_3())){
  #   
  #   afterallchange <- all_after_allchange_3()
  #   
  # }else{
  #   
  #   afterallchange <- all_after_allchange_21()
  #   
  # }
  # 
  # select columns needed for summary table 
  
  afterallchange <- all_after_allchange()
  
  lease_after_summary.allchange <- afterallchange %>% select(Lease_ID, PRPTY_NUM, Legal_Entity, Fiscal_Year, Fiscal_Period,
                                                             Fiscal_Quarter, type,
                                                             Right_of_Use_Ending, Financial_Liability_Ending_Detail,
                                                             ARO_Ending , Tenant_Inducement_Ending ,retained_earnings_YTD ,
                                                             retained_earnings_QTD, retained_earnings_PTD , YTD_Depn,
                                                             YTD_Interest, YTD_ARO_Interest, YTD_TI_Amort, YTD_Rent_Payments,
                                                             QTD_Depn,
                                                             QTD_Interest, QTD_ARO_Interest, QTD_TI_Amort, QTD_Rent_Payments,
                                                             PTD_Depn,
                                                             PTD_Interest, PTD_ARO_Interest, PTD_TI_Amort, PTD_Rent_Payments,
  )
  
  mergetabe.allchange <- lease_after_summary.allchange %>% full_join(lease_before_summary,
                                                                     by = c( 'Lease_ID' = 'Lease_ID',
                                                                             "PRPTY_NUM" = "PRPTY_NUM",
                                                                             "Legal_Entity" = "Legal_Entity",
                                                                             "Fiscal_Year" = "Fiscal_Year",
                                                                             "Fiscal_Period" = "Fiscal_Period",
                                                                             'Fiscal_Quarter' = 'Fiscal_Quarter'
                                                                     )) %>% mutate_if(is.numeric, coalesce, 0) 
  
  mergetabe.allchange <- mergetabe.allchange[, !(names(mergetabe.allchange) %in% c('type.x', 'type.y'))]
  
  diff_allchange <- mergetabe.allchange[ , grepl("*\\.x$",names(mergetabe.allchange))] - mergetabe.allchange[,grepl("*\\.y$",names(mergetabe.allchange))]
  
  diff_allchange_update <- cbind(mergetabe.allchange[,1:6, drop = FALSE], diff_allchange)
  
  names(diff_allchange_update) <- fix_names(names(diff_allchange_update))
  
  #####################
  # combine before, after, delta together 
  
  diff_allchange_update <-  diff_allchange_update %>% mutate(type = 'Delta') 
  
  bf_after_delta_allchange <- dplyr::bind_rows(lease_before_summary, lease_after_summary.allchange, diff_allchange_update)
  
  #################################################################################################
  PTDdf.allchange <- bf_after_delta_allchange %>% select(Lease_ID, PRPTY_NUM , Legal_Entity, Fiscal_Year, Fiscal_Period, Fiscal_Quarter,
                                                         type,
                                                         Right_of_Use_Ending, Financial_Liability_Ending_Detail,
                                                         ARO_Ending, Tenant_Inducement_Ending, retained_earnings_PTD,
                                                         PTD_Depn, PTD_Interest, PTD_ARO_Interest, PTD_TI_Amort, PTD_Rent_Payments ) %>% 
    mutate(reporttype = 'Period to Date')
  
  
  QTDdf.allchange  <- bf_after_delta_allchange %>% select(Lease_ID, PRPTY_NUM , Legal_Entity, Fiscal_Year, Fiscal_Period, Fiscal_Quarter,
                                                          type,
                                                          Right_of_Use_Ending, Financial_Liability_Ending_Detail,
                                                          ARO_Ending, Tenant_Inducement_Ending, retained_earnings_QTD,
                                                          QTD_Depn, QTD_Interest, QTD_ARO_Interest, QTD_TI_Amort, QTD_Rent_Payments ) %>% 
    mutate(reporttype = 'Quarter to Date')
  
  YTDdf.allchange  <- bf_after_delta_allchange %>% select(Lease_ID, PRPTY_NUM , Legal_Entity, Fiscal_Year, Fiscal_Period, Fiscal_Quarter,
                                                          type,
                                                          Right_of_Use_Ending, Financial_Liability_Ending_Detail,
                                                          ARO_Ending, Tenant_Inducement_Ending, retained_earnings_YTD,
                                                          YTD_Depn, YTD_Interest, YTD_ARO_Interest, YTD_TI_Amort, YTD_Rent_Payments ) %>% 
    mutate(reporttype = 'Year to Date')
  
  
  bf_after_delta_allchange_made <- dplyr::bind_rows(PTDdf.allchange , QTDdf.allchange , YTDdf.allchange )
  
  return(bf_after_delta_allchange_made) 
  
})
###################################################################################################################
# 4) overall change Summary output tab

Overall_Lease_VS_Table_df <- reactive({    
  
    print(paste("isTruthy(all_after.allchange())",isTruthy(all_after.allchange())))
  
    if(isTruthy(all_after.allchange()) & nrow(all_after.allchange()) >0 ){

       alllseadd_lse <- all_after.allchange()
       
       alllseadd_lse$Fiscal_Year <- as.character(alllseadd_lse$Fiscal_Year)
       alllseadd_lse$Fiscal_Period <- as.character(alllseadd_lse$Fiscal_Period)
       alllseadd_lse$Fiscal_Quarter <- as.character(alllseadd_lse$Fiscal_Quarter)

    }else{ return(NULL)}

    # if(isTruthy(all_after_newleaseadd())){
    #      
    #   alllseadd_lse <- all_after_newleaseadd()
    #   
    # }else if(isTruthy( all_after.mulchange())){
    #      
    #      
    #   alllseadd_lse <-  all_after.mulchange()
    #   
    # }else if (isTruthy(all_after.unichange())){
    #      
    #   alllseadd_lse <- all_after.unichange()
    #   
    #    }else{ return(NULL)}
      
    
  if(isTruthy(alllseadd_lse) & nrow(alllseadd_lse) >0){
       
       if(input$ttlselectbreakdownby == 'Lease ID'){
         
         
         if(isTruthy(input$ttlreporttype) == TRUE & input$ttlreporttype == 'Period to Date'){
           
           alllseadd_lse <- alllseadd_lse %>% filter(reporttype == input$ttlreporttype) %>% 
                            select(-ends_with("QTD"), -ends_with("YTD"), - starts_with("QTD"), -starts_with("YTD"), -Fiscal_Quarter ) %>%
                            mutate(Financial_Liability_Ending_Detail = -abs(Financial_Liability_Ending_Detail)
                                   ,PTD_Rent_Payments = -abs(PTD_Rent_Payments)
                                   ) %>% 
                            rename(  ROU_Closing = Right_of_Use_Ending,
                                     Liability_Closing = Financial_Liability_Ending_Detail,
                                     ARO_Closing = ARO_Ending,
                                     TI_Closing = Tenant_Inducement_Ending,
                                     Retained_Earning_Opening_PTD = retained_earnings_PTD,
                                     ROU_Depreciation_PTD = PTD_Depn,
                                     Liability_Interest_PTD = PTD_Interest,
                                     ARO_Interest_PTD = PTD_ARO_Interest,
                                     TI_Amortization_PTD = PTD_TI_Amort,
                                     Rental_Payments_PTD = PTD_Rent_Payments
                                  ) #%>% mutate_if(is.numeric, round, digits = 0L) %>% 
                             #mutate_if(is.numeric, formatC, digits = 0L, format="d", big.mark = ",")
           
           if(isTruthy(input$ttlreportcutoff)){
           
                if(input$ttlreportcutoff == 'Delta'){
                   
                   alllseadd_lse <- alllseadd_lse %>% filter(type == input$ttlreportcutoff)
                   
                   alllseadd_lse <- alllseadd_lse[rowSums(abs(alllseadd_lse[sapply(alllseadd_lse,is.numeric)]), na.rm = T)>0,,drop=F]
                   
                 }else{
                   
                   alllseadd_lse <- alllseadd_lse %>% filter(type == input$ttlreportcutoff)
                   
                 }
           
           }
           
           if(isTruthy(input$ttlselectyear)){
          
           alllseadd_lse <- alllseadd_lse %>% filter(Fiscal_Year %in% input$ttlselectyear) 
             
           }
           if(isTruthy(input$ttlselectperiod)){
             
             alllseadd_lse <- alllseadd_lse %>% filter(Fiscal_Period  %in% input$ttlselectperiod )
           }
           
           return(alllseadd_lse)
           
         }else if(isTruthy(input$ttlreporttype) & input$ttlreporttype == 'Quarter to Date'){
           
           alllseadd_lse <- alllseadd_lse %>% filter(reporttype == 'Quarter to Date') %>% 
                    select(-ends_with("PTD"), -ends_with("YTD"), - starts_with("PTD"), -starts_with("YTD"), -Fiscal_Period ) %>% 
                    mutate(Financial_Liability_Ending_Detail = -abs(Financial_Liability_Ending_Detail)
                           ,QTD_Rent_Payments = -abs(QTD_Rent_Payments)
                           ) %>% 
                     rename( ROU_Closing = Right_of_Use_Ending,
                            Liability_Closing = Financial_Liability_Ending_Detail,
                            ARO_Closing = ARO_Ending,
                            TI_Closing = Tenant_Inducement_Ending,
                             Retained_Earning_Opening_QTD = retained_earnings_QTD,
                             ROU_Depreciation_QTD = QTD_Depn,
                             Liability_Interest_QTD = QTD_Interest,
                             ARO_Interest_QTD = QTD_ARO_Interest,
                             TI_Amortization_QTD = QTD_TI_Amort,
                             Rental_Payments_QTD = QTD_Rent_Payments
                            ) #%>% mutate_if(is.numeric, round, digits = 0L) %>% 
                             #mutate_if(is.numeric, formatC, digits = 0L, format="d", big.mark = ",")
           
           if(isTruthy(input$ttlreportcutoff)){
           
                 if(input$ttlreportcutoff == 'Delta'){
                   
                   alllseadd_lse <- alllseadd_lse %>% filter(type == input$ttlreportcutoff)
                   
                   alllseadd_lse <- alllseadd_lse[rowSums(abs(alllseadd_lse[sapply(alllseadd_lse,is.numeric)]), na.rm = T)>0,,drop=F]
                   
                 }else{
                   
                   alllseadd_lse <- alllseadd_lse %>% filter(type == input$ttlreportcutoff)
                   
                 }
           
           }
           
           if(isTruthy(input$ttlselectyear)){
          
           alllseadd_lse <- alllseadd_lse %>% filter(Fiscal_Year %in% input$ttlselectyear) 
             
           }
           if(isTruthy(input$ttlselectperiod)){
             
             alllseadd_lse <- alllseadd_lse %>% filter(Fiscal_Period  %in% input$ttlselectperiod )
           }
           
           return(alllseadd_lse)
           
         }else {
           
           alllseadd_lse <- alllseadd_lse %>% filter(reporttype == 'Year to Date') %>% 
                    select(-ends_with("PTD"), -ends_with("QTD"), - starts_with("PTD"), -starts_with("QTD"), 
                           -Fiscal_Period, -Fiscal_Quarter) %>% 
                   mutate(Financial_Liability_Ending_Detail = -abs(Financial_Liability_Ending_Detail)
                          ,YTD_Rent_Payments = -abs(YTD_Rent_Payments)
                          ) %>% 
                   rename( ROU_Closing = Right_of_Use_Ending,
                            Liability_Closing = Financial_Liability_Ending_Detail,
                            ARO_Closing = ARO_Ending,
                            TI_Closing = Tenant_Inducement_Ending,
                             Retained_Earning_Opening_YTD = retained_earnings_YTD,
                             ROU_Depreciation_YTD = YTD_Depn,
                             Liability_Interest_YTD = YTD_Interest,
                             ARO_Interest_YTD = YTD_ARO_Interest,
                             TI_Amortization_YTD = YTD_TI_Amort,
                             Rental_Payments_YTD = YTD_Rent_Payments
                          ) #%>% mutate_if(is.numeric, round, digits = 0L) %>% 
                            # mutate_if(is.numeric, formatC, digits = 0L, format="d", big.mark = ",")
           
           if(isTruthy(input$ttlreportcutoff)){
           
                 if(input$ttlreportcutoff == 'Delta'){
                   
                   alllseadd_lse <- alllseadd_lse %>% filter(type == input$ttlreportcutoff)
                   
                   alllseadd_lse <- alllseadd_lse[rowSums(abs(alllseadd_lse[sapply(alllseadd_lse,is.numeric)]), na.rm = T)>0,,drop=F]
                   
                 }else{
                   
                   alllseadd_lse <- alllseadd_lse %>% filter(type == input$ttlreportcutoff)
                   
                 }
               
           }
           
           if(isTruthy(input$ttlselectyear)){
          
           alllseadd_lse <- alllseadd_lse %>% filter(Fiscal_Year %in% input$ttlselectyear) 
             
           }
           if(isTruthy(input$ttlselectperiod)){
             
             alllseadd_lse <- alllseadd_lse %>% filter(Fiscal_Period  %in% input$ttlselectperiod )
           
                }
         
              }
          return(alllseadd_lse)
         
         }else{
         
         
         if(isTruthy(input$ttlreporttype) == TRUE & input$ttlreporttype == 'Period to Date'){
           
           uniq_lse_alllse <- length(unique(subset(all_after.allchange(), all_after.allchange()$reporttype == input$ttlreporttype)$Lease_ID)) 
         
          alllseadd_lse <- alllseadd_lse %>% filter(reporttype == 'Period to Date') %>% 
                      select(type , Lease_ID, Legal_Entity, Fiscal_Year, Fiscal_Period, Fiscal_Quarter,Right_of_Use_Ending,  
                             Financial_Liability_Ending_Detail,
                              ARO_Ending, Tenant_Inducement_Ending,retained_earnings_PTD ,
                               PTD_Depn, PTD_Interest  , PTD_ARO_Interest  ,PTD_TI_Amort  ,PTD_Rent_Payments
                              ) %>% group_by(type, Legal_Entity, Fiscal_Year, Fiscal_Period) %>%
                               summarise( Lease_ID_Count = n_distinct(Lease_ID),
                                 ROU_Closing = sum(Right_of_Use_Ending, na.rm = T),
                                 Liability_Closing = -(sum(Financial_Liability_Ending_Detail, na.rm = T)),
                                 ARO_Closing = sum(ARO_Ending, na.rm = T),
                                 TI_Closing = sum(Tenant_Inducement_Ending, na.rm = T),
                                 Retained_Earning_Opening_PTD = sum(retained_earnings_PTD, na.rm = T),
                                 ROU_Depreciation_PTD = sum(PTD_Depn, na.rm = T),
                                 Liability_Interest_PTD = sum(PTD_Interest, na.rm = T),
                                 ARO_Interest_PTD = sum(PTD_ARO_Interest, na.rm = T),
                                 TI_Amortization_PTD = sum(PTD_TI_Amort, na.rm = T),
                                 Rental_Payments_PTD = (sum(PTD_Rent_Payments, na.rm = T))
                               )  %>% arrange(type, Legal_Entity, Fiscal_Year, as.numeric(Fiscal_Period)) #%>% mutate_if(is.numeric, round, digits = 0L) %>% 
                            # mutate_if(is.numeric, formatC, digits = 0L, format="d", big.mark = ",")
          
          alllseadd_lse$Lease_ID_Count <- as.character(alllseadd_lse$Lease_ID_Count)
          
          alllseadd_lse <- add_column(alllseadd_lse, ttl_unique_lse = rep(uniq_lse_alllse, nrow(alllseadd_lse)), .after = 4)
          
          alllseadd_lse$ttl_unique_lse <- as.character(alllseadd_lse$ttl_unique_lse)
                     
           if(isTruthy(input$ttlreportcutoff)){
           
              uniq_lse_alllse <- length(unique(subset(all_after.allchange(), all_after.allchange()$reporttype == input$ttlreporttype & 
                                                       all_after.allchange()$type == input$ttlreportcutoff)$Lease_ID))
      
              
              if(input$ttlreportcutoff == 'Delta'){
                
                
                alllseadd_lse <- alllseadd_lse %>% filter(type == input$ttlreportcutoff)
                
                alllseadd_lse <- alllseadd_lse[rowSums(abs(alllseadd_lse[sapply(alllseadd_lse,is.numeric)]), na.rm = T)>0,,drop=F]
                
                alllseadd_lse$ttl_unique_lse <- rep(uniq_lse_alllse, nrow(alllseadd_lse))
                
              }else{
                
                alllseadd_lse <- alllseadd_lse %>% filter(type == input$ttlreportcutoff)
                
                alllseadd_lse$ttl_unique_lse <- rep(uniq_lse_alllse, nrow(alllseadd_lse))
                
              }
              
              
           
           }
           
           if(isTruthy(input$ttlselectyear)){
             
             uniq_lse_alllse <- length(unique(subset(all_after.allchange(), all_after.allchange()$reporttype == input$ttlreporttype & 
                                                       all_after.allchange()$type == input$ttlreportcutoff &
                                                       all_after.allchange()$Fiscal_Year %in% input$ttlselectyear)$Lease_ID))
          
              alllseadd_lse <- alllseadd_lse %>% filter(Fiscal_Year %in% input$ttlselectyear) 
              
              alllseadd_lse$ttl_unique_lse <- rep(uniq_lse_alllse, nrow(alllseadd_lse))
             
           }
           if(isTruthy(input$ttlselectperiod)){
             
                 if(!isTruthy(input$ttlselectyear)){
                   
                   uniq_lse_alllse <- length(unique(subset(all_after.allchange(), all_after.allchange()$reporttype == input$ttlreporttype & 
                                                             all_after.allchange()$type == input$ttlreportcutoff &
                                                             all_after.allchange()$Fiscal_Period %in% input$ttlselectperiod)$Lease_ID))
                 }else{
                   
                   uniq_lse_alllse <- length(unique(subset(all_after.allchange(), all_after.allchange()$reporttype == input$ttlreporttype & 
                                                             all_after.allchange()$type == input$ttlreportcutoff &
                                                             all_after.allchange()$Fiscal_Year %in% input$ttlselectyear &
                                                             all_after.allchange()$Fiscal_Period %in% input$ttlselectperiod)$Lease_ID))
                   
                 }
                 
                 alllseadd_lse <- alllseadd_lse %>% filter(Fiscal_Period  %in% input$ttlselectperiod )
                 
                 alllseadd_lse$ttl_unique_lse <- rep(uniq_lse_alllse, nrow(alllseadd_lse))
             
               }
          
          return(alllseadd_lse)
 
         }else if(isTruthy(input$ttlreporttype) == TRUE & input$ttlreporttype == 'Quarter to Date'){
           
           uniq_lse_alllse <- length(unique(subset(all_after.allchange(), all_after.allchange()$reporttype == input$ttlreporttype)$Lease_ID)) 
      
           alllseadd_lse <- alllseadd_lse %>% filter(reporttype == 'Quarter to Date') %>% 
                    select(type , Lease_ID, Legal_Entity, Fiscal_Year, Fiscal_Period, Fiscal_Quarter, Right_of_Use_Ending,  
                           Financial_Liability_Ending_Detail,
                           ARO_Ending, Tenant_Inducement_Ending,retained_earnings_QTD ,
                           QTD_Depn, QTD_Interest  , QTD_ARO_Interest  ,QTD_TI_Amort  ,QTD_Rent_Payments
                    ) %>% group_by(type, Legal_Entity, Fiscal_Year, Fiscal_Quarter) %>%
                    summarise( Lease_ID_Count = n_distinct(Lease_ID),
                               ROU_Closing = sum(Right_of_Use_Ending, na.rm = T),
                               Liability_Closing = -(sum(Financial_Liability_Ending_Detail, na.rm = T)),
                               ARO_Closing = sum(ARO_Ending, na.rm = T),
                               TI_Closing = sum(Tenant_Inducement_Ending, na.rm = T),
                               Retained_Earning_Opening_QTD = sum(retained_earnings_QTD, na.rm = T),
                               ROU_Depreciation_QTD = sum(QTD_Depn, na.rm = T),
                               Liability_Interest_QTD = sum(QTD_Interest, na.rm = T),
                               ARO_Interest_QTD = sum(QTD_ARO_Interest, na.rm = T),
                               TI_Amortization_QTD = sum(QTD_TI_Amort, na.rm = T),
                               Rental_Payments_QTD = (sum(QTD_Rent_Payments, na.rm = T))
                              )  %>% arrange(type, Legal_Entity, Fiscal_Year, Fiscal_Quarter) #%>% mutate_if(is.numeric, round, digits = 0L) %>% 
                             #mutate_if(is.numeric, formatC, digits = 0L, format="d", big.mark = ",")
           alllseadd_lse$Lease_ID_Count <- as.character(alllseadd_lse$Lease_ID_Count)
           
           alllseadd_lse <- add_column(alllseadd_lse, ttl_unique_lse = rep(uniq_lse_alllse, nrow(alllseadd_lse)), .after = 4)
           
           alllseadd_lse$ttl_unique_lse <- as.character(alllseadd_lse$ttl_unique_lse)
           
           if(isTruthy(input$ttlreportcutoff)){
             
                 uniq_lse_alllse <- length(unique(subset(all_after.allchange(), all_after.allchange()$reporttype == input$ttlreporttype & 
                                                           all_after.allchange()$type == input$ttlreportcutoff)$Lease_ID))
                 
                 
                 if(input$ttlreportcutoff == 'Delta'){
                   
                   
                   alllseadd_lse <- alllseadd_lse %>% filter(type == input$ttlreportcutoff)
                   
                   alllseadd_lse <- alllseadd_lse[rowSums(abs(alllseadd_lse[sapply(alllseadd_lse,is.numeric)]), na.rm = T)>0,,drop=F]
                   
                   alllseadd_lse$ttl_unique_lse <- rep(uniq_lse_alllse, nrow(alllseadd_lse))
                   
                 }else{
                   
                   alllseadd_lse <- alllseadd_lse %>% filter(type == input$ttlreportcutoff)
                   
                   alllseadd_lse$ttl_unique_lse <- rep(uniq_lse_alllse, nrow(alllseadd_lse))
                   
                 }
             
             
             
           }
           
           
           if(isTruthy(input$ttlselectyear)){
          
             uniq_lse_alllse <- length(unique(subset(all_after.allchange(), all_after.allchange()$reporttype == input$ttlreporttype & 
                                                       all_after.allchange()$type == input$ttlreportcutoff &
                                                       all_after.allchange()$Fiscal_Year %in% input$ttlselectyear)$Lease_ID))
             
             alllseadd_lse <- alllseadd_lse %>% filter(Fiscal_Year %in% input$ttlselectyear) 
             
             alllseadd_lse$ttl_unique_lse <- rep(uniq_lse_alllse, nrow(alllseadd_lse))
             
           }
           if(isTruthy(input$ttlselectperiod)){
             
             if(!isTruthy(input$ttlselectyear)){
               
               uniq_lse_alllse <- length(unique(subset(all_after.allchange(), all_after.allchange()$reporttype == input$ttlreporttype & 
                                                         all_after.allchange()$type == input$ttlreportcutoff &
                                                         all_after.allchange()$Fiscal_Period %in% input$ttlselectperiod)$Lease_ID))
             }else{
               
               uniq_lse_alllse <- length(unique(subset(all_after.allchange(), all_after.allchange()$reporttype == input$ttlreporttype & 
                                                         all_after.allchange()$type == input$ttlreportcutoff &
                                                         all_after.allchange()$Fiscal_Year %in% input$ttlselectyear &
                                                         all_after.allchange()$Fiscal_Period %in% input$ttlselectperiod)$Lease_ID))
               
             }
             
             alllseadd_lse <- alllseadd_lse %>% filter(Fiscal_Period  %in% input$ttlselectperiod )
             
             alllseadd_lse$ttl_unique_lse <- rep(uniq_lse_alllse, nrow(alllseadd_lse))
         
           }
           return(alllseadd_lse)
           
          }else{
            
                  uniq_lse_alllse <- length(unique(subset(all_after.allchange(), all_after.allchange()$reporttype == input$ttlreporttype)$Lease_ID)) 
            
               alllseadd_lse <- alllseadd_lse %>% filter(reporttype == 'Year to Date') %>% 
                 select(type , Lease_ID, Legal_Entity, Fiscal_Year, Fiscal_Period, Fiscal_Quarter, Right_of_Use_Ending, 
                        Financial_Liability_Ending_Detail,
                         ARO_Ending, Tenant_Inducement_Ending,retained_earnings_YTD ,
                         YTD_Depn, YTD_Interest  , YTD_ARO_Interest  ,YTD_TI_Amort  ,YTD_Rent_Payments
                            ) %>% group_by(type, Legal_Entity, Fiscal_Year, Fiscal_Period) %>%
                            summarise( Lease_ID_Count = n_distinct(Lease_ID),
                                       ROU_Closing = sum(Right_of_Use_Ending, na.rm = T),
                                       Liability_Closing = -(sum(Financial_Liability_Ending_Detail, na.rm = T)),
                                       ARO_Closing = sum(ARO_Ending, na.rm = T),
                                       TI_Closing = sum(Tenant_Inducement_Ending, na.rm = T),
                                       Retained_Earning_Opening_YTD = sum(retained_earnings_YTD, na.rm = T),
                                       ROU_Depreciation_YTD = sum(YTD_Depn, na.rm = T),
                                       Liability_Interest_YTD = sum(YTD_Interest, na.rm = T),
                                       ARO_Interest_YTD = sum(YTD_ARO_Interest, na.rm = T),
                                       TI_Amortization_YTD = sum(YTD_TI_Amort, na.rm = T),
                                       Rental_Payments_YTD = (sum(YTD_Rent_Payments, na.rm = T))
                            )  %>% arrange(type, Legal_Entity, Fiscal_Year, as.numeric(Fiscal_Period)) #%>% mutate_if(is.numeric, round, digits = 0L) %>% 
                                  # mutate_if(is.numeric, formatC, digits = 0L, format="d", big.mark = ",")
               
               alllseadd_lse$Lease_ID_Count <- as.character(alllseadd_lse$Lease_ID_Count)
               
               alllseadd_lse <- add_column(alllseadd_lse, ttl_unique_lse = rep(uniq_lse_alllse, nrow(alllseadd_lse)), .after = 4)
               
               alllseadd_lse$ttl_unique_lse <- as.character(alllseadd_lse$ttl_unique_lse)
               
               if(isTruthy(input$ttlreportcutoff)){
                 
                       uniq_lse_alllse <- length(unique(subset(all_after.allchange(), all_after.allchange()$reporttype == input$ttlreporttype & 
                                                                 all_after.allchange()$type == input$ttlreportcutoff)$Lease_ID))
                       
                       
                       if(input$ttlreportcutoff == 'Delta'){
                         
                         
                         alllseadd_lse <- alllseadd_lse %>% filter(type == input$ttlreportcutoff)
                         
                         alllseadd_lse <- alllseadd_lse[rowSums(abs(alllseadd_lse[sapply(alllseadd_lse,is.numeric)]), na.rm = T)>0,,drop=F]
                         
                         alllseadd_lse$ttl_unique_lse <- rep(uniq_lse_alllse, nrow(alllseadd_lse))
                         
                       }else{
                         
                         alllseadd_lse <- alllseadd_lse %>% filter(type == input$ttlreportcutoff)
                         
                         alllseadd_lse$ttl_unique_lse <- rep(uniq_lse_alllse, nrow(alllseadd_lse))
                         
                       }
                       
                 
                 
               }
           
           if(isTruthy(input$ttlselectyear)){
          
                 uniq_lse_alllse <- length(unique(subset(all_after.allchange(), all_after.allchange()$reporttype == input$ttlreporttype & 
                                                           all_after.allchange()$type == input$ttlreportcutoff &
                                                           all_after.allchange()$Fiscal_Year %in% input$ttlselectyear)$Lease_ID))
                 
                 alllseadd_lse <- alllseadd_lse %>% filter(Fiscal_Year %in% input$ttlselectyear) 
                 
                 alllseadd_lse$ttl_unique_lse <- rep(uniq_lse_alllse, nrow(alllseadd_lse))
             
           }
           if(isTruthy(input$ttlselectperiod)){
             
                 if(!isTruthy(input$ttlselectyear)){
                   
                   uniq_lse_alllse <- length(unique(subset(all_after.allchange(), all_after.allchange()$reporttype == input$ttlreporttype & 
                                                             all_after.allchange()$type == input$ttlreportcutoff &
                                                             all_after.allchange()$Fiscal_Period %in% input$ttlselectperiod)$Lease_ID))
                 }else{
                   
                   uniq_lse_alllse <- length(unique(subset(all_after.allchange(), all_after.allchange()$reporttype == input$ttlreporttype & 
                                                             all_after.allchange()$type == input$ttlreportcutoff &
                                                             all_after.allchange()$Fiscal_Year %in% input$ttlselectyear &
                                                             all_after.allchange()$Fiscal_Period %in% input$ttlselectperiod)$Lease_ID))
                   
                 }
                 
                 alllseadd_lse <- alllseadd_lse %>% filter(Fiscal_Period  %in% input$ttlselectperiod )
                 
                 alllseadd_lse$ttl_unique_lse <- rep(uniq_lse_alllse, nrow(alllseadd_lse))
         
           }
      }
           
    return(alllseadd_lse)
 
 }
    
  return(alllseadd_lse)  
      
} else{ return(NULL)}

  }
)


output$Overall_Lease_VS_Table <- DT::renderDataTable( 

       { 
         
         # for breakdown by Legal ID 
         
if(is.null(Overall_Lease_VS_Table_df()) == FALSE & !("Lease_ID_Count" %in% colnames(Overall_Lease_VS_Table_df()))){   
         
         DT::datatable(data = Overall_Lease_VS_Table_df(),
                       container = htmltools::withTags(
                         table(tableHeader(colnames(Overall_Lease_VS_Table_df())),
                               tableFooter(sapply(Overall_Lease_VS_Table_df(), function(x) if(is.numeric(x)) currency(sum(x, na.rm = TRUE), digits = 0L)))
                         )),
                    rownames = FALSE,
              
                    options = list(paging = TRUE,
                                   searching = TRUE,
                                   #fixedColumns = FALSE,
                                   autoWidth = TRUE,
                                   ordering = TRUE,
                                   dom = 'Bfrtip',
                                   #deferRender = TRUE,
                                   scrollY = 400,
                                   scrollX = TRUE,
                                   scroller = TRUE,
                                   #buttons = c( 'csv'),
                                   pageLength =80,
                                   footerCallback = JS(jsCode)
                                   )
                      
                   )
}else if(is.null(Overall_Lease_VS_Table_df()) == FALSE & ("Lease_ID_Count" %in% colnames(Overall_Lease_VS_Table_df()))) {
  
  
  jsCode2 <- sub("LeaseNum", unique(Overall_Lease_VS_Table_df()$ttl_unique_lse),jsCode2)
  
  report_all <- Overall_Lease_VS_Table_df()
  
  report_all$ttl_unique_lse <- NULL
  
  DT::datatable(data = report_all,
                container = htmltools::withTags(
                  table(tableHeader(colnames(report_all)),
                        tableFooter(sapply(report_all, function(x) 
                          if(is.numeric(x)) currency(sum(x, na.rm = TRUE), digits = 0L)))
                  )),
                
                rownames = FALSE,
                options = list(paging = TRUE,
                               searching = TRUE,
                               #fixedColumns = FALSE,
                               autoWidth = TRUE,
                               ordering = TRUE,
                               dom = 'Bfrtip',
                               #deferRender = TRUE,
                               scrollY = 400,
                               scrollX = TRUE,
                               scroller = TRUE,
                               #buttons = c( 'csv'),
                               pageLength = 80 ,  #nrow(unichange_summary),
                               footerCallback = JS(jsCode2)
                               
                )
                
  )
}
})
  
    # Download Button for new leases
output$download_Overall_Change <- downloadHandler(

    filename = function(){
      paste0("Overall Change Lease Report", "_", now(), ".", "xlsx")
    },

    if(!isTruthy( Overall_Lease_VS_Table_df()) == FALSE){
      content = function(file) {
        

        # Write to a file specified by the 'file' argument
        writexl::write_xlsx( Overall_Lease_VS_Table_df(),
                     file 
                           )
      }

    } else{return(NULL)}

  )  

################################################################################################################## 
####################################################################################################  
###################################################################################################
# output$out <- renderPrint({
#   
#   print(paste("is.null(new.lease.table.update2())", is.null(new.lease.table.update2())))
#   
#   print(paste("is.null(uni_change_lease2())", is.null(uni_change_lease2())))
#   
#   print(paste("is.null(multi_change_lease2())", is.null(multi_change_lease2())))
# 
#   
# 
# })




}


shinyApp(ui = ui, server = server)

#################################################

