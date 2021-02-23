#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

source("ona2r.R")

library(tidyverse)
library(shiny)
library(shinyWidgets)
library(DT)

backcheck_keep <- c("serial_no_ML", "Member_org_BL",
                    "Region_BL", "District_BL", "Community_BL")
var_to_backcheck <- c("HouseholdSize",
                      "livelihood_zone",
                      "shock_main",
                      "rCSI",
                      "house_ownership",
                      "own_land",
                      "has_livestock",
                      "time_to_water",
                      "head_occupation")
backcheck_keep <- paste0(backcheck_keep,".s")
backcheck_var <- paste0(var_to_backcheck,".s")

midline_keep <- c("serial_no_ML", "Member_org_BL",
                  "Region_BL", "District_BL", "Community_BL", "username", 
                  "midline_who", 
                  "contact_number", "contact_number2","start_time", "CompletionDateTime","date",
                  "interviewDuration",  "interviewDuringDay", "veryshort", "short",  "reasonableDuration")

midline_keep <- paste0(midline_keep,".m")
midline_var <- paste0(var_to_backcheck,".m")





varshown <- c("percentMatch", "serial_no_ML.m", "Member_org_BL.m","username.m", "start_time.m", "CompletionDateTime.m","interviewDuration.m",
              "Region_BL.m","District_BL.m", "Community_BL.m", c(rbind(midline_var, backcheck_var)))
allvars <- c("percentMatch",midline_keep, midline_var, backcheck_keep, backcheck_var)

listData <- function(){
  d <- data.frame(matrix(ncol=length(varshown), nrow = 0))
  colnames(d)<-varshown
  return(d)
}


prepareData <- function(midline, backcheck){
  midline$CompletionDateTime <- as.POSIXct(str_sub(gsub("T", " ", midline$CompletionDateTime), 1, 19)) # fixing some issue with the format of the completion time
  midline$date <- as.character(as.Date(midline$CompletionDateTime, tz = "GMT"))
  midline$interviewDuration <- difftime(midline$CompletionDateTime, midline$start, units='mins')
  midline$interviewDuringDay <- between(format(midline$start, format="%H%M%S"),70000, 190000)
  midline$reasonableDuration <- between(midline$interviewDuration, 30, 90)
  midline$short <- between(midline$interviewDuration, 25, 30)
  midline$veryshort <- midline$interviewDuration<25
    #midline$nbDontknow <- apply(midline,1,function(x) sum(x=="dontknow", na.rm=T))

  colnames(midline) <- paste0(colnames(midline),".m")
  colnames(backcheck) <- paste0(colnames(backcheck),".s")
  
  data_check <- left_join(midline[,c(midline_keep,midline_var)], backcheck[,c(backcheck_keep,backcheck_var)], by=c("serial_no_ML.m"="serial_no_ML.s"), keep=TRUE)
  
  data_check$qualScore <-0
  
  for(i in 1:length(var_to_backcheck)){
    isItDifferent <- ifelse(data_check[,backcheck_var[i]]!=data_check[,midline_var[i]], 1, 0)
    if(var_to_backcheck[i] %in% c("HouseholdSize", "rCSI")){
      isItDifferent <- (abs(data_check[,backcheck_var[i]]-data_check[,midline_var[i]])) > 2
    }else if(var_to_backcheck[i]=="time_to_water"){
      max_time <- pmax(data_check[,backcheck_var[i]], data_check[,midline_var[i]]) 
      isItDifferent <- ((abs(data_check[,backcheck_var[i]]-data_check[,midline_var[i]])) > (max_time*0.2)) | (max_time <3)
    }
    data_check$qualScore <- data_check$qualScore + ifelse(is.na(isItDifferent), 0.5, isItDifferent)
  }
  
  data_check$percentMatch <- ifelse(is.na(data_check$serial_no_ML.s), NA, 100-data_check$qualScore/length(backcheck_var)*100)
  
  
  return(data_check)
}



get_data <- function(login, password){
    d_midline <- tryCatch(onaDownload("BRCiS_Midline_Survey_2021", "BRCiS",login,password, keepGroupNames=FALSE), error=function(e){message("can't access data")})
    d_backcheck <- tryCatch(onaDownload("BRCiS_spot_check_midline", "BRCiS",login,password, keepGroupNames=FALSE), error=function(e){message("can't access data")})
    if(length(d_midline)>1 & length(d_midline)>1){
      midline <- as.data.frame(d_midline) %>%
        dplyr::rename(
          contact_number=a_1705,
          contact_number2=a_1708,
          livelihood_zone=e0.cluster.e5.b_110,
          shock_main=f.f1.main_shock,
          house_ownership=j4.a_516,
          own_land=k2.a_616,
          has_livestock=l.a_618,
          time_to_water=m.m1.a_704_a,
          head_occupation=v.a_1604
        )
      backcheck <- as.data.frame(d_backcheck) %>%
        dplyr::rename(
          livelihood_zone=Spot_Check.b_110,
          shock_main=Spot_Check.shock_main,
          house_ownership=Spot_Check.a_516,
          own_land=Spot_Check.a_616,
          has_livestock=Spot_Check.a_618,
          time_to_water=Spot_Check.a_704_a,
          head_occupation=Spot_Check.a_1604
        )
      return(prepareData(midline, backcheck))
    }
}

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    titlePanel("BRCiS data collection monitoring dashboard"),
    
    sidebarLayout(
      sidebarPanel(
        actionButton("load_data", "Load data"),
        #"Region_BL", "District_BL", "Community_BL", "username"
        #selectInput("summary_by", "Choose focus of quality summary table", c("agency.m", "username.m"), multiple = TRUE),
        pickerInput("summary_by", "Summary by (top table)", c("Member_org_BL.m", "District_BL.m", "Community_BL.m", "username.m"), selected = c("Member_org_BL.m"), multiple = TRUE),
        #pickerInput("summary_by", "Summary by (top table)", c("Member_org_BL.m"), selected = c("Member_org_BL.m"), multiple = TRUE),
        pickerInput("filter_date", "Filter date",sort(unique(listData()$date.m), na.last=TRUE),selected = unique(listData()$date.m),options = list(`actions-box` = TRUE), multiple = T),
        pickerInput("filter_partner", "Filter Member Organisation",sort(unique(listData()$Member_org_BL.m), na.last=TRUE),selected = unique(listData()$Member_org_BL.m),options = list(`actions-box` = TRUE), multiple = T),
        pickerInput("filter_district", "Filter district",sort(unique(listData()$District_BL.m), na.last=TRUE),selected = unique(listData()$District_BL.m),options = list(`actions-box` = TRUE), multiple = T),
        pickerInput("filter_username", "Filter username",sort(unique(listData()$username.m), na.last=TRUE),selected=unique(listData()$username.m),options = list(`actions-box` = TRUE), multiple = T),
        h5("For bottom table:"),
        #sliderInput("dontknow_threshold",
        #            "Show when nomber of dont know is greater than... ",
        #            min=0,
        #            max=50,
        #            value=0),
        #sliderInput("duration_threshold",
        #            "Show when interview durations is less than... ",
        #            min=0,
        #            max=999900,
        #            value=200),
        sliderInput("data_quality_threshold",
                    "Show when back-check percent match is less than... ",
                    min=0,
                    max=100,
                    value=100),
        downloadButton("downloadtable1", "Download top table"),
        downloadButton("downloadtable2", "Download bottom table")
      ),
      
      mainPanel(
        dataTableOutput("summary_table"),
        br(),br(),
        dataTableOutput("data")
      )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    data <- reactiveValues()
    data$check <- data.frame(matrix(ncol=length(allvars), nrow = 0, dimnames=list(NULL, allvars)) )
    # observe({showModal(modalDialog(
    #   title = "start",
    #   paste(colnames(data$check)[1:5], collapse=","),
    #   easyClose = TRUE,
    #   footer = NULL
    # ))
    # })
    observeEvent(input$load_data, {
        showModal(modalDialog(
            textInput('login', 'Please enter your ona username'),
            passwordInput('password', 'Please enter your ona password'),
            footer=tagList(
                actionButton('submit', 'Submit'),
                modalButton('cancel')
            )
        ))
    })
    observeEvent(input$submit, {
        data$check <- get_data(isolate(input$login), isolate(input$password))
        removeModal()
        updatePickerInput(session, "filter_partner", choices = sort(unique((data$check)$Member_org_BL.m), na.last=TRUE),selected = unique((data$check)$Member_org_BL.m))
        updatePickerInput(session, "filter_date", choices = sort(unique((data$check)$date.m), na.last=TRUE),selected = unique((data$check)$date.m))
        updatePickerInput(session, "filter_district", choices = sort(unique((data$check)$District_BL.m), na.last=TRUE),selected = unique((data$check)$District_BL.m))
    })
    

    # update list of usernames
    updatedChoices = reactive({
      filtered_data <- isolate(data$check) %>%
        filter(Member_org_BL.m %in% input$filter_partner,
               District_BL.m %in% input$filter_district,
               date.m %in% input$filter_date)
      enum_choices <- filtered_data %>%
        pull(username.m) %>% unique() %>% sort(na.last=TRUE)
      tmp<-list(enum_choices)
      return(tmp)
    })
    observe({
      updatePickerInput(session, "filter_username", choices = updatedChoices()[[1]], selected=updatedChoices()[[1]])
    })
    
    

    
    # Prepare top table (summary)
    summaryTable <- reactive({
      # showModal(modalDialog(
      #      title = "start",
      #      paste(colnames(data$check)[1:5], collapse=","),
      #      easyClose = TRUE,
      #      footer = NULL
      #    ))
      isolate(data$check)%>%
        filter(Member_org_BL.m%in%input$filter_partner,
               username.m %in% input$filter_username,
               date.m %in% input$filter_date,
               District_BL.m%in% input$filter_district)%>%
         group_by_at(dplyr::vars(input$summary_by))%>%
         dplyr::summarise(N=sum(!is.na(serial_no_ML.m), na.rm=T),
                   time_ok=mean(interviewDuringDay.m, na.rm=TRUE),
                   avg_duration = mean(interviewDuration.m, na.rm=TRUE),
                   `<25min`=mean(veryshort.m, na.rm=TRUE),
                   `25-30min`=mean(short.m, na.rm=TRUE),
                   `30-90min` = mean(reasonableDuration.m, na.rm=TRUE),
                   #avg_dontknow = mean(nbDontknow.m),
                   N_backchecked=sum(!is.na(serial_no_ML.s), na.rm=T),
                   #prop_bc=sum(!is.na(index.s), na.rm=T)/sum(!is.na(index.m), na.rm=T),
                   avg_match_perc=mean(percentMatch, na.rm=T)/100,
                   min_match_perc=min(percentMatch, na.rm=T)/100
                   #`25percMatch`=quantile(percentMatch, probs = .25, na.rm=T)/100

         )
    })
    
    # Prepare bottom table
    filteredRawData <- reactive({
      data$check %>%

        # Apply filter
        #filter(!is.na(index.s))%>%
        #filter(percentMatch<=input$data_quality_threshold | input$data_quality_threshold==100)%>%
        #filter(nbDontknow.m>=input$dontknow_threshold)%>%
        #filter(reasonableDuration.m<=input$duration_threshold | is.na(input$duration_threshold))%>%
            
        filter(username.m %in% input$filter_username,
               Member_org_BL.m%in%input$filter_partner,
               date.m %in% input$filter_date,
               District_BL.m%in% input$filter_district)%>%
        filter(percentMatch<=input$data_quality_threshold | input$data_quality_threshold==100)%>%
        .[,varshown]
    })


    # show the top table
    output$summary_table <- renderDataTable({
      datatable(summaryTable()) %>%
        formatPercentage(c("avg_match_perc", "min_match_perc", "<25min", "25-30min", "30-90min", "time_ok"), 0)%>%
        formatRound(c("avg_duration"), 1)
    })

    # show the bottom table
    output$data <- renderDataTable({
      datatable(filteredRawData()) %>%
        formatRound(c("percentMatch"), 2)%>%
        formatRound(c("interviewDuration.m"), 0)
    })

    # make the download top table button
    output$downloadtable1 <- downloadHandler(
      filename = function() "summaryTable.csv",
      content = function(file) {
        write.csv(summaryTable(), file, row.names = FALSE)
      }
    )

    # make the download bottom table button
    output$downloadtable2 <- downloadHandler(
      filename = function() "filteredRawData.csv",
      content = function(file) {
        write.csv(filteredRawData(), file, row.names = FALSE)
      }
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
