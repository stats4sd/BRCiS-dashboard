#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# contains code that allows to connect to ona.io
source("ona2r.R")

library(tidyverse)
library(shiny)
library(shinyWidgets)
library(DT)

# read the table containing the targets + tweak it
d_targets <- read.csv("BackCheck-VariableMatching.csv")
d_targets <- d_targets %>% 
  mutate(N = NA,Member_org_BL=Member_id_BL) %>%
  mutate(Member_id_BL=NULL, Country="Somalia")
colnames(d_targets)=paste0(colnames(d_targets), ".m")


# list of the variables use to check midline/backcheck match
var_to_backcheck <- c("HouseholdSize",
                      "livelihood_zone",
                      "shock_main",
                      "rCSI",
                      "house_ownership",
                      "own_land",
                      "has_livestock",
                      "time_to_water",
                      "head_occupation")
backcheck_var <- paste0(var_to_backcheck,".s")
midline_var <- paste0(var_to_backcheck,".m")


# list other variables to keep
backcheck_keep <- c("serial_no_ML", "Member_org_BL",
                    "Region_BL", "District_BL", "Community_BL")
backcheck_keep <- paste0(backcheck_keep,".s")

midline_keep <- c("serial_no_ML", "Member_org_BL",
                  "Region_BL", "District_BL", "Community_BL", "TeamLeader", "username", 
                  "midline_who", 
                  "contact_number", "contact_number2","start_time", "CompletionDateTime","date",
                  "interviewDuration",  "interviewDuringDay", "extrashort", "supershort", "veryshort", "short",  "reasonableDuration", "toolong", "nbDontknow", "nb0s")
midline_keep <- paste0(midline_keep,".m")



# variables that will be shown in the bottom tables
varshown <- c("percentMatch", "serial_no_ML.m", "Member_org_BL.m","username.m","TeamLeader.m", "start_time.m", "CompletionDateTime.m","interviewDuration.m","nbDontknow.m","nb0s.m",
              "Region_BL.m","District_BL.m", "Community_BL.m", c(rbind(midline_var, backcheck_var)))
allvars <- c("percentMatch",midline_keep, midline_var, backcheck_keep, backcheck_var)


# function that initialise a dataframe in order not to get errors at the beginning - could do it much simpler, but used some older code and didn't change it
listData <- function(){
  d <- data.frame(matrix(ncol=length(varshown), nrow = 0))
  colnames(d)<-varshown
  return(d)
}

# function that makes the necessary preparation to the data, like calculating the right variables to be used in the summary table
# also merges the midline and backcheck data and calculate the percent match 
prepareData <- function(midline, backcheck){
  midline$CompletionDateTime <- as.POSIXct(str_sub(gsub("T", " ", midline$CompletionDateTime), 1, 19)) # fixing some issue with the format of the completion time
  midline$date <- as.character(as.Date(midline$CompletionDateTime, tz = "GMT"))
  midline$interviewDuration <- difftime(midline$CompletionDateTime, midline$start, units='mins')
  midline$interviewDuringDay <- between(format(midline$start, format="%H%M%S"),70000, 190000)
  midline$toolong <- midline$interviewDuration>90
  midline$reasonableDuration <- between(midline$interviewDuration, 25, 90)
  midline$short <- between(midline$interviewDuration, 25, 30)
  midline$veryshort <- between(midline$interviewDuration, 20, 25)
  midline$supershort <- between(midline$interviewDuration, 15, 20)
  midline$extrashort <- midline$interviewDuration<15
  midline$nbDontknow <- apply(midline,1,function(x) sum(x%in%c(-8, -9), na.rm=T))
  midline$nb0s <- apply(midline,1,function(x) sum(x==0, na.rm=T))
  midline$TeamLeader <- as.character(midline$TeamLeader)
  
  #add .m/.s to differentiate midline and backcheck
  colnames(midline) <- paste0(colnames(midline),".m")
  colnames(backcheck) <- paste0(colnames(backcheck),".s")
  
  #merge midline and backcheck
  data_check <- left_join(midline[,c(midline_keep,midline_var)], backcheck[,c(backcheck_keep,backcheck_var)], by=c("serial_no_ML.m"="serial_no_ML.s"), keep=TRUE)
  
  #Calculates matching percent score
  data_check$qualScore <-0
  
  for(i in 1:length(var_to_backcheck)){
    isItDifferent <- ifelse(data_check[,backcheck_var[i]]!=data_check[,midline_var[i]], 1, 0)
    if(var_to_backcheck[i] %in% c("HouseholdSize", "rCSI")){
      isItDifferent <- (abs(as.numeric(data_check[,backcheck_var[i]])-data_check[,midline_var[i]])) > 2
    }else if(var_to_backcheck[i]=="time_to_water"){
      max_time <- pmax(as.numeric(data_check[,backcheck_var[i]]), data_check[,midline_var[i]]) 
      isItDifferent <- ((abs(as.numeric(data_check[,backcheck_var[i]])-data_check[,midline_var[i]])) > (max_time*0.2)) | (max_time <3)
    }
    data_check$qualScore <- data_check$qualScore + ifelse(is.na(isItDifferent), 0.5, isItDifferent)
  }
  
  data_check$percentMatch <- ifelse(is.na(data_check$serial_no_ML.s), NA, 100-data_check$qualScore/length(backcheck_var)*100)
  
  return(data_check)
}


# function to retrieve data from ona and rename the important variables to match
get_data <- function(login, password){
    d_midline <- tryCatch(onaDownload("BRCiS_Midline_Survey_2021", "BRCiS",login,password, keepGroupNames=FALSE), error=function(e){message("can't access data")})
    d_backcheck <- tryCatch(onaDownload("BRCiS_spot_check_midline", "BRCiS",login,password, keepGroupNames=FALSE), error=function(e){message("can't access data")})
    if(length(d_midline)>1 & length(d_midline)>1){
      d_midline <- d_midline[!is.na(d_midline$serial_no_ML),-711]
      d_backcheck <- d_backcheck[!is.na(d_backcheck$serial_no_ML),]
      midline <- as.data.frame(d_midline) %>%
        dplyr::rename(
          contact_number=a_1705,
          contact_number2=a_1708,
          #livelihood_zone=e0.cluster.e5.b_110,
          livelihood_zone=f.f1.b_110,
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


# Define the app look, buttons, select inputs
ui <- fluidPage(

    titlePanel("BRCiS data collection monitoring dashboard"),
    
    sidebarLayout(
      sidebarPanel(
        actionButton("load_data", "Load data"),
        #"Region_BL", "District_BL", "Community_BL", "username"
        #selectInput("summary_by", "Choose focus of quality summary table", c("agency.m", "username.m"), multiple = TRUE),
        pickerInput("target_by", "%target by (top table)", c("Country.m","Member_org_BL.m", "Region_BL.m", "District_BL.m"), selected = c("Country.m"), multiple = TRUE),
        pickerInput("summary_by", "Summary by (middle table)", c("date.m","Member_org_BL.m", "Region_BL.m","District_BL.m", "Community_BL.m","TeamLeader.m", "username.m"), selected = c("Member_org_BL.m"), multiple = TRUE),
        #pickerInput("summary_by", "Summary by (top table)", c("Member_org_BL.m"), selected = c("Member_org_BL.m"), multiple = TRUE),
        pickerInput("filter_date", "Filter date",sort(unique(listData()$date.m), na.last=TRUE),selected = unique(listData()$date.m),options = list(`actions-box` = TRUE), multiple = T),
        pickerInput("filter_partner", "Filter Member Organisation",sort(unique(listData()$Member_org_BL.m), na.last=TRUE),selected = unique(listData()$Member_org_BL.m),options = list(`actions-box` = TRUE), multiple = T),
        pickerInput("filter_district", "Filter district",sort(unique(listData()$District_BL.m), na.last=TRUE),selected = unique(listData()$District_BL.m),options = list(`actions-box` = TRUE), multiple = T),
        pickerInput("filter_teamLeader", "Filter team leader",sort(unique(listData()$TeamLeader.m), na.last=TRUE),selected=unique(listData()$TeamLeader.m),options = list(`actions-box` = TRUE), multiple = T),
        pickerInput("filter_username", "Filter username",sort(unique(listData()$username.m), na.last=TRUE),selected=unique(listData()$username.m),options = list(`actions-box` = TRUE), multiple = T),
        h5("For bottom table:"),

        sliderInput("data_quality_threshold",
                    "Show when back-check percent match is less than... ",
                    min=0,
                    max=100,
                    value=100),
        downloadButton("downloadtable1", "Download top table"),
        downloadButton("downloadtable2", "Download bottom table")
      ),
      
      mainPanel(
        dataTableOutput("target_table"),
        br(),br(),
        dataTableOutput("summary_table"),
        br(),br(),
        dataTableOutput("data")
      )
    )
)

# Define what tables to show and what to do when pressed button/select input
server <- function(input, output, session) {
    data <- reactiveValues() # store data to be used for tables
    
    #initiate table for when data has not been loaded yet
    data$check <- data.frame(matrix(ncol=length(allvars), nrow = 0, dimnames=list(NULL, allvars)) )
    data$targets <- d_targets %>% slice(0)
    
    #ask for ona username and password
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
    
    # updates picker input choices
    observeEvent(input$submit, {
        data$check <- get_data(isolate(input$login), isolate(input$password))
        removeModal()
        updatePickerInput(session, "filter_partner", choices = sort(unique((data$check)$Member_org_BL.m), na.last=TRUE),selected = unique((data$check)$Member_org_BL.m))
        updatePickerInput(session, "filter_date", choices = sort(unique((data$check)$date.m), na.last=TRUE),selected = unique((data$check)$date.m))
        updatePickerInput(session, "filter_district", choices = sort(unique((data$check)$District_BL.m), na.last=TRUE),selected = unique((data$check)$District_BL.m))
        updatePickerInput(session, "filter_teamLeader", choices = sort(unique((data$check)$TeamLeader.m), na.last=TRUE),selected = unique((data$check)$TeamLeader.m))
        #updatePickerInput(session, "filter_teamLeader", choices = sort(unique((data$check)$TeamLeader.m), na.last=TRUE),selected = unique((data$check)$TeamLeader.m))
        
        # merges targets with actual data so that %target can later be calculated
        forTargets<- data$check %>%
          dplyr::group_by(Member_org_BL.m, Region_BL.m, District_BL.m)%>%
          dplyr::summarise(N.m=sum(!is.na(unique(serial_no_ML.m)), na.rm=T))
        data$targets <- left_join(select(d_targets, -N.m), forTargets, by=c("Member_org_BL.m"="Member_org_BL.m","Region_BL.m"="Region_BL.m", "District_BL.m"="District_BL.m"))
        
    })
    

    # update list of usernames
    updatedChoices = reactive({
      filtered_data <- isolate(data$check) %>%
        filter(Member_org_BL.m %in% input$filter_partner,
               District_BL.m %in% input$filter_district,
               date.m %in% input$filter_date, 
               TeamLeader.m %in% input$filter_teamLeader)
      enum_choices <- filtered_data %>%
        pull(username.m) %>% unique() %>% sort(na.last=TRUE)
      tmp<-list(enum_choices)
      return(tmp)
    })
    observe({
      updatePickerInput(session, "filter_username", choices = updatedChoices()[[1]], selected=updatedChoices()[[1]])
    })
    
    # Prepare the very top table (%target table)
    targetTable <- reactive({
      data$targets%>%
        mutate(N.m=ifelse(is.na(N.m), 0, N.m)) %>%
        group_by_at(dplyr::vars(input$target_by))%>%
        dplyr::summarise(AreaHHAvail.m=sum(AreaHHAvail.m),ParticipantHHAvail.m=sum(ParticipantHHAvail.m),
                         TargetNoHH.m=sum(TargetNoHH.m), NumHHAvail.m=sum(NumHHAvail.m),N.m=sum(N.m)) %>%
        mutate(target_perc=N.m/TargetNoHH.m)
    })

    
    # Prepare top table (summary)
    summaryTable <- reactive({
      isolate(data$check)%>%
        filter(Member_org_BL.m%in%input$filter_partner,
               username.m %in% input$filter_username,
               date.m %in% input$filter_date,
               District_BL.m%in% input$filter_district)%>%
         group_by_at(dplyr::vars(input$summary_by))%>%
         dplyr::summarise(N=sum(!is.na(serial_no_ML.m), na.rm=T),
                   time_ok=mean(interviewDuringDay.m, na.rm=TRUE),
                   avg_duration = mean(interviewDuration.m, na.rm=TRUE),
                   `<15min`=mean(extrashort.m, na.rm=TRUE),
                   `15-20min`=mean(supershort.m, na.rm=TRUE),
                   `20-25min`=mean(veryshort.m, na.rm=TRUE),
                   #`25-30min`=mean(short.m, na.rm=TRUE),
                   `25-90min` = mean(reasonableDuration.m, na.rm=TRUE),
                   `>90min`= mean(toolong.m, na.rm=TRUE),
                   avg_dontknow = mean(nbDontknow.m),
                   avg_0s = mean(nb0s.m),
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
        filter(username.m %in% input$filter_username,
               Member_org_BL.m%in%input$filter_partner,
               date.m %in% input$filter_date,
               District_BL.m%in% input$filter_district)%>%
        filter(percentMatch<=input$data_quality_threshold | input$data_quality_threshold==100)%>%
        .[,varshown]
    })
    
    # show the top table
    output$target_table <- renderDataTable({
      datatable(targetTable()) %>%
        formatPercentage(c("target_perc"), 1)
    })

    # show the summary table
    output$summary_table <- renderDataTable({
      datatable(summaryTable()) %>%
        formatPercentage(c("avg_match_perc", "min_match_perc", "<15min","15-20min", "20-25min", "25-90min", ">90min", "time_ok"), 0)%>%
        formatRound(c("avg_duration", "avg_dontknow"), 1)
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
