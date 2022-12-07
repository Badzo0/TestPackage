

likert_recode_4 <- function(x) {
  y <- ifelse(is.na(x), NA,
              ifelse(x == 1, "Never",
                     ifelse(x == 2, "Sometimes",
                            ifelse(x == 3, "Often", "Very Often"))))

  y <- factor(y, levels = c("Never", "Sometimes", "Often", "Very Often"))

  return(y)
}

likert_recode <- function(x) {
  y <- ifelse(is.na(x), NA,
              ifelse(x == 1, "Poor",
                     ifelse(x == 2, "2",
                            ifelse(x == 3, "3",
                                   ifelse(x == 4, "4",
                                          ifelse(x == 5, "5",
                                                 ifelse(x == 6, "6", "Excellent")))))))

  y <- factor(y, levels = c("Poor", "2", "3", "4","5","6", "Excellent"))

  return(y)
}

df<-NULL
shinyServer(function(input, output, session) {
  # Data<-df
  hideTab(inputId = "tabs", target = "Supportive Environment")
  hideTab(inputId = "tabs", target = "Quantitative Reasoning")
  hideTab(inputId = "tabs", target = "Quality of Interactions")


  df <- reactive({
    req(input$file1)
    read_excel(input$file1$datapath, sheet = "Data")
  })

  #-------------- Logging for troubleshooting--------------
  observe({
    print(input$file1)
  })
  observe({

    print(input$inSelect)
  })
  observe({
    print("N-grams:")
    print(input$obs)
  })



  #-------------- end --------------



  observeEvent(input$file1, {
    names<-df() %>% group_by(CourseNm) %>% tally() %>% filter(n>20) %>%  select(CourseNm)

    # names <- unique(df()$CourseNm)
    # Method 1
    updatePickerInput(session = session, inputId = "inSelect",
                      choices = names)
  }, ignoreInit = TRUE)




  output$contents <- renderReactable({
    req(input$file1)
    tryCatch(
      {
        #browser()
        #df <-  read_excel(input$file1$datapath, sheet = "Data")
        showTab(inputId = "tabs", target = "Supportive Environment")
        showTab(inputId = "tabs", target = "Quantitative Reasoning")
        showTab(inputId = "tabs", target = "Quality of Interactions")
        if(is.null(input$inSelect)==T ) Data=df()
        else Data= df() %>%  filter(CourseNm %in% input$inSelect )
        Data %>% group_by(study_group) %>% dplyr::summarise(n=n()) %>%
          reactable()
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )

  })


  output$contents2 <- renderReactable({
    if(is.null(input$inSelect)==T ) Data=df()
    else Data= df() %>%  dplyr::filter(CourseNm %in% input$inSelect )
    Data %>% tally() %>% reactable()

  })






  #DOWNLOAD HANDLER
  source('download_handler.R', local = TRUE)

  #Do an if and look for null in path and only load once the path is there
  # reactive(
  #if (is.null(df)) {
  # }else   {
  #----------High Level Indicators-----------
  # source('High_level_Indicators.R', local = TRUE)
  waitress <- Waitress$new("nav",theme = "line", infinite = TRUE)

  Waitress$new("#plot")$auto(20, 2000)
  #creates report link from dropdown selection
  reportname <- reactive({
    reportname<- paste0(input$selectreport,".Rmd")
  })
  #Download handler

  ending <- reactive({
    if (input$selectreport =="Student_Survey_2021_Indicators") ending<-".html"
    else if(input$selectreport =="Student_Survey_2022")ending<-".html"
    else{ending <-".html"}
  })

  parameters <- reactive({
    if (input$selectreport =="Student_Survey_2021_Indicators") {
      list(n = input$file1$datapath,
           obs=input$obs)}else if(input$selectreport =="Student_Survey_2022"){list(n = input$file1$datapath,
                                                                                   obs=input$obs,
                                                                                   avgr=input$seleccompare)}else{
                                                                                     list(n = input$file1$datapath,
                                                                                          course=input$inSelect,
                                                                                          avgr=input$seleccompare)}
  })

  filename <- reactive({
    filename<-paste0(input$selectreport , ending())
  })

  observe({
    print(paste0("ending:",ending()))
    print(paste0("parameters:", parameters()))
    print(paste0("filename:", filename()))

  })

  output$report <- downloadHandler(




    # For PDF output, change this to "report.pdf"
    filename =function() filename(),
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), reportname())
      file.copy(reportname(), tempReport, overwrite = TRUE)
      tempReport2 <- file.path(tempdir(), "logo1.png")
      file.copy("logo1.png", tempReport2, overwrite = TRUE)
      tempReport2 <- file.path(tempdir(), "img7.png")
      file.copy("img7.png", tempReport2, overwrite = TRUE)
      tempReport2 <- file.path(tempdir(), "img8.png")
      file.copy("img8.png", tempReport2, overwrite = TRUE)
      tempReport2 <- file.path(tempdir(), "Picture1.png")
      file.copy("Picture1.png", tempReport2, overwrite = TRUE)
      # Set up parameters to pass to Rmd document
      params <- parameters()

      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      waitress$start()



      #-------------- Error handling-------------


      tryCatch({
        return( if(input$inSelect=="" && input$selectreport=="Course_report"){stop("throw error")}else{        rmarkdown::render(tempReport, output_file = file,
                                                                                                                                 params = params,
                                                                                                                                 envir = new.env(parent = globalenv())
        )
          waitress$close() # hide when done
        }



        )

      },
      error = function(e) {
        message('No data in chosen Analysis.', e)
        showModal(
          modalDialog(
            title="Error in generating report",
            div("Looks like a course was not selected which prevents Course_report from generating
                "),
            div("Error Details:"),
            div(paste("Selected report:",input$selectreport)),
            div(paste("Selected course:",input$inSelect))
            ,easyClose = TRUE,
            fade = T)
        )
        return(NULL)
      }
      )



      #-------------- end --------------


      waitress$close() # hide when done

    }
  )
  #--------Supportive Environment--------
  # source('Supportive_environment.R', local = TRUE)
  # #--------Quantitative Reasoning--------
  # source('Quantitative_reasoning.R', local = TRUE)
  # #--------Quality of interactions--------
  # source('Quality_of_interactions.R', local = TRUE)
  #})















  # output$table2 <- renderReactable({
  #   avgr=data.frame(0)
  #   ifelse(input$seleccompare=="All StudentSurvey.ie",avgr<-StudentSurvey.ie,
  #          ifelse(input$seleccompare=="StudentSurvey.ie Universities",avgr<-Universities,
  #                 ifelse(input$seleccompare=="StudentSurvey.ie Technological Higher Education Institutions",avgr<-Higher.Education.Institutions,avgr<-Other.Institutions)))
  #gh
  #
  #   avgr%>% reactable()
  #
  # })


})
