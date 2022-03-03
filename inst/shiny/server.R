outputFolder <- shinySettings$dataFolder
dataYr0 <- readRDS(file.path(outputFolder, "dataByYear.RDS"))
dataYr0 <- dataYr0 %>% filter(calDate > 2016)
dataMth0 <- readRDS(file.path(outputFolder, "dataByMonth.RDS"))
dataMth0 <- dataMth0 %>% filter(calDate > as.Date("2016-12-31"))


shinyServer(function(input, output) {

  output$disease_condition <- renderUI({
    disease_list <- unique(dataYr0[dataYr0$analysis==input$analysis,]$diseaseCohortName)
    selectInput("disease", "Disease", disease_list, selected = disease_list[1])
  })

  output$drug_condition <- renderUI({
    if(input$analysis%in%c("Incidence of Disease", "Prevalence of Disease", "Prevalence of Drug")){
      drug_list <- unique(dataYr0[dataYr0$analysis==input$analysis, ]$drugCohortName)
    } else {
      drug_list <- unique(dataYr0[dataYr0$analysis==input$analysis & dataYr0$diseaseCohortName==input$disease, ]$drugCohortName)
    }
    selectInput("drug", "Drug", drug_list, selected = drug_list[1])
  })


  genderSelected <- reactive({
    if(input$gender=="All"){
      return("All")
    } else {
      return(input$bygender)
    }
  })

  ageSelected <- reactive({
    if(input$ageGroup=="All"){
      return("All")
    } else {
      return(input$byageGroup)
    }
  })

  #Yearly rate
  dataYr <- reactive({
    if(any(is.null(input$analysis), is.null(input$disease), is.null(input$drug))) return(NULL)
    dataYr <- dataYr0
    genderSelected <- genderSelected()
    ageSelected <- ageSelected()
    dataYr <- dataYr[dataYr$database==input$database & dataYr$analysis==input$analysis & dataYr$diseaseCohortName==input$disease & dataYr$drugCohortName==input$drug & dataYr$ageGroup%in%ageSelected & dataYr$Gender%in%genderSelected, ]
    dataYr$rate <- round(dataYr$rate*100, 4)
    dataYr
  })

  output$YrTable <- renderDataTable({
    if(any(is.null(input$analysis), is.null(input$disease), is.null(input$drug))) return(NULL)
    dataYr <- dataYr()
    dataYr <- dataYr[, c("diseaseCohortName", "drugCohortName", "calDate", "ageGroup", "Gender", "rate")]
    dataYr$calDate <- as.character(dataYr$calDate)
    dataYr$rate <- as.character(dataYr$rate)
    if(input$analysis=="Incidence of Disease"){
      names(dataYr) <- c("diseaseCohortName", "drugCohortName", "calDate", "ageGroup", "Gender", "Incidence(%)")
    } else {
      names(dataYr) <- c("diseaseCohortName", "drugCohortName", "calDate", "ageGroup", "Gender", "Prevalence(%)")
    }

    YrTable <- datatable(dataYr,
                         options = list(pageLength = 10),
                         rownames = FALSE)
    return(YrTable)
  })

  yrplot <- reactive({
    if(any(is.null(input$analysis), is.null(input$disease), is.null(input$drug))) return(NULL)
    dataYr <- dataYr()
    if(input$analysis=="Incidence of Disease") {
      plot_title <- "Incidence of Disease (Yearly)"
      plot_ylab <- "Incidence (%)"
    } else if(input$analysis=="Prevalence of Disease") {
      plot_title <- "Prevalence of Disease (Yearly)"
      plot_ylab <- "Prevalence (%)"
    } else if(input$analysis=="Prevalence of Drug") {
      plot_title <- "Prevalence of Drug (Yearly)"
      plot_ylab <- "Prevalence (%)"
    } else if(input$analysis=="Prevalence of Drug in Disease Cohort") {
      plot_title <- "Prevalence of Drug in Disease Cohort (Yearly)"
      plot_ylab <- "Prevalence (%)"
    }
    yrplot <- ggplot(dataYr, aes(x = calDate, y = rate, color = ageGroup, linetype = Gender)) +
      geom_line() +
      ggtitle(plot_title) +
      xlab("Date") +
      ylab(plot_ylab)
    return(yrplot)
  })

  output$YrPlot <- renderPlotly({
    if(any(is.null(input$analysis), is.null(input$disease), is.null(input$drug))) return(NULL)
    if(nrow(dataYr())==0) return(NULL)
    yrplot <- yrplot()
    yrplot
  })


  #Monthly rate
  dataMth <- reactive({
    if(any(is.null(input$analysis), is.null(input$disease), is.null(input$drug))) return(NULL)
    dataMth <- dataMth0
    genderSelected <- genderSelected()
    ageSelected <- ageSelected()
    dataMth <- dataMth[dataMth$database==input$database & dataMth$analysis==input$analysis & dataMth$diseaseCohortName==input$disease & dataMth$drugCohortName==input$drug & dataMth$ageGroup%in%ageSelected & dataMth$Gender%in%genderSelected, ]
    dataMth$rate <- round(dataMth$rate*100, 4)
    dataMth
  })

  output$MthTable <- renderDataTable({
    if(any(is.null(input$analysis), is.null(input$disease), is.null(input$drug))) return(NULL)
    dataMth <- dataMth()
    dataMth <- dataMth[, c("diseaseCohortName", "drugCohortName", "calDate", "ageGroup", "Gender", "rate")]
    dataMth$calDate <- as.character(dataMth$calDate)
    dataMth$rate <- as.character(dataMth$rate)
    if(input$analysis=="Incidence of Disease"){
      names(dataMth) <- c("diseaseCohortName", "drugCohortName", "calDate", "ageGroup", "Gender", "Incidence(%)")
    } else {
      names(dataMth) <- c("diseaseCohortName", "drugCohortName", "calDate", "ageGroup", "Gender", "Prevalence(%)")
    }

    MthTable <- datatable(dataMth,
                         options = list(pageLength = 10),
                         rownames = FALSE)
    return(MthTable)
  })

  mthplot <- reactive({
    if(any(is.null(input$analysis), is.null(input$disease), is.null(input$drug))) return(NULL)
    dataMth <- dataMth()
    if(input$analysis=="Incidence of Disease") {
      plot_title <- "Incidence of Disease (Monthly)"
      plot_ylab <- "Incidence (%)"
    } else if(input$analysis=="Prevalence of Disease") {
      plot_title <- "Prevalence of Disease (Monthly)"
      plot_ylab <- "Prevalence (%)"
    } else if(input$analysis=="Prevalence of Drug") {
      plot_title <- "Prevalence of Drug (Monthly)"
      plot_ylab <- "Prevalence (%)"
    } else if(input$analysis=="Prevalence of Drug in Disease Cohort") {
      plot_title <- "Prevalence of Drug in Disease Cohort (Monthly)"
      plot_ylab <- "Prevalence (%)"
    }
    mthplot <- ggplot(dataMth, aes(x = calDate, y = rate, color = ageGroup, linetype = Gender)) +
      geom_line() +
      scale_x_continuous(breaks = seq(min(dataMth$calDate), max(dataMth$calDate), by = "6 month"),
                         labels = format(seq(min(dataMth$calDate), max(dataMth$calDate), by = "6 month"), "%Y-%m")) +
      ggtitle(plot_title) +
      xlab("Date") +
      ylab(plot_ylab)
    return(mthplot)
  })

  output$MthPlot <- renderPlotly({
    if(any(is.null(input$analysis), is.null(input$disease), is.null(input$drug))) return(NULL)
    if(nrow(dataMth())==0) return(NULL)
    mthplot <- mthplot()
    mthplot
  })

})
