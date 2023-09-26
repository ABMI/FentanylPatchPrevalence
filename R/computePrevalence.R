computePrevalence <- function(yearStartDate,
                              yearEndDate,
                              monthStartDate,
                              monthEndDate,
                              databaseName,
                              outputFolder,
                              minCellCount){

  yearStartDate <- max(as.Date(yearStartDate), as.Date("2012-01-01"))
  yearEndDate <- as.Date(yearEndDate)
  monthStartDate <- max(as.Date(monthStartDate), as.Date("2012-01-01"))
  monthEndDate <- as.Date(monthEndDate)

  tmpDir <- file.path(outputFolder, "tmpData")

  incYr_disease<-data.frame()
  incMth_disease<-data.frame()
  prevYr_disease<-data.frame()
  prevMth_disease<-data.frame()
  incYr_drug<-data.frame()
  incMth_drug<-data.frame()
  prevYr_drug<-data.frame()
  prevMth_drug<-data.frame()
  prevYr <- data.frame()
  prevMth<- data.frame()
  incYr<-data.frame()
  incMth<-data.frame()

  outcomesOfInterest <- read.csv(file.path("inst", "settings", "DiseaseDrugOfInterest.csv"))
  cohortsToCreate <- read.csv(file.path("inst", "settings", "CohortsToCreate.csv"))

  `%>%` <- magrittr::`%>%`

  # Calculate number of people observed in the whole database by year/month
  data_whole <- readRDS(file.path(tmpDir, "DatasetCohort.RDS")) %>%
    dplyr::rename(
      start_year = START_YEAR,
      end_year = END_YEAR,
      start_yearMth = START_YEARMTH,
      end_yearMth = END_YEARMTH
    )

  whole_yr <- data.frame()
  for(y in lubridate::year(yearStartDate):lubridate::year(yearEndDate)){
    whole_yr_y <- data_whole %>%
      dplyr::filter(y>=start_year & y<=end_year) %>%
      dplyr::mutate(calDate = y,
                    AGE_YR = lubridate::as.period(lubridate::interval(DOB, as.Date(paste0(y, '-07-01'))))$year) %>%
      #dplyr::mutate(AGE_YR_GROUP = cut(AGE_YR, breaks = c(0, 17, 24, 44, 64, 74, 84, 150), include.lowest = TRUE)) %>%
      dplyr::mutate(AGE_YR_GROUP = cut(AGE_YR, breaks = c(0, 17, 24, 34, 44, 54, 64, 150), include.lowest = TRUE)) %>%
      dplyr::group_by(calDate, AGE_YR_GROUP, GENDER) %>%
      dplyr::summarise(w = dplyr::n_distinct(SUBJECT_ID), .groups = 'drop')
    whole_yr <- rbind(whole_yr, whole_yr_y)
  }

  whole_mth <- data.frame()
  yearMth_seq <- seq(monthStartDate, monthEndDate, by = "month")
  for(m in as.list(yearMth_seq)){
    whole_mth_m <- data_whole %>%
      dplyr::filter(m>=start_yearMth & m<=end_yearMth) %>%
      dplyr::mutate(calDate = m,
                    AGE_MTH = lubridate::as.period(lubridate::interval(DOB, as.Date(paste0(lubridate::year(m), '-07-01'))))$year) %>%
      #dplyr::mutate(AGE_MTH_GROUP = cut(AGE_MTH, breaks = c(0, 17, 24, 44, 64, 74, 84, 150), include.lowest = TRUE)) %>%
      dplyr::mutate(AGE_MTH_GROUP = cut(AGE_MTH, breaks = c(0, 17, 24, 34, 44, 54, 64, 150), include.lowest = TRUE)) %>%
      dplyr::group_by(calDate, AGE_MTH_GROUP, GENDER) %>%
      dplyr::summarise(w = dplyr::n_distinct(SUBJECT_ID), .groups = 'drop')
    whole_mth <- rbind(whole_mth, whole_mth_m)
  }
  rm(whole_yr_y, whole_mth_m)

  #####
  DatasetCountsFile <- file.path(outputFolder, "DatasetCounts.csv")
  if (file.exists(DatasetCountsFile)) {
    DatasetCounts <- read.csv(DatasetCountsFile)
  } else {
  # Calculate the total number of people observed in the whole database and the total follow-up time in year
    DatasetCounts <-data_whole %>%
    dplyr::summarise(total_counts = dplyr::n_distinct(SUBJECT_ID),
                     total_followup_yr = sum(round(as.numeric(COHORT_END_DATE - COHORT_START_DATE)/365.25, 0)))

    write.csv(DatasetCounts, file.path(outputFolder, "DatasetCounts.csv"), row.names = F)

    rm(DatasetCounts)
  }

  #####
  dataMthFile <- file.path(outputFolder, "dataMth.csv")
  dataYrFile <- file.path(outputFolder, "dataYr.csv")

  if (file.exists(dataYrFile)) {
    dataYr <- read.csv(dataYrFile)
    dataMth <- read.csv(dataMthFile)
  } else {


  for(disease_i in unique(outcomesOfInterest$diseaseId)){
    writeLines(paste("Computing incidence for disease cohort:", cohortsToCreate[cohortsToCreate$cohortId==disease_i, "name"], "in Dataset"))
    data_disease <- readRDS(file.path(tmpDir, paste0("diseaseCohort_", disease_i, ".RDS"))) %>%
      dplyr::filter(start_yearMth>=monthStartDate & start_yearMth<=monthEndDate)

    # Calculate the incidence for disease in the data set
    data_disease_incidence <- data_disease %>%
      dplyr::group_by(SUBJECT_ID) %>%
      dplyr::filter(COHORT_START_DATE == min(COHORT_START_DATE)) %>%
      dplyr::ungroup()

    disease_yr <- data_disease_incidence %>%
      dplyr::mutate(calDate = start_year) %>%
      dplyr::group_by(calDate, AGE_YR_GROUP, GENDER) %>%
      dplyr::summarise(d = dplyr::n_distinct(SUBJECT_ID), .groups = 'drop')

    disease_mth <- data_disease_incidence %>%
      dplyr::mutate(calDate = start_yearMth) %>%
      dplyr::group_by(calDate, AGE_MTH_GROUP, GENDER) %>%
      dplyr::summarise(d = dplyr::n_distinct(SUBJECT_ID), .groups = 'drop')

    incYr_i <- whole_yr %>%
      dplyr::left_join(disease_yr, by = c("calDate", "AGE_YR_GROUP", "GENDER"))
    incYr_i[is.na(incYr_i)] <- 0
    incYr_i <- incYr_i %>%
      dplyr::mutate(diseaseId = disease_i,rate = d/w) %>%
      dplyr::select(diseaseId, calDate, ageGroup = AGE_YR_GROUP, Gender = GENDER, totalPatients = w, Counts = d, rate)

    incMth_i <- whole_mth %>%
      dplyr::left_join(disease_mth, by = c("calDate", "AGE_MTH_GROUP", "GENDER"))
    incMth_i[is.na(incMth_i)] <- 0
    incMth_i <- incMth_i %>%
      dplyr::mutate(diseaseId = disease_i,rate= d/w) %>%
      dplyr::select(diseaseId, calDate, ageGroup = AGE_MTH_GROUP, Gender = GENDER, totalPatients = w, Counts = d, rate)

    incYr_disease <- rbind(incYr_disease, incYr_i)
    incMth_disease <- rbind(incMth_disease, incMth_i)

    rm(data_disease_incidence, disease_yr, disease_mth, incYr_i, incMth_i)


    # Calculate the prevalence for disease in the whole cohort
    writeLines(paste("Computing prevalence for disease cohort:", cohortsToCreate[cohortsToCreate$cohortId==disease_i, "name"], "in the whole cohort"))

    disease_yr <- data_disease %>%
      dplyr::mutate(calDate = start_year)%>%
      dplyr::group_by(calDate, AGE_YR_GROUP, GENDER) %>%
      dplyr::summarise(d = dplyr::n_distinct(SUBJECT_ID), .groups = 'drop')

    disease_mth <- data_disease %>%
      dplyr::mutate(calDate = start_yearMth)%>%
      dplyr::group_by(calDate, AGE_MTH_GROUP, GENDER) %>%
      dplyr::summarise(d = dplyr::n_distinct(SUBJECT_ID), .groups = 'drop')

    prevYr_i <- whole_yr %>%
      dplyr::left_join(disease_yr, by = c("calDate", "AGE_YR_GROUP", "GENDER"))
    prevYr_i[is.na(prevYr_i)] <- 0
    prevYr_i <- prevYr_i %>%
      dplyr::mutate(diseaseId = disease_i,rate = d/w) %>%
      dplyr::select(diseaseId, calDate, ageGroup = AGE_YR_GROUP, Gender = GENDER, totalPatients = w, Counts = d, rate)

    prevMth_i <- whole_mth %>%
      dplyr::left_join(disease_mth, by = c("calDate", "AGE_MTH_GROUP", "GENDER"))
    prevMth_i[is.na(prevMth_i)] <- 0
    prevMth_i <- prevMth_i %>%
      dplyr::mutate(diseaseId = disease_i,rate = d/w) %>%
      dplyr::select(diseaseId, calDate, ageGroup = AGE_MTH_GROUP, Gender = GENDER, totalPatients = w, Counts = d, rate)

    prevYr_disease <- rbind(prevYr_disease, prevYr_i)
    prevMth_disease <- rbind(prevMth_disease, prevMth_i)

    remove(prevYr_i, prevMth_i)


    for(drug_j in outcomesOfInterest[outcomesOfInterest$diseaseId==disease_i, "drugId"]){

      data_drug <- readRDS(file.path(tmpDir, paste0("drugCohort_", drug_j, ".RDS"))) %>%
        dplyr::filter(start_yearMth>=monthStartDate & start_yearMth<=monthEndDate)

      if(!drug_j%in%unique(prevYr_drug$drugId)){
      # Calculate the prevalence for drugs in the whole cohort
      writeLines(paste("Computing prevalence for drug cohort:", cohortsToCreate[cohortsToCreate$cohortId==drug_j, "name"], "in the whole cohort"))

      drug_yr <- data_drug%>%
        dplyr::mutate(calDate = start_year)%>%
        dplyr::group_by(calDate, AGE_YR_GROUP, GENDER) %>%
        dplyr::summarise(d = dplyr::n_distinct(SUBJECT_ID), .groups = 'drop')

      drug_mth <- data_drug %>%
        dplyr::mutate(calDate = start_yearMth)%>%
        dplyr::group_by(calDate, AGE_MTH_GROUP, GENDER) %>%
        dplyr::summarise(d = dplyr::n_distinct(SUBJECT_ID), .groups = 'drop')

      prevYr_j <- whole_yr %>%
        dplyr::left_join(drug_yr, by = c("calDate", "AGE_YR_GROUP", "GENDER"))
      prevYr_j[is.na(prevYr_j)] <- 0
      prevYr_j <- prevYr_j %>%
        dplyr::mutate(drugId = drug_j, rate = d/w) %>%
        dplyr::select(drugId, calDate, ageGroup = AGE_YR_GROUP, Gender = GENDER, totalPatients = w, Counts = d, rate)

      prevMth_j <- whole_mth %>%
        dplyr::left_join(drug_mth, by = c("calDate", "AGE_MTH_GROUP", "GENDER"))
      prevMth_j[is.na(prevMth_j)] <- 0
      prevMth_j <- prevMth_j %>%
        dplyr::mutate(drugId = drug_j, rate = d/w) %>%
        dplyr::select(drugId, calDate, ageGroup = AGE_MTH_GROUP, Gender = GENDER, totalPatients = w, Counts = d, rate)

      prevYr_drug <- rbind(prevYr_drug, prevYr_j)
      prevMth_drug <- rbind(prevMth_drug, prevMth_j)

      rm(drug_yr, drug_mth, prevYr_j, prevMth_j)

      # Calculate the incidence for drug in the whole cohort
      writeLines(paste("Computing incidence for drug cohort:", cohortsToCreate[cohortsToCreate$cohortId==drug_j, "name"], "in the whole cohort"))
      data_drug_incidence <- data_drug %>%
        dplyr::group_by(SUBJECT_ID) %>%
        dplyr::filter(COHORT_START_DATE == min(COHORT_START_DATE)) %>%
        dplyr::ungroup()

      drug_yr_in <- data_drug_incidence %>%
        dplyr::mutate(calDate = start_year) %>%
        dplyr::group_by(calDate, AGE_YR_GROUP, GENDER) %>%
        dplyr::summarise(d = dplyr::n_distinct(SUBJECT_ID), .groups = 'drop')

      drug_mth_in <- data_drug_incidence %>%
        dplyr::mutate(calDate = start_yearMth) %>%
        dplyr::group_by(calDate, AGE_MTH_GROUP, GENDER) %>%
        dplyr::summarise(d = dplyr::n_distinct(SUBJECT_ID), .groups = 'drop')

      incYr_i <- whole_yr %>%
        dplyr::left_join(drug_yr_in, by = c("calDate", "AGE_YR_GROUP", "GENDER"))
      incYr_i[is.na(incYr_i)] <- 0
      incYr_i <- incYr_i %>%
        dplyr::mutate(drugId = drug_j,rate = d/w) %>%
        dplyr::select(drugId, calDate, ageGroup = AGE_YR_GROUP, Gender = GENDER, totalPatients = w, Counts = d, rate)

      incMth_i <- whole_mth %>%
        dplyr::left_join(drug_mth_in, by = c("calDate", "AGE_MTH_GROUP", "GENDER"))
      incMth_i[is.na(incMth_i)] <- 0
      incMth_i <- incMth_i %>%
        dplyr::mutate(drugId = drug_j,rate= d/w) %>%
        dplyr::select(drugId, calDate, ageGroup = AGE_MTH_GROUP, Gender = GENDER, totalPatients = w, Counts = d, rate)

      incYr_drug <- rbind(incYr_drug, incYr_i)
      incMth_drug <- rbind(incMth_drug, incMth_i)

      rm(data_drug_incidence, drug_yr_in, drug_mth_in, incYr_i, incMth_i)
    }


      # Calculate the prevalence for drug in disease cohort
      writeLines(paste("Computing prevalence for drug cohort", cohortsToCreate[cohortsToCreate$cohortId==drug_j, "name"], "in disease cohort", cohortsToCreate[cohortsToCreate$cohortId==disease_i, "name"]))

      data_drug_include_yr <- data_drug %>%
        dplyr::inner_join(data_disease %>% dplyr::select(SUBJECT_ID, start_year) %>% unique(), by = c("SUBJECT_ID", "start_year"))

      drug_yr <- data_drug_include_yr %>%
        dplyr::select(SUBJECT_ID, calDate = start_year, AGE_YR_GROUP, GENDER) %>%
        dplyr::group_by(calDate, AGE_YR_GROUP, GENDER) %>%
        dplyr::summarise(n = dplyr::n_distinct(SUBJECT_ID), .groups = "drop")

      data_drug_include_mth <- data_drug %>%
        dplyr::inner_join(data_disease %>% dplyr::select(SUBJECT_ID, start_yearMth) %>% unique(), by = c("SUBJECT_ID", "start_yearMth"))

      drug_mth <- data_drug_include_mth %>%
        dplyr::select(SUBJECT_ID, calDate = start_yearMth, AGE_MTH_GROUP, GENDER) %>%
        dplyr::group_by(calDate, AGE_MTH_GROUP, GENDER) %>%
        dplyr::summarise(n = dplyr::n_distinct(SUBJECT_ID), .groups = "drop")

      prevYr_ij <- disease_yr %>%
        dplyr::left_join(drug_yr, by = c("calDate", "AGE_YR_GROUP", "GENDER"))
      prevYr_ij[is.na(prevYr_ij)] <- 0
      prevYr_ij <- prevYr_ij %>%
        dplyr::mutate(diseaseId = disease_i, drugId = drug_j, rate = n/d) %>%
        dplyr::select(diseaseId, drugId, calDate, ageGroup = AGE_YR_GROUP, Gender = GENDER, totalPatients = d, Counts = n, rate)

      prevMth_ij <- disease_mth %>%
        dplyr::left_join(drug_mth, by = c("calDate", "AGE_MTH_GROUP", "GENDER"))
      prevMth_ij[is.na(prevMth_ij)] <- 0
      prevMth_ij <- prevMth_ij %>%
        dplyr::mutate(diseaseId = disease_i, drugId = drug_j, rate = n/d) %>%
        dplyr::select(diseaseId, drugId, calDate, ageGroup = AGE_MTH_GROUP, Gender = GENDER, totalPatients = d, Counts = n,  rate)

      prevYr <- rbind(prevYr, prevYr_ij)
      prevMth <- rbind(prevMth, prevMth_ij)

      rm(data_drug_include_yr, data_drug_include_mth, drug_yr, drug_mth, prevYr_ij, prevMth_ij)
      ###
      # Calculate the incidence for drug in disease cohort
      writeLines(paste("Computing incidence for drug cohort", cohortsToCreate[cohortsToCreate$cohortId==drug_j, "name"], "in disease cohort", cohortsToCreate[cohortsToCreate$cohortId==disease_i, "name"]))

      data_drug_incidence <- data_drug %>%
        dplyr::group_by(SUBJECT_ID) %>%
        dplyr::filter(COHORT_START_DATE == min(COHORT_START_DATE)) %>%
        dplyr::ungroup()

      data_drug_include_yr_inc <- data_drug_incidence %>%
        dplyr::inner_join(data_disease %>% dplyr::select(SUBJECT_ID, start_year) %>% unique(), by = c("SUBJECT_ID", "start_year"))

      drug_yr_inc <- data_drug_include_yr_inc %>%
        dplyr::select(SUBJECT_ID, calDate = start_year, AGE_YR_GROUP, GENDER) %>%
        dplyr::group_by(calDate, AGE_YR_GROUP, GENDER) %>%
        dplyr::summarise(n = dplyr::n_distinct(SUBJECT_ID), .groups = "drop")

      data_drug_include_mth_inc <- data_drug_incidence %>%
        dplyr::inner_join(data_disease %>% dplyr::select(SUBJECT_ID, start_yearMth) %>% unique(), by = c("SUBJECT_ID", "start_yearMth"))

      drug_mth_inc <- data_drug_include_mth_inc %>%
        dplyr::select(SUBJECT_ID, calDate = start_yearMth, AGE_MTH_GROUP, GENDER) %>%
        dplyr::group_by(calDate, AGE_MTH_GROUP, GENDER) %>%
        dplyr::summarise(n = dplyr::n_distinct(SUBJECT_ID), .groups = "drop")

      incYr_ij <- disease_yr %>%
        dplyr::left_join(drug_yr_inc, by = c("calDate", "AGE_YR_GROUP", "GENDER"))
      incYr_ij[is.na(incYr_ij)] <- 0
      incYr_ij <- incYr_ij %>%
        dplyr::mutate(diseaseId = disease_i, drugId = drug_j, rate = n/d) %>%
        dplyr::select(diseaseId, drugId, calDate, ageGroup = AGE_YR_GROUP, Gender = GENDER, totalPatients = d, Counts = n, rate)

      incMth_ij <- disease_mth %>%
        dplyr::left_join(drug_mth_inc, by = c("calDate", "AGE_MTH_GROUP", "GENDER"))
      incMth_ij[is.na(incMth_ij)] <- 0
      incMth_ij <- incMth_ij %>%
        dplyr::mutate(diseaseId = disease_i, drugId = drug_j, rate = n/d) %>%
        dplyr::select(diseaseId, drugId, calDate, ageGroup = AGE_MTH_GROUP, Gender = GENDER, totalPatients = d, Counts = n,  rate)

      incYr <- rbind(incYr, incYr_ij)
      incMth <- rbind(incMth, incMth_ij)

      rm(data_drug, data_drug_include_yr_inc, data_drug_include_mth_inc, drug_yr_inc, drug_mth_inc, incYr_ij, incMth_ij)
    }

    rm(data_disease, disease_yr, disease_mth)
  }


  ## export output in csv
  incYr_disease <- formatOutput(incYr_disease, databaseName)
  incMth_disease <- formatOutput(incMth_disease, databaseName)
  prevYr_disease <- formatOutput(prevYr_disease, databaseName)
  prevMth_disease <- formatOutput(prevMth_disease, databaseName)
  incYr_drug <- formatOutput(incYr_drug, databaseName)
  incMth_drug <- formatOutput(incMth_drug, databaseName)
  prevYr_drug <- formatOutput(prevYr_drug, databaseName)
  prevMth_drug <- formatOutput(prevMth_drug, databaseName)
  prevYr <- formatOutput(prevYr, databaseName)
  prevMth <- formatOutput(prevMth, databaseName)
  incYr <- formatOutput(incYr, databaseName)
  incMth <- formatOutput(incMth, databaseName)

  dataYr <- dplyr::bind_rows(incYr_disease %>% dplyr::mutate(analysis = "Incidence of Disease"),
                             prevYr_disease %>% dplyr::mutate(analysis = "Prevalence of Disease"),
                             incYr_drug %>% dplyr::mutate(analysis = "Incidence of Drug"),
                             prevYr_drug %>% dplyr::mutate(analysis = "Prevalence of Drug"),
                             prevYr %>% dplyr::mutate(analysis = "Prevalence of Drug in Disease Cohort"),
                             incYr %>% dplyr::mutate(analysis = "incidence of Drug in Disease Cohort"))
  dataYr <- dataYr %>% dplyr::mutate(diseaseId = ifelse(diseaseId==9999999, "Nil", diseaseId),
                                     drugId = ifelse(drugId==9999999, "Nil", drugId))

  dataMth <- dplyr::bind_rows(incMth_disease %>% dplyr::mutate(analysis = "Incidence of Disease"),
                              prevMth_disease %>% dplyr::mutate(analysis = "Prevalence of Disease"),
                              incMth_drug %>% dplyr::mutate(analysis = "Incidence of Drug"),
                              prevMth_drug %>% dplyr::mutate(analysis = "Prevalence of Drug"),
                              prevMth %>%dplyr:: mutate(analysis = "Prevalence of Drug in Disease Cohort"),
                              incMth %>%dplyr:: mutate(analysis = "incidence of Drug in Disease Cohort"))
  dataMth <- dataMth %>% dplyr::mutate(diseaseId = ifelse(diseaseId==9999999, "Nil", diseaseId),
                                       drugId = ifelse(drugId==9999999, "Nil", drugId))

  rm(incYr_disease, incMth_disease, prevYr_disease, prevMth_disease, incYr_drug, incMth_drug, prevYr_drug, prevMth_drug, prevYr, prevMth, incYr, incMth)

  write.csv(dataYr, file.path(outputFolder, "dataYr.csv"), row.names = F)
  write.csv(dataMth, file.path(outputFolder, "dataMth.csv"), row.names = F)

  }


  ##export fentanyl dose / prescription
  data_fentanyl_dose <- readRDS(file.path(tmpDir, paste0("drugCohort_", "1239", ".RDS"))) %>%
    dplyr::filter(start_yearMth>=monthStartDate & start_yearMth<=monthEndDate)

  drug_fentanyl_dose <- data_fentanyl_dose%>%
    dplyr::mutate(calDate = start_year)%>%
    dplyr::group_by(calDate, AGE_YR_GROUP, GENDER) %>%
    dplyr::summarise(w = dplyr::n_distinct(SUBJECT_ID), .groups = 'drop')
##
  data_fentanyl_dose_1 <- readRDS(file.path(tmpDir, paste0("drugCohort_", "1284", ".RDS"))) %>%
    dplyr::filter(start_yearMth>=monthStartDate & start_yearMth<=monthEndDate)

  drug_fentanyl_dose_1 <- data_fentanyl_dose_1%>%
    dplyr::mutate(calDate = start_year)%>%
    dplyr::group_by(calDate, AGE_YR_GROUP, GENDER) %>%
    dplyr::summarise(w1 = dplyr::n_distinct(dplyr::row_number()), .groups = 'drop')
##
  data_fentanyl_dose_2 <- readRDS(file.path(tmpDir, paste0("drugCohort_", "1285", ".RDS"))) %>%
    dplyr::filter(start_yearMth>=monthStartDate & start_yearMth<=monthEndDate)

  drug_fentanyl_dose_2 <- data_fentanyl_dose_2%>%
    dplyr::mutate(calDate = start_year)%>%
    dplyr::group_by(calDate, AGE_YR_GROUP, GENDER) %>%
    dplyr::summarise(w2 = dplyr::n_distinct(dplyr::row_number()), .groups = 'drop')
##
  data_fentanyl_dose_3 <- readRDS(file.path(tmpDir, paste0("drugCohort_", "1286", ".RDS"))) %>%
    dplyr::filter(start_yearMth>=monthStartDate & start_yearMth<=monthEndDate)

  drug_fentanyl_dose_3 <- data_fentanyl_dose_3%>%
    dplyr::mutate(calDate = start_year)%>%
    dplyr::group_by(calDate, AGE_YR_GROUP, GENDER) %>%
    dplyr::summarise(w3 = dplyr::n_distinct(dplyr::row_number()), .groups = 'drop')
##
  data_fentanyl_dose_4 <- readRDS(file.path(tmpDir, paste0("drugCohort_", "1287", ".RDS"))) %>%
    dplyr::filter(start_yearMth>=monthStartDate & start_yearMth<=monthEndDate)

  drug_fentanyl_dose_4 <- data_fentanyl_dose_4%>%
    dplyr::mutate(calDate = start_year)%>%
    dplyr::group_by(calDate, AGE_YR_GROUP, GENDER) %>%
    dplyr::summarise(w4 = dplyr::n_distinct(dplyr::row_number()), .groups = 'drop')
##
  data_fentanyl_dose_5 <- readRDS(file.path(tmpDir, paste0("drugCohort_", "1288", ".RDS"))) %>%
    dplyr::filter(start_yearMth>=monthStartDate & start_yearMth<=monthEndDate)

  drug_fentanyl_dose_5 <- data_fentanyl_dose_5%>%
    dplyr::mutate(calDate = start_year)%>%
    dplyr::group_by(calDate, AGE_YR_GROUP, GENDER) %>%
    dplyr::summarise(w5 = dplyr::n_distinct(dplyr::row_number()), .groups = 'drop')
##
  data_fentanyl_dose_6 <- readRDS(file.path(tmpDir, paste0("drugCohort_", "1346", ".RDS"))) %>%
    dplyr::filter(start_yearMth>=monthStartDate & start_yearMth<=monthEndDate)

  drug_fentanyl_dose_6 <- data_fentanyl_dose_5%>%
    dplyr::mutate(calDate = start_year)%>%
    dplyr::group_by(calDate, AGE_YR_GROUP, GENDER) %>%
    dplyr::summarise(w6 = dplyr::n_distinct(dplyr::row_number()), .groups = 'drop')
##

  dose_cal <- drug_fentanyl_dose %>%
    dplyr::left_join(drug_fentanyl_dose_1, by = c("calDate", "AGE_YR_GROUP", "GENDER")) %>%
    dplyr::left_join(drug_fentanyl_dose_2, by = c("calDate", "AGE_YR_GROUP", "GENDER")) %>%
    dplyr::left_join(drug_fentanyl_dose_3, by = c("calDate", "AGE_YR_GROUP", "GENDER")) %>%
    dplyr::left_join(drug_fentanyl_dose_4, by = c("calDate", "AGE_YR_GROUP", "GENDER")) %>%
    dplyr::left_join(drug_fentanyl_dose_5, by = c("calDate", "AGE_YR_GROUP", "GENDER")) %>%
    dplyr::left_join(drug_fentanyl_dose_6, by = c("calDate", "AGE_YR_GROUP", "GENDER"))
  dose_cal[is.na(dose_cal)] <- 0
  dose_cal <- dose_cal %>%
    dplyr::mutate(total_counts = (w1+w2+w3+w4+w5+w6), rate = (12*w1+25*w2+37.5*w3+50*w4+75*w5+100*w6)/w) %>%
    dplyr::select(calDate = calDate, ageGroup = AGE_YR_GROUP, Gender = GENDER, totalPatients = w, totalCounts = total_counts, Counts_12 = w1, Counts_25 = w2, Counts_375 = w3, Counts_50 = w4, Counts_75 = w5, Counts_100 = w6, rate)

  rm(drug_fentanyl_dose,data_fentanyl_dose, drug_fentanyl_dose_1,data_fentanyl_dose_1, drug_fentanyl_dose_2,data_fentanyl_dose_2,
     drug_fentanyl_dose_3,data_fentanyl_dose_3, drug_fentanyl_dose_4,data_fentanyl_dose_4, drug_fentanyl_dose_5,data_fentanyl_dose_5, drug_fentanyl_dose_6,data_fentanyl_dose_6)

  write.csv(dose_cal, file.path(outputFolder, "dose_fentanyl.csv"), row.names = F)

  ##export fentanyl demographics (indications and psychiatric disorders)

  demo_fentanyl <- data.frame()

  data_fentanyl <- readRDS(file.path(tmpDir, paste0("drugCohort_", "1239", ".RDS"))) %>%
    dplyr::filter(start_yearMth>=monthStartDate & start_yearMth<=monthEndDate)

  drug_fentanyl <- data_fentanyl%>%
    dplyr::group_by(AGE_YR_GROUP, GENDER) %>%
    dplyr::summarise(w = dplyr::n_distinct(SUBJECT_ID), .groups = 'drop')

  for(disease_i in unique(outcomesOfInterest$diseaseId)){

    data_disease <- readRDS(file.path(tmpDir, paste0("diseaseCohort_", disease_i, ".RDS"))) %>%
      dplyr::filter(start_yearMth>=monthStartDate & start_yearMth<=monthEndDate)

    disease_yr <- data_disease %>%
      dplyr::group_by(AGE_YR_GROUP, GENDER) %>%
      dplyr::summarise(d = dplyr::n_distinct(SUBJECT_ID), .groups = 'drop')


    prevYr_i <- drug_fentanyl %>%
      dplyr::left_join(disease_yr, by = c("AGE_YR_GROUP", "GENDER"))
    prevYr_i[is.na(prevYr_i)] <- 0
    prevYr_i <- prevYr_i %>%
      dplyr::mutate(diseaseId = disease_i,rate = d/w) %>%
      dplyr::select(diseaseId, ageGroup = AGE_YR_GROUP, Gender = GENDER, totalPatients = w, Counts = d, rate)

    demo_fentanyl <- rbind(demo_fentanyl, prevYr_i)

    rm(prevYr_i)

    }

  rm(data_fentanyl,drug_fentanyl)

  write.csv(demo_fentanyl, file.path(outputFolder, "demo_fentanyl.csv"), row.names = F)

  ## mask the counts if counts < minCellCount
  if(minCellCount>0){
    dataYrMasked <- maskCount(dataYr, minCellCount)
    dataMthMasked <- maskCount(dataMth, minCellCount)

    write.csv(dataYrMasked, file.path(outputFolder, "dataYrMasked.csv"), row.names = F)
    write.csv(dataMthMasked, file.path(outputFolder, "dataMthMasked.csv"), row.names = F)
  }


  ## export output in RDS for Shiny Dashboard
  if (!file.exists(file.path(outputFolder, "Shiny")))
    dir.create(file.path(outputFolder, "Shiny"))


  ## aggregate results by gender and age group
  dataYrShiny <- rbind(dataYr,
                       dataYr %>%
                         dplyr::group_by(database, diseaseId, diseaseCohortName, drugId, drugCohortName, calDate, ageGroup, analysis, Gender = 'All') %>%
                         dplyr::summarise(totalPatients = sum(totalPatients),
                                          Counts = sum(Counts), .groups = 'drop') %>%
                         dplyr::mutate(rate = Counts/totalPatients),
                       dataYr %>%
                         dplyr::group_by(database, diseaseId, diseaseCohortName, drugId, drugCohortName, calDate, Gender, analysis, ageGroup = 'All') %>%
                         dplyr::summarise(totalPatients = sum(totalPatients),
                                          Counts = sum(Counts), .groups = 'drop') %>%
                         dplyr::mutate(rate = Counts/totalPatients),
                       dataYr %>%
                         dplyr::group_by(database, diseaseId, diseaseCohortName, drugId, drugCohortName, calDate, analysis, ageGroup = 'All', Gender = 'All') %>%
                         dplyr::summarise(totalPatients = sum(totalPatients),
                                          Counts = sum(Counts), .groups = 'drop') %>%
                         dplyr::mutate(rate = Counts/totalPatients))
  dataMthShiny <- rbind(dataMth,
                        dataMth %>%
                          dplyr::group_by(database, diseaseId, diseaseCohortName, drugId, drugCohortName, calDate, ageGroup, analysis, Gender = 'All') %>%
                          dplyr::summarise(totalPatients = sum(totalPatients),
                                           Counts = sum(Counts),.groups = 'drop') %>%
                          dplyr::mutate(rate = Counts/totalPatients),
                        dataMth %>%
                          dplyr::group_by(database, diseaseId, diseaseCohortName, drugId, drugCohortName, calDate, Gender, analysis, ageGroup = 'All') %>%
                          dplyr::summarise(totalPatients = sum(totalPatients),
                                           Counts = sum(Counts),.groups = 'drop') %>%
                          dplyr::mutate(rate = Counts/totalPatients),
                        dataMth %>%
                          dplyr::group_by(database, diseaseId, diseaseCohortName, drugId, drugCohortName, calDate, analysis, ageGroup = 'All', Gender = 'All') %>%
                          dplyr::summarise(totalPatients = sum(totalPatients),
                                           Counts = sum(Counts),.groups = 'drop') %>%
                          dplyr::mutate(rate = Counts/totalPatients))


  ## mask the counts if counts < minCellCount
  if(minCellCount>0){
    dataYrShinyMasked <- maskCount(dataYrShiny, minCellCount)
    dataMthShinyMasked <- maskCount(dataMthShiny, minCellCount)

    saveRDS(dataYrShinyMasked, file.path(outputFolder, "Shiny", "dataByYear.RDS"))
    saveRDS(dataMthShinyMasked, file.path(outputFolder, "Shiny", "dataByMonth.RDS"))

  } else {
    saveRDS(dataYrShiny, file.path(outputFolder, "Shiny", "dataByYear.RDS"))
    saveRDS(dataMthShiny, file.path(outputFolder, "Shiny", "dataByMonth.RDS"))
  }

}

formatOutput <- function(dataRate, databaseName){

  cohortsToCreate <- read.csv(file.path("inst", "settings", "CohortsToCreate.csv"))
  `%>%` <- magrittr::`%>%`
  if("drugId"%in%colnames(dataRate)==FALSE){
    dataRate <- dataRate %>%
      dplyr::inner_join(cohortsToCreate %>% dplyr::select(cohortId, diseaseCohortName = name), by = c("diseaseId" = "cohortId")) %>%
      dplyr::mutate(database = databaseName, drugId = 9999999, drugCohortName = "Nil") %>%
      dplyr::select(database, diseaseId, diseaseCohortName, drugId, drugCohortName, calDate, ageGroup, Gender,
                    totalPatients, Counts, rate)
    #levels(dataRate$ageGroup) <- c("<18", "18-24", "25-44", "45-64", "65-74", "75-84", ">=85")
    levels(dataRate$ageGroup) <- c("<18", "18-24", "25-34", "35-44", "45-54", "55-64", ">=65")
  }
  else if("diseaseId"%in%colnames(dataRate)==FALSE){
    dataRate <- dataRate %>%
      dplyr::inner_join(cohortsToCreate %>% dplyr::select(cohortId, drugCohortName = name), by = c("drugId" = "cohortId")) %>%
      dplyr::mutate(database = databaseName, diseaseId = 9999999, diseaseCohortName = "Nil") %>%
      dplyr::select(database, diseaseId, diseaseCohortName, drugId, drugCohortName, calDate, ageGroup, Gender,
                    totalPatients, Counts, rate)
    levels(dataRate$ageGroup) <- c("<18", "18-24", "25-34", "35-44", "45-54", "55-64", ">=65")
  }
  else {
    dataRate <- dataRate %>%
      dplyr::inner_join(cohortsToCreate %>% dplyr::select(cohortId, diseaseCohortName = name), by = c("diseaseId" = "cohortId")) %>%
      dplyr::inner_join(cohortsToCreate %>% dplyr::select(cohortId, drugCohortName = name), by = c("drugId" = "cohortId")) %>%
      dplyr::mutate(database = databaseName) %>%
      dplyr::select(database, diseaseId, diseaseCohortName, drugId, drugCohortName, calDate, ageGroup, Gender,
                    totalPatients, Counts, rate)
    levels(dataRate$ageGroup) <- c("<18", "18-24", "25-34", "35-44", "45-54", "55-64", ">=65")
  }
  return(dataRate)
}


maskCount <- function(dataRate, minCellCount) {
  `%>%` <- magrittr::`%>%`
  dataRate <- dataRate%>%
    dplyr::mutate(totalPatients = ifelse(totalPatients<minCellCount, paste0("<", minCellCount), totalPatients),
                  Counts = ifelse(totalPatients==paste0("<", minCellCount), 0, Counts),
                  rate = ifelse(totalPatients==paste0("<", minCellCount), 0, rate)) %>%
    dplyr::mutate(Counts = ifelse(Counts>0&Counts<minCellCount, paste0("<", minCellCount), Counts),
                  rate = ifelse(Counts==paste0("<", minCellCount), 0, rate))
}
