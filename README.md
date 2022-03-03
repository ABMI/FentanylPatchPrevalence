CERVELLOPrevalence
=================
<img src="https://img.shields.io/badge/Study%20Status-Started-blue.svg" alt="Study Status: Started">

- Analytics use case(s): **Characterization**
- Study type: **Clinical Application**
- Tags: **OHDSI, psychiatryWG, mental-health, covid-19**
- Study lead: **Hao Luo, PhD, University of Hong Kong** // 
              **Yi Chai, PhD, University of Hong Kong** //
              **Carmen Olga Torre, MSc, IQVIA** 
- Study lead forums tag:  **[HaoLuo]**, **[YiChai18](https://github.com/YiChai18)**, **[CarmenOT](https://forums.ohdsi.org/u/carmenot)**
- Study start date: **21st January, 2021**
- Study end date: **4th March 2022**
- Protocol: **[Word Doc]**

## Research Questions and Objectives:

### Research Questions:
Before and during the pandemic, are threr any changes in
1.	Incidence rates of mental health conditions and of neurodevelopmental disorders 
2.	Proportions of people with visits/contacts for particular mental health conditions and neurodevelopmental disorders
3.	Proportions of people having a psychotropic drug prescribed
4.	Proportions of people having a psychotropic drug prescribed in people with mental health conditions and with neurodevelopmental disorders

### Primary analysis: 
To examine temporal trends of the prevalence of mental health and neurodevelopmental disorder diagnosis and the prevalence of eight types of psychotropic drug use before and during the pandemic. To examine the prevalence of psychotropic drug use in people with mental health issues and with neurodevelopmental disorders by condition. The prevalence will be stratified by year, month, sex and age groups. 

## Requirements
- A database in [Common Data Model version 5](https://github.com/OHDSI/CommonDataModel) in one of these platforms: SQL Server, Oracle, PostgreSQL, IBM Netezza, Apache Impala, Amazon RedShift, or Microsoft APS.
- R version 3.6.0 or newer
- On Windows: [RTools](http://cran.r-project.org/bin/windows/Rtools/)
- [Java](http://java.com)
- Suggested: 25 GB of free disk space

## How to run the study
1. Follow [these instructions](https://ohdsi.github.io/Hades/rSetup.html) for setting up your R environment, including RTools and Java. 
2. Launce `CERVELLOPrevalence.Rproj` in the study package folder. 
3. Use the following code to initiate the project:

	```r
	renv::init()
	```
4. Use the following code to to install all the dependencies:
 
	```r
	renv::restore()
	```
5. In RStudio, build the pakcage by clicking "Install and Restart" under 'Build' tab. If there is no error message, the package is built successfully.
6. Once installed, the study can be executed by modifying and using the code below. For your convenience, this code is also provided under `extras/CodeToRun.R`.

	```r
	library(CERVELLOPrevalence)

        # Maximum number of cores to be used:
        maxCores <- parallel::detectCores()

        # The folder where the study intermediate and result files will be written:
        outputFolder <- file.path("CervelloResults")

        # Details for connecting to the server:
        dbName <- 'MydbName'

        connectionDetails <- DatabaseConnector::createConnectionDetails(
	          dbms = 'postgresql',
                  server = 'myserver',
                  user = 'joe',
                  password = 'secret',
                  pathToDriver = 'S:/jdbcDrivders')


        # The name of the database schema where the CDM data can be found:
        cdmDatabaseSchema<-'CDM_mydb.dbo'

        # The name of the database schema and table where the study-specific cohorts will be instantiated:
        cohortDatabaseSchema <- 'mydb.dbo'
        cohortTable <- "Cevelloprevalence"

        # Some meta-information that will be used by the export function:
        databaseName <- 'MYDATABASE'



        execute(connectionDetails = connectionDetails,
                cdmDatabaseSchema = cdmDatabaseSchema,
                cohortDatabaseSchema = cohortDatabaseSchema,
                cohortTable = cohortTable,
                outputFolder = outputFolder,
                databaseName = databaseName,
                createCohorts = TRUE,
                runPrevalence = TRUE,
                yearStartDate = "2016-01-01",
                yearEndDate = "2021-07-01",
                monthStartDate = "2016-01-01",
                monthEndDate = "2021-07-01",
                maxCores = maxCores,
                minCellCount= 0)
	```
