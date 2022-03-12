CERVELLOPrevalence
=================
<img src="https://img.shields.io/badge/Study%20Status-Started-blue.svg" alt="Study Status: Started">

- Analytics use case(s): **Characterization**
- Study type: **Clinical Application**
- Tags: **OHDSI, psychiatryWG, mental-health, COVID-19**
- Study lead: **Hao Luo, PhD, The University of Hong Kong** // 
              **Yi Chai, MSc, The University of Hong Kong** 
- Study lead forums tag:  **[HaoLuo](https://github.com/haoluo429)**, **[YiChai](https://github.com/YiChai18)**
- Study start date: **1st September, 2021**
- Study end date: **-**
- Protocol: **[PDF](https://github.com/YiChai18/CERVELLOPrevalence/tree/main/Protocol)**
- Results explorer: **-**


The mental health effects of COVID-19 may vary across populations. To ensure limited mental health resources are efficiently targeted to crucial areas of mental health problems, high-quality data on the short-, medium- and long-term mental health effects of the COVID-19 pandemic across the whole-population and vulnerable groups are needed. In this project, we aim to examine temporal trends of the incidence and prevalence of mental health and neurodevelopmental disorder diagnosis before and during the pandemic; the prevalence of eight types of psychotropic drug prescribing before and during the pandemic; and the prevalence of psychotropic drug prescribing in people with mental health issues and with neurodevelopmental disorders by condition. The incidence and prevalence will be stratified by year, month, sex, and age groups. 

## Requirements
- A database in [Common Data Model version 5](https://github.com/OHDSI/CommonDataModel) in one of these platforms: SQL Server, Oracle, PostgreSQL, IBM Netezza, Apache Impala, Amazon RedShift, or Microsoft APS.
- R version 3.6.0 or newer
- On Windows: [RTools](http://cran.r-project.org/bin/windows/Rtools/)
- [Java](http://java.com)
- Suggested: 25 GB of free disk space

## How to run the study
1. Follow [these instructions](https://ohdsi.github.io/Hades/index.html) on how to set up the R environment on Windows.
2. Launch `CERVELLOPrevalence.Rproj` in the study package folder. 
3. Use the following code to initiate the project:

	```r
	renv::init()
	```
4. Use the following code to install all the dependencies:
 
	```r
	renv::restore()
	```
5. In RStudio, build the pakcage by clicking "Install and Restart" under the 'Build' tab. If there is no error message, the package is built successfully.
6. Download DatabaseConnector JDBC driver according to your own platform [here](https://ohdsi.github.io/DatabaseConnectorJars/) if you have not done so. 
7. Once installed, open `extras/CodeToRun.R` or use the code below to run the package on your database. Please customise the parameters before executing the package. For more information on how to set these parameters, please refer to [DatabaseConnector](http://ohdsi.github.io/DatabaseConnector/) and  `R/Main.R`

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
                  pathToDriver = 's:/jdbcDrivers')

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
8. View your results in the Shiny App
	```r
        # Launch Shiny
        shinyDataFolder <- file.path(getwd(), outputFolder, "Shiny")
        .GlobalEnv$shinySettings <- list(dataFolder = shinyDataFolder)
        shinyFolder <- file.path("inst", "shiny")
        shiny::runApp(shinyFolder)
	```
9. Share the results with the study lead  
   Use the following code to compress your results:
 
	```r
	DatabaseConnector::createZipFile(zipFile ="Results_<DatabaseId>.zip",files = outputFolder)
	```
Send the file `Export/Results_<DatabaseId>.zip`in your folder to the study coordinator [Yi Chai](mailto:chaiyi18@connect.hku.hk).

## License

The CERVELLOPrevalence package is licensed under Apache License 2.0

## Development
 
CERVELLOPrevalence was developed in ATLAS and R Studio.

## Results

Send the file `Export/Results_<DatabaseId>.zip` in the output folder to the study coordinator [Yi Chai](mailto:chaiyi18@connect.hku.hk).
