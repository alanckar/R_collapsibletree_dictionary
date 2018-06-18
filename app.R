library(shiny)
library(collapsibleTree)
library(RODBC)
#dplyer and reader

# List of choices
selectlist <<- list("DEV" = "dev",
                    "QA" = "qa" ,
                    "PROD" = "prd")

# Define UI
ui <- fluidPage(
  
  # Application title
  titlePanel("Google Cloud Data Dictionary"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput("environment",
                  "Bucket:",
                  selectlist
      )
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      collapsibleTreeOutput("treeplot")
    )
  )
)

# Define server logic
server <- function(input, output, session) 
{
  output$treeplot <- renderCollapsibleTree({
    # define connection information
    driver.name <- "SQL Server"
    db.name <- "DW"
    host.name <- "ServerName"
    port <-""
    server.name <-"ServerName"
    user.name <- "AKTestUser"
    pwd <- "test123"
    
    # # full connection string to connect to a DW database
    con.text <- paste("DRIVER=",driver.name,
                      ";Database=",db.name,
                      ";Server=",server.name,
                      ";Port=",port,
                      ";PROTOCOL=TCPIP",
                      ";UID=", user.name,
                      ";PWD=",pwd,sep="")
    
    con1 <- odbcDriverConnect(con.text)
    # 
    
    #datalake_csv <- read.csv("C:\\GCP_DataDictionary\\DataDictionary_Test.csv", header = FALSE, sep = ",", quote = "\"")
    
    sql_query_string <- paste("SELECT TOP 5000 Level01 AS Bucket,
                              Level02 AS Source,
                              Level03 AS SubjectArea ,
                              Level04 as TableName,
                              FileName  as FileName
                              FROM rpt.SplitFileList
                              WHERE Level01 LIKE '%",input$environment,"%'", sep = "")
    
    datalake <- sqlQuery(con1, sql_query_string)
    
    #head(datalake, 1)
    
    odbcCloseAll()
    
    # Convert factor column types with character
    datalake <- data.frame(lapply(datalake, as.character), stringsAsFactors=FALSE)
    
    
    # Replace blank Filenames with NA
    #atalake$FileName[datalake$FileName==""] <- NA
    
    is.na(datalake$FileName[datalake$FileName==""]) <- TRUE
    
    # Ignore data rows with colons as filenames
    datalake$FileName[datalake$FileName == ":"] <- NA
    
    # Check datatypes of all columns
    sapply(datalake, typeof)
    
    if(nrow(datalake) == 0){
      #Do nothing
      print("Empty Result")
      return()
    }
    
    # Plot the collapsibleTree
    return(collapsibleTree(datalake, c("Bucket","Source", "SubjectArea", "TableName", "FileName"), collapsed = TRUE))
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

