#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#install.packages("shiny")
#install.packages("rJava")
#install.packages("dplyr")
#install.packages("stringr")
#install.packages("xlsx")
#install.packages("DT")

library(shiny)
library(rJava)
library(dplyr)
library(xlsx)
library(stringr)
library(DT)

options(shiny.maxRequestSize = 30*1024^2)# Increasing the upload file size limit to 30Mb
options(expressions=500000)

ui <- fluidPage(
   
   # Application title
   titlePanel("Click-City Analysis"),
   
   # Sidebar with a csv upload option
   sidebarLayout(
      sidebarPanel(
        fileInput('InputData', 'Choose a csv file to upload',
                  accept=c('text/csv','.csv'))
      ),
      
      # Show the final output table on main panel
      mainPanel(
        
        # Output: Data file ----
        DT::dataTableOutput("mytable")
    
      )
   )
)

# Define server logic required to do the analysis and display output
server <- function(input, output) 

{
    #reactive function since we want to display a dynamic output based on user-input  
    myData <- reactive({
    inFile <- input$InputData
    if (is.null(inFile)) return(NULL)
    Data_Main <- read.csv(inFile$datapath, header = TRUE, stringsAsFactors = FALSE)
    
    WDir = "C:/Users/perceptive/Dropbox/Clicks_City_Analysis_App"#setting the working directory in a variable
    setwd(paste0(WDir))# Setting the working directory in a dynamic way using paste
    Top_Cities = read.csv("Top_Cities.csv",stringsAsFactors = FALSE)#Loading the city repository
    
    for (a in 1: nrow(Top_Cities))
    {
      Top_Cities$Name[a] = tolower(Top_Cities$Name[a])
    }
    
    for (i in 1 : nrow(Data_Main))
    {
      Data_Main$City[i] = "NA"
      sentence = Data_Main$SearchQuery[i]
      sentence = as.character(sentence)
      sentence = tolower(sentence)
      for (j in 1: nrow(Top_Cities)) 
      {
        if (grepl(Top_Cities$Name[j], sentence, fixed = TRUE) == TRUE)
        {
            Data_Main$City[i] = Top_Cities$Name[j]
        }
        
      }
      
    }
    
    Mapped_Cities = subset(Data_Main, City != "NA")
    Mapped_Cities$City = as.factor(Mapped_Cities$City)
    Mapped_Cities$Impressions = as.numeric(Mapped_Cities$Impressions)
    Mapped_Cities <- Mapped_Cities %>% group_by(City) %>% summarise(Total_Queries = n(), Total_Clicks = sum(Clicks), Total_Impressions = sum(Impressions))
    # Mapped_Cities[order(-Mapped_Cities$Total_Clicks),]
     datatable(Mapped_Cities,rownames = FALSE, options = list(pageLength = 20,
               lengthMenu = c(20, 40, 60, 80, 100),
       order =  list(2, 'desc')
       ))
    })
  
  output$mytable <- DT::renderDataTable({
    myData () 
  })
   
   
}

# Run the application 
shinyApp(ui = ui, server = server)

