# Loading library
library(dplyr)
library(readxl)

# Read dataset
df <- read_excel("dataset.xlsx")

# Remove usefulness columns
df$sira_nomresi <- NULL
# df$il <- NULL
# df$universitet <- NULL

# Remove usefulness rows
df <- subset(df, qrup!=5)

# df <- df %>%
#   mutate(form = dplyr::recode(
#     tehsil_formasi, 
#     "Əyani" = "Ə", 
#     "Qiyabi" = "Q")
#   )

# Replace variables
df$odenishsiz[df$odenishsiz == "0"] <- NA

# Shiny ----

# Loading Shiny library
library(shiny)
library(shinythemes)
library(shinyWidgets)


# UI  ----

ui <- fluidPage(
  
  div(id = "header",
      h2("EduCar - Onlayn ixtisas seçimi"),
      h5("Ətraflı məlumat və layihəyə dəstək üçün GitHub",
         a(href = "https://github.com/hasanaliyev/educar",
           "reposu.")
      ),
      # strong(
      #     span("Created by "),
      #     a("Hasanagha Aliyev", href = "https://www.linkedin.com/in/hasanaliyev/"),
      #     HTML("&bull;"),
      #     span("Code"),
      #     a("on GitHub", href = "https://github.com/hasanaliyev/educar"),
      #     HTML("&bull;"),
      #     a("More apps", href = "https://github.com/hasanaliyev"), "by EduCar")
  ),
  
  #theme = shinytheme("flatly"),
  
  
  sidebarLayout(#fluid = TRUE, 
    
    # Create a new Row in the UI for selectInputs
    sidebarPanel(
      
      pickerInput(inputId = "year",
                  label = "Qəbul ili", 
                  choices = c(#"Qəbul ili",
                    unique(as.character(df$il))),
                  selected = "2018",
                  multiple = TRUE
      ),
      
      radioGroupButtons("group", 
                        "İxtisas qrupu", 
                        c("I" = 1, 
                          "II" = 2,
                          "III" = 3,
                          "IV" = 4),
                        justified = TRUE,
                        status = "primary"
                        #,selected = 1
                        #,inline   = T
      ),
      
      radioGroupButtons("lang", 
                        "Tədris dili", 
                        c("az" = "az", 
                          "eng" = "ing",
                          "rus" = "rus",
                          "türk" = "türk"
                        ),
                        justified = TRUE,
                        #status = "primary"
                        #,selected = 1
                        #,inline   = T
      ),
      
      materialSwitch(inputId = "forma",
                     label = "Qiyabi",
                     value = FALSE,
                     right = TRUE,
                     status = "primary"),
      
      materialSwitch(inputId = "unpaid",
                     label = "Ödənişsiz",
                     value = FALSE,
                     right = TRUE,
                     #size = "mini",
                     status = "danger"),
      
      selectInput("university",
                  label = NULL,
                  c("Universitetlər",
                    unique(as.character(df$uni)))),
      
      sliderInput("bal", "Bal",
                  min = 0, max = 700,
                  value = c(200,500),
                  step = 50
                  #,width = '100%'
      ),
      
      selectInput("city",
                  label = NULL,
                  c("Şəhərlər",
                    unique(as.character(df$sheher))),
                  #selected = "Bakı"
      )
      
      ,width = 3),
    
    mainPanel(
      navbarPage(
        title = NULL,
        tabPanel('General',            DT::dataTableOutput("table")),
        tabPanel('BoxPlots',           DT::dataTableOutput('box'))
      ))
  ))


# Server ----

server <- function(input, output) {
  
  # Filter data based on selections
  output$table <- DT::renderDataTable(DT::datatable({
    
    data <- df
    
    # il
    #if (input$year != "Tədris ili") 
    {data <- data[data$il == input$year,]}
    
    # Qrup
    {data <- data[data$qrup == input$group,]}
    
    # Odenish
    if (input$unpaid == TRUE) {
      data <- data %>% filter(odenishsiz != 0)
    }
    
    # Dil
    { data <- data[data$tedris_dili == input$lang,]}
    
    # Tehsil formasi
    if (input$forma == TRUE) {
      data <- data[data$tehsil_formasi == "Qiyabi",]
    }
    
    # Universitet
    if (input$university != "Universitetlər") {
      data <- data[data$uni == input$university,]
    }
    
    # Kechid bali
    { data <- data[data$kechid_bali >= input$bal[1] & 
                   data$kechid_bali <= input$bal[2],]
    }
    
    # Sheher
    if (input$city != "Şəhərlər") {
      data <- data[data$sheher == input$city,]
    }
    
    data
    
    # View
    select(data, uni, ixtisasin_adi, kechid_bali, odenishsiz, tehsil_formasi, tedris_dili)
    
  }, 
  
  # extensions = c('Responsive', 'FixedHeader', 'Buttons'),
  
  colnames = c(#"İD" = 1,
                "uni." = 2,
                "ixtisas" = 3,
                "keçid balı" = 4,
                "odenishsiz" = 5,
                "tehsil forması" = 6,
                "tedris dili" = 7
              ),
  
  options = list(autoWidth = FALSE,
                 fixedHeader = TRUE
                 #,buttons = I('colvis'), dom = 'Bfrtip'
                 #,paging = FALSE
                 #,columnDefs = list(list(width = '400px', targets = "_all")))
  )
  )
  )
}

# Create Shiny app ----
shinyApp(ui, server)


