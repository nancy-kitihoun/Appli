library(shiny)
library(shinydashboard)



dashboardPage(title = "Détection des fraudes sur les cartes de crédits",
              skin = ("green"),
              dashboardHeader(title = "Détection des fraudes sur les cartes de crédits", titleWidth=800),
              
              dashboardSidebar(
                  sidebarMenu(
                      menuItem("Introduction", tabName = "intro", icon = icon("home")),
                      menuItem("Données", tabName = "datafile", icon = icon("table")),
                      menuItem("Analyse", tabName = "analysis", icon = icon("chart-bar")
                      ),
                      menuItem("Paper", tabName = "paper", icon = icon("file-pdf-o")),
                      hr(),
                      sidebarUserPanel(name = a("Camille AZANGHO",
                                                href = "https://www.univ-orleans.fr/deg/masters/ESA/etu/promo_M22020.html"),
                                       subtitle = "brunellekas@gmail.com"),br(),
                      sidebarUserPanel(name = a("MAKHOKH Lamyae",
                                                href = "https://www.univ-orleans.fr/deg/masters/ESA/etu/promo_M22020.html"),
                                       subtitle = "lamayae1993makhokh@gmail.com"),br(),
                      sidebarUserPanel(name = a("KITIHOUN Nancy",
                                                href = "https://www.univ-orleans.fr/deg/masters/ESA/etu/promo_M22020.html"),
                                       subtitle = "nancy.kitihoun@yahoo.com")
                      
                      
                      
                      
                  ),
                  tags$head(tags$style(HTML(
                      '/*header title font*/
.main-header .logo{
  font-family: "Geogia",Times,"Times New Roman",Serif;
  font-weight:  bold;
  font-size: 24px;
}

/*background color of hearder (logo part)*/
.skin-blue  .main-header .logo{
  background-color: #8a6777;
}

/* change the background color of hearder (logo part) when mouse hover */
 .skin-blue .main-header .logo:hover {
   background-color:orange;
 }
 
 /*background color for remaining part of the header */
 .skin-blue .main-header .navbar {
   background-color: #67818a;
 }
 
 /* main sidebar */
   .skin-blue .main-sidebar {
           background-color:#67818a;
   }
 
 
 /* active sidebar menu item */
 
    .skin-blue .main-sidebar .sidebar-menu .active a{
      background-color: white;
      color:black;
      
    }
    /*  sidebar menuitems */
        .skin-blue .main-sidebar .sider .sidebar-menu a{
                   background-color: orange;
                          color:white;
      
        }
    /* sider menuitems when mouse hover */
    
     .skin-blue .main-sidebar  .sidebar .sidebar-menu a:hover{
        background-color: brown;
        color:white;
        
     }
     /* sidebar toggle button */
     .skin-blue .main-header  .navbar .sidebar-toggle:hover {
       background-color:black;
     }
        
                              ')))
                  
              ),
              dashboardBody(
                  
                  
                  tabItems(
                      # Read data
                      tabItem(tabName = "intro", includeHTML("intro.html")),    
                      
                      
                      
                      tabItem(tabName = "datafile",
                              style = "overflow-y:scroll;",
                              
                              # The id lets us use input$tabset1 on the server to find the current tab
                              height = "250px",
                              tabBox(selected = "Description1",width = 12,
                                     tabPanel("Description1",
                                              box(width = 12, status = "primary",
                                                  title = "detection de fraude sur des cartes bancaires", 
                                                  DT::dataTableOutput("dataTable")
                                              ),
                                              box(width = 6,status = "warning", 
                                                  title=" simple statistique descriptive",
                                                  DT::dataTableOutput("Des1")),
                                              box(width = 6,status = "info",
                                                  "le nombre de valeurs nulles (q_zeros) et son pourcentage (p_zeros)",br(),
                                                  "le nombre de valeurs manquantes (q_na) et son pourcentage (p_na)",br(),
                                                  "le nombre de valeur infinies (q_inf) et son pourcentage (p_inf)",br(),
                                                  
                                                  DT::dataTableOutput("Des2"))),
                                     tabPanel("Description2",
                                              
                                              
                                              box(with=12 , status = "warning",
                                                  
                                                  sidebarLayout(
                                                      sidebarPanel(
                                                          #Definition 1er menu deroulant
                                                          selectInput("variable1", "Variable (axe des x):",
                                                                      list("Time" , "V1" ,"V2", "V3" ,"V4" , "V5" ,"V6","V7" ,"V8" ,
                                                                           "V9","V10","V11" , "V12","V13","V14"  , 
                                                                           "V15", "V16"   , "V17"   , "V18" ,   "V19"  ,  "V20"  ,  "V21"  ,  
                                                                           "V22", "V23", "V24", "V25" , "V26", "V27"  ,  "V28" , "Amount" )),
                                                          
                                                          #Definition du second menu deroulant
                                                          selectInput("variable2", "Variable (axe des y):",
                                                                      list( "V1" ,"V2", "V3" ,"V4" , "V5" ,"V6","V7" ,"V8" ,
                                                                            "V9","V10","V11" , "V12","V13","V14"  , 
                                                                            "V15", "V16"   , "V17"   , "V18" ,   "V19"  ,  "V20"  ,  "V21"  ,  
                                                                            "V22", "V23", "V24", "V25" , "V26", "V27"  ,  "V28" , "Amount","Time"))),
                                                      
                                                      mainPanel(  plotOutput("Grap1", height = "300px")))),
                                              box( status = "info",  plotOutput("Grap2", height = "300px")),
                                              box( title="CorrÃ©lation", plotOutput("tabCor") ,status = "primary")
                                     ),
                                     tabPanel("Un Tout",status = "info",width = 24,
                                              plotOutput("Grap3", width = "100%" ))
                                     
                              )),
                      # Analyse
                      tabItem(tabName="analysis",
                              style = "overflow-y:scroll;",
                              sliderInput("proportion ", "proportion 0", min=0.001, max=0.005, value=0.003,
                                          width = '95%'),
                              
                              tabBox(type = "pills",selected = "SVM",width = 12,
                                     
                                     tabPanel("SVM", 
                                              
                                              box(width = 6, verbatimTextOutput("svm"),status = "primary", title="sortie svm"),
                                              box(width=6,verbatimTextOutput("performence"),status = "primary",title="matrice de confusion")
                                              #box(plotOutput("ROC", height = "300px" ),status = "primary",title="courbe AUC")
                                              
                                              
                                     ),
                                     tabPanel("Abre de classification",
                                              box(width = 6, verbatimTextOutput("tree"),status = "primary", title="sortie tree"),
                                              box(width = 6, plotOutput("abre", height = "300px" ),status = "primary"),
                                              box(width = 6, plotOutput("tre_roc", height = "300px" ),status = "primary")
                                              
                                     ),
                                     tabPanel("gradient boosting",
                                              box(width = 6, plotOutput("gradient"),status = "primary", title="gradient boosting"),
                                              box(width = 6, verbatimTextOutput("perfgradient" ),status = "primary")
                                              
                                     )))
                      
                      
                      
                      
                  )))