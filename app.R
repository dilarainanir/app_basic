############PAY ATTENTION!!!!!######
#IN ORDER TO NOT RECEIVE STACK TRACE ERROR: 
#DATAFRAME CANT HAVE NAs
#THE VARIABLES MUST BE ADJUSTED WHETHER NUMERIC, FACTOR OR CHARACTER
#Dont use command view()!



#Load libraries
library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(magrittr)
library(randomForest)
library(Metrics)
library("openxlsx")
library("readxl")
library("jtools")
library(ggstance)
library(ggplot2)
library(ggpubr)
theme_set(theme_pubr())
library(rlang)
library(plotly)

###LOADING DATA#######
data1<-read.csv("https://raw.githubusercontent.com/dilarainanir/app_basic/main/Heizkoerper_bereinigt%20-%20alle%20Normleistungen.csv", sep=";", stringsAsFactors = FALSE,encoding='UTF-8')
data2<-read.csv("https://raw.githubusercontent.com/dilarainanir/app_basic/main/Heizkoerper_bereinigt.csv", sep=";", stringsAsFactors = FALSE,encoding='UTF-8')

data1$PreisNL<-data1$Preis/data1$Normleistung
data2$PreisNL<-data2$Preis/data2$Normleistung



#######################PREPARE THE VARIABLES FOR REGRESSION###########################

#PreisNL   -> ok
is.numeric(data1$PreisNL)
data1$PreisNL<-as.numeric(data1$PreisNL)
quantile(data1$PreisNL)



#Preis   -> ok
is.numeric(data1$Preis)

#2_b_Hoehe_mm   -> ok
is.numeric(data1$Hoehe)
data1$Hoehe<-as.numeric(data1$Hoehe)
quantile(data1$Hoehe)




#3_b_Ausfuehrung
is.factor(data1$Ausfuehrung)
data1$Ausfuehrung<-factor(data1$Ausfuehrung)


data1<-data1[,c("Ausfuehrung", "Preis", "Normleistung", "PreisNL")]


#PreisNL   -> ok
is.numeric(data2$PreisNL)
quantile(data2$PreisNL)



#Preis   -> ok
is.numeric(data2$Preis)

#2_b_Hoehe_mm   -> ok
is.numeric(data2$Hoehe)
data2$Hoehe<-as.numeric(data2$Hoehe)
quantile(data2$Hoehe)




#3_b_Ausfuehrung
is.factor(data2$Ausfuehrung)
data2$Ausfuehrung<-factor(data2$Ausfuehrung)


data2<-data2[,c("Ausfuehrung", "Preis", "Normleistung", "PreisNL")]


new_data_full<- data2
new_data<-new_data_full
#new_data<-read_excel("prediction_test.xlsx")
#new_data_full<-read_excel("prediction_full.xlsx")
new_data$Preis=NA
new_data$Preis=NA



test_set<-data2

#Importing model
githubURL<-("https://github.com/dilarainanir/app_basic/blob/main/rf.rda?raw=true")
model_rf <- readRDS(url(githubURL))
y_pred = predict(model_rf, newdata = test_set)
mae_rf = mae(data2[[2]], y_pred)
rmse_rf = rmse(data2[[2]], y_pred)



#R Shiny ui
ui <- dashboardPage(skin = "yellow",
                    
                    #Dashboard title
                    dashboardHeader(title = "Preisvorhersage f\u00FCr Heizk\u00F6rper", titleWidth = 390),
                    
                    #Sidebar layout
                    dashboardSidebar(width = 290,
                                     sidebarMenu(menuItem("Plots", tabName = "plots", icon = icon('poll')),
                                                 menuItem("Prediction", tabName = 'pred', icon = icon('search')))),
                    
                    #Tabs layout
                    dashboardBody(tags$head(tags$style(HTML('.main-header .logo {font-weight: bold; }'))),
                                  fluidRow(
                                    #Plots tab content
                                    tabItems(tabItem('plots', 
                                                     #Histogram filter
                                                     box(status = 'warning', title = 'Filter f\u00FCr die Histogrammdarstellung',
                                                         selectInput('num', "Numerische Variablen:", c('Preis pro Normleistung (CHF/W)', 'Normleistung (W)', 'Preis (CHF)'))),
                                                     #Frequency plot filter
                                                     box(status = 'warning', title = 'Filtern Sie nach dem Frequenzdiagramm',
                                                         selectInput('cat', 'Kategorische Variablen:', c('Ausf\u00FChrung'))),
                                                     #Boxes to display the plots
                                                     box(status = 'warning', plotOutput('histPlot')),
                                                     box(status = 'warning', plotOutput('freqPlot'))),
                                             
                                             #Prediction tab content
                                             tabItem('pred',
                                                     #Waermeleistung-Normleistung Berechnung
                                                     box(title = 'Berechnung der Normleistung von W\u00E4rmeleistung', status = 'warning', width = 12, solidHeader = TRUE,
                                                         splitLayout(tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible;}"))),
                                                                     
                                                                     div(),
                                                                     numericInput("p_Waermeleistung", "W\u00E4rmeleistung (W)", 0),
                                                                     div(),
                                                                     numericInput("p_Temperatur1", "Vorlauftemperatur (\u00B0C)", 0),
                                                                     div(),
                                                                     numericInput("p_Temperatur2", "R\u00FCcklauftemperatur (\u00B0C)", 0),
                                                                     div(),
                                                                     numericInput("p_Temperatur3", "Raumlufttemperatur (\u00B0C)", 0),
                                                                     div(),
                                                                     sliderInput("p_Exponent", "Exponent (n)", 0.9, 4, value=c(1.24)),
                                                                     div())),
                                                     
                                                     
                                                     #Filters for Kategorische Variable
                                                     box(title = 'W\u00E4hlen Sie Variablen f\u00FCr Preissch\u00E4tzung', status = 'warning', solidHeader = TRUE, width = 12,
                                                         cellWidths = c('2%', '20%', '20%', '20%', '20%', '12%'),
                                                         div(h5('Normleistung:')),
                                                         verbatimTextOutput("b_result", placeholder = TRUE),
                                                         selectInput('p_Ausfuehrung', 'Ausf\u00FChrung', c('Heizwand', 'Konvektor', 'Flachrohrradiator', 'Gliederheizkoerper', 'Roehrenradiatoren')),
                                                         div(),
                                                         sliderInput("p_PreisNL", label = div(style='width:1540px;',
                                                                                              div("Preisklasse (Preis pro Normleistung [CHF/W])"),
                                                                                              div(style='float:left;', 'g\u00FCnstig'), 
                                                                                              div(style='float:right;', 'teuer')), min=0.4, max=2.2, value=0.6, step = 0.05),
                                                         div()),
                                                     #Box to display the prediction results
                                                     #Box to display information about the model
                                                     box(title = 'Modellerkl\u00E4rung', status = 'warning', width = 12, 
                                                         helpText('Das folgende Modell prognostiziert den Preis des gew\u00E4hlten Heizk\u00F6rpers auf der Grundlage von Preis pro Normleistung, Normleistung und Ausf\u00FChrung.'),
                                                         helpText('Der Datensatz, der zum Trainieren des Vorhersagemodells verwendet wird, stammt aus Preislisten von Zehnder und Prolux Brands. Die Daten enthalten 15205 Beobachtungen und 3 Attribute (Ausf\u00FChrung, Normleistung und Preis pro Normleistung) in Bezug auf den Preis.'),
                                                         helpText('Die Vorhersage basiert auf einem multiplen Regressionslernmodell. Ausserdem liefern die Modelle eine Preisspanne als Sicherheitsspanne f\u00FCr die Preissch\u00E4tzung. Beachten Sie, dass der tats\u00E4chliche Preis des von Ihnen gew\u00E4hlten Heizk\u00F6rpers in einigen F\u00E4llen h\u00F6her oder niedriger als die angegebene Preisspanne sein kann.')),
                                                     
                                                     box(title = 'Ergebnis der Preissch\u00E4tzung', status = 'warning', width = 12, 
                                                         div(h5('Preis (CHF):')),
                                                         verbatimTextOutput("value", placeholder = TRUE),
                                                         div(h5('Preisspanne (CHF):')),
                                                         verbatimTextOutput("range", placeholder = TRUE)),
                                                     
                                                     box(title = 'W\u00E4hlen Sie Variablen f\u00FCr Preisspannediagramm', status="success",solidHeader = TRUE, width = 12, 
                                                         splitLayout(tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible;}"))),
                                                                     selectInput('pp_Ausfuehrung', 'Ausf\u00FChrung', c('Heizwand', 'Konvektor', 'Flachrohrradiator', 'Gliederheizkoerper', 'Roehrenradiatoren')),
                                                                     div(),
                                                                     sliderInput("pp_Normleistung", "Normleistung (W)", 600, 20000, value=c(700,900)),
                                                                     div(),
                                                                     sliderInput("pp_PreisNL", "Preisklasse (Preis pro Normleistung [CHF/W])", label = div(style='width:300px;', 
                                                                                                                                                           div("Preisklasse (Preis pro Normleistung [CHF/W])"),
                                                                                                                                                           div(style='float:left;', 'g\u00FCnstig'), 
                                                                                                                                                           div(style='float:right;', 'teuer')), min=0.4, max=2.2, value=c(0.4,0.6), step = 0.05),
                                                                     div())),
                                                     
                                                     #Box to display information about the curve
                                                     box(title = 'Erkl\u00E4rung des Preisspannendiagramms', status = 'success', width = 12, 
                                                         helpText('Die obigen Preisspannendiagramm zeigt die m\u00F6glichen Preise, die ein Heizk\u00F6rper basierend auf Ihrer gew\u00E4hlten Ausf\u00FChrung, Normleistung und Preis pro Normleistung haben kann. Alle diese Informationen stammen aus Preiskatalogen und stellen daher die tats\u00E4chliche Preisspanne von Heizk\u00F6rpern dar.'),
                                                         helpText('Wenn Sie speziell den Wert der Normlesistung und den Preis des im Diagramm dargestellten Heizk\u00F6rpers sehen m\u00F6chten, setzen Sie bitte nur Ihre Maus auf diese Punkte.'),
                                                         helpText('Sie k\u00F6nnen auch das Ergebnis des prognostizierten Preises des Heizk\u00F6rpers basierend auf den Attributen, die Sie im vorherigen Abschnitt ausgew\u00E4hlt haben, als roten Punkt sehen.')),
                                                     box(title = 'Range Diagramm', status = 'success', width = 12,
                                                         plotlyOutput("distPlot"))
                                                     
                                                     
                                                     
                                                     
                                                     
                                             )
                                    ))
                    )
)

# R Shiny server
server <- shinyServer(function(input, output) {
  
  #Univariate analysis
  output$histPlot <- renderPlot({
    
    #Column name variable
    num_val = ifelse(input$num == 'Preis pro Normleistung (CHF/W)', 'PreisNL',
                     ifelse(input$num == 'Normleistung (W)', 'Normleistung',
                            ifelse(input$num == 'Preis (CHF)', 'Preis')))
    
    #Histogram plot
    ggplot(data = data2, aes(x = (data2[[num_val]])))+ 
      geom_histogram(stat = "bin", fill = 'darkolivegreen4', color = 'seashell2')+
      xlim(min(data2[[num_val]]), max(data2[[num_val]])) +
      theme(axis.text = element_text(size = 12),
            axis.title = element_text(size = 14),
            plot.title = element_text(size = 16, face = 'bold'))+
      labs(title = sprintf('Histogrammdarstellung', num_val),
           x = sprintf('%s', input$num),y = 'Frequency')
  })
  
  output$freqPlot <- renderPlot({
    
    #Column name variable
    cat_val = ifelse(input$cat == 'Ausf\u00FChrung', 'Ausfuehrung')
    
    #Frequency plot
    ggplot(data = data2, aes(x = (data2[[cat_val]])))+
      geom_bar(stat = 'count', fill = 'olivedrab3', color = 'seashell2', 
               width = 0.5)+
      stat_count(geom = 'text', size = 4,
                 aes(label = ..count..),
                 position = position_stack(vjust = 1.03))+
      theme(axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.text = element_text(size = 12),
            axis.title = element_text(size = 14),
            plot.title = element_text(size = 16, face="bold"))+
      labs(title = sprintf('Frequenzdiagramm', cat_val),
           x = sprintf('%s', input$cat), y = 'Count')
    
  })
  
  #Dashboard analysis
  
  #Prediction model
  #React value when using the action button
  
  output$b_result<- renderPrint({
    round((input$p_Waermeleistung/(((((input$p_Temperatur1+input$p_Temperatur2)/2)-input$p_Temperatur3)/49.8)^input$p_Exponent)), digits = 0)
    
  })
  
  a <- reactiveValues(result = NULL)
  
  output$value <- renderPrint({
    #Copy of the test data without the dependent variable
    test_pred <- test_set[-2]
    test_pred$Ausfuehrung<-as.factor(test_pred$Ausfuehrung)
    test_pred = na.omit(test_pred)
    
    #Dataframe for the single prediction
    values = data.frame(Ausfuehrung = input$p_Ausfuehrung,
                        PreisNL = input$p_PreisNL, 
                        Normleistung = (input$p_Waermeleistung/(((((input$p_Temperatur1+input$p_Temperatur2)/2)-input$p_Temperatur3)/49.8)^input$p_Exponent)))
    
    #Inclued the values into the new data
    test_pred$Ausfuehrung<-as.factor(test_pred$Ausfuehrung)
    test_pred$PreisNL <- as.numeric(test_pred$PreisNL)
    test_pred$Normleistung <- as.numeric(test_pred$Normleistung)
    
    model=lm(Preis~ Normleistung+Ausfuehrung+PreisNL,
             data = data1)
    
    
    #Single preiction using the multilinear regression
    a$result <-  (as.data.frame(predict(model, 
                                        newdata = values, interval = "prediction")))$fit
    paste(round(a$result, digits=0))
    
  })
  
  
  
  
  output$range <- renderPrint({
    #Display the range of prediction value using the MAE value
    input$cal
    values = data.frame(Ausfuehrung = input$p_Ausfuehrung,
                        PreisNL = input$p_PreisNL, 
                        Normleistung = (input$p_Waermeleistung/(((((input$p_Temperatur1+input$p_Temperatur2)/2)-input$p_Temperatur3)/49.8)^input$p_Exponent)))
    
    model=lm(Preis~ Normleistung+Ausfuehrung+log(PreisNL),
             data = data1)
    
    isolate(sprintf('(%s) - (%s)', 
                    round((as.data.frame(predict(model, 
                                                 newdata = values, interval = "prediction")))$lwr, digits=0), 
                    round((as.data.frame(predict(model, 
                                                 newdata = values, interval = "prediction")))$upr, digits=0)))
  })
  
  
  
  output$distPlot<-renderPlotly({
    
    ggplotly(test_set %>%
               dplyr::filter(Normleistung >= input$pp_Normleistung[1] & Normleistung <= input$pp_Normleistung[2] & Ausfuehrung==input$pp_Ausfuehrung[1] &PreisNL >= input$pp_PreisNL[1] & PreisNL <= input$pp_PreisNL[2]) %>% 
               ggplot(aes(Normleistung, Preis)) + 
               geom_point(alpha = 0.9, size = 0.65) + 
               geom_point(aes(x=(input$p_Waermeleistung/(((((input$p_Temperatur1+input$p_Temperatur2)/2)-input$p_Temperatur3)/49.8)^input$p_Exponent)), y=a$result), colour="red")+
               labs( x = "Normleistung", y = "Preis"))
  })
  
  
  
})


shinyApp(ui, server)







