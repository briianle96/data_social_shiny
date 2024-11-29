# Librerías necesarias
library(shiny)
library(plotly)
library(readr)
library(tidyverse)
library(corrplot)
library(caret)  # Necesario para createDataPartition()
library(car)    # Necesario para vif()
library(nortest)  # Necesario para lillie.test()
library(usethis)

# VARIABLES GLOBALES
fb_data <- NULL
fb_data_a_modelo <- NULL
fb_data_a_modelo_train <- NULL
fb_data_a_modelo_test <- NULL
fb_mod_full <- NULL
fb_mod_ajus <- NULL
fb_mod_full_residuos <- NULL

fb_data <- read.csv2("data.fb.csv", sep = ";", header = TRUE, fill = TRUE, stringsAsFactors = FALSE)
normalizacion_nombres <- c("post_total_reach", "post_total_impressions", "engaged_users", 
                           "post_consumers", "post_consumptions", "pi_people_who_liked_Page", 
                           "pr_people_who_like_Page", "people_liked_Page_and_engaged_post", "total_interactions")
names(fb_data) <- normalizacion_nombres

# Convertimos los datos a numéricos
fb_data <- data.frame(lapply(fb_data, as.numeric))

# Eliminamos outliers y preparamos los datos globalmente
fb_data_a_modelo <- fb_data
fb_data_a_modelo <- fb_data_a_modelo[!(fb_data$post_total_reach %in% c(122944,128064,139008,153536,158208)),]
fb_data_a_modelo <- fb_data_a_modelo[!(fb_data_a_modelo$post_total_impressions %in% c(220447,229733,251269,252207,277100,453213,457509,497910)),]
fb_data_a_modelo <- fb_data_a_modelo[!(fb_data_a_modelo$post_consumptions %in% c(14974,19779)),]
fb_data_a_modelo <- fb_data_a_modelo[!(fb_data_a_modelo$pi_people_who_liked_Page %in% c(160270,184270)),]

# Dividimos los datos globalmente
set.seed(2018)
fb_data_a_modelo_train_test <- createDataPartition(fb_data_a_modelo$total_interactions, p = 0.8, list = FALSE)
fb_data_a_modelo_train <- fb_data_a_modelo[fb_data_a_modelo_train_test, ]
fb_data_a_modelo_test <- fb_data_a_modelo[-fb_data_a_modelo_train_test, ]

# Creamos el modelo completo globalmente
fb_mod_full <- lm(total_interactions ~ ., data = fb_data_a_modelo_train)

# Obtenemos los residuos del modelo
fb_mod_full_residuos <- residuals(fb_mod_full)

# Ajuste manual del modelo sin "engaged_users"
fb_mod_ajus <- lm(total_interactions ~ . - engaged_users, data = fb_data_a_modelo_train)

fb_data_predecir <- data.frame(
  post_total_reach = 5000,
  post_total_impressions = 7390,
  engaged_users = 103,
  post_consumers = 101,
  post_consumptions = 161,
  pi_people_who_liked_Page = 3001,
  pr_people_who_like_Page = 2010,
  people_liked_Page_and_engaged_post = 254
)

# UI de la aplicación
ui <- fluidPage(
  navbarPage("Análisis de Datos de Facebook",
             tabPanel("Boxplots",
                      fluidRow(
                        column(
                          3,  # Filtro de lista desplegable
                          selectInput(
                            inputId = "selected_variable",
                            label = "Seleccionar variable para el Boxplot:",
                            choices = c("post_total_reach" = "post_total_reach",
                                        "post_total_impressions" = "post_total_impressions",
                                        "engaged_users" = "engaged_users",
                                        "post_consumers" = "post_consumers",
                                        "post_consumptions" = "post_consumptions",
                                        "pi_people_who_liked_Page" = "pi_people_who_liked_Page",
                                        "pr_people_who_like_Page" = "pr_people_who_like_Page",
                                        "people_liked_Page_and_engaged_post" = "people_liked_Page_and_engaged_post",
                                        "total_interactions" = "total_interactions")
                          )
                        ),
                        column(
                          9,  # Gráfico principal
                          h3("Boxplots"),
                          plotlyOutput("boxplot_graph")
                        )
                      )
             ),
             tabPanel("Resumen",
                      fluidRow(
                        column(
                          12,
                          h3("Resumen de Datos"),
                          verbatimTextOutput("summary_output")
                        )
                      )
             ),
             tabPanel("Matriz de Correlación",
                      fluidRow(
                        column(
                          12,
                          h3("Matriz de Correlación"),
                          plotOutput("corrplot_graph", height = "600px", width = "100%")
                        )
                      )
             ),
             tabPanel("APLICACIÓN DE MODELOS DE RLM",
                      fluidRow(
                        column(
                          12,
                          h3("APLICACIÓN DE MODELOS DE RLM"),
                          p("En esta sección, se presenta el modelo de regresión lineal múltiple creado utilizando los datos procesados."),
                          verbatimTextOutput("rlm_summary")
                        )
                      )
             ),
             tabPanel("GRÁFICO DE RESIDUOS Y RECTA",
                      fluidRow(
                        column(
                          12,
                          h3("GRÁFICO DE RESIDUOS Y RECTA"),
                          p("Este gráfico representa los residuos y la recta ajustada del modelo de regresión lineal múltiple."),
                          plotOutput("residuals_graph", height = "600px", width = "100%")
                        )
                      )
             ),
             tabPanel("Normalidad de residuos: ver en QQplot",
                      fluidRow(
                        column(
                          12,
                          h3("Normalidad de residuos: ver en QQplot"),
                          p("Este gráfico muestra diferentes diagnósticos del modelo, incluyendo un QQ-plot para evaluar la normalidad de los residuos."),
                          plotOutput("qqplot_graph", height = "600px", width = "100%")
                        ),
                        column(
                          12,
                          h4("Resultados de los Tests de Normalidad y VIF:"),
                          p("Shapiro test"),
                          verbatimTextOutput("shapiro_test_output"),
                          p("Kolmogorov test"),
                          verbatimTextOutput("kolmogorov_test_output"),
                          p("Análisis de VIFs sobre el modelo FULL"),
                          verbatimTextOutput("vif_test_output")
                        )
                      )
             ),
             tabPanel("Ajuste del Modelo",
                      fluidRow(
                        column(
                          12,
                          h3("Ajuste del Modelo"),
                          h4("Ajustes mediante métodos de selección"),
                          verbatimTextOutput("forward_selection"),
                          verbatimTextOutput("backward_selection"),
                          verbatimTextOutput("both_selection"),
                          h4("Ajuste sin métodos de selección"),
                          verbatimTextOutput("manual_adjustment"),
                          p("VIF sobre el ajuste manual"),
                          verbatimTextOutput("manual_vif")
                        )
                      )
             ),
             tabPanel("Prediccion",
                      fluidRow(
                        column(
                          12,
                          verbatimTextOutput("prediction_data"),
                          h4("Prediccion con el modelo full"),
                          verbatimTextOutput("full_model_prediction"),
                          h4("Prediccion con el modelo ajustado de forma manual"),
                          verbatimTextOutput("manual_model_prediction"),
                          h3("Prediccion con el dataset de test"),
                          h4("Prediccion con el modelo full"),
                          verbatimTextOutput("full_model_prediction_test"),
                          h4("Prediccion con el modelo ajustado de forma manual"),
                          verbatimTextOutput("manual_model_prediction_test"),
                          
                          
                          
                        )
                      )
             )
             
             
  )
)

server <- function(input, output, session) {

  
  
  
  
  #Bloque de BOXPLOT
  output$boxplot_graph <- renderPlotly({
    selected_var <- input$selected_variable
    plot_ly(
      y = fb_data[[selected_var]], 
      type = "box", 
      name = selected_var
    )
  })
  
  #Generar resumen de los datos
  output$summary_output <- renderPrint({
    summary(fb_data)
  })
  
  #Generar matriz de correlación
  output$corrplot_graph <- renderPlot({
    fb_data_a_modelo_corr <- cor(fb_data_a_modelo, use = "complete.obs")
    col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
    corrplot(fb_data_a_modelo_corr, method = "shade", shade.col = NA, tl.col = "black", tl.srt = 27,
             col = col(200), addCoef.col = "black", cl.pos = "n", order = "AOE")
  })
  
  #Mostrar el summary del modelo RLM
  output$rlm_summary <- renderPrint({
    summary(fb_mod_full)
  })
  
  #Gráfico de residuos y recta
  output$residuals_graph <- renderPlot({
    ggplot(fb_data_a_modelo_train, aes(
      x = post_total_reach + post_total_impressions + engaged_users + post_consumers +
        post_consumptions + pi_people_who_liked_Page + pr_people_who_like_Page + 
        people_liked_Page_and_engaged_post, 
      y = total_interactions
    )) +
      geom_point() +
      geom_smooth(method = 'lm', se = FALSE, col = 'green') +
      theme_light()
  })
  
  
  #Normalidad de residuos: QQplot
  output$qqplot_graph <- renderPlot({
    par(mfrow = c(2, 2))
    plot(fb_mod_full)
  })
  
  #Tests de normalidad y VIFs
  output$shapiro_test_output <- renderPrint({
    shapiro.test(fb_mod_full_residuos)
  })
  
  output$kolmogorov_test_output <- renderPrint({
    lillie.test(fb_mod_full_residuos)
  })
  
  output$vif_test_output <- renderPrint({
    vif(fb_mod_full)
  })
  
  
  #Ajustes mediante métodos de selección
  output$forward_selection <- renderPrint({
    forward_model <- step(fb_mod_full, direction = "forward", trace = TRUE)
    list(summary = summary(forward_model), vif = vif(forward_model))
  })
  
  output$backward_selection <- renderPrint({
    backward_model <- step(fb_mod_full, direction = "backward", trace = TRUE)
    list(summary = summary(backward_model), vif = vif(backward_model))
  })
  
  output$both_selection <- renderPrint({
    both_model <- step(fb_mod_full, direction = "both", trace = FALSE)
    list(summary = summary(both_model), vif = vif(both_model))
  })
  
  #Ajuste sin métodos de selección
  output$manual_adjustment <- renderPrint({
    summary(fb_mod_ajus)
  })
  
  output$manual_vif <- renderPrint({
    vif(fb_mod_ajus)
  })
  
  
  
  #Mostrar los datos de predicción
  output$prediction_data <- renderPrint({
    fb_data_predecir
  })
  
  #Predicción con el modelo completo
  output$full_model_prediction <- renderPrint({
    predict(fb_mod_full, fb_data_predecir)
  })
  
  #Predicción con el modelo ajustado manualmente
  output$manual_model_prediction <- renderPrint({
    predict(fb_mod_ajus, fb_data_predecir)
  })
  
  #Predicción con el modelo completo en test
  output$full_model_prediction_test <- renderPrint({
    predict(fb_mod_full,fb_data_a_modelo_test)
  })
  
  #Predicción con el modelo ajustado en test
  output$manual_model_prediction_test <- renderPrint({
    predict(fb_mod_ajus,fb_data_a_modelo_test)
  })
  
}

shinyApp(ui = ui, server = server)
