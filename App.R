#----------------------------------------------------------------
#Gráfica de Porcentaje de No Cumplimiento de AV por Genración y Método de Verificación
#                                   Krýstal
#---------------------------------------------------------------
#Contempla Semanas y REC

# ---- Cargar las librerías necesarias ----
load("datos_necesarios_app.RData", envir = .GlobalEnv)
load("entorno_global-22-07-25.RData")

if(!require(pacman)){
  install.packages('pacman')
};
library(pacman)
#library(readr)
pacman::p_load(shiny,plotly,DT,stringr,shinythemes,shinycssloaders,openxlsx)

# if (grupo_num >= 0 && grupo_num <= 23) {
#   title_text <- paste0('Tabla de Reactivos para el Módulo ', grupo_num)
# } else if (grupo_num >= 100 && grupo_num < 105){
#   title_text <- paste0('Tabla de Reactivos para REC Bloque', grupo_num-100)
# } else if (grupo_num == 105){
#   title_text <- paste0('Tabla de Reactivos para REC Módulo 22')
# } else if (grupo_num == 106){
#   title_text <- paste0('Tabla de Reactivos para REC Módulo 23')
# }

#' dselect_func <- function(grupo,dataset) {
#'   
#'   data_seleccionada <- reactive({
#'     grupo_value <- grupo() # Verificamos que 'grupo()' sea uno de los valores esperados
#'     grupo_num <- as.numeric(gsub("grupo", "", grupo_value))  # Extrae el número del grupo
#'     if (!is.na(grupo_num) && grupo_num >= 0 && grupo_num <= 107) {
#'       return(dataset[[grupo_value]])  # Accede dinámicamente al grupo correspondiente
#'     } else {
#'       return(NULL)  # En caso de que el valor de grupo no sea válido
#'     }
#'   })
#'   return(data_seleccionada)
#' }
#' 
#' ui_foc_gen_func <- function(df,grup,num_col,colores) {
#'   
#'   # output$ui_foc_gen <- renderPlotly({
#'   renderPlotly({
#'     data <- df()
#'     
#'     grupo_num <- as.numeric(gsub("grupo", "", grup()))  # Extrae el número del grup
#'     
#'     req(data)
#'     
#'     #colores= c("#1f77b4", "#ff7f0e", "#2ca02c")
#'     etiquetas <- as.character(colnames(data)[2:4])
#'     valores <- as.numeric(data[num_col, 2:4])
#'     
#'     plot_ly(
#'       labels = etiquetas,
#'       values = valores,
#'       type = "pie",
#'       insidetextorientation = "radial",
#'       insidetextorientation = "radial",
#'       textinfo = "label+percent",#"none",  # 👈 no repite texto
#'       hoverinfo = "text",  # 👈 solo muestra tu texto personalizado
#'       text = ~paste0(etiquetas, ": ", valores, "%"),
#'       marker = list(colors = colores)
#'     ) %>%
#'       layout(title= list(text = paste("Distribución de", as.character(data[num_col,1])),y = 0.95),
#'              xaxis = list(cliponaxis = FALSE),
#'              yaxis = list(cliponaxis = FALSE),
#'              margin = list(t = 120))
#'   })
#' }
#' 
#' # carac_tabla_func <- function(bg_color, f_color, input_fontSize) {
#' #   tags$style(HTML(sprintf("
#' #     table.dataTable td {
#' #       font-size: %dpx;
#' #     }
#' #     table.dataTable th {
#' #       font-size: %dpx;
#' #       background-color: %s;
#' #       color: %s;
#' #     }
#' #   ", input_fontSize, input_fontSize, bg_color, f_color)))
#' # }
#' 
#' carac_tabla_func <- function(table_id, bg_color, f_color, input_fontSize) {
#'   tags$style(HTML(sprintf("
#'     #%s td {
#'       font-size: %dpx;
#'     }
#'     #%s th {
#'       font-size: %dpx;
#'       background-color: %s;
#'       color: %s;
#'     }
#'   ", table_id, input_fontSize, table_id, input_fontSize, bg_color, f_color)))
#' }
#' 
#' tabla_gt_func <- function(data) {
#'   #col_grupos <- names(data)[grep("Grupos", names(data))]
#'   col_grupos1 <- names(data)[grep("^REC", names(data))]
#'   col_grupos2 <- names(data)[grep("^G", names(data))]
#'   col_grupos  <- c(col_grupos1, col_grupos2)  # Combinar ambas listas
#'   col_nivel  <- names(data)[grep("Nivel de Cumplimiento", names(data))]
#'   
#'   datatable(
#'     data,
#'     options = list(autoWidth = TRUE),
#'     class = "compact stripe hover"
#'   ) %>%
#'     formatStyle(
#'       columns = col_grupos,
#'       valueColumns = col_nivel,  # <- Aquí defines la columna de referencia
#'       #'Nivel de cumplimiento',
#'       backgroundColor = styleEqual(
#'         c("Muy bueno", "Bueno", "Regular", "Malo", "Muy malo"),
#'         c("#2D8B3F", "#78AE6D","#F4D166","#F9866F",'#D2241F'),
#'         #c("#008B00", "green","yellow","red",'#CD0000')
#'       ),
#'       color = "white"  # texto blanco para contrastar, opcional
#'     )
#' }
#' 
#' descargar_excel_func <- function(nombre, df_reactivo, grupo_reactivo) {
#'   downloadHandler(
#'     filename = function() {
#'       grupo_num <- as.numeric(gsub("grupo", "", grupo_reactivo()))
#'       nom_mod_descarga <- nombre_m_archivos_func(grupo_num)
#'       paste0(nombre, nom_mod_descarga, ".xlsx")
#'     },
#'     content = function(file) {
#'       write.xlsx(df_reactivo(), file)
#'     }
#'   )
#' }
#' 
#' nombre_m_archivos_func <- function(num) {
#'   if (num >= 0 && num <= 23) {
#'     nom_mod_descarga <- paste0('M',num)
#'   } else if (num >= 100 && num < 105){
#'     nom_mod_descarga <- paste0('REC Bloque', num-100)
#'   } else if (num == 105){
#'     nom_mod_descarga <- paste0('REC Módulo 22')
#'   } else if (num == 106){
#'     nom_mod_descarga <- paste0('REC Módulo 23')
#'   }
#'   return(nom_mod_descarga)
#' }

ui <- fluidPage(
  #theme = shinytheme("cerulean"),  # otros: "cerulean", "darkly", "cosmo", etc. cyborg
  #titlePanel("Mi Aplicación con pestañas"),
  titlePanel(uiOutput("titulo")),
  
  tags$head(
    # CSS para permitir scroll horizontal en todo el body
    tags$style(HTML("
      body {overflow-x: auto;}
      table.dataTable {width: auto !important;}
      
          /* Estilo para todos los botones por defecto (incluye downloadButton) */
    .btn-default {
      background-color: #28a745 !important;  /* verde */
      color: white !important;
      border-color: #28a745 !important;
    }
    .btn-default:hover {
      background-color: #218838 !important; /* verde oscuro */
      border-color: #1e7e34 !important;
    }
    "))
  ),
  
  tabsetPanel(
    tabPanel("Inicio",
             h3("Introducción"),
             p("\nTe damos la bienvenida a la página, donde podrás encontrar los reportes de Apoyo de Seguimiento Modular, Intervención Focalizada e información adicional relacionada.
               \nCualquier duda o comentario, puedes dirigirte con el Equipo de reportes modulares de la DASI.")
    ),
    tabPanel("Seguimiento Modular",
             fluidPage(
               #h3("Análisis"),
               p("\nEn este reporte conocerás datos y estadísticas del desempeño del módulo."),
               tabsetPanel(
                 
                 tabPanel("Gráficas",
                          p("\nConoce los porcentajes de Cumplimiento e Incumplimiento del módulo:"),
                          fluidPage(
                            # Menú para seleccionar productos, ediciones, categorías y regiones
                            plotlyOutput("ui_mod_graf_g1"),
                            checkboxGroupInput("Semana",
                                               "Selecciona las semanas:",
                                               choices = NULL,
                                               selected = NULL),
                            checkboxGroupInput("Verificacion",
                                               "Selecciona el método de verificación:",
                                               choices = NULL,
                                               selected = NULL),
                            selectInput("Generación",
                                        "Selecciona la Generación:",
                                        choices = NULL,
                                        selected = NULL,
                                        multiple = TRUE),
                            withSpinner(plotlyOutput("ui_mod_graf_g2")),
                            uiOutput("ui_mod_graf2_porcentaje"),
                            checkboxGroupInput("ui_mod_graf2_Semana",
                                               "Selecciona las semanas:",
                                               choices = NULL,
                                               selected = NULL),
                            checkboxGroupInput("ui_mod_graf2_Verificacion",
                                               "Selecciona el método de verificación:",
                                               choices = NULL,
                                               selected = NULL),
                            selectInput("ui_mod_graf2_Generación",
                                        "Selecciona la Generación:",
                                        choices = NULL,
                                        selected = NULL,
                                        multiple = TRUE)
                          )),
                 
                 tabPanel("Revisión",
                          p("\nA continuación verás cómo es el desempeño de los asesores en el módulo y un histórico de no cumplimiento."),
                          fluidPage(
                            DTOutput("tabla_color_mod_rev_cumpl"),
                            downloadButton("descargar_mod_rev_cumpl", "Descargar Excel"),
                            
                            uiOutput("estiloTabla_mod_rev_rev"),  # Aquí va el CSS dinámico
                            sliderInput("fontSize_mod_rev_rev", "Tamaño de fuente:", 
                                        min = 8, max = 20, value = 12),
                            DTOutput("ui_mod_rev_rev"),
                            downloadButton("descargar_mod_rev_rev", "Descargar Excel")
                          )
                 ),
                 
                 tabPanel("Base Completa",
                          p("\nAquí encontrarás un compilado de lo registrado en los importables agrupado por método de verificación."),
                          sliderInput("fontSize_mod_bas_com", "Tamaño de fuente:", 
                                      min = 8, max = 20, value = 12),
                          uiOutput("estiloTabla_mod_bas_com"),  # Aquí va el CSS dinámico
                          
                          DTOutput("ui_mod_bas_com"),
                          downloadButton("descargar_mod_bas_com", "Descargar Excel")
                 ),
                 
                 tabPanel("Semana 1",
                          p("\nEste apartado muestra el compilado de los importables de semana 1 del módulo."),
                          sliderInput("fontSize_mod_sem1", "Tamaño de fuente:", 
                                      min = 8, max = 20, value = 12),
                          uiOutput("estiloTabla_mod_sem1"),  # Aquí va el CSS dinámico
                          
                          DTOutput("ui_mod_sem1"),
                          downloadButton("descargar_mod_sem1", "Descargar Excel")
                 ),
                 
                 tabPanel("Semana 2",
                          p("\nEste apartado muestra el compilado de los importables de semana 2 del módulo."),
                          sliderInput("fontSize_mod_sem2", "Tamaño de fuente:", 
                                      min = 8, max = 20, value = 12),
                          uiOutput("estiloTabla_mod_sem2"),  # Aquí va el CSS dinámico
                          
                          DTOutput("ui_mod_sem2"),
                          downloadButton("descargar_mod_sem2", "Descargar Excel")
                 ),
                 tabPanel("Semana 3",
                          p("\nEste apartado muestra el compilado de los importables de semana 3 del módulo."),
                          sliderInput("fontSize_mod_sem3", "Tamaño de fuente:", 
                                      min = 8, max = 20, value = 12),
                          uiOutput("estiloTabla_mod_sem3"),  # Aquí va el CSS dinámico
                          
                          DTOutput("ui_mod_sem3"),
                          downloadButton("descargar_mod_sem3", "Descargar Excel")
                 ),
                 tabPanel("Semana 4",
                          p("\nEste apartado muestra el compilado de los importables de semana 4 del módulo."),
                          sliderInput("fontSize_mod_sem4", "Tamaño de fuente:", 
                                      min = 8, max = 20, value = 12),
                          uiOutput("estiloTabla_mod_sem4"),  # Aquí va el CSS dinámico
                          
                          DTOutput("ui_mod_sem4"),
                          downloadButton("descargar_mod_sem4", "Descargar Excel")
                 )
               ))
    ),
    tabPanel("Intervención focalizada",
             #h3("Configuración"),
             p("\nEn este reporte encontrarás datos y estadísticas relacionadas con el desempeño del personal de asesores virtuales del módulo."),
             tabsetPanel(
               
               tabPanel("General",
                        p("\nAquí verás cómo se distribuye el desempeño del personal en el módulo y por método de verificación."),
                        DTOutput("tabla_color_foc_gen_cumpl"),
                        downloadButton("descargar_foc_gen_cumpl", "Descargar Excel"),
                        
                        sidebarLayout(
                          h3(textOutput("titulo_grafico")),
                          withSpinner(plotlyOutput("ui_foc_gen_mb"))
                        ),
                        sidebarLayout(
                          h3(textOutput("titulo_grafico")),
                          withSpinner(plotlyOutput("ui_foc_gen_fne"))
                        ),
                        sidebarLayout(
                          h3(textOutput("titulo_grafico")),
                          withSpinner(plotlyOutput("ui_foc_gen_fe"))
                        ),
                        sidebarLayout(
                          h3(textOutput("titulo_grafico")),
                          withSpinner(plotlyOutput("ui_foc_gen_fn"))
                        ),
                        sidebarLayout(
                          h3(textOutput("titulo_grafico")),
                          withSpinner(plotlyOutput("ui_foc_gen_eva"))
                        ),
                        sidebarLayout(
                          h3(textOutput("titulo_grafico")),
                          withSpinner(plotlyOutput("ui_foc_gen_peaa"))
                        ),
                        sidebarLayout(
                          h3(textOutput("titulo_grafico")),
                          withSpinner(plotlyOutput("ui_foc_gen_eaa"))
                        )
               ),
               tabPanel("Cumplimiento AV",
                        p("\nAquí encontrarás un resumen de cómo se distribuye el desempeño del personal de asesores virtuales y sobre quienes se puede realizar intervenciones más puntuales de acuerdo a su desmpeño en el módulo."),
                        DTOutput("tabla_color_foc_cumav_rev"),
                        downloadButton("descargar_foc_cumav_rev", "Descargar Excel"),
                        
                        sliderInput("fontSize_foc_cumav_avs", "Tamaño de fuente:", 
                                    min = 8, max = 20, value = 12),
                        uiOutput("estiloTabla_foc_cumav_avs"),  # Aquí va el CSS dinámico
                        DTOutput("ui_foc_cumav_avs"),
                        downloadButton("descargar_foc_cumav_avs", "Descargar Excel")
               ),
               
               tabPanel("Revisión",
                        p("\nAquí verás un resumen del tipo de intervenció por método de verificación."),
                        sliderInput("fontSize_foc_rev", "Tamaño de fuente:", 
                                    min = 8, max = 20, value = 12),
                        uiOutput("estiloTabla_foc_rev"),  # Aquí va el CSS dinámico
                        DTOutput("ui_foc_rev"),
                        downloadButton("descargar_foc_rev", "Descargar Excel")
               ),
               tabPanel("Seguimiento AV",
                        p("\nEn este apartado podrás visualizar el desempeño individual del personal de aseroría virtual por reactivo, semana, método de verificación y generación."),
                        fluidPage(#column(#fixedRow(#bootstrapPage(#fillPage(#pageWithSidebar(#navbarPage(#fixedPage(##
                          plotlyOutput("ui_foc_segav_grafico_barras"),
                          
                          uiOutput("ui_foc_segav_porcentaje"),
                          uiOutput("ui_foc_segav_generacion"),
                          uiOutput("ui_foc_segav_av"),
                          uiOutput("ui_foc_segav_semana"),
                          uiOutput("ui_foc_segav_verificacion"),
                          DTOutput("ui_foc_segav_tabla"),
                          sliderInput("fontSize_foc_segav_tabla", "Tamaño de fuente:", 
                                      min = 8, max = 20, value = 12),
                          uiOutput("estiloTabla_foc_segav_tabla"),  # Aquí va el CSS dinámico
                          
                        )
               ),
               
               tabPanel("Mensaje de bienvenida",
                        p("\nAquí encontrarás un histórico del desempeño individual en la categoría del mensaje de bienvenida."),
                        sliderInput("fontSize_foc_mensaje_bienvenida", "Tamaño de fuente:", 
                                    min = 8, max = 20, value = 12),
                        uiOutput("estiloTabla_foc_mensaje_bienvenida"),  # Aquí va el CSS dinámico
                        DTOutput("ui_foc_mensaje_bienvenida"),
                        downloadButton("descargar_foc_mensaje_bienvenida", "Descargar Excel")
               ),
               
               tabPanel("Foro no evaluable",
                        p("\nAquí encontrarás un histórico del desempeño individual en la categoría de Foro no evaluable."),
                        sliderInput("fontSize_foc_foro_no_evaluable", "Tamaño de fuente:", 
                                    min = 8, max = 20, value = 12),
                        uiOutput("estiloTabla_foc_foro_no_evaluable"),  # Aquí va el CSS dinámico
                        DTOutput("ui_foc_foro_no_evaluable"),
                        downloadButton("descargar_foc_foro_no_evaluable", "Descargar Excel")
               ),
               
               tabPanel("Foro evaluable",
                        p("\nAquí encontrarás un histórico del desempeño individual en la categoría de Foro evaluable."),
                        sliderInput("fontSize_foc_foro_evaluable", "Tamaño de fuente:", 
                                    min = 8, max = 20, value = 12),
                        uiOutput("estiloTabla_foc_foro_evaluable"),  # Aquí va el CSS dinámico
                        DTOutput("ui_foc_foro_evaluable"),
                        downloadButton("descargar_foc_foro_evaluable", "Descargar Excel")
               ),
               tabPanel("Foro de novedades",
                        p("\nAquí encontrarás un histórico del desempeño individual en la categoría de Foro de novedades."),
                        sliderInput("fontSize_foc_foro_novedades", "Tamaño de fuente:", 
                                    min = 8, max = 20, value = 12),
                        uiOutput("estiloTabla_foc_foro_novedades"),  # Aquí va el CSS dinámico
                        DTOutput("ui_foc_foro_novedades"),
                        downloadButton("descargar_foc_foro_novedades", "Descargar Excel")
               ),
               tabPanel("Evaluación",
                        p("\nAquí encontrarás un histórico del desempeño individual en la categoría de Evaluación."),
                        sliderInput("fontSize_foc_evaluacion", "Tamaño de fuente:", 
                                    min = 8, max = 20, value = 12),
                        uiOutput("estiloTabla_foc_evaluacion"),  # Aquí va el CSS dinámico
                        DTOutput("ui_foc_evaluacion"),
                        downloadButton("descargar_foc_evaluacion", "Descargar Excel")
               ),
               tabPanel("Pizarra de EAA",
                        p("\nAquí encontrarás un histórico del desempeño individual en la categoría de Pizarra de EAA."),
                        sliderInput("fontSize_foc_pizarra_eaa", "Tamaño de fuente:", 
                                    min = 8, max = 20, value = 12),
                        uiOutput("estiloTabla_foc_pizarra_eaa"),  # Aquí va el CSS dinámico
                        DTOutput("ui_foc_pizarra_eaa"),
                        downloadButton("descargar_foc_pizarra_eaa", "Descargar Excel")
               ),
               tabPanel("EAA",
                        p("\nAquí encontrarás un histórico del desempeño individual en la categoría de EAA."),
                        sliderInput("fontSize_foc_eaa", "Tamaño de fuente:", 
                                    min = 8, max = 20, value = 12),
                        uiOutput("estiloTabla_foc_eaa"),  # Aquí va el CSS dinámico
                        DTOutput("ui_foc_eaa"),
                        downloadButton("descargar_foc_eaa", "Descargar Excel")
               )
               
             )
    )
  )
)

server <- function(input, output, session) {
  # Lógica del servidor
  
  # Accedemos al parámetro 'grupo' de la URL dentro de un entorno reactivo
  grupo <- reactive({
    query <- parseQueryString(session$clientData$url_search)
    grupo_value <- query$grupo
    
    # Si no se encuentra el parámetro 'grupo', asignamos un valor por defecto
    if (is.null(grupo_value)) {
      grupo_value <- "grupo103"  # Puedes cambiar el valor por defecto según lo necesites
    }
    return(grupo_value)
  })
  
  output$titulo <- renderText({
    
    grupo_num <- as.numeric(gsub("grupo", "", grupo()))  # Extrae el número del grupo
    tema <- Tema_mod$tema[Tema_mod$modulo == grupo_num]
    
    # En caso de que no encuentre un match
    if (length(tema) == 0) {
      return("Tema no encontrado")
    }
    
    return(tema)
  })
  
  #Inicio---------------------------------------------
  #-------Bienvenida-------
  
  #Modular---------------------------------------------
  #-------Gráf1-------
  # p.mod_graf1 <- "seg_mod_up_M"
  # datasets_mod_graf1 <- dataset_func(datasets_mod_graf1,p.mod_graf1)
  select_mod_graf1 <- dselect_func(grupo,datasets_mod_graf1)
  
  observeEvent(select_mod_graf1(), {
    semanas_disponibles <- unique(select_mod_graf1()$Semana)
    updateCheckboxGroupInput(session, "Semana", choices = semanas_disponibles, selected = semanas_disponibles)
    
    verificacion_disponibles <- unique(select_mod_graf1()$Verificacion)
    updateCheckboxGroupInput(session, "Verificacion", choices = verificacion_disponibles, selected = verificacion_disponibles)
    
    generacion_disponibles <- unique(select_mod_graf1()$Generación)
    updateSelectInput(session, "Generación", choices = generacion_disponibles, selected = generacion_disponibles)
    
  })
  
  output$ui_mod_graf_g1 <- renderPlotly({
    
    grupo_num <- as.numeric(gsub("grupo", "", grupo()))  # Extrae el número del grupo
    
    data <- select_mod_graf1()
    
    # Filtrar los datos según la selección del usuario
    data_filtrada <- data[data$Generación %in% input$Generación &
                            data$Semana %in% input$Semana &
                            data$Verificacion %in% input$Verificacion, ]
    
    # Crear el gráfico de barras apiladas con los datos filtrados
    plot_ly(data_filtrada,
            x = ~IDReactivo,
            y = ~porcentaje_no,
            color = ~Semana,  
            #text = ~paste(Generación, Semana, Verificacion),
            text = ~paste0(
              round(porcentaje_no, 1), "% | ",
              "Generación: ", Generación, "<br>",
              "Semana: ", Semana, "<br>",
              "Verificación: ", Verificacion
            ),
            #text = ~paste("Generación:", Generación, "<br>Semana:", Semana, "<br>Verificacion:", Verificacion),
            hoverinfo =  'text',
            type = 'bar') %>%
      layout(
        barmode = 'stack',  # Apilado de barras
        title = 'Porcentaje de incumplimiento \npor generación, semana y método de verificación',
        xaxis = list(title = 'ID Reactivo'),
        yaxis = list(title = 'Incumplimiento (%)'),
        legend = list(title = list(text = "Semanas del módulo")),
        margin = list(t = 100)
      )
  })
  
  #-------Gráf2-------
  select_mod_graf2 <- dselect_func(grupo,datasets_mod_graf2)
  
  observeEvent(select_mod_graf2(), {
    semanas_disponibles <- unique(select_mod_graf2()$Semana)
    updateCheckboxGroupInput(session, "ui_mod_graf2_Semana", choices = semanas_disponibles, selected = semanas_disponibles)
    
    verificacion_disponibles <- unique(select_mod_graf2()$Verificacion)
    updateCheckboxGroupInput(session, "ui_mod_graf2_Verificacion", choices = verificacion_disponibles, selected = verificacion_disponibles)
    
    generacion_disponibles <- unique(select_mod_graf2()$Generación)
    updateSelectInput(session, "ui_mod_graf2_Generación", choices = generacion_disponibles, selected = generacion_disponibles)
    
  })
  
  output$ui_mod_graf2_porcentaje <- renderUI({
    selectInput("ui_mod_graf2_Porc", "Selecciona el %:", choices = c(
      "Incumplimiento" = "No",
      "Cumplimiento" = "Sí",
      "NA" = "NAN"))
  })
  
  
  output$ui_mod_graf_g2 <- renderPlotly({
    
    req(select_mod_graf2())
    req(input$ui_mod_graf2_Generación)
    req(input$ui_mod_graf2_Semana)
    req(input$ui_mod_graf2_Verificacion)
    req(input$ui_mod_graf2_Porc)
    
    data <- select_mod_graf2()
    
    grupo_num <- as.numeric(gsub("grupo", "", grupo()))  # Extrae el número del grup
    
    ui_mod_graf2_y_variable <- input$ui_mod_graf2_Porc  # Esto es 'porcentaje_no' o 'porcentaje_si'
    
    if (ui_mod_graf2_y_variable == "No") {
      
      #title_text <- 'Porcentaje de No Cumplimiento de AV por Generación y Método de Verificación'
      yaxis_title <- 'Porcentaje incumplimiento (%)'
      if (grupo_num >= 0 && grupo_num <= 23) {
        title_text <- paste0('Módulo ', grupo_num, '\nPorcentaje de incumplimiento\npor generación y método de Verificación')
      } else if (grupo_num >= 101 && grupo_num <= 104) {
        modulos_bloque <- list(
          '101' = '(Módulos 1, 5, 9, 13, 17, 21)',
          '102' = '(Módulos 2, 6, 10, 14, 18)',
          '103' = '(Módulos 3, 7, 11, 15, 19)',
          '104' = '(Módulos 4, 8, 12, 16, 20)'
        )
        bloque <- grupo_num - 100
        title_text <- paste0('REC Bloque ', bloque, ' ', modulos_bloque[[as.character(grupo_num)]], '\nPorcentaje de incumplimiento\npor Generación y método de verificación')
      } else if (grupo_num == 105) {
        title_text <- 'REC Bloque 2 Módulo 22 \nPorcentaje de incumplimiento\npor generación y método de verificación'
      } else if (grupo_num == 106) {
        title_text <- 'REC Bloque 3 Módulo 23 \nPorcentaje de incumplimiento\npor generación y método de verificación'
      }
      
    } else if (ui_mod_graf2_y_variable == "Sí") {
      
      #title_text <- 'Porcentaje de Cumplimiento de AV por Generación y Método de Verificación'
      yaxis_title <- 'Porcentaje cumplimiento (%)'
      if (grupo_num >= 0 && grupo_num <= 23) {
        title_text <- paste0('Módulo ', grupo_num, '\nPorcentaje de cumplimiento\npor generación y método de Verificación')
      } else if (grupo_num %in% 101:104) {
        bloque <- grupo_num - 100
        modulos_text <- switch(
          as.character(grupo_num),
          '101' = '(Módulos 1, 5, 9, 13, 17, 21)',
          '102' = '(Módulos 2, 6, 10, 14, 18)',
          '103' = '(Módulos 3, 7, 11, 15, 19)',
          '104' = '(Módulos 4, 8, 12, 16, 20)'
        )
        title_text <- paste0('REC Bloque ', bloque, ' ', modulos_text, '\nPorcentaje de cumplimiento\npor generación y método de Verificación')
      } else if (grupo_num == 105) {
        title_text <- 'REC Bloque 2 Módulo 22 \nPorcentaje de cumplimiento\npor generación y método de verificación'
      } else if (grupo_num == 106) {
        title_text <- 'REC Bloque 3 Módulo 23 \nPorcentaje de cumplimiento\npor generación y método de verificación'
      }
      
    } else {
      # title_text <- 'Gráfico'
      # yaxis_title <- 'Porcentaje'
      
      #title_text <- 'Porcentaje de Cumplimiento de AV por Generación y Método de Verificación'
      yaxis_title <- 'Porcentaje de NA (%)'
      if (grupo_num >= 0 && grupo_num <= 23) {
        title_text <- paste0('Módulo ', grupo_num, '\nPorcentaje de NA\npor generación y método de verificación')
      } else if (grupo_num %in% 101:104) {
        bloque <- grupo_num - 100
        modulos_text <- switch(
          as.character(grupo_num),
          '101' = '(Módulos 1, 5, 9, 13, 17, 21)',
          '102' = '(Módulos 2, 6, 10, 14, 18)',
          '103' = '(Módulos 3, 7, 11, 15, 19)',
          '104' = '(Módulos 4, 8, 12, 16, 20)'
        )
        title_text <- paste0('REC Bloque ', bloque, ' ', modulos_text, '\nPorcentaje de NA\npor generación y método de verificación')
      } else if (grupo_num == 105) {
        title_text <- 'REC Bloque 2 Módulo 22 \nPorcentaje de NA\npor generación y método de verificación'
      } else if (grupo_num == 106) {
        title_text <- 'REC Bloque 3 Módulo 23 \nPorcentaje de NA\npor generación y método de verificación'
      }
      
    } 
    
    #mod_graf2_y_variable <- input$ui_mod_graf2_Porc  # Esto es 'porcentaje_no' o 'porcentaje_si'
    # Filtrar los datos según la selección del usuario
    data_filtrada <- data[data$Generación %in% input$ui_mod_graf2_Generación &
                            data$Semana %in% input$ui_mod_graf2_Semana &
                            data$Verificacion %in% input$ui_mod_graf2_Verificacion, ]
    
    # Crear el gráfico de barras apiladas con los datos filtrados
    plot_ly(data_filtrada,
            x = ~Verificacion,
            y = as.formula(paste("~", input$ui_mod_graf2_Porc)),#~get(input$ui_mod_graf2_Porc)#
            color = ~paste(Generación, Verificacion, Semana),  
            #text = ~paste(Generación, Semana, Verificacion),
            text = ~paste0(round(No, 1), "% | ",
                           "Generación: ", Generación, "<br>",
                           "Semana: ", Semana, "<br>",
                           "Verificación: ", Verificacion
            ),
            
            hoverinfo =  'text',
            type = 'bar') %>%
      layout(
        barmode = 'stack',  # Apilado de barras
        xaxis = list(title = 'Método de verificación'),
        # yaxis = list(title = 'Porcentaje de No cumplió'),
        # title = 'Porcentaje de Incumplimiento por Generación, semana y método de verificación',
        #title = title_text,
        title = list(text = title_text, y = 0.95),
        yaxis = list(title = yaxis_title),
        legend = list(title = list(text = "Métodos de verificación")),
        margin = list(t = 100)
      )
  })
  #-------Revisión-------
  #Tabla de cumplimiento
  select_mod_rev_cumpl <- dselect_func(grupo,datasets_mod_rev_cumpl)
  
  output$tabla_color_mod_rev_cumpl <- renderDT({
    df <-  select_mod_rev_cumpl()
    tabla_gt_func(df)  # Renderiza tabla con colores
  })
  
  df_mod_rev_cumpl <- reactive({
    select_mod_rev_cumpl()
  })
  
  nombre_arc_mod_rev_cumpl <- paste0("Modular_Cumplimiento ")
  output$descargar_mod_rev_cumpl <- descargar_excel_func(nombre_arc_mod_rev_cumpl, df_mod_rev_cumpl,grupo)
  
  #Tabla de revisión
  select_mod_rev_rev <- dselect_func(grupo,datasets_mod_rev_rev)
  
  # Selecciona la tabla de acuerdo al grupo
  output$ui_mod_rev_rev <- renderDT({
    data <-  select_mod_rev_rev()
    
    datatable(data, options = list(autoWidth = TRUE),#list(scrollX = TRUE), 
              class = "compact stripe hover")#, 
    #container = htmltools::tags$div(style = "font-size:12px"))
    
  })
  
  output$estiloTabla_mod_rev_rev <- renderUI({
    carac_tabla_func("ui_mod_rev_rev", "#EF9A9A", "#7F000D", input$fontSize_mod_rev_rev)
  })
  
  df_mod_rev_rev <- reactive({
    select_mod_rev_rev()
  })
  
  nombre_arc_mod_rev_rev <- paste0("Modular_Revision ")
  output$descargar_mod_rev_rev <- descargar_excel_func(nombre_arc_mod_rev_rev, df_mod_rev_rev,grupo)
  
  #-------Base Completa-------
  select_mod_bas_com <- dselect_func(grupo,datasets_mod_bas_com)
  
  # Selecciona la tabla de acuerdo al grupo
  output$ui_mod_bas_com <- renderDT({
    data <- select_mod_bas_com()
    
    datatable(data, options = list(autoWidth = TRUE),#list(scrollX = TRUE), 
              class = "compact stripe hover")#, 
    #container = htmltools::tags$div(style = "font-size:12px"))
    
  })
  
  output$estiloTabla_mod_bas_com <- renderUI({
    carac_tabla_func("ui_mod_bas_com", '#FFADB1', '#760312', input$fontSize_mod_bas_com)
  })
  
  df_mod_bas_com <- reactive({
    select_mod_bas_com()
  })
  
  nombre_arc_mod_bas_com <- paste0("Modular_Base Completa ")
  output$descargar_mod_bas_com <- descargar_excel_func(nombre_arc_mod_bas_com, df_mod_bas_com,grupo)
  
  #-------Semana1-------
  select_mod_sem1 <- dselect_func(grupo,datasets_mod_sem1)
  
  output$ui_mod_sem1 <- renderDT({
    data <- select_mod_sem1()
    
    datatable(data, options = list(autoWidth = TRUE),#list(scrollX = TRUE), 
              class = "compact stripe hover")#, 
    #container = htmltools::tags$div(style = "font-size:12px"))
    
  })
  
  output$estiloTabla_mod_sem1 <- renderUI({
    carac_tabla_func("ui_mod_sem1", '#F48FB1','#880E4F', input$fontSize_mod_sem1)
  })
  
  df_mod_sem1 <- reactive({
    select_mod_sem1()
  })
  
  nombre_arc_mod_sem1 <- paste0("Modular_Semana1 ")
  output$descargar_mod_sem1 <- descargar_excel_func(nombre_arc_mod_sem1, df_mod_sem1,grupo)
  
  #-------Semana2-------
  select_mod_sem2 <- dselect_func(grupo,datasets_mod_sem2)
  
  output$ui_mod_sem2 <- renderDT({
    data <- select_mod_sem2()
    
    datatable(data, options = list(autoWidth = TRUE),#list(scrollX = TRUE), 
              class = "compact stripe hover")#, 
    #container = htmltools::tags$div(style = "font-size:12px"))
    
  })
  
  output$estiloTabla_mod_sem2 <- renderUI({
    carac_tabla_func("ui_mod_sem2",'#EBCDF3','#962445', input$fontSize_mod_sem2)
  })
  
  df_mod_sem2 <- reactive({
    select_mod_sem2()
  })
  
  nombre_arc_mod_sem2 <- paste0("Modular_Semana2 ")
  output$descargar_mod_sem2 <- descargar_excel_func(nombre_arc_mod_sem2, df_mod_sem2,grupo)
  
  #-------Semana3-------
  select_mod_sem3 <- dselect_func(grupo,datasets_mod_sem3)
  
  output$ui_mod_sem3 <- renderDT({
    data <- select_mod_sem3()
    
    datatable(data, options = list(autoWidth = TRUE),#list(scrollX = TRUE), 
              class = "compact stripe hover")#, 
    #container = htmltools::tags$div(style = "font-size:12px"))
    
  })
  
  output$estiloTabla_mod_sem3 <- renderUI({
    carac_tabla_func("ui_mod_sem3",'#D1C4E9','#493A92', input$fontSize_mod_sem3)
  })
  
  df_mod_sem3 <- reactive({
    select_mod_sem3()
  })
  
  nombre_arc_mod_sem3 <- paste0("Modular_Semana3 ")
  output$descargar_mod_sem3 <- descargar_excel_func(nombre_arc_mod_sem3, df_mod_sem3,grupo)
  
  #-------Semana4-------
  select_mod_sem4 <- dselect_func(grupo,datasets_mod_sem4)
  
  output$ui_mod_sem4 <- renderDT({
    data <- select_mod_sem4()
    
    datatable(data, options = list(autoWidth = TRUE),#list(scrollX = TRUE), 
              class = "compact stripe hover")#, 
    #container = htmltools::tags$div(style = "font-size:12px"))
    
  })
  
  output$estiloTabla_mod_sem4 <- renderUI({
    carac_tabla_func("ui_mod_sem4",'#A09CC0','#4527A0', input$fontSize_mod_sem4)
  })
  
  df_mod_sem4 <- reactive({
    select_mod_sem4()
  })
  
  nombre_arc_mod_sem4 <- paste0("Modular_Semana4 ")
  output$descargar_mod_sem4 <- descargar_excel_func(nombre_arc_mod_sem4, df_mod_sem4,grupo)
  
  
  #Focalizado--------------------------------------------
  #-------General-------
  select_foc_gen_cumpl <- dselect_func(grupo,datasets_foc_gen_cumpl)
  
  output$tabla_color_foc_gen_cumpl <- renderDT({
    df <-  select_foc_gen_cumpl()
    tabla_gt_func(df)  # Renderiza tabla con colores
  })
  
  df_foc_gen_cumpl <- reactive({
    select_foc_gen_cumpl()
  })
  
  nombre_arc_foc_gen_cumpl <- paste0("Focalizado_Cumplimiento ")
  output$descargar_foc_gen_cumpl <- descargar_excel_func(nombre_arc_foc_gen_cumpl, df_foc_gen_cumpl,grupo)
  
  select_foc_gen_graf <- dselect_func(grupo,datasets_foc_gen_graf)
  
  colors1= c("#8B0000", "#EE0000", "#CD5555")
  output$ui_foc_gen_mb <- ui_foc_gen_func(select_foc_gen_graf,grupo,1,colors1)
  colors2= c("#8B2500", "#FF4500", "#FFA07A")
  output$ui_foc_gen_fne <- ui_foc_gen_func(select_foc_gen_graf,grupo,2,colors2)
  colors3= c("#FFD700", "#FFFF00", "#FFEC8B") 
  output$ui_foc_gen_fe <- ui_foc_gen_func(select_foc_gen_graf,grupo,3,colors3)
  colors4= c("#008B00", "#458B00", "#76EE00")#27408B
  output$ui_foc_gen_fn <- ui_foc_gen_func(select_foc_gen_graf,grupo,4,colors4)
  colors5= c("#1874CD", "#00BFFF", "#00EEEE")
  output$ui_foc_gen_eva <- ui_foc_gen_func(select_foc_gen_graf,grupo,5,colors5)
  colors6= c("#473C8B", "#8B008B", "#8B1C62")
  output$ui_foc_gen_peaa <- ui_foc_gen_func(select_foc_gen_graf,grupo,6,colors6)
  colors7= c("#7A378B", "#8968CD", "#FF6EB4")
  output$ui_foc_gen_eaa <- ui_foc_gen_func(select_foc_gen_graf,grupo,7,colors7)
  
  #-------Cumplimiento AV-------
  select_foc_cumav_rev <- dselect_func(grupo,datasets_foc_cumav_rev)
  
  output$tabla_color_foc_cumav_rev <- renderDT({
    df <-  select_foc_cumav_rev()
    tabla_gt_func(df)  # Renderiza tabla con colores
  })
  
  df_foc_cumav_rev <- reactive({
    select_foc_cumav_rev()
  })
  
  nombre_arc_foc_cumav_rev <- paste0("Focalizado_Cumplimiento_Revisión ")
  output$descargar_foc_cumav_rev <- descargar_excel_func(nombre_arc_foc_cumav_rev, df_foc_cumav_rev,grupo)
  
  select_foc_cumav_avs <- dselect_func(grupo,datasets_foc_cumav_avs)
  
  # Selecciona la tabla de acuerdo al grupo
  output$ui_foc_cumav_avs <- renderDT({
    data <- select_foc_cumav_avs()
    
    datatable(data, options = list(autoWidth = TRUE),#list(scrollX = TRUE),
              class = "compact stripe hover")#,
    #container = htmltools::tags$div(style = "font-size:12px"))
    
  })
  
  output$estiloTabla_foc_cumav_avs <- renderUI({
    carac_tabla_func("ui_foc_cumav_avs",'#9FA8DA','#1A237E', input$fontSize_foc_cumav_avs)
  })
  
  df_foc_cumav_avs <- reactive({
    select_foc_cumav_avs()
  })
  
  nombre_arc_foc_cumav_avs <- paste0("Focalizado_Cumplimiento_AV_regular ")
  output$descargar_foc_cumav_avs <- descargar_excel_func(nombre_arc_foc_cumav_avs, df_foc_cumav_avs,grupo)
  
  #-------Revisión-------
  select_foc_rev <- dselect_func(grupo,datasets_foc_rev)
  
  output$ui_foc_rev <- renderDT({
    data <- select_foc_rev()
    
    datatable(
      data,
      options = list(autoWidth = TRUE),
      class = "compact stripe hover"
    ) %>%
      formatStyle(
        #'Mensaje de Bienvenida',
        c("Mensaje de Bienvenida", "Foro no evaluable", "Foro evaluable", "Foro de novedades", "Evaluación", "Pizarra EAA", "EAA"),
        backgroundColor = styleEqual(
          c("I/I"),
          c("#FF7256")
        ),
        color = "black"  # texto blanco para contrastar, opcional
      )
  })
  
  output$estiloTabla_foc_rev <- renderUI({
    carac_tabla_func("ui_foc_rev",'#95BDED','#00366C', input$fontSize_foc_rev)
  })
  
  df_foc_rev <- reactive({
    select_foc_rev()
  })
  
  nombre_arc_foc_rev <- paste0("Focalizado_Revisión ")
  output$descargar_foc_rev <- descargar_excel_func(nombre_arc_foc_rev, df_foc_rev,grupo)
  
  #-------Seguimiento AV-------
  select_foc_segav <- dselect_func(grupo,datasets_foc_segav)
  
  # Renderizar los selectInput dinámicos basados en el grupo
  output$ui_foc_segav_generacion <- renderUI({
    #grupo_num <- as.numeric(gsub("grupo", "", grupo()))  # Extrae el número del grupo
    
    data <- select_foc_segav()
    
    # if (grupo_num >= 0 && grupo_num <= 23) {
    # Para grupos menores o iguales a 104, mostramos el selectInput normal
    selectInput("Generación", "Selecciona la Generación", choices = unique(data$Generación),
                selected = unique(data$Generación),
                multiple = TRUE)
    # } else if (grupo_num >= 100 && grupo_num <= 106) {
    #   # Para grupo 105 y 106, mostrar las opciones con el prefijo "REC"
    #   generaciones_numeros <- str_extract(unique(data$Generación), "(?<=G)\\d+(?=M)")
    #   opciones_usuario <- paste0("REC", generaciones_numeros)
    #   choices_list <- setNames(unique(data$Generación), opciones_usuario)
    #   
    #   selectInput("Generación", "Selecciona la Generación", 
    #               choices = choices_list,  
    #               selected = unique(data$Generación),
    #               multiple = TRUE)
    # }
  })
  
  # Selecciona la tabla de acuerdo al grupo
  output$ui_foc_segav_tabla <- renderDT({
    data <- select_foc_segav()
    
    # Verifica que el grupo seleccionado sea válido
    if (is.null(data)) {
      return(NULL)  # Si no hay datos para ese grupo, no se muestra nada
    }
    
    # Lógica para decidir cuál tabla se muestra
    if (grupo() %in% c("grupo22", "grupo23", "grupo105", "grupo106")) {  # Si es un grupo MV2
      return(datatable(categorias_mv_2))  # Mostrar categorias_mv_2
    } else {  # Para cualquier otro grupo (MV1)
      return(datatable(categorias_mv_1))  # Mostrar categorias_mv_1
    }
  })
  
  output$estiloTabla_foc_segav_tabla <- renderUI({
    carac_tabla_func("ui_foc_segav_tabla",'#BBDEFB','#0D47A1', input$fontSize_foc_segav_tabla)
  })
  
  output$ui_foc_segav_av <- renderUI({
    data <- select_foc_segav()
    selectInput("AV", "Selecciona el Asesor Virtual", choices = unique(data$AV),
                selected = unique(data$AV),
                multiple = FALSE)
  })
  
  output$ui_foc_segav_semana <- renderUI({
    data <- select_foc_segav()
    checkboxGroupInput("Semana","Selecciona la(s) semana(s):",choices = unique(data$Semana),
                       selected = unique(data$Semana))
  })
  
  output$ui_foc_segav_verificacion <- renderUI({
    data <- select_foc_segav()
    checkboxGroupInput("Verificacion", "Selecciona la Verificación", choices = unique(data$Verificacion), selected = unique(data$Verificacion))
  })
  
  output$ui_foc_segav_porcentaje <- renderUI({
    selectInput("Porcentaje", "Selecciona el %:", choices = c(
      "Incumplimiento" = "porcentaje_no", 
      "Cumplimiento" = "porcentaje_si"))
  })
  
  # Crear el gráfico dinámico basado en los filtros seleccionados
  output$ui_foc_segav_grafico_barras <- renderPlotly({
    # Obtener los datos filtrados
    data <- select_foc_segav()
    
    # Verificamos si los datos existen antes de proceder
    if (is.null(data)) {
      return(NULL)  # Si no hay datos, no renderizamos el gráfico
    }
    
    y_variable <- input$Porcentaje  # Esto es 'porcentaje_no' o 'porcentaje_si'
    
    data_filtrada <- data[data$Generación %in% input$Generación &
                            data$AV %in% input$AV &
                            data$Semana %in% input$Semana &
                            data$Verificacion %in% input$Verificacion , ]
    
    # Si no hay datos, no mostrar gráfico
    if (nrow(data_filtrada) == 0) {
      return(NULL)
    }
    
    # Generamos el título dinámico basado en el grupo y el porcentaje seleccionado
    grupo_num <- as.numeric(gsub("grupo", "", grupo()))  # Extrae el número del grupo
    
    if (y_variable == "porcentaje_no") {
      
      #title_text <- 'Porcentaje de No Cumplimiento de AV por Generación y Método de Verificación'
      yaxis_title <- 'Porcentaje incumplimiento (%)'
      if (grupo_num >= 0 && grupo_num <= 23) {
        title_text <- paste0('Módulo ', grupo_num, '\nPorcentaje de incumplimiento de AV por generación y método de verificación')
      } else if (grupo_num >= 101 && grupo_num <= 104) {
        modulos_bloque <- list(
          '101' = '(Módulos 1, 5, 9, 13, 17, 21)',
          '102' = '(Módulos 2, 6, 10, 14, 18)',
          '103' = '(Módulos 3, 7, 11, 15, 19)',
          '104' = '(Módulos 4, 8, 12, 16, 20)'
        )
        bloque <- grupo_num - 100
        title_text <- paste0('REC Bloque ', bloque, ' ', modulos_bloque[[as.character(grupo_num)]], '\nPorcentaje de incumplimiento de AV por generación y método de verificación')
      } else if (grupo_num == 105) {
        title_text <- 'REC Bloque 2 Módulo 22 \nPorcentaje de incumplimiento de AV por generación y método de verificación'
      } else if (grupo_num == 106) {
        title_text <- 'REC Bloque 3 Módulo 23 \nPorcentaje de incumplimiento de AV por generación y método de verificación'
      }
      
    } else if (y_variable == "porcentaje_si") {
      
      #title_text <- 'Porcentaje de Cumplimiento de AV por Generación y Método de Verificación'
      yaxis_title <- 'Porcentaje de Cumplimiento (%)'
      if (grupo_num >= 0 && grupo_num <= 23) {
        title_text <- paste0('Módulo ', grupo_num, '\nPorcentaje de cumplimiento de AV por generación y método de verificación')
      } else if (grupo_num %in% 101:104) {
        bloque <- grupo_num - 100
        modulos_text <- switch(
          as.character(grupo_num),
          '101' = '(Módulos 1, 5, 9, 13, 17, 21)',
          '102' = '(Módulos 2, 6, 10, 14, 18)',
          '103' = '(Módulos 3, 7, 11, 15, 19)',
          '104' = '(Módulos 4, 8, 12, 16, 20)'
        )
        title_text <- paste0('REC Bloque ', bloque, ' ', modulos_text, '\nPorcentaje de cumplimiento de AV por Generación y Método de verificación')
      } else if (grupo_num == 105) {
        title_text <- 'REC Bloque 2 Módulo 22 \nPorcentaje de cumplimiento de AV por generación y método de verificación'
      } else if (grupo_num == 106) {
        title_text <- 'REC Bloque 3 Módulo 23 \nPorcentaje de cumplimiento de AV por generación y método de verificación'
      }
      
    } else {
      title_text <- 'Gráfico'
      yaxis_title <- 'Porcentaje'
    }
    
    #Crear el gráfico de barras apiladas
    plot_ly(data_filtrada,
            x = ~IDReactivo,
            y = as.formula(paste("~", input$Porcentaje)),
            color = ~paste(Generación, Verificacion, Semana),
            text = ~paste(Generación, AV, Semana, Verificacion),
            hoverinfo = 'text+y',
            type = 'bar') %>%
      layout(
        barmode = 'stack',
        title = title_text,
        yaxis = list(title = yaxis_title),
        xaxis = list(title = 'ID Reactivo'),
        legend = list(title = list(text = "Generación")),
        margin = list(t = 100)
      )
  })
  
  #-------Mensaje de Bienvenida-------
  select_foc_mensaje_bienvenida <- dselect_func(grupo,datasets_foc_mensaje_bienvenida)
  
  output$ui_foc_mensaje_bienvenida <- renderDT({
    data <- select_foc_mensaje_bienvenida()
    
    datatable(data, options = list(autoWidth = TRUE),#list(scrollX = TRUE), 
              class = "compact stripe hover")#, 
    #container = htmltools::tags$div(style = "font-size:12px"))
    
  })
  
  output$estiloTabla_foc_mensaje_bienvenida <- renderUI({
    carac_tabla_func("ui_foc_mensaje_bienvenida",'#B2DFDB','#006064', input$fontSize_foc_mensaje_bienvenida)
  })
  
  df_foc_mensaje_bienvenida <- reactive({
    select_foc_mensaje_bienvenida()
  })
  
  nombre_arc_foc_mensaje_bienvenida <- paste0("Focalizado_Mensaje de bienvenida ")
  output$descargar_foc_mensaje_bienvenida <- descargar_excel_func(nombre_arc_foc_mensaje_bienvenida, df_foc_mensaje_bienvenida,grupo)
  
  #-------Foro no evaluable-------
  select_foc_foro_no_evaluable <- dselect_func(grupo,datasets_foc_foro_no_evaluable)
  
  output$ui_foc_foro_no_evaluable <- renderDT({
    data <- select_foc_foro_no_evaluable()
    
    datatable(data, options = list(autoWidth = TRUE),#list(scrollX = TRUE), 
              class = "compact stripe hover")#, 
    #container = htmltools::tags$div(style = "font-size:12px"))
    
  })
  
  output$estiloTabla_foc_foro_no_evaluable <- renderUI({
    carac_tabla_func("ui_foc_foro_no_evaluable",'#C8E6C9','#1B5E20', input$fontSize_foc_foro_no_evaluable)
  })
  
  df_foc_foro_no_evaluable <- reactive({
    select_foc_foro_no_evaluable()
  })
  
  nombre_arc_foc_foro_no_evaluable <- paste0("Focalizado_Foro no evaluable ")
  output$descargar_foc_foro_no_evaluable <- descargar_excel_func(nombre_arc_foc_foro_no_evaluable, df_foc_foro_no_evaluable,grupo)
  #-------Foro evaluable-------
  select_foc_foro_evaluable <- dselect_func(grupo,datasets_foc_foro_evaluable)
  
  output$ui_foc_foro_evaluable <- renderDT({
    data <- select_foc_foro_evaluable()
    
    datatable(data, options = list(autoWidth = TRUE),#list(scrollX = TRUE), 
              class = "compact stripe hover")#, 
    #container = htmltools::tags$div(style = "font-size:12px"))
    
  })
  
  output$estiloTabla_foc_foro_evaluable <- renderUI({
    carac_tabla_func("ui_foc_foro_evaluable",'#DCEDC8','#33691E', input$fontSize_foc_foro_evaluable)
  })
  
  df_foc_foro_evaluable <- reactive({
    select_foc_foro_evaluable()
  })
  
  nombre_arc_foc_foro_evaluable <- paste0("Focalizado_Foro evaluable ")
  output$descargar_foc_foro_evaluable <- descargar_excel_func(nombre_arc_foc_foro_evaluable, df_foc_foro_evaluable,grupo)
  
  #-------Foro novedades-------
  select_foc_foro_novedades <- dselect_func(grupo,datasets_foc_foro_novedades)
  
  output$ui_foc_foro_novedades <- renderDT({
    data <- select_foc_foro_novedades()
    
    datatable(data, options = list(autoWidth = TRUE),#list(scrollX = TRUE), 
              class = "compact stripe hover")#, 
    #container = htmltools::tags$div(style = "font-size:12px"))
    
  })
  
  output$estiloTabla_foc_foro_novedades <- renderUI({
    carac_tabla_func("ui_foc_foro_novedades",'#FFF59D','#FFCC00', input$fontSize_foc_foro_novedades)
  })
  
  df_foc_foro_novedades <- reactive({
    select_foc_foro_novedades()
  })
  
  nombre_arc_foc_foro_novedades <- paste0("Focalizado_Foro novedades ")
  output$descargar_foc_foro_novedades <- descargar_excel_func(nombre_arc_foc_foro_novedades, df_foc_foro_novedades,grupo)
  
  #-------Evaluación-------
  select_foc_evaluacion <- dselect_func(grupo,datasets_foc_evaluacion)
  
  output$ui_foc_evaluacion <- renderDT({
    data <- select_foc_evaluacion()
    
    datatable(data, options = list(autoWidth = TRUE),#list(scrollX = TRUE), 
              class = "compact stripe hover")#, 
    #container = htmltools::tags$div(style = "font-size:12px"))
    
  })
  
  output$estiloTabla_foc_evaluacion <- renderUI({
    carac_tabla_func("ui_foc_evaluacion",'#FFCC80','#FF6F00', input$fontSize_foc_evaluacion)
  })
  
  df_foc_evaluacion <- reactive({
    select_foc_evaluacion()
  })
  
  nombre_arc_foc_evaluacion <- paste0("Focalizado_Evaluación ")
  output$descargar_foc_evaluacion <- descargar_excel_func(nombre_arc_foc_evaluacion, df_foc_evaluacion,grupo)
  
  #-------Pizarra EAA-------
  select_foc_pizarra_eaa <- dselect_func(grupo,datasets_foc_pizarraEAA)
  
  output$ui_foc_pizarra_eaa <- renderDT({
    data <- select_foc_pizarra_eaa()
    
    datatable(data, options = list(autoWidth = TRUE),#list(scrollX = TRUE), 
              class = "compact stripe hover")#, 
    #container = htmltools::tags$div(style = "font-size:12px"))
    
  })
  
  output$estiloTabla_foc_pizarra_eaa <- renderUI({
    carac_tabla_func("ui_foc_pizarra_eaa",'#FFCCBC','#BF360C', input$fontSize_foc_pizarra_eaa)
  })
  
  df_foc_pizarra_eaa <- reactive({
    select_foc_pizarra_eaa()
  })
  
  nombre_arc_foc_pizarra_eaa <- paste0("Focalizado_Pizarra EAA ")
  output$descargar_foc_pizarra_eaa <- descargar_excel_func(nombre_arc_foc_pizarra_eaa, df_foc_pizarra_eaa,grupo)
  
  #-------EAA-------
  select_foc_eaa <- dselect_func(grupo,datasets_foc_EAA)
  
  output$ui_foc_eaa <- renderDT({
    data <- select_foc_eaa()
    
    datatable(data, options = list(autoWidth = TRUE),#list(scrollX = TRUE), 
              class = "compact stripe hover")#, 
    #container = htmltools::tags$div(style = "font-size:12px"))
    
  })
  
  output$estiloTabla_foc_eaa <- renderUI({
    carac_tabla_func("ui_foc_eaa",'#D8AF97','#662F00', input$fontSize_foc_eaa)
  })
  
  df_foc_eaa <- reactive({
    select_foc_eaa()
  })
  
  nombre_arc_foc_eaa <- paste0("Focalizado_EAA ")
  output$descargar_foc_eaa <- descargar_excel_func(nombre_arc_foc_eaa, df_foc_eaa,grupo)
  
}

shinyApp(ui, server)
