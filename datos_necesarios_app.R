dselect_func <- function(grupo,dataset) {
  
  data_seleccionada <- reactive({
    grupo_value <- grupo() # Verificamos que 'grupo()' sea uno de los valores esperados
    grupo_num <- as.numeric(gsub("grupo", "", grupo_value))  # Extrae el número del grupo
    if (!is.na(grupo_num) && grupo_num >= 0 && grupo_num <= 107) {
      return(dataset[[grupo_value]])  # Accede dinámicamente al grupo correspondiente
    } else {
      return(NULL)  # En caso de que el valor de grupo no sea válido
    }
  })
  return(data_seleccionada)
}

ui_foc_gen_func <- function(df,grup,num_col,colores) {
  
  # output$ui_foc_gen <- renderPlotly({
  renderPlotly({
    data <- df()
    
    grupo_num <- as.numeric(gsub("grupo", "", grup()))  # Extrae el número del grup
    
    req(data)
    
    #colores= c("#1f77b4", "#ff7f0e", "#2ca02c")
    etiquetas <- as.character(colnames(data)[2:4])
    valores <- as.numeric(data[num_col, 2:4])
    
    plot_ly(
      labels = etiquetas,
      values = valores,
      type = "pie",
      insidetextorientation = "radial",
      insidetextorientation = "radial",
      textinfo = "label+percent",#"none",  # 👈 no repite texto
      hoverinfo = "text",  # 👈 solo muestra tu texto personalizado
      text = ~paste0(etiquetas, ": ", valores, "%"),
      marker = list(colors = colores)
    ) %>%
      layout(title= list(text = paste("Distribución de", as.character(data[num_col,1])),y = 0.95),
             xaxis = list(cliponaxis = FALSE),
             yaxis = list(cliponaxis = FALSE),
             margin = list(t = 120))
  })
}

# carac_tabla_func <- function(bg_color, f_color, input_fontSize) {
#   tags$style(HTML(sprintf("
#     table.dataTable td {
#       font-size: %dpx;
#     }
#     table.dataTable th {
#       font-size: %dpx;
#       background-color: %s;
#       color: %s;
#     }
#   ", input_fontSize, input_fontSize, bg_color, f_color)))
# }

carac_tabla_func <- function(table_id, bg_color, f_color, input_fontSize) {
  tags$style(HTML(sprintf("
    #%s td {
      font-size: %dpx;
    }
    #%s th {
      font-size: %dpx;
      background-color: %s;
      color: %s;
    }
  ", table_id, input_fontSize, table_id, input_fontSize, bg_color, f_color)))
}

tabla_gt_func <- function(data) {
  #col_grupos <- names(data)[grep("Grupos", names(data))]
  col_grupos1 <- names(data)[grep("^REC", names(data))]
  col_grupos2 <- names(data)[grep("^G", names(data))]
  col_grupos  <- c(col_grupos1, col_grupos2)  # Combinar ambas listas
  col_nivel  <- names(data)[grep("Nivel de Cumplimiento", names(data))]
  
  datatable(
    data,
    options = list(autoWidth = TRUE),
    class = "compact stripe hover"
  ) %>%
    formatStyle(
      columns = col_grupos,
      valueColumns = col_nivel,  # <- Aquí defines la columna de referencia
      #'Nivel de cumplimiento',
      backgroundColor = styleEqual(
        c("Muy bueno", "Bueno", "Regular", "Malo", "Muy malo"),
        c("#2D8B3F", "#78AE6D","#F4D166","#F9866F",'#D2241F'),
        #c("#008B00", "green","yellow","red",'#CD0000')
      ),
      color = "white"  # texto blanco para contrastar, opcional
    )
}

nombre_m_archivos_func <- function(num) {
  if (num >= 0 && num <= 23) {
    nom_mod_descarga <- paste0('M',num)
  } else if (num >= 100 && num < 105){
    nom_mod_descarga <- paste0('REC Bloque', num-100)
  } else if (num == 105){
    nom_mod_descarga <- paste0('REC Módulo 22')
  } else if (num == 106){
    nom_mod_descarga <- paste0('REC Módulo 23')
  }
  return(nom_mod_descarga)
}

descargar_excel_func <- function(nombre, df_reactivo, grupo_reactivo) {
  downloadHandler(
    filename = function() {
      grupo_num <- as.numeric(gsub("grupo", "", grupo_reactivo()))
      nom_mod_descarga <- nombre_m_archivos_func(grupo_num)
      paste0(nombre, nom_mod_descarga, ".xlsx")
    },
    content = function(file) {
      write.xlsx(df_reactivo(), file)
    }
  )
}


Tema_mod <- data.frame(modulo=c(0:23, 101:106), tema=c("Módulo 00. Propedéutico","Módulo 01. Tecnología de la información y comunicación","Módulo 02. De la información al conocimiento","Módulo 03. El lenguaje en relación del hombre con el mundo","Módulo 04. Textos y visiones del mundo","Módulo 05. Argumentación","Módulo 06. Mi mundo en otra lengua","Módulo 07. Mi vida en otra lengua","Módulo 08. Ser social y sociedad","Módulo 09. Sociedad mexicana contemporánea","Módulo 10. Transformaciones en el mundo contemporáneo","Módulo 11. Representaciones simbólicas y algoritmos","Módulo 12. Matemáticas y representaciones del sistema natural","Módulo 13. Variación en procesos sociales","Módulo 14. Universo natural","Módulo 15. Hacia un desarrollo sustentable","Módulo 16. Evolución y sus repercusiones sociales","Módulo 17. Estadística en fenómenos naturales y procesos sociales","Módulo 18. Cálculo en fenómenos naturales y procesos sociales","Módulo 19. Dinámica en la naturaleza: el movimiento","Módulo 20. Optimización en sistemas naturales y sociales","Módulo 21. Impacto de la ciencia y tecnología","Módulo 22. Tecnologías emergentes en la solución de problemas","Módulo 23. Tecnologías emergentes para la administración y gestión", rep("Recursamiento", times=6)))

save.image("datos_necesarios_app.RData")
