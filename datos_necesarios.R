#----------------------------------------------------------------
#       Código para compilar datos para el reporte
#           "Apoyo a tu seguimiento modular"
# Contempla generaciones cerradas y cumplimiento por semana
#                 con información de los AV
#                         Krýstal
#           Parte 1: environment necesario
#---------------------------------------------------------------
rm(list = ls())

if(!require(pacman)){
  install.packages('pacman')
};
library(pacman)
#library(readr)
pacman::p_load(here,tidyverse, openxlsx, readr,dplyr, stringr,tidyr, purrr, rsconnect, magrittr, rlang, DT,shinythemes)
date_dat_nec <- "-22-07-25"

# ---- Crear directorios ----
##crea los directorios para base de datos y productos
#directorio de insumos
dir_insumos<- paste0(here(),'/INSUMOS')
if (!dir.exists(dir_insumos)) {
  dir.create(dir_insumos)
}

dir_insumos_SAME<- paste0(here(),'/INSUMOS_SAME')
if (!dir.exists(dir_insumos_SAME)) {
  dir.create(dir_insumos_SAME)
}
#directorio productos
dir_productos<- paste0(here(),'/PRODUCTOS')
if (!dir.exists(dir_productos)) {
  dir.create(dir_productos)
}
#Analiza los archivos en el directorio y los clasifica segun su tipo
#DF de archivos
archivos<-data.frame(archivos = list.files(here()))
# #busca dentro de los archivos
# for (archivo in archivos[,1]) {
#   if (grepl('\\.(xlsx)$', archivo,ignore.case = TRUE)) {
#     file.rename(paste0(here(), "/", archivo), paste0(dir_insumos, "/", archivo))
#   }
# }
#Para organizar archivos de REC
asignar_bloque <- function(numero, parte4) {
  # Lógica para MV1 (del 101 al 104)
  if (parte4 == "V1") {
    return(100 + ((numero - 2) %% 4) + 1)
  }
  # Lógica para MV2 (del 105 al 106, solo si el número es 3 o 4)
  else if (parte4 == "V2") {
    bloque <- (100 + ((numero - 2) %% 4) + 1)
    if (bloque == 102) {
      return(105)
    } else if (bloque == 103) {
      return(106)
    }
  }
  # Por defecto, se devuelve el bloque normal (esto es por si hay otros casos)
  # return(100 + ((numero - 2) %% 4) + 1)
}

for (archivo in archivos[,1]) { #Diferenciar entre archivos de Asignación General e importables
  if (grepl('\\.(xlsx)$', archivo,ignore.case = TRUE)) {
    if (grepl('AsignacionGeneral', archivo,ignore.case = TRUE)) {
      
      file.rename(paste0(here(), "/", archivo), paste0(dir_insumos_SAME, "/", archivo))
      #print(paste0(dir_insumos_SAME, "/", archivo))
      
    } else {
      
      if (grepl('REC', archivo,ignore.case = TRUE)) {
        parte1 <- str_extract(archivo, "Asignacion_REC")  # "Asignacion_REC "
        parte2 <- str_extract(archivo, "(?<=REC )\\d+")  # Número después de "REC "
        parte3 <- str_extract(archivo, "(?<=\\d{3} ).*?(?= MV)")  # Extrae " Semana_2 "
        parte4 <- str_extract(archivo, "V[12]")  # Extrae "V1" o "V2" #"(?<=Semana_\\d+ )\\w+")
        parte5 <- str_extract(archivo, "(\\d{2}-\\d{2}-\\d{4})( \\(\\d+\\))?\\.xlsx$")  # 
        
        # Aquí se pasa parte4 a la función asignar_bloque
        nuevo_nombre <- paste0(dir_insumos, "/",parte1, "_G", parte2, "M",asignar_bloque(as.numeric(str_extract(archivo, "\\d+")), parte4),  # Se pasa parte4
                               " ", parte3, " ", parte5)
        file.rename(paste0(here(), "/", archivo), nuevo_nombre)
        #print(nuevo_nombre)
      } else {
        file.rename(paste0(here(), "/", archivo), paste0(dir_insumos, "/", archivo))
        #print(paste0(dir_insumos, "/", archivo))
      }
      
    }
  }
}

bases <- data.frame(nombre = list.files(dir_insumos))  
bases_SAME <- data.frame(nombre = list.files(dir_insumos_SAME))

Modulos <- str_extract(bases$nombre, "(?<=M)\\d+(?= Semana)")
# str_extract(bases$nombre, "(?<=Asignacion_).+?(?= Semana)")
#unique(str_extract(bases$nombre, "(?<=M)\\d+(?= Semana)"))
N.Mod <- length(Modulos)

Tema_mod <- data.frame(modulo=c(0:23, 101:106), tema=c("Módulo 00. Propedéutico","Módulo 01. Tecnología de la información y comunicación","Módulo 02. De la información al conocimiento","Módulo 03. El lenguaje en relación del hombre con el mundo","Módulo 04. Textos y visiones del mundo","Módulo 05. Argumentación","Módulo 06. Mi mundo en otra lengua","Módulo 07. Mi vida en otra lengua","Módulo 08. Ser social y sociedad","Módulo 09. Sociedad mexicana contemporánea","Módulo 10. Transformaciones en el mundo contemporáneo","Módulo 11. Representaciones simbólicas y algoritmos","Módulo 12. Matemáticas y representaciones del sistema natural","Módulo 13. Variación en procesos sociales","Módulo 14. Universo natural","Módulo 15. Hacia un desarrollo sustentable","Módulo 16. Evolución y sus repercusiones sociales","Módulo 17. Estadística en fenómenos naturales y procesos sociales","Módulo 18. Cálculo en fenómenos naturales y procesos sociales","Módulo 19. Dinámica en la naturaleza: el movimiento","Módulo 20. Optimización en sistemas naturales y sociales","Módulo 21. Impacto de la ciencia y tecnología","Módulo 22. Tecnologías emergentes en la solución de problemas","Módulo 23. Tecnologías emergentes para la administración y gestión", rep("Recursamiento", times=6)))

#Compilado con datos de AV
contador <- 0
contador.m <- 0

categorias_mv_completo <- data.frame(IDReactivo = c("199","201", "200", "203", "202", "204", "190", "189", "193", "194", "191","192", "195", "196", "197", "198", "184", "186","182", "185", "187", "183", "188", "180", "181", "...19", "...24", "...13", "...18", "...22", "...16", "...26", "...15", "...21", "...20", "...12", "...17"),
                                     Reactivo = c("¿Publicó un mensaje de bienvenida, en el foro de Novedades, el primer día del módulo?",
                                                  "En el mensaje de bienvenida que publicó en el foro de Novedades: [¿Indicó los medios para comunicarse con él?]",
                                                  "En el mensaje de bienvenida que publicó en el foro de Novedades: [¿Señaló los horarios en los que estará disponible para establecer comunicación en la plataforma?]",
                                                  "En el mensaje de bienvenida que publicó en el foro de Novedades: [¿Señaló el tiempo máximo que se demoraría en contestar dudas?]",
                                                  "En el mensaje de bienvenida que publicó en el foro de Novedades: [¿Mencionó las reglas de comunicación virtual?]",
                                                  "En el mensaje de bienvenida que publicó en el foro de Novedades: [¿Describió la dinámica durante el módulo y entregables: plazos para la entrega de actividades, tiempos de evaluación y retroalimentación?]",
                                                  "¿Publicó al menos un mensaje diario relacionado con el propósito del foro o la participación de los estudiantes, en los foros no evaluables?",
                                                  "¿Publicó al menos un mensaje diario relacionado con el propósito del foro o la participación de los estudiantes, en los foros evaluables?",
                                                  "¿Generó una conclusión de los foros evaluables el día viernes, que resume las aportaciones de los estudiantes y el contenido temático del foro?",
                                                  "¿Exhortó el día viernes a los estudiantes que no han comentado en los foros evaluables a participar?",
                                                  "¿Publicó en el foro de Novedades, el lunes de cada semana, una invitación para participar en el foro Aprendiendo?",
                                                  "¿Publicó en el foro de Novedades, el lunes de cada semana, una invitación para participar en el foro de Clase?",
                                                  "¿Registró las calificaciones de las actividades y proyectos integradores dentro de los plazos establecidos para cada semana a partir de la entrega realizada por los estudiantes?",
                                                  "¿Registró las calificaciones de los foros evaluables, en los plazos establecidos en el Manual del Asesor Virtual, después del cierre del foro?",
                                                  "¿Retroalimentó todas las actividades y proyectos integradores dentro de los plazos señalados en el Manual del Asesor Virtual?",
                                                  "¿Retroalimentó a todos los estudiantes que participaron en los foros evaluables, dentro de los plazos establecidos en el Manual del Asesor Virtual, a partir del cierre del foro?",
                                                  "¿Publicó, en la pizarra de sesiones, la invitación con el enlace a la sesión de espacio abierto para el aprendizaje entre 24 y 48 horas de anticipación? [Miércoles]",
                                                  "¿Incluyó la fecha y hora en el mensaje de invitación a la sesión de espacio abierto para el aprendizaje? [Miércoles]",
                                                  "¿Publicó en la pizarra de sesiones el enlace que reproduce la grabación de la sesión de espacio abierto para el aprendizaje el mismo día de su realización? [Miércoles]",
                                                  "¿Publicó, en la pizarra de sesiones, la invitación con el enlace a la sesión de espacio abierto para el aprendizaje entre 24 y 48 horas de anticipación? [Viernes]",
                                                  "¿Incluyó la fecha y hora en el mensaje de invitación a la sesión de espacio abierto para el aprendizaje? [Viernes]",
                                                  "¿Publicó en la pizarra de sesiones el enlace que reproduce la grabación de la sesión de espacio abierto para el aprendizaje el mismo día de su realización? [Viernes]",
                                                  "¿Precisó el propósito de las sesiones de espacio abierto para el aprendizaje en los mensajes de invitación a éstas?",
                                                  "¿Realizó la sesión de espacio abierto para el aprendizaje el día miércoles con una duración mínima de 45 min.?",
                                                  "¿Realizó la sesión de espacio abierto para el aprendizaje el día viernes con una duración miníma de 45 min.?",
                                                  "DURACIÓN DE LA SESIÓN (min.)",
                                                  "DURACIÓN DE LA SESIÓN (min.)",
                                                  "DURACIÓN DE LA SESIÓN (min.)",
                                                  "DURACIÓN DE LA SESIÓN (min.)",
                                                  "DURACIÓN DE LA SESIÓN (min.)",
                                                  "DURACIÓN DE LA SESIÓN (min.)",
                                                  "DURACIÓN DE LA SESIÓN (min.)",
                                                  "DURACIÓN DE LA SESIÓN (min.)",
                                                  "DURACIÓN DE LA SESIÓN (min.)",
                                                  "DURACIÓN DE LA SESIÓN (min.)",
                                                  "DURACIÓN DE LA SESIÓN (min.)",
                                                  "DURACIÓN DE LA SESIÓN (min.)"),
                                     Verificacion = rep(c("Mensaje de Bienvenida", "Foro no evaluable", "Foro evaluable", "Foro de novedades", "Evaluación", "Pizarra EAA", "EAA", "Duración"), times = c(6, 1, 3, 2, 4, 7, 2, 12)))

categorias_mv_1 <- categorias_mv_completo[!(
  categorias_mv_completo$Verificacion %in% c("Duración") |
    categorias_mv_completo$IDReactivo %in% c("192")), ]

categorias_mv_2 <- categorias_mv_completo[!(
  categorias_mv_completo$Verificacion %in% c("Duración") |
    categorias_mv_completo$IDReactivo %in% c("191")), ]

# Observaciones_mv1 <- rep(c(1,4,1,4,1,4,1,4), times = c(6,1,3,2,1,1,1,9))
# Observaciones_mv2 <- rep(c(1,3,3,4,3,4,3,4), times = c(7,3,1,1,1,1,1,9))
# categorias_mv_1 <- cbind(categorias_mv_1,Observaciones = Observaciones_mv1)
# categorias_mv_2 <- cbind(categorias_mv_2,Observaciones = Observaciones_mv2)

safe_bind_cols <- function(df1, df2) {
  n1 <- nrow(df1)
  n2 <- nrow(df2)
  n_max <- max(n1, n2)
  
  # Rellenar df1 si tiene menos filas
  if (n1 < n_max) {
    df1[(n1 + 1):n_max, ] <- NA
  }
  
  # Rellenar df2 si tiene menos filas
  if (n2 < n_max) {
    df2[(n2 + 1):n_max, ] <- NA
  }
  
  # Combinar
  bind_cols(df1, df2)
}

asignar_reactivo <- function(df) {
  
  cols_con_sufijo <- grep("\\.s[1-4]$", names(df), value = TRUE)
  nombres_sin_sufijo <- sub("\\.s[1-4]$", "", cols_con_sufijo)
  
  cols_con_sufijo <- grep("\\.s[1-4]$", names(df), value = TRUE)
  nombres_sin_sufijo <- sub("\\.s[1-4]$", "", cols_con_sufijo)
  
  valores_para_nueva_fila <- sapply(nombres_sin_sufijo, function(nom) {
    val <- categorias_mv_completo$Reactivo[categorias_mv_completo$IDReactivo == nom]
    if(length(val) == 0) NA else val
  })
  
  nueva_fila <- vector("list", length = length(names(df)))
  names(nueva_fila) <- names(df)
  nueva_fila[cols_con_sufijo] <- valores_para_nueva_fila
  otras_cols <- setdiff(names(df), cols_con_sufijo)
  nueva_fila[otras_cols] <- NA
  
  df <- rbind(df, nueva_fila)
  
  df <- df[c(nrow(df),1:(nrow(df)-1)), ]
  rownames(df) <- NULL
  return(df)
}

data_frames_config_MV1 <- list(
  mensaje_bienvenida = list(df = data.frame(), columnas_seleccionadas = c("199.s1","201.s1", "200.s1", "203.s1", "202.s1", "204.s1")),
  foro_no_evaluable = list(df = data.frame(), columnas_seleccionadas = c("190.s1", "190.s2", "190.s3", "190.s4")),
  foro_evaluable = list(df = data.frame(), columnas_seleccionadas = c("189.s4", "193.s4", "194.s4")),
  foro_novedades = list(df = data.frame(), columnas_seleccionadas = c("191.s1", "191.s2", "191.s3", "191.s4")),
  evaluacion = list(df = data.frame(), columnas_seleccionadas = c("195.s1", "195.s2", "195.s3", "195.s4", "196.s4", "197.s1", "197.s2", "197.s3", "197.s4", "198.s4")),
  pizarraEAA = list(df = data.frame(), columnas_seleccionadas = c("184.s1", "184.s2", "184.s3", "184.s4", "186.s1", "186.s2", "186.s3", "186.s4","182.s1", "182.s2", "182.s3", "182.s4", "185.s1", "185.s2", "185.s3", "185.s4", "187.s1", "187.s2", "187.s3", "187.s4", "183.s1", "183.s2", "183.s3", "183.s4", "188.s1", "188.s2", "188.s3", "188.s4")),
  EAA = list(df = data.frame(), columnas_seleccionadas = c("180.s1", "180.s2", "180.s3", "180.s4", "181.s1", "181.s2", "181.s3", "181.s4"))
)

data_frames_config_MV2 <- list(
  mensaje_bienvenida = list(df = data.frame(), columnas_seleccionadas = c("199.s1","201.s1", "200.s1", "203.s1", "202.s1", "204.s1")),
  foro_no_evaluable = list(df = data.frame(), columnas_seleccionadas = c("190.s4")),
  foro_evaluable = list(df = data.frame(), columnas_seleccionadas = c("189.s1","189.s2","189.s3","193.s1","193.s2", "193.s3", "194.s1", "194.s2", "194.s3")),
  foro_novedades = list(df = data.frame(), columnas_seleccionadas = c("192.s1", "192.s2", "192.s3")),
  evaluacion = list(df = data.frame(), columnas_seleccionadas = c("195.s1", "195.s2", "195.s3", "195.s4", "196.s1", "196.s2", "196.s3", "197.s1", "197.s2", "197.s3", "197.s4", "198.s1", "198.s2", "198.s3")),
  pizarraEAA = list(df = data.frame(), columnas_seleccionadas = c("184.s1", "184.s2", "184.s3", "184.s4", "186.s1", "186.s2", "186.s3", "186.s4","182.s1", "182.s2", "182.s3", "182.s4", "185.s1", "185.s2", "185.s3", "185.s4", "187.s1", "187.s2", "187.s3", "187.s4", "183.s1", "183.s2", "183.s3", "183.s4", "188.s1", "188.s2", "188.s3", "188.s4")),
  EAA = list(df = data.frame(), columnas_seleccionadas = c("180.s1", "180.s2", "180.s3", "180.s4", "181.s1", "181.s2", "181.s3", "181.s4"))
)

data_frames_config_MV1_s3 <- lapply(data_frames_config_MV1, function(item) {
  columnas_filtradas <- item$columnas_seleccionadas[!grepl("\\.s4$", item$columnas_seleccionadas)]
  list(df = item$df, columnas_seleccionadas = columnas_filtradas)
})

data_frames_config_MV2_s3 <- lapply(data_frames_config_MV2, function(item) {
  columnas_filtradas <- item$columnas_seleccionadas[!grepl("\\.s4$", item$columnas_seleccionadas)]
  list(df = item$df, columnas_seleccionadas = columnas_filtradas)
})

data_frames_config_MV1_s2 <- lapply(data_frames_config_MV1_s3, function(item) {
  columnas_filtradas <- item$columnas_seleccionadas[!grepl("\\.s3$", item$columnas_seleccionadas)]
  list(df = item$df, columnas_seleccionadas = columnas_filtradas)
})

data_frames_config_MV2_s2 <- lapply(data_frames_config_MV2_s3, function(item) {
  columnas_filtradas <- item$columnas_seleccionadas[!grepl("\\.s3$", item$columnas_seleccionadas)]
  list(df = item$df, columnas_seleccionadas = columnas_filtradas)
})

data_frames_config_MV1_s1 <- lapply(data_frames_config_MV1_s2, function(item) {
  columnas_filtradas <- item$columnas_seleccionadas[!grepl("\\.s2$", item$columnas_seleccionadas)]
  list(df = item$df, columnas_seleccionadas = columnas_filtradas)
})

data_frames_config_MV2_s1 <- lapply(data_frames_config_MV2_s2, function(item) {
  columnas_filtradas <- item$columnas_seleccionadas[!grepl("\\.s2$", item$columnas_seleccionadas)]
  list(df = item$df, columnas_seleccionadas = columnas_filtradas)
})

obser_MV1 <- list(mensaje_bienvenida = 6,foro_no_evaluable = 4,foro_evaluable = 3,foro_novedades = 4, evaluacion = 10,pizarraEAA = 28,EAA = 8)

obser_MV2 <- list(mensaje_bienvenida = 6,foro_no_evaluable = 1,foro_evaluable = 9,foro_novedades = 3,evaluacion = 14,pizarraEAA = 28,EAA = 8)

categorias_mv <- unique(categorias_mv_completo[,"Verificacion"])
columnas_frec_porc <- c("Porcentaje_s","Porcentaje_n","Porcentaje_na")
# calcular_frec_porc <- function(df){
#   resultados <- map_dfr(categorias_mv_func$IDReactivo, function(prefijo) {
#     # Seleccionamos solo las columnas necesarias para los cálculos
#     df_filtrado <- df %>%
#       select(Grupo, starts_with(prefijo)) %>%  # Filtramos las columnas que empiezan con el prefijo
#       mutate(
#         'Total de respuestas' = ncol(select(df, starts_with(prefijo))),
#         No = rowSums(across(everything(), ~ grepl("^No$", iconv(., to = "ASCII//TRANSLIT"), ignore.case = TRUE))),
#         Sí = rowSums(across(everything(), ~ grepl("^Si$", iconv(., to = "ASCII//TRANSLIT"), ignore.case = TRUE))),
#         'Porcentaje No' = round(((No / ncol(select(df, starts_with(prefijo)))) * 100),2),
#         'Porcentaje Sí' = round(((Sí / ncol(select(df, starts_with(prefijo)))) * 100),2)
#       )
#     
#     # Ahora agregamos las columnas adicionales (AV, Folio, Correo) con left_join
#     df_filtrado <- df_filtrado %>%
#       left_join(select(df, Generación, Grupo, AV, Folio, Correo), by = "Grupo") %>%
#       select(Generación, Grupo, AV, Folio, Correo, 'Total de respuestas', No, Sí, 'Porcentaje No', 'Porcentaje Sí') %>%
#       mutate(IDReactivo = prefijo, 
#              Reactivo = categorias_mv_func$Reactivo[categorias_mv_func$IDReactivo == prefijo],
#              Verificacion = categorias_mv_func$Verificacion[categorias_mv_func$IDReactivo == prefijo]) %>% # Agregar la columna Verificacion
#       filter(Verificacion != "Duración") #Eliminar reactivos de Duración
#     
#     return(df_filtrado)
#   })
#   return(resultados)
# }

# calcular_frec_porc1 <- function(df) {
#   
#   resultados <- map_dfr(categorias_mv_func$IDReactivo, function(prefijo) {
#     # Filtrar las columnas que empiezan con el prefijo
#     columnas_prefijo <- select(df, starts_with(prefijo))
#     
#     # Si no hay columnas con ese prefijo, saltar al siguiente
#     if (ncol(columnas_prefijo) == 0) {
#       return(NULL)
#     }
#     # Seleccionamos solo las columnas necesarias para los cálculos
#     df_filtrado <- df %>%
#       select(Grupo, starts_with(prefijo))  # Filtramos las columnas que empiezan con el prefijo
#     #print(paste0("Analizando recativo ", prefijo))
#     df_filtrado <- df_filtrado %>%
#       pivot_longer(cols = starts_with(prefijo), names_to = "Columna", values_to = "Valor") %>%
#       mutate(
#         Semana = paste0("Semana ", gsub(".*\\.s(\\d+).*", "\\1", Columna)),  # Extraemos el número después de ".s"
#         'Total de respuestas' = ncol(select(df, starts_with(prefijo))),
#         No = ifelse(grepl("^No$", iconv(Valor, to = "ASCII//TRANSLIT"), ignore.case = TRUE), 1, 0),
#         Sí = ifelse(grepl("^Si$", iconv(Valor, to = "ASCII//TRANSLIT"), ignore.case = TRUE), 1, 0),
#         NAN = ifelse(grepl("^NA$", iconv(Valor, to = "ASCII//TRANSLIT"), ignore.case = TRUE), 1, 0),
#         'Porcentaje No' = round(((No / ncol(select(df, starts_with(prefijo)))) * 100),2),
#         'Porcentaje Sí' = round(((Sí / ncol(select(df, starts_with(prefijo)))) * 100),2),
#         'Porcentaje NAN' = round(((NAN / ncol(select(df, starts_with(prefijo)))) * 100),2)
#       ) #%>%
#     
#     # Ahora agregamos las columnas adicionales (AV, Folio, Correo) con left_join
#     df_filtrado <- df_filtrado %>%
#       left_join(select(df, Generación, Grupo, AV, Folio, Correo), by = "Grupo") %>%
#       select(Generación, Grupo, AV, Folio, Correo, Semana, 'Total de respuestas', No, Sí, NAN, 'Porcentaje No', 'Porcentaje Sí', 'Porcentaje NAN') %>%
#       mutate(IDReactivo = prefijo, 
#              Reactivo = categorias_mv_func$Reactivo[categorias_mv_func$IDReactivo == prefijo],
#              Verificacion = categorias_mv_func$Verificacion[categorias_mv_func$IDReactivo == prefijo]) %>% # Agregar la columna Verificacion
#       filter(Verificacion != "Duración")  #Eliminar reactivos de Duración
#     
#     return(df_filtrado)
#   })
#   return(resultados)
# }

calcular_frec_porc1 <- function(df) {
  
  resultados <- map_dfr(categorias_mv_func$IDReactivo, function(prefijo) {
    
    # Filtrar las columnas que empiezan con el prefijo
    columnas_prefijo <- select(df, starts_with(prefijo))
    
    # Si no hay columnas con ese prefijo, saltar al siguiente
    if (ncol(columnas_prefijo) == 0) {
      return(NULL)
    }
    
    df_filtrado <- df %>%
      select(Grupo, starts_with(prefijo)) %>%
      pivot_longer(cols = starts_with(prefijo), names_to = "Columna", values_to = "Valor") %>%
      mutate(
        Semana = paste0("Semana ", gsub(".*\\.s(\\d+).*", "\\1", Columna)),
        'Total de respuestas' = ncol(columnas_prefijo),
        No = ifelse(grepl("^No$", iconv(Valor, to = "ASCII//TRANSLIT"), ignore.case = TRUE), 1, 0),
        Sí = ifelse(grepl("^Si$", iconv(Valor, to = "ASCII//TRANSLIT"), ignore.case = TRUE), 1, 0),
        NAN = ifelse(grepl("^NA$", iconv(Valor, to = "ASCII//TRANSLIT"), ignore.case = TRUE), 1, 0),
        'Porcentaje No' = round(((No / ncol(columnas_prefijo)) * 100), 2),
        'Porcentaje Sí' = round(((Sí / ncol(columnas_prefijo)) * 100), 2),
        'Porcentaje NAN' = round(((NAN / ncol(columnas_prefijo)) * 100), 2)
      )
    
    df_filtrado <- df_filtrado %>%
      left_join(select(df, Generación, Grupo, AV, Folio, Correo), by = "Grupo") %>%
      select(Generación, Grupo, AV, Folio, Correo, Semana, 'Total de respuestas', No, Sí, NAN, 'Porcentaje No', 'Porcentaje Sí', 'Porcentaje NAN') %>%
      mutate(
        IDReactivo = prefijo, 
        Reactivo = categorias_mv_func$Reactivo[categorias_mv_func$IDReactivo == prefijo],
        Verificacion = categorias_mv_func$Verificacion[categorias_mv_func$IDReactivo == prefijo]
      ) %>%
      filter(Verificacion != "Duración")
    
    return(df_filtrado)
  })
  
  return(resultados)
}

av_sust <- function(df){
  sustituciones <- df[grepl("sustit", df$Folio), ]
  return(sustituciones)
}

nv_cumplimiento <- function(no) {
  prom_no <- mean(no, na.rm = TRUE) 
  
  if (prom_no <= 10) {
    return("Muy bueno")
  } else if (prom_no <= 20) {
    return("Bueno")
  } else if (prom_no <= 30) {
    return("Regular")
  } else if (prom_no <= 40) {
    return("Malo")
  } else if (prom_no <= 50) {
    return("Muy malo")
  } #else {
  #return("Fuera de rango")
  #}
}

intervenciones_func <- function(mv) {
  df <- Revision_modular %>%
    #select(matches("AV|Correo"), matches(mv))
    select(matches("AV|Correo"), matches(paste0("^", mv)))

  if (as.numeric(modular) ==0) { #En caso de prope
  df <- df %>%
    mutate(Estatus = case_when(
      df[,3] <= 0.2 ~ "B/D",
      df[,3] >= 0.4 ~ "I/I",
      #apply(.[, 3:ncol(.)], 1, function(x) all(x > 0)) ~ "I/R",
      df[,3] >= 0.2 ~ "R",
      TRUE ~ ""  # equivalente a SI.ERROR(...,"")
      ))
  } else {
  df <- df %>%
    mutate(Estatus = case_when(
      df[,3] == 0 ~ "B/D",
      df[,3] >= 0.4 ~ "I/I",
      apply(.[, 3:ncol(.)], 1, function(x) all(x > 0)) ~ "I/R",
      df[,3] > 0 ~ "R",
      TRUE ~ ""  # equivalente a SI.ERROR(...,"")
    ))  
  }
  return(df)
}

list_df_av <- list()
list_df_sm <- list()

#load("historico.RData")

siglas_inter <- data.frame(Siglas=c("I/R", "I/I", "R", "B/D"), Concepto=c("Intervención por reiteración", "Intervención por Incumplimiento", "Retroalimentación", "Buen desempeño"))

calendario <- data.frame(Bloque= c("bloque1", "bloque2", "bloque3", "bloque4"), Semana=c(1, 1, 1, 1), ColumnasMV1 = c(22, 38, 53, 73), ColumnasMV2 = c(26, 46, 65, 79),
                         config_MV1=c("data_frames_config_MV1_s1", "data_frames_config_MV1_s2", "data_frames_config_MV1_s3", "data_frames_config_MV1"), config_MV2=c("data_frames_config_MV2_s1", "data_frames_config_MV2_s2", "data_frames_config_MV2_s3", "data_frames_config_MV2"))

get_semana <- function(mod) {
  if (mod %in% bloque1) {
    semana_corte <- calendario$Semana[calendario$Bloque == "bloque1"]
    ncolMV1 <- calendario$ColumnasMV1[calendario$Bloque == "bloque1"]
    ncolMV2 <- calendario$ColumnasMV2[calendario$Bloque == "bloque1"]
    frames_config_MV1 <- calendario$config_MV1[calendario$Bloque == "bloque1"]
    frames_config_MV2 <- calendario$config_MV2[calendario$Bloque == "bloque1"]
    
  } else if (mod %in% bloque2) {
    semana_corte <- calendario$Semana[calendario$Bloque == "bloque2"]
    ncolMV1 <- calendario$ColumnasMV1[calendario$Bloque == "bloque2"]
    ncolMV2 <- calendario$ColumnasMV2[calendario$Bloque == "bloque2"]
    frames_config_MV1 <- calendario$config_MV1[calendario$Bloque == "bloque2"]
    frames_config_MV2 <- calendario$config_MV2[calendario$Bloque == "bloque2"]
    
  } else if (mod %in% bloque3) {
    semana_corte <- calendario$Semana[calendario$Bloque == "bloque3"]
    ncolMV1 <- calendario$ColumnasMV1[calendario$Bloque == "bloque3"]
    ncolMV2 <- calendario$ColumnasMV2[calendario$Bloque == "bloque3"]
    frames_config_MV1 <- calendario$config_MV1[calendario$Bloque == "bloque3"]
    frames_config_MV2 <- calendario$config_MV2[calendario$Bloque == "bloque3"]
    
  } else if (mod %in% bloque4) {
    semana_corte <- calendario$Semana[calendario$Bloque == "bloque4"]
    ncolMV1 <- calendario$ColumnasMV1[calendario$Bloque == "bloque4"]
    ncolMV2 <- calendario$ColumnasMV2[calendario$Bloque == "bloque4"]
    frames_config_MV1 <- calendario$config_MV1[calendario$Bloque == "bloque4"]
    frames_config_MV2 <- calendario$config_MV2[calendario$Bloque == "bloque4"]
    
  } #else {
  #   return(NA)
  # }
  #return(semana_corte, ncolMV1, ncolMV2)
  return(list(semana_corte = semana_corte, ncolMV1 = ncolMV1, ncolMV2 = ncolMV2, frames_config_MV1=frames_config_MV1, frames_config_MV2=frames_config_MV2))
}

base_semana_func <- function(lista,indice) {
  
  mod_sem <- do.call(rbind, lista)
  mod_sem <- asignar_reactivo(mod_sem)
  if (indice == 2){
    mod_sem[1,length(mod_sem)] <- "Retroalimentación"
  }
  nom_mod_sem <- paste0("mod_sem", i, "_M", modular) 
  assign(nom_mod_sem,mod_sem)
  #return(mod_sem)
  #assign(nom_mod_sem1,mod_sem)
}

bloque1 <- c(1, 5, 9, 13, 17, 21, 101)
bloque2 <- c(0, 2, 6, 10, 14, 18, 22, 102, 105)
bloque3 <- c(3, 7, 11, 15, 19, 23, 103, 106)
bloque4 <- c(4, 8, 12, 16, 20, 104)

#save.image("datos_necesarios.RData")
save.image(file = paste("datos_necesarios", date_dat_nec, ".RData", sep = ""))


