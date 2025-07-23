#----------------------------------------------------------------
#       Código para compilar datos para el reporte
#           "Apoyo a tu seguimiento modular"
# Contempla generaciones cerradas y cumplimiento por semana
#                 con información de los AV
#                         Krýstal
#           Parte 3: Organizar datos para la app
#---------------------------------------------------------------
# #Creando las listas para la APP

rm(list = ls())
#Modulares <- c("15")
Modulares <- as.character(c(0:23, 101:106))
date_carga <- "-22-07-25"
date_save <- "-22-07-25"
for (modular in Modulares) {
  archivo <- paste0("entorno_M", modular, date_carga,".RData")
  
  
  if (file.exists(archivo)) {
    load(archivo)
    message("Cargado: ", archivo)
  } else {
    warning("No se encontró el archivo: ", archivo)
  }
}

########PARA PORCESAR LOS DATOS EN CONJUNTO##########
dataset_func <- function(lista,patron, columnas = NULL) {

  lista <- setNames(
    lapply(c(0:23, 101:106), function(i) get(paste0(patron,i))),
    paste0("grupo", c(0:23, 101:106))
    #lapply(c(15), function(i) get(paste0(patron,i))),
    #paste0("grupo", c(15))

  )

  return(lista)
}

datasets_mod_graf1 <- mget(ls(pattern = "^seg_mod_up_M"))
datasets_mod_graf2 <- mget(ls(pattern = "^seg_mod_bt_M"))
datasets_mod_rev_cumpl <- mget(ls(pattern = "^tabla_cumpl_mod"))
datasets_mod_rev_rev <- mget(ls(pattern = "^revision_mod_M"))
datasets_mod_bas_com <- mget(ls(pattern = "^base_completa_mod_M"))
datasets_mod_sem1 <- mget(ls(pattern = "^mod_sem1_M"))
datasets_mod_sem2 <- mget(ls(pattern = "^mod_sem2_M"))
datasets_mod_sem3 <- mget(ls(pattern = "^mod_sem3_M"))
datasets_mod_sem4 <- mget(ls(pattern = "^mod_sem4_M"))
datasets_foc_rev <- mget(ls(pattern = "^revision_foc_M"))
datasets_foc_cumav_rev <- mget(ls(pattern = "^tabla_cumpl_foc_M"))
datasets_foc_gen_cumpl <- mget(ls(pattern = "^tabla_cumpl_foc_M"))

nombres_originales <- names(datasets_foc_gen_cumpl)# Obtener los nombres originales
numeros <- as.numeric(gsub(".*?(\\d+)$", "\\1", nombres_originales))

# Aplicar la función condicionalmente y conservar nombres
datasets_foc_gen_cumpl <- setNames(
  lapply(seq_along(datasets_foc_gen_cumpl), function(i) {
    df <- datasets_foc_gen_cumpl[[i]]
    nombre <- nombres_originales[i]

    # Extraer número del nombre
    numero <- as.numeric(gsub(".*?(\\d+)$", "\\1", nombre))

    # Aplicar solo si el número es menor que 24
    if (!is.na(numero) && numero < 24) {
      df <- df %>% dplyr::select(1:6)
    }

    return(df)
  }),
  nombres_originales
)

nuevos_nombres <- paste0("grupo", numeros)
names(datasets_foc_gen_cumpl) <- nuevos_nombres# Asignar los nuevos nombres a la lista

datasets_foc_cumav_avs <- mget(ls(pattern = "^av_reg_malo_M"))
datasets_foc_segav <- mget(ls(pattern = "^frec_porc_M"))

lista_foc_inter <- ls(pattern = "^inter_mv")

dataset_inter_func <- function(nom_categoria) {

  datasets_foc_inter <- list()

  for (nombre in lista_foc_inter) {
    grupo <- get(nombre)
    coincidencias <- grupo[grepl(paste0("^", nom_categoria), names(grupo))]
    mod <- str_extract(nombre, "(?<=inter_mv)\\d+")
    #names(coincidencias)[1] <- paste0(nom_categoria, "_M", mod)
    names(coincidencias)[1] <- paste0("grupo", mod)
    datasets_foc_inter <- c(datasets_foc_inter, coincidencias)
  }

  return(datasets_foc_inter)
}

intervenciones_mv <- list(
  mensaje_bienvenida = data.frame(),
  foro_no_evaluable = data.frame(),
  foro_evaluable = data.frame(),
  foro_novedades = data.frame(),
  evaluacion = data.frame(),
  pizarraEAA = data.frame(),
  EAA = data.frame())

var_categoria <- names(intervenciones_mv)

for (nombre in names(intervenciones_mv)) {

  inter_temporal <- dataset_inter_func(nombre)
  nuevos_nombre_datasets_foc <- paste0("datasets_foc_", nombre)
  assign(nuevos_nombre_datasets_foc,inter_temporal)

}

datasets_foc_gen_graf <- mget(ls(pattern = "^cumplimiento_M"))

#Inicio---------------------------------------------
#-------Bienvenida-------

#Modular---------------------------------------------
#-------Gráf1-------
p.mod_graf1 <- "seg_mod_up_M"
datasets_mod_graf1 <- dataset_func(datasets_mod_graf1,p.mod_graf1)
#-------Gráf2-------
p.mod_graf2 <- "seg_mod_bt_M"
datasets_mod_graf2 <- dataset_func(datasets_mod_graf2,p.mod_graf2)
#-------Revisión-------
p.mod_rev_cumpl <- "tabla_cumpl_mod"
datasets_mod_rev_cumpl <- dataset_func(datasets_mod_rev_cumpl,p.mod_rev_cumpl)

p.mod_rev_rev <- "revision_mod_M"
datasets_mod_rev_rev <- dataset_func(datasets_mod_rev_rev,p.mod_rev_rev)

#-------Base Completa-------
p.mod_bas_com <- "base_completa_mod_M"
datasets_mod_bas_com <- dataset_func(datasets_mod_bas_com,p.mod_bas_com)

#-------Semana1-------
p.mod_sem1 <- "mod_sem1_M"
datasets_mod_sem1 <- dataset_func(datasets_mod_sem1,p.mod_sem1)

#-------Semana2-------
p.mod_sem2 <- "mod_sem2_M"
datasets_mod_sem2 <- dataset_func(datasets_mod_sem2,p.mod_sem2)

#-------Semana3-------
p.mod_sem3 <- "mod_sem3_M"
datasets_mod_sem3 <- dataset_func(datasets_mod_sem3,p.mod_sem3)

#-------Semana4-------
p.mod_sem4 <- "mod_sem4_M"
datasets_mod_sem4 <- dataset_func(datasets_mod_sem4,p.mod_sem4)

#Focalizado--------------------------------------------
#-------General-------
#p.foc_gen_cumpl <- "tabla_cumpl_foc_M"
#datasets_foc_gen_cumpl <- dataset_func(datasets_foc_gen_cumpl,p.foc_gen_cumpl)

p.foc_gen_graf <- "cumplimiento_M"
datasets_foc_gen_graf <- dataset_func(datasets_foc_gen_graf,p.foc_gen_graf)

#-------Cumplimiento AV-------
p.foc_cumav_rev <- "tabla_cumpl_foc_M"
datasets_foc_cumav_rev <- dataset_func(datasets_foc_cumav_rev,p.foc_cumav_rev)

p.foc_cumav_avs <- "av_reg_malo_M"
datasets_foc_cumav_avs <- dataset_func(datasets_foc_cumav_avs,p.foc_cumav_avs)

#-------Revisión-------
p.foc_rev <- "revision_foc_M"
datasets_foc_rev <- dataset_func(datasets_foc_rev,p.foc_rev)

#-------Seguimiento AV-------
p.foc_segav <- "frec_porc_M"
datasets_foc_segav <- dataset_func(datasets_foc_segav,p.foc_segav)

#-------Mensaje de Bienvenida-------

todos_los_objetos <- ls()
#Guardar datos para correr la app
objetos_a_guardar <- grep("^(datasets_|categorias_mv_1|categorias_mv_2)", todos_los_objetos, value = TRUE) #ls(pattern = "^datasets_")
save(list = objetos_a_guardar, file = paste0("entorno_global",date_save,".RData"))

#Para guardar los históricos actualizados
# datasets_foc_cumav_cump_hist <- datasets_foc_cumav_rev
# datasets_foc_cumav_avreg_hist <- datasets_foc_cumav_avs
# historicos_save <- list(datasets_foc_cumav_cump_hist,datasets_foc_cumav_avreg_hist)
# save(datasets_foc_cumav_cump_hist, datasets_foc_cumav_avreg_hist, file = "historico.RData")
# save(list = names(historicos_save), paste0("historico",date_save,".RData"))# Guardar los data frames en un archivo .RData

# #Para guardar los data frames en un archivo .Rdata dependiendo del bloque o para entregables
# dfs_to_save <- list()
# # Iterar sobre cada valor de X
# for (X in bloque2) {
#   df_name <- paste0("M", X, "_frec_porc")# Construir el nombre del data frame dinámicamente
#   if (exists(df_name)) {# Verificar si el data frame existe en el environment
#     dfs_to_save[[df_name]] <- get(df_name)# Obtener el data frame y añadirlo a la lista
#   } else {
#     warning(paste("El data frame", df_name, "no existe en el environment."))
#   }
# }
# for (df_to_save in names(dfs_to_save)) {
#   write.xlsx(dfs_to_save[[df_to_save]], here(dir_productos, paste0(df_to_save, ".10.04.25.xlsx")))
# }
# save(list = names(dfs_to_save), file = "data_frames_sm_av.RData")# Guardar los data frames en un archivo .RData

#save.image(file = "mi_entorno.RData")
##Para verificar qué columnas no están en el compiado
#ids_faltantes <- G64M5_semana2$Grupo[!(G64M5_semana2$Grupo %in% G64M5$Grupo)]

#Para encontrar un valor específico en una columna
# unique(M19_frec_porc$Generación) # Para ver las generaciones del análisis
# unique(seg_mod_M106[seg_mod_M106$Generación == "G144M106", "Semana"])

##Comandos para verificar el número de columas de reactivos
# contiene <- grepl("190", colnames(compilado_asignacion))
# numero_elementos <- sum(contiene)
# which(grepl("190", colnames(compilado_asignacion)))

# colnames(compilado_asignacion[which(grepl("190", colnames(compilado_asignacion)))])

#Para ver archivos de generaciones por módulo en carpeta
# Modulares <- unique(str_extract(bases$nombre, "(?<=M)\\d+(?= Semana)"))
# for (modular in Modulares) {
#   bases_modulo_global <- subset(bases, grepl(paste0("M", modular, " "), nombre))
#   Generaciones_global <- unique(str_extract(bases_modulo_global$nombre, "(?<=_G)\\d+(?=M)"))
#   N.Gen_global <- length(Generaciones_global)
#   print(paste0("Modulo ",modular," con: ", N.Gen_global, " generaciones\nGeneraciones: ", Generaciones_global))
# }

#Para guardar históricos 
# leer_historico_func <- function(nombre) {
#   
#   hojas <- excel_sheets(nombre_archivo)
#   hojas <- hojas[hojas != "Hoja2"]
#   
#   lista_hojas <- lapply(hojas, function(hoja) {
#     read_excel(nombre_archivo, sheet = hoja)
#   })
#   
#   # Asigna nombres a los elementos de la lista
#   names(lista_hojas) <- hojas
#   
#   return(lista_hojas)
# }
# 
# nombre_archivo <- "foc_tabla_cumpl_histo.xlsx"
# datasets_foc_cumav_cump_hist <- leer_historico_func(nombre_archivo)
# datasets_foc_cumav_cump_hist
# nombre_archivo <- "foc_tav_reg_histo.xlsx"
# datasets_foc_cumav_avreg_hist <- leer_historico_func(nombre_archivo)
# 
# #dfs_to_save <- list(datasets_foc_cumav_cump_hist,datasets_foc_cumav_avreg_hist)
# save(datasets_foc_cumav_cump_hist, datasets_foc_cumav_avreg_hist, file = "historico.RData")
# save(list = names(dfs_to_save), file = "historico.RData")# Guardar los data frames en un archivo .RData

#http://127.0.0.1:6544/
#http://127.0.0.1:6544/?grupo=grupo1
#http://127.0.0.1:6544/?grupo=grupo2
#save.image("entorno_global-04-07-25.RData")
