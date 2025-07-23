#----------------------------------------------------------------
#       Código para compilar datos para el reporte
#           "Apoyo a tu seguimiento modular"
# Contempla generaciones cerradas y cumplimiento por semana
#                 con información de los AV
#                         Krýstal
#           Parte 2: Procesar datos
#---------------------------------------------------------------

#Con Semana y REC (Completo)
rm(list = ls())

Modulares <- c("0")
#Modulares <- as.character(c(0:23, 101:106))
#Modulares <- as.character(c(103:106))
#save.image("entorno_106.RData")
#rm(list = ls())
for (modular in Modulares) {
  date_save <- "-22-07-25"
  date_carga <- "-11-07-25"
  load(paste0("historico",date_carga,".RData"))
  
  #load("historico.RData")
  #load("datos_necesarios.RData")
  load(paste0("datos_necesarios", date_save, ".RData"))
  
  # Filtra las bases que corresponden a este módulo
  #bases_modulo <- bases[grepl(paste0("M", modulo), bases$nombre), ]
  bases_modulo_global <- subset(bases, grepl(paste0("M", modular, " "), nombre))
  Generaciones_global <- unique(str_extract(bases_modulo_global$nombre, "(?<=_G)\\d+(?=M)"))
  N.Gen_global <- length(Generaciones_global)
  print(paste0("Modulo ", modular, " con ", N.Gen_global, " generaciones: ", paste(Generaciones_global, collapse = ", ")))
  
  
  if (modular %in% bloque1){
    N.Gen_interes <- 6 #Número de generaciones de interés
    print(paste0("Modulo ",modular,", bloque: 1, generaciones de interés: ", N.Gen_interes))
  }else if (modular %in% bloque2){
    N.Gen_interes <- 6 #Número de generaciones de interés
    print(paste0("Modulo ",modular,", bloque: 2, generaciones de interés: ", N.Gen_interes))
  }else if (modular %in% bloque3){
    N.Gen_interes <- 6 #Número de generaciones de interés
    print(paste0("Modulo ",modular,", bloque: 3, generaciones de interés: ", N.Gen_interes))
  }else if (modular %in% bloque4){
    N.Gen_interes <- 6 #Número de generaciones de interés
    print(paste0("Modulo ",modular,", bloque: 4, generaciones de interés: ", N.Gen_interes))
  }
  
  # Filtra las bases que corresponden a este módulo y generaciones de interés
  Generaciones <- tail(sort(Generaciones_global, decreasing = FALSE), N.Gen_interes) # Extrae generaciones de interés de todo el pool
  bases_modulo <- subset(bases_modulo_global, grepl(paste0(paste(Generaciones, collapse = "|"), "M", modular, " "), nombre))
  N.Gen <- length(Generaciones)
  print(paste0("Modulo ",modular,", generaciones a analizar:", paste(Generaciones, collapse = ", ")))
  # unique(M19_frec_porc$Generación) # Para ver las generaciones del análisis
  
  if (modular %in% c(22, 23, 105, 106)){
    categorias_mv_func <- categorias_mv_2
    obser_MV_func <- obser_MV2
  }else {
    categorias_mv_func <- categorias_mv_1
    obser_MV_func <- obser_MV1
  }
  
  if (modular %in% c(101,102,103,104,105,106)){
    SAME_modulo <- bases_SAME[grepl('REC', bases_SAME$nombre), ]
  } else {
    SAME_modulo <- bases_SAME[grepl(paste0("M", modular), bases_SAME$nombre), ]
  }
  
  for (name in SAME_modulo) {
    #if (grepl('\\.(xlsx)$',nombre, ignore.case = TRUE)) {
    nombre_SAME<-paste0(dir_insumos_SAME,'/', name)
    base1<-readxl::read_xlsx(paste0(nombre_SAME))
    if (!exists("baseSAME")) { # Si no existe
      baseSAME <- base1
    } else {
      baseSAME <- rbind(baseSAME, base1)  # Combina los datos #bind_rows(M, base1)
    }
  }
  
  datos_SAME_base <- baseSAME %>%
    #select(23:24) %>%
    mutate(SAME = apply(.[, 23:24], 1, function(x) str_c(x, collapse = " ")) %>%  # Concatenar con espacio
             str_to_title()) %>%  # Convierte a formato de nombre
    #select(1,2,Nombre_SAME,22,25) # Selecciona datos de interés
    select(1,SAME) # Selecciona datos de interés
  
  for (base in bases_modulo[, 1]) {
    nombre <- paste0(dir_insumos, '/', base)  # Obtener el nombre del archivo
    base1 <- readxl::read_xlsx(nombre)  # Leer el archivo
    filas_extra <- base1[1:2, ]  # Tomamos las filas de los encabezados del importable
    datos_av_base <- base1 %>%
      select(1:6)
    colnames(datos_av_base) <- as.character(unlist(datos_av_base[2, ])) #Renombrar columnas
    colnames(datos_av_base)[6] <- "Grupo" #Renombrar columna 6 a "Grupo"
    datos_av_base <- datos_av_base %>%
      slice(-1,-2) %>%
      mutate(AV = apply(.[, 2:4], 1, function(x) str_c(x, collapse = " ")) %>%  # Concatenar con espacio
               str_to_title()) %>%  # Convierte a formato de nombre
      select(AV, 1, 5, 6) # Selecciona datos de interés
    preg_reactivo <- filas_extra %>% #El resultado en un data.frame con las preguntas de los reactivos
      select(-1,-2,-3,-4,-5,-6) %>% #Quitamos las columnas de AV y el encabezado "Grupo/Reactivos"
      slice(-2) #Quitamos la fila con encabezados extra
    colnames(base1)[1:5] <- base1[2,1:5] #Renombramos los encabezados del data frame con los nombres de la segunda columna de las columnas 1 a 5
    colnames(base1)[6] <- "Grupo" #Renombrar columna 6 a "Grupo"
    base1 <- base1 %>%
      slice(-1,-2) %>% # Eliminar las primeras dos filas de la base
      select(-1,-2,-3,-4,-5) #Eliminar las primeras 5 columnas de la base (datos de AV)
    
    for (g in 1:N.Gen) {
      asignación <- paste0("G", Generaciones[g], "M", modular)  # Nombre de asignación
      modulo <- paste0("M", modular)  # Nombre de asignación
      nombre_asig  <- paste0("G", Generaciones[g], "M", modular)  # Nombre para asignación
      nombre_asig_metodos  <- paste0("G", Generaciones[g], "M", modular, "_metodos")  # Nombre de asignación para guardar conteo por métodos de verificación
      nom_frec_porc_mv_gen <- paste0(asignación, "_frec_porc")
      nom_compilado_asignacion_completo <- paste0(asignación, "_completo")
      nom_frec_porc_mod <- paste0("frec_porc_M",modular)
      nom_av_sust_gen <- paste0("av_sust_",asignación)
      nom_sm_m <- paste0("seg_mod_up_M",modular)
      nom_sm2_m <- paste0("seg_mod_bt_M",modular)
      nom_base_modular <- paste0(asignación,"_base_mod_M",modular)
      nom_base_completa_modular <- paste0("base_completa_mod_M",modular)
      nom_revision_modular <- paste0("revision_mod_M",modular)
      nom_tabla_cumplimiento_mod <- paste0("tabla_cumpl_mod",modular)
      nom_intervenciones_mv <- paste0("inter_mv",modular)
      nom_revision_focalizado <- paste0("revision_foc_M",modular)
      nom_tabla_foc <- paste0("tabla_cumpl_foc_M",modular)
      nom_av_reg_malo <- paste0("av_reg_malo_M",modular)
      nom_cumplimiento <- paste0("cumplimiento_M",modular)
      
      # Si el nombre de la base contiene el nombre de asignación
      if (grepl(paste0(asignación), base, ignore.case = TRUE)) {
        
        for (s in 1:4) {
          semana_reactivos <- paste0("Semana_", s) # Crea el nombre de la variable semana x
          if (grepl(paste0('Semana_', s), base, ignore.case = TRUE) && (!exists(semana_reactivos))){ #Si corresponde a la semana y no existe el data frame,
            #assign(semana_reactivos, data.frame())
            assign(semana_reactivos, preg_reactivo)  # Crea el data frame vacío si no existe
          }
          
          asignaciónxsemana <- paste0("G", Generaciones[g], "M", modular, "_semana", s)# Nombre para el data frame de cada Generación por semana
          nom_av_semana_gen <- paste0("av_G", Generaciones[g], "M", modular, "_semana", s)# Nombre para el data frame de AV de cada semana
          
          if (!exists(asignaciónxsemana)) { # Si no existe el dataframe de Generación por semana
            assign(asignaciónxsemana, data.frame())  # Crear el data frame vacío
          }
          
          if (!exists(nom_av_semana_gen)) { # Si no existe el dataframe de AV por semana
            assign(nom_av_semana_gen, data.frame())  # Crear el data frame vacío
          }
          # Concatenamos los datos correspondientes a cada semana si corresponde
          if (grepl(paste0('Semana_', s), base, ignore.case = TRUE)) {
            contador <- contador+1 # Suma al contador
            n.archivos_asignaciónxsemana <- sum(grepl(paste0("G", Generaciones[g], "M", modular, " Semana_", s), bases_modulo$nombre)) # Cuántas veces hay un archivo de Gen x semana 
            if (grepl(paste0('M', modular), base, ignore.case = TRUE)) {
              contador.m <- contador.m+1 # Suma al contador
              #print(paste0())
            }
            
            asignación.semana <- assign(asignaciónxsemana, rbind(get(asignaciónxsemana), base1))  # Agrega las filas de la base a Gen x semana en caso de corresponder
            av.semana <- assign(nom_av_semana_gen, rbind(get(nom_av_semana_gen), datos_av_base))  # Agrega los datos de AV x semana en caso de corresponder
            
            #Cambio de nombre en col del importable de acuerdo a la semana
            sufijo <- paste0(".s", s) # Nombre de sufijo de acuerdo a la semana
            nombres_columnas <- names(asignación.semana)[2:length(asignación.semana)] # Seleccionar las columnas con cambio ----> Cambiar en caso de trabajar con nombre de AV
            nombres_columnas_modificados <- paste0(nombres_columnas, sufijo) # Nombre de col con el sufijo
            names(asignación.semana)[2:length(asignación.semana)] <- nombres_columnas_modificados #Cambio de nombres de col en el data.frame de Gen x semana
            
            if (contador == n.archivos_asignaciónxsemana){ #Cuando ya se leen todos los archivos de Gen x semana
              print(paste0("Contador: ",contador, " G", Generaciones[g], "M", modular, " Semana_", s, " ", nrow(asignación.semana)))
              contador <- 0 #Reinicia contador para la suma
              
              #Aquí se calcula el total de sí y no
              conteo_s <- sapply(asignación.semana[, -1], function(columna) sum(grepl("^Si$", iconv(columna, to = "ASCII//TRANSLIT"), ignore.case = TRUE))) #sapply(asignación.semana, function(columna) sum(grepl("[sSíÍ]", columna)))
              conteo_n <- sapply(asignación.semana[, -1], function(columna) sum(grepl("^No$", iconv(columna, to = "ASCII//TRANSLIT"), ignore.case = TRUE))) #sapply(asignación.semana, function(columna) sum(grepl("[nNóÓ]", columna)))
              conteo_na <- sapply(asignación.semana[, -1], function(columna) sum(grepl("^NA$", iconv(columna, to = "ASCII//TRANSLIT"), ignore.case = TRUE))) #sapply(asignación.semana, function(columna) sum(grepl("[sSíÍ]", columna)))
              asig <- rep(c(asignación), length(conteo_s))
              sem <- rep(paste0("Semana ", s), length(conteo_s))
              assign(semana_reactivos, rbind(get(semana_reactivos), asig))
              assign(semana_reactivos, rbind(get(semana_reactivos), sem))
              assign(semana_reactivos, rbind(get(semana_reactivos), colnames(preg_reactivo)))
              assign(semana_reactivos, rbind(get(semana_reactivos), preg_reactivo[1,]))
              assign(semana_reactivos, rbind(get(semana_reactivos), conteo_s))
              assign(semana_reactivos, rbind(get(semana_reactivos), conteo_n))
              assign(semana_reactivos, rbind(get(semana_reactivos), conteo_na))
              
              av_sustituidos <- av_sust(get(nom_av_semana_gen))
              if (isTRUE(nrow(av_sustituidos)>0)) { # Si no existe el dataframe de AV por semana
                if (!exists("eliminados_av")) {
                  eliminados_av <- av_sustituidos 
                } else {
                  eliminados_av <- rbind(eliminados_av,av_sustituidos)
                }
              } 
              
              if (s == 1){
                print('pegar semana 1')
                compilado_asignacion <- assign(asignación, asignación.semana)
              } else {
                print(paste0("pegar semana", s))
                #compilado_asignacion <- merge(compilado_asignacion, asignación.semana, by = c('Grupo', 'Nombre', 'Apellido Paterno', 'Apellido Materno', 'Correo', 'Folio')) #Compilar por grupo y AV
                compilado_asignacion <- merge(compilado_asignacion, asignación.semana, by = c('Grupo')) #Compilar por grupo pero se agregan el resto de filas del AV por cada semana
                
              }
              # Agregar encabezado de reactivos
              vector_gen <- rep(asignación, nrow(asignación.semana))
              
              asignación.semana <- merge(asignación.semana, av.semana, by = "Grupo")
              asignación.semana <- left_join(asignación.semana, datos_SAME_base, by = "Grupo")
              if (as.numeric(modular) > 100) {
                # df <- df %>%
                #   mutate(Generación = paste0("REC", Generaciones[g]))
                asignación.semana <- asignación.semana %>%
                  mutate(Generación = paste0("REC", Generaciones[g]))
              } else {
                asignación.semana <- asignación.semana %>%
                  mutate(Generación = asignación)
              }
              
              asignación.semana <- asignación.semana %>%
                select("Generación","Grupo", which(names(.) == "AV"):which(names(.) == "SAME"), # Todo hasta "Generación"
                       everything())
              #asignación.semana <- asignar_reactivo(asignación.semana)
              # asignación.semana <- asignación.semana %>%
              #   select("Grupo",which(names(.) == "AV"):which(names(.) == "SAME"), # Todo hasta "Generación"
              #          everything())                      # El resto
              assign(asignaciónxsemana, asignación.semana)
            }
          }
          
          calendar <- get_semana(modular)
          
          if (s == 4 && contador == 0 && Generaciones[g] != tail(sort(Generaciones_global, decreasing = FALSE), 1) ||
              s== calendar[["semana_corte"]] && contador == 0 && Generaciones[g] == tail(sort(Generaciones_global, decreasing = FALSE), 1)){
            
            assign(nombre_asig,compilado_asignacion) #compilado de importable por gen
            compilado_asignacion_completo <- merge(compilado_asignacion, av.semana, by = "Grupo")
            
            if (exists("eliminados_av")){
              grupos_eliminados_av <- unique(eliminados_av[,4]) #Grupos donde hubo sustituciones
              compilado_asignacion_completo <- compilado_asignacion_completo %>% # Limpiar los datos de sustituciones
                filter(!Grupo %in% grupos_eliminados_av$Grupo)
              
              compilado_asignacion <- compilado_asignacion %>% # Limpiar los datos de sustituciones
                filter(!Grupo %in% grupos_eliminados_av$Grupo)
            }
            grupos_eliminados_av <- data.frame()
            
            asig <- as.data.frame(rep(c(asignación), nrow(compilado_asignacion_completo)))
            compilado_asignacion_completo <- cbind(compilado_asignacion_completo, asig)
            colnames(compilado_asignacion_completo)[ncol(compilado_asignacion_completo)] <- "Generación" #Renombrar última columna
            
            #frec_porc <- calcular_frec_porc(compilado_asignacion_completo) # Aplicar función de # y % a asigna           
            #assign(nom_frec_porc_mv_gen,frec_porc) # Renormbar df de # y %  de asigna
            
            print(paste0("Contador.m: ",contador.m, " G", Generaciones[g], "M", modular, " Semana_", s, " ", nrow(compilado_asignacion), " ", ncol(compilado_asignacion)))
            colnames(compilado_asignacion)
            
            # if ((s == 4) && (contador == 0) && (Generaciones[g] != tail(sort(Generaciones_global, decreasing = FALSE), 1))){
            #   ncolMV1 <- 73
            #   ncolMV2 <- 79
            #   print(paste0("Analizando módulo ", modular, " semana ", s, " número de ítems ", ncolMV1, " ", ncolMV2))
            #   
            # } 
            
            #if ((s == 4) && (contador == 0) && (Generaciones[g] != tail(sort(Generaciones_global, decreasing = FALSE), 1)) && ncol(compilado_asignacion)==73 | (s == 4) && (contador == 0) && (Generaciones[g] != tail(sort(Generaciones_global, decreasing = FALSE), 1)) && ncol(compilado_asignacion)== 79|(s== calendar[["semana_corte"]]) && (contador == 0) && (Generaciones[g] == tail(sort(Generaciones_global, decreasing = FALSE), 1)) && ncol(compilado_asignacion)== calendar[["ncolMV1"]]|(s== calendar[["semana_corte"]]) && (contador == 0) && (Generaciones[g] == tail(sort(Generaciones_global, decreasing = FALSE), 1)) && ncol(compilado_asignacion)== calendar[["ncolMV2"]])
            if ( (ncol(compilado_asignacion) == 73 && s == 4 && Generaciones[g] != tail(sort(Generaciones_global, decreasing = FALSE), 1)) ||
                 (ncol(compilado_asignacion) == 79 && s == 4 && Generaciones[g] != tail(sort(Generaciones_global, decreasing = FALSE), 1)) ||
                 (ncol(compilado_asignacion) == calendar[["ncolMV1"]] && s == calendar[["semana_corte"]] &&Generaciones[g] == tail(sort(Generaciones_global, decreasing = FALSE), 1)) ||
                 (ncol(compilado_asignacion) == calendar[["ncolMV2"]] && s == calendar[["semana_corte"]]) &&Generaciones[g] == tail(sort(Generaciones_global, decreasing = FALSE), 1)){ 
              n.archivos_mod <- sum(grepl(paste0("M", modular), bases_modulo$nombre))
              print(paste0("Contador.m=", contador.m, " de",  n.archivos_mod, " asignación ", asignación," semana", s, ", ncol ", ncol(compilado_asignacion)))
              # Seleccionamos el data frame adecuado según la condición
              if (Generaciones[g] != tail(sort(Generaciones_global, decreasing = FALSE), 1)){
                if (ncol(compilado_asignacion)==73) {
                  print(paste0("Número de columnas: ",ncol(compilado_asignacion)))
                  data_frames_config <- data_frames_config_MV1
                } else if (ncol(compilado_asignacion)==79) {
                  data_frames_config <- data_frames_config_MV2
                }
              } else if (Generaciones[g] == tail(sort(Generaciones_global, decreasing = FALSE), 1)){
                if (ncol(compilado_asignacion)== calendar[["ncolMV1"]]) {
                  print(paste0("Número de columnas: ",ncol(compilado_asignacion)))
                  data_frames_config <- get(calendar[["frames_config_MV1"]])
                } else if (ncol(compilado_asignacion)== calendar[["ncolMV2"]]) {
                  data_frames_config <- get(calendar[["frames_config_MV2"]])
                }}
              
              for (i in 1:length(data_frames_config)) {
                data_frames_config[[i]]$df <- compilado_asignacion %>%
                  select(all_of(data_frames_config[[i]]$columnas_seleccionadas))
              }
              
              for (i in 1:length(data_frames_config)) {
                data_frames_config[[i]][["df"]][["conteo_s"]] <- apply(data_frames_config[[i]][["df"]], 1, function(fila) sum(grepl("^Si$", iconv(fila, to = "ASCII//TRANSLIT"), ignore.case = TRUE)))
                data_frames_config[[i]][["df"]][["conteo_n"]] <- apply(data_frames_config[[i]][["df"]], 1, function(fila) sum(grepl("^No$", iconv(fila, to = "ASCII//TRANSLIT"), ignore.case = TRUE)))
                if (names(data_frames_config)[i] %in% c("evaluacion", "pizarraEAA", "EAA")) {
                  data_frames_config[[i]][["df"]][["conteo_na"]] <- apply(data_frames_config[[i]][["df"]], 1, function(fila) sum(grepl("^NA$", iconv(fila, to = "ASCII//TRANSLIT"), ignore.case = TRUE)))
                }
                data_frames_config[[i]][["df"]][["Porcentaje_s"]] <- apply(data_frames_config[[i]][["df"]], 1, function(fila) {
                  conteo_n <- as.numeric(fila ['conteo_s'])
                  total_i <- obser_MV_func[[i]] #length(data_frames_config[[i]][["columnas_seleccionadas"]])
                  porcentaje <- round(((conteo_n / total_i)*100), 2)
                  return(porcentaje)
                })
                data_frames_config[[i]][["df"]][["Porcentaje_n"]] <- apply(data_frames_config[[i]][["df"]], 1, function(fila) 
                {
                  conteo_n <- as.numeric(fila ['conteo_n'])
                  total_i <- obser_MV_func[[i]] #length(data_frames_config[[i]][["columnas_seleccionadas"]])
                  porcentaje <- round(((conteo_n / total_i)*100), 2)
                  return(porcentaje)
                })
                if (names(data_frames_config)[i] %in% c("evaluacion", "pizarraEAA", "EAA")) {
                  data_frames_config[[i]][["df"]][["Porcentaje_na"]] <- apply(data_frames_config[[i]][["df"]], 1, function(fila) {
                    conteo_n <- as.numeric(fila ['conteo_na'])
                    total_i <- obser_MV_func[[i]] #length(data_frames_config[[i]][["columnas_seleccionadas"]])
                    porcentaje <- round(((conteo_n / total_i)*100), 2)
                    return(porcentaje)
                  }) 
                }
              }              
              
              #Compilado para Gráfica de Gen Modular
              assign(nombre_asig_metodos, data_frames_config)
              
              assign(nom_compilado_asignacion_completo,compilado_asignacion_completo)
              print(paste0("Aplicando calculo de # y % en la asignación: ",asignación," semana : ",s," , número de col: ", ncol(compilado_asignacion_completo)))
              frec_porc <- calcular_frec_porc1(compilado_asignacion_completo)
              assign(nom_frec_porc_mv_gen,frec_porc) # Renormbar df de # y %  de asigna
              
              # Lista para compilado por módulo de AV
              list_df_av[[length(list_df_av) + 1]] <- frec_porc #Agregar # y % a una lista
              print(paste0("Agregar a la lista ", asignación, "semana ", s))
              #n.archivos_mod <- sum(grepl(paste0("M", modular), bases_modulo$nombre)) # Cuántas veces hay un archivo de Gen x semana
              
              # if (contador.m == as.numeric(n.archivos_mod)){
              #   print(paste0("Archivos del módulo: ",contador.m, " de G", Generaciones[g], "M", modular, " analizados hasta la Semana ", s))
              #   base_completa_av <- do.call(rbind, list_df_av)
              #   colnames(base_completa_av)[11] <- "porcentaje_no"
              #   colnames(base_completa_av)[12] <- "porcentaje_si"
              #   colnames(base_completa_av)[13] <- "porcentaje_nan"
              #   assign(nom_frec_porc_mod, base_completa_av)
              #   list_df_av <- list()
              #   contador.m <- 0
              # }
              
              base_modular <- compilado_asignacion_completo %>%
                select(Grupo, AV, Folio, Correo, Generación)
              
              for (i in seq_along(data_frames_config)) {
                columnas_deseadas <- data_frames_config[[i]][["columnas_seleccionadas"]]
                columnas_existentes <- compilado_asignacion_completo[, columnas_deseadas, drop = FALSE]
                
                df_temporal_frec_porc <- data_frames_config[[i]][["df"]]
                df_select_temporal_frec_porc <- df_temporal_frec_porc %>%
                  select(any_of(columnas_frec_porc))
                
                nuevo_nombre_s  <- paste(categorias_mv[i], "Porcentaje si ", sep = "\n")
                nuevo_nombre_n  <- paste(categorias_mv[i], "Porcentaje no ", sep = "\n")
                nuevo_nombre_na <- paste(categorias_mv[i], "Porcentaje na ", sep = "\n")
                
                # Renombrar siempre "si" y "no", y condicionalmente "na"
                df_select_temporal_frec_porc <- df_select_temporal_frec_porc %>%
                  rename(
                    !!nuevo_nombre_s := Porcentaje_s,
                    !!nuevo_nombre_n := Porcentaje_n
                  )
                
                # Solo renombrar "na" si existe
                if ("Porcentaje_na" %in% names(df_select_temporal_frec_porc)) {
                  df_select_temporal_frec_porc <- df_select_temporal_frec_porc %>%
                    rename(!!nuevo_nombre_na := Porcentaje_na)
                }
                
                base_modular <- bind_cols(base_modular, columnas_existentes,df_select_temporal_frec_porc)  # Combina los datos #bind_rows(M, base1)
                assign(nom_base_modular,base_modular)
                
              }
            }
            calendar <- list()
          } #4 semanas con todos los archivos de la generación
        } #semana
      }
    } #generación
  } #archivo
  
  for (i in 1:s) {
    
    list_sem <- mget(ls(pattern = paste0("^G[0-9]+M", modular, "_semana", i,"$"), envir = .GlobalEnv))
    mod_sem <- base_semana_func(list_sem,i)
    nom_mod_sem <- paste0("mod_sem", i, "_M", modular) 
    assign(nom_mod_sem,mod_sem)
    
  }
  
  # #Crear base completa
  list_asig_completo <- mget(ls(pattern = paste0("_base_mod_M",modular)))
  
  all_cols <- unique(unlist(lapply(list_asig_completo, names)))
  
  dfs_alineados <- lapply(list_asig_completo, function(df) {
    missing_cols <- setdiff(all_cols, names(df))
    df[missing_cols] <- NA #"empty"
    df <- df[all_cols]
    return(df)
  })
  
  base_completa_modular <- do.call(rbind, dfs_alineados)
  base_completa_modular <- left_join(base_completa_modular, datos_SAME_base, by = "Grupo") # Unir nombre del SAME
  base_completa_modular <- base_completa_modular %>%
    select(1:which(names(.) == "Generación"), # Todo hasta "Generación"
           SAME,                              # La columna que quieres mover
           everything())                      # El resto
  
  if (as.numeric(modular) > 100) {
    base_completa_modular$Generación <- paste0("REC", str_extract(base_completa_modular$Generación, "(?<=G)\\d+(?=M)"))
  }
  
  #Creando la base de Revisión de seguimiento modular
  Promedios_modular <- base_completa_modular %>% # Se procede a sar promeidos
    select(matches("Grupo|AV|Folio|Correo|Generación|SAME|Porcentaje si|Porcentaje no|Porcentaje na")) %>%
    mutate(
      #total_columnas = ncol(.),
      'Promedio si' = round(((rowSums(across(matches("Porcentaje si")), na.rm = TRUE)*100) / 700), 2),
      'Promedio no' = round(((rowSums(across(matches("Porcentaje no")), na.rm = TRUE)*100) / 700), 2),
      'Promedio na' = round(((rowSums(across(matches("Porcentaje na")), na.rm = TRUE)*100) / 700), 2)
    ) %>%
    select(matches("Grupo|AV|Folio|Correo|Generación|SAME|Porcentaje no|Promedio")) %>%
    #mutate(ClaveG = str_extract(Grupo, "-\\d+$")) # Sacar una clave para compilarlos
    mutate(ClaveG = case_when(str_detect(Grupo, "REC") ~ str_replace(Grupo, "^([^\\-]+)-[^\\-]+-[^\\-]+-([^\\-]+)$", "\\1-\\2"),TRUE ~ str_extract(Grupo, "-\\d+$")))
  
  df_dividido <- split(Promedios_modular, Promedios_modular$Generación) #Dividir por generación
  
  cols_a_renombrar <- c("Mensaje de Bienvenida\nPorcentaje no ","Foro no evaluable\nPorcentaje no ", "Foro evaluable\nPorcentaje no ", "Foro de novedades\nPorcentaje no ", "Evaluación\nPorcentaje no ", "Pizarra EAA\nPorcentaje no ", "EAA\nPorcentaje no ", "Promedio si", "Promedio no", "Promedio na")
  
  # Aplicamos la transformación a cada data frame
  df_dividido_con_sufijo <- lapply(df_dividido[], function(df) {
    #sufijo <- case_when(str_detect(Grupo, "REC") ~ unique(str_extract(df$Generación,"(?<=REC).*")), TRUE ~ unique(str_extract(df$Generación, "G\\d+(?=M)"))
    #sufijo <- unique(str_extract(df$Generación, "G\\d+(?=M)"))
    sufijo <- unique(str_extract(df$Generación, "(?<=G)\\d+(?=M)|(?<=REC)\\d+"))
    # Crear nombres nuevos para columnas seleccionadas
    nuevos_nombres <- paste0(cols_a_renombrar, " ", sufijo)
    names(df)[names(df) %in% cols_a_renombrar] <- nuevos_nombres
    return(df)
  })
  
  # for (i in rev(seq_along(df_dividido_con_sufijo))) {
  #   if (i==length(df_dividido_con_sufijo)){
  #     Revision_modular <- df_dividido_con_sufijo[[i]] 
  #   } else {
  #     PruebaY <- df_dividido_con_sufijo[[i]]%>%
  #       select(-1,-2,-3,-4,-5,-6)
  #     Revision_modular <- left_join(Revision_modular, PruebaY, by = "ClaveG")
  #   }
  # }
  
  for (i in rev(seq_along(df_dividido_con_sufijo))) {
    
    if (i==length(df_dividido_con_sufijo)){
      Revision_modular <- df_dividido_con_sufijo[[i]] 
      
    } else if (i==length(df_dividido_con_sufijo)-1){
      PruebaY <- df_dividido_con_sufijo[[i]]%>%
        select(-1,-2,-3,-4,-5,-6)
      Revision_modular <- left_join(Revision_modular, PruebaY, by = "ClaveG")
      
      base_focalizada <- df_dividido_con_sufijo[[i]]
      
    }else {
      PruebaY <- df_dividido_con_sufijo[[i]]%>%
        select(-1,-2,-3,-4,-5,-6)
      
      Revision_modular <- left_join(Revision_modular, PruebaY, by = "ClaveG")
      base_focalizada <- left_join(base_focalizada, PruebaY, by = "ClaveG")
    }
  }
  
  Revision_modular <- Revision_modular%>% # Quitar columnas de identificación
    select(-contains("ClaveG"))
  
  Revision_modular$Cumplimiento <- apply(select(Revision_modular, matches("Promedio no")),1,nv_cumplimiento) # Sacar nv de cumplimiento
  
  Revision_modular <- Revision_modular %>%
    select(1:which(names(.) == "SAME"), # Todo hasta "Generación"
           matches("Promedio"),                              # La columna que quieres mover
           Cumplimiento,
           matches("Mensaje de Bienvenida"),
           matches("Foro no evaluable"),
           matches("Foro evaluable"),
           matches("Foro de novedades"),
           matches("Evaluación"),
           matches("Pizarra EAA"),
           matches("EAA"),
           everything())                      # El resto
  
  assign(nom_revision_modular,Revision_modular)
  
  base_completa_modular$numero <- as.numeric(sub("G([0-9]+)M[0-9]+", "\\1", base_completa_modular$Generación))
  base_completa_modular <- base_completa_modular[order(base_completa_modular$numero, decreasing = TRUE), ]
  base_completa_modular$numero <- NULL
  rownames(base_completa_modular) <- NULL
  
  base_completa_modular <- asignar_reactivo(base_completa_modular)
  assign(nom_base_completa_modular,base_completa_modular)
  
  tabla_cumplimiento_mod <- data.frame("Nivel de Cumplimiento" = c("Muy bueno", "Bueno", "Regular", "Malo", "Muy malo"), Grupos = c(rep(NA, 5)), check.names = FALSE)
  
  tabla_cumplimiento_mod$Grupos[1] <- sum(Revision_modular$Cumplimiento == "Muy bueno")
  tabla_cumplimiento_mod$Grupos[2] <- sum(Revision_modular$Cumplimiento == "Bueno")
  tabla_cumplimiento_mod$Grupos[3] <- sum(Revision_modular$Cumplimiento == "Regular")
  tabla_cumplimiento_mod$Grupos[4] <- sum(Revision_modular$Cumplimiento == "Malo")
  tabla_cumplimiento_mod$Grupos[5] <- sum(Revision_modular$Cumplimiento == "Muy malo")
  
  if (as.numeric(modular) > 100) {
    suf_foc1 <- paste0("REC ")
  } else {
    suf_foc1 <- paste0("G")
  }
  suf_foc2 <- Generaciones[length(Generaciones)-1]
  
  #names(tabla_cumplimiento_mod)[2] <- "" # Eliminar el nombre de la segunda columna
  names(tabla_cumplimiento_mod)[1] <- paste0("Nivel de Cumplimiento ",suf_foc1,suf_foc2)
  names(tabla_cumplimiento_mod)[2] <- paste0(suf_foc1,suf_foc2)
  
  assign(nom_tabla_cumplimiento_mod, tabla_cumplimiento_mod)
  
  intervenciones_mv <- list(
    mensaje_bienvenida = data.frame(),
    foro_no_evaluable = data.frame(),
    foro_evaluable = data.frame(),
    foro_novedades = data.frame(),
    evaluacion = data.frame(),
    pizarraEAA = data.frame(),
    EAA = data.frame())
  
  for (i in 1:(length(categorias_mv)-1)) {
    
    df_inter <- intervenciones_func(categorias_mv[i])
    intervenciones_mv[[i]] <- df_inter #Agregar # y % a una lista
    
    if (i==1){
      revision_focalizado <- df_inter %>%
        select(AV, Correo, Estatus) %>%
        rename(!!categorias_mv[i] := Estatus)
    } else {
      df_inter <- df_inter %>%
        select(AV, Estatus) %>%
        rename(!!categorias_mv[i] := Estatus)
      revision_focalizado <- left_join(revision_focalizado, df_inter, by = "AV", "Correo")
    }
    
  }
  
  # # Este sería en caso de prope
  # intervenciones_func <- function(mv) {
  #   df <- Revision_modular %>%
  #     #select(matches("AV|Correo"), matches(mv))
  #     select(matches("AV|Correo"), matches(paste0("^", mv)))
  #   
  #   df <- df %>%
  #     mutate(Estatus = case_when(
  #       df[,3] <= 0.2 ~ "B/D",
  #       df[,3] >= 0.4 ~ "I/I",
  #       #apply(.[, 3:ncol(.)], 1, function(x) all(x > 0)) ~ "I/R",
  #       df[,3] >= 0.2 ~ "R",
  #       TRUE ~ ""  # equivalente a SI.ERROR(...,"")
  #     ))  
  # }
  
  revision_focalizado <- revision_focalizado %>%
    mutate('Intervenciones' = apply(revision_focalizado[,], 1, function(fila) sum(grepl("^I/", iconv(fila, to = "ASCII//TRANSLIT"), ignore.case = TRUE))))%>%
    arrange(desc(Intervenciones)) #%>%
  #rename(!!'# de intervenciones' := Intervenciones)
  
  assign(nom_intervenciones_mv, intervenciones_mv)
  assign(nom_revision_focalizado, revision_focalizado)
  
  base_focalizada <- base_focalizada%>% # Quitar columnas de identificación
    select(-contains("ClaveG"))
  
  base_focalizada$Cumplimiento <- apply(select(base_focalizada, matches("Promedio no")),1,nv_cumplimiento) # Sacar nv de cumplimiento
  
  tabla_cumplimiento_foc <- data.frame("Nivel de Cumplimiento" = c("Muy bueno", "Bueno", "Regular", "Malo", "Muy malo"), Grupos = c(rep(NA, 5)), check.names = FALSE)
  
  tabla_cumplimiento_foc$Grupos[1] <- sum(base_focalizada$Cumplimiento == "Muy bueno")
  tabla_cumplimiento_foc$Grupos[2] <- sum(base_focalizada$Cumplimiento == "Bueno")
  tabla_cumplimiento_foc$Grupos[3] <- sum(base_focalizada$Cumplimiento == "Regular")
  tabla_cumplimiento_foc$Grupos[4] <- sum(base_focalizada$Cumplimiento == "Malo")
  tabla_cumplimiento_foc$Grupos[5] <- sum(base_focalizada$Cumplimiento == "Muy malo")
  
  #names(tabla_cumplimiento_foc)[2] <- "" # Eliminar el nombre de la segunda columna
  
  names(tabla_cumplimiento_foc)[1] <- paste0("Nivel de Cumplimiento ",suf_foc1,suf_foc2)
  names(tabla_cumplimiento_foc)[2] <- paste0(suf_foc1,suf_foc2)
  
  if (as.numeric(modular) <= 23){
    print(paste0("Analizando histórico de cumplimiento focalizado de M",modular))
    tabla_cumplimiento_foc <- bind_cols(tabla_cumplimiento_foc, datasets_foc_cumav_cump_hist[[paste0("M",modular)]])
    #   # tabla_cumplimiento_foc <- tabla_cumplimiento_foc %>%
    #   #   select(1,2,3,4,5,6)
  }
  
  assign(nom_tabla_foc,tabla_cumplimiento_foc)
  
  av_reg_malo <- base_focalizada %>%
    filter(Cumplimiento %in% c("Regular", "Malo", "Muy malo")) %>%
    select(matches("AV|Cumplimiento"))%>% 
    arrange(Cumplimiento)
  
  colnames(av_reg_malo) <- paste0(colnames(av_reg_malo), " ",suf_foc1,suf_foc2)
  
  if (as.numeric(modular) <= 23){
    print(paste0("Analizando histórico de av focalizado de M",modular))
    av_reg_malo <- safe_bind_cols(av_reg_malo,datasets_foc_cumav_avreg_hist[[paste0("M", modular)]])
  }
  
  assign(nom_av_reg_malo,av_reg_malo)
  
  gen_actual_foc <- df_dividido[[length(df_dividido)-1]]
  
  for (i in 1:(length(categorias_mv)-1)) {
    
    categoria <- categorias_mv[i]
    cum_1 <- gen_actual_foc %>%
      summarise(across(matches(paste0("^", categorias_mv[i])),~ round(mean(.x == 0) * 100, 2)))
    
    cum_2 <- gen_actual_foc %>%
      summarise(across(matches(paste0("^", categorias_mv[i])),~ round(mean(.x > 0 & .x <= 40) * 100,2)))
    
    cum_3 <- gen_actual_foc %>%
      summarise(across(matches(paste0("^", categorias_mv[i])), ~ round(mean(.x > 40) * 100,2)))
    
    total <- cum_1+cum_2+cum_3
    
    cump <- cbind(categoria,cum_1,cum_2, cum_3,total)
    
    colnames(cump) <- c("Categoría", "Cumplimiento al 100%", "Incumplimiento mayor al 40%", "Incumplimiento menor al 40%", "Total")
    
    if (i==1) { # Si no existe el dataframe de Generación por semana
      cumplimiento <- cump
    } else {
      cumplimiento <- rbind(cumplimiento,cump)
    }
  }
  
  assign(nom_cumplimiento, cumplimiento)  
  
  list_df_sm <- mget(ls(pattern = "^Semana_"))
  
  list_df_sm <- list_df_sm %>% #Transponer cada data frame para que cada fila corresponda a una generación
    map(~ {
      df_sm <- as.data.frame(t(.))# Transponer y convertir a data.frame
      df_sm <- df_sm[, -1]# Eliminar la primera columna
      return(df_sm) # Devolver el data.frame modificado
    })
  
  list_df_sm <- list_df_sm %>% #Dividir para que cada data.frame correspoda a generación y semana
    map(~ {num_partes <- ceiling(ncol(.) / 7) # Dividir el dataframe en partes de 6 columnas (Cambiar núm de columnas dependiendo de cuántos datos se tiene por fila)
    
    partes <- map(1:num_partes, function(i) {
      start_col <- (i - 1) * 7 + 1
      end_col <- min(i * 7, ncol(.))
      parte <- .[, start_col:end_col]
      
      colnames(parte) <- c("Generación", "Semana", "IDReactivo", "Reactivo", "Sí", "No", "NAN")# Asignar nombres de columnas a cada parte
      
      return(parte)
    })
    bind_rows(partes) # Unir todas las partes nuevamente en un solo data.frame
    })
  # # Valores que quieres eliminar
  # valores_a_excluir <- c("G", tail(sort(Generaciones_global, decreasing = FALSE), 1), "M", modular)
  # 
  # # Filtrar cada data.frame de la lista
  # list_df_sm <- lapply(list_df_sm, function(df) {
  #   df[!(df$Generación %in% valores_a_excluir), ]
  # })
  
  list_df_sm <- list_df_sm %>%
    map(~ left_join(.x, categorias_mv_completo, by = "IDReactivo"))
  
  base_completa_asignacion <- do.call(rbind, list_df_sm)
  
  filas_a_eliminar <- base_completa_asignacion$Verificacion %in% c("Duración")
  base_completa_asignacion <- base_completa_asignacion[!(base_completa_asignacion$Verificacion %in% c("Duración")), ] #Eliminar datos de duración de las sesiones
  
  # Preprocesar los datos para obtener los porcentajes correctos por IDReactivo
  base_completa_asignacion <- base_completa_asignacion %>%
    mutate(across(c("Sí", "No", "NAN"), as.numeric),  # Convertir "Sí" y "No" a numérico
           total_respuestas = (Sí + No + NAN)*4,  # Sumar respuestas "Sí" y "No"
           porcentaje_si = round(Sí / total_respuestas * 100, 2),  # Porcentaje de "Sí"
           porcentaje_no = round(No / total_respuestas * 100, 2),  # Porcentaje de "No"
           porcentaje_na = round(NAN / total_respuestas * 100, 2)   # Porcentaje de "NA"
    )
  
  base_completa_asignacion <- base_completa_asignacion %>%
    select(-Reactivo.y)%>% #Quitar columna duplicada
    rename(Reactivo = Reactivo.x) #Renombrar columna
  rownames(base_completa_asignacion) <- NULL
  
  if (as.numeric(modular) >= 23){
    base_completa_asignacion$Generación <- paste0("REC", str_extract(base_completa_asignacion$Generación, "(?<=G)\\d+(?=M)"))
  }
  
  assign(nom_sm_m,  base_completa_asignacion)
  
  lista_filtrados <- base_completa_asignacion %>%
    group_by(Generación, Semana, Verificacion) %>%
    group_split()
  
  list_sm2 <- list()
  df_temporal_sm2 <- data.frame(Generación=NA, Semana=NA, Verificacion =NA, Sí = numeric(1), No = numeric(1), NAN = numeric(1),Total = numeric(1))
  
  for (i in 1:length(lista_filtrados)) {
    
    df_temporal_sm2[,1] <- unique(lista_filtrados[[i]][["Generación"]])
    df_temporal_sm2[,2] <- unique(lista_filtrados[[i]][["Semana"]])
    df_temporal_sm2[,3] <- unique(lista_filtrados[[i]][["Verificacion"]])
    df_temporal_sm2[,4] <- round((sum(lista_filtrados[[i]][["Sí"]])*100)/sum(lista_filtrados[[i]][["Sí"]],lista_filtrados[[i]][["No"]], lista_filtrados[[i]][["NAN"]]),2)
    df_temporal_sm2[,5] <- round((sum(lista_filtrados[[i]][["No"]])*100)/sum(lista_filtrados[[i]][["Sí"]],lista_filtrados[[i]][["No"]], lista_filtrados[[i]][["NAN"]]),2)
    df_temporal_sm2[,6] <- round((sum(lista_filtrados[[i]][["NAN"]])*100)/sum(lista_filtrados[[i]][["Sí"]],lista_filtrados[[i]][["No"]], lista_filtrados[[i]][["NAN"]]),2)
    df_temporal_sm2[,7] <- sum(df_temporal_sm2[,4], df_temporal_sm2[,5], df_temporal_sm2[,6])
    
    if (df_temporal_sm2[,7]>101){
      warning("La proporción de respuestas por Generación, semana y verificación da más de 100% en la fila ",i)
    }
    
    list_sm2[[length(list_sm2) + 1]] <- df_temporal_sm2 #Agregar # y % a una lista 
    
    df_temporal_sm2 <- data.frame(Generación=NA, Semana=NA, Verificacion =NA, Sí = numeric(1), No = numeric(1), NAN = numeric(1), Total = numeric(1))
  }
  
  seg_mod_bt <- do.call(rbind, list_sm2)  
  assign(nom_sm2_m,  seg_mod_bt)
  
  list_df_sm <- list()
  rm(Semana_1, Semana_2, Semana_3, Semana_4) #Eliminar df de semanas para reiniciar datos por M
  
  base_completa_av <- do.call(rbind, list_df_av)
  colnames(base_completa_av)[11] <- "porcentaje_no"
  colnames(base_completa_av)[12] <- "porcentaje_si"
  colnames(base_completa_av)[13] <- "porcentaje_nan"
  if (as.numeric(modular) > 100) {
    base_completa_av$Generación <- paste0("REC", str_extract(base_completa_av$Generación, "(?<=G)\\d+(?=M)"))
  }
  
  assign(nom_frec_porc_mod, base_completa_av)
  
  
  # list_df_av <- list()
  # 
  # todos_los_objetos <- ls()
  # objetos_a_guardar <- grep("^(seg_mod_up_M|seg_mod_bt_M|tabla_cumpl_mod|revision_mod_M|base_completa_mod_|mod_sem1_M|mod_sem2_M|mod_sem3_M|mod_sem4_M|revision_foc_M|tabla_cumpl_foc_M|tabla_cumpl_foc_M|av_reg_malo_M|frec_porc_M|inter_mv|cumplimiento_M|categorias_mv_1|categorias_mv_2)", todos_los_objetos, value = TRUE) # 2. Filtra los que empiecen con "df_" o "mod_"
  # save(list = objetos_a_guardar, file = paste0("entorno_M", modular, date_save,".RData"))# 3. Guarda solo esos objetos
  # message("Guardando ",length(objetos_a_guardar)," objetos del environment para el Módulo ",modular)
  # rm(list = ls())
} #Múdulo  

