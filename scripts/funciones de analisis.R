#Funciones utiles y de tratamiento de manejo de la base de datos pre-graficacion ----

#Objetos y funciones utiles ----
niveles_r_edad <- c("0-17", "18-39", "40-59", "60 o más", "N/R")

#Funcion que genera todas combinaciones de dos strings siendo una la fecha:

todas_fechas <- function(fecha_inicial, fecha_final, nombre_fecha = "fecha_min"){
  d <- data.table(as.IDate(seq(fecha_inicial, fecha_final, by ="days")))
  names(d) <- nombre_fecha
  d
}

todas_fechas_r_edad <- function(fecha_inicial, fecha_final, nombre_fecha = "fecha_min"){
  e <- data.table(expand.grid(as.IDate(seq(fecha_inicial, fecha_final, by ="days")), niveles_r_edad))
  names(e) <- c(nombre_fecha, "r_edad")
  e
}

#Funciones de filtrado: ----
#Filtrado por fecha
filtro_fecha <- function(base, fecha_inicial, fecha_final){
  base[(fecha_min >= fecha_inicial & fecha_min <= fecha_final),]
}

filtro_fecha_fallecimiento <- function(base, fecha_inicial, fecha_final){
  base[!is.na(fecha_fallecimiento) & (fecha_fallecimiento >= fecha_inicial & fecha_fallecimiento <= fecha_final),]
}

filtro_prov <- function(base, prov){
  base[residencia_provincia_id == prov,]
}

filtro_region <- function(base, base_codigos, region_name){
  cod <- base_codigos[region == region_name,]
  if(region_name == "AMBA"){
    bsas <- cod[prov_code == 6, codigo]
    base[(residencia_provincia_id == 6 & residencia_departamento_id %in% bsas) | residencia_provincia_id == 2,]
  }else if(region_name == "Buenos Aires no AMBA"){
    bsas <- cod[prov_code == 6, codigo]
    base[(residencia_provincia_id == 6 & residencia_departamento_id %in% bsas),]
  }else{
    provincias <- unique(cod$prov_code)
    base[residencia_provincia_id %in% cod,]
  }
}

filtro_depto <- function(base, depto){
  base[residencia_departamento_id == depto,]
}


#Las bases que se usan deben ser funciones para que puedan ser filtradas por lo seleccionado en el shiny -----
#casos diarios filtrado por fechas, provincia,  municipio -----
confirmados <- function(base){
  c <- base[clasificacion_resumen == "Confirmado" ,][, .(conteo = .N), by = fecha_min]
  relleno <- todas_fechas(min(c$fecha_min), max(c$fecha_min))
  c <- merge(relleno, c, by = "fecha_min", all = TRUE)
  c[ , "conteo" := lapply(.SD, nafill, fill=0), .SDcols = "conteo"]
  c %>%
    mutate(label = str_glue("<b>Fecha</b>: {format({as.Date(fecha_min)}, format = '%d/%m/%Y')}<br><b>Cantidad</b>: {conteo}<br>") %>% 
        map(htmltools::HTML))
}

#confirmados acumulados, usa la base que sale de confirmados:
confirmados_acumulados <- function(base){
  c <- base[, "conteo" := cumsum(conteo)]
  c %>%
    mutate(label = str_glue("<b>Fecha</b>: {format({as.Date(fecha_min)}, format = '%d/%m/%Y')}<br><b>Cantidad</b>: {conteo}<br>") %>% 
             map(htmltools::HTML))
}


#casos diarios por rango de edad filtrado por fechas, provincia,  municipio -----
confirmados_r_edad <- function(base){
  c <- base[clasificacion_resumen == "Confirmado",][, .(conteo = .N), by = .(fecha_min, r_edad)]
  relleno <- todas_fechas_r_edad(min(c$fecha_min), max(c$fecha_min))
  c <- merge(relleno, c, by = c("fecha_min", "r_edad"), all = TRUE)
  c[ , "conteo" := lapply(.SD, nafill, fill=0), .SDcols = "conteo"]
  c %>%
    mutate(label = str_glue("<b>Fecha</b>: {format({as.Date(fecha_min)}, format = '%d/%m/%Y')}<br><b>Edad</b>: {r_edad}<br><b>Cantidad</b>: {conteo}<br>") %>% 
             map(htmltools::HTML))
}

#confirmados acumulados, usa la base que sale de confirmados:
confirmados_r_edad_acumulados <- function(base){
  c <- base[, "conteo" := cumsum(conteo), by ="r_edad"]
  c %>%
    mutate(label = str_glue("<b>Fecha</b>: {format({as.Date(fecha_min)}, format = '%d/%m/%Y')}<br><b>Edad</b>: {r_edad}<br><b>Cantidad</b>: {conteo}<br>") %>% 
             map(htmltools::HTML))
}


#cantidad de casos posibles filtrado por fechas, provincia, municipio -----
casos_posibles <- function(base){
  c <- base[, .(conteo = .N), by = fecha_min]
  relleno <- todas_fechas(min(c$fecha_min), max(c$fecha_min))
  c <- merge(relleno, c, by = "fecha_min", all = TRUE)
  c[ , "conteo" := lapply(.SD, nafill, fill=0), .SDcols = "conteo"]
  c %>%
    mutate(label = str_glue("<b>Fecha</b>: {format({as.Date(fecha_min)}, format = '%d/%m/%Y')}<br><b>Cantidad</b>: {conteo}<br>") %>% 
             map(htmltools::HTML))
}

#casos_posibles acumulados, usa la base que sale de casos_posibles:
casos_posibles_acumulados <- function(base){
  c <- base[, "conteo" := cumsum(conteo)]
  c %>%
    mutate(label = str_glue("<b>Fecha</b>: {format({as.Date(fecha_min)}, format = '%d/%m/%Y')}<br><b>Cantidad</b>: {conteo}<br>") %>% 
             map(htmltools::HTML))
}


#cantidad de testeos por rango de edad filtrado por fechas, provincia, municipio -----
casos_posibles_r_edad <- function(base){
  c <- base[, .(conteo = .N), by = .(fecha_min, r_edad)]
  relleno <- todas_fechas_r_edad(min(c$fecha_min), max(c$fecha_min))
  c <- merge(relleno, c, by = c("fecha_min", "r_edad"), all = TRUE)
  c[ , "conteo" := lapply(.SD, nafill, fill=0), .SDcols = "conteo"]
  c %>%
    mutate(label = str_glue("<b>Fecha</b>: {format({as.Date(fecha_min)}, format = '%d/%m/%Y')}<br><b>Edad</b>: {r_edad}<br><b>Cantidad</b>: {conteo}<br>") %>% 
             map(htmltools::HTML))
}

#casos_posibles acumulados, usa la base que sale de casos_posibles:
casos_posibles_r_edad_acumulados <- function(base){
  c <- base[, "conteo" := cumsum(conteo), by ="r_edad"]
  c %>%
    mutate(label = str_glue("<b>Fecha</b>: {format({as.Date(fecha_min)}, format = '%d/%m/%Y')}<br><b>Edad</b>: {r_edad}<br><b>Cantidad</b>: {conteo}<br>") %>% 
             map(htmltools::HTML))
}


#fallecidos filtrado por fechas, provincia, municipio ----
fallecidos <- function(base){
  f <- base[, .(conteo = .N), by = fecha_fallecimiento]
  relleno <- todas_fechas(min(f$fecha_fallecimiento), max(f$fecha_fallecimiento), "fecha_fallecimiento")
  f <- merge(relleno, f, by = "fecha_fallecimiento", all = TRUE)
  f[ , "conteo" := lapply(.SD, nafill, fill=0), .SDcols = "conteo"]
  f %>%
    mutate(label = str_glue("<b>Fecha</b>: {format({as.Date(fecha_fallecimiento)}, format = '%d/%m/%Y')}<br><b>Cantidad</b>: {conteo}<br>") %>% 
             map(htmltools::HTML))
}

#fallecidos acumulados, usa la base que sale de fallecidos:
fallecidos_acumulados <- function(base){
  c <- base[, "conteo" := cumsum(conteo)]
  c %>%
    mutate(label = str_glue("<b>Fecha</b>: {format({as.Date(fecha_fallecimiento)}, format = '%d/%m/%Y')}<br><b>Cantidad</b>: {conteo}<br>") %>% 
             map(htmltools::HTML))
}

#fallecidos por rango de edad filtrado por fechas, provincia, municipio ----
fallecidos_r_edad <- function(base){
  f <- base[, .(conteo = .N), by = .(fecha_fallecimiento, r_edad)]
  relleno <- todas_fechas_r_edad(min(f$fecha_fallecimiento), max(f$fecha_fallecimiento), "fecha_fallecimiento")
  f <- merge(relleno, f, by = c("fecha_fallecimiento", "r_edad"),all = TRUE)
  f[ , "conteo" := lapply(.SD, nafill, fill=0), .SDcols = "conteo"]
  f  %>%
    mutate(label = str_glue("<b>Fecha</b>: {format({as.Date(fecha_fallecimiento)}, format = '%d/%m/%Y')}<br><b>Edad</b>: {r_edad}<br><b>Cantidad</b>: {conteo}<br>") %>% 
             map(htmltools::HTML))
}

#fallecidos acumulados, usa la base que sale de fallecidos:
fallecidos_r_edad_acumulados <- function(base){
  c <- base[, "conteo" := cumsum(conteo), by ="r_edad"]
  c %>%
    mutate(label = str_glue("<b>Fecha</b>: {format({as.Date(fecha_fallecimiento)}, format = '%d/%m/%Y')}<br><b>Edad</b>: {r_edad}<br><b>Cantidad</b>: {conteo}<br>") %>% 
             map(htmltools::HTML))
}


#positividad entre fechas filtrado por provincia, municipio -----
positividad <- function(base_confirmados, base_casos_posibles){
  p <- merge(base_confirmados, base_casos_posibles, by = "fecha_min", all = TRUE)
  relleno <- todas_fechas(min(p$fecha_min), max(p$fecha_min))
  p <- merge(relleno, p, by = "fecha_min", all = TRUE)
  p <- p[ , c("conteo.x", "conteo.y") := lapply(.SD, nafill, fill=0), .SDcols = c("conteo.x", "conteo.y")]
  p <- p[, c("conteo.x", "conteo.y") := .(cumsum(conteo.x), cumsum(conteo.y))]
  p <- p[, positividad := conteo.x/conteo.y]
  p <- p[, c("fecha_min", "positividad")] 
  p %>%
    mutate(label = str_glue("<b>Fecha</b>: {format({as.Date(fecha_min)}, format = '%d/%m/%Y')}<br><b>Positividad</b>: {round(positividad,3)}<br>") %>% 
             map(htmltools::HTML))
}


#positividad por rango de edades entre fechas filtrado por provincia, municipio -----
positividad_r_edad <- function(base_confirmados_r_edad, base_casos_posibles_r_edad){
  p <- merge(base_confirmados_r_edad, base_casos_posibles_r_edad, by = c("fecha_min", "r_edad"), all = TRUE)
  relleno <- todas_fechas_r_edad(min(p$fecha_min), max(p$fecha_min))
  p <- merge(relleno, p, by = c("fecha_min", "r_edad"), all = TRUE)
  p <- p[ , c("conteo.x", "conteo.y") := lapply(.SD, nafill, fill=0), .SDcols = c("conteo.x", "conteo.y")]
  p <- p[, c("conteo.x", "conteo.y") := .(cumsum(conteo.x), cumsum(conteo.y)), by = "r_edad"]
  p <- p[, positividad := conteo.x/conteo.y]
  p <- p[, c("fecha_min", "r_edad", "positividad")] 
  p  %>%
    mutate(label = str_glue("<b>Fecha</b>: {format({as.Date(fecha_min)}, format = '%d/%m/%Y')}<br><b>Edad</b>: {r_edad}<br><b>Positividad</b>: {round(positividad,3)}<br>") %>% 
             map(htmltools::HTML))
}


#letalidad entre fechas filtrado por provincia, municipio -----
letalidad<- function(base_fallecidos, base_casos_posibles){
  l <- merge(base_fallecidos, base_casos_posibles, by.x = "fecha_fallecimiento", by.y = "fecha_min", all = TRUE)
  relleno <- todas_fechas(min(l$fecha_fallecimiento), max(l$fecha_fallecimiento), "fecha_fallecimiento")
  l <- merge(relleno, l, by = "fecha_fallecimiento", all = TRUE)
  l <- l[ , c("conteo.x", "conteo.y") := lapply(.SD, nafill, fill=0), .SDcols = c("conteo.x", "conteo.y")]
  l <- l[, c("conteo.x", "conteo.y") := .(cumsum(conteo.x), cumsum(conteo.y))]
  l <- l[, letalidad := conteo.x/conteo.y]
  l <- l[, c("fecha_fallecimiento", "letalidad")] 
  l  %>%
    mutate(label = str_glue("<b>Fecha</b>: {format({as.Date(fecha_fallecimiento)}, format = '%d/%m/%Y')}<br><b>Letalidad</b>: {round(letalidad,3)}<br>") %>% 
             map(htmltools::HTML))
}


#letalidad por rango de edad entre fechas filtrado por provincia, municipio -----
letalidad_r_edad <- function(base_fallecidos_r_edad, base_casos_posibles_r_edad){
  l <- merge(base_fallecidos_r_edad, base_casos_posibles_r_edad, by.x = c("fecha_fallecimiento", "r_edad"), by.y = c("fecha_min", "r_edad"), all = TRUE)
  relleno <- todas_fechas_r_edad(min(l$fecha_fallecimiento), max(l$fecha_fallecimiento), "fecha_fallecimiento")
  l <- merge(relleno, l, by = c("fecha_fallecimiento", "r_edad"), all = TRUE)
  l <- l[ , c("conteo.x", "conteo.y") := lapply(.SD, nafill, fill=0), .SDcols = c("conteo.x", "conteo.y")]
  l <- l[, c("conteo.x", "conteo.y") := .(cumsum(conteo.x), cumsum(conteo.y)), by = "r_edad"]
  l <- l[, letalidad := conteo.x/conteo.y]
  l <- l[, c("fecha_fallecimiento", "r_edad", "letalidad")] 
  l %>%
    mutate(label = str_glue("<b>Fecha</b>: {format({as.Date(fecha_fallecimiento)}, format = '%d/%m/%Y')}<br><b>Edad</b>: {r_edad}<br><b>Letalidad</b>: {round(letalidad,3)}<br>") %>% 
             map(htmltools::HTML))
}

