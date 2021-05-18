#Funciones utiles y de tratamiento de manejo de la base de datos pre-graficacion ----


#Funciones de filtrado: ----
#Filtrado por fecha
filtro_fecha <- function(base, fecha_inicial, fecha_final){
  base[(fecha_min >= fecha_inicial & fecha_min <= fecha_final),]
}

filtro_fecha_fallecimiento <- function(base, fecha_inicial, fecha_final){
  base[!is.na(fecha_fallecimiento) & (fecha_fallecimiento >= fecha_inicial & fecha_fallecimiento <= fecha_final),]
}

filtro_prov <- function(base, prov_p){
  base[prov_p == prov,]
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

filtro_depto <- function(base, depto_p){
  base[depto_p == depto,]
}


#Agrupamientos:
agrupamiento_argentina <- function(base){
  grupos <- names(base)[!names(base) %in% c("prov", "depto", "confirmados", "confirmados_14", "casos_posibles", "casos_posibles_14", "fallecidos", "fallecidos_14", "positividad", "letalidad")]
  c <- base[, .(confirmados = sum(confirmados),
                confirmados_14 = sum(confirmados_14),
                casos_posibles = sum(casos_posibles),
                casos_posibles_14 = sum(casos_posibles_14),
                fallecidos = sum(fallecidos),
                fallecidos_14 = sum(fallecidos_14)), by = mget(grupos)]
  c
}

agrupamiento_provincias <- function(base){
  grupos <- names(base)[!names(base) %in% c("depto", "confirmados", "confirmados_14", "casos_posibles", "casos_posibles_14", "fallecidos", "fallecidos_14", "positividad", "letalidad")]
  c <- base[, .(confirmados = sum(confirmados),
                confirmados_14 = sum(confirmados_14),
                casos_posibles = sum(casos_posibles),
                casos_posibles_14 = sum(casos_posibles_14),
                fallecidos = sum(fallecidos),
                fallecidos_14 = sum(fallecidos_14)), by = mget(grupos)]
  c
}


#Calculo de acumulados despues de filtrar por la fecha y de agrupar:
agregar_acumulados_positividad_letalidad <- function(base){
  grupos <- names(base)[!names(base) %in% c("fecha_min", "confirmados", "confirmados_14", "casos_posibles", "casos_posibles_14", "fallecidos", "fallecidos_14", "positividad", "letalidad")]
  c <- base %>% arrange(fecha_min)
  if(length(grupos) != 0){
    c[, c("confirmados_acumulados", "casos_posibles_acumulados", "fallecidos_acumulados") := .(cumsum(confirmados), cumsum(casos_posibles), cumsum(fallecidos)), by = mget(grupos)]
  }else{
    c[, c("confirmados_acumulados", "casos_posibles_acumulados", "fallecidos_acumulados") := .(cumsum(confirmados), cumsum(casos_posibles), cumsum(fallecidos))]
  }
  c[, c("positividad", "letalidad") := .(100 * confirmados_acumulados / casos_posibles_acumulados, 100 * fallecidos_acumulados / confirmados_acumulados)]
  c
}

#Razón e incidencia:
razon_incidencia <- function(base){
  c <- base
  prov_code <- base[1, prov]
  c <- c[fecha_min >= as.Date(fecha_maxima) - 29][, dias := fcase(fecha_min < fecha_maxima - 14, "Anteriores",
                                                                  fecha_min >= fecha_maxima - 14, "Ultimos")][, .(conteo = sum(confirmados)), by = .(dias, depto)]
  c <- dcast(c, ... ~ dias, value.var = "conteo")[, razon := round(Ultimos / Anteriores, 2)]
  c <- merge(poblaciones[province_code == prov_code], c[, -c("Anteriores")], by.x = "department_code", by.y = "depto", all.x = TRUE)
  c[, incidencia := round(100000 * Ultimos / department_poblacion, 2)]
  c[razon == Inf, "razon"] <- NA 
  c <- c[, .(department_name, razon, incidencia)] %>% arrange(department_name)
  if(prov_code == 2){
    x <- paste("Comuna", 1:15)
    c <- c %>%
      slice(match(x, department_name))
    names(c) <- c("Comuna", "Razón", "Incidencia")
    c
  }else if(prov_code == 6){
    names(c) <- c("Partido", "Razón", "Incidencia")
    c
  }else{
    names(c) <- c("Departamento", "Razón", "Incidencia")
    c
  }
}
