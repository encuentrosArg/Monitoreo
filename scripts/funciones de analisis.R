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


#Las bases que se usan deben ser funciones para que puedan ser filtradas por lo seleccionado en el shiny -----
#casos diarios filtrado por fechas, provincia,  municipio -----
confirmados <- function(base, prov, depto,  fecha_inicial, fecha_final){
  c <- base[clasificacion_resumen == "Confirmado" &
              residencia_provincia_id == prov &
              residencia_departamento_id == depto &
              between(fecha_min, fecha_inicial, fecha_final),][, .(conteo = .N), by = fecha_min]
  relleno <- todas_fechas(fecha_inicial, fecha_final)
  c <- merge(relleno, c, by = "fecha_min", all = TRUE)
  c[ , "conteo" := lapply(.SD, nafill, fill=0), .SDcols = "conteo"]
  c
}


#casos diarios por rango de edad filtrado por fechas, provincia,  municipio -----
confirmados_r_edad <- function(base, prov, depto,  fecha_inicial, fecha_final){
  c <- base[clasificacion_resumen == "Confirmado" &
              residencia_provincia_id == prov &
              residencia_departamento_id == depto &
              between(fecha_min, fecha_inicial, fecha_final),][, .(conteo = .N), by = .(fecha_min, r_edad)]
  relleno <- todas_fechas_r_edad(fecha_inicial, fecha_final)
  c <- merge(relleno, c, by = c("fecha_min", "r_edad"), all = TRUE)
  c[ , "conteo" := lapply(.SD, nafill, fill=0), .SDcols = "conteo"]
  c
}


#cantidad de casos posibles filtrado por fechas, provincia, municipio -----
casos_posibles <- function(base, prov, depto,  fecha_inicial, fecha_final){
  c <- base[residencia_provincia_id == prov &
              residencia_departamento_id == depto &
              between(fecha_min, fecha_inicial, fecha_final),][, .(conteo = .N), by = fecha_min]
  relleno <- todas_fechas(fecha_inicial, fecha_final)
  c <- merge(relleno, c, by = "fecha_min", all = TRUE)
  c[ , "conteo" := lapply(.SD, nafill, fill=0), .SDcols = "conteo"]
  c
}


#cantidad de testeos por rango de edad filtrado por fechas, provincia, municipio -----
casos_posibles_r_edad <- function(base, prov, depto,  fecha_inicial, fecha_final){
  c <- base[residencia_provincia_id == prov &
              residencia_departamento_id == depto &
              between(fecha_min, fecha_inicial, fecha_final),][, .(conteo = .N), by = .(fecha_min, r_edad)]
  relleno <- todas_fechas_r_edad(fecha_inicial, fecha_final)
  c <- merge(relleno, c, by = c("fecha_min", "r_edad"), all = TRUE)
  c[ , "conteo" := lapply(.SD, nafill, fill=0), .SDcols = "conteo"]
  c
}


#fallecidos filtrado por fechas, provincia, municipio ----
fallecidos <- function(base, prov, depto,  fecha_inicial, fecha_final){
  f <- base[!is.na(fecha_fallecimiento) &
              residencia_provincia_id == prov &
              residencia_departamento_id == depto &
              between(fecha_min, fecha_inicial, fecha_final),][, .(conteo = .N), by = fecha_fallecimiento]
  relleno <- todas_fechas(fecha_inicial, fecha_final, "fecha_fallecimiento")
  f <- merge(relleno, f, by = "fecha_fallecimiento", all = TRUE)
  f[ , "conteo" := lapply(.SD, nafill, fill=0), .SDcols = "conteo"]
  f
}

#fallecidos por rango de edad filtrado por fechas, provincia, municipio ----
fallecidos_r_edad <- function(base, prov, depto,  fecha_inicial, fecha_final){
  f <- base[!is.na(fecha_fallecimiento) &
              residencia_provincia_id == prov &
              residencia_departamento_id == depto &
              between(fecha_min, fecha_inicial, fecha_final),][, .(conteo = .N), by = .(fecha_fallecimiento, r_edad)]
  relleno <- todas_fechas_r_edad(fecha_inicial, fecha_final, "fecha_fallecimiento")
  f <- merge(relleno, f, by = c("fecha_fallecimiento", "r_edad"),all = TRUE)
  f[ , "conteo" := lapply(.SD, nafill, fill=0), .SDcols = "conteo"]
  f
}


#positividad entre fechas filtrado por provincia, municipio -----
positividad <- function(base, prov, depto,  fecha_inicial, fecha_final){
  p <- merge(confirmados(base = base, prov = prov, depto = depto, fecha_inicial = fecha_inicial, fecha_final = fecha_final),
             casos_posibles(base = base, prov = prov, depto = depto, fecha_inicial = fecha_inicial, fecha_final = fecha_final),
             by = "fecha_min", all = TRUE)
  relleno <- todas_fechas(fecha_inicial, fecha_final)
  p <- merge(relleno, p, by = "fecha_min", all = TRUE)
  p <- p[ , c("conteo.x", "conteo.y") := lapply(.SD, nafill, fill=0), .SDcols = c("conteo.x", "conteo.y")]
  p <- p[, c("conteo.x", "conteo.y") := .(cumsum(conteo.x), cumsum(conteo.y))]
  p <- p[, positividad := conteo.x/conteo.y]
  p <- p[, c("fecha_min", "positividad")] 
  p
}


#positividad por rango de edades entre fechas filtrado por provincia, municipio -----
positividad_r_edad <- function(base, prov, depto,  fecha_inicial, fecha_final){
  p <- merge(confirmados_r_edad(base = base, prov = prov, depto = depto, fecha_inicial = fecha_inicial, fecha_final = fecha_final),
             casos_posibles_r_edad(base = base, prov = prov, depto = depto, fecha_inicial = fecha_inicial, fecha_final = fecha_final),
             by = c("fecha_min", "r_edad"), all = TRUE)
  relleno <- todas_fechas_r_edad(fecha_inicial, fecha_final)
  p <- merge(relleno, p, by = c("fecha_min", "r_edad"), all = TRUE)
  p <- p[ , c("conteo.x", "conteo.y") := lapply(.SD, nafill, fill=0), .SDcols = c("conteo.x", "conteo.y")]
  p <- p[, c("conteo.x", "conteo.y") := .(cumsum(conteo.x), cumsum(conteo.y))]
  p <- p[, positividad := conteo.x/conteo.y]
  p <- p[, c("fecha_min", "r_edad", "positividad")] 
  p
}


#letalidad entre fechas filtrado por provincia, municipio -----
letalidad<- function(base, prov, depto,  fecha_inicial, fecha_final){
  l <- merge(fallecidos(base = base, prov = prov, depto = depto, fecha_inicial = fecha_inicial, fecha_final = fecha_final),
             casos_posibles(base = base, prov = prov, depto = depto, fecha_inicial = fecha_inicial, fecha_final = fecha_final),
             by.x = "fecha_fallecimiento", by.y = "fecha_min", all = TRUE)
  relleno <- todas_fechas(fecha_inicial, fecha_final, "fecha_fallecimiento")
  l <- merge(relleno, l, by = "fecha_fallecimiento", all = TRUE)
  l <- l[ , c("conteo.x", "conteo.y") := lapply(.SD, nafill, fill=0), .SDcols = c("conteo.x", "conteo.y")]
  l <- l[, c("conteo.x", "conteo.y") := .(cumsum(conteo.x), cumsum(conteo.y))]
  l <- l[, letalidad := conteo.x/conteo.y]
  l <- l[, c("fecha_fallecimiento", "letalidad")] 
  l
}


#letalidad por rango de edad entre fechas filtrado por provincia, municipio -----
letalidad_r_edad <- function(base, prov, depto,  fecha_inicial, fecha_final){
  l <- merge(fallecidos_r_edad(base = base, prov = prov, depto = depto, fecha_inicial = fecha_inicial, fecha_final = fecha_final),
             casos_posibles_r_edad(base = base, prov = prov, depto = depto, fecha_inicial = fecha_inicial, fecha_final = fecha_final),
             by.x = c("fecha_fallecimiento", "r_edad"), by.y = c("fecha_min", "r_edad"), all = TRUE)
  relleno <- todas_fechas_r_edad(fecha_inicial, fecha_final, "fecha_fallecimiento")
  l <- merge(relleno, l, by = c("fecha_fallecimiento", "r_edad"), all = TRUE)
  l <- l[ , c("conteo.x", "conteo.y") := lapply(.SD, nafill, fill=0), .SDcols = c("conteo.x", "conteo.y")]
  l <- l[, c("conteo.x", "conteo.y") := .(cumsum(conteo.x), cumsum(conteo.y))]
  l <- l[, letalidad := conteo.x/conteo.y]
  l <- l[, c("fecha_fallecimiento", "r_edad", "letalidad")] 
  l
}

