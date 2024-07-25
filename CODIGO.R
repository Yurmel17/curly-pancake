# Cargar paquetes necesarios 

install.packages("readxl", repos='https://cloud.r-project.org') 

install.packages("writexl", repos='https://cloud.r-project.org') 

install.packages("tidyverse", repos='https://cloud.r-project.org')  # Este incluye tidyr y dplyr 

install.packages("stringr", repos='https://cloud.r-project.org') 

install.packages("openxlsx", repos='https://cloud.r-project.org') 



library(readxl) 

library(writexl) 

library(tidyr) 

library(dplyr) 

library(stringr)   

library(openxlsx) 


# Especifica la ruta del archivo 
# file.choose() 

# Especifica la ruta del archivo 
ruta_excel <-"C:\\Users\\Asus\\Downloads\\EMPRESAS DE LA MUESTRA\\KNU INDUSTRY COOPERATION FOUNDATION.xlsx"

# Leer el archivo Excel 
data <- read_excel(ruta_excel) 

# Verificar que data es un data frame 
if (!is.data.frame(data)) { 
  
  stop("El objeto data no es un data frame") 
  
} 

# Mostrar los primeros registros para verificar la estructura 
print(head(data)) 

# Verificar los nombres de las columnas 
print(colnames(data)) 


# Asegurarte de que la columna Application Date existe 

if (!"Application Date" %in% colnames(data)) { 
  
  stop("La columna 'Application Date' no existe en el data frame") 
  
} 


# Convertir Application Date a fecha 
data <- data %>% 
  
  mutate(Application_Date = as.Date(`Application Date`, format = "%d.%m.%Y")) 


# Función para filtrar por rango de años y contar patentes 

filtrar_y_contar <- function(data, start_year, end_year) { 
  
  data_filtrada <- data %>% 
    
    filter(format(Application_Date, "%Y") >= start_year & format(Application_Date, "%Y") <= end_year) 
  
  
  
  conteo_por_ano <- data_filtrada %>% 
    
    group_by(ano = format(Application_Date, "%Y")) %>% 
    
    summarize(conteo = n()) 
  
  
  
  total_patentes <- nrow(data_filtrada) 
  
  rango <- paste(start_year, end_year, sep = "-") 
  
  
  
  list(data = data_filtrada, conteo_por_ano = conteo_por_ano, total_patentes = total_patentes, rango = rango) 
  
} 


# Filtrar por los rangos especificados 

rango_1 <- filtrar_y_contar(data, 2012, 2016) 
rango_2 <- filtrar_y_contar(data, 2013, 2017) 
rango_3 <- filtrar_y_contar(data, 2014, 2018) 





#CÓMO HALLAR LA DISTANCIA TECNOLÓGICA 



# Crear un nuevo data frame con los resultados resumidos 

df_rango_1 <- rango_1$data 
df_rango_2 <- rango_2$data 
df_rango_3 <- rango_3$data 


# Comprobar si la columna "Application Id" existe 

if(!"Application Id" %in% names(df_rango_1)) { 
  
  stop("La columna 'Application Id' no existe en df_rango_1") 
  
} 

if(!"Application Id" %in% names(df_rango_2)) { 
  
  stop("La columna 'Application Id' no existe en df_rango_2") 
  
} 

if(!"Application Id" %in% names(df_rango_3)) { 
  
  stop("La columna 'Application Id' no existe en df_rango_3") 
  
} 



# Función para separar los códigos I P C en columnas y mantener las otras columnas 

separar_codigos <- function(df) { 
  
  # Separar los códigos I P C en columnas 
  
  df_codigos <- df %>% 
    
    mutate(`I P C` = strsplit(as.character(`I P C`), ";")) %>% 
    
    unnest_wider(`I P C`, names_sep = "_") 
  
  
  
  # Extraer los primeros tres dígitos de cada código I P C 
  
  df_codigos <- df_codigos %>% 
    
    mutate(across(starts_with("I P C_"), ~ substr(.x, 1, 3), .names = "Codigo_{col}")) 
  
  
  
  # Seleccionar solo las columnas con los códigos separados 
  
  df_codigos <- df_codigos %>% 
    
    select(starts_with("Codigo_")) 
  
  
  
  # Combinar el DataFrame original con los códigos separados 
  
  df_final <- bind_cols(df, df_codigos) 
  
  
  
  return(df_final) 
  
} 



# Aplicar la función a los DataFrames 

df_rango_1_niveles <- separar_codigos(df_rango_1) 

df_rango_2_niveles <- separar_codigos(df_rango_2) 

df_rango_3_niveles <- separar_codigos(df_rango_3) 



# Función para crear la matriz de comparación con columna de sumatoria 

crear_matriz_comparacion <- function(df_niveles, codigos_objetivo) { 
  
  # Obtener los nombres de las columnas que contienen los códigos I P C 
  
  columnas_codigos <- grep("Codigo_", names(df_niveles), value = TRUE) 
  
  
  
  # Obtener los Application Ids 
  
  application_ids <- df_niveles$`Application Id` 
  
  
  
  # Crear una matriz vacía con las dimensiones adecuadas 
  
  matriz <- matrix(0, nrow = length(codigos_objetivo), ncol = nrow(df_niveles)) 
  
  
  
  # Rellenar la matriz con 1 si hay coincidencia de códigos I P C 
  
  for (i in 1:nrow(df_niveles)) { 
    
    codigos_presentes <- df_niveles[i, columnas_codigos] %>% unlist() %>% na.omit() 
    
    for (j in 1:length(codigos_objetivo)) { 
      
      if (codigos_objetivo[j] %in% codigos_presentes) { 
        
        matriz[j, i] <- 1 
        
      } 
      
    } 
    
  } 
  
  
  
  # Convertir la matriz en un data frame 
  
  df_matriz <- as.data.frame(matriz) 
  
  
  
  # Añadir los nombres de columna (Application Ids) 
  
  colnames(df_matriz) <- application_ids 
  
  
  
  # Añadir los códigos como una columna al data frame 
  
  df_matriz <- cbind(Codigo = codigos_objetivo, df_matriz) 
  
  
  
  # Añadir una columna con la sumatoria por fila 
  
  df_matriz$Total <- rowSums(df_matriz[, -1])  # Excluir la columna "Codigo" en el cálculo de sumas 
  
  
  
  return(df_matriz) 
  
} 



# Definir los códigos objetivo 

codigos_objetivo <- c( 
  
  "A01", "A21", "A22", "A23", "A24", "A41", "A42", "A43", "A44",   
  
  "A45", "A46", "A47", "A61", "A62", "A63", "A99", "B01", "B02", "B03", "B04",   
  
  "B05", "B06", "B07", "B08", "B09", "B21", "B22", "B23", "B24", "B25", "B26",   
  
  "B27", "B28", "B29", "B30", "B31", "B32", "B33", "B41", "B42", "B43",   
  
  "B44", "B60", "B61", "B62", "B63", "B64", "B65", "B66", "B67", "B68", "B81",   
  
  "B82", "B99", "C01", "C02", "C03", "C04", "C05", "C06", "C07", "C08", "C09",   
  
  "C10", "C11", "C12", "C13", "C14", "C21", "C22", "C23", "C25", "C30", "C40",   
  
  "C99", "D01", "D02", "D03", "D04", "D05", "D06", "D07", "D21", "D99", "E01",   
  
  "E02", "E03", "E04", "E05", "E06", "E21", "E99", "F01", "F02", "F03", "F04",   
  
  "F15", "F16", "F17", "F21", "F22", "F23", "F24", "F25", "F26", "F27", "F28",   
  
  "F41", "F42", "F99", "G01", "G02", "G03", "G04", "G05", "G06", "G07", "G08",   
  
  "G09", "G10", "G11", "G12", "G16", "G21", "G99", "H01", "H02", "H03", "H04",   
  
  "H05", "H10", "H99")  



# Crear las matrices de comparación 

matriz_rango_1 <- crear_matriz_comparacion(df_rango_1_niveles, codigos_objetivo) 

matriz_rango_2 <- crear_matriz_comparacion(df_rango_2_niveles, codigos_objetivo) 

matriz_rango_3 <- crear_matriz_comparacion(df_rango_3_niveles, codigos_objetivo) 




#PARA OBTENER LAS PATENTES BIOTECNOLÓGICAS 

# Procesamiento de códigos específicos 

codigos_especificos <- c( 
  
  "C12M1/00", "C12M1/02", "C12M1/04", "C12M1/06", "C12M1/08", "C12M1/09", "C12M1/10", "C12M1/107", "C12M1/113", "C12M1/12", "C12M1/14", "C12M1/16", "C12M1/18", "C12M1/20", "C12M1/21", "C12M1/22", "C12M/24", "C12M1/26", "C12M1/28", "C12M1/30", "C12M1/32", "C12M1/33", "C12M1/34", "C12M1/36", "C12M1/38", "C12M1/40", "C12M1/42", "C12M3/00",  "C12M3/02", "C12M3/04", "C12M3/06", "C12M3/08", "C12M3/10",   
  
  "C12N1/00", "C12N1/02", "C12N1/04", "C12N1/06", "C12N1/08", "C12N1/10", "C12N1/11", "C12N1/12", "C12N1/13", "C12N1/14", "C12N1/15", "C12N1/15", "C12N1/16", "C12N1/18", "C12N1/19", "C12N1/20", "C12N1/21", "C12N1/22", "C12N1/24", "C12N1/26", "C12N1/28", "C12N1/30", "C12N1/32", "C12N1/34", "C12N1/36", "C12N1/38", "C12N3/00", "C12N5/00", "C12N5/02", "C12N5/04", "C12N5/07", "C12N5/071", "C12N5/073", "C12N5/0735", "C12N5/074", "C12N5/075", "C12N5/076", "C12N5/077", "C12N5/0775", "C12N5/078", "C12N5/0781", "C12N5/0783", "C12N5/0784", "C12N5/0786", "C12N5/0787", "C12N5/0789", "C12N5/079", "C12N5/0793", "C12N5/0797", "C12N5/09", "C12N5/095", "C12N5/10", "C12N5/12", "C12N5/14", "C12N5/16", "C12N5/18", "C12N5/20", "C12N5/22", "C12N5/24", "C12N5/26", "C12N5/28", "C12N7/00", "C12N7/01", "C12N7/02", "C12N7/04", "C12N7/06", "C12N7/08", "C12N9/00", "C12N9/02", "C12N9/04", "C12N9/06", "C12N9/08", "C12N9/10", "C12N9/12", "C12N9/14", "C12N9/16", "C12N9/18", "C12N9/20", "C12N9/22", "C12N9/24", "C12N9/26", "C12N9/28", "C12N9/30", "C12N9/32", "C12N9/34", "C12N9/36", "C12N9/38", "C12N9/40", "C12N9/42", "C12N9/44", "C12N9/46", "C12N9/48", "C12N9/50", "C12N9/52", "C12N9/54", "C12N9/56", "C12N/58", "C12N9/60", "C12N9/62", "C12N9/64", "C12N9/66", "C12N9/68", "C12N9/70", "C12N9/72", "C12N9/74", "C12N9/76", "C12N9/78", "C12N9/80", "C12N9/82", "C12N9/84", "C12N9/86", "C12N9/88", "C12N9/90", "C12N9/92", "C12N9/94", "C12N9/96", "C12N9/98", "C12N9/99", "C12N11/00", "C12N11/02", "C12N11/04", "C12N11/06", "C12N11/08", "C12N11/082", "C12N11/084", "C12N11/087", "C12N11/089", "C12N11/091", "C12N11/093", "C12N11/096", "C12N11/098", "C12N11/10", "C12N11/12", "C12N11/14", "C12N11/16", "C12N11/18", "C12N13/00", "C12N15/00", "C12N15/01", "C12N15/02", "C12N15/03", "C12N15/04", "C12N15/05", "C12N15/06", "C12N15/07", "C12N15/08", "C12N15/09", "C12N15/10", "C12N15/11", "C12N15/113",  "C12N15/115", "C12N15/117", "C12N15/12", "C12N15/13", "C12N15/14", "C12N15/15", "C12N15/16", "C12N15/17", "C12N15/18", "C12N15/19", "C12N15/20", "C12N15/21", "C12N15/22", "C12N15/23", "C12N15/24", "C12N15/25", "C12N15/26", "C12N15/27", "C12N15/28", "C12N15/29", "C12N15/30", "C12N15/31", "C12N15/32", "C12N15/33", "C12N15/34", "C12N15/35", "C12N15/36", "C12N15/37", "C12N15/38", "C12N15/39", "C12N15/40", "C12N15/41", "C12N15/42", "C12N15/43", "C12N15/44", "C12N15/45", "C12N15/46", "C12N15/47", "C12N15/48", "C12N15/49", "C12N15/50", "C12N15/51", "C12N15/52", "C12N15/53", "C12N15/54", "C12N15/55", "C12N15/56", "C12N15/57", "C12N15/58", "C12N15/59", "C12N15/60", "C12N15/61", "C12N15/62", "C12N15/63", "C12N15/64", "C12N15/65", "C12N15/66", "C12N15/67", "C12N15/68", "C12N15/69", "C12N15/70", "C12N15/71", "C12N15/72", "C12N15/73", "C12N15/74", "C12N15/75", "C12N15/76", "C12N15/77", "C12N15/78", "C12N15/79", "C12N15/80", "C12N15/81", "C12N15/82", "C12N15/83", "C12N15/84", "C12N15/85", "C12N15/86", "C12N15/861", "C12N15/863", "C12N15/864", "C12N15/866", "C12N15/867", "C12N15/869", "C12N15/87", "C12N15/873", "C12N15/877", "C12N15/88", "C12N15/89", "C12N15/90",   
  
  "C12P1/00", "C12P1/02", "C12P1/04", "C12P1/06", "C12P3/00", "C12P5/00", "C12P5/02", "C12P7/00", "C12P7/02", "C12P7/04", "C12P7/06", "C12P7/08", "C12P7/10", "C12P7/12", "C12P7/14", "C12P7/16", "C12P7/18", "C12P7/20", "C12P7/22", "C12P7/24", "C12P7/26", "C12P7/28", "C12P2/30", "C12P7/32", "C12P7/34", "C12P7/36", "C12P7/38", "C12P7/40", "C12P7/42", "C12P7/44", "C12P7/46", "C12P7/48", "C12P7/50", "C12P7/52", "C12P7/54", "C12P7/56", "C12P7/58", "C12P7/60", "C12P7/62", "C12P7/625", "C12P7/64", "C12P7/6409", "C12P7/6418", "C12P7/6428", "C12P7/6431", "C12P7/6432", "C12P7/6434", "C12P7/6436", "C12P7/6445", "C12P7/6454", "C12P7/6458", "C12P7/6463", "C12P7/6472", "C12P7/6481", "C12P7/649", "C12P7/66", "C12P9/00", "C12P11/00", "C12P13/00", "C12P13/02", "C12P13/04", "C12P13/06", "C12P13/08", "C12P13/10", "C12P13/12", "C12P13/14", "C12P13/16", "C12P13/18", "C12P13/20", "C12P13/22", "C12P13/24", "C12P15/00", "C12P17/00", "C12P17/02", "C12P17/04", "C12P17/06", "C12P17/08", "C12P17/10", "C12P17/12", "C12P17/14", "C12P17/16", "C12P17/18", "C12P19/00", "C12P19/02", "C12P19/04", "C12P19/06", "C12P19/08", "C12P19/10", "C12P19/12", "C12P19/14", "C12P19/16", "C12P19/18", "C12P19/20", "C12P19/22", "C12P19/24", "C12P19/26", "C12P19/28", "C12P19/30", "C12P19/32", "C12P19/34", "C12P19/36", "C12P19/38", "C12P19/40", "C12P19/42", "C12P19/44", "C12P19/46", "C12P19/48", "C12P19/50", "C12P19/52", "C12P19/54", "C12P19/56", "C12P19/58", "C12P19/60", "C12P19/62", "C12P19/64", "C12P21/00", "C12P21/02", "C12P21/04", "C12P21/06", "C12P21/08", "C12P23/00", "C12P25/00", "C12P27/00", "C12P29/00", "C12P31/00", "C12P33/00", "C12P33/02", "C12P33/04", "C12P33/06", "C12P33/08", "C12P33/10", "C12P33/12", "C12P33/14", "C12P33/16", "C12P33/18", "C12P33/20", "C12P35/00", "C12P35/00", "C12P35/02", "C12P35/04", "C12P35/06", "C12P35/08", "C12P37/00", "C12P37/02", "C12P37/04", "C12P37/06", "C12P39/00", "C12P41/00", "C12Q1/00", "C12Q1/02", "C12Q1/04", "C12Q1/06", "C12Q1/08", "C12Q1/10", "C12Q1/12", "C12Q1/14", "C12Q1/16", "C12Q1/18", "C12Q1/20", "C12Q1/22", "C12Q1/24", "C12Q1/25", "C12Q1/26", "C12Q1/28", "C12Q1/30", "C12Q1/32", "C12Q1/34", "C12Q1/37", "C12Q1/40", "C12Q1/42", "C12Q1/44", "C12Q1/46", "C12Q1/48", "C12Q1/50", "C12Q1/52", "C12Q1/527", "C12Q1/533", "C12Q1/54", "C12Q1/56", "C12Q1/58", "C12Q1/60", "C12Q1/61", "C12Q1/62", "C12Q1/64", "C12Q1/66", "C12Q1/68", "C12Q1/6804", "C12Q1/6806", "C12Q1/6809", "C12Q1/6811", "C12Q1/6813", "C12Q1/6816", "C12Q1/6818", "C12Q1/682", "C12Q1/6823", "C12Q1/6825", "C12Q1/6827", "C12Q1/683", "C12Q1/6832", "C12Q1/6834", "C12Q1/6837", "C12Q1/6839", "C12Q1/6841", "C12Q1/6844", "C12Q1/6848", "C12Q1/6851", "C12Q1/6853", "C12Q1/6855", "C12Q1/6858", "C12Q1/686", "C12Q1/6862", "C12Q1/6865", "C12Q1/6867", "C12Q1/6869", "C12Q1/6872", "C12Q1/6874", "C12Q1/6876", "C12Q1/6879", "C12Q1/6881", "C12Q1/6883", "C12Q1/6886", "C12Q1/6888", "C12Q1/689", "C12Q1/6893", "C12Q1/6895", "C12Q1/6897", "C12Q2/00", "A01H1/00", "A01H1/02", "A01H1/04", "A01H1/06", "A01H1/08","A01H4/00", "C07K4/00", "C07K4/02", "C07K4/04", "C07K4/06", "C07K4/08", "C07K4/10", "C07K4/12", "A01K67/00", "A01K67/02", "A01K67/027", "A01K67/0271", "A01K67/0273", "A01K67/0275", "A01K67/0276", "A01K67/0278", "A01K67/033", "A01K67/04",   
  
  "A61K38/00", "A61K38/01", "A61K38/02", "A61K38/03", "A61K38/04", "A61K38/05", "A61K38/06", "A61K38/07", "A61K38/08", "A61K38/09", "A61K38/095", "A61K38/10", "A61K38/12", "A61K38/13", "A61K38/14", "A61K38/15", "A61K38/16", "A61K38/17", "A61K38/18", "A61K38/19", "A61K38/20", "A61K38/21", "A61K38/22", "A61K38/23", "A61K38/24", "A61K38/25", "A61K38/26", "A61K38/27", "A61K38/28", "A61K38/29", "A61K38/30", "A61K38/31", "A61K38/32", "A61K38/33", "A61K38/34", "A61K38/35", "A61K38/36", "A61K38/37", "A61K38/38", "A61K38/39", "A61K38/40", "A61K38/41", "A61K38/42", "A61K38/43", "A61K38/44", "A61K38/45", "A61K38/46", "A61K38/47", "A61K38/48", "A61K38/49", "A61K38/50", "A61K38/51", "A61K38/52", "A61K38/53", "A61K38/54", "A61K38/55", "A61K38/56", "A61K38/57", "A61K38/58",   
  
  "A61K39/00", "A61K39/002", "A61K39/005", "A61K39/008", "A61K39/012", "A61K39/015", "A61K39/018", "A61K39/02", "A61K39/04", "A61K39/05", "A61K39/07", "A61K39/08", "A61K39/085", "A61K39/09", "A61K39/095", "A61K39/10", "A61K39/102", "A61K39/104", "A61K39/106", "A61K39/108", "A61K39/112", "A61K39/114", "A61K39/116", "A61K39/118", "A61K39/12", "A61K39/125", "A61K39/13", "A61K39/135", "A61K39/145", "A61K39/15", "A61K39/155", "A61K39/165", "A61K39/17", "A61K39/175", "A61K39/187", "A61K39/193", "A61K39/20", "A61K39/205", "A61K39/21", "A61K39/215", "A61K39/225", "A61K39/23", "A61K39/235", "A61K39/245", "A61K39/25", "A61K39/255", "A61K39/265", "A61K39/27", "A61K39/275", "A61K39/285", "A61K39/29", "A61K39/295", "A61K39/35", "A61K39/36", "A61K39/38", "A61K39/385", "A61K39/39", "A61K39/395", "A61K39/40", "A61K39/42", "A61K39/44",   
  
  "A61K48/00", "C07G11/00", "C07G13/00", "C07G15/00",   
  
  "C07K14/00", "C07K14/005", "C07K14/01", "C07K14/015", "C07K14/02", "C07K14/025", "C07K14/03", "C07K14/035", "C07K14/04", "C07K14/045", "C07K14/05", "C07K14/055", "C07K14/06", "C07K14/065", "C07K14/07", "C07K14/075", "C07K14/08",   
  
  "C07K16/00", "C07K16/02", "C07K16/04", "C07K16/06", "C07K16/08", "C07K16/10", "C07K16/12", "C07K16/14", "C07K16/16", "C07K16/18", "C07K16/20", "C07K16/22", "C07K16/24", "C07K16/26", "C07K16/28", "C07K16/30", "C07K16/32", "C07K16/34", "C07K16/36", "C07K16/38", "C07K16/40", "C07K16/42", "C07K16/44", "C07K16/46",   
  
  "C07K14/085", "C07K14/09", "C07K14/095", "C07K14/10", "C07K14/105", "C07K14/11", "C07K14/115", "C07K14/12", "C07K14/125", "C07K14/13", "C07K14/135", "C07K14/14", "C07K14/145", "C07K14/15", "C07K14/155", "C07K14/16", "C07K14/165",   
  
  "C07K14/17", "C07K14/175", "C07K14/18", "C07K14/185",   
  
  "C07K14/19", "C07K14/195", "C07K14/20", "C07K14/205", "C07K14/21", "C07K14/215", "C07K14/22", "C07K14/225", "C07K14/23", "C07K14/235", "C07K14/24", "C07K14/245", "C07K14/25", "C07K14/255", "C07K14/26", "C07K14/265", "C07K14/27", "C07K14/275", "C07K14/28",   
  
  "C07K14/285", "C07K14/29", "C07K14/295", "C07K14/30", "C07K14/305", "C07K14/31", "C07K14/315", "C07K14/32", "C07K14/325", "C07K14/33", "C07K14/335", "C07K14/34", "C07K14/345", "C07K14/35", "C07K14/355", "C07K14/36", "C07K14/365", "C07K14/37", "C07K14/375", "C07K14/38", "C07K14/385", "C07K14/39", "C07K14/395", "C07K14/40", "C07K14/405", "C07K14/41", "C07K14/415", "C07K14/42", "C07K14/425", "C07K14/43", "C07K14/435", "C07K14/44", "C07K14/445", "C07K14/45", "C07K14/455", "C07K14/46", "C07K14/465", "C07K14/47", "C07K14/475", "C07K14/48", "C07K14/485", "C07K14/49", "C07K14/495", "C07K14/50", "C07K14/505", "C07K14/51", "C07K14/515", "C07K14/52", "C07K14/525", "C07K14/53", "C07K14/535", "C07K14/54", "C07K14/545", "C07K14/55",   
  
  "C07K14/555", "C07K14/56", "C07K14/565", "C07K14/57", "C07K14/575", "C07K14/58", "C07K14/585", "C07K14/59", "C07K14/595", "C07K14/60", "C07K14/605", "C07K14/61", "C07K14/615", "C07K14/62", "C07K14/625", "C07K14/63", "C07K14/635", "C07K14/64", "C07K14/645", "C07K14/65", "C07K14/655", "C07K14/66", "C07K14/665", "C07K14/67", "C07K14/675", "C07K14/68", "C07K14/685", "C07K14/69", "C07K14/695", "C07K14/70", "C07K14/705", "C07K14/71", "C07K14/715", "C07K14/72", "C07K14/725", "C07K14/73",   
  
  "C07K14/735", "C07K14/74", "C07K14/745", "C07K14/75", "C07K14/755", "C07K14/76", "C07K14/765", "C07K14/77", "C07K14/775", "C07K14/78", "C07K14/785", "C07K14/79", "C07K14/795", "C07K14/80", "C07K14/805", "C07K14/81", "C07K14/815", "C07K14/82", "C07K14/825",   
  
  "C07K17/00", "C07K17/02", "C07K17/04", "C07K17/06", "C07K17/08", "C07K17/10", "C07K17/12", "C07K17/14",    
  
  "C07K19/00","C40B10/00", "C40B40/02", "C40B40/08", "C40B50/06", "C02F3/34", "A61K35/12",   
  
  "G01N33/53", "G01N33/531", "G01N33/532", "G01N33/533", "G01N33/534", "G01N33/535", "G01N33/536", "G01N33/537", "G01N33/538", "G01N33/539",   
  
  "G01N33/541", "G01N33/542", "G01N33/543", "G01N33/544", "G01N33/545", "G01N33/546", "G01N33/547", "G01N33/548", "G01N33/549",   
  
  "G01N33/551", "G01N33/552", "G01N33/553", "G01N33/554", "G01N33/555", "G01N33/556", "G01N33/557", "G01N33/558", "G01N33/559",   
  
  "G01N33/571", "G01N33/573", "G01N33/574", "G01N33/576", "G01N33/577", "G01N33/579",   
  
  "G01N33/68", "G01N33/74", "G01N33/76", "G01N33/78", "G01N33/88", "G01N33/92"   
  
  
) 


# Separar la columna 'IPC' en nuevas columnas 

df_separado <- data %>% 
  
  separate(`I P C`, into = paste0("I P C", 1:50), sep = ";", fill = "right") 



# Función para verificar si algún código de la fila coincide con los códigos específicos 

check_codigo_en_fila <- function(fila, codigos) { 
  
  codigos_limpios <- str_replace_all(fila, "[[:space:];]", "") 
  
  for (col in codigos_limpios) { 
    
    if (!is.na(col) && col %in% codigos) { 
      
      return(1) 
      
    } 
    
  } 
  
  return(0) 
  
} 



# Aplicar la función y crear la nueva columna 'Contador' 

df_separado <- df_separado %>% 
  
  rowwise() %>% 
  
  mutate(Contador = check_codigo_en_fila(c_across(starts_with("I P C")), codigos_especificos)) %>% 
  
  ungroup() 



# Filtrar las filas donde Contador es 1 

df_filtrado <- df_separado %>% filter(Contador == 1) 



# Encontrar la menor fecha 

min_date <- min(df_filtrado$Application_Date, na.rm = TRUE) 



# Formatear la menor fecha al formato 'día.mes.año' 

formatted_min_date <- format(min_date, "%d.%m.%Y") 

# Extraer solo el año de la menor fecha 

min_year <- format(min_date, "%Y") 



# Función para filtrar por rango de años y contar patentes BIOTECNOLÓGICAS 

filtrarbiotech_y_contarbiotech <- function(df_filtrado, start_year, end_year) { 
  
  data_filtrada_biotech <- df_filtrado %>% 
    
    filter(format(Application_Date, "%Y") >= start_year & format(Application_Date, "%Y") <= end_year) 
  
  
  
  conteo_por_ano <- data_filtrada_biotech %>% 
    
    group_by(ano = format(Application_Date, "%Y")) %>% 
    
    summarize(conteo = n()) 
  
  
  
  total_patentes_biotech <- nrow(data_filtrada_biotech) 
  
  rango_biotech <- paste(start_year, end_year, sep = "-") 
  
  
  
  list(df_filtrado = data_filtrada_biotech, conteo_por_ano = conteo_por_ano, total_patentes_biotech = total_patentes_biotech, rango_biotech = rango_biotech) 
  
} 



# Filtrar por los rangos especificados 

rango_1_biotech <- filtrarbiotech_y_contarbiotech(df_filtrado, 2012, 2016) 

rango_2_biotech <- filtrarbiotech_y_contarbiotech(df_filtrado, 2013, 2017) 

rango_3_biotech <- filtrarbiotech_y_contarbiotech(df_filtrado, 2014, 2018) 

rango_4_biotech <- filtrarbiotech_y_contarbiotech(df_filtrado, 2018, 2020) 

rango_5_biotech <- filtrarbiotech_y_contarbiotech(df_filtrado, 2019, 2021) 

rango_6_biotech <- filtrarbiotech_y_contarbiotech(df_filtrado, 2020, 2022) 



# Verificación de la salida de la función 

print(rango_6_biotech) 



# Crear un nuevo archivo Excel y agregar hojas con los resultados 

wb <- createWorkbook() 



agregar_hoja <- function(wb, rango, nombre_hoja) { 
  
  addWorksheet(wb, nombre_hoja) 
  
  if (!is.null(rango$data) && nrow(rango$data) > 0) { 
    
    writeData(wb, nombre_hoja, rango$data, startCol = 1, startRow = 1) 
    
    writeData(wb, nombre_hoja, paste("Total patentes en el rango", rango$rango, ":", rango$total_patentes), startCol = 1, startRow = nrow(rango$data) + 2) 
    
    if (!is.null(rango$conteo_por_ano) && nrow(rango$conteo_por_ano) > 0) { 
      
      writeData(wb, nombre_hoja, rango$conteo_por_ano, startCol = 1, startRow = nrow(rango$data) + 4) 
      
    } 
    
  } else { 
    
    writeData(wb, nombre_hoja, "No hay datos disponibles", startCol = 1, startRow = 1) 
    
  } 
  
} 



agregar_hoja_biotech <- function(wb, rango, nombre_hoja) { 
  
  addWorksheet(wb, nombre_hoja) 
  
  if (!is.null(rango$df_filtrado) && nrow(rango$df_filtrado) > 0) { 
    
    writeData(wb, nombre_hoja, rango$df_filtrado, startCol = 1, startRow = 1) 
    
    writeData(wb, nombre_hoja, paste("Total patentes en el rango", rango$rango_biotech, ":", rango$total_patentes_biotech), startCol = 1, startRow = nrow(rango$df_filtrado) + 2) 
    
    if (!is.null(rango$conteo_por_ano) && nrow(rango$conteo_por_ano) > 0) { 
      
      writeData(wb, nombre_hoja, rango$conteo_por_ano, startCol = 1, startRow = nrow(rango$df_filtrado) + 4) 
      
    } 
    
  } else { 
    
    writeData(wb, nombre_hoja, "No hay datos disponibles", startCol = 1, startRow = 1) 
    
  } 
  
} 



# Agregar hojas con los datos filtrados 

agregar_hoja(wb, rango_1, "2012-2016") 

agregar_hoja(wb, rango_2, "2013-2017") 

agregar_hoja(wb, rango_3, "2014-2018") 

agregar_hoja_biotech(wb, rango_1_biotech, "2012-2016_BIO") 

agregar_hoja_biotech(wb, rango_2_biotech, "2013-2017_BIO") 

agregar_hoja_biotech(wb, rango_3_biotech, "2014-2018_BIO") 

agregar_hoja_biotech(wb, rango_4_biotech, "2018-2020_BIO_DI") 

agregar_hoja_biotech(wb, rango_5_biotech, "2019-2021_BIO_DI") 

agregar_hoja_biotech(wb, rango_6_biotech, "2020-2022_BIO_DI") 



# Agregar una hoja con los datos del IPC 

addWorksheet(wb, "IPC_Contador") 

writeData(wb, "IPC_Contador", df_separado, startCol = 1, startRow = 1) 



# Agregar una hoja con la información de la Edad Tecnológica 

addWorksheet(wb, "Edad_Tecnologica") 

writeData(wb, "Edad_Tecnologica", df_filtrado, startCol = 1, startRow = 1) 

writeData(wb, "Edad_Tecnologica", data.frame( 
  
  "La menor fecha válida es:" = formatted_min_date, 
  
  "El año de la menor fecha válida es:" = min_year 
  
), startCol = 1, startRow = nrow(df_filtrado) + 2) 



# Añadir cada matriz como una hoja separada 

addWorksheet(wb, "Matriz_Rango_1") 

writeData(wb, "Matriz_Rango_1", matriz_rango_1) 



addWorksheet(wb, "Matriz_Rango_2") 

writeData(wb, "Matriz_Rango_2", matriz_rango_2) 



addWorksheet(wb, "Matriz_Rango_3") 

writeData(wb, "Matriz_Rango_3", matriz_rango_3) 



# Guardar el archivo Excel 

saveWorkbook(wb, "C:\\Users\\Asus\\Downloads\\EMPRESAS DE LA MUESTRA\\DATA VARIABLES\\KNU INDUSTRY COOPERATION FOUNDATION.xlsx", overwrite = TRUE)



# Función para procesar cada archivo Excel para el almacenamiento 

procesar_y_almacenar_excel <- function(ruta_excel, ruta_almacenamiento) { 
  
  # Leer el archivo Excel generado 
  
  data_matriz_1 <- read_excel(ruta_excel, sheet = "Matriz_Rango_1") 
  
  data_matriz_2 <- read_excel(ruta_excel, sheet = "Matriz_Rango_2") 
  
  data_matriz_3 <- read_excel(ruta_excel, sheet = "Matriz_Rango_3") 
  
  
  
  # Verificar que data_matriz son data frames 
  
  if (!is.data.frame(data_matriz_1) | !is.data.frame(data_matriz_2) | !is.data.frame(data_matriz_3)) { 
    
    stop("Alguno de los objetos data no es un data frame") 
    
  } 
  
  
  
  # Extraer los códigos IPC de las hojas matriz 
  
  codigos_ipc_1 <- data_matriz_1$Codigo 
  
  codigos_ipc_2 <- data_matriz_2$Codigo 
  
  codigos_ipc_3 <- data_matriz_3$Codigo 
  
  
  
  # Extraer la columna "Total" de las hojas matriz 
  
  total_1 <- data_matriz_1$Total 
  
  total_2 <- data_matriz_2$Total 
  
  total_3 <- data_matriz_3$Total 
  
  
  
  # Crear un nuevo data frame con los códigos IPC como columnas 
  
  df_resultado_1 <- tibble( 
    
    Empresa = tools::file_path_sans_ext(basename(ruta_excel)), 
    
    !!!setNames(as.list(total_1), codigos_ipc_1) 
    
  ) 
  
  
  
  df_resultado_2 <- tibble( 
    
    Empresa = tools::file_path_sans_ext(basename(ruta_excel)), 
    
    !!!setNames(as.list(total_2), codigos_ipc_2) 
    
  ) 
  
  
  
  df_resultado_3 <- tibble( 
    
    Empresa = tools::file_path_sans_ext(basename(ruta_excel)), 
    
    !!!setNames(as.list(total_3), codigos_ipc_3) 
    
  ) 
  
  
  
  # Leer el contenido actual del archivo de almacenamiento 
  
  if (file.exists(ruta_almacenamiento)) { 
    
    wb_almacenamiento <- loadWorkbook(ruta_almacenamiento) 
    
  } else { 
    
    # Si el archivo no existe, crearlo y agregar las hojas necesarias 
    
    wb_almacenamiento <- createWorkbook() 
    
    addWorksheet(wb_almacenamiento, "Almacenamiento_Rango_1") 
    
    addWorksheet(wb_almacenamiento, "Almacenamiento_Rango_2") 
    
    addWorksheet(wb_almacenamiento, "Almacenamiento_Rango_3") 
    
  } 
  
  
  
  # Escribir los datos en cada hoja (verificando si ya existen) 
  
  if ("Almacenamiento_Rango_1" %in% names(wb_almacenamiento)) { 
    
    current_data_1 <- read.xlsx(wb_almacenamiento, sheet = "Almacenamiento_Rango_1") 
    
    writeData(wb_almacenamiento, "Almacenamiento_Rango_1",  
              
              rbind(current_data_1, df_resultado_1), colNames = TRUE) 
    
  } 
  
  
  
  if ("Almacenamiento_Rango_2" %in% names(wb_almacenamiento)) { 
    
    current_data_2 <- read.xlsx(wb_almacenamiento, sheet = "Almacenamiento_Rango_2") 
    
    writeData(wb_almacenamiento, "Almacenamiento_Rango_2",  
              
              rbind(current_data_2, df_resultado_2), colNames = TRUE) 
    
  } 
  
  
  
  if ("Almacenamiento_Rango_3" %in% names(wb_almacenamiento)) { 
    
    current_data_3 <- read.xlsx(wb_almacenamiento, sheet = "Almacenamiento_Rango_3") 
    
    writeData(wb_almacenamiento, "Almacenamiento_Rango_3",  
              
              rbind(current_data_3, df_resultado_3), colNames = TRUE) 
    
  } 
  
  
  
  # Guardar el archivo Excel de almacenamiento 
  
  saveWorkbook(wb_almacenamiento, ruta_almacenamiento, overwrite = TRUE) 
  
} 

ruta <- getwd()

# Ruta del archivo Excel de almacenamiento 

ruta_almacenamiento <- "C:\\Users\\Asus\\Downloads\\EMPRESAS DE LA MUESTRA\\DATA VARIABLES\\ALMACENAMIENTO_DT.xlsx"



# Procesar y almacenar la información del archivo generado 

procesar_y_almacenar_excel("C:\\Users\\Asus\\Downloads\\EMPRESAS DE LA MUESTRA\\DATA VARIABLES\\KNU INDUSTRY COOPERATION FOUNDATION.xlsx", ruta_almacenamiento) 


# Obtener el directorio de trabajo actual 

print(getwd()) 

