import pandas as pd
import subprocess
import logging
import os

# Configure logging
logging.basicConfig(filename='scan_log.log', level=logging.INFO, format='%(asctime)s - %(message)s')
logger = logging.getLogger()

# Function to load scanned files
def load_scanned_files(log_file='empresas_revisadas.txt'):
    if os.path.exists(log_file):
        with open(log_file, 'r', encoding='utf-8') as f:
            scanned_files = set(f.read().splitlines())
    else:
        scanned_files = set()
    return scanned_files

# Function to update scanned files
def update_scanned_files(scanned_files, log_file='empresas_revisadas.txt'):
    with open(log_file, 'w', encoding='utf-8') as f:
        for file in scanned_files:
            f.write(f"{file}\n")

# Function to perform the scanning logic
def scan_file(file):
    # Ruta al script de R
    ruta_script_r = 'CODIGO.R'

    # Leer el contenido del script de R
    with open(ruta_script_r, 'r', encoding='utf-8') as rscript:
        script_r = rscript.readlines()

    # Líneas del script de R que contienen el nombre del archivo
    ruta_archivo_escanear = 31
    ruta_guardar_archivo = 598
    ruta_procesar_archivo = 754

    # Ruta base con nombre del archivo al final
    base_dir_escanear = f"COLOCAR-PATH\\EMPRESAS DE LA MUESTRA\\{file}.xlsx"
    base_dir_guardar = f"COLOCAR-PATH\\EMPRESAS DE LA MUESTRA\\DATA VARIABLES\\{file}.xlsx"

    # Combinar ruta base y nombre de archivo
    full_path_escanear = base_dir_escanear.replace("\\", "\\\\")
    full_path_guardar = base_dir_guardar.replace("\\", "\\\\")

    # Modificar las líneas del nombre del archivo en el script de R
    script_r[ruta_archivo_escanear] = f'ruta_excel <-"{full_path_escanear}"\n'
    script_r[ruta_guardar_archivo] = f'saveWorkbook(wb, "{full_path_guardar}", overwrite = TRUE)\n'
    script_r[ruta_procesar_archivo] = f'procesar_y_almacenar_excel("{full_path_guardar}", ruta_almacenamiento)\n'

    # Guardar el script de R modificado
    with open('temp_script.R', 'w', encoding='utf-8') as rscript:
        rscript.writelines(script_r)

    # Ejecutar el script de R
    try:
        subprocess.run(['Rscript', 'temp_script.R'], check=True)
    except subprocess.CalledProcessError as e:
        logger.error(f"Error executing R script for file {file}: {e}")
        print(f"Error al ejecutar el script de R para el archivo {file}: {e}")
        return False

    # Eliminar el script temporal
    os.remove('temp_script.R')

    logger.info(f"Scanned file: {file}")
    print(f"Se ha seleccionado la empresa: {file}") 
    return True

# Main logic of your script
def main():
    scanned_files = load_scanned_files()

    # Leer archivos a escanear desde la lista de Excel
    try:
        # Leer todas las hojas
        xls = pd.ExcelFile('matriz.xlsx')
        # Seleccionar la última hoja
        df = pd.read_excel(xls, sheet_name=xls.sheet_names[-1])
    except Exception as e:
        logger.error(f"Error reading Excel file: {e}")
        return
    
    files_to_scan = df['NOMBRE'].tolist()

    for file in files_to_scan:
        full_path = f"COLOCAR-PATH\\EMPRESAS DE LA MUESTRA\\{file}.xlsx"
        if file not in scanned_files:
            if os.path.exists(full_path):               
                if scan_file(file):
                    scanned_files.add(file)
            else:
                logger.warning(f"File not found: {file}")
                print(f"Empresa no encontrada: {file}")
        else:
            logger.info(f"Skipped already scanned file: {file}")
            print(f"Ya se escaneó: {file}")

    update_scanned_files(scanned_files)

if __name__ == "__main__":
    main()
