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
        with open(log_file, 'r') as f:
            scanned_files = set(f.read().splitlines())
    else:
        scanned_files = set()
    return scanned_files

# Function to update scanned files
def update_scanned_files(scanned_files, log_file='empresas_revisadas.txt'):
    with open(log_file, 'w') as f:
        for file in scanned_files:
            f.write(f"{file}\n")

# Function to perform the scanning logic
def scan_file(file):
    # Your actual scanning logic here
    logger.info(f"Scanned file: {file}")
    print(f"Scanned file: {file}")  # Example output, replace with your actual processing

# Main logic of your script
def main():
    scanned_files = load_scanned_files()
    files_to_scan = ['file1.txt', 'file2.txt', 'file3.txt']  # Replace with your actual list of files

    for file in files_to_scan:
        if file not in scanned_files:
            scan_file(file)
            scanned_files.add(file)
        else:
            logger.info(f"Skipped already scanned file: {file}")
            print(f"Skipped already scanned file: {file}")  # Example output, replace with your actual processing

    update_scanned_files(scanned_files)

    # Leer el archivo Excel que contiene los nombres de los archivos
    df = pd.read_excel('nombres_archivos.xlsx')

    # Ruta al script de R
    ruta_script_r = 'tu_script.R'

    # Leer el contenido del script de R
    with open(ruta_script_r, 'r') as file:
        script_r = file.readlines()

    # Línea del script de R que contiene el nombre del archivo (ajusta esto a tu script)
    linea_nombre_archivo = 2  # Por ejemplo, si es la tercera línea

    # Procesar cada archivo
    for index, row in df.iterrows():
        nombre_archivo = row['nombre_archivo']  # Ajusta esto según el nombre de la columna en tu Excel

        # Modificar la línea del nombre del archivo en el script de R
        script_r[linea_nombre_archivo] = f'archivo_entrada <- "{nombre_archivo}"\n'

        # Guardar el script de R modificado
        with open('temp_script.R', 'w') as file:
            file.writelines(script_r)

        # Ejecutar el script de R
        subprocess.run(['Rscript', 'temp_script.R'])

    # Eliminar el script temporal
    os.remove('temp_script.R')

if __name__ == "__main__":
    main()
