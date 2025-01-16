#!/bin/bash
# load-env.sh

# Verifica si el archivo .env existe
if [ ! -f .env ]; then
    echo "Error: El archivo .env no existe en el directorio actual."
    exit 1
fi

# Lee y exporta las variables de entorno, manejando la última línea sin nueva línea
while IFS='=' read -r key value || [ -n "$key" ]; do
    # Elimina espacios y comillas
    value=$(echo "$value" | sed -e 's/^[[:space:]]*//' \
                               -e 's/[[:space:]]*$//' \
                               -e 's/^"//' \
                               -e 's/"$//')
    export "$key=$value"
    echo "Variable exportada: $key=$value"
done < .env

# Verifica si NEO4J_PASSWORD está establecido
if [ -z "$NEO4J_PASSWORD" ]; then
    echo "Error: NEO4J_PASSWORD no está definido."
    exit 1
fi

# Ejecuta el proyecto
cabal run vinc-chatbot