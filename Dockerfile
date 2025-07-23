# Usar una imagen de R con Shiny preinstalado
FROM rocker/shiny:latest

# Instalar paquetes adicionales que tu app necesite
RUN R -e "install.packages(c('shiny', 'here','tidyverse', 'openxlsx', 'readr','dplyr', 'stringr','tidyr', 'purrr', 'rsconnect', 'here', 'shiny', 'plotly','DT','stringr','shinythemes','shinycssloaders','pacman'))"

# Copiar los archivos de la app al contenedor
COPY . /srv/shiny-server/
  
  # Exponer el puerto que usa Shiny
  EXPOSE 3838

# Comando para ejecutar la app
CMD ["/usr/bin/shiny-server"]