FROM rocker/rstudio:3.6.1

# Install R packages (uncomment next line if needed)
# RUN R -e "install.packages(c('jsonlite', 'gdata', 'NCmisc', 'stringr'))"

# Copy contents of MyDocker folder to project folder in container 
COPY --chown=rstudio:rstudio . /home/rstudio/
