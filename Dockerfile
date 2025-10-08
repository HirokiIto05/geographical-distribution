FROM rocker/rstudio:4.3.3

RUN apt update && apt install -y 

# R Package
RUN apt update && \
    apt install -y  mecab mecab-ipadic-utf8 fonts-ipafont libmecab-dev && \
    apt install -y  libcurl4-openssl-dev libssl-dev libxml2-dev libfontconfig1-dev

RUN R -e "install.packages(c('renv', 'languageserver', 'httpgd'))"

# Package Cahce & Permission
RUN cd /home/rstudio && mkdir .cache && \
    chown rstudio:rstudio .cache