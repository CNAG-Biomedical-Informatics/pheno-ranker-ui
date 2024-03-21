FROM manuelrueda/pheno-ranker:0.05 as base

WORKDIR /usr/share/pheno-ranker

FROM base as builder

ENV RENV_CONFIG_PAK_ENABLED=TRUE
ENV R_VERSION=4.3.3
ENV PhenoRankeR_VERSION=0.0.0.9007
ENV PPM_URL=packagemanager.posit.co/cran/__linux__/bullseye/latest

# COPY ./shiny-app/renv.lock /app/
RUN apt-get update -qq && apt-get -y --no-install-recommends install \ 
  gdebi-core \
  libfribidi-dev \
  libharfbuzz-dev \
  libjq-dev \
  libssl-dev \
  libxml2-dev \
  #for PostgreSQL
  unixodbc-dev \
  odbc-postgresql && \
  apt-get clean

WORKDIR /opt
RUN curl -O https://cdn.rstudio.com/r/debian-11/pkgs/r-${R_VERSION}_1_amd64.deb
RUN gdebi -n r-${R_VERSION}_1_amd64.deb
RUN rm r-${R_VERSION}_1_amd64.deb
RUN ln -s /opt/R/${R_VERSION}/bin/R /usr/local/bin/R
RUN ln -s /opt/R/${R_VERSION}/bin/Rscript /usr/local/bin/Rscript
RUN chown -R dockeruser:dockeruser /opt/R

USER dockeruser
WORKDIR /app

RUN Rscript -e "options(repos = c(PPM = 'https://${PPM_URL}'));install.packages(c('pak','renv'))"

COPY renv.lock .
RUN Rscript -e "renv::init(bare=TRUE);options(renv.config.ppm.url = 'https://${PPM_URL}');renv::restore()"
# TODO figure out how to do a renv::restore() without installing all the development dependencies
# interesting Github issue:https://github.com/rstudio/renv/issues/1019

COPY phenoRankeR_${PhenoRankeR_VERSION}.tar.gz .
# somhow pack gets stuck with Loading metadata database ... done
ENV RENV_CONFIG_PAK_ENABLED=FALSE
RUN Rscript -e "renv::install('/app/phenoRankeR_${PhenoRankeR_VERSION}.tar.gz')"
ARG IGNORE_CACHE_FROM_HERE=unknown
RUN echo "IGNORE_CACHE_FROM_HERE=${IGNORE_CACHE_FROM_HERE}"
COPY run_phenoRankeR.R . 
COPY data/examples/example.csv inst/extdata/examples/example.csv

FROM builder as deploy
EXPOSE 3838

# CMD ["sh"]

CMD ["Rscript","run_phenoRankeR.R"]
