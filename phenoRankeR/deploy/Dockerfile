FROM manuelrueda/pheno-ranker:0.05 as base

WORKDIR /usr/share/pheno-ranker

FROM base as builder

ENV RENV_CONFIG_PAK_ENABLED=TRUE
ENV R_VERSION=4.3.3
ENV PhenoRankeR_VERSION=0.0.0.9017
ENV PPM_URL=packagemanager.posit.co/cran/__linux__/bullseye/latest

RUN apt-get update -qq && apt-get -y --no-install-recommends install \ 
  gdebi-core \
  libfribidi-dev \
  libharfbuzz-dev \
  libjq-dev \
  libssl-dev \
  libxml2-dev \
  odbc-postgresql \
  unixodbc-dev  \
  libsqliteodbc && \
  apt-get clean

# Add user "phenoranker" (non-root)
ARG UID=1001
ARG GID=1001

RUN groupadd -g "${GID}" phenoranker \
  && useradd --create-home --no-log-init -u "${UID}" -g "${GID}" phenoranker

WORKDIR /opt
RUN curl -O https://cdn.rstudio.com/r/debian-11/pkgs/r-${R_VERSION}_1_amd64.deb
RUN gdebi -n r-${R_VERSION}_1_amd64.deb
RUN rm r-${R_VERSION}_1_amd64.deb
RUN ln -s /opt/R/${R_VERSION}/bin/R /usr/local/bin/R
RUN ln -s /opt/R/${R_VERSION}/bin/Rscript /usr/local/bin/Rscript
RUN chown -R phenoranker:phenoranker /opt/R

USER phenoranker
WORKDIR /app

RUN Rscript -e "options(repos = c(PPM = 'https://${PPM_URL}'));install.packages(c('pak','renv'))"

COPY renv.lock .
RUN Rscript -e "renv::init(bare=TRUE);options(renv.config.ppm.url = 'https://${PPM_URL}');renv::restore()"
# TODO figure out how to do a renv::restore() without installing all the development dependencies
# interesting Github issue:https://github.com/rstudio/renv/issues/1019

COPY phenoRankeR_${PhenoRankeR_VERSION}.tar.gz .
COPY config/odbc/odbcinst.ini /app/config/odbc/odbcinst.ini
COPY --chown=phenoranker:phenoranker data/ /app/data

# somhow pak gets stuck with Loading metadata database ... done
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
