SHELL := /bin/bash

# the env file is needed for testing
ifneq (,$(wildcard ./.env))
    include .env
    export
endif

phenoRankeR_VERSION := $(shell grep -oP 'Version: \K.*' phenoRankeR/DESCRIPTION)
CURRENT_DATE = $(shell date)

export IGNORE_CACHE_FROM_HERE:=$(CURRENT_DATE)

db-test:
	cd phenoRankeR && Rscript "tests/manually/test_db_connection.R"

db-empty:
	cd phenoRankeR && Rscript "tests/manually/empty_db.R"

db-run:
	docker run -d --name pheno-ranker-db -p 5432:5432 -e POSTGRES_PASSWORD=shiny -e POSTGRES_USER=shiny -e POSTGRES_DB=shiny postgres:13.3-alpine

sqlite-run:
	@echo "Creating SQLite database..."
	echo "CREATE TABLE init (id INTEGER PRIMARY KEY);" | sqlite3 shiny.sqlite
	@echo "Done."

db-populate-with-timestamps:
	cd phenoRankeR && Rscript "tests/manually/populate_db_with_timestamps.R"

dev:
	cd phenoRankeR && Rscript dev/run_dev.R

stop:
	fuser -k 3839/tcp

test:
	cd phenoRankeR && Rscript -e "devtools::test()"

install:
	cd phenoRankeR && \
	R CMD INSTALL deploy/phenoRankeR_$(phenoRankeR_VERSION).tar.gz && \
	Rscript -e "library(phenoRankeR);options(shiny.port = 3840);phenoRankeR::run_app()"

build:
	cd phenoRankeR && Rscript dev/03_deploy.R

build_package_builder_image:
	docker build -f Dockerfile_package_builder -t phenorankerui_package_builder:latest .

build_phenoRankeR_with_docker:
	bash build/build_phenoRankeR_w_docker.sh

build-docker:
	cd phenoRankeR/deploy && docker build -f Dockerfile -t phenorankerui:$(phenoRankeR_VERSION) .

d-run:
	docker run --rm phenorankerui:$(phenoRankeR_VERSION)

run:
	docker compose up

b-shiny:
	docker compose up --build shiny

db-ip:
	docker inspect -f '{{range .NetworkSettings.Networks}}{{.IPAddress}}{{end}}' pheno-ranker-db

kc-db-ip:
	docker inspect -f '{{range .NetworkSettings.Networks}}{{.IPAddress}}{{end}}' kc-db

proxy:
	docker compose up shinyproxy

cert:
	/usr/local/bin/mkcert -cert-file ./nginx_mountpoint/certs/${DOMAIN}.pem -key-file ./nginx_mountpoint/certs/${DOMAIN}-key.pem ${DOMAIN}

prod:
	docker compose -f docker-compose.prod.yml up

restart-shinyproxy:
	docker compose -f docker-compose.prod.yml down shinyproxy
	docker compose -f docker-compose.prod.yml up -d shinyproxy

# below is needed so that the user in the docker container
# can write to the mounted volume data
change-owner:
	sudo chown -R ${UID}:${GID} data