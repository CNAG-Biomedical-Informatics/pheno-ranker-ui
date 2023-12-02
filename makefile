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

db-populate-with-timestamps:
	cd phenoRankeR && Rscript "tests/manually/populate_db_with_timestamps.R"

dev:
	cd phenoRankeR && Rscript dev/run_dev.R

stop:
	fuser -k 3839/tcp

test:
	cd phenoRankeR && Rscript -e "devtools::test()"

install:
	R CMD INSTALL phenoRankeR_$(phenoRankeR_VERSION).tar.gz
	cd phenoRankeR && Rscript -e "library(phenoRankeR); phenoRankeR::run_app()"

build:
	cd phenoRankeR && Rscript dev/03_deploy.R

build_package_builder_image:
	DB_IP=$(shell docker inspect -f '{{range .NetworkSettings.Networks}}{{.IPAddress}}{{end}}' pheno-ranker-db) && \
	echo $$DB_IP && \
	docker build -f Dockerfile_package_builder --build-arg DB_IP=$${DB_IP} -t phenorankerui_package_builder:latest .

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
	/usr/local/bin/mkcert -cert-file ./nginx_mountpoint/certs/shinyproxy.cnag.dev.pem -key-file ./nginx_mountpoint/certs/shinyproxy.cnag.dev-key.pem shinyproxy.cnag.dev

