SHELL := /bin/bash

CURRENT_DATE = $(shell date)
export IGNORE_CACHE_FROM_HERE:=$(CURRENT_DATE)

ui:
	cd shiny-app && Rscript -e "renv::activate(); shiny::runApp('src/',host='0.0.0.0',port=5050)"

db-test:
	cd shiny-app && Rscript "tests/manually/test_db_connection.R"

db-empty:
	cd shiny-app && Rscript "tests/manually/empty_db.R"

db-run:
	docker run -d --name pheno-ranker-db -p 5432:5432 -e POSTGRES_PASSWORD=shiny -e POSTGRES_USER=shiny -e POSTGRES_DB=shiny postgres:13.3-alpine

db-populate-with-timestamps:
	cd shiny-app && Rscript "tests/manually/populate_db_with_timestamps.R"

dev:
	cd phenoRankeR && Rscript dev/run_dev.R

build:
	cd phenoRankeR && Rscript dev/03_deploy.R

build-docker:
	cd phenoRankeR/deploy && docker build -f Dockerfile_pheno-ranker -t phenorankerui:latest .

d-run:
	docker run --rm -it --entrypoint sh phenorankerui:latest

stop:
	fuser -k 5050/tcp
editor:
	Rscript -e "library(shinyuieditor); shinyuieditor::launch_editor(app_loc='shiny-app/src/',host='0.0.0.0',port=5052)"
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

