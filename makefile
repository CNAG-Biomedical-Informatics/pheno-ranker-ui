SHELL := /bin/bash

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

build:
	cd phenoRankeR && Rscript dev/03_deploy.R

build-docker:
	cd phenoRankeR/deploy && docker build -f Dockerfile_pheno-ranker -t phenorankerui:latest .

d-run:
	docker run --rm -it --entrypoint sh phenorankerui:latest

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

