## General pre-requisites for deployment:

Ideally a unix (GNU/Linux, MacOS) based distribution.
Windows with [Docker Desktop](https://docs.docker.com/desktop/install/windows-install) installed should work as well - not tested

## Prepare the environment

The following steps are only for unix based systems.
It is tested on Ubuntu 22.04 LTS and CentOS Linux 8.

### Step 0: Clone the repository

### Step 1: Setting up a local domain

Add the following line to your `/etc/hosts` file:

```
127.0.0.1       <yourDomain>
```

### Step 2: SSL certificate for your domain

Get a certificate for your domain and place it in the folder `nginx_mountpoint/certs/` with the name `<yourDomain>.pem` and the key with the name `<yourDomain-key>.pem`.
you could generate a locally-trusted development certificate with e.g. [mkcert](https://github.com/FiloSottile/mkcert)

Command to generate a certificate using `mkcert`:

```shell
mkcert -install
mkcert -cert-file ./nginx_mountpoint/certs/<yourDomain>.pem -key-file ./nginx_mountpoint/certs/<yourDomain>-key.pem <yourDomain>
```

### Step 3: Configure nginx

Modify the file `nginx_mountpoint/templates` and replace the domain names in the following lines:

```
ssl_certificate /etc/nginx/shinyproxy.local.dev.pem;
ssl_certificate_key /etc/nginx/shinyproxy.local.dev-key.pem;
```

with your domain name.

## Everything containerized

1. Install [Docker](https://docs.docker.com/get-docker/) and [Docker compose](https://docs.docker.com/compose/install/)
2. rename the file [example.env](https://github.com/CNAG-Biomedical-Informatics/pheno-ranker-ui/blob/main/example.env) to .env and fill in the variables
3. Change the owner of the folder "data": `sudo chown -R 1000:1000 data`
4. Run `docker-compose up -d`
5. Open your browser and go to `https://<yourDomain>/auth` to access the Keycloak admin console
6. Create an admin and a test user in the realm you defined in the .env file
    > **Important:** 
    >
    > all users must have an email otherwise the login will fail
7. Assign the realm role `admin` to the user admin and `user` to your users

## Only Pheno-Ranker-UI containerized

### General Pre-requisites:

- Install and run a PostgreSQL database server (>= 9.5)

- Install and configure: 
	- [Keycloak](https://www.keycloak.org/)
  - [ShinyProxy](https://www.shinyproxy.io/)

### ShinyProxy configuration :
- Edit the file `shinyproxy/application.yml` and replace the environment variables:
  - KC_INTERNAL_URL
  - KC_REALM
  - KC_CLIENT_ID
  - KC_CLIENT_SECRET

### System requirements:
- Unix based distribution (it is tested on Ubuntu 22.04 LTS and CentOS Linux 8)
