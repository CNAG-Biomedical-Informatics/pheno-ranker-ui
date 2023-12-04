# FAQs when dealing with ShinyProxy

# Shinyproxy does not realize that the logged in user is an admin

Double-check that the:
- admin user has the realm role `admin` assigned
- User Realm Role mapper is configured correctly
	- See: https://shinyproxy.io/faq#how-to-use-keycloak-groups-in-shinyproxy