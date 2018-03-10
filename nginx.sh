#! /bin/bash

NGINX_ID=$(docker run -d \
  -p 80:80 \
  -v `pwd`/_static:/usr/share/nginx/www \
  -v `pwd`/sites-enabled:/etc/nginx/sites-enabled \
  noteed/nginx)
NGINX_IP=$(docker inspect --format '{{ .NetworkSettings.IPAddress }}' ${NGINX_ID})
echo nginx:
echo "  container: $NGINX_ID"
echo "  address: $NGINX_IP"
