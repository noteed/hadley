server {
  listen  80;
  server_name hadley.noteed.com;
  root /usr/share/nginx/www;
  include /etc/nginx/mime.types;
  types {
    text/plain cabal;
    text/plain md;
    text/plain hs;
  }

  location /hlint/ {
    types {
      text/html hs;
    }
  }
}
