FROM nginx:stable-alpine

COPY nginx.http.conf /etc/nginx/templates/nginx.http.conf
COPY nginx.tls.conf /etc/nginx/templates/nginx.tls.conf
COPY nginx.routes.conf /etc/nginx/includes/nginx.routes.inc
COPY docker-entrypoint.sh /docker-entrypoint.sh

RUN chmod +x /docker-entrypoint.sh

EXPOSE 80 443

ENTRYPOINT ["/docker-entrypoint.sh"]
CMD ["nginx", "-g", "daemon off;"]
