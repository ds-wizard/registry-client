#!/bin/sh

# create config
config=/usr/share/nginx/html/config.js
echo -n "window.dsw={apiUrl:'"$API_URL"'};" > ${config}

nginx -g 'daemon off;'
