#!/bin/bash

if [ -n "$HYDRA_URL" ]; then
    echo "HYDRA_URL is set, starting kuber-hydra..."
    exec /bin/kuber-hydra "$@"
else
    echo "HYDRA_URL is not set, starting kuber-server..."
    exec /bin/kuber-server "$@"
fi
