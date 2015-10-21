#!/bin/sh

DOCKER_MACHINE="default"
docker-machine start ${DOCKER_MACHINE}
eval "$(docker-machine env ${DOCKER_MACHINE})"

cp "../../../apps/ejabberd/priv/mysql.sql" schema.sql
docker-compose up -d
