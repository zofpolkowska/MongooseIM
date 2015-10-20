#!/bin/sh

DOCKER_MACHINE="default"

docker-machine start ${DOCKER_MACHINE}
eval "$(docker-machine env ${DOCKER_MACHINE})"

LDAP_PORT=1389
LDAP_HOST=`docker-machine ip ${DOCKER_MACHINE}`
LDAP_ROOTPASS="mongoose"

LDAP_CONT_NAME="mongooseim_ldap"

MIM_LDAP_CONTAINERS=`docker ps -a -s -f "name=${LDAP_CONT_NAME}" | wc -l`

if [ ${MIM_LDAP_CONTAINERS} -le 1 ]; then

docker run --name ${LDAP_CONT_NAME} \
	-p ${LDAP_PORT}:389 \
	-e LDAP_DOMAIN=esl.com \
	-e LDAP_ORGANISATION="MongooseIM" \
	-e LDAP_ROOTPASS=mongoose \
	-d nickstenning/slapd

LDIF_FILE_PATH="/tmp/init_entries.ldif"
cat > ${LDIF_FILE_PATH} << EOL
dn: ou=Users,dc=esl,dc=com
objectClass: organizationalUnit
ou: users
EOL

ldapadd -h ${LDAP_HOST} -p ${LDAP_PORT} -c -x -D cn=admin,dc=esl,dc=com -w ${LDAP_ROOTPASS} \
	-f ${LDIF_FILE_PATH}
else
	echo "Container exists, starting"
	docker start  ${LDAP_CONT_NAME}
fi

