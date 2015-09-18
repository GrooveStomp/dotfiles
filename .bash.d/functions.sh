function list-functions() {
    echo "catapult-auth-token"
    echo "catapult-signup"
    echo "copy-database"
    echo "refresh-soa-mysql-db"
    echo "rmq-publish"
    echo "shutdown-conflicting-bus-services"
    echo "vmware-recover-keyboard"
}

function vmware-recover-keyboard() {
    echo "WARNING! Specific to System76 Galago UltraPro with Kinesis Advantage keyboard."
    setxkbmap -model pc105 -layout us -option -option ',winkeys' -option 'caps:ctrl-modifier'
    # The following command shows current settings
    # setxkbmap -query
}

function shutdown-conflicting-bus-services() {
    sudo service mysql stop > /dev/null 2>&1
    sudo service apache2 stop > /dev/null 2>&1
    sudo service redis-server stop > /dev/null 2>&1
    sudo service rabbitmq-server stop > /dev/null 2>&1
}

function rename-terminal() {
    echo -n -e "\033]0;$1\007"
}

function copy-database() {
    FROM=$1
    TO=$2

    if [[ -z "$FROM" ]] || [[ -z "$TO" ]]; then
        echo "Usage: ${FUNCNAME[0]} source_db dest_db"
        return
    fi

    mysql -u root -h 127.0.0.1 -e "create database $TO;"
    mysqldump -u root -h 127.0.0.1 $FROM | mysql -u root -h 127.0.0.1 $TO
    mysql -u root -h 127.0.0.1 $TO -e "show tables;"
}

function rmq-publish() {
    routing_key=$1
    headers=$2
    properties=$3
    properties="${properties%?}"
    properties="$properties, \"headers\": $2 }"
    payload=$4

    if [[ -z "$1" ]] || [[ -z "$2" ]] || [[ -z "$3" ]] || [[ -z $4 ]]; then
        echo "Usage: ${FUNCNAME[0]} routing_key headers properties payload"
        echo "  eg.: ${FUNCNAME[0]} credit_application.perform '{ \"member_id\": 1, \"version\": 1 }' '{ \"correlation_id\": \"65535\", \"type\": \"get_foo\" }' '{ \"credit_application_id\": 1 }'"
        echo
        return
    fi

    sudo rabbitmqadmin publish exchange=fanout routing_key="$routing_key" properties="$properties" payload="$payload"
}

function refresh-soa-mysql-db() {
    RELEASE=$1

    if [[ -z "$1" ]]; then
        echo "Usage: ${FUNCNAME[0]} product"
        echo "  eg.: ${FUNCNAME[0]} liquid"
        return
    fi

    pushd /home/aaron/code/mogo/devops/services/soa/liquid > /dev/null 2>&1
    sudo docker-compose kill
    sudo docker rm ${RELEASE}_mysqlschema_1 > /dev/null 2>&1
    sudo docker rm ${RELEASE}_mysql_1 > /dev/null 2>&1
    # NOTE(AARON): The following commented-out block seems unnecessary and slow.
    IMAGES=$(sudo docker images | grep schema | grep $RELEASE | awk '{print $3}')
    for IMG in "${IMAGES[@]}"; do
        sudo docker rmi -f $IMG > /dev/null 2>&1
    done
    sudo docker-compose up -d
    popd > /dev/null 2>&1
}

function catapult-auth-token() {
    pushd /home/aaron/code/mogo/http_service > /dev/null 2>&1
    AUTH_TOKEN=`catapult exec login auto aarono@mogo.ca password1 | grep auth_token | awk '{print $2}'`
    echo ${AUTH_TOKEN:1:-2}
    popd > /dev/null 2>&1
}

function catapult-signup() {
    USERNAME=$1
    PASSWORD=$2

    if [[ -z "$USERNAME" ]] || [[ -z "$PASSWORD" ]]; then
        echo "Usage: ${FUNCNAME[0]} username password"
        echo "  eg.: ${FUNCNAME[0]} aarono@mogo.ca password1"
        return
    fi

    pushd /home/aaron/code/mogo/http_service > /dev/null 2>&1

    echo "confirmation: $CONFIRMATION"
    if [[ -z "$CONFIRMATION" ]]; then
        echo "Confirmation Token was blank. Username was probably already used"
        return
    fi

    CONFIRMATION=`echo ${CONFIRMATION:1:-2}`
    catapult exec activate auto $CONFIRMATION
    popd > /dev/null 2>&1
}
