#!/usr/bin/env bash
# Functions to help out with random tasks at Mogo.
#
function list-functions() {
    echo "vmware-recover-keyboard"
    echo "shutdown-conflicting-bus-services"
    echo "rename-terminal"
    echo "mysql-copy-database"
    echo "mysql-db"
    echo "rmq-publish"
    echo "mogo-refresh-db"
    echo "mogo-prune-docker-images"
    echo "mogo-clean-git-branches"
    echo "mogo-rgtc"
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

function mysql-copy-database() {
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

function mysql-db() {
    if [[ "-h" == "$1" ]] || [[ "--help" == "$1" ]]; then
        echo "Usage: ${FUNCNAME[0]} [db]"
        echo "  db defaults to 'soa_db'"
        return
    fi

    db=$1
    if [[ -z "$db" ]]; then
        db=soa_db
    fi

    mysql -u root -h 127.0.0.1 $db
}

function mogo-refresh-db() {
    RELEASE=$1

    if [[ -z "$1" ]]; then
        echo "Usage: ${FUNCNAME[0]} product"
        echo "  eg.: ${FUNCNAME[0]} liquid"
        echo "  eg.: ${FUNCNAME[0]} dev"
        return
    fi

    sudo docker-compose kill
    sudo docker rm ${RELEASE}_mysqlschema_1 > /dev/null 2>&1
    sudo docker rm ${RELEASE}_mysql_1 > /dev/null 2>&1
    # NOTE(AARON): The following commented-out block seems unnecessary and slow.
    IMAGES=$(sudo docker images | grep schema | grep $RELEASE | awk '{print $3}')
    for IMG in "${IMAGES[@]}"; do
        sudo docker rmi -f $IMG > /dev/null 2>&1
    done
    sudo docker-compose up -d
}

function mogo-prune-docker-images() {
    if [[ "-h" == "$1" ]]; then
        echo "Usage: ${FUNCNAME[0]} [options] tag"
        echo -e "\ttag: Tag name, eg. 'liquid_m4' (without quotes)"
        echo -e ""
        echo "Options:"
        echo -e "\t-h: This help dialog"
        return
    fi

    echo "Removing dangling images"
    images=$(sudo docker images -q -f "dangling=true")
    if [[ "" != "$images" ]]; then
        sudo docker rmi -f $images
    fi

    echo "Removing images tagged '<none>'"
    images=$(sudo docker images | grep '<none>' | tr -s ' ' | cut -d ' ' -f 3)
    if [[ "" != "$images" ]]; then
        sudo docker rmi -f $images
    fi

    TAG=$1
    echo "Removing images identified by '$TAG'"
    if [[ "-h" != "$1" ]] || [[ "" != "$1" ]]; then
        images=$(sudo docker images | grep "$TAG" | tr -s ' ' | cut -d ' ' -f 3)
        if [[ "" != "$images" ]]; then
            sudo docker rmi -f $images
        fi
    fi
}

function mogo-clean-git-branches() {
    if [[ "-h" == "$1" ]]; then
        echo "Usage: ${FUNCNAME[0]}"
        echo "Deletes all local branches not matching 'liquid_m[0-9]+$' or 'master$'"
        return
    fi

    git fetch --prune
    git checkout master
    git branch -l | grep -Ev 'liquid_m[0-9]+$' | grep -v 'master$' | xargs git branch -D
}

function mogo-rgtc() {
    if [[ "-h" == "$1" ]]; then
        echo "Usage: ${FUNCNAME[0]}"
        echo "Means: Mogo [R]evert [G]it [T]esting [C]hanges"
        echo "Reverts changes to spec/support/circle_ci/db/schema.sql"
        echo "Reverts changes to Gemfile.lock"
        return
    fi

    git checkout -- spec/support/circle_ci/db/schema.sql
    git checkout -- Gemfile.lock
}
