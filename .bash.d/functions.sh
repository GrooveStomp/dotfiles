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
    echo "mogo-update-soa-docs"
}

function mogo-update-soa-docs() {
    if [[ "-h" == "$1" ]] || [[ "--help" == "$1" ]];  then
        echo "Usage: ${FUNCNAME[0]} [options]"
        echo "Options:"
        echo -e "\t-v|--verbose: Verbose output"
        echo -e "\t-h|--help: This help dialog"
        return
    fi

    local silence='> /dev/null 2>&1'
    local cmd=''
    local verbose=false
    if [[ "-v" == "$1" ]] || [[ "--verbose" == "$1" ]]; then
        verbose=true
    fi

    local date="$(date +%Y-%m-%d)"

    cmd='pushd ~/code/mogo/soa_docs'
    $verbose && echo $cmd
    eval "$cmd $silence"

      local soa_docs_update_branch="$date-update"
      local soa_docs_current_branch="$(git rev-parse --abbrev-ref HEAD)"

      $verbose && echo "git stash save && git checkout dev && git pull origin dev && git checkout -b $soa_docs_update_branch"
      git stash save > /dev/null 2>&1 && \
          git checkout dev > /dev/null 2>&1 && \
          git pull origin dev > /dev/null 2>&1 && \
          git checkout -b $soa_docs_update_branch > /dev/null 2>&1

      cmd='pushd ~/code/mogo/docker_compose_files'
      $verbose && echo $cmd
      eval "$cmd $silence"

        local devops_current_branch="$(git rev-parse --abbrev-ref HEAD)"
        $verbose && echo "git stash save && git checkout master && git pull origin master"
        git stash save > /dev/null 2>&1 && \
            git checkout master > /dev/null 2>&1 && \
            git pull origin master > /dev/null 2>&1

        cmd='pushd ~/code/mogo/docker_compose_files/soa/dev > /dev/null 2>&1'
        $verbose && echo $cmd
        eval "$cmd $silence"

          $verbose && echo "sudo docker-compose kill && sudo docker-compose pull && sudo docker-compose up -d"
          sudo docker-compose kill > /dev/null 2>&1 && \
              sudo docker-compose pull > /dev/null 2>&1 && \
              sudo docker-compose up -d > /dev/null 2>&1

          cmd='pushd ~/code/mogo/docker_compose_files/soa/scripts > /dev/null 2>&1'
          $verbose && echo $cmd
          eval "$cmd $silence"

            sleep 15
            echo -n "Press any key to continue evaluating Autorun: "
            read -n 1 -s
            echo
            cmd='./autorun.rb --mysql-host-ip 127.0.0.1 --generate-docs'
            $verbose && echo $cmd
            eval $cmd

            cmd='cp -f {rmq,http}_documentation.md ~/code/mogo/soa_docs/'
            $verbose && echo $cmd
            eval "$cmd $silence"

          cmd='popd'
          $verbose && echo $cmd
          eval "$cmd $silence" # ~/code/mogo/docker_compose_files/soa/dev

          cmd='sudo docker-compose kill > /dev/null 2>&1'
          $verbose && echo $cmd
          eval "$cmd $silence"


        cmd='popd'
        $verbose && echo $cmd
        eval "$cmd $silence" # ~/code/mogo/docker_compose_files

        cmd="git checkout $devops_current_branch"
        $verbose && echo $cmd
        eval "$cmd $silence"

        cmd='pushd ~/code/mogo/docker_compose_files/soa/dev'
        $verbose && echo $cmd
        eval "$cmd $silence"

          $verbose && echo 'sudo docker-compose kill && sudo docker-compose pull'
          sudo docker-compose kill > /dev/null 2>&1 && \
              sudo docker-compose pull > /dev/null 2>&1

        cmd='popd'
        $verbose && echo $cmd
        eval "$cmd $silence" # ~/code/mogo/docker_compose_files

      cmd='popd'
      $verbose && echo $cmd
      eval "$cmd $silence" # ~/code/mogo/soa_docs

      $verbose && echo "git add . && git commit -m \"$date documentation update\" && git push origin $soa_docs_update_branch && git checkout $soa_docs_current_branch"
      git add . > /dev/null 2>&1 && \
          git commit -m "$date documentation update" > /dev/null 2>&1 && \
          git push origin $soa_docs_update_branch > /dev/null 2>&1 && \
          git checkout $soa_docs_current_branch > /dev/null 2>&1

    cmd='popd'
    $verbose && echo $cmd
    eval "$cmd $silence" # starting directory
}

# /home/aaron/.rubies/ruby-2.2.2/lib/ruby/2.2.0/json/common.rb:155:in `parse': 757: unexpected token at '<html><body><h1>503 Service Unavailable</h1> (JSON::ParserError)
# No server is available to handle this request.
# </body></html>'
#         from /home/aaron/.rubies/ruby-2.2.2/lib/ruby/2.2.0/json/common.rb:155:in `parse'
#         from ./autorun.rb:113:in `post'
#         from ./autorun.rb:180:in `signup'
#         from ./autorun.rb:491:in `block in <main>'
#         from ./autorun.rb:415:in `with_timer'
#         from ./autorun.rb:482:in `<main>'

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
