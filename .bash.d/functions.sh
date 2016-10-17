#!/usr/bin/env bash
# Functions to help out with random tasks at Mogo.
#
function list-functions() {
    echo "autorun-aaron"
    echo "startup"
    echo "git-clean-branches"
    echo "git-drop-changes"
    echo "app-grep"
    echo "app-tree"
    echo "docker-build-image"
    echo "docker-run-container"
    echo "docker-debug-container"
    echo "docker-logs"
    echo "docker-remove-none-images"
    echo "docker-refresh-db"
    echo "docker-prune-images"
    echo "docker-nuclear"
    echo "dc"
    echo "vmware-recover-keyboard"
    echo "shutdown-conflicting-bus-services"
    echo "rename-terminal"
    echo "mysql-copy-database"
    echo "mysql-db"
    echo "rmq-publish"
    echo "mogo-update-soa-docs"
}

function autorun-aaron() {
    local path=$(find ~ -name 'autorun')
    local firefox=
    source "${path}/env/shell" &&
        bundle_install > /dev/null 2>&1 &&
        AUTORUN_ROOT=/home/aaron/code/mogo/docker_compose_files/soa/autorun \
        autorun --firefox /home/aaron/code/firefox/46.0.1/firefox \
                --firefox-profile 46.0.1
}

function startup() {
    # NOTE(AARON): I have systemd set up to run a custom ssh-agent script.
    # See: https://wiki.archlinux.org/index.php/SSH_keys#ssh-agent
    # cat ~/.config/systemd/user/ssh-agent.service
    # --------------------------------------------------------------------------
    # [Unit]
    # Description=SSH key agent

    # [Service]
    # Type=forking
    # Environment=SSH_AUTH_SOCK=%t/ssh-agent.socket
    # ExecStart=/usr/bin/ssh-agent -a $SSH_AUTH_SOCK

    # [Install]
    # WantedBy=default.target
    # --------------------------------------------------------------------------
    #
    # Then: systemctl --user enable ssh-agent.service # Enables it on future startups.
    # systemctl --user start ssh-agent.service # Starts it now.
    ssh-add ~/.ssh/id_rsa

    # Xbindkeys custom keyboard shortcuts
    local xbindkeys_pid=$(ps aux | grep [x]bindkeys | awk '{print $2}')
    if [[ -z $xbindkeys_pid ]]; then
        xbindkeys -f ~/.xbindkeysrc.scm
    fi

    # Kill conflicting services.
    (shutdown-conflicting-bus-services) >/dev/null 2>&1

    # Start private internet access VPN.
    # This comes from yaourt. pia-launch.
    (sudo killall pia-launch) >/dev/null 2>&1
    (sudo pia-launch &) >/dev/null 2>&1
}

function has_arg() {
    local __args=($@)
    local argc=${#__args[@]}
    local last_arg=${__args[$argc-1]}
    local args=${__args[@]:0:$argc-1}

    local arg="$last_arg"
    if [[ -z "$arg" ]]; then return; fi

    for i in $args; do
        if [[ "$arg" = "$i" ]]; then
            echo "1"
            return
        fi
    done
}

function arg_index() {
    local __args=($@)
    local argc=${#__args[@]}
    local last_arg=${__args[$argc-1]}
    local args=${__args[@]:0:$argc-1}

    local arg="$last_arg"
    if [[ -z "$arg" ]]; then return; fi

    local index=0
    for i in $args; do
        if [[ "$arg" = "$i" ]]; then
            echo "$index"
            return
        else
            ((index++))
        fi
    done
}

function arg_at_index() {
    local __args=($@)
    local argc=${#__args[@]}
    local last_arg=${__args[$argc-1]}
    local args=${__args[@]:0:$argc-1}

    local wanted_index="$last_arg"
    if [[ -z "$wanted_index" ]]; then return; fi

    local index=0
    for i in $args; do
        if [[ "$index" = "$wanted_index" ]]; then
            echo "$i"
            return
        else
            ((index++))
        fi
    done
}

function get_arg_after() {
    local __args=($@)
    local argc=${#__args[@]}
    local last_arg=${__args[$argc-1]}
    local args=${__args[@]:0:$argc-1}

    local arg="$last_arg"
    if [[ -z "$arg" ]]; then return; fi

    local index=$(arg_index "$args" "$arg")
    if [[ -z "$index" ]]; then return; fi

    ((index++))
    echo $(arg_at_index "$args" "$index")
}

function test_arg_functions() {
    local __args=($@)
    local argc=${#__args[@]}

    local last_arg=${__args[$argc-1]}
    local args=${__args[@]:0:$argc-1}

    echo "has arg: $(has_arg $args $last_arg)"
    local index=$(arg_index $args $last_arg)
    echo "arg index: $index"
    echo "arg at index: $(arg_at_index $args $index)"
    echo "get arg after: $(get_arg_after $args $last_arg)"
}

function has_help_flag() {
    local args=$@

    if [[ $(has_arg $args "-h") || $(has_arg $args "--help") ]]; then
        echo "true"
        return
    fi

}
function app-grep() {
    grep "$@" | grep -v env | grep -v spec | grep -v schema
}

function app-tree() {
    tree -I "env|spec" "$@"
}

function docker-logs() {
    docker logs -f --since `date +%Y-%m-%dT%H:%M:%S` $@
}

function docker-remove-none-images () {
    local name=${FUNCNAME[0]}
    local args=$@

    if [[ $(has_help_flag $args) ]]; then
        echo "Usage: $name"
        echo "Removes images with <none> tag"
        return
    fi

    local images=$(docker images | grep none)
    if [[ -n $images ]]; then
        docker rmi -f $(echo $images | awk '{print $3}')
    fi
}

function dc() {
    local args=$@
    local name=${FUNCNAME[0]}

    if [[ $(has_help_flag $args) ]]; then
        echo "Usage: $name parameters"
        echo "Just a wrapper function around docker-compose that uses dockerhub instead of quay.io."
        return
    fi

    local exe=$(which docker-compose)
    local root_path=$(find ~ -name 'docker_compose_files' 2>/dev/null)
    local file="${root_path}/soa/dev/docker-compose.yml"

    $exe -f $file $args
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
#    ps aux | grep [m]ysql | awk '{print $2}' | xargs sudo kill -9
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
    local name=${FUNCNAME[0]}

    if [[ $(has_help_flag $args) ]]; then
        echo "Usage: ${name} parameters"
        echo "Simple wrapper around mysql client. Connects as root on host 127.0.0.1."
        echo "All parameters are passed directly to mysql-client"
        return
    fi

    mysql -u root -h 127.0.0.1 "$@"
}

function docker-refresh-db() {
    local name=${FUNCNAME[0]}
    local args=$@

    if [[ $(has_help_flag $args) ]]; then
        echo "Usage: ${name}"
        echo "options:"
        echo -e "\t--branch: which branch to use. Defaults to \`dev'"
        return
    fi

    local branch="dev"
    if [[ $(has_arg $args "--branch") ]]; then
        branch=$(get_arg_after $args "--branch")
    fi

    docker-compose kill
    docker rm ${branch}_mysqlschema_1 > /dev/null 2>&1
    docker rm ${branch}_mysql_1 > /dev/null 2>&1

    IMAGES=$(docker images | grep schema | grep $branch | awk '{print $3}')
    for IMG in "${IMAGES[@]}"; do
        docker rmi -f $IMG > /dev/null 2>&1
    done
    docker-compose $args up -d
}

function docker-prune-images() {
    local name=${FUNCNAME[0]}
    local tag=${__args[$argc-1]}

    if [[ $(has_help_flag $args) ]]; then
        echo "Usage: $name [options] tag"
        echo -e "\ttag: Tag name, eg.: liquid_m4, dev, latest (single label only)"
        echo -e ""
        echo "Options:"
        echo -e "\t-h: This help dialog"
        return
    fi

    echo "Removing dangling images"
    images=$(docker images -q -f "dangling=true")
    if [[ "" != "$images" ]]; then
        docker rmi -f $images
    fi

    echo "Removing images tagged '<none>'"
    images=$(docker images | grep '<none>' | tr -s ' ' | cut -d ' ' -f 3)
    if [[ "" != "$images" ]]; then
        docker rmi -f $images
    fi

    echo "Removing images identified by '$tag'"
    if [[ "-h" != "$tag" ]] || [[ "" != "$tag" ]]; then
        images=$(docker images | grep "$tag" | tr -s ' ' | cut -d ' ' -f 3)
        if [[ "" != "$images" ]]; then
            docker rmi -f $images
        fi
    fi
}

function docker-nuclear() {
    dc kill
    shutdown-conflicting-bus-services
    dc rm
    docker-remove-none-images
    docker-prune-images
    service systemctl restart docker
    dc pull
    dc up -d
}

function git-clean-branches() {
    local args=$@
    local name=${FUNCNAME[0]}

    if [[ $(has_help_flag $args) ]]; then
        echo "Usage: $name [options]"
        echo "Deletes all local branches except for the branch you are currently on."
        echo
        echo "Options:"
        echo -e "\t--keep: comma-separated list of branch names to keep"
        echo -e "\t        eg.: master,dev"
        return
    fi

    local keep=
    if [[ $(has_arg $args "--keep") ]]; then
        keep=$(get_arg_after $args "--keep")
    fi

    local ignore_string="grep -v \*"
    local keep_array=
    IFS=',' read -ra keep_array <<< "$keep"
    for i in "${keep_array[@]}"; do
        ignore_string="$ignore_string | grep -v $i"
    done

    local branches=$(eval "git branch -l | $ignore_string")
    if [[ -z "$branches" ]]; then return; fi

    local command="git branch -l | $ignore_string | xargs git branch -D"
    eval $command
}

function git-drop-changes()
{
    local args=$@
    local name="${FUNCNAME[0]}"

    function usage() {
        echo "Usage: $name"
        echo
        echo "Drops all local changes"
        echo
    }

    local wants_help=$(has_help_flag $args)
    if [ ! -z "$wants_help" ]; then usage; return; fi

    local output=$(git stash save)
    if [[ "No local changes to save" != "$output" ]]; then
        git stash drop stash@{0} > /dev/null
    fi
}

function docker-build-image() {
    local args=$@

    function usage() {
        echo "Usage: ${FUNCNAME[0]} [options] image-tag"
        echo "Builds a Docker image assuming a Dockerfile in the current directory"
        echo
        echo "options are passed through to the docker command"
    }

    local wants_help=$(has_help_flag $args)
    if [ ! -z "$wants_help" ]; then usage; return; fi

    local args="${@:1:${#}-1}" # Set args to all but last argument.
    local tag="${@: -1}" # Set $tag to last argument.

    if [[ -z "$tag" ]]; then usage; return; fi

    docker build $args -t $tag .
}

function docker-run-container() {
    local args=$@

    function usage() {
        echo
        echo "Usage: ${FUNCNAME[0]} links image-tag"
        echo -e "\tlinks: comma-separated list of services to link with or --link-all or --link-none. No spaces"
        echo -e "\teg.: ${FUNCNAME[0]} --link-all local-service"
        echo
        echo " Export environment variables starting with 'MOGO_' to have this function forward them to the container."
        echo
    }

    local wants_help=$(has_help_flag $args)
    if [ ! -z "$wants_help" ]; then usage; return; fi
    if [[ -z "$1" || -z "$2" ]]; then usage; return; fi

    raw_links=$1
    tag=$2

    link_string=""
    if [[ "--link-none" == "$raw_links" ]]; then
        link_string=""
    elif [[ "--link-all" == "$raw_links" ]]; then
        link_string=" --link dev_mysql_1:mysql --link dev_bus_1:bus --link dev_redis_1:redis --link dev_httpservice_1:http"
    else
        IFS=',' read -ra links <<< "$raw_links"
        for i in "${links[@]}"; do
            link_string="$link_string --link dev_${i}_1:${i}"
        done
    fi

    pass_through_vars=""
    while read env_var; do
        tmp=$(echo $env_var | awk '{ gsub("^MOGO_", "", $1); print $1}')
        [[ -z "$tmp" ]] || pass_through_vars="$pass_through_vars -e $tmp"
    done <<< "$(env | grep '^MOGO_')"

    docker rm -f $tag > /dev/null 2>&1
    docker run -it $link_string --name $tag \
           $pass_through_vars \
           -e DB_HOST=mysql \
           -e DB_USERNAME=root \
           -e DB_PASSWORD= \
           -e MQ_HOST=bus \
           -e MQ_VHOST=/ \
           -e REDIS_HOST=redis \
           -e REDIS_PORT=6379 \
           -e REDIS_DB=0 \
           -e VERIFY_ACCOUNT=false \
           $tag
}

function docker-build-and-run() {
    if [[ "-h" == "$1" || -z "$1" || -z "$2" ]]; then
        echo
        echo "Usage: ${FUNCNAME[0]} links image-tag"
        echo -e "\tlinks: comma-separated list of services to link with or --link-all. No spaces"
        echo -e "\teg.: ${FUNCNAME[0]} --link-all local-service"
        echo
        return
    fi

    docker-build-image $2 && docker-run-container $1 $2
}

function docker-debug-container() {
    if [[ "-h" == "$1" || -z "$1" ]]; then
        echo
        echo "Usage: ${FUNCNAME[0]} container-name"
        echo -e "\teg.: ${FUNCNAME[0]} local-agreement"
        echo
        return
    fi

    local container=$1
    docker rm -f $container > /dev/null 2>&1
    docker run -it --name $container --link dev_bus_1:bus --link dev_mysql_1:mysql \
           -e DB_HOST=mysql -e DB_USERNAME=root -e DB_PASSWORD= \
           -e MQ_HOST=bus -e MQ_VHOST=/ \
           $container /bin/bash
}
