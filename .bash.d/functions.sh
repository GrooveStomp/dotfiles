# Functions to help out with random tasks at Mogo.
#
function list-functions() {
    echo "vmware-recover-keyboard"
    echo "shutdown-conflicting-bus-services"
    echo "rename-terminal"
    echo "mysql-copy-database"
    echo "rmq-publish"
    echo "soa-refresh-mysql-db"
    echo "soa-auth-token"
    echo "soa-member-id"
    echo "soa-signup"
    echo "soa-get-credit-app-id"
    echo "soa-run"
    echo "soa-create-liquid-loan"
    echo "soa-upload-document"
    echo "soa-verify-pay_stub"
    echo "soa-verify-photo_id"
    echo "soa-verify-bank_statement"
    echo "docker-gc"
    echo "git-clean-branches"
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

function soa-refresh-mysql-db() {
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

function soa-auth-token() {
    local last_arg="${!#}"
    local verbosity=0
    if [[ "-v" == "$last_arg" ]] || [[ "--verbose" == "$last_arg" ]]; then
        verbosity="-v"
    fi
    if [[ -z "$HOST" ]]; then
        HOST=localhost
    fi
    if [[ -z "$PORT" ]]; then
        PORT=443
    fi
    echo "${FUNCNAME[0]}, HOST=$HOST, PORT=$PORT, verbosity=$verbosity"

    local username=$1
    local password=$2

    if [[ -z "$username" ]] || [[ -z "$password" ]]; then
        echo "Usage: ${FUNCNAME[0]} username password"
        echo "  eg.: ${FUNCNAME[0]} aarono@mogo.ca password1"
        return
    fi

    local auth_token=$(HOST=$HOST PORT=$PORT catapult exec login auto $username $password | grep auth_token | awk '{print $2}')
    echo ${auth_token:1:-2}
}

function soa-member-id() {
    local last_arg="${!#}"
    local verbosity=0
    if [[ "-v" == "$last_arg" ]] || [[ "--verbose" == "$last_arg" ]]; then
        verbosity="-v"
    fi
    if [[ -z "$HOST" ]]; then
        HOST=localhost
    fi
    if [[ -z "$PORT" ]]; then
        PORT=443
    fi
    echo "${FUNCNAME[0]}, HOST=$HOST, PORT=$PORT, verbosity=$verbosity"

    local username=$1
    local password=$2

    if [[ -z "$username" ]] || [[ -z "$password" ]]; then
        echo "Usage: ${FUNCNAME[0]} username password"
        echo "  eg.: ${FUNCNAME[0]} aarono@mogo.ca password1"
        return
    fi

    local id=$(HOST=$HOST PORT=$PORT catapult exec login auto $username $password | grep member_id | awk '{print $2}')
    echo ${id%?} # Strip the trailing comma.
}

function soa-signup() {
    local last_arg="${!#}"
    local verbosity=0
    if [[ "-v" == "$last_arg" ]] || [[ "--verbose" == "$last_arg" ]]; then
        verbosity="-v"
    fi
    if [[ -z "$HOST" ]]; then
        HOST=localhost
    fi
    if [[ -z "$PORT" ]]; then
        PORT=443
    fi
    echo "${FUNCNAME[0]}, HOST=$HOST, PORT=$PORT, verbosity=$verbosity"

    local username=$1
    local password=$2

    if [[ -z "$username" ]] || [[ -z "$password" ]]; then
        echo "Usage: ${FUNCNAME[0]} username password"
        echo "  eg.: ${FUNCNAME[0]} aarono@mogo.ca password1"
        return
    fi

    local log="${FUNCNAME[0]}.log"

    catapult exec signup default $username $password > $log
    [ "-v" == "$verbosity" ] && cat $log

    local confirmation_token=$(cat $log | grep confirmation_token | awk '{print $2}')
    confirmation_token=$(echo ${confirmation_token:1:-2})
    rm -f $log

    if [[ -z "$confirmation_token" ]]; then
        "Couldn't find confirmation token"
        return
    fi

    echo "Confirmation Token: $confirmation_token"

    catapult exec activate auto $confirmation_token > $log
    rm -f $log
}

function soa-get-credit-app-id() {
    local last_arg="${!#}"
    local verbosity=0
    if [[ "-v" == "$last_arg" ]] || [[ "--verbose" == "$last_arg" ]]; then
        verbosity="-v"
    fi
    if [[ -z "$HOST" ]]; then
        HOST=localhost
    fi
    if [[ -z "$PORT" ]]; then
        PORT=443
    fi
    echo "${FUNCNAME[0]}, HOST=$HOST, PORT=$PORT, verbosity=$verbosity"

    local username=$1
    local password=$2

    if [[ -z "$username" ]] || [[ -z "$password" ]]; then
        echo "Usage: ${FUNCNAME[0]} username password"
        echo "  eg.: ${FUNCNAME[0]} aarono@mogo.ca password1"
        return
    fi

    local credit_app_id=$(HOST=$HOST PORT=$PORT catapult exec index_credit_applications auto $(HOST=$HOST PORT=$PORT soa-auth-token $username $password $verbosity) | grep '^       "id"' | awk '{print $2}')
    echo ${credit_app_id%?} # Strip the trailing comma.
}

function soa-create-liquid-loan() {
    local last_arg="${!#}"
    local verbosity=0
    if [[ "-v" == "$last_arg" ]] || [[ "--verbose" == "$last_arg" ]]; then
        verbosity="-v"
    fi
    if [[ -z "$HOST" ]]; then
        HOST=localhost
    fi
    if [[ -z "$PORT" ]]; then
        PORT=443
    fi
    echo "${FUNCNAME[0]}, HOST=$HOST, PORT=$PORT, verbosity=$verbosity"

    local username=$1
    local password=$2

    local log="${FUNCNAME[0]}.log"

    if [[ -z "$username" ]] || [[ -z "$password" ]]; then
        echo "Usage: ${FUNCNAME[0]} username password"
        echo "  eg.: ${FUNCNAME[0]} aarono@mogo.ca password1"
        return
    fi

    local credit_app_id=$(HOST=$HOST PORT=$PORT soa-get-credit-app-id $username $password $verbosity)
    if [[ -z "$credit_app_id" ]]; then
        echo "Couldn't find credit application ID"
        return
    fi

    catapult exec create_liquid_loan default $(HOST=$HOST PORT=$PORT soa-auth-token $username $password $verbosity) $credit_app_id > $log
    [ "-v" == "$verbosity" ] && cat $log
    rm -f $log
}

function soa-run() {
    local last_arg="${!#}"
    local verbosity=0
    if [[ "-v" == "$last_arg" ]] || [[ "--verbose" == "$last_arg" ]]; then
        verbosity="-v"
    fi
    if [[ -z "$HOST" ]]; then
        HOST=localhost
    fi
    if [[ -z "$PORT" ]]; then
        PORT=443
    fi
    echo "${FUNCNAME[0]}, HOST=$HOST, PORT=$PORT, verbosity=$verbosity"

    local username=$1
    local password=$2
    local tmp
    local log="${FUNCNAME[0]}.log"

    if [[ -z "$username" ]] || [[ -z "$password" ]]; then
        echo "Usage: ${FUNCNAME[0]} username password"
        echo "  eg.: ${FUNCNAME[0]} aarono@mogo.ca password1"
        return
    fi

    soa-signup $username $password $verbosity

    catapult exec create_credit_application default $(HOST=$HOST PORT=$PORT soa-auth-token $username $password $verbosity) > $log
    local credit_app_id=$(cat $log | grep '^   "id"' | awk '{print $2}')
    credit_app_id=$(echo ${credit_app_id%?}) # Strip the trailing comma.

    if [[ -z "$credit_app_id" ]]; then
        [ "-v" == "$verbosity" ] && cat $log
        rm -f log
        echo "Credit Application ID not found"
        return
    fi

    echo "Credit Application ID: $credit_app_id"

    catapult exec edit_credit_application default $(HOST=$HOST PORT=$PORT soa-auth-token $username $password $verbosity) $credit_app_id 000065535 > $log
    if [[ $(cat $log) =~ "\"errors\":" ]]; then
        [ "-v" == "$verbosity" ] && cat $log
        rm -f $log
        echo "Encountered an error editing credit application"
        return
    fi
    [ "-v" == "$verbosity" ] && cat $log

    catapult exec create_liquid_loan default $(HOST=$HOST PORT=$PORT soa-auth-token $username $password $verbosity) $credit_app_id > $log
    if [[ $(cat $log) =~ "\"errors\":" ]]; then
        [ "-v" == "$verbosity" ] && cat $log
        rm -f $log
        echo "Encountered an error creating liquid loan"
        return
    fi
    [ "-v" == "$verbosity" ] && cat $log

    catapult exec create_funding_info default $(HOST=$HOST PORT=$PORT soa-auth-token $username $password $verbosity) $credit_app_id > $log
    if [[ $(cat $log) =~ "\"errors\":" ]]; then
        [ "-v" == "$verbosity" ] && cat $log
        rm -f $log
        echo "Encountered an creating funding info"
        return
    fi
    [ "-v" == "$verbosity" ] && cat $log

    catapult exec create_employment_info default $(HOST=$HOST PORT=$PORT soa-auth-token $username $password $verbosity) $credit_app_id > $log
    if [[ $(cat $log) =~ "\"errors\":" ]]; then
        [ "-v" == "$verbosity" ] && cat $log
        rm -f $log
        echo "Encountered an creating employment info"
        return
    fi
    [ "-v" == "$verbosity" ] && cat $log

    catapult exec create_contact_number default $(HOST=$HOST PORT=$PORT soa-auth-token $username $password $verbosity) $credit_app_id > $log
    if [[ $(cat $log) =~ "\"errors\":" ]]; then
        [ "-v" == "$verbosity" ] && cat $log
        rm -f $log
        echo "Encountered an creating contact number"
        return
    fi
    [ "-v" == "$verbosity" ] && cat $log

    catapult exec create_verification_contact_number auto $(HOST=$HOST PORT=$PORT soa-auth-token $username $password $verbosity) sms $credit_app_id > $log
    [ "-v" == "$verbosity" ] && cat $log
    local verification_pin=$(cat $log | grep verification_pin | awk '{print $2}')
    verification_pin=$(echo ${verification_pin:1:-2})
    rm -f $log

    if [[ -z "$verification_pin" ]]; then
        "Couldn't find verification pin"
        return
    fi

    echo "Verification PIN: $verification_pin"

    catapult exec edit_verification_contact_number auto $(HOST=$HOST PORT=$PORT soa-auth-token $username $password $verbosity) $credit_app_id $verification_pin > $log
    [ "-v" == "$verbosity" ] && cat $log

    local pay_stub_id
    local photo_id_id
    local bank_statement_id

    #-- Perform document uploads -----------------------------------------------------------------------------
    declare -a local file_uploads=('pay_stub' 'photo_id' 'bank_statement')

    for doc_type in "${file_uploads[@]}"; do
        local doc_id=$(HOST=$HOST PORT=$PORT soa-upload-document $username $password $doc_type $verbosity | tail -n 1)
        tmp="${doc_type}_id"
        eval "$tmp=$doc_id"

        if [ "$doc_id" -ne "$doc_id" ] 2>/dev/null; then # Check if $doc_id is an integer
            "Couldn't find a valid Document ID for $doc_type"
            return
        fi
    done

    catapult exec checkout auto $(HOST=$HOST PORT=$PORT soa-auth-token $username $password $verbosity) $credit_app_id > $log
    [ "-v" == "$verbosity" ] && cat $log

    #-- Verify uploaded documents ----------------------------------------------------------------------------
    for doc_type in "${file_uploads[@]}"; do
        tmp="${doc_type}_id"
        local current_id=$(echo ${!tmp})
        local current_fn="soa-verify-${doc_type}"
        $(HOST=$HOST PORT=$PORT $current_fn $username $password $current_id $verbosity)
    done

    rm -f $log
}

function soa-upload-document() {
    local last_arg="${!#}"
    local verbosity=0
    if [[ "-v" == "$last_arg" ]] || [[ "--verbose" == "$last_arg" ]]; then
        verbosity="-v"
    fi
    if [[ -z "$HOST" ]]; then
        HOST=localhost
    fi
    if [[ -z "$PORT" ]]; then
        PORT=443
    fi
    echo "${FUNCNAME[0]}, HOST=$HOST, PORT=$PORT, verbosity=$verbosity"

    local username=$1
    local password=$2
    local doc_type=$3
    local log="${FUNCNAME[0]}.log"

    if [[ -z "$username" ]] || [[ -z "$password" ]] || [[ -z "$doc_type" ]]; then
        echo "Usage: ${FUNCNAME[0]} username password doc_type"
        echo "  eg.: ${FUNCNAME[0]} aarono@mogo.ca password1 bank_statement"
        echo "       doc_type: bank_statement or pay_stub or photo_id"
        return
    fi

    #-- Get a local file and prepare it for upload -----------------------------------------------------------
    local full_name=$(ls -f | grep -v '^\.$' | grep -v '^\..$' | head -1 | xargs basename)
    local upload_doc="${full_name%.*}.jpg"
    cp $full_name $upload_doc
    upload_doc="$(pwd)/$upload_doc"

    local credit_app_id=$(HOST=$HOST PORT=$PORT soa-get-credit-app-id $username $password $verbosity)

    curl --insecure -s -X POST \
        -H "X-Auth-Token: $(soa-auth-token $username $password)" \
        -H "Accept: application/vnd.mogo.v2" \
        -H "Content-Type: multipart/form-data" \
        -F "upload=@$upload_doc" \
        https://$HOST:$PORT/credit_applications/$credit_app_id/additional_doc/$doc_type > $log
    [ "-v" == "$verbosity" ] && cat $log

    if [[ ! ($(cat $log) =~ "polling_url") ]]; then
        echo "Something bad happened when uploading pay_stub"
        rm -f $upload_doc
        rm -f $log
        return
    fi

    local polling_url=$(cat $log | jq . | grep polling_url | awk '{print $2}')
    polling_url=$(echo ${polling_url:1:-1})
    rm -f $upload_doc

    while :; do
        curl --insecure -s -X GET \
            -H "X-Auth-Token: $(soa-auth-token $username $password)" \
            -H "Accept: application/vnd.mogo.v2" \
            -H "Content-Type: application/json" \
            https://$HOST:$PORT$polling_url > $log

        if [[ $(cat $log) =~ "polling_url" ]]; then
            [ "-v" == "$verbosity" ] && cat $log
            sleep 1
        else
            catapult exec get_additional_doc auto $(soa-auth-token $username $password) $credit_app_id $doc_type > $log
            [ "-v" == "$verbosity" ] && cat $log
            local doc_id=$(cat $log | grep '^   "id"' | awk '{print $2}')
            echo ${doc_id%?} # Strip the trailing comma.
            rm -f $log
            break
        fi
    done

    rm -f $upload_doc
    rm -f $log
}

function soa-verify-pay_stub() {
    local last_arg="${!#}"
    local verbosity=0
    if [[ "-v" == "$last_arg" ]] || [[ "--verbose" == "$last_arg" ]]; then
        verbosity="-v"
    fi
    if [[ -z "$HOST" ]]; then
        HOST=localhost
    fi
    if [[ -z "$PORT" ]]; then
        PORT=443
    fi
    echo "${FUNCNAME[0]}, HOST=$HOST, PORT=$PORT, verbosity=$verbosity"

    local username=$1
    local password=$2
    local doc_id=$3
    local log="${FUNCNAME[0]}.log"

    if [[ -z "$username" ]] || [[ -z "$password" ]] || [[ -z $doc_id ]]; then
        echo "Usage: ${FUNCNAME[0]} username password uploaded_document_id"
        echo "  eg.: ${FUNCNAME[0]} aarono@mogo.ca password1 35"
        return
    fi

    rabbitmqadmin publish exchange=fanout routing_key=verification.perform \
        properties="{\"type\": \"edit_additional_document\", \"correlation_id\": \"$(echo $RANDOM)\", \"headers\": { \"member_id\": $(soa-member-id $username $password) } }" \
        payload="{
            \"credit_application_id\": $(soa-get-credit-app-id $username $password),
            \"additional_document_id\": $doc_id,
            \"verified_by\": \"Aaron\",
            \"document_verification_questions\": [
              { \"question_id\": 1, \"answer\": true },
              { \"question_id\": 2, \"answer\": true }
            ]
        }" > $log
    [ "-v" == "$verbosity" ] && cat $log
    rm -f $log
}

function soa-verify-photo_id() {
    local last_arg="${!#}"
    local verbosity=0
    if [[ "-v" == "$last_arg" ]] || [[ "--verbose" == "$last_arg" ]]; then
        verbosity="-v"
    fi
    if [[ -z "$HOST" ]]; then
        HOST=localhost
    fi
    if [[ -z "$PORT" ]]; then
        PORT=443
    fi
    echo "${FUNCNAME[0]}, HOST=$HOST, PORT=$PORT, verbosity=$verbosity"

    local username=$1
    local password=$2
    local doc_id=$3
    local log="${FUNCNAME[0]}.log"

    if [[ -z "$username" ]] || [[ -z "$password" ]] || [[ -z $doc_id ]]; then
        echo "Usage: ${FUNCNAME[0]} username password uploaded_document_id"
        echo "  eg.: ${FUNCNAME[0]} aarono@mogo.ca password1 35"
        return
    fi

    rabbitmqadmin publish exchange=fanout routing_key=verification.perform \
        properties="{\"type\": \"edit_additional_document\", \"correlation_id\": \"$(echo $RANDOM)\", \"headers\": { \"member_id\": $(soa-member-id $username $password) } }" \
        payload="{
            \"credit_application_id\": $(soa-get-credit-app-id $username $password),
            \"additional_document_id\": $doc_id,
            \"verified_by\": \"Aaron\",
            \"document_verification_questions\": [
              { \"question_id\": 3, \"answer\": true },
              { \"question_id\": 4, \"answer\": true },
              { \"question_id\": 5, \"answer\": true },
              { \"question_id\": 6, \"answer\": true }
            ]
        }" > $log
    [ "-v" == "$verbosity" ] && cat $log
    rm -f $log
}

function soa-verify-bank_statement() {
    local last_arg="${!#}"
    local verbosity=0
    if [[ "-v" == "$last_arg" ]] || [[ "--verbose" == "$last_arg" ]]; then
        verbosity="-v"
    fi
    if [[ -z "$HOST" ]]; then
        HOST=localhost
    fi
    if [[ -z "$PORT" ]]; then
        PORT=443
    fi
    echo "${FUNCNAME[0]}, HOST=$HOST, PORT=$PORT, verbosity=$verbosity"

    local username=$1
    local password=$2
    local doc_id=$3
    local log="${FUNCNAME[0]}.log"

    echo "${FUNCNAME[0]} $username $password $doc_id"

    if [[ -z "$username" ]] || [[ -z "$password" ]] || [[ -z $doc_id ]]; then
        echo "Usage: ${FUNCNAME[0]} username password uploaded_document_id"
        echo "  eg.: ${FUNCNAME[0]} aarono@mogo.ca password1 35"
        return
    fi

    rabbitmqadmin publish exchange=fanout routing_key=verification.perform \
        properties="{\"type\": \"edit_additional_document\", \"correlation_id\": \"$(echo $RANDOM)\", \"headers\": { \"member_id\": $(soa-member-id $username $password) } }" \
        payload="{
            \"credit_application_id\": $(soa-get-credit-app-id $username $password),
            \"additional_document_id\": $doc_id,
            \"verified_by\": \"Aaron\",
            \"document_verification_questions\": [
              { \"question_id\": 10, \"answer\": true },
              { \"question_id\": 11, \"answer\": true },
              { \"question_id\": 12, \"answer\": true },
              { \"question_id\": 13, \"answer\": true }
            ]
        }" > $log
    [ "-v" == "$verbosity" ] && cat $log
    rm -f $log
}

function docker-gc() {
    if [[ "-h" == "$1" ]]; then
        echo "Usage: ${FUNCNAME[0]} options"
        echo "Options:"
        echo -e "\t-h: This help dialog"
        echo -e "\ttag: Tag name, eg. 'liquid_m4' (without quotes)"
        return
    fi

    sudo docker rmi -f $(sudo docker images -q -f "dangling-true") > /dev/null 2>&1
    sudo docker rmi -f $(sudo docker images | grep '<none>' | tr -s ' ' | cut -d ' ' -f 3) > /dev/null 2>&1

    TAG=$1
    if [[ "-h" != "$1" ]] || [[ "" != "$1" ]]; then
        sudo docker rmi $(sudo docker images | grep '$TAG' | tr -s ' ' | cut -d ' ' -f 3) > /dev/null 2>&1
    fi
}

function git-clean-branches() {
    if [[ "-h" == "$1" ]]; then
        echo "Usage: ${FUNCNAME[0]}"
        echo "Deletes all local branches not matching 'liquid_m[0-9]+$' or 'master$'"
        return
    fi

    git fetch --prune
    git checkout master
    git branch -l | grep -Ev 'liquid_m[0-9]+$' | grep -v 'master$' | xargs git branch -D
}
