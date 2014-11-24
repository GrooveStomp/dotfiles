#!/bin/bash

fs-commands() {
    echo "fs-reset-db fs-grep"
}

fs-reset-db-usage() {
    echo "fs-reset-db: drop and rebuild dev and test DBs for FS."
    echo "usage:"
    echo "  fs-reset-db-usage help|info"
    echo "  -> shows this help dialog"
    echo "  -> if blank, just run command."
}

fs-reset-db() {
    local arg=$1

    if [ "$arg" == "help" ]; then
        fs-reset-db-usage
        return
    elif [ "$arg" == "info" ]; then
        fs-reset-db-usage
        return
    fi

    rake db:drop db:create db:migrate db:seed
    rake RAILS_ENV=test db:drop db:create db:migrate db:seed
}

fs-grep-usage() {
    echo "fs-grep: grep in FS project with expected exclusions."
    echo "usage:"
    echo "  fs-grep help"
    echo "  -> shows this help dialog"
    echo "  fs-grep \"search-term\" flavor"
    echo "  -> flavor is one of: (ruby, with-templates)"
    echo "     if blank, ruby is assumed"
}

# my-grep search_term flavor
#
# search_term: String to search for
#              Alternatively, specify "help" for documentation.
#
# flavor: blank, "ruby" or "with-templates"
#         blank is the same as "ruby"
#
#
fs-grep() {
    local search_term=$1
    local flavor=$2

    if [ -z "$search_term" ]; then
        search_term="help"
    fi

    if [ "$search_term" == "help" ]; then
        fs-grep-usage
        return
    fi

    if [ -z "$flavor" ]; then
        flavor="ruby"
    fi

    case $flavor in
        "ruby")
            grep -r "$search_term" * | grep -v "\#\w+\#:" | grep -v "\.json" | grep -v "\.js" | grep -v "\.log" | grep -v devops/ | grep -v tmp/ | grep -v coverage/ | grep -v "\.slim" | grep -v "\.csv"
            ;;
        "with-templates")
            grep -r "$search_term" * | grep -v "\#\w+\#:" | grep -v "\.json" | grep -v "\.js" | grep -v "\.log" | grep -v devops/ | grep -v tmp/ | grep -v coverage/ | grep -v "\.csv"
            ;;
    esac
}
