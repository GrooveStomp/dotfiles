#!/bin/bash

ruby-ctags() {
    ctags --verbose -e -R --fields="+afikKlmnsSzt"
}

# This is a version I thought was good...
# I think it can safely be replaced by the above.
ruby-ctags--() {
    local ruby_version=$(ruby -v | awk '{print $2}' | sed 's/p[0-9]\+//')

    ctags -f TAGS --extra=-f \
        --languages=-javascript \
        --exclude=.git \
        --exclude=log \
        --exclude=coverage \
        --exclude=devops \
        --exclude=features \
        --exclude=maintenance \
        --exclude=tmp \
        --exclude=bin \
        --exclude=doc \
        -e -R . ~/.gem/ruby/$ruby_version
}
