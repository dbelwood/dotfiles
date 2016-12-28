#! /usr/bin/env bash

export BASH_IT="${HOME}/.bash_it"
root_dir=~/.dotfiles

# Load paths
source $root_dir/bash_include/paths
source $root_dir/bash_include/paths.local

export BASH_IT_THEME='minimal'

unset MAILCHECK

source $BASH_IT/bash_it.sh

source $root_dir/bash_include/exports
source $root_dir/bash_include/exports.local
source $root_dir/bash_include/aliases
source $root_dir/bash_include/aliases.local
source $root_dir/bash_include/functions
source $root_dir/bash_include/functions.local
source $root_dir/bash_include/init
source $root_dir/bash_include/init.local
##### ADDED by Ansible Bootstrap #####
export AWS_ACCESS_KEY_ID='AKIAJCUZDLGW3UCOKSAA'
export AWS_SECRET_ACCESS_KEY='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'
export GCE_INI_PATH='/etc/ansible/inventory/gce.ini'
export SL_USERNAME='ansible@telnyx.com'
export SL_API_KEY='20e956e03791282b31a40b8deb10912c300b131bfec918bd297a15159fa335bd'
export AZURE_CERT_PATH=~/.ssh/azu.pem
export AZURE_SUBSCRIPTION_ID=ba7e312e-8e4c-4459-aebc-3e20cbf4c8e3
export ANSIBLE_CACHE_DIR=~/.ansible/tmp/
export ANSIBLE_AZURE_CACHE_MAX_AGE=10
alias provision='ansible-playbook ~/ansible/playbooks/provision.yml'
#####
