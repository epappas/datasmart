#!/bin/bash

# Example
# ./bootstrap ENVIRONMENT SSH_KEY

# ------------------------------------------
# Enviroment Values
# ------------------------------------------
# $1                # ENVIRONMENT Name,     Set by User shell
# $2                # SSH_KEY,              Set by User shell

# ------------------------------------------
# SET Service Values
# ------------------------------------------
: ${NODE_ENV=${1}}
: ${SSH_KEY=${10}}

# ------------------------------------------
# Export
# ------------------------------------------
export NODE_ENV;                echo "export NODE_ENV="${NODE_ENV} >> /etc/bashrc

source /etc/bashrc

# ------------------------------------------
# Vital dependencies
# ------------------------------------------
yum install -y gcc gcc-c++ make
yum install -y openssl
yum install -y curl-devel expat-devel gettext-devel openssl-devel zlib-devel gcc perl-ExtUtils-MakeMaker
yum groupinstall -y 'Development Tools'
yum remove -y git

# ------------------------------------------
# SSH Keys injection
# ------------------------------------------
if [ -f ${SSH_KEY} ]; then
    eval `ssh-agent -s`
    ssh-add ${SSH_KEY}
    if [ -f ${SSH_KEY}'.pub' ]; then
        cat ${SSH_KEY}'.pub' | cat >> ~/.ssh/authorized_keys
    fi
    ssh-add -l

fi

# ------------------------------------------
# Setup git 1.9.4
# ------------------------------------------
cd ~ && \
wget https://www.kernel.org/pub/software/scm/git/git-1.9.4.tar.gz && \
tar xzf git-1.9.4.tar.gz

cd git-1.9.4 && \
make prefix=/usr/local/git all && \
make prefix=/usr/local/git install && \
echo "export PATH=$PATH:/usr/local/git/bin" >> /etc/bashrc && \
source /etc/bashrc

git config --global http.sslVerify false

# ------------------------------------------
# Setup node & npm
# ------------------------------------------
curl -sL https://rpm.nodesource.com/setup | bash -
yum install -y nodejs

cd /vagrant

# ------------------------------------------
# Setup node & pm2
# ------------------------------------------
npm install pm2 -g
npm install nodemon -g
npm install express -g


# ------------------------------------------
# Setup the app
# ------------------------------------------
# TODO

# ------------------------------------------
# Completion
# ------------------------------------------
echo Done.

