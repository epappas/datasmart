# -*- mode: ruby -*-
# vi: set ft=ruby :

# ==========================================
# MAIN Requirements & configurations
# ==========================================
require 'fileutils'
require 'etc'

Vagrant.require_version ">= 1.6.0"

CONFIG = File.join(File.dirname(__FILE__), "./devops/Vagrant-config.rb")
VAGRANTFILE_API_VERSION = "2"

$ENVIRONMENTS     = ['dev', 'test', 'production']
$DOMAIN           = 'local'

$num_instances    = $ENVIRONMENTS.length
$vb_gui           = false
$vb_memory        = 1024
$vb_cpus          = 1
$key              = ''
$key_pub          = ''
$keyBaseName      = ''
$user             = Etc.getlogin

rsa_key = File.expand_path('~') + '/.ssh/id_rsa'
dsa_key = File.expand_path('~') + '/.ssh/id_dsa'

# Vagrant Nodes
# ==========================================
nodes = [
    # { :hostname => 'erl',           :ip => '192.168.1.3', :box => 'trusty64', :box_url => '"https://vagrantcloud.com/ubuntu/boxes/trusty64/versions/14.04/providers/virtualbox.box'},
    # { :hostname => 'openapi',       :ip => '192.168.1.4', :box => 'trusty64', :box_url => '"https://vagrantcloud.com/ubuntu/boxes/trusty64/versions/14.04/providers/virtualbox.box'},
    { :hostname => 'experimental',  :ip => '192.168.1.5', :box => 'trusty64', :box_url => '"https://vagrantcloud.com/ubuntu/boxes/trusty64/versions/14.04/providers/virtualbox.box'}
]

# Key Finder
# ==========================================
if File.exists?(rsa_key)
  $key = rsa_key
  $key_pub = rsa_key + '.pub'
  $keyBaseName = File.basename($key)
elsif  File.exists?(dsa_key)
  $key = dsa_key
  $key_pub = rsa_key + '.pub'
  $keyBaseName = File.basename($key)
end

# Config Injection
# ==========================================
if File.exist?(CONFIG)
  require CONFIG
end

# ==========================================
# MAIN
# ==========================================
Vagrant.configure(VAGRANTFILE_API_VERSION) do |config|
  # ==========================================
  # Spin for each node in each environment
  # ==========================================
  nodes.each do |node|
    (0..($num_instances-1)).each do |i|
      config.vm.define vm_name = "%s.%s" % [node[:hostname], $ENVIRONMENTS[i]] do |node_config|

        # Set the box
        # ==========================================
        node_config.vm.box = "trusty64"
        node_config.vm.box_url = "https://vagrantcloud.com/ubuntu/boxes/trusty64/versions/14.04/providers/virtualbox.box"
        node_config.vm.hostname = "%s.%s" % [vm_name, $DOMAIN]

        # virtualbox configs
        # ==========================================
        node_config.vm.provider :virtualbox do |v|
          v.check_guest_additions = false
          v.functional_vboxsf = false
          v.gui = $vb_gui
          v.memory = $vb_memory
          v.cpus = $vb_cpus
        end

        # plugin conflict
        # ==========================================
        if Vagrant.has_plugin?("vagrant-vbguest") then
          node_config.vbguest.auto_update = true
        end

        # network
        # ==========================================
        ip = "%s%s" % [node[:ip], (i + 1)]
        config.vm.network "private_network", :ip => ip

        # ssh
        # ==========================================
        node_config.ssh.forward_agent = true

        # Copy over my keys
        # ==========================================
        if $key
          unless File.exist?("./devops/%s" % node[:hostname])
            FileUtils.mkdir_p("./devops/%s/.ssh" % node[:hostname])
          end

          FileUtils.cp($key, "./devops/%s/.ssh/%s" % [node[:hostname], $keyBaseName])
          FileUtils.cp($key_pub, "./devops/%s/.ssh/%s" % [node[:hostname], $keyBaseName + '.pub'])
        end

        # Docker
        # ==========================================
        # node_config.vm.provision "docker", :images => ["ubuntu", "shykes/couchdb", "redis", "coreos/etcd", "dockerfile/haproxy"]
        #
        # # Docker - couchdb
        # # ==========================================
        # node_config.vm.provision "docker" do |d|
        #   d.run "couchdb", :image => "shykes/couchdb", :args => "-d -p 5984:5984"
        # end
        #
        # # Docker - redis
        # # ==========================================
        # node_config.vm.provision "docker" do |d|
        #   d.run "redis", :image => "redis", :args => "-d -p 6379:6379 -p 26379:26379"
        # end

        # Syncs
        # ==========================================
        node_config.vm.synced_folder "./devops",          "/app/devops",        :id => "vagrant-devops",         :nfs => true
        node_config.vm.synced_folder "./data",            "/app/data",          :id => "vagrant-data",           :nfs => true
        node_config.vm.synced_folder "./datasmart_erl",   "/app/datasmart_erl", :id => "vagrant-datasmart_erl",  :nfs => true
        node_config.vm.synced_folder "./openapi",         "/app/openapi",       :id => "vagrant-openapi",        :nfs => true
        node_config.vm.synced_folder "./opentests",       "/app/opentests",     :id => "vagrant-opentests",      :nfs => true

      end # config.vm.define
    end # $num_instances loop
  end # nodes loop
end # of main

# config.vm.provider :digital_ocean do |provider, override|
#   override.ssh.private_key_path = '~/.ssh/id_rsa'
#   override.vm.box = 'digital_ocean'
#   override.vm.box_url = "https://github.com/smdahlen/vagrant-digitalocean/raw/master/box/digital_ocean.box"
#
#   provider.token = 'TOKEN'
#   provider.image = 'Ubuntu 14.04 x64'
#   provider.region = 'nyc2'
#   provider.size = '512mb'
# end

# config.vm.provision "shell" do |s|
#   s.path = "./bootstrap.sh"
#   s.args = []
# end