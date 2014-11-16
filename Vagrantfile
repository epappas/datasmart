# -*- mode: ruby -*-
# vi: set ft=ruby :

require 'fileutils'

Vagrant.require_version ">= 1.6.0"

CONFIG = File.join(File.dirname(__FILE__), "config.rb")
VAGRANTFILE_API_VERSION = "2"

$num_instances = 1
$vb_gui = false
$vb_memory = 1024
$vb_cpus = 1

$expose_http_tcp = 8090
$expose_docker_tcp = 4290
$expose_couchdb_tcp = 5990
$expose_redis_tcp = 6380
$expose_redis_sent_tcp = 26380

# ENV["..."]

if File.exist?(CONFIG)
  require CONFIG
end

Vagrant.configure(VAGRANTFILE_API_VERSION) do |config|
  # config.vm.box = "precise64"
  # config.vm.box_url = "http://files.vagrantup.com/precise64.box"
  config.vm.box = "coreos"
  config.vm.box_url = "http://storage.core-os.net/coreos/amd64-generic/dev-channel/coreos_production_vagrant.box"

  config.vm.provider :virtualbox do |v|
    v.check_guest_additions = false
    v.functional_vboxsf = false
  end

  # plugin conflict
  if Vagrant.has_plugin?("vagrant-vbguest") then
    config.vbguest.auto_update = true
  end

  (1..$num_instances).each do |i|
    # config.vm.define vm_name = "ubuntu-%02d.datasmart" % i do |config|
    config.vm.define vm_name = "core-%02d.datasmart" % i do |config|
      config.vm.hostname = vm_name

      # config.vm.network "forwarded_port", guest: 8080, host: ($expose_http_tcp + i - 1), auto_correct: true
      # config.vm.network "forwarded_port", guest: 2375, host: ($expose_docker_tcp + i - 1), auto_correct: true
      # config.vm.network "forwarded_port", guest: 5984, host: ($expose_couchdb_tcp + i - 1), auto_correct: true
      # config.vm.network "forwarded_port", guest: 6379, host: ($expose_redis_tcp + i - 1), auto_correct: true
      # config.vm.network "forwarded_port", guest: 26379, host: ($expose_redis_sent_tcp + i - 1), auto_correct: true

      config.vm.provider :virtualbox do |vb|
        vb.gui = $vb_gui
        vb.memory = $vb_memory
        vb.cpus = $vb_cpus
      end

      ip = "192.168.40.#{i+100}"
      config.vm.network :private_network, ip: ip

      # config.vm.network "public_network"

      config.ssh.forward_agent = true

      config.vm.synced_folder "./data", "/home/core/share/data"
      config.vm.synced_folder "./datasmart_erl", "/home/core/share/src/datasmart_erl"
      config.vm.synced_folder "./devops", "/home/core/share/src/devops"
      config.vm.synced_folder "./openapi", "/home/core/share/src/openapi"
      config.vm.synced_folder "./opentests", "/home/core/share/src/opentests"

      config.vm.provision "docker", images: ["ubuntu", "shykes/couchdb", "redis", "coreos/etcd", "dockerfile/haproxy"]

      config.vm.provision "docker" do |d|
        d.run "couchdb-1", image: "shykes/couchdb", args: "-d -p 5984:5984"
      end

      config.vm.provision "docker" do |d|
        d.run "redis-1", image: "redis", args: "-d -p 6379:6379 -p 26379:26379"
      end

      # config.vm.provision "docker" do |d|
      #   d.run "etcd-1", image: "coreos/etcd", args: "-d -p 4001:4001 -p 7001:7001 -p 2379:2379 -p 2380:2380"
      # end

      config.vm.provision "docker" do |d|
        d.run "haproxy-1", image: "dockerfile/haproxy", args: "-d -p 80:80 -p 443:443"
      end
    end
  end
end