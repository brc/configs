###############################################
## VARS
###############################################
_USER = "duck"
_HOME_DIR = "/home/#{_USER}"
_CHEF_DIR = "#{_HOME_DIR}/.chef"
_PASS_DIR = "#{_CHEF_DIR}/passwords.d"
###############################################

log_level               :info
log_location            STDOUT
node_name               _USER
validation_client_name  'chef-validator'
validation_key          "#{_CHEF_DIR}/chef-validator.pem"
client_key              "/etc/chef/#{_USER}.pem"
chef_server_url         File.read("#{_CHEF_DIR}/chef_server_url").chomp

knife[:rackspace_api_auth_url] = "identity.api.rackspacecloud.com"
knife[:rackspace_api_username] = "cloud10"
knife[:rackspace_api_key]      = File.read("#{_PASS_DIR}/cloud10_apikey").chomp

# TODO DenyHosts
knife[:t_packages] = "tmux colordiff"
knife[:t_packages_apt] = "vim-nox"
knife[:t_packages_rpm] = "vim-enhanced nc strace ccze python-six"
knife[:t_cf_container] = "bc-bitbucket"
knife[:t_cf_tarball] = "bootstrap.tar.gz"

knife[:t_os_auth_url] = "https://identity.api.rackspacecloud.com/v2.0/"
knife[:t_cf_username] = File.read("#{_PASS_DIR}/cloud10_cf_user").chomp
knife[:t_os_password] = File.read("#{_PASS_DIR}/cloud10").chomp
knife[:t_os_tenant_name] = File.read("#{_PASS_DIR}/cloud10_tenant").chomp
knife[:t_os_region_name] = "DFW"

knife[:t_working_dir] = "/root"
knife[:t_run_command] = "/root/bootstrap/doit.sh"
