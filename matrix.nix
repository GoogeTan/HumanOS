{ pkgs, lib, config, ... }: {
  networking.firewall.allowedTCPPorts = [ 80 443 8008 ];
  services.postgresql.enable = true;
  services.matrix-synapse = {
    enable = true;
    #settings.server_name = "http://100.64.0.7";
    #settings.public_baseurl = "http://100.64.0.7";
    settings.listeners = [
      { port = 8008;
        bind_addresses = [ "::1" "100.64.0.7" ];
        type = "http";
        tls = false;
        x_forwarded = true;
        resources = [ {
          names = [ "client" "federation" ];
          compress = false;
        } ];
      }
    ];
    settings.database.name = "sqlite3";
    settings.registration_shared_secret = "secret";
  };
}
