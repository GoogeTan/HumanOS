{pkgs, ...}: {
  services.zerotierone = {
    enable = true;
    joinNetworks = ["856127940c67c518"];
    localConf = {
      setttings = {
        allowManaged = true;
        allowGlobal = true;
        allowDefault = true;
        allowDNS = true;
      };
    };
  };

  environment.systemPackages = with pkgs; [
	zerotierone
  ];
}
