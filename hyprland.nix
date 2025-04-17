{pkgs, ...}: {
	environment.systemPackages = with pkgs; [
		dunst
		pipewire
		hyprland-workspaces

		graphite-cursors
		graphite-gtk-theme
	];

	programs.hyprland.enable = true;

	home-manager.users.zahara = {pkgs, ...}: {
		gtk = {
    			enable = true;

    			iconTheme = {
      				name = "Colloid-Light";
      				package = pkgs.colloid-icon-theme;
    			};

    			theme = {
      				name = "graphite-light";
      				package = pkgs.graphite-gtk-theme;
    			};

    			cursorTheme = {
      				name = "graphite-light";
      				package = pkgs.graphite-cursors; # Cursor
    			};

    			gtk3.extraConfig = {
      				Settings = ''
        				gtk-application-prefer-dark-theme=1
      				'';
   			};

    			gtk4.extraConfig = {
      				Settings = ''
       			 		gtk-application-prefer-dark-theme=1
      				'';
    			};
  		};
		home.sessionVariables.GTK_THEME = "graphite";
	};

	environment.etc = {
     		"xdg/gtk-3.0".source =  ./gtk-4.0;
     		"xdg/gtk-4.0".source =  ./gtk-3.0;
   	};
}
