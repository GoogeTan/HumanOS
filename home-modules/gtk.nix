{pkgs, ...}: {
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
	
	#home.file = {
	#    ".config/gtk-3.0".source = ../gtk-3.0;
	#    ".config/gtk-4.0".source = ../gtk-4.0;
	#};
	home.packages = with pkgs; [
		dunst
		pipewire
		hyprland-workspaces

		graphite-cursors
		graphite-gtk-theme
	];
}
