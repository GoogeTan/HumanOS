{pkgs, ...}: {
	environment.systemPackages = with pkgs; [
		waybar
		pavucontrol
		playerctl
	];
}
