{pkgs, ...}: {
	envairoment.systemPaclages = with pkgs; [
		haskellPackages.hoogle
     		haskell.compiler.native-bignum.ghcHEAD
     		haskellPackages.stack
	];
}
