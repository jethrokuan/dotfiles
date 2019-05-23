function nix_update -d "Pull latest nixos and rebuild"
    pushd $HOME/nix-config/nixpkgs
    git checkout master
    git pull origin master
    git push fork master
    popd
end
