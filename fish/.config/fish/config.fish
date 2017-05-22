for file in ~/.config/fish/conf.d/*.fish
    source $file
end

# The next line updates PATH for the Google Cloud SDK.
if [ -f '/Users/jethrokuan/Downloads/google-cloud-sdk/path.fish.inc' ]; if type source > /dev/null; source '/Users/jethrokuan/Downloads/google-cloud-sdk/path.fish.inc'; else; . '/Users/jethrokuan/Downloads/google-cloud-sdk/path.fish.inc'; end; end
