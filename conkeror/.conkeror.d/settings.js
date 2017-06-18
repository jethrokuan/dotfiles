theme_load_paths.unshift("/home/jethro/.conkeror.d/themes");
theme_unload("default");
theme_load("zenburn");

homepage = "http://en.wikipedia.org/wiki/Special:Random";

define_key(default_global_keymap, "M-a", "execute-extended-command");

view_source_use_external_editor = true;
editor_shell_command = "urxvt -e emacsclient -nw";

cwd = get_home_directory();
cwd.append("Downloads");

add_hook("mode_line_hook", mode_line_adder(buffer_count_widget), true);

session_pref("xpinstall.whitelist.required", false);

function darken_page (I) {
  var styles='* { background: black !important; color: grey !important; }'+
      ':link, :link * { color: #4986dd !important; }'+
      ':visited, :visited * { color: #d75047 !important; }';
  var document = I.buffer.document;
  var newSS=document.createElement('link');
  newSS.rel='stylesheet';
  newSS.href='data:text/css,'+escape(styles);
  document.getElementsByTagName("head")[0].appendChild(newSS);
}

interactive("darken-page", "Darken the page in an attempt to save your eyes.",
    darken_page);
