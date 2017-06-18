theme_load_paths.unshift("/home/jethro/.conkeror.d/themes");
theme_unload("default");
theme_load("zenburn");

stylesheet = 'file:///home/jethro/.conkeror.d/themes/dark.css';

register_user_stylesheet(stylesheet);

global_css_registered = true;

function toggle_global_css(I){
  global_css_registered=global_css_registered ? false : true;
  if(global_css_registered){
    register_user_stylesheet(stylesheet);
  }else{
    unregister_user_stylesheet(stylesheet);
  }
}

interactive("toggle-global-css", "Toggle global.css", toggle_global_css);
define_key(default_global_keymap, "D", "toggle-global-css");

active_hint_background_color = "#dcdccc";
