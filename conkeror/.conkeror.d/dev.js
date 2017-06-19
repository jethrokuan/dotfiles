// Examine element properties and style.

interactive("examine-element",
            "Examine the attributes and style of a DOM node.",
            function print_attribute (I) {
              var element = yield read_browser_object(I);
              var list = [];
              var style = I.window.getComputedStyle(element);
              var attributes = element.attributes;
              var name = element.tagName.toLowerCase();

              if (element.id) {
                name += "#" + element.id;
              }

              for (i = 0 ; i < element.classList.length ; i += 1) {
                name += "." + element.classList.item(i);
              }
              
              for (i = 0 ; i < style.length ; i += 1) {
                list.push([style.item(i), 1]);
              }

              for (i = 0 ; i < attributes.length ; i += 1) {
                list.push([attributes.item(i).name, 2]);
              }
              
              yield I.minibuffer.read(
                $prompt = name + ":",
                $completer = new prefix_completer(
                  $completions = list.sort(),
                  $get_string = function(item) item[0],
                  $get_description = function(item) {
                    var s, value;

                    switch(item[1]) {
                      case 1:
                        s = "CSS property";
                        value = style.getPropertyValue(item[0]);
                        
                        break;
                        
                      case 2:
                        s = "Attribute";
                        value = element.getAttribute(item[0]);
                        
                        break;
                    }
                    
                    if (value) {
                      s += " with value " + value;
                    }
                    
                    return s;
                  }),
                $auto_complete = true,
                $auto_complete_initial = true,
                $auto_complete_delay = 0,
                $require_match = false);
            },
            $browser_object = browser_object_dom_node);

define_key(default_base_keymap, "C-x x", "examine-element");
