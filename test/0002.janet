(import /xmljan1 :as "x")

(def name (dyn *current-file*))

(def gui-xml
  `
<?xml version="1.0" encoding="UTF-8"?>
<!-- test/0002.janet -->
<gui name="texteditor"
  version="1"
  xmlns="http://www.kde.org/standards/kxmlgui/1.0"
  xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
  xsi:schemaLocation="http://www.kde.org/standards/kxmlgui/1.0
 http://www.kde.org/standards/kxmlgui/1.0/kxmlgui.xsd">
  <MenuBar>
    <Menu name="file">
      <action name="clear" />
    </Menu>
    <Menu>
      <text>A&amp;nother Menu</text>
      <action name="clear" />
    </Menu>
  </MenuBar>
  <ToolBar name="mainToolBar">
    <text>Main Toolbar</text>
    <action name="clear" />
  </ToolBar>
</gui>

`)

(assert
 (=
  gui-xml
  (x/emit-to-string
   {:code
      ['(gui [name texteditor
              version "1"
              xmlns "http://www.kde.org/standards/kxmlgui/1.0"
              xmlns:xsi "http://www.w3.org/2001/XMLSchema-instance"
              xsi:schemaLocation `http://www.kde.org/standards/kxmlgui/1.0
                                  http://www.kde.org/standards/kxmlgui/1.0/kxmlgui.xsd`]
         (MenuBar
          (Menu [name file] (action [name clear]))
          (Menu (text "A&nother Menu")
                (action [name clear])))

         (ToolBar [name mainToolBar]
                  (text "Main Toolbar")
                  (action [name clear])))]
    :source-name name})))
