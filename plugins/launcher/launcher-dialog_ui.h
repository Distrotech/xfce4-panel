/* automatically generated from launcher-dialog.glade */
#ifdef __SUNPRO_C
#pragma align 4 (launcher_dialog_ui)
#endif
#ifdef __GNUC__
static const char launcher_dialog_ui[] __attribute__ ((__aligned__ (4))) =
#else
static const char launcher_dialog_ui[] =
#endif
{
  "<?xml version=\"1.0\"?><interface><requires lib=\"gtk+\" version=\"2.14"
  "\"/><object class=\"GtkListStore\" id=\"arrow-position-model\"><columns"
  "><column type=\"gchararray\"/></columns><data><row><col id=\"0\" transl"
  "atable=\"yes\">Default</col></row><row><col id=\"0\" translatable=\"yes"
  "\">North</col></row><row><col id=\"0\" translatable=\"yes\">West</col><"
  "/row><row><col id=\"0\" translatable=\"yes\">East</col></row><row><col "
  "id=\"0\" translatable=\"yes\">South</col></row><row><col id=\"0\" trans"
  "latable=\"yes\">Inside Button</col></row></data></object><object class="
  "\"GtkListStore\" id=\"add-store\"><columns><column type=\"GdkPixbuf\"/>"
  "<column type=\"gchararray\"/><column type=\"GObject\"/><column type=\"g"
  "chararray\"/><column type=\"gchararray\"/></columns></object><object cl"
  "ass=\"GtkListStore\" id=\"item-store\"><columns><column type=\"GdkPixbu"
  "f\"/><column type=\"gchararray\"/><column type=\"GObject\"/><column typ"
  "e=\"gchararray\"/></columns></object><object class=\"GtkTreeModelFilter"
  "\" id=\"add-store-filter\"><property name=\"child_model\">add-store</pr"
  "operty></object><object class=\"GtkImage\" id=\"image11\"><property nam"
  "e=\"visible\">True</property><property name=\"can_focus\">False</proper"
  "ty><property name=\"stock\">gtk-edit</property><property name=\"icon-si"
  "ze\">1</property></object><object class=\"GtkImage\" id=\"image12\"><pr"
  "operty name=\"visible\">True</property><property name=\"can_focus\">Fal"
  "se</property><property name=\"stock\">gtk-delete</property><property na"
  "me=\"icon-size\">1</property></object><object class=\"GtkImage\" id=\"i"
  "mage13\"><property name=\"visible\">True</property><property name=\"can"
  "_focus\">False</property><property name=\"stock\">gtk-new</property><pr"
  "operty name=\"icon-size\">1</property></object><object class=\"GtkImage"
  "\" id=\"image14\"><property name=\"visible\">True</property><property n"
  "ame=\"can_focus\">False</property><property name=\"icon_name\">applicat"
  "ions-internet</property><property name=\"icon-size\">1</property></obje"
  "ct><object class=\"GtkImage\" id=\"image15\"><property name=\"visible\""
  ">True</property><property name=\"can_focus\">False</property><property "
  "name=\"stock\">gtk-add</property><property name=\"icon-size\">1</proper"
  "ty></object><object class=\"GtkMenu\" id=\"popup-menu\"><property name="
  "\"visible\">True</property><property name=\"can_focus\">False</property"
  "><child><object class=\"GtkImageMenuItem\" id=\"mi-move-up\"><property "
  "name=\"label\">gtk-go-up</property><property name=\"visible\">True</pro"
  "perty><property name=\"can_focus\">False</property><property name=\"use"
  "_underline\">True</property><property name=\"use_stock\">True</property"
  "></object></child><child><object class=\"GtkImageMenuItem\" id=\"mi-mov"
  "e-down\"><property name=\"label\">gtk-go-down</property><property name="
  "\"visible\">True</property><property name=\"can_focus\">False</property"
  "><property name=\"use_underline\">True</property><property name=\"use_s"
  "tock\">True</property></object></child><child><object class=\"GtkSepara"
  "torMenuItem\" id=\"smi1\"><property name=\"visible\">True</property><pr"
  "operty name=\"can_focus\">False</property></object></child><child><obje"
  "ct class=\"GtkImageMenuItem\" id=\"mi-edit\"><property name=\"label\" t"
  "ranslatable=\"yes\">_Edit Item</property><property name=\"visible\">Tru"
  "e</property><property name=\"can_focus\">False</property><property name"
  "=\"use_underline\">True</property><property name=\"image\">image11</pro"
  "perty><property name=\"use_stock\">False</property></object></child><ch"
  "ild><object class=\"GtkImageMenuItem\" id=\"mi-delete\"><property name="
  "\"label\" translatable=\"yes\">D_elete Item</property><property name=\""
  "visible\">True</property><property name=\"can_focus\">False</property><"
  "property name=\"use_underline\">True</property><property name=\"image\""
  ">image12</property><property name=\"use_stock\">False</property></objec"
  "t></child><child><object class=\"GtkSeparatorMenuItem\" id=\"smi2\"><pr"
  "operty name=\"visible\">True</property><property name=\"can_focus\">Fal"
  "se</property></object></child><child><object class=\"GtkImageMenuItem\""
  " id=\"mi-add\"><property name=\"label\" translatable=\"yes\">Add Appli_"
  "cation</property><property name=\"visible\">True</property><property na"
  "me=\"can_focus\">False</property><property name=\"use_underline\">True<"
  "/property><property name=\"image\">image15</property><property name=\"u"
  "se_stock\">False</property></object></child><child><object class=\"GtkS"
  "eparatorMenuItem\" id=\"smi3\"><property name=\"visible\">True</propert"
  "y><property name=\"can_focus\">False</property></object></child><child>"
  "<object class=\"GtkImageMenuItem\" id=\"mi-application\"><property name"
  "=\"label\" translatable=\"yes\">New _Application</property><property na"
  "me=\"visible\">True</property><property name=\"can_focus\">False</prope"
  "rty><property name=\"use_underline\">True</property><property name=\"im"
  "age\">image13</property><property name=\"use_stock\">False</property></"
  "object></child><child><object class=\"GtkImageMenuItem\" id=\"mi-link\""
  "><property name=\"label\" translatable=\"yes\">New _Link</property><pro"
  "perty name=\"visible\">True</property><property name=\"can_focus\">Fals"
  "e</property><property name=\"use_underline\">True</property><property n"
  "ame=\"image\">image14</property><property name=\"use_stock\">False</pro"
  "perty></object></child></object><object class=\"XfceTitledDialog\" id=\""
  "dialog\"><property name=\"title\" translatable=\"yes\">Launcher</proper"
  "ty><property name=\"window_position\">center</property><property name=\""
  "default_width\">350</property><property name=\"default_height\">400</pr"
  "operty><property name=\"icon_name\">gtk-properties</property><property "
  "name=\"type_hint\">normal</property><property name=\"has_separator\">Fa"
  "lse</property><child internal-child=\"vbox\"><object class=\"GtkVBox\" "
  "id=\"dialog-vbox2\"><property name=\"visible\">True</property><property"
  " name=\"orientation\">vertical</property><property name=\"spacing\">2</"
  "property><child><object class=\"GtkNotebook\" id=\"notebook2\"><propert"
  "y name=\"visible\">True</property><property name=\"can_focus\">True</pr"
  "operty><property name=\"border_width\">6</property><child><object class"
  "=\"GtkHBox\" id=\"hbox1\"><property name=\"visible\">True</property><pr"
  "operty name=\"border_width\">6</property><property name=\"spacing\">6</"
  "property><child><object class=\"GtkScrolledWindow\" id=\"scrolledwindow"
  "2\"><property name=\"visible\">True</property><property name=\"can_focu"
  "s\">True</property><property name=\"hscrollbar_policy\">automatic</prop"
  "erty><property name=\"vscrollbar_policy\">automatic</property><property"
  " name=\"shadow_type\">etched-in</property><child><object class=\"GtkTre"
  "eView\" id=\"item-treeview\"><property name=\"visible\">True</property>"
  "<property name=\"can_focus\">True</property><property name=\"model\">it"
  "em-store</property><property name=\"headers_visible\">False</property><"
  "property name=\"rules_hint\">True</property><property name=\"enable_sea"
  "rch\">False</property><property name=\"tooltip_column\">3</property><ch"
  "ild><object class=\"GtkTreeViewColumn\" id=\"treeviewcolumn2\"><propert"
  "y name=\"spacing\">2</property><child><object class=\"GtkCellRendererPi"
  "xbuf\" id=\"itemrenderericon\"/><attributes><attribute name=\"pixbuf\">"
  "0</attribute></attributes></child><child><object class=\"GtkCellRendere"
  "rText\" id=\"itemrenderername\"/><attributes><attribute name=\"markup\""
  ">1</attribute></attributes></child></object></child></object></child></"
  "object><packing><property name=\"position\">0</property></packing></chi"
  "ld><child><object class=\"GtkAlignment\" id=\"alignment1\"><property na"
  "me=\"visible\">True</property><property name=\"yalign\">0</property><pr"
  "operty name=\"xscale\">0</property><property name=\"yscale\">0</propert"
  "y><child><object class=\"GtkVBox\" id=\"vbox3\"><property name=\"visibl"
  "e\">True</property><property name=\"orientation\">vertical</property><p"
  "roperty name=\"spacing\">6</property><property name=\"homogeneous\">Tru"
  "e</property><child><object class=\"GtkButton\" id=\"item-move-up\"><pro"
  "perty name=\"visible\">True</property><property name=\"can_focus\">True"
  "</property><property name=\"receives_default\">True</property><property"
  " name=\"tooltip_text\" translatable=\"yes\">Move currently selected ite"
  "m up by one row</property><child><object class=\"GtkImage\" id=\"image2"
  "\"><property name=\"visible\">True</property><property name=\"stock\">g"
  "tk-go-up</property></object></child></object><packing><property name=\""
  "expand\">False</property><property name=\"position\">0</property></pack"
  "ing></child><child><object class=\"GtkButton\" id=\"item-move-down\"><p"
  "roperty name=\"visible\">True</property><property name=\"can_focus\">Tr"
  "ue</property><property name=\"receives_default\">True</property><proper"
  "ty name=\"tooltip_text\" translatable=\"yes\">Move currently selected i"
  "tem down by one row</property><child><object class=\"GtkImage\" id=\"im"
  "age3\"><property name=\"visible\">True</property><property name=\"stock"
  "\">gtk-go-down</property></object></child></object><packing><property n"
  "ame=\"expand\">False</property><property name=\"position\">1</property>"
  "</packing></child><child><object class=\"GtkButton\" id=\"item-add\"><p"
  "roperty name=\"visible\">True</property><property name=\"can_focus\">Tr"
  "ue</property><property name=\"receives_default\">True</property><proper"
  "ty name=\"tooltip_text\" translatable=\"yes\">Add one or more existing "
  "items to the launcher</property><child><object class=\"GtkImage\" id=\""
  "image4\"><property name=\"visible\">True</property><property name=\"sto"
  "ck\">gtk-add</property></object></child></object><packing><property nam"
  "e=\"expand\">False</property><property name=\"position\">2</property></"
  "packing></child><child><object class=\"GtkButton\" id=\"item-new\"><pro"
  "perty name=\"visible\">True</property><property name=\"can_focus\">True"
  "</property><property name=\"receives_default\">True</property><property"
  " name=\"tooltip_text\" translatable=\"yes\">Add a new empty item</prope"
  "rty><child><object class=\"GtkImage\" id=\"image10\"><property name=\"v"
  "isible\">True</property><property name=\"stock\">gtk-new</property></ob"
  "ject></child></object><packing><property name=\"position\">3</property>"
  "</packing></child><child><object class=\"GtkButton\" id=\"item-delete\""
  "><property name=\"visible\">True</property><property name=\"can_focus\""
  ">True</property><property name=\"receives_default\">True</property><pro"
  "perty name=\"tooltip_text\" translatable=\"yes\">Delete the currently s"
  "elected item</property><child><object class=\"GtkImage\" id=\"image9\">"
  "<property name=\"visible\">True</property><property name=\"stock\">gtk-"
  "delete</property></object></child></object><packing><property name=\"ex"
  "pand\">False</property><property name=\"position\">4</property></packin"
  "g></child><child><object class=\"GtkButton\" id=\"item-edit\"><property"
  " name=\"visible\">True</property><property name=\"can_focus\">True</pro"
  "perty><property name=\"receives_default\">True</property><property name"
  "=\"tooltip_text\" translatable=\"yes\">Edit the currently selected item"
  "</property><child><object class=\"GtkImage\" id=\"image1\"><property na"
  "me=\"visible\">True</property><property name=\"stock\">gtk-edit</proper"
  "ty></object></child></object><packing><property name=\"expand\">False</"
  "property><property name=\"position\">5</property></packing></child></ob"
  "ject></child></object><packing><property name=\"expand\">False</propert"
  "y><property name=\"position\">1</property></packing></child></object></"
  "child><child type=\"tab\"><object class=\"GtkLabel\" id=\"label1\"><pro"
  "perty name=\"visible\">True</property><property name=\"label\" translat"
  "able=\"yes\">General</property></object><packing><property name=\"tab_f"
  "ill\">False</property></packing></child><child><object class=\"GtkVBox\""
  " id=\"vbox1\"><property name=\"visible\">True</property><property name="
  "\"border_width\">6</property><property name=\"orientation\">vertical</p"
  "roperty><property name=\"spacing\">6</property><child><object class=\"G"
  "tkCheckButton\" id=\"disable-tooltips\"><property name=\"label\" transl"
  "atable=\"yes\">Disable t_ooltips</property><property name=\"visible\">T"
  "rue</property><property name=\"can_focus\">True</property><property nam"
  "e=\"receives_default\">False</property><property name=\"tooltip_text\" "
  "translatable=\"yes\">Select this option to disable the tooltips when mo"
  "ving over the panel button or menu items.</property><property name=\"us"
  "e_underline\">True</property><property name=\"draw_indicator\">True</pr"
  "operty></object><packing><property name=\"expand\">False</property><pro"
  "perty name=\"position\">0</property></packing></child><child><object cl"
  "ass=\"GtkCheckButton\" id=\"show-label\"><property name=\"label\" trans"
  "latable=\"yes\">Show _label instead of icon</property><property name=\""
  "visible\">True</property><property name=\"can_focus\">True</property><p"
  "roperty name=\"receives_default\">False</property><property name=\"use_"
  "underline\">True</property><property name=\"draw_indicator\">True</prop"
  "erty></object><packing><property name=\"expand\">False</property><prope"
  "rty name=\"position\">1</property></packing></child><child><object clas"
  "s=\"GtkCheckButton\" id=\"move-first\"><property name=\"label\" transla"
  "table=\"yes\">Show last _used item in panel</property><property name=\""
  "visible\">True</property><property name=\"can_focus\">True</property><p"
  "roperty name=\"receives_default\">False</property><property name=\"tool"
  "tip_text\" translatable=\"yes\">Select this option to move the clicked "
  "menu item to the panel.</property><property name=\"use_underline\">True"
  "</property><property name=\"draw_indicator\">True</property></object><p"
  "acking><property name=\"expand\">False</property><property name=\"posit"
  "ion\">2</property></packing></child><child><object class=\"GtkHBox\" id"
  "=\"hbox2\"><property name=\"visible\">True</property><property name=\"s"
  "pacing\">12</property><child><object class=\"GtkLabel\" id=\"arrow-posi"
  "tion-label\"><property name=\"visible\">True</property><property name=\""
  "label\" translatable=\"yes\">_Arrow button position:</property><propert"
  "y name=\"use_underline\">True</property></object><packing><property nam"
  "e=\"expand\">False</property><property name=\"position\">0</property></"
  "packing></child><child><object class=\"GtkComboBox\" id=\"arrow-positio"
  "n\"><property name=\"visible\">True</property><property name=\"model\">"
  "arrow-position-model</property><child><object class=\"GtkCellRendererTe"
  "xt\" id=\"cellrenderertext1\"/><attributes><attribute name=\"text\">0</"
  "attribute></attributes></child></object><packing><property name=\"expan"
  "d\">False</property><property name=\"position\">1</property></packing><"
  "/child></object><packing><property name=\"expand\">False</property><pro"
  "perty name=\"position\">3</property></packing></child></object><packing"
  "><property name=\"position\">1</property></packing></child><child type="
  "\"tab\"><object class=\"GtkLabel\" id=\"label5\"><property name=\"visib"
  "le\">True</property><property name=\"label\" translatable=\"yes\">Advan"
  "ced</property></object><packing><property name=\"position\">1</property"
  "><property name=\"tab_fill\">False</property></packing></child></object"
  "><packing><property name=\"position\">1</property></packing></child><ch"
  "ild internal-child=\"action_area\"><object class=\"GtkHButtonBox\" id=\""
  "dialog-action_area2\"><property name=\"visible\">True</property><proper"
  "ty name=\"layout_style\">end</property><child><object class=\"GtkButton"
  "\" id=\"help-button\"><property name=\"label\">gtk-help</property><prop"
  "erty name=\"visible\">True</property><property name=\"can_focus\">True<"
  "/property><property name=\"receives_default\">True</property><property "
  "name=\"use_stock\">True</property></object><packing><property name=\"ex"
  "pand\">False</property><property name=\"fill\">False</property><propert"
  "y name=\"position\">0</property><property name=\"secondary\">True</prop"
  "erty></packing></child><child><object class=\"GtkButton\" id=\"button2\""
  "><property name=\"label\">gtk-close</property><property name=\"visible\""
  ">True</property><property name=\"can_focus\">True</property><property n"
  "ame=\"receives_default\">True</property><property name=\"use_stock\">Tr"
  "ue</property></object><packing><property name=\"expand\">False</propert"
  "y><property name=\"fill\">False</property><property name=\"position\">1"
  "</property></packing></child></object><packing><property name=\"expand\""
  ">False</property><property name=\"pack_type\">end</property><property n"
  "ame=\"position\">0</property></packing></child></object></child><action"
  "-widgets><action-widget response=\"1\">help-button</action-widget><acti"
  "on-widget response=\"0\">button2</action-widget></action-widgets></obje"
  "ct><object class=\"XfceTitledDialog\" id=\"dialog-add\"><property name="
  "\"title\" translatable=\"yes\">Add New Item</property><property name=\""
  "window_position\">center-on-parent</property><property name=\"default_w"
  "idth\">400</property><property name=\"default_height\">400</property><p"
  "roperty name=\"destroy_with_parent\">True</property><property name=\"ic"
  "on_name\">gtk-add</property><property name=\"type_hint\">normal</proper"
  "ty><property name=\"has_separator\">False</property><property name=\"su"
  "btitle\" translatable=\"yes\">Add one or more existing items to the lau"
  "ncher</property><child internal-child=\"vbox\"><object class=\"GtkVBox\""
  " id=\"dialog-vbox4\"><property name=\"visible\">True</property><propert"
  "y name=\"orientation\">vertical</property><property name=\"spacing\">2<"
  "/property><child><object class=\"GtkVBox\" id=\"vbox2\"><property name="
  "\"visible\">True</property><property name=\"border_width\">6</property>"
  "<property name=\"orientation\">vertical</property><property name=\"spac"
  "ing\">6</property><child><object class=\"GtkHBox\" id=\"hbox4\"><proper"
  "ty name=\"visible\">True</property><property name=\"spacing\">6</proper"
  "ty><child><object class=\"GtkLabel\" id=\"label4\"><property name=\"vis"
  "ible\">True</property><property name=\"xalign\">1</property><property n"
  "ame=\"label\" translatable=\"yes\">_Search:</property><property name=\""
  "use_underline\">True</property></object><packing><property name=\"posit"
  "ion\">0</property></packing></child><child><object class=\"GtkEntry\" i"
  "d=\"add-search\"><property name=\"visible\">True</property><property na"
  "me=\"can_focus\">True</property></object><packing><property name=\"expa"
  "nd\">False</property><property name=\"position\">1</property></packing>"
  "</child></object><packing><property name=\"expand\">False</property><pr"
  "operty name=\"position\">0</property></packing></child><child><object c"
  "lass=\"GtkScrolledWindow\" id=\"scrolledwindow1\"><property name=\"visi"
  "ble\">True</property><property name=\"can_focus\">True</property><prope"
  "rty name=\"hscrollbar_policy\">automatic</property><property name=\"vsc"
  "rollbar_policy\">automatic</property><property name=\"shadow_type\">etc"
  "hed-in</property><child><object class=\"GtkTreeView\" id=\"add-treeview"
  "\"><property name=\"visible\">True</property><property name=\"can_focus"
  "\">True</property><property name=\"model\">add-store-filter</property><"
  "property name=\"headers_visible\">False</property><property name=\"rule"
  "s_hint\">True</property><property name=\"enable_search\">False</propert"
  "y><property name=\"tooltip_column\">3</property><child><object class=\""
  "GtkTreeViewColumn\" id=\"treeviewcolumn1\"><property name=\"spacing\">2"
  "</property><child><object class=\"GtkCellRendererPixbuf\" id=\"addrende"
  "rericon\"/><attributes><attribute name=\"pixbuf\">0</attribute></attrib"
  "utes></child><child><object class=\"GtkCellRendererText\" id=\"addrende"
  "rername\"/><attributes><attribute name=\"markup\">1</attribute></attrib"
  "utes></child></object></child></object></child></object><packing><prope"
  "rty name=\"position\">1</property></packing></child></object><packing><"
  "property name=\"position\">1</property></packing></child><child interna"
  "l-child=\"action_area\"><object class=\"GtkHButtonBox\" id=\"dialog-act"
  "ion_area4\"><property name=\"visible\">True</property><property name=\""
  "layout_style\">end</property><child><object class=\"GtkButton\" id=\"bu"
  "tton-add\"><property name=\"label\">gtk-add</property><property name=\""
  "visible\">True</property><property name=\"sensitive\">False</property><"
  "property name=\"can_focus\">True</property><property name=\"receives_de"
  "fault\">True</property><property name=\"use_stock\">True</property></ob"
  "ject><packing><property name=\"expand\">False</property><property name="
  "\"fill\">False</property><property name=\"position\">0</property></pack"
  "ing></child><child><object class=\"GtkButton\" id=\"button5\"><property"
  " name=\"label\">gtk-close</property><property name=\"visible\">True</pr"
  "operty><property name=\"can_focus\">True</property><property name=\"rec"
  "eives_default\">True</property><property name=\"use_stock\">True</prope"
  "rty></object><packing><property name=\"expand\">False</property><proper"
  "ty name=\"fill\">False</property><property name=\"position\">1</propert"
  "y></packing></child></object><packing><property name=\"expand\">False</"
  "property><property name=\"pack_type\">end</property><property name=\"po"
  "sition\">0</property></packing></child></object></child><action-widgets"
  "><action-widget response=\"1\">button-add</action-widget><action-widget"
  " response=\"0\">button5</action-widget></action-widgets></object></inte"
  "rface>"
};

static const unsigned launcher_dialog_ui_length = 20033u;

