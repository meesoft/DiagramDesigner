-------------------------------------------------------------------------------                                
                                Diagram Designer
-------------------------------------------------------------------------------                                
        Copyright (C) 2003-2023  |  Michael Vinther  |  mv@logicnet�dk
                        MeeSoft  |  http://meesoft.logicnet.dk
-------------------------------------------------------------------------------                                

Description:
------------
  Simple vector graphics editor for creating flowchart and diagrams.


Some of the features:
---------------------
  * Customizable template object palette.
  * Slide show viewer.
  * Import/export WMF, EMF, GIF, BMP, JPEG, PNG, MNG, and PCX images. 
  * Simple graph plotter to plot mathematical expressions.
  * Spellchecker.
  * Advanced "pocket" calculator with equation solver.
  * MeeSoft Image Analyzer integration for bitmap image editing and extended 
    file format support.
  * Uses compressed file format for minimizing drawing file size.
  

License information:
--------------------    
Conditions for using and copying the software:
  * I (Michael Vinther) or MeeSoft can not in any way be held responsible for 
    any damage or loss caused by using this software.
  * The software can only be copied/distributed in its original form (installer
    or zip archive) unless otherwise agreed with the author.
  * Distribution of the software on CD-ROMs, Internet or other media is not 
    allowed without consent from the author.
This means that you do not have to ask my permission to use it in your company
or for personal use! It is meant to be free for everyone to use but of course a 
donation is always appreciated...
  
Thanks to Markus Oberhumer and Laszlo Molnar for their brilliant EXE compressor
UPX. (See http://upx.sourceforge.net/)

AutoRealm symbols are used with permission of Andy Gryc. They are free and may 
not be sold. AutoRealm is a GNU copyrighted program for making maps used in 
role playing games. AutoRealm is available at:
http://autorealm.sourceforge.net/index.html

Electronic symbols by Schelte Heeringa


Version history:
----------------
Note that this version history is incomplete. It does not include all changes
in all versions. 

Changes in version 1.30.0  (2023-01-22):
  Improve handling of damaged files
  Translation updates
  Released from open source repo
Changes in version 1.29.5  (2019-10-10):
  Enable web links on objects in slideshow mode
Changes in version 1.29.4  (2019-03-24):
  Fixed value edit box display problem in WinXP 
Changes in version 1.29.2  (2019-01-20):
  CR2 and CR3 raw file preview image import 
  Translation update
Changes in version 1.29.2  (2018-07-12):
  Fixed issue copy/pasting curved connectors
Changes in version 1.29.1  (2017-09-26):
  Fixed issue with reading some diagrams from previous versions
Changes in version 1.29  (2017-08-26):
  "Diagram | Connector line style" option to control whether the background of connector/curve texts are filled
  Support for curved connectors
  Automatically try to fix corrupted files
  Fixed text rendering issues after automatic line breaks
Changes in version 1.28  (2015-05-10):
  Fix for text alignment in rotated text
  Line/connector endpoints in the property editor
  Don't hide mouse position in the status bar when hiding the template panel
  Bugfix in text editor dialog
Changes in version 1.27.3  (2014-11-20):
  Fix for error during mouse drawing
  Fix for line breaking in rotated text
  Reset size buttons in picture object properties
Changes in version 1.27.1  (2014-08-31):
  Installer fix (error during file format association)
Changes in version 1.27  (2014-08-17):
  Transparent PNG handling bugfix
  Drag-drop support for changing object order in tree
  \@ ... \@ around text with disable other escape sequences inside
  Link points can be edited for standard rectangles
  Improved object text margin
  Improved link editor
Changes in version 1.26  (2013-01-06):
  More text position options on polygon objects
  Change line break rules
  Option to disable connector snapping  
  Fixed bug when enabling/disabling displays
  Full page in zoom menu
  Copy/paste link points (link editor)
  User defined default link points for groups and images (options dialog)
  Improved connector routing
  Text rotation
  Store flowchart templates pane visibility
Changes in version 1.25  (2012-01-22):
  Auto crop option in export
  Improved connector routing
  Grid size in toolbar
  Distribute in Align menu
  Tabs for pages and layers
  Template palette drop down
  Connector text background painted with fill color
  Automatic line breaks
  Drop down menus for changing line end styles (arrows etc.)
  Many relation arrow style
Changes in version 1.24  (2011-03-12):
  Click to apply format in text editor
  Strikeout text format
Changes in version 1.23.6  (2010-10-03):
  Improved diagram export
  Bugfix for printing multiple pages
  Lock on grid editing in Options dialog
  Copy to clipboard bugfix
Changes in version 1.23  (2010-06-12):
  Bugfix related to setting default diagram font 
  Windows 7 slideshow mouse cursor fix
  Gradient color in filled objects
  Print as bitmap mode for better handling of transparent objects
Changes in version 1.22  (2009-08-30):
  If DiagramDesigner.ini exists settings are stored here instead of in registry  
  Default links and anchors on group objects for easy scaling
  Option to show printer margins (the area where the printer can actually print)
  The junction between connected lines can be moved without disconnecting the lines
  Object size display in status bar while resizing
  Links to other pages now working in slide show mode 
Changes in version 1.21.2  (2008-12-02):
  Template display bugfix  
Changes in version 1.21.1  (2008-??-??):
  Win98 bugfix
Changes in version 1.21  (2008-10-14):
  UML class diagram symbols
  Line start/end marker can be changed in toolbar
  Print scaling setting
  Link point markers (red x) only shown while dragging an object
  New improved antialiasing algorithm
  Object shadows option
  Find text function
Changes in version 1.20  (2008-03-16):
  Improved Vista support
  Diamond line end style
  Line segments style for curve lines
  New outline connector style
  Scaling anchors on objects in group to support scaling of a group
  Bitmap alpha blending (cannot be used with halftone scaling)
  Antialiasing in slide show and bitmap export
  Text formatting bugfix
  Easier editing of link points using the mouse
Changes in version 1.19  (2007-06-05):
  Closed curve can be converted to polygon and filled
  Preview of template palettes in load dialog
  Preview/thumbnails of diagrams in Explorer
  Flowchart palette based on the work of Allen Titley and Alessandro Visentin and others
  Move up/down buttons in link editor
  Template palette remembered from last run
Changes in version 1.18.1  (2007-01-12):
  New curve line types
  Connector selection bug fix
  Topmost non-empty layer activated by default when changing page 
Changes in version 1.18  (2006-12-30):
  Ctrl+arrow keys move selected objects by one grid unit
  Ctrl+Alt+arrow keys move selected objects by one screen pixel
  Layer menu can be opened from the status bar 
  New menu shortcut system: Just begin typing the name of the menu item or press F10
  Fixed bug in formatting of text with different font sizes and multiple lines 
  Radius property on rectangles and connectors (only works with solid line style)
  Symmetrical resizing of objects using the Shift key (press Shift AFTER pressing the mouse button on a corner)
  Fixed bug with text on objects related to multi-byte character sets
Changes in version 1.17  (2006-09-26):
  Resizing of group objects
  Anchors on grouped objects for use when resizing
  GUI design palette
  Screen saver disabled during full screen slide show
Changes in version 1.16.5  (2006-09-07):
  Small bugfix
Changes in version 1.16.4  (2006-09-05):
  Shortcut menu to template palette switching in popup menu
  Quick rotation of metafile objects using the mouse 
Changes in version 1.16.3  (2006-08-20):
  Fixed bug where mirror/flip of metafile objects are not saved to the DDD file
  Default zoom can now be set by creating a DWORD registry value called Zoom
Changes in version 1.16.2  (2006-08-09):
  Fixed bug in line and curve drawing introduced in 1.16.1 
  Editing text when multiple objects are selected will update the text of all
Changes in version 1.16.1  (2006-07-04):
  Enabled keyboard shortcuts to menus
  Quick copy by dragging objects while holding Ctrl
  Text object in toolbar
Changes in version 1.16  (2006-06-06):
  New text translation system with plain text language files
  User interface updates
  Popup note text attribute (\N)
  A number of bugfixes
Changes in version 1.15.4  (2006-04-03):
  Fixed subscript/superscript bug with multiline text objects 
Changes in version 1.15.3  (2006-02-19):
  General speedup
  Fixed rotation bug for group and bitmap objects
Changes in version 1.15.2  (2006-02-05):
  Selecting "Edit text" for a group object now allows editing the text of the first object inside the group
  Fixed rotation/mirror bug for metafile and polygon objects
Changes in version 1.15  (2006-01-07):
  Text color (\C) attribute
  Improved BMP and JPEG import
  Better handling of "Large fonts" setting in Windows
  New program help file by Allen Titley
Changes in version 1.14  (2005-08-12):
  Links between pages (\Apage)
  User interface updates
  Underline (\U) and overline (\O) text
  Improved text editing dialog
Changes in version 1.13.2  (2005-02-06):
  Preview bugfix   
Changes in version 1.13  (2005-01-23):
  Links between documents (\Afilename.ddd)
  Text margin
  User interface updates
Changes in version 1.12  (2004-08-07):
  Problem with graph plotter in expression evaluator fixed 
  Link point deletion bugfix
  Non-latin characters in translation supported
  Windows XP theme support  
Changes in version 1.11  (2004-05-31):
  GIF import/export
  Double arrow type
  Minor improvements
Changes in version 1.09  (2004-03-18):
  Slide show capabilities
  More keyboard shortcuts
  Option to align objects to page or selection boundaries
  Spell checker
  Rotation (only implemented for some object types)   
Changes in version 1.00  (2004-02-19):
  Double-click object to edit text
  Multiple connected lines can be drawn by left+right clicking while drawing
  Object style editing in tool bar
Changes in version 0.99  (2004-01-24):
  Connect links option
  Connections marked with green boxes when objects are selected
Changes in version 0.98  (2004-01-15):
  "Paste special" option
  Text objects no longer has a link point
  Font name and size can be changed in text labels
  Template palette can be edited as normal diagram page
  New line styles
  Holding Ctrl while editing changes edit mode for lines, shapes and pictures
  Holding Alt while editing will disable "snap to grid"
Changes in version 0.97  (2003-12-26):
  Expression evaluator precision and plot axis auto-scaling improved
  Import MNG pictures
  New text alignment options
  Language module
Changes in version 0.96  (2003-12-06):
  New page properties dialog
  Default line width changed to 3/4 point
Changes in version 0.94  (2003-11-25):
  Multi-layer designs
  Automatic renaming of objects to match text
  Additional object shapes
Changes in version 0.92  (2003-11-12):
  All objects/text can be converted to metafile objects
  Rotation of metafile objects
Changes in version 0.91  (2003-11-08):
  Object tree display
  Curve object type
Changes in version 0.90  (2003-10-25):
  First public release
  