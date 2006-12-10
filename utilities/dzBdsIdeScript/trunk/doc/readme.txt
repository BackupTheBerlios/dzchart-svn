Disclaimer: By using this sample, you agree not to hold me liable for anything
concerning it. If you do not agree, do not use it, read it, or distribute it.
If you do agree, you can do anything you want with it, taking full
responsability.

--------------------------------------------------------------------------------
Purpose: This sample has a number of experts: one to turn an ActionList into a
MainMenu, another to search the web, and two others to sort lines and
declaration items.

The CodeCentral snippet id is: 14674

Have fun,

Marcelo Lopez Ruiz
marcelo.lopezruiz@xlnet.com.ar

For comments, suggestions, flames, and job offerings at Borland, please e-mail
me at marcelo.lopezruiz@xlnet.com.ar.

--------------------------------------------------------------------------------
Requirements:
Delphi 5.

--------------------------------------------------------------------------------
Overview:

In this snippet, I have not inlcuded a package, because my own package has a
few more components (which are not ready to be released, yet).

To install, create a package, add the units, accept the suggestions, build,
and you are done.

The ActionsToMenu expert will get enabled when an ActionList component is
selected. It will create a new MenuItem, top-level menu items for each
category, and menu items for each action. You can then rearrange them as you
please. You will find the expert as a menu item in the Database menu (it will
make more sense when I release the other experts).

The WebSearch expert will read entries from the Registry (you can click on
customize to find the Registry locations). The name of a value is the
text displayed, and the value itself is the URL to launch. This value is
a format string, as specified in the "Format" help topic (use '%s' to insert
the selected text). This expert can be found at the end of the Help menu.

I suppose it could also be used to launch executables, but it encodes the
selected text for a URL, so you should be careful.

Two sorting actions were added to the ActionsToMenu expert (even if they
don't properly belong there) in the second release. Be careful with two things:
all declarations must be one-liners, and you should select full lines (it
doesn't matter if the last line has no selected characters on it; just go to
the first declaration, start of the line, and shift+down all of the
declarations).

Probably, the most interesting part of this snippet is the set of functions
declared in MLROTExpertsUnit.pas. They encapsulate calls to the OTA, so
the experts (which I wrote in DataModules, using the technique offered by
Ian Marteens at http://www.marteens.com/trick15.htm - great work, Ian!) do not
even have to reference the ToolsAPI unit.

--------------------------------------------------------------------------------
History
2000.04.27
+ Second release. Added debugging support, turning off web search refreshing,
  better help from the "(customize)" menu item, and sorting lines and methods.
  Moved all items to a 'MLR' menu.
2000.04.26
+ First release.
