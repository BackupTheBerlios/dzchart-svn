dummzeuch.de chart component package version 2.1.0

      copyright 2003-2004 by Thomas Mueller

 *****************************************************************************
 * Version: MPL 1.1
 *
 * The contents of this package are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original package is dummzeuch.de Charts.
 *
 * The Initial Developer of the Original package is
 * Thomas Mueller <chart@dummzeuch.de>.
 * Portions created by the Initial Developer are Copyright (C) 2003-2004
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 * * Paul Warren (homegrown charts, the basis for these components)
 * * Andre Snepvangers / Andreas Hausladen (QWindows from Jedi VCL)
 *****************************************************************************

Update instructions:

NOTE: In dzCharts 2.1 I have changed the TXYChart.Grid property from being an
enum to being a subcomponent. If you have already used dzCharts 2.0 in some
of your forms, you will get an access violation when you try to open them.
This is due to Delphi trying to use the enum stored in the dfm file as an object.
To fix this, open your .dfm file in a text editor (e.g. Notepad), search for all
lines containing the Grid property and delete them. After that it should be
possible to open the forms in Delphi again. If your .dfm file is still in
binary format, you must use the convert utility that comes with Delphi to
convert it to text (and back, if you still want binary format).
 
Installation instructions:

Please note that this package has only been tested with Delphi 7 and Kylix 3.
It might work with other Delphi versions but probably requires changes.

* Load dzchart-d7.bpl or dzchart-k3.bpl respectively into the ide
* Each bpl contains 3 projects:
    dummzeuchchart-??.dpk - the runtime package
    dcldummzeuchchart-??.dpk - the designtime package
    ChartTest.dpr - a test / demo program
* adjust output paths for the projects
* compile dummzeuchchart-??.dpk and dcldummzeuchchart-??.dpk
* install dcldummzeuchchart-??.dpk

There should now be an additional page "dummzeuch.de" on the component palette
containing three components: dzPieChart, dzPolynomialDataseries and dzXYChart.

Documentation is supplied in the source code itself in a format suitable for
PasDoc (http://pasdoc.sf.net), use ":" as marker. The utils subdirectory contains
precompiled versions of the pasdoc executable for Windows and Linux so you
can just call the makedoc/makedoc.cmd script to generate documentation in
the subdirectory docs.

There are lots of new things, but unfortunately I only recently started to document
the changes. The most important change is that dzChart now supports not only single
value data series but also multi value data series (see u_dzDataSeries). Also the
callback events for adding horizontal/vertical lines and up/down arrows have been
replaced by a new chart type ctAlternatingLine and a new point style (psUpArrow,
psDownArrow).
I think this is much more convenient than my original solution.
Scaling the axes has also been improved, see the Scale subcomponent of the axis.

Again, if you have any comments or want to contribute to dzChart, feel free to contact
me at the above address.

Also I would like feedback if you use these components. So far I have received none
at all which could mean everything between nobody even looked at it, or everybody
who looked at it turned away in horror. ;-)

last updated 2004-08-28 by twm
