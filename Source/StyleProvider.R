# MIT License
#
# Copyright(c) 2018 Ilia Kosenkov[ilia.kosenkov.at.gm@gmail.com]
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files(the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and / or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
# IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
# DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
# TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE
# OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

purrr::pmap(tibble::tribble(
    ~Vars,                    ~Vals,
    "Style_LabelFontSz",      17,
    "Style_TickFontSz",       15,
    "Style_GroupColors",      c(brewer.pal(8, "Paired")
                                [c(1, 5, 3, 9, 2, 6, 4, 10)], "#000000"),
    "Style_GroupFills",       c(brewer.pal(8, "Paired")
                                [c(1, 5, 3, 9, 2, 6, 4, 10)], "#000000"),
    "Style_GroupShapes",      c(1, 0, 5, 2, 21, 22, 23, 24, 21),
    "Style_Groups",           c(0:7, -1),
    "Style_GroupsAvgOnly",    c(4:7, 0:3, -1),
    "Style_GroupsBands",      c(2:4, -1),
    "Style_GroupLinesBands",  c(1, 2, 4, 1),
    "Style_GroupColorsBands", c(brewer.pal(3, "Set1")[c(2, 3, 1)], "#000000"),
    "Style_GroupShapesBands", c(21, 22, 23, 21),
    "Style_LabsMarStd",       margin(0.0, 0.0, 1, 1, "cm"),
    "Style_AxisMarStd",       margin(0.25, 0.25, 0.5, 0.75, "cm"),
    "Style_VGapStd",          unit(0.1, "cm"),
    "Style_WidthStdInch",     6,
    "Style_HeightStdInch",    6,
    "Style_SymbolSize",       4,
    "Style_SymbolSizeSmall",  2,
    "Style_ErrorBarSize",     0.75,
    "Style_AlphaBackground",  0.15,
    "Style_LineSize",         0.75,
    "Style_ArrowLength",      unit(10, "pt")
    ),
    ~makeActiveBinding(.x, function() .y, .GlobalEnv))