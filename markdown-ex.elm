module Main exposing (..)

import Markdown


main =
    Markdown.toHtml [] markdown


markdown =
    """
# Header1
## Header 2
### Header 3
*bold*
_italic_
~what~
"""
