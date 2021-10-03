
####################
Highlight Block Mode
####################

This is a minor mode for highlighting blocks by scope.

.. figure:: https://user-images.githubusercontent.com/1869379/56872098-975b5900-6a68-11e9-8907-ccca12b5608a.png

Available via `melpa <https://melpa.org/#/hl-block-mode>`__.


Options
=======

:hl-block-bracket:
   (symbol) bracket to use, ``{`` by default,
   set to ``nil`` to match all bracket types.
:hl-block-delay:
   (float) delay in seconds until the drawing the block scope.
:hl-block-color-tint:
   (color) color tint for each level.
:hl-block-multi-line:
   Skip highlighting blocks that begin and end on the same line.

TODO
====

- Optionally, update instantly (without any delay).
- Avoid updating overlays when highlights havn't moved.
- Add default brackets based on the major mode, ``()`` for lisp's, ``{}`` for C/C++ etc.
