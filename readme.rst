
####################
Highlight Block Mode
####################

This is a minor mode for highlighting blocks by scope.

.. figure:: https://user-images.githubusercontent.com/1869379/56872098-975b5900-6a68-11e9-8907-ccca12b5608a.png

Available via `melpa <https://melpa.org/#/hl-block-mode>`__.


Options
=======

``hl-block-bracket``: ``"{"``.
   (string or null) bracket to use, set to ``nil`` to match all bracket types.
``hl-block-delay``: ``0.2``
   (float) delay in seconds until the drawing the block scope.
``hl-block-multi-line``: ``t``
   Skip highlighting blocks that begin and end on the same line.
``hl-block-single-level``: ``nil``
   Only highlight a single level of surrounding braces.
``hl-block-style``: ``'color-tint``
   The draw-style used to highlight blocks.

   ``'color-tint``:
      Apply a color tint to the background, increasing for each level.

      ``hl-block-color-tint``: ``"#040404"``
         (color) color tint for each level.

   ``'bracket``:
      Highlight each bracket using a custom face, without changing the background.

      ``hl-block-bracket-face``: ``'(t (:inverse-video t))``
         The face used to highlight brackets.


Examples
========

This example shows how this package can be used to highlight only the surrounding brackets.

.. code-block:: elisp

   (use-package hl-block-mode
     :commands (hl-block-mode)
     :config
     (setq hl-block-bracket nil)    ;; Match all brackets.
     (setq hl-block-single-level t) ;; Only one pair of brackets.
     (setq hl-block-style 'bracket) ;; Highlight only the brackets.
     :hook ((prog-mode) . hl-block-mode))


TODO
====

- Optionally, update instantly (without any delay).
- Avoid updating overlays when highlights haven't moved.
- Add default brackets based on the major mode, ``()`` for lisp's, ``{}`` for C/C++ etc.
