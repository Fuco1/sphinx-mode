============================
Sample reStructuredText file
============================

This is a reStructuredText file containing various code blocks.

.. code-block:: elisp

  (use-package rst-mode)

  (use-package sphinx-mode)


- Below is a code-block inside an indented list item.

  .. code-block::

    def foo(a=1):
        pass

    def bar(b=2):
        pass

This is the concise syntax using double colons::

  def foo():
      pass

Double colons can also occur on their own line, so that this paragraph
can end with other punctuation. Nice, isn't it?

::

  def foo():
      pass


.. note::

   This is not a code block, even though it follows a line ending with
   a double colon.

This is a paragraph that announces a code block that should span to
the end of the file, but it is missing (or not yet written). While this
is not valid reStructuredText syntax, it should not cause the
highlighting to hang or fail::
