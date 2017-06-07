# sphinx-mode

Minor mode providing sphinx support.

# Usage

In your `rst-mode` buffer call `M-x sphinx-mode` to add all the extra features.

# Features

* [x] Add fontification support
* [ ] Add support for insertion of labels
* [x] Add support for insertion of references (`:ref:`) (with a menu of available labels)
* [x] Add support for jumping to a label/reference
* [ ] Add footnote (`[#note]`) support
* [ ] Add reference (`[ref]`) support

## Native code-block fontification

Similar to `org-mode`, we provide native emacs fontification of code blocks.

![fontification](docs/_images/native-fontification.png)
