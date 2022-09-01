==================
python-coverage.el
==================

This is an experimental Emacs package to report code coverage output
produced by Python's ``coverage`` package directly inside Emacs
buffers.

Best used together with `python-pytest.el`__.

__ https://github.com/wbolster/emacs-python-pytest


Screenshot
==========

.. image:: screenshot.png
   :alt: mandatory screenshot

Overlays
========

Minor mode for automagically updated overlays:

- ``python-coverage-overlay-mode``

Alternatively, commands for manually updated overlays:

- ``python-coverage-overlay-refresh``
- ``python-coverage-overlay-remove-all``

Navigation commands:

- ``python-coverage-overlay-jump-next``
- ``python-coverage-overlay-jump-previous``
- ``python-coverage-overlay-jump-first``

Flycheck checker
================

Run ``flycheck-select-checker``, pick ``python-coverage``.

Coverage data
=============

This package can read both the SQLite ``.coverage`` file created by Python’s
``coverage`` package, and the XML report it produces. The latter is preferred if
present, because the ``coverage xml`` command does some additional analysis to
ignore docstrings etc.

With plain ``coverage``, generate the XML file like this:

.. code-block:: shell

  $ coverage xml

With ``pytest-cov``, pass ``--cov-report=xml``, e.g. via ``pyproject.toml``:

.. code-block:: toml

  [tool.pytest.ini_options]
  addopts = [
    "--cov=your-package",
    "--cov=test",
    "--cov-report=xml",


Customization
=============

Command for manual coverage file selection:

- ``python-coverage-select-coverage-file``

Customizable settings (see their description for details) in the
``python-coverage`` group, e.g. via ``M-x customize-group``:

- ``python-coverage-default-xml-file-name``
- ``python-coverage-default-sqlite-file-name``
- ``python-coverage-overlay-width``

Styling via custom faces, e.g. via ``M-x customize-face``:

- ``python-coverage-overlay-missing``
- ``python-coverage-overlay-partial``
- ``python-coverage-overlay-missing-outdated``
- ``python-coverage-overlay-partial-outdated``

Note: by default this package inherits from ``magit-diff-*`` faces,
which should work in most themes. This requires ``magit`` to be
installed, but it's not otherwise used.

Credits
=======

This package was created by `wouter bolsterlee (@wbolster)`__.

__ https://github.com/wbolster

License
=======

BSD; see ``LICENSE.rst``.
