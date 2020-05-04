# Configuration file for the Sphinx documentation builder.
#
# This file only contains a selection of the most common options. For a full
# list see the documentation:
# https://www.sphinx-doc.org/en/master/usage/configuration.html

# -- Path setup --------------------------------------------------------------

# If extensions (or modules to document with autodoc) are in another directory,
# add these directories to sys.path here. If the directory is relative to the
# documentation root, use os.path.abspath to make it absolute, like shown here.
#
# import os
# import sys
# sys.path.insert(0, os.path.abspath('.'))
import sphinx_rtd_theme, subprocess, os

# Uses the READTHEDOCS environment variable to determine whether or not we are 
# building on the ReadTheDocs (https://readthedocs.org/) servers. ReadTheDocs 
# set this environment variable for this purpose 
# (https://docs.readthedocs.io/en/latest/faq.html#how-do-i-change-behavior-for-read-the-docs)
read_the_docs_build = os.environ.get('READTHEDOCS', None) == 'True'
subprocess.call('doxygen', shell=True)

# -- Project information -----------------------------------------------------

project = 'Interactive Power Flow'
copyright = '2020, Interactive Power Flow Developers'
author = 'IPF Developers'

numfig = True
math_numfig = True
numfig_secnum_depth = 2
math_eqref_format = "Eq.{number}"

# -- General configuration ---------------------------------------------------

# Add any Sphinx extension module names here, as strings. They can be
# extensions coming with Sphinx (named 'sphinx.ext.*') or your custom
# ones.
extensions = [
    'sphinx_rtd_theme',
    'breathe'
]

# Use index.rst as master instead of default contents.rst
master_doc = 'index'

# Add any paths that contain templates here, relative to this directory.
templates_path = ['_templates']

# List of patterns, relative to source directory, that match files and
# directories to ignore when looking for source files.
# This pattern also affects html_static_path and html_extra_path.
exclude_patterns = ['_build', 'Thumbs.db', '.DS_Store']

# -- Options for HTML output -------------------------------------------------

# The theme to use for HTML and HTML Help pages.  See the documentation for
# a list of builtin themes.
#
html_theme = 'sphinx_rtd_theme'

# Add any paths that contain custom static files (such as style sheets) here,
# relative to this directory. They are copied after the builtin static files,
# so a file named "default.css" will overwrite the builtin "default.css".
html_static_path = ['_static']

# -- Extra configuration -----------------------------------------------------
# Define breathe project and path to Doxygen XML output
breathe_projects = { "ipfdoxygen": "doxygen/xml" }

# Set default breathe project
breathe_default_project = "ipfdoxygen"