import os
import sys

sys.path.insert(0, os.path.abspath('../'))

project = 'Precipitable-Water Model Analysis Tool'
copyright = '2022, Spencer Riley'
author = 'Spencer Riley'
version = 'latest'
release = ''


# -- General configuration

extensions = [
    'sphinx.ext.autodoc',
    'sphinx.ext.autosummary',
    'sphinx.ext.coverage',
    'sphinx.ext.napoleon',
    'sphinx.ext.viewcode',
]


# -- Options for HTML output

html_title = 'PMAT Documentation'
html_logo = 'banner_icon.png'
html_favicon = 'icon.png'

html_static_path = ['_static']
html_css_files = [
    'css/custom.css',
]
html_theme = 'sphinx_rtd_theme'

html_theme_options = {
    'logo_only': True,
}

# -- Options for LaTeX output
latex_engine = 'pdflatex'
latex_elements = {
    'papersize': 'a5paper',
    'pointsize': '11pt',
    }
latex_documents = [
 ('index', 'pmat.tex', u'Precipitable-Water Model Analysis Tool Documentation', u'Spencer Riley', 'manual'),
]
latex_logo = 'banner_icon.png'
latex_domain_indices = True
latex_show_urls = 'footnote'
latex_use_xindy = False
# -- Options for EPUB output
epub_show_urls = 'footnote'
