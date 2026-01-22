import sys, os
from deltachat import __version__ as release
version = '.'.join(release.split('.')[:2])
extensions = ['sphinx.ext.autodoc', 'sphinx.ext.autosummary', 'sphinx.ext.todo', 'sphinx.ext.viewcode', 'breathe']
templates_path = ['_templates']
source_suffix = '.rst'
master_doc = 'index'
project = u'deltachat'
copyright = u'2018, holger krekel and contributors'
exclude_patterns = ['sketch', '_build', 'attic']
pygments_style = 'sphinx'
breathe_projects = {'deltachat': '../../docs/xml/'}
breathe_default_project = 'deltachat'
sys.path.append(os.path.abspath('_themes'))
html_theme_path = ['_themes']
html_theme = 'alabaster'
html_theme_options = {'logo': '_static/delta-chat.svg', 'font_size': '1.1em', 'caption_font_size': '0.9em', 'code_font_size': '1.1em'}
html_logo = '_static/delta-chat.svg'
html_favicon = '_static/favicon.ico'
html_static_path = ['_static']
html_sidebars = {'index': ['sidebarintro.html', 'globaltoc.html', 'searchbox.html'], '**': ['sidebarintro.html', 'globaltoc.html', 'relations.html', 'searchbox.html']}
html_show_sourcelink = False
html_show_sphinx = False
html_use_opensearch = 'https://doc.devpi.net'
htmlhelp_basename = 'deltachat-python'
latex_elements = {'pointsize': '12pt'}
latex_documents = [('index', 'devpi.tex', u'deltachat documentation', u'holger krekel', 'manual')]
man_pages = [('index', 'deltachat', u'deltachat documentation', [u'holger krekel'], 1)]
texinfo_documents = [('index', 'devpi', u'devpi Documentation', u'holger krekel', 'devpi', 'One line description of project.', 'Miscellaneous')]
intersphinx_mapping = {'http://docs.python.org/': None}
autodoc_member_order = 'bysource'
def skip(app, what, name, obj, skip, options):
    import attr
    if name == '__init__':
        if not hasattr(obj.im_class, '__attrs_attrs__'):
            return False
    return skip
def setup(app):
    app.connect('autodoc-skip-member', skip)