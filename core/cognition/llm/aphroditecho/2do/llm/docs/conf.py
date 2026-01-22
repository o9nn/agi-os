from subprocess import PIPE, Popen
extensions = ['myst_parser', 'sphinx_copybutton', 'sphinx_markdown_builder', 'sphinx.ext.autodoc']
myst_enable_extensions = ['colon_fence']
markdown_http_base = 'https://llm.datasette.io/en/stable'
markdown_uri_doc_suffix = '.html'
templates_path = ['_templates']
source_suffix = '.rst'
master_doc = 'index'
project = 'LLM'
copyright = '2025, Simon Willison'
author = 'Simon Willison'
pipe = Popen('git describe --tags --always', stdout=PIPE, shell=True)
git_version = pipe.stdout.read().decode('utf8')
if git_version:
    version = git_version.rsplit('-', 1)[0]
    release = git_version
else:
    version = ''
    release = ''
language = 'en'
exclude_patterns = ['_build', 'Thumbs.db', '.DS_Store']
pygments_style = 'sphinx'
todo_include_todos = False
html_theme = 'furo'
html_theme_options = {}
html_title = 'LLM'
html_static_path = []
htmlhelp_basename = 'llm-doc'
latex_elements = {}
latex_documents = [(master_doc, 'llm.tex', 'LLM documentation', 'Simon Willison', 'manual')]
man_pages = [(master_doc, 'llm', 'LLM documentation', [author], 1)]
texinfo_documents = [(master_doc, 'llm', 'LLM documentation', author, 'llm', ' Access large language models from the command-line ', 'Miscellaneous')]