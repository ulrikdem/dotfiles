import subprocess
from base64 import b64decode
from IPython.lib.latextools import latex_to_png

def load_ipython_extension(ipython):
    types = ["text/plain", "text/latex"]
    ipython.mime_renderers["text/latex"] = lambda data, _: display_img(latex_to_png(data, color="White", backend="dvipng"))
    for mime in ["image/png", "image/jpeg", "image/svg+xml", "application/pdf"]:
        ipython.mime_renderers[mime] = display_img
        types.append(mime)
    ipython.display_formatter.active_types = types

def display_img(data, _metadata=None):
    if isinstance(data, str):
        data = b64decode(data)
    print()
    subprocess.run(["kitten", "icat", "--align=left", "--stdin=yes"], input=data)
