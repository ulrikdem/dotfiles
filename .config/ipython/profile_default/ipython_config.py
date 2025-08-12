from importlib.util import find_spec
from pathlib import Path
import sys

c.TerminalIPythonApp.display_banner = False

c.TerminalInteractiveShell.editing_mode = "vi"
c.TerminalInteractiveShell.prompt_includes_vi_mode = False
c.TerminalInteractiveShell.extra_open_editor_shortcuts = True
c.TerminalInteractiveShell.confirm_exit = False

if find_spec("numpy"):
    c.InteractiveShellApp.exec_lines.append("import numpy as np")

if find_spec("matplotlib"):
    c.InteractiveShellApp.exec_lines.append("import matplotlib.pyplot as plt")
    c.InteractiveShellApp.matplotlib = "inline"

sys.path.append(str(Path(__file__).parent / "extensions"))
if find_spec("PIL"):
    c.InteractiveShellApp.extensions.append("kitty_ipython_ext")
