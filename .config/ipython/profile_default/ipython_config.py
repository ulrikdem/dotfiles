from pathlib import Path
import sys

c.TerminalIPythonApp.display_banner = False
c.TerminalInteractiveShell.editing_mode = "vi"
c.TerminalInteractiveShell.prompt_includes_vi_mode = False
c.TerminalInteractiveShell.extra_open_editor_shortcuts = True
c.TerminalInteractiveShell.confirm_exit = False

try:
    import numpy
    c.InteractiveShellApp.exec_lines.append("import numpy as np")
    import matplotlib
    c.InteractiveShellApp.exec_lines.append("import matplotlib.pyplot as plt")
    c.InteractiveShellApp.matplotlib = "inline"
except ModuleNotFoundError:
    pass

sys.path.append(str(Path(__file__).parent / "extensions"))
c.InteractiveShellApp.extensions.append("kitty_ipython_ext")
